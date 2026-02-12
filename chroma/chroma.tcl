#!/usr/bin/env tclsh
#
# convert.tcl - Convert between ColorFORTH binary (.cf) and Fifth text (.fifth)
#
# Usage:
#   tclsh convert.tcl input.cf output.fifth
#   tclsh convert.tcl input.fifth output.cf
#

package require Tcl 8.5

# Convert integer to binary string (Tcl 8.5 compatible)
proc int_to_bin {n width} {
    set result ""
    for {set i [expr {$width - 1}]} {$i >= 0} {incr i -1} {
        append result [expr {($n >> $i) & 1}]
    }
    return $result
}

# Convert binary string to integer
proc bin_to_int {bits} {
    set n 0
    foreach b [split $bits ""] {
        set n [expr {($n << 1) | $b}]
    }
    return $n
}

# Build encoding tables based on actual ColorFORTH Shannon codes
# 4-bit: 0xxx (8 chars)
# 5-bit: 10xxx (8 chars)
# 7-bit: 11xxxxx (32 chars)

proc init_char_tables {} {
    set ::char_to_bits [dict create]
    set ::bits_to_char [dict create]

    # 4-bit codes (0000-0111): space r t o e a n i
    set chars4 " rtoeanismcylgfwdvpbhxuqkzj3456789"
    append chars4 "1-0.2/;:!+@*,?"

    # Actual encoding from ColorFORTH chars.html
    # 4-bit: 0xxx
    set codes4 {
        " " 0000  r 0001  t 0010  o 0011
        e 0100  a 0101  n 0110  i 0111
    }
    # 5-bit: 10xxx
    set codes5 {
        s 10000  m 10001  c 10010  y 10011
        l 10100  g 10101  f 10110  w 10111
    }
    # 7-bit: 11xxxxx
    set codes7 {
        d 1100000  v 1100001  p 1100010  b 1100011
        h 1100100  x 1100101  u 1100110  q 1100111
        k 1101000  z 1101001  j 1101010  3 1101011
        4 1101100  5 1101101  6 1101110  7 1101111
        8 1110000  9 1110001  1 1110010  - 1110011
        0 1110100  . 1110101  2 1110110  / 1110111
        ; 1111000  : 1111001  ! 1111010  + 1111011
        @ 1111100  * 1111101  , 1111110  ? 1111111
    }

    foreach {c bits} $codes4 {
        dict set ::char_to_bits $c $bits
        dict set ::bits_to_char $bits $c
    }
    foreach {c bits} $codes5 {
        dict set ::char_to_bits $c $bits
        dict set ::bits_to_char $bits $c
    }
    foreach {c bits} $codes7 {
        dict set ::char_to_bits $c $bits
        dict set ::bits_to_char $bits $c
    }
}

# Emoji as raw UTF-8 bytes (Tcl 8.5 workaround)
# 🟥 U+1F7E5 = F0 9F 9F A5
# 🟨 U+1F7E8 = F0 9F 9F A8
# 🟩 U+1F7E9 = F0 9F 9F A9
# ⬜ U+2B1C = E2 AC 9C
# 🟦 U+1F7E6 = F0 9F 9F A6
# 💬 U+1F4AC = F0 9F 92 AC
set ::EMOJI_RED    "\xF0\x9F\x9F\xA5"
set ::EMOJI_YELLOW "\xF0\x9F\x9F\xA8"
set ::EMOJI_GREEN  "\xF0\x9F\x9F\xA9"
set ::EMOJI_WHITE  "\xE2\xAC\x9C"
set ::EMOJI_BLUE   "\xF0\x9F\x9F\xA6"
set ::EMOJI_COMMENT "\xF0\x9F\x92\xAC"

# Tag definitions (4-bit)
# Maps ColorFORTH tags to emoji color markers
set ::tag_to_emoji [dict create \
    0  ""                \
    1  $::EMOJI_YELLOW   \
    2  $::EMOJI_WHITE    \
    3  $::EMOJI_YELLOW   \
    4  $::EMOJI_RED      \
    5  $::EMOJI_GREEN    \
    6  $::EMOJI_GREEN    \
    7  $::EMOJI_GREEN    \
    8  $::EMOJI_WHITE    \
    9  $::EMOJI_COMMENT  \
    10 $::EMOJI_COMMENT  \
    11 $::EMOJI_COMMENT  \
    12 $::EMOJI_BLUE     \
]

# Reverse mapping: emoji to primary tag
set ::emoji_to_tag [dict create \
    $::EMOJI_RED     4  \
    $::EMOJI_YELLOW  1  \
    $::EMOJI_GREEN   6  \
    $::EMOJI_WHITE   8  \
    $::EMOJI_BLUE    12 \
    $::EMOJI_COMMENT 9  \
]

# Decode a 32-bit word into tag and characters
proc decode_word {word} {
    set tag [expr {$word & 0xF}]
    set chars_bits [expr {$word >> 4}]

    # Convert 28 bits to characters
    set text ""
    set bits [int_to_bin $chars_bits 28]
    set pos 0

    while {$pos < 28} {
        set remaining [expr {28 - $pos}]
        if {$remaining < 4} break

        set b0 [string index $bits $pos]

        if {$b0 eq "0"} {
            # 4-bit code (0xxx)
            set code [string range $bits $pos [expr {$pos + 3}]]
            if {[dict exists $::bits_to_char $code]} {
                set c [dict get $::bits_to_char $code]
                if {$c eq " "} {
                    # Space = end of word
                    break
                }
                append text $c
            }
            incr pos 4
        } else {
            # Starts with 1
            if {$remaining < 5} break
            set b1 [string index $bits [expr {$pos + 1}]]

            if {$b1 eq "0"} {
                # 5-bit code (10xxx)
                set code [string range $bits $pos [expr {$pos + 4}]]
                if {[dict exists $::bits_to_char $code]} {
                    append text [dict get $::bits_to_char $code]
                }
                incr pos 5
            } else {
                # 7-bit code (11xxxxx)
                if {$remaining < 7} break
                set code [string range $bits $pos [expr {$pos + 6}]]
                if {[dict exists $::bits_to_char $code]} {
                    append text [dict get $::bits_to_char $code]
                }
                incr pos 7
            }
        }
    }

    return [list $tag $text]
}

# Encode a word with tag to list of 32-bit values (handles long words)
# Splits on character boundaries to avoid corrupting encoding
proc encode_word {tag text} {
    set words {}
    set chars [split [string tolower $text] ""]
    set first 1

    while {[llength $chars] > 0} {
        set bits ""
        set used_chars {}

        # Add characters until we exceed 28 bits
        foreach c $chars {
            if {![dict exists $::char_to_bits $c]} continue

            set char_bits [dict get $::char_to_bits $c]
            if {[string length $bits] + [string length $char_bits] > 28} {
                break
            }
            append bits $char_bits
            lappend used_chars $c
        }

        # Remove used characters from list
        set chars [lrange $chars [llength $used_chars] end]

        # Pad to 28 bits
        while {[string length $bits] < 28} {
            append bits "0"
        }

        set chars_val [bin_to_int $bits]

        if {$first} {
            # First word uses the original tag
            lappend words [expr {($chars_val << 4) | ($tag & 0xF)}]
            set first 0
        } else {
            # Extension words use tag 0
            lappend words [expr {($chars_val << 4) | 0}]
        }
    }

    # Handle empty text
    if {[llength $words] == 0} {
        lappend words [expr {($tag & 0xF)}]
    }

    return $words
}

# Read ColorFORTH binary file
proc read_cf {filename} {
    set f [open $filename rb]
    set data [read $f]
    close $f

    set words {}
    set len [string length $data]

    for {set i 0} {$i + 4 <= $len} {incr i 4} {
        binary scan $data "@${i}iu" word
        lappend words $word
    }

    return $words
}

# Write ColorFORTH binary file
proc write_cf {filename words} {
    set f [open $filename wb]
    foreach word $words {
        puts -nonewline $f [binary format iu $word]
    }
    close $f
}

# Convert .cf to .fifth
proc cf_to_fifth {cf_file fifth_file} {
    init_char_tables

    set words [read_cf $cf_file]
    set output ""
    set last_tag -1
    set pending_word ""

    foreach word $words {
        lassign [decode_word $word] tag text

        if {$text eq ""} continue

        if {$tag == 0} {
            # Extension word: append to pending word
            append pending_word $text
            continue
        }

        # Flush pending word if any
        if {$pending_word ne ""} {
            append output "$pending_word "
            set pending_word ""
        }

        # Add emoji if tag changed
        if {$tag != $last_tag && [dict exists $::tag_to_emoji $tag]} {
            set emoji [dict get $::tag_to_emoji $tag]
            if {$emoji ne ""} {
                if {$output ne ""} {
                    append output "\n"
                }
                append output "$emoji "
            }
            set last_tag $tag
        }

        set pending_word $text
    }

    # Flush final pending word
    if {$pending_word ne ""} {
        append output "$pending_word "
    }

    set f [open $fifth_file wb]
    puts -nonewline $f [string trim $output]
    puts -nonewline $f "\n"
    close $f

    puts "Converted [llength $words] words to $fifth_file"
}

# Parse .fifth format
proc parse_fifth {text} {
    set tokens {}
    set current_tag 1  ;# default: yellow execute

    # Split on emoji markers while preserving them
    set emojis [list $::EMOJI_RED $::EMOJI_YELLOW $::EMOJI_GREEN $::EMOJI_WHITE $::EMOJI_BLUE $::EMOJI_COMMENT]

    # Process character by character
    set i 0
    set len [string length $text]
    set current_word ""

    while {$i < $len} {
        set found_emoji 0

        # Check for emoji at current position
        foreach emoji $emojis {
            set elen [string length $emoji]
            if {[string range $text $i [expr {$i + $elen - 1}]] eq $emoji} {
                # Emit current word if any
                if {[string trim $current_word] ne ""} {
                    foreach w [split [string trim $current_word]] {
                        if {$w ne ""} {
                            lappend tokens [list $current_tag $w]
                        }
                    }
                }
                set current_word ""

                # Update tag
                set current_tag [dict get $::emoji_to_tag $emoji]
                incr i $elen
                set found_emoji 1
                break
            }
        }

        if {!$found_emoji} {
            append current_word [string index $text $i]
            incr i
        }
    }

    # Emit final word
    if {[string trim $current_word] ne ""} {
        foreach w [split [string trim $current_word]] {
            if {$w ne ""} {
                lappend tokens [list $current_tag $w]
            }
        }
    }

    return $tokens
}

# Convert .fifth to .cf
proc fifth_to_cf {fifth_file cf_file} {
    init_char_tables

    set f [open $fifth_file rb]
    set text [read $f]
    close $f

    set tokens [parse_fifth $text]
    set words {}

    foreach token $tokens {
        lassign $token tag text
        # encode_word returns a list (may include extension words)
        foreach w [encode_word $tag $text] {
            lappend words $w
        }
    }

    write_cf $cf_file $words

    puts "Converted [llength $tokens] tokens to $cf_file ([llength $words] words)"
}

# Detect format from extension
proc get_format {filename} {
    set ext [string tolower [file extension $filename]]
    switch $ext {
        .cf     { return cf }
        .fifth  { return fifth }
        default { return unknown }
    }
}

# Main
proc main {args} {
    if {[llength $args] != 2} {
        puts "Usage: convert.tcl <input> <output>"
        puts ""
        puts "Converts between ColorFORTH binary (.cf) and Fifth text (.fifth)"
        puts ""
        puts "Examples:"
        puts "  tclsh convert.tcl program.cf program.fifth"
        puts "  tclsh convert.tcl program.fifth program.cf"
        exit 1
    }

    lassign $args input output

    set in_fmt [get_format $input]
    set out_fmt [get_format $output]

    if {$in_fmt eq "unknown"} {
        puts "Error: Unknown input format for $input"
        puts "Supported: .cf, .fifth"
        exit 1
    }

    if {$out_fmt eq "unknown"} {
        puts "Error: Unknown output format for $output"
        puts "Supported: .cf, .fifth"
        exit 1
    }

    if {$in_fmt eq $out_fmt} {
        puts "Error: Input and output formats are the same"
        exit 1
    }

    if {![file exists $input]} {
        puts "Error: Input file not found: $input"
        exit 1
    }

    if {$in_fmt eq "cf" && $out_fmt eq "fifth"} {
        cf_to_fifth $input $output
    } else {
        fifth_to_cf $input $output
    }
}

main {*}$argv
