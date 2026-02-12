#!/usr/bin/env tclsh
#
# fifth.tcl - ColorFORTH to SPARK compiler
#
# SafeTcl-style: no eval, no uplevel, no dynamic tricks
#
# Usage:
#   tclsh fifth.tcl input.fifth output.ads output.adb
#

package require Tcl 8.5

set ::VERSION "0.1.0-alpha"

#############################################################################
# Character Encoding (from Chroma)
#############################################################################

proc int_to_bin {n width} {
    set result ""
    for {set i [expr {$width - 1}]} {$i >= 0} {incr i -1} {
        append result [expr {($n >> $i) & 1}]
    }
    return $result
}

proc bin_to_int {bits} {
    set n 0
    foreach b [split $bits ""] {
        set n [expr {($n << 1) | $b}]
    }
    return $n
}

#############################################################################
# Emoji Constants (raw UTF-8 for Tcl 8.5)
#############################################################################

set ::EMOJI_RED    "\xF0\x9F\x9F\xA5"
set ::EMOJI_YELLOW "\xF0\x9F\x9F\xA8"
set ::EMOJI_GREEN  "\xF0\x9F\x9F\xA9"
set ::EMOJI_WHITE  "\xE2\xAC\x9C"
set ::EMOJI_BLUE   "\xF0\x9F\x9F\xA6"
set ::EMOJI_COMMENT "\xF0\x9F\x92\xAC"

#############################################################################
# Token Types
#############################################################################

set ::TAG_DEFINE   4   ;# Red - define word
set ::TAG_EXECUTE  1   ;# Yellow - execute
set ::TAG_COMPILE  6   ;# Green - compile
set ::TAG_NUMBER   8   ;# White - number
set ::TAG_VARIABLE 12  ;# Blue - variable
set ::TAG_COMMENT  9   ;# Cyan - comment

set ::emoji_to_tag [dict create \
    $::EMOJI_RED     $::TAG_DEFINE  \
    $::EMOJI_YELLOW  $::TAG_EXECUTE \
    $::EMOJI_GREEN   $::TAG_COMPILE \
    $::EMOJI_WHITE   $::TAG_NUMBER  \
    $::EMOJI_BLUE    $::TAG_VARIABLE \
    $::EMOJI_COMMENT $::TAG_COMMENT \
]

#############################################################################
# Lexer - Parse .fifth to token list
#############################################################################

proc lex_fifth {text} {
    set tokens {}
    set current_tag $::TAG_EXECUTE
    set emojis [list $::EMOJI_RED $::EMOJI_YELLOW $::EMOJI_GREEN \
                     $::EMOJI_WHITE $::EMOJI_BLUE $::EMOJI_COMMENT]

    set i 0
    set len [string length $text]
    set current_word ""

    while {$i < $len} {
        set found_emoji 0

        foreach emoji $emojis {
            set elen [string length $emoji]
            if {[string range $text $i [expr {$i + $elen - 1}]] eq $emoji} {
                if {[string trim $current_word] ne ""} {
                    foreach w [split [string trim $current_word]] {
                        if {$w ne ""} {
                            lappend tokens [list $current_tag $w]
                        }
                    }
                }
                set current_word ""
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

    if {[string trim $current_word] ne ""} {
        foreach w [split [string trim $current_word]] {
            if {$w ne ""} {
                lappend tokens [list $current_tag $w]
            }
        }
    }

    return $tokens
}

#############################################################################
# Parser - Build IR from tokens
#############################################################################

proc parse_tokens {tokens} {
    set definitions {}
    set variables {}
    set current_name ""
    set current_body {}
    set in_definition 0

    foreach token $tokens {
        set tag [lindex $token 0]
        set word [lindex $token 1]

        if {$tag == $::TAG_DEFINE} {
            if {$in_definition && $current_name ne ""} {
                lappend definitions [list $current_name $current_body]
            }
            set current_name $word
            set current_body {}
            set in_definition 1
        } elseif {$tag == $::TAG_VARIABLE} {
            lappend variables $word
            if {$in_definition} {
                lappend current_body [list var $word]
            }
        } elseif {$tag == $::TAG_COMPILE} {
            if {$word eq ";"} {
                if {$in_definition && $current_name ne ""} {
                    lappend definitions [list $current_name $current_body]
                }
                set current_name ""
                set current_body {}
                set in_definition 0
            } elseif {$word eq "if"} {
                lappend current_body [list if_start]
            } elseif {$word eq "-if"} {
                lappend current_body [list if_neg_start]
            } elseif {$word eq "then"} {
                lappend current_body [list if_end]
            } elseif {$word eq "else"} {
                lappend current_body [list if_else]
            } elseif {$word eq "for"} {
                lappend current_body [list for_start]
            } elseif {$word eq "next"} {
                lappend current_body [list for_end]
            } elseif {$word eq "begin"} {
                lappend current_body [list begin]
            } elseif {$word eq "while"} {
                lappend current_body [list while]
            } elseif {$word eq "until"} {
                lappend current_body [list until]
            } else {
                lappend current_body [list call $word]
            }
        } elseif {$tag == $::TAG_NUMBER} {
            if {$in_definition} {
                lappend current_body [list push $word]
            }
        } elseif {$tag == $::TAG_EXECUTE} {
            if {$in_definition} {
                lappend current_body [list call $word]
            }
        }
    }

    if {$in_definition && $current_name ne ""} {
        lappend definitions [list $current_name $current_body]
    }

    return [list $definitions $variables]
}

#############################################################################
# Primitive Words - Stack Effects and SPARK Code
#############################################################################

# Primitives: name -> {spark_code needs_pop needs_push}
set ::primitives [dict create \
    dup    {needs_stack 1} \
    drop   {needs_stack 1} \
    swap   {needs_stack 2} \
    over   {needs_stack 2} \
    nip    {needs_stack 2} \
    tuck   {needs_stack 2} \
    rot    {needs_stack 3} \
    -rot   {needs_stack 3} \
    2dup   {needs_stack 2} \
    2drop  {needs_stack 2} \
    2swap  {needs_stack 4} \
    +      {needs_stack 2} \
    -      {needs_stack 2} \
    *      {needs_stack 2} \
    /      {needs_stack 2} \
    mod    {needs_stack 2} \
    /mod   {needs_stack 2} \
    */     {needs_stack 3} \
    negate {needs_stack 1} \
    abs    {needs_stack 1} \
    min    {needs_stack 2} \
    max    {needs_stack 2} \
    2*     {needs_stack 1} \
    2/     {needs_stack 1} \
    and    {needs_stack 2} \
    or     {needs_stack 2} \
    xor    {needs_stack 2} \
    not    {needs_stack 1} \
    =      {needs_stack 2} \
    <>     {needs_stack 2} \
    <      {needs_stack 2} \
    >      {needs_stack 2} \
    <=     {needs_stack 2} \
    >=     {needs_stack 2} \
    0<     {needs_stack 1} \
    0=     {needs_stack 1} \
    u<     {needs_stack 2} \
    @      {needs_stack 1} \
    !      {needs_stack 2} \
    +!     {needs_stack 2} \
    c@     {needs_stack 1} \
    c!     {needs_stack 2} \
    .      {needs_stack 1} \
    h.     {needs_stack 1} \
    emit   {needs_stack 1} \
    key    {needs_stack 0} \
    cr     {needs_stack 0} \
    space  {needs_stack 0} \
    type   {needs_stack 2} \
    i      {needs_stack 0} \
    j      {needs_stack 0} \
]

proc is_primitive {word} {
    return [dict exists $::primitives $word]
}

proc spark_primitive {word indent} {
    set pad [string repeat "   " $indent]

    switch $word {
        dup {
            return "${pad}S.SP := S.SP + 1;
${pad}S.Data (S.SP) := S.Data (S.SP - 1);"
        }
        drop {
            return "${pad}S.SP := S.SP - 1;"
        }
        swap {
            return "${pad}declare
${pad}   Temp : constant Cell := S.Data (S.SP);
${pad}begin
${pad}   S.Data (S.SP) := S.Data (S.SP - 1);
${pad}   S.Data (S.SP - 1) := Temp;
${pad}end;"
        }
        over {
            return "${pad}S.SP := S.SP + 1;
${pad}S.Data (S.SP) := S.Data (S.SP - 2);"
        }
        nip {
            return "${pad}S.Data (S.SP - 1) := S.Data (S.SP);
${pad}S.SP := S.SP - 1;"
        }
        tuck {
            return "${pad}declare
${pad}   Temp : constant Cell := S.Data (S.SP);
${pad}begin
${pad}   S.Data (S.SP) := S.Data (S.SP - 1);
${pad}   S.Data (S.SP - 1) := Temp;
${pad}   S.SP := S.SP + 1;
${pad}   S.Data (S.SP) := Temp;
${pad}end;"
        }
        rot {
            return "${pad}declare
${pad}   Temp : constant Cell := S.Data (S.SP - 2);
${pad}begin
${pad}   S.Data (S.SP - 2) := S.Data (S.SP - 1);
${pad}   S.Data (S.SP - 1) := S.Data (S.SP);
${pad}   S.Data (S.SP) := Temp;
${pad}end;"
        }
        -rot {
            return "${pad}declare
${pad}   Temp : constant Cell := S.Data (S.SP);
${pad}begin
${pad}   S.Data (S.SP) := S.Data (S.SP - 1);
${pad}   S.Data (S.SP - 1) := S.Data (S.SP - 2);
${pad}   S.Data (S.SP - 2) := Temp;
${pad}end;"
        }
        2dup {
            return "${pad}S.SP := S.SP + 2;
${pad}S.Data (S.SP - 1) := S.Data (S.SP - 3);
${pad}S.Data (S.SP) := S.Data (S.SP - 2);"
        }
        2drop {
            return "${pad}S.SP := S.SP - 2;"
        }
        + {
            return "${pad}S.Data (S.SP - 1) := S.Data (S.SP - 1) + S.Data (S.SP);
${pad}S.SP := S.SP - 1;"
        }
        - {
            return "${pad}S.Data (S.SP - 1) := S.Data (S.SP - 1) - S.Data (S.SP);
${pad}S.SP := S.SP - 1;"
        }
        * {
            return "${pad}S.Data (S.SP - 1) := S.Data (S.SP - 1) * S.Data (S.SP);
${pad}S.SP := S.SP - 1;"
        }
        / {
            return "${pad}S.Data (S.SP - 1) := S.Data (S.SP - 1) / S.Data (S.SP);
${pad}S.SP := S.SP - 1;"
        }
        mod {
            return "${pad}S.Data (S.SP - 1) := S.Data (S.SP - 1) mod S.Data (S.SP);
${pad}S.SP := S.SP - 1;"
        }
        /mod {
            return "${pad}declare
${pad}   N : constant Cell := S.Data (S.SP - 1);
${pad}   D : constant Cell := S.Data (S.SP);
${pad}begin
${pad}   S.Data (S.SP - 1) := N mod D;
${pad}   S.Data (S.SP) := N / D;
${pad}end;"
        }
        negate {
            return "${pad}S.Data (S.SP) := -S.Data (S.SP);"
        }
        abs {
            return "${pad}if S.Data (S.SP) < 0 then
${pad}   S.Data (S.SP) := -S.Data (S.SP);
${pad}end if;"
        }
        min {
            return "${pad}if S.Data (S.SP) < S.Data (S.SP - 1) then
${pad}   S.Data (S.SP - 1) := S.Data (S.SP);
${pad}end if;
${pad}S.SP := S.SP - 1;"
        }
        max {
            return "${pad}if S.Data (S.SP) > S.Data (S.SP - 1) then
${pad}   S.Data (S.SP - 1) := S.Data (S.SP);
${pad}end if;
${pad}S.SP := S.SP - 1;"
        }
        2* {
            return "${pad}S.Data (S.SP) := S.Data (S.SP) * 2;"
        }
        2/ {
            return "${pad}S.Data (S.SP) := S.Data (S.SP) / 2;"
        }
        and {
            return "${pad}S.Data (S.SP - 1) := Cell (Interfaces.Integer_32 (S.Data (S.SP - 1)) and Interfaces.Integer_32 (S.Data (S.SP)));
${pad}S.SP := S.SP - 1;"
        }
        or {
            return "${pad}S.Data (S.SP - 1) := Cell (Interfaces.Integer_32 (S.Data (S.SP - 1)) or Interfaces.Integer_32 (S.Data (S.SP)));
${pad}S.SP := S.SP - 1;"
        }
        xor {
            return "${pad}S.Data (S.SP - 1) := Cell (Interfaces.Integer_32 (S.Data (S.SP - 1)) xor Interfaces.Integer_32 (S.Data (S.SP)));
${pad}S.SP := S.SP - 1;"
        }
        not {
            return "${pad}S.Data (S.SP) := Cell (not Interfaces.Integer_32 (S.Data (S.SP)));"
        }
        = {
            return "${pad}if S.Data (S.SP - 1) = S.Data (S.SP) then
${pad}   S.Data (S.SP - 1) := -1;
${pad}else
${pad}   S.Data (S.SP - 1) := 0;
${pad}end if;
${pad}S.SP := S.SP - 1;"
        }
        <> {
            return "${pad}if S.Data (S.SP - 1) /= S.Data (S.SP) then
${pad}   S.Data (S.SP - 1) := -1;
${pad}else
${pad}   S.Data (S.SP - 1) := 0;
${pad}end if;
${pad}S.SP := S.SP - 1;"
        }
        < {
            return "${pad}if S.Data (S.SP - 1) < S.Data (S.SP) then
${pad}   S.Data (S.SP - 1) := -1;
${pad}else
${pad}   S.Data (S.SP - 1) := 0;
${pad}end if;
${pad}S.SP := S.SP - 1;"
        }
        > {
            return "${pad}if S.Data (S.SP - 1) > S.Data (S.SP) then
${pad}   S.Data (S.SP - 1) := -1;
${pad}else
${pad}   S.Data (S.SP - 1) := 0;
${pad}end if;
${pad}S.SP := S.SP - 1;"
        }
        <= {
            return "${pad}if S.Data (S.SP - 1) <= S.Data (S.SP) then
${pad}   S.Data (S.SP - 1) := -1;
${pad}else
${pad}   S.Data (S.SP - 1) := 0;
${pad}end if;
${pad}S.SP := S.SP - 1;"
        }
        >= {
            return "${pad}if S.Data (S.SP - 1) >= S.Data (S.SP) then
${pad}   S.Data (S.SP - 1) := -1;
${pad}else
${pad}   S.Data (S.SP - 1) := 0;
${pad}end if;
${pad}S.SP := S.SP - 1;"
        }
        0< {
            return "${pad}if S.Data (S.SP) < 0 then
${pad}   S.Data (S.SP) := -1;
${pad}else
${pad}   S.Data (S.SP) := 0;
${pad}end if;"
        }
        0= {
            return "${pad}if S.Data (S.SP) = 0 then
${pad}   S.Data (S.SP) := -1;
${pad}else
${pad}   S.Data (S.SP) := 0;
${pad}end if;"
        }
        @ {
            return "${pad}S.Data (S.SP) := Memory (Address (S.Data (S.SP)));"
        }
        ! {
            return "${pad}Memory (Address (S.Data (S.SP))) := S.Data (S.SP - 1);
${pad}S.SP := S.SP - 2;"
        }
        +! {
            return "${pad}Memory (Address (S.Data (S.SP))) := Memory (Address (S.Data (S.SP))) + S.Data (S.SP - 1);
${pad}S.SP := S.SP - 2;"
        }
        . {
            return "${pad}Put (Cell'Image (S.Data (S.SP)));
${pad}S.SP := S.SP - 1;"
        }
        h. {
            return "${pad}Put (To_Hex (S.Data (S.SP)));
${pad}S.SP := S.SP - 1;"
        }
        emit {
            return "${pad}Put (Character'Val (Integer (S.Data (S.SP)) mod 256));
${pad}S.SP := S.SP - 1;"
        }
        key {
            return "${pad}S.SP := S.SP + 1;
${pad}S.Data (S.SP) := Cell (Character'Pos (Get_Immediate));"
        }
        cr {
            return "${pad}New_Line;"
        }
        space {
            return "${pad}Put (' ');"
        }
        i {
            return "${pad}S.SP := S.SP + 1;
${pad}S.Data (S.SP) := Loop_Index;"
        }
        j {
            return "${pad}S.SP := S.SP + 1;
${pad}S.Data (S.SP) := Outer_Loop_Index;"
        }
        default {
            return "${pad}-- Unknown primitive: $word"
        }
    }
}

#############################################################################
# SPARK Code Generator
#############################################################################

proc spark_header {package_name has_io has_memory} {
    set code "-- Generated by Fifth compiler
-- ColorFORTH to SPARK
--
-- DO NOT EDIT - Generated code

pragma SPARK_Mode (On);

package $package_name is

   --  Stack configuration
   Stack_Size : constant := 256;
   type Stack_Index is range 0 .. Stack_Size;
   subtype Valid_Stack_Index is Stack_Index range 1 .. Stack_Size;

   --  Cell is a 32-bit signed integer
   type Cell is range -(2**31) .. (2**31 - 1);

   --  Stack storage
   type Stack_Array is array (Valid_Stack_Index) of Cell;

   --  Stack with pointer
   type Stack_Type is record
      Data : Stack_Array;
      SP   : Stack_Index;
   end record;

   --  Initialize stack
   procedure Init (S : out Stack_Type)
     with Post => S.SP = 0;

"
    return $code
}

proc spark_spec_word {name min_stack} {
    set ada_name [string totitle [string map {- _} $name]]
    return "   procedure $ada_name (S : in out Stack_Type)
     with Pre => S.SP >= $min_stack;

"
}

proc spark_footer {package_name} {
    return "end $package_name;
"
}

proc spark_body_header {package_name has_io has_memory} {
    set code "-- Generated by Fifth compiler
-- ColorFORTH to SPARK
--
-- DO NOT EDIT - Generated code

pragma SPARK_Mode (On);
"
    if {$has_io} {
        append code "with Ada.Text_IO; use Ada.Text_IO;
"
    }
    if {$has_memory} {
        append code "with Interfaces; use Interfaces;
"
    }

    append code "
package body $package_name is

   procedure Init (S : out Stack_Type) is
   begin
      S.Data := (others => 0);
      S.SP := 0;
   end Init;

"
    return $code
}

proc spark_body_footer {package_name} {
    return "end $package_name;
"
}

proc spark_body_word {name body} {
    set ada_name [string totitle [string map {- _} $name]]
    set code "   procedure $ada_name (S : in out Stack_Type) is\n"

    # Check if we need loop variables
    set has_for 0
    set has_begin 0
    foreach op $body {
        set opc [lindex $op 0]
        if {$opc eq "for_start"} { set has_for 1 }
        if {$opc eq "begin"} { set has_begin 1 }
    }

    if {$has_for} {
        append code "      Loop_Index : Cell := 0;\n"
        append code "      Loop_Limit : Cell := 0;\n"
    }

    append code "   begin\n"

    set indent 2
    set if_count 0
    set for_count 0
    set begin_count 0

    foreach op $body {
        set opcode [lindex $op 0]
        set arg [lindex $op 1]
        set pad [string repeat "   " $indent]

        switch $opcode {
            push {
                append code "${pad}S.SP := S.SP + 1;\n"
                append code "${pad}S.Data (S.SP) := $arg;\n"
            }
            call {
                if {[is_primitive $arg]} {
                    append code [spark_primitive $arg $indent]
                    append code "\n"
                } else {
                    set called [string totitle [string map {- _} $arg]]
                    append code "${pad}$called (S);\n"
                }
            }
            var {
                # Variable reference - push address
                set var_name [string toupper $arg]
                append code "${pad}S.SP := S.SP + 1;\n"
                append code "${pad}S.Data (S.SP) := ${var_name}_Addr;\n"
            }
            if_start {
                append code "${pad}declare\n"
                append code "${pad}   If_Flag : constant Cell := S.Data (S.SP);\n"
                append code "${pad}begin\n"
                append code "${pad}   S.SP := S.SP - 1;\n"
                append code "${pad}   if If_Flag /= 0 then\n"
                incr indent 2
                incr if_count
            }
            if_neg_start {
                append code "${pad}declare\n"
                append code "${pad}   If_Flag : constant Cell := S.Data (S.SP);\n"
                append code "${pad}begin\n"
                append code "${pad}   S.SP := S.SP - 1;\n"
                append code "${pad}   if If_Flag < 0 then\n"
                incr indent 2
                incr if_count
            }
            if_else {
                incr indent -1
                set pad [string repeat "   " $indent]
                append code "${pad}else\n"
                incr indent
            }
            if_end {
                incr indent -1
                set pad [string repeat "   " $indent]
                append code "${pad}end if;\n"
                incr indent -1
                set pad [string repeat "   " $indent]
                append code "${pad}end;\n"
            }
            for_start {
                append code "${pad}Loop_Limit := S.Data (S.SP);\n"
                append code "${pad}S.SP := S.SP - 1;\n"
                append code "${pad}Loop_Index := 0;\n"
                append code "${pad}while Loop_Index < Loop_Limit loop\n"
                incr indent
                incr for_count
            }
            for_end {
                append code "${pad}Loop_Index := Loop_Index + 1;\n"
                incr indent -1
                set pad [string repeat "   " $indent]
                append code "${pad}end loop;\n"
            }
            begin {
                append code "${pad}loop\n"
                incr indent
                incr begin_count
            }
            while {
                append code "${pad}exit when S.Data (S.SP) = 0;\n"
                append code "${pad}S.SP := S.SP - 1;\n"
            }
            until {
                append code "${pad}exit when S.Data (S.SP) /= 0;\n"
                append code "${pad}S.SP := S.SP - 1;\n"
                incr indent -1
                set pad [string repeat "   " $indent]
                append code "${pad}end loop;\n"
            }
        }
    }

    append code "   end $ada_name;\n\n"
    return $code
}

proc analyze_words {definitions} {
    set has_io 0
    set has_memory 0

    foreach def $definitions {
        set body [lindex $def 1]
        foreach op $body {
            set opcode [lindex $op 0]
            set arg [lindex $op 1]
            if {$opcode eq "call"} {
                if {$arg in {. h. emit cr space key type}} {
                    set has_io 1
                }
                if {$arg in {@ ! +! c@ c!}} {
                    set has_memory 1
                }
            }
        }
    }

    return [list $has_io $has_memory]
}

proc compute_min_stack {body} {
    # Track stack depth to find minimum initial requirement
    # We simulate the stack effect of each operation
    set depth 0
    set min_depth 0

    foreach op $body {
        set opcode [lindex $op 0]
        set arg [lindex $op 1]

        switch $opcode {
            push {
                incr depth
            }
            var {
                incr depth
            }
            call {
                if {[is_primitive $arg]} {
                    # Apply stack effect of primitive
                    switch $arg {
                        dup - over - tuck {
                            # needs 1 (or 2 for over/tuck), adds 1
                            set need [expr {$arg eq "dup" ? 1 : 2}]
                            if {$depth < $need} {
                                set min_depth [expr {max($min_depth, $need - $depth)}]
                            }
                            incr depth
                        }
                        2dup {
                            if {$depth < 2} {
                                set min_depth [expr {max($min_depth, 2 - $depth)}]
                            }
                            incr depth 2
                        }
                        drop - emit - . - h. {
                            if {$depth < 1} {
                                set min_depth [expr {max($min_depth, 1 - $depth)}]
                            }
                            incr depth -1
                        }
                        2drop {
                            if {$depth < 2} {
                                set min_depth [expr {max($min_depth, 2 - $depth)}]
                            }
                            incr depth -2
                        }
                        swap - nip - + - - - * - / - mod - and - or - xor - = - <> - < - > - <= - >= - min - max - u< {
                            if {$depth < 2} {
                                set min_depth [expr {max($min_depth, 2 - $depth)}]
                            }
                            incr depth -1
                        }
                        rot - -rot {
                            if {$depth < 3} {
                                set min_depth [expr {max($min_depth, 3 - $depth)}]
                            }
                        }
                        2swap {
                            if {$depth < 4} {
                                set min_depth [expr {max($min_depth, 4 - $depth)}]
                            }
                        }
                        /mod {
                            if {$depth < 2} {
                                set min_depth [expr {max($min_depth, 2 - $depth)}]
                            }
                        }
                        negate - abs - 2* - 2/ - not - 0< - 0= - @ {
                            if {$depth < 1} {
                                set min_depth [expr {max($min_depth, 1 - $depth)}]
                            }
                        }
                        ! - +! - c! {
                            if {$depth < 2} {
                                set min_depth [expr {max($min_depth, 2 - $depth)}]
                            }
                            incr depth -2
                        }
                        key - i - j {
                            incr depth
                        }
                        cr - space {
                            # No stack effect
                        }
                        type {
                            if {$depth < 2} {
                                set min_depth [expr {max($min_depth, 2 - $depth)}]
                            }
                            incr depth -2
                        }
                    }
                } else {
                    # User-defined word - assume needs 1 for safety
                    if {$depth < 1} {
                        set min_depth [expr {max($min_depth, 1 - $depth)}]
                    }
                }
            }
            if_start - if_neg_start - while - until {
                if {$depth < 1} {
                    set min_depth [expr {max($min_depth, 1 - $depth)}]
                }
                incr depth -1
            }
            for_start {
                if {$depth < 1} {
                    set min_depth [expr {max($min_depth, 1 - $depth)}]
                }
                incr depth -1
            }
        }
    }

    return $min_depth
}

#############################################################################
# Main Compiler
#############################################################################

proc compile {input_file spec_file body_file} {
    # Read input
    set f [open $input_file rb]
    set source [read $f]
    close $f

    # Lex
    set tokens [lex_fifth $source]
    puts "Lexed [llength $tokens] tokens"

    # Parse
    lassign [parse_tokens $tokens] definitions variables
    puts "Parsed [llength $definitions] definitions, [llength $variables] variables"

    # Analyze
    lassign [analyze_words $definitions] has_io has_memory

    # Determine package name from output file
    set package_name [file rootname [file tail $spec_file]]
    set package_name [string totitle [string map {- _} $package_name]]

    # Generate spec
    set spec [spark_header $package_name $has_io $has_memory]

    # Add variable declarations
    foreach var $variables {
        set var_name [string toupper $var]
        append spec "   ${var_name}_Addr : constant Cell := [incr ::var_addr];\n"
    }
    if {[llength $variables] > 0} {
        append spec "\n"
    }

    foreach def $definitions {
        set name [lindex $def 0]
        set body [lindex $def 1]
        set min_stack [compute_min_stack $body]
        append spec [spark_spec_word $name $min_stack]
    }
    append spec [spark_footer $package_name]

    # Generate body
    set body_code [spark_body_header $package_name $has_io $has_memory]

    foreach def $definitions {
        set name [lindex $def 0]
        set ops [lindex $def 1]
        append body_code [spark_body_word $name $ops]
    }
    append body_code [spark_body_footer $package_name]

    # Write outputs
    set f [open $spec_file w]
    puts -nonewline $f $spec
    close $f
    puts "Wrote $spec_file"

    set f [open $body_file w]
    puts -nonewline $f $body_code
    close $f
    puts "Wrote $body_file"
}

# Variable address counter
set ::var_addr 0

#############################################################################
# Entry Point
#############################################################################

proc usage {} {
    puts "Fifth - ColorFORTH to SPARK Compiler v$::VERSION"
    puts ""
    puts "Usage: fifth.tcl <source.fifth> <output.ads> <output.adb>"
    puts ""
    puts "Compiles Fifth/ColorFORTH source to verified SPARK code."
    puts ""
    puts "Input format (.fifth):"
    puts "  🟥 word  - Define word (red)"
    puts "  🟩 code  - Compile (green)"
    puts "  🟨 code  - Execute (yellow)"
    puts "  ⬜ num   - Number (white)"
    puts "  🟦 var   - Variable (blue)"
    puts "  💬 text  - Comment (cyan)"
    puts ""
    puts "Example:"
    puts "  🟥 square 🟩 dup * ;"
    puts "  🟥 test ⬜ 5 🟩 square . cr ;"
}

proc main {args} {
    if {[llength $args] != 3} {
        usage
        exit 1
    }

    lassign $args input spec body

    if {![file exists $input]} {
        puts "Error: Input file not found: $input"
        exit 1
    }

    compile $input $spec $body
    puts "Done."
}

main {*}$argv
