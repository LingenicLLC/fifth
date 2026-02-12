# CHROMA

Convert between ColorFORTH binary (`.cf`) and Fifth text (`.fifth`) formats.

## Usage

```
tclsh chroma.tcl input.fifth output.cf
tclsh chroma.tcl input.cf output.fifth
```

## Requirements

- Tcl 8.5+

## The .fifth Format

Fifth represents ColorFORTH's color-encoded tokens as emoji:

| Emoji | Color | Meaning |
|-------|-------|---------|
| 🟥 | Red | Define word |
| 🟨 | Yellow | Execute |
| 🟩 | Green | Compile |
| ⬜ | White | Number |
| 🟦 | Blue | Variable |
| 💬 | Cyan | Comment |

Color persists until the next marker. Space required after each emoji.

### Example

```
🟥 square 🟩 dup * ;
⬜ 5 🟨 square 💬 result: 25
```

## ColorFORTH Binary Format

Each 32-bit word contains:
- **Bits 3-0**: Tag (color/type)
- **Bits 31-4**: Shannon-coded characters (28 bits)

### Character Encoding

Variable-length Shannon codes based on frequency:

| Bits | Characters |
|------|------------|
| 4-bit | ` ` `r` `t` `o` `e` `a` `n` `i` |
| 5-bit | `s` `m` `c` `y` `l` `g` `f` `w` |
| 7-bit | `d` `v` `p` `b` `h` `x` `u` `q` `0-9` `j` `-` `k` `.` `z` `/` `;` `:` `!` `+` `@` `*` `,` `?` |

### Tag Values

| Tag | Color | Meaning |
|-----|-------|---------|
| 0 | — | Extension (continues previous word) |
| 1, 3 | Yellow | Execute |
| 4 | Red | Define |
| 5, 6, 7 | Green | Compile |
| 8 | White | Number |
| 9, 10, 11 | Cyan | Comment |
| 12 | Blue | Variable |

### Long Words

Words exceeding 28 bits use extension words (tag 0). The converter handles this automatically.

## Examples

```bash
# Convert .fifth to ColorFORTH binary
tclsh chroma.tcl examples/square.fifth examples/square.cf

# Convert back to .fifth
tclsh chroma.tcl examples/square.cf examples/output.fifth
```

## References

- [ColorFORTH](https://colorforth.github.io/cf.htm)
- [Character Encoding](https://colorforth.github.io/chars.html)
- [Compression](https://colorforth.github.io/compress.htm)

## License

Apache 2.0
