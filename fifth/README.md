# Fifth Compiler

ColorFORTH to SPARK compiler. **Alpha.**

## Overview

Compiles ColorFORTH source to verified SPARK/Ada code. Input is the `.fifth` text format—ColorFORTH with colors represented as emoji markers. Output includes contracts verifiable with GNATprove.

## Requirements

- Tcl 8.5+
- GNAT/SPARK (for compiling/verifying output)

## Usage

```bash
tclsh fifth.tcl <source.fifth> <output.ads> <output.adb>
```

Example:
```bash
tclsh fifth.tcl examples/square.fifth examples/square.ads examples/square.adb
```

## .fifth Format

ColorFORTH uses color to encode meaning. The `.fifth` format represents these colors as emoji markers:

| Emoji | Color  | Meaning              |
|-------|--------|----------------------|
| 🟥    | Red    | Define word          |
| 🟨    | Yellow | Execute immediately  |
| 🟩    | Green  | Compile              |
| ⬜    | White  | Number literal       |
| 🟦    | Blue   | Variable             |
| 💬    | Cyan   | Comment              |

Colors persist until the next marker. Space required after emoji.

### Example

```
🟥 square 🟩 dup * ;
🟥 test ⬜ 5 🟩 square . cr ;
```

This defines:
- `square`: duplicates top of stack, multiplies
- `test`: pushes 5, squares it, prints result

## Primitives

### Stack Operations
| Word | Effect | Description |
|------|--------|-------------|
| `dup` | ( n -- n n ) | Duplicate top |
| `drop` | ( n -- ) | Discard top |
| `swap` | ( a b -- b a ) | Swap top two |
| `over` | ( a b -- a b a ) | Copy second to top |
| `nip` | ( a b -- b ) | Drop second |
| `tuck` | ( a b -- b a b ) | Copy top below second |
| `rot` | ( a b c -- b c a ) | Rotate three |
| `-rot` | ( a b c -- c a b ) | Reverse rotate |
| `2dup` | ( a b -- a b a b ) | Duplicate pair |
| `2drop` | ( a b -- ) | Drop pair |
| `2swap` | ( a b c d -- c d a b ) | Swap pairs |

### Arithmetic
| Word | Effect | Description |
|------|--------|-------------|
| `+` | ( a b -- a+b ) | Add |
| `-` | ( a b -- a-b ) | Subtract |
| `*` | ( a b -- a*b ) | Multiply |
| `/` | ( a b -- a/b ) | Divide |
| `mod` | ( a b -- a mod b ) | Modulo |
| `/mod` | ( a b -- rem quot ) | Divide with remainder |
| `negate` | ( n -- -n ) | Negate |
| `abs` | ( n -- |n| ) | Absolute value |
| `min` | ( a b -- min ) | Minimum |
| `max` | ( a b -- max ) | Maximum |
| `2*` | ( n -- n*2 ) | Double |
| `2/` | ( n -- n/2 ) | Halve |

### Logic (Bitwise)
| Word | Effect | Description |
|------|--------|-------------|
| `and` | ( a b -- a&b ) | Bitwise AND |
| `or` | ( a b -- a\|b ) | Bitwise OR |
| `xor` | ( a b -- a^b ) | Bitwise XOR |
| `not` | ( n -- ~n ) | Bitwise NOT |

### Comparison
| Word | Effect | Description |
|------|--------|-------------|
| `=` | ( a b -- flag ) | Equal |
| `<>` | ( a b -- flag ) | Not equal |
| `<` | ( a b -- flag ) | Less than |
| `>` | ( a b -- flag ) | Greater than |
| `<=` | ( a b -- flag ) | Less or equal |
| `>=` | ( a b -- flag ) | Greater or equal |
| `0<` | ( n -- flag ) | Negative? |
| `0=` | ( n -- flag ) | Zero? |
| `u<` | ( a b -- flag ) | Unsigned less than |

Flags: -1 = true, 0 = false

### I/O
| Word | Effect | Description |
|------|--------|-------------|
| `.` | ( n -- ) | Print number |
| `h.` | ( n -- ) | Print as hex |
| `emit` | ( c -- ) | Print character |
| `key` | ( -- c ) | Read character |
| `cr` | ( -- ) | Newline |
| `space` | ( -- ) | Print space |

### Memory
| Word | Effect | Description |
|------|--------|-------------|
| `@` | ( addr -- n ) | Fetch |
| `!` | ( n addr -- ) | Store |
| `+!` | ( n addr -- ) | Add to memory |
| `c@` | ( addr -- c ) | Fetch byte |
| `c!` | ( c addr -- ) | Store byte |

## Control Flow

### Conditionals

```
🟩 condition if true-part then
🟩 condition if true-part else false-part then
🟩 condition -if negative-part then
```

`if` executes if top is non-zero. `-if` executes if top is negative.

### Counted Loops

```
🟩 count for body next
```

Executes `body` count times. Use `i` to get current index (0 to count-1).

### Indefinite Loops

```
🟩 begin body condition until
🟩 begin condition while body repeat
```

`until` exits when condition is true. `while` exits when condition is false.

## Generated Code

The compiler generates SPARK packages with:

- **Spec (.ads)**: Type definitions, procedure declarations with Pre contracts
- **Body (.adb)**: Procedure implementations

### Stack Type

```ada
type Stack_Type is record
   Data : Stack_Array;  -- 256 cells
   SP   : Stack_Index;  -- 0..256
end record;
```

### Contracts

Each procedure includes a `Pre` contract specifying minimum stack depth:

```ada
procedure Square (S : in out Stack_Type)
  with Pre => S.SP >= 1;  -- needs 1 element
```

## Examples

### Square (examples/square.fifth)

```
🟥 square 🟩 dup * ;
🟥 test ⬜ 5 🟩 square . cr ;
```

### Countdown (examples/countdown.fifth)

```
🟥 countdown 🟩 for i . space next cr ;
🟥 test ⬜ 10 🟩 countdown ;
```

### Absolute Value

```
🟥 abs 🟩 dup 0< if negate then ;
```

### Maximum of Two

```
🟥 max2 🟩 2dup < if swap then drop ;
```

## Verification

To verify generated code with GNATprove:

```bash
gnatprove -P project.gpr --mode=prove
```

## SafeTcl Style

The compiler follows SafeTcl conventions:
- No `eval`, `uplevel`, `upvar`
- No dynamic code generation
- Pure procedures with explicit data flow

## License

Apache 2.0

## See Also

- [Fifth Language](https://fifthlang.org)
- [ColorFORTH](https://colorforth.github.io)
- [SPARK](https://docs.adacore.com/spark2014-docs/html/ug/)
