# HAM-ASM

The programm ham-asm is a assembler for the HAM-Assembler of the University
Osnabrück. The assembly language is used in the course 
"Einführung in die Technische Informatik" of the university and the
corresponding CPU is build over the duration of the course.

## Compiling
To compile the project you will need the programm `stack` installed on your
system. Then just execute the command `stack build ham-asm:ham-asm`. It will
build the executable. To install it for usage, execute 
`stack install ham-asm:ham-asm`. This command will build the executable and put
it into `~/.local/bin/`. If the directory is in your PATH, the command `ham-asm`
will then be usable in the shell.

## Usage
To assemble a given source file (ending in ".asm") use the command `ham-asm
FILEPATH`. You will find the assembled programm in a file with the same name,
but the extension `.hex`. It can be used in Digital to be loaded into the RAM of 
the constructed CPU.

If the flag `-d` or `--debug` is passed, a addtional file, ending in `.txt` will
be generated. There the generated machine code, the address and the given
assembler expressions are shown.

## HAM Syntax
The syntax of this assembly language is case-insensitive.
Values and instructions only use hexadecimal digits.

### Instructions
At the moment it supports 9 instruction:

| Instruction | What it does                                                                                                                                     |
|-------------|--------------------------------------------------------------------------------------------------------------------------------------------------|
| LOAD        | Load the value at the given address.                                                                                                             |
| STORE       | Store the value at the given address.                                                                                                            |
| ADD         | Adds the value at the given address to the currently loaded value.                                                                               |
| AND         | Bitwise and of the currently loaded value and the value at the given address.                                                                    |
| JUMP        | Jumps to the given address.                                                                                                                      |
| JUMPZ       | Jumps to the given address, when the currently loaded address is zero.                                                                           |
| INV         | Inverts the currently loaded value.                                                                                                              |
| RSHIFT      | Ringshift to the left of the currently loaded value.                                                                                             |
| ADDI        | Adds the given value to the currently loaded value. The MSB of the given value is propagated to the left, to expand the value into a 16 bit one. |

Furthermore the assembler understands the following: Lables, Values and Comments.

### Labels

A label marks the given instruction and replaces it in instructions using it
with the corresponding address.

Example:
```
test: load 0
jump test
```

### Values

Values are like labels, but are not folled by an instruction. Instead a hex
value follows. 

Example:
```
load val1
add val2

end: jump end

val1 = 1
val2 = 2
```

### Comments

Comments are everything in the file, which does not correspond to an
instruction, a label or a value.

Example:

```
This is a comment.
A comment can follow an instruction, a label or a value.
load: load 0 This is a comment to 
```

## Functional example
An example of the syntax can be found in the following code. It calculates the
fibonnacci sequence. `cnt` denotes the iteration which should be shown.

```
-- Special cases for fib(0)
-- Check if the counter is 0
load  cnt   -- Load cnt
jumpz end  -- when 0, jump to end

calcFib: load  n0       -- add n0 and n1 and store it
         add   n1
         store tmp      
         load  n1       -- move  n1 to n0 and current to n1
         store n0       -- for use in next iteration
         load  tmp
         store n1

-- Check if the current iteration is the last one.
load   cnt -- Load current CNT
add    mone -- Substract one
store  cnt -- Store the CNT
jumpz  end -- If it's zero, we are done
jump   calcFib -- Else we calculate the next one

end:     load n0
display: jump display

-- Values
n0   = 0
n1   = 1
tmp  = 0
mone = ffff
cnt  = 8    -- display the eigth fibonnaci number.
```

## Contribution
I welcome contributions, as I am not a very good Haskell programmer. I saw this
project as a chance to try it out on something a bit bigger the learning
exercises.

If you have feedback or ideas to make the code better, just open an issue, send
me a mail or open a pull request. I would appreciate it.

### TODO
- Better error messages
- Code Cleanups (this project was written by a Haskell newbie)

### Bug Reports
If you want to report a bug, please open an issue with the following content.

- The version you are using.
- What should happen
- What did happen
- A `.asm` file containing just enough code to reproduce the errors.
- The corresponding debugging file (the one with the extension `.txt`)

If you don't follow these simple instructions, I'll probably still look into the
bug, but it will definitly take me longer to fix it. 
