Modes
================================================================================

When processing construct code, the mode affects how statements are handled.
In the default mode, the statement handler checks that every statement
evaluates to null, otherwise, it throws an error.
```
let a 1; // OK
let b 2  // ERROR: unhandled statement value 'b'
3        // ERROR: unhandled statement value '4'
```
However this mode can be changed. Let's say you wanted every statement to
be printed and evaluated as a number, like a calculator:
```
defmode calculator source result
{
  if result == null {
    throw "in calculator mode, every statement must evaluate to a number";
  }
  if result is number {
    message source;
    message "    = " result;
  } else {
    throw "in calculator mode, every statement must evalulate to a number, but got " result;
  }
}
changeMode calculator
1
2+3
"Hello, World!".length
```
Output:
```
1
    = 1
2 + 3
    = 5
"Hello, World!" . length
    = 13
```
The default mode would look somethig like this:
```
defmode default source result {
  if result != null {
    throw "unhandled statement value " result;
  }
}
```

--------------------------------------------------------------------------------
For another example, let's suppose you wanted your construct code to represent
an assembly language.
```
//
// Some classes to support the assembly language...
//
class AssemblyInstruction
{
}
class MovInstruction : AssemblyInstruction
{
  Register dst;
  Number src;
}
enum Register
{
  ax, bx, ...
}
defcon ax() { return Register.ax }
defcon mov (register Register, value number)
{
  return MovInstruction(register, value);
}

//
// The mode definition
//
let instructions builderOf AssemblyInstruction;
defmode assembly source result
{
  if(result == null) {
    throw "in assembly mode, every statement must evaluate to an AssemblyInstruction";
  }
  if(result is AssemblyInstruction) {
    instructions.add(result);    
  } else {
    throw "in assembly mode, every statement must evaluate to an AssemblyInstruction, but got " result;
  }

changeMode assembly
mov ax 10
mov ax 100

changeMode default
// Now the instructions variable will
// contain the assembly that was just written.

// You can also execute a mode within a block like this:

blockMode assembly
{
  mov ax 100
  mov ax 0
}

// Now instructions will contain the instructions inside that block
```


