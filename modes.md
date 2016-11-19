The Statement Mode
================================================================================
When processing construct code, the "statement mode" affects how statements are
handled. The default statement mode enforces that all statements evaluate to
to null, otherwise, it throws an error.
```
let a 1; // OK
let b 2  // ERROR: unhandled statement value 'b'
3        // ERROR: unhandled statement value '4'
```

The application can also define new statement modes using the 'defStatementMode'
construct. Let's say you wanted every statement to be printed and evaluated as
a number, like a calculator:
```
defStatementMode calculator source result
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
setStatementMode calculator
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
The default statement mode would look somethig like this:
```
defStatementMode default source result {
  if result != null {
    throw "unhandled statement value " result;
  }
}
```

Note: statement mode only persists inside the block it was set, i.e.
```
// current statement mode is "default"
{
  setStatementMode calculator
  //assert getStatementMode == "calculator"
  // current statement mode "calculator"
}
// current statement mode is back to "default"
//assert getStatementMode == "default"
```

Another Example
--------------------------------------------------------------------------------
Let's suppose you wanted your construct code to represent an assembly language.
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


