What is the Construct Programming Language
===================================================
Construct is a new programming language that does some things very differently from previous languages.
- Has a very **verbose** and simple syntax
- Seperates program logic from code
- Provides mechanisms for applications to extend the semantics of the language

Construct will have a core set of semantics that all other semantics must reduce to.  Adding semantics to the language is an excercise in translating one set of semantics into another existing set.

## Simple Syntax
Construct uses an extremely simple syntax that requires more text but allows infinite customization.  A rich syntax allows programmers to say what they want in a small number of characters.  A good syntax is extremely powerful, however, as semantics change through new libraries and new techniques, the syntax gets left behind as it cannot be changed without breaking existing code or creating a mess with syntax that looks "hacked on".

The construct syntax sacrifices brevity for flexibility.  It's verbosity is designed to allow it to adhere to any set of new semantics.  This allows the semantics to be modified and extended without having to figure out how to add the new semantics to the language.

> Old: This langauge attempts to decouple the language syntax from the semantics. It accomplishes this by defining the semantics using a very simple syntax and giving the job of creating a syntax up to other parts of the system.  This is different from other languages which have a the syntax tightly coupled with the semantics.  This is restrictive since the syntax may make it difficult to add to the semantics.

# The Construct Language

The language is made up of units called **constructs**.

### What can a construct do
- Inject code
- Modify all code within a certain context
- Create another construct
- Basically anything you can think of

Every construct has a name. Use the **construct** keyword to define one.
```
construct <name>
```

Hello World
```
writeln("Hello, World!");
```

```
construct write
{
  inputs {
    format : string,
    args : ...,
    appendNewline : bool = false,
  }
}
construct writeln
{
  // Forward the call the write construct with the specified option
  forward(write, appendNewline=true)
}


```







Let's look at the standard **set** construct.
```
construct set
```

Example:
```
set (a b)
```



# The Construct Language (This is old)
Start by defining the primitive structures:

(<name> <required-args>... (operation <args>))

<type>
--------------------------------------------------
...

<return-types>
--------------------------------------------------
<type>
(<type>...)

decl construct
--------------------------------------------------
decl <options> <type> <name> <value>

set construct
--------------------------------------------------
set <targets> <sources>

function construct
--------------------------------------------------
signature     : function <return-types> <name> (<arg-type> <arg-name>...) <options>;
implementation: function <return-types> <name> (<arg-type> <arg-name>...) <options> { <implementation> }

if construct
--------------------------------------------------
if <condition> <options> { <body> }
if <condition> <options> { <body> } else <options> { <body> }
if <condition> <options> { <body> } else if <condition> <options> { <body> } else { <body> }

call construct
--------------------------------------------------
call <function-name> <options> [(<outputs>...)](<values>...)

return construct
--------------------------------------------------
return <options> <expression>



string literal
--------------------------------------------------
\n (newline character)
\r (carriage return)
\N (the system newline string either "\r\n" or "\n")



Used to attach a comment to things
(comment <comment-string> <things>...)
(comment <comment-string> <things>...)

Comment on the next thing only
/* <comment-string> */ <thing>

// <comment-string> NEWLINE <thing>

(assign <destination> <source>)
(allocate <size> <name>)
(decl <type> <name>)

(program <instructions>...)



A function is a block of code that operates on a stack allocated context.
(function <input-args> <output-args> (block <body>))


(scope (vars <vars>...) <statements>)

(call <function-name>)

Types:

Bit Group (Each type is 1 bit)
--------------------------------------------------
bool       | boolean (true/false)

Byte Group (Each type is 1 byte or 8 bits)
--------------------------------------------------
byte       | generic signed byte
ubyte      | generic unsigned byte
utf8       | all or part of a UTF8 encoded character
ascii      | ascii character

Short Group (Each type is 2 bytes or 16 bits)
--------------------------------------------------
short      | generic signed short
ushort     | generic unsigned short
utf16      | UTF16 encoded character

Word Group (Each type is the same number of bytes as a word (pointer))
--------------------------------------------------
sword      | generic signed word
uword      | generic unsigned word

TypeModifiers:

^ <type>   | pointer to <type>
[l] <type> | length-array of <type> struct{^<type> ptr;uword length}
[e] <type> | end-array of <type> struct{^<type> ptr;^<type> end}
[] <type>  | undefined-array of <type> (could be a 'length' or 'end' array)

Note: I should use 'auto' to say that the type should be automatic

(array-length <type>) OR (ar <type>)

// The Hello World Program
(console-program

(function main () (void) (body
	(call write "Hello, World!\N")
))

)

(console-program

(function main ( ((ar (ar ubyte)) args) ) (int) (body
	(call write "Hello, World!\N")
))

)



Idea: How to return stack allocated memory
	1. Instead of allocating the new function on the stack (The end of the normal stack but reversed), allocate it on the reverse stack, then it can allocate as much memory as it wants on the normal stack!  Or you could have multiple virtual stacks
Not every function needs to behave the same way.  If their stack memory can be computed at compile time, then the stack memory can be allocated by the caller.

Note: Structs/Classes/Objects are very similar to c program.  A C program has a set of globals which are equivalent to members in an object, and a C program has functions which are equivalent to object methods.  An object is sort of like a little C program, except it has multiple entry points that can be called at any time and maintain it's state.  A C program has one entry point and clears its state after the first function call.  It would be cool if I could use the same mechanism for an object for a program.  A console program is a special type of object that has one public method (the main function is the constructor and does not return an object reference but instead executes and then the object is cleaned up).

Note: some types of dlang version blocks (like BigEndian/LittleEndian) are grouped together...meaning that exactly one from the group must be defined.  The code should be able to do a final switch on those types of versions.
