# The Construct Language (This is old)

> These were my initial ideas on what the Construct programming language was going to be.
> They've since evolved but I keep this around to remember where it started.

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
