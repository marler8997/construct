


Current Ideas
===========================================================
Every construct begins with the name of the construct.
This is to prevent ambiguity.  Say you had the construct
called "not" and the construct called "inc", where
"not" expects an arguments after the construct, and "inc"
expects the argument before:
```
not <arg>
<arg> inc
```
If you saw
```
not 2 inc
```
You wouldn't know if it was (not 2) inc or not (2 inc).

You could say that it always evaulates left to right, or
right to left, but I'm not sure the added complexity of this
is worth the benifit.  So for now, all constructs must
begin with the name of the constructs.  That's not to say
that a construct has to be alpha-numberic characters. Maybe
symbols like "+" or "/" could be constructs themselves, but
for now, but syntax has no way to parse represent these special characters.

By assuming that all constructs start with the construct name, you can
easily determine the order of operations (todo: come up with examples)

#### The Dot Operattor




The semi-colon
--------------------------------
Some constructs require a semi-colon to terminate, however some do not.
The ones that do not require a semi-colon, will return a value. i.e.
```
  set x 3
```
This returns the symbol x, unless you terminate it with a semicolon
```
  set x 3
```

Construct Patterns
--------------------------------

A construct consumes objects based on a pattern definition.
The pattern definition determines how construct objects are consumed
and also defines the interface to the construct implementation.



```
// defcon import (names string ...) requiresBreak=true;
// The 'import' construct takes one or more strings, followed by a semi-colon.
// It also returns nothing.
import-pattern ::= string* break

// the 'let' construct: grammar definition
let-pattern ::= symbol object break?

// the 'message' construct grammar definition
message-pattern ::= anything* break

// the 'try' construct grammar definition
try-pattern :: block ('catch' block)? ('finally' block)?

// the 'return' construct
return-patter :: value? break



defcon defcon (pattern list null) requiresBreak=true;
// A defcon with no code block implementation requires a terminating semi-colon break.
// If no params list is specified, the values is null

defcon defcon (pattern list null, implementation constructBlock) noBreak=true;
// This version includes an implementation and must not have a terminating semi-colon break.
// If no params list is specified, the values is null

defcon exec (code constructBlock);

//defcon deftype
```


The Symbol Context
================================================================================
The "symbol context" applies whenever a construct expects a symbol.  For
example, the 'defcon' construct always expects a symbol immediately afterwards:
```C
defcon someSymbol
//     |--------|
//         |
//     This is where the symbol context applies
```
Normally, whenever a symbol appears, it is looked up in the symbol table, but
in the symbol context, this lookup is not done and whatever symbol appears
is used.

However, there are times when the developer may want to use a symbol based on
a construct expression.  Say you wanted to define a construct based on some
value.
```C
// assume 'name' is a string
defcon name
```
This won't work because name appears in a symbol context, so the fact that it's
already defined as a string is ignored and a new construct named 'name' is added
to the symbol table shadowing the current entry.  This is where the 'sym' keyword
comes in.  The sym keyword is used to escape the symbol context and go back to
normal construct evaluation.  'sym' will take any expression that evaluates to
a string or a symbol, and return it as a symbol as if it were written in the
original symbol context. So if we use it in the previous example:
```C
// assume 'name' is a string again
defcon sym name
```
This time, the sym keyword caused name to be looked up.  Since it evaluates to
a string, the sym keyword will transform it to a symbol and now we've defined
a new construct whose name was derived from a string. If the name variable was
set to "myCoolConstruct" then this would have been equivalent to
```C
defcon myCoolConstruct
```

### Examples of using the sym keyword
```C
defcon setTo10(x symbol) {
  set sym x 10;
}

let myvar 0;
message "x is: " x;
setTo10 myvar
message "x is: " x;

// Output:
// x is: 0
// x is: 10


//

let postfix "SomePostfix";

defcon sym append "myCoolFunc" postfix() {}
// equivalent to defcon myCoolFuncSomePostfix
```



Pattern Consumers:
--------------------------------------------------
Consume objects if they match the pattern.


The '+' construct
  number '+' number
  pointer '+' number
  pointer '+' pointer

The '/' construct
  number '/' number


8 + 4 / 2
8 / 4 + 2

The '++' construct
  number '++'
  pointer '++'
  



The 'message' construct
  'message' anything* ';'





Pattern Branches
-------------------------------------
```
#### let construt
symbol value break?

symbol value break
symbol value

defcon let (newSymbol symbol, value, optional break)
{
    // code to add symbol
    
}

#### struct construct

defcon struct block
{
  // ...
}

#### try construct

Expression: block ('catch' (symbol type?)? block)* ('finally' block)?

struct CatchInfo {
  errorSymbol symbol;
  errorType type;
  handler block;
}

defcon try (tryBlock block, catchBlocks CatchInfo list itemPrefix="catch", finallyBlock block prefix="finally")
{
  // ...   
}

defcon <name> (<consumer>)

<consumer> = <name> [<type>] [options]

consumer option: itemPrefix=<string>
    The presence of <string> means another item is about to appear
consumer option: prefix=<string>
    The argument is only present if prefixed by <string>




// Pattern
  Type (some examples):
    anything named? nullable? type symbol bool pointer
    number integer signed int byte unsigned uint ubyte
    uni construct block list array string ascii unicode
    utf8
  CountType:
    one (must appear once)
    optional (may or may not appear)
      prefix (presence may be dependent on a prefix keyword)
    list
      prefix (The list as a whole is present based on a keyword)
      itemPrefix (each item has the given prefix)



try {
  // some code
} catch e MyCoolException {
  // the catch code
} catch e AnotherException {
  // more catch code
} finally {
  // the finally code
}

```


Pattern State Machines
================================================================================
Given that you have every pattern for a construct, construct a state machine
that will consume that construct.

defcon let (name symbol, value object)

let catchPattern pattern(varName symbol, catchType type optional, catchBlock block);

defcon try (tryBlock block, catchBlocks catchPattern list itemPrefix='catch', finallyBlock block option prefix='finally')

### How to create a state machine from a pattern

nodes and transitions

start with a node:

pattern (s string)




