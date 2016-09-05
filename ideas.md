
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

I would like the dot operator to work, but not sure how yet.
```
let fruits ("apple" "banana" "orange")
message "I have " fruits.length " fruits";

// try to find an example that make the dot operator ambiguous

something.length // works


constructA constructB . name

constructA (constructB.name)
OR
(constructA constructB).name

One idea, the dot operator has higher precedence than a construct.
So that would mean this one:

constructA (constructB.name)

I think I need to change the semantics of parenthesis a little.
Instead of using them to delimit lists, maybe they should be used
to indicate to the compiler about execution precedence?

So to indicate the other way, you would write

(constructA constructB).name


What about construct patterns?
pattern aValue
pattern (aValue)
pattern aValue string
pattern (aValue string)
pattern aValue string, anotherValue string
pattern (aValue string, anotherValue string)

With the order of execution semantics, you could do this:

(pattern aValue)

What about other operators?

plusplus?

3 plus 2 plusplus?


I think it has to work like this. Every time you go to match a value to a construct, you need
to look at the next object.

3 ?

If the next object takes an object on it's left side, you have to evaluate it immediately, unless,
it's outside a paranthesis boundary...or I could use another boundary?

```

If I do this, then the operations are going to have to have a precendence, picture this:
```
a.b.c
```

The first '.' should have higher priority then the second '.' because it should be evaluated left to right:
```
(a.b).c
```

But if you mixed different operations say this:
```
a+b.c
```
Then this MUST BE:
```
a+(b.c)
```
Which isn't left to right.  This can only be performed correctly if you know that
the '.' operator has a higher precedence than the '+' operator.  So when the '.'
operator is defined, it must be stated in some way:
```
opPrecedence . higherThan *;
opPrecedence * higherThan +;
```


So every time a value is processed, there must be 2 precedences, the current
precendence, and the precedence of a potential operation. i.e.
```
a+b*c // a+(b*c)
a.b*c // (a.b)*c
a*b.c // a*(b.c)
```

What about defining the operations? Similar to defcons:
```
defop . (value, nameless "type")
{
  // returns the type of value
}
// allows somevalue.type

defop . (s string, nameless "byteLength")
{
  // adds the ".byteLength" property to strings
}

```


### Construct and Operator Precedence

Precedence can be represented by railroad diagrams.
In order to be comparable, two operators must be connected
by a rail that doesn't switch directions.

A can be compared to B:
```
( A )---(...)           (...)---( B )
           \             /
            \---(...)---/
```

A cannot be compared to B:
```
( A )---(...)   ( B )---(...)
           \             /
            \---(...)---/
```

These rails are created with a sequence of "higherThan" and "lowerThan" constructs:
```
precedenceGroup summation + -;
precedenceGroup multipler * /;
precedence multipler higherThan summation;

// ERRORS:
precedence * higherThan summation; // ERROR: * is already part of a precedenceGroup 'multipler'
precedence multipler higherThan +; // ERROR: + is already part of a precedenceGroup 'summation'

precedence . higherThan multipler;
precedence construct higherThan .;

// ( + - )---( * / )---( . )---(construct)
```
Say the operator modulus `%` is higher than summation,
but not int the same group as `*` and `/`:
```
precedence % higherThan summation;
//
// ( + - )---( * / )---( . )---(construct)
//      \
//       \---( % )
//
```
Now if you saw `a.b%c`, you will get an error because
the `%` operator is not comparable with the `.` operator.

But now let's put it in the same tree:
```
precedence % lowerThan .;
//
// ( + - )---( * / )---( . )---(construct)
//      \               /
//       \---( % )-----/
//
```
Now `%` can be compared to everything but `.` and `construct`.
If you would like `%` to be comparable to `%`, you could use this construct:
```
addToPrecedenceGroup multipler %;
// ( + - )---( * / % )---( . )---(construct)
```

Implicit Cast Operators
--------------------------------
Provide a way to write implicit cast operators.

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




