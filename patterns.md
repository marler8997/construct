
Patterns
================================================================================

Limits of the symbol object
--------------------------------------------------------------------------------
A symbol object cannot appear in a branch within a pattern. In other words, when
the pattern matcher can match a symbol, it CAN ONLY match a symbol and nothing
else. The following examples demonstrate where a symbol CAN and CANNOT exist in
a pattern:
```
// The symbol can only be the default count type (one)
pattern(s           symbol, ...) // OK
pattern(s optional  symbol, ...) // ERROR
pattern(s many      symbol, ...) // ERROR
pattern(s oneOrMore symbol, ...) // ERROR

// A symbol cannot appear after non default count object
pattern(..., x          , s symbol, ...) // OK
pattern(..., x optional , s symbol, ...) // ERROR
pattern(..., x many     , s symbol, ...) // ERROR
pattern(..., x oneOrMore, s symbol, ...) // ERROR
```

The Symbol Context
--------------------------------------------------------------------------------
The "symbol context" applies whenever a pattern expects a symbol, i.e.
```
(s symbol, ...)
```
Note this only applies when the pattern expects a general symbol, not a
single symbol value like this,
```
(s "someSymbol") // does not apply since this is a single symbol value
```
When processing construct objects, whenever a symbol is encountered, it is
looked up in the symbol table, however, in the symbol context this lookup is
bypassed and the symbol object is processed as it exists. For example, the
`defcon` construct consumes a symbol as its first argument:
```C
defcon someSymbol
//     |--------|
//         |
//     This is where the symbol context applies
```
Instead of looking up `someSymbol` in the symbol table, it gets passed to the
`defcon` construct as it exists.

Using `sym` to escape the Symbol Context
--------------------------------------------------------------------------------
There are times when the developer may want to use a symbol based on a construct
expression.  Say you wanted to define a construct whose name is a string:
```C
// assume 'name' is a string
defcon name
```
This won't work because `name` appears in a symbol context, so the fact that
it's already defined as a string is ignored and a new construct 'name' is added
to the symbol table shadowing the current entry.  This is where the `sym`
keyword comes in.  The `sym` keyword is used to escape the symbol context and
return to normal construct evaluation.  `sym` will take any expression that
evaluates to a string or a symbol, and return it as a symbol as if it were
written in the original symbol context. So if we use it in the previous example:
```C
// assume 'name' is a string again
defcon sym name
```
This time, the `sym` keyword causes `name` to be looked up.  Since it's defined
as a string, the `sym` keyword will transform it to a symbol and now we've
defined a new construct whose name was derived from a string. If the `name`
variable was set to "myCoolConstruct" then this would have been equivalent to
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

let postfix "SomePostfix";

defcon sym append "myCoolFunc" postfix() {}
// equivalent to defcon myCoolFuncSomePostfix
```
