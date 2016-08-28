The Construct Programming Language
===================================================
Construct is a programmable programming language.
It starts with a set of basic data structures like string, numbers, symbols, lists and even code blocks.
Using these basic data structures, it provides the means to create and compile the basic unit of logic called the 'construct'.
A construct is a peice of code that consumes semantic objects during compilation.
There are a set of predefined constructs, but the code itself can add new constructs and extend existing ones.
Constructs are defined by a name, a pattern that consumes objects, and code to process those objects.
Let's look at an example:
```C
defcon sayHello () {
    message "Hello, World!";
}
```

As the compiler reads this code, it will identify the `defcon` symbol as a predefined construct.
The defcon construct has it's own pattern that will consume the rest of the code in this example.
The `sayHello` symbol is passed to defcon and will be used as the symbol name for the newly defined construct.
The parenthesis `()` include the pattern of objects that the `sayHello` construct will consume, since it
is empty, `sayHello` does not consume any objects.
The last part is the code block delimited by `{`curly braces`}`.
In this case, it simply prints "Hello, World!" to the user.
Executing this construct is a matter of writing the construct name as a symbol like this:
```C
sayHello
sayHello
// will print:
// Hello, World!
// Hello, World!
```

Now lets define another construct that does consume objects.
```C
defcon sayHelloTo(name string) {
    message "Hello, " name "!";
}
```
This construct includes the pattern `(name string)`. A pattern is a comma
separated list of items that indicate what the construct consumes.
In this case, it consumes one object called `name` that is of type `string`.
This construct can be called like this:
```C
sayHelloTo "George"
// will print:
// Hello, George!
```

## Custom Backends

What the constructs do depend on the libraries and the backend.
One backend may execute the constructs, one may emit machine code, another
may emit code in another programming language.

If you wanted to add a function definition and function call construct,
you could write a library that defines these constructs and the code
to intepret the semantics.  In fact this is what the std.procedural
library does.

It may or may not be clear that construct does things very differently from other
programming languages.  Construct moves all the semantic complexity of a program
from the language into libraries.  This allows new semantics to be utilized
without having to change the language.  On top of that, since construct
is defined by data structures instead of a syntax, a parser that generates
construct data structures is all that's required to support new syntax.

> The real power of construct comes from it's ability to adopt new semantics.

> Note: Construct is more than a programming language. It's more like
>       a programming language language.

> Note: Add a section about the problems that Construct solves with
>       programming langauge not being able to keep up with the times
>       because they are limited by their semantics, and even more so
>       by their syntax.

Since I haven't implemented a data structure library for construct, I'm going to represent the data structure definitions using pseudocode for now:
```
class ConstructObject {
  // ...
}
class Construct : ConstructObject {
  string name;
  ConstructObject[] params;
  ConstructObject[string] namedParams;
}
class ConstructString : ConstructObject {
  string value;
}
class ParamList : ConstructObject {
  ConstructObject[] params;
  ConstructObject[string] namedParams;
}

// Primitive Type Heirarchy
// Defined using C-Family Construct Syntax
type {
  pointer "a pointer whose bit-width is the standard platform bit-width"
  number {
    integer "any integral type" {
      int  "a signed number whose bit-width is the standard platform bit-width";
      byte "a signed number whose bit-width is 8";
      unsigned "any non-negative integral type" {
        uint  "an unsigned number whose bit-width is the standard platform bit-width";
        ubyte "an unsigned number whose bit-width is 8";
      }
    }
  }
  uni "A unicode character whose bit-width is 32";
  array {
    unicode "an array of unicode characters" {
      ascii "an array of type
      utf8 "an array of type ubyte that contains unicode characters encoded in UTF8";
    }
  }
}
```
> Note: I will want the define the construct data structures in construct at some point.

Concstruct does come with some standard syntaxes:

#### The C Family Syntax
A syntax meant to be easy for programmers familair with "c-like" languages like C, C++, C#, Java, Javascript, D, etc).

#### The Json Syntax
Uses standard JSON to represent the construct data structures.


One big difference between construct and other languages is that it is defined using a data structure instead of a syntax.
That being said, it supports a few standard syntaxes that are meant to serve different domains.


Construct code is defined by a list of constructs.
Whatever syntax is used, it needs to compile to the following intermediate representation:
```
```

Summary:
- construct is defined by data structures instead of a syntax.
  - allows construct code to be represented by any syntax that has a parser.
- construct allows libraries to add semantics to the language.
  - allows the "language" to adopt new semantics
  - allows applications to create their own "extensions" to the language for their applications

> (Move this note to another section that describes how to add semantics)
> Note: An application can extend the language without having to write the backend for their new semantics
as long as it reduces to other constructs.  If it cannot reduce, then they will have to implement the new constructs
for every back they want to support.

## Configurable Syntax
Construct uses an simple syntax that requires more text but allows infinite customization.  A rich syntax allows programmers to say what they want in a small number of characters.  A good syntax is extremely powerful, however, as semantics change through new libraries and new techniques, the syntax gets left behind as it cannot be changed without breaking existing code or creating a mess with syntax that looks "hacked on".

The construct syntax sacrifices brevity for flexibility.  It's verbosity is designed to allow it to adhere to any set of new semantics.  This allows the semantics to be modified and extended without having to figure out how to add the new semantics to the language.
