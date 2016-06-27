The Construct Programming Language
===================================================
Construct is a very simple programming language.
In fact, it's so simple it doesn't even have a syntax.
Instead, construct is defined by a small set of data structures.
These data structures fit inside the basic unit of the construct language, the "construct",
which is simply a object with a name and some arguments.
Then language consists entirely of construct objects.

What the constructs do depend on the backend.
One backend may execute the constructs, one may emit machine code, another
may emit code in another programming language.

The semantics of the constructs are defined by the 'semantics' construct.
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
Construct uses an extremely simple syntax that requires more text but allows infinite customization.  A rich syntax allows programmers to say what they want in a small number of characters.  A good syntax is extremely powerful, however, as semantics change through new libraries and new techniques, the syntax gets left behind as it cannot be changed without breaking existing code or creating a mess with syntax that looks "hacked on".

The construct syntax sacrifices brevity for flexibility.  It's verbosity is designed to allow it to adhere to any set of new semantics.  This allows the semantics to be modified and extended without having to figure out how to add the new semantics to the language.
