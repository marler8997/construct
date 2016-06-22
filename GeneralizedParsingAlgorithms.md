
# Generalized Parsing Algorithms

## Goals

Seperate the logic of a parsing algorithm from the instances of it.
You may have a parsing algorithm that parses integers.
One instance may only support 32bit numbers while another supports different bit widths at runtime.
One instance may accept input with a termination sequence or accept a pointer to indicate the end of the input.
One instance may support parsing the integer as multiple chunks.
All the instances however should use the same algorithm.
This will require a new language to represent these generalized semantics.
The language will need to allow the algorithm to indicate things like
- When input is expected to end or if it could accept more input
- When it finds certain kinds of data (i.e. a keyword, a number, ...)
- If it needs to save data from the give chunk to possibly use with the next chunk

## Definition

The parsing algorithms will accept input as chunks. There will be 2 standard interfaces for inputing
a chunk of data to an algorithm. The first one will be a simple pointer to the data, with some sequence
of characters used to delimit the end of the input.
```
// Input Argument: byte* data
//
//             Memory
// data ->  | some_data   |
//          | some_data   |
//          |    ...      |
//          | some_data   | (last input character)
//          | end_marker1 |
//          | end_marker2 |
//               ...
//          | end_markerX |
```

The end-of-input sequence could be a single zero (also called a null-terminated chunk).
It could be a set of things, like "all non-digit characters".
It could also be multiple characters like, three double quotes in a row.

The second standard interface for sending data to the algorithm will
be a pointer to the data, and a _limit_ pointer which points to one address past the last character.

```
// Input Arguments: byte* data, byte* limit
//
//             Memory
// data ->  | some_data |
//          | some_data |
//          |    ...    |
//          | some_data | (last input character)
// limit -> | undefined |
```

> Note: Using a _limit_pointer instead of a _length_ value is more reasonable for parsing algorithms because
parsing algorithms are commonly scanning the input by incrementing the data pointer.  If the algorithm
used the length parameter to track how much input was left, it would constantly have to increment the
data pointer, and decrement the length value. This causes more code (source code and machine code) than
necessary and requires more maintainence to be sure that the pointer and the length are always kept in sync.
It's true that the algorithm could simply calculate the _limit_ pointer if it was given a _length_, but then
almost every algorithmn would start with this calculation, and chaining algorithms would result in alot of
unnecessary calculations.

An algorithm that only supports the second interface is find, however, any algorithm that supports the first
interface should probably also support the second. For example, say you have an algorithm that parses integers
and use the '\0' character to delimit the end. There will likely be times where a user will want to parse an
integer but the integer is part of a bigger set of input that the user will not want to modify.  Supporting both
interfaces is easy though because the semantics allow the algorithm to use a generalized interface to check for
the end of the chunk.
```
// some algorithm code
if(endOfChunk()) {
    // do something
}
```

## Encoding

If an algorithm only supports ascii characters (0 - 127), then use ascii.  If it supports more, then it's
highly recommended to use UTF8.  However, the semantics should be general enough to abstract the encoding
away from the algorithm logic.

## Types

There is a heirarcy of types.

unsignedXX : unsigned XX-bit integer
signedXX : signed XX-bit integer

unsigned : any unsigned integer
signed : any signed integer


## Calling a parser

A parser is a state machine.  Unlike a function, you can pre-allocate space for
the state machine and call it multiple times.

```
parser myParser;
result = myParser.input(myInput);
```

## Generic Code Interface

```
parser <ParserName>
{
    // Access to functions:
    //   endOfInput()         : Checks if there is no more input
    //   nextChar()           : Reads the next character
    //   needMoreInputError() : 
    //   unexpectedInputError()
    // Note: if you call nextChar(), and you have not checked endOfInput(),
    //       it should submit an error
    //
    // endOfInput()
    //    
    
}
```


## Example: Parse Integer



```
// Note: unsigned represents a generic class of types.
parser parseUnsigned
{
    // Note: Inherits standard parse interface inputs and outputs
    // We have access to endOfInput() function, nextChar() function
    output result : unsigned;
    
    if(endOfInput()) {
        throw needMoreInputError();
    }
    
    c = nextChar();
    if( c < '1' || c > '9' ) {
      // We already know what input was expected by analyzing the code
      throw unexpectedInputError();
    }
    
    result = c - '0';
    
    loop {
        if(endOfInput()) {
            return result;
        }
    }
}
```

## Example: Http Parser

