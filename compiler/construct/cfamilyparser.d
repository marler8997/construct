/*
Grammar (The 'C' family syntax.  Supposed to look similar to "C-Like" languages)
====================================================================

Version that shows internals
----------------------------
constructs ::= (name (basic-arg | '/' '{' constructs '}' )* (';' | '{' constructs '}') )*
basic-arg ::= string  ('=' arg)? | list | arg-construct
arg ::= basic-arg | '{' construct '}'
list ::= '(' ( ',' | arg | '/' '{' constructs '}' ) * ')'
arg-construct ::= '/' name arg* '\'?

NOTE:
  construct {...}
  construct (/{...}); is the same as

The first version is just a shorthand way to write it.


*/

module construct.cfamilyparser;

import std.stdio  : writeln, writefln, stdout;
import std.array  : appender, Appender;
import std.file   : read, exists, mkdir, baseName, dirName, buildNormalizedPath;
import std.string : format;
import std.format : formattedWrite;

import utf8;
import construct.ir;
import construct.parser : ConstructParseException;

// A wrapper that will print any character in a human readable format
// only using ascii characters
struct AsciiPrint
{
  dchar c;
  void toString(scope void delegate(const(char)[]) sink) const
  {
    if(c >= 127) {
      if(c <= 255) {
	formattedWrite(sink, "\\x%02x", c);
      } else {
	formattedWrite(sink, "\\u%04x", c);
      }
    } else if (c >= 32) {
      sink((cast(char*)&c)[0..1]);
    } else {
      if(c == '\n') {
	sink(`\n`);
      } else if(c == '\t') {
	sink(`\t`);
      } else if(c == '\r') {
	sink(`\r`);
      } else if(c == '\0') {
	sink(`\0`);
      } else {
	formattedWrite(sink, "\\x%02x", c);
      }
    }
  }
}
// A wrapper that will print any character in a human readable format
// only using ascii characters
struct AsciiPrintString
{
  const(char)[] str;
  void toString(scope void delegate(const(char)[]) sink) const
  {
    const(char)* next = str.ptr;
    const char* limit = next + str.length;
    while(next < limit) {
      AsciiPrint(decodeUtf8(&next, limit)).toString(sink);
    }
  }
}

enum ErrorType {
  //unknown,
  notImplemented,
  unspecified,
  endedEarly,
  invalidChar,
  constructEndedEarly,
  argsWithSameName,
  //expectedEnd,
}
class ConstructCFamilyParseException : ConstructParseException
{
  ErrorType type;
  this(ErrorType type, size_t constructLineNumber, string msg, string codeFile = __FILE__, size_t codeLine = __LINE__) {
    super(constructLineNumber, msg, codeFile, codeLine);
    this.type = type;
  }
}

void implement(size_t lineNumber, size_t codeLine = __LINE__)
{
  throw new ConstructCFamilyParseException(ErrorType.notImplemented, lineNumber, format("parser not fully implemented (%s line %s)", __FILE__, codeLine));
}

private bool isFirstCharacterOfName(dchar c)
{
  if(c >= 'A') {
    if(c <= 'Z') return true;
    if(c < 'a') return c == '_';
    return c <= 'z';
  }
  if(c < '0') return c == '-';
  return (c <= '9');
}
private bool isConstructNameCharacter(dchar c)
{
  if(c >= 'A') {
    if(c <= 'Z') return true;
    if(c < 'a') return c == '_';
    return c <= 'z';
  }
  if(c < '0') {
    return c == '.' || c == '-';
  }
  return (c <= '9');
}

struct ConstructConsumerState
{
  size_t lineNumber;
  const(char)* next;
  const(char)* cpos;
  dchar c;

  //Appender!(const(char)[][]) comments;
}


struct ConstructConsumer
{
  // TODO: this should be const
  const(char)* limit;

  // InputState:
  //   next points to first character
  final const(Construct)[] parseConstructs(ConstructConsumerState* state) const
  {
    auto constructs = appender!(const(Construct)[])();
    parseConstructs(false, constructs, state);
    return constructs.data;
  }

  // InputState:
  //   next points to the character to read (note: could be at the limit)
  // OutputState:
  //   cpos points to chracter that next was pointing to
  //   if(cpos < limit) {
  //     c contains character pointed to by cpos
  //     next points to character after cpos
  //   }
  private final void readChar(ConstructConsumerState* state) const
  {
    state.cpos = state.next;
    if(state.next < limit) {
      state.c = decodeUtf8(&state.next, limit);
    }
  }
  
  // InputState:
  //   next points to first character
  // OutputState:
  //   if(insideBlock) {
  //     next points to first character after '}'
  //   } else {
  //     no more state. all input should be processed
  //   }
  private final void parseConstructs(bool insideBlock, Appender!(const(Construct)[]) constructs, ConstructConsumerState* state) const
  {
    if(state.next >= limit) {
      if(insideBlock) {
	throw new ConstructCFamilyParseException
	  (ErrorType.constructEndedEarly, state.lineNumber, "construct block not ended with '}' character");
      }
      return;
    }
    readChar(state);
    
    while(true) {
      consumeWhitespaceAndComments(state);
      if(state.cpos >= limit) {
	if(insideBlock) {
	  throw new ConstructCFamilyParseException
	    (ErrorType.constructEndedEarly, state.lineNumber, "construct block not ended with '}' character");
	}
        return;
      }

      if(state.c == '}') {
	if(insideBlock) {
	  return;
	}
	throw new ConstructCFamilyParseException
	  (ErrorType.invalidChar, state.lineNumber, "mismatched curly-braces, got an extra close-brace '}'");
      }
      
      if(!isFirstCharacterOfName(state.c)) {
	// Make sure it's not a ArgConstruct
	if(state.c == '/') {
	  throw new ConstructCFamilyParseException
	    (ErrorType.invalidChar, state.lineNumber,
	     "a construct can only have a '/' prefix if it's a parameter to another construct");
	}
	throw new ConstructCFamilyParseException
	  (ErrorType.invalidChar, state.lineNumber,
	   format("expected a construct name but got '%s'", AsciiPrint(state.c)));
      }

      auto constructLineNumber = state.lineNumber;
      auto constructName = parseName(state);
      if(state.cpos >= limit) {
        throw new ConstructCFamilyParseException
          (ErrorType.endedEarly, state.lineNumber, "the last construct is  not ended with a ';'");
      }

      auto args = appender!(ConstructObject[])();
      ConstructObject[const(char)[]] namedArgs = null;
      parseArgs(ArgContext.normalConstruct, args, namedArgs, state);
      constructs.put(new Construct(constructLineNumber, constructName, args.data, namedArgs));
    }
  }

  enum ArgContext {
    normalConstruct,
    list,
    argConstruct,
  }
  
  // InputState:
  //   cpos points to the first potential character
  //   c contains the character to pointed to by cpos
  //   next points to first character
  // OutputState:
  //   cpos points to character after construct (after ';' or '}') or
  //       after list (')') depending on the context
  //   if(cpos < limit) {
  //     c contains character after construct
  //     next points to character after cpos
  //   }
  // 
  private final void parseArgs(ArgContext context,
			       Appender!(ConstructObject[]) args,
			       ref ConstructObject[const(char)[]] namedArgs,
			       ConstructConsumerState* state) const
  {
    // Parse arguments
    while(true) {
      consumeWhitespaceAndComments(state);
      if(state.cpos >= limit) {
	throw new ConstructCFamilyParseException
	  (ErrorType.endedEarly, state.lineNumber, "the last construct is not ended with a ';'");
      }

    NEXT_ARG:
      auto c = state.c;

      if(c == ';') {
	if(context == ArgContext.list) {
	  throw new ConstructCFamilyParseException
	    (ErrorType.invalidChar, state.lineNumber, "expected ')' to end arg list but got ';'");
	}
	if(context == ArgContext.argConstruct) {
	  // the next character is already read
	} else {
	  // Need to read one more character to normalize output state
	  readChar(state);
	}
	return;
      }
      
      if(c == '{') {

	if(context == ArgContext.argConstruct) {
	  throw new ConstructCFamilyParseException
	    (ErrorType.unspecified, state.lineNumber, "I haven't decided how to handle construct blocks in arg-constructs yet");
	}
	auto blockLineNumber = state.lineNumber;
	auto newConstructs = appender!(const(Construct)[])();
	parseConstructs(true, newConstructs, state);
	args.put(new ConstructBlock(blockLineNumber, newConstructs.data));
	readChar(state);
	if(context == ArgContext.list) {
	  continue;
	}
	return;
	
      }

      // Handle lists
      if(c == '(') {

        auto listLineNumber = state.lineNumber;
	auto listArgs = appender!(ConstructObject[])();
	ConstructObject[const(char)[]] listNamedArgs = null;
	readChar(state);
	parseArgs(ArgContext.list, listArgs, listNamedArgs, state);
	args.put(new ConstructList(listLineNumber, listArgs.data/*, listNamedArgs*/));
	continue;

      }

      // Handle List Break
      if(c == ',') {
	if(context != ArgContext.list) {
	  throw new ConstructCFamilyParseException
	    (ErrorType.invalidChar, state.lineNumber, "commas are only allowed inside lists");
	}
	args.put(new ListBreak(state.lineNumber));
	readChar(state);
	continue;
      }

      // Handle arg construct
      if(c == '/') {
	readChar(state);
	consumeWhitespaceAndComments(state);
	if(state.cpos >= limit) {
	  throw new ConstructCFamilyParseException
	    (ErrorType.endedEarly, state.lineNumber, "ended inside a arg construct");
	}

	// Escaped Construct Block
	if(state.c == '{') {
	  parseConstructs(true, cast(Appender!(const(Construct)[]))args, state);
	  readChar(state);
	  continue;
	}

	// Must be an argument construct
	if(!isFirstCharacterOfName(state.c)) {
	  throw new ConstructCFamilyParseException
	    (ErrorType.invalidChar, state.lineNumber,
	     format("expected a construct name but got '%s'", AsciiPrint(state.c)));
	}

	auto constructLineNumber = state.lineNumber;
	auto constructName = parseName(state);
	if(state.cpos >= limit) {
	  throw new ConstructCFamilyParseException
	    (ErrorType.endedEarly, state.lineNumber, "input ended inside a arg construct");
	}
	
	auto argConstructArgs = appender!(ConstructObject[])();
	ConstructObject[const(char)[]] argConstructNamedArgs = null;
	parseArgs(ArgContext.argConstruct, argConstructArgs, argConstructNamedArgs, state);
	args.put(new Construct(constructLineNumber, constructName, argConstructArgs.data, argConstructNamedArgs));
	continue;
      }
      
      // Quoted Strings
      if(c == '"') {

        auto stringLineNumber = state.lineNumber;
	auto next = state.next;
	while(true) {
	  if(next >= limit) {
	    throw new ConstructCFamilyParseException
	      (ErrorType.endedEarly, state.lineNumber, "input ended inside a quoted string");
	  }
	  auto cpos = next;
	  c = decodeUtf8(&next, limit);
	  if(c == '\\') {
	    throw new ConstructCFamilyParseException
	      (ErrorType.notImplemented, state.lineNumber, "quoted string escapes not implemented");
	  } else if(c == '"') {
	    args.put(new ConstructString(stringLineNumber, state.next[0..cpos-state.next]));
	    state.next = next;
	    break;
	  }
	}
	readChar(state);
	continue;
      }

      // Unquoted Strings
      if(isFirstCharacterOfName(c)) {
        auto symbolLineNumber = state.lineNumber;
	auto symbol = parseName(state);
	consumeWhitespaceAndComments(state);
	if(state.cpos >= limit) {
	  throw new ConstructCFamilyParseException
	    (ErrorType.endedEarly, state.lineNumber, "the last construct is not ended with a ';'");
	}
	if(state.c == '=') {
	  auto existing = namedArgs.get(symbol, null);
	  if(existing) {
	    throw new ConstructCFamilyParseException
	      (ErrorType.argsWithSameName, state.lineNumber,
	       format("multiple arguments with the same name '%s'", symbol));
	  }
	  namedArgs[symbol] = parseNamedArg(state);
	  continue;
	}
	
	args.put(ConstructObject.fromUnquoted(symbolLineNumber, symbol));
	// don't need to go to the beginning of the loop because we're already at the next character
	goto NEXT_ARG;
      }

      if(c == '\\') {
        if(context == ArgContext.argConstruct) {
          readChar(state);
          return;
        }
      }
      
      if(c == ')') {
	if(context == ArgContext.list) {
	  readChar(state);
	  return;
	}
	if(context == ArgContext.argConstruct) {
	  return;
	}
	goto BAD_ARG_CHAR;
      }

      BAD_ARG_CHAR:
      throw new ConstructCFamilyParseException
	(ErrorType.invalidChar, state.lineNumber,
	 format("construct arguments cannot start with '%s'", AsciiPrint(c)));
    }
    assert(0, "CodeBug: this code should never execute");
  }


  // InputState:
  //   next points to first character (should be after the '=' character)
  // OutputState:
  //   cpos points the character after the argument
  //   c contains the character pointed to by cpos
  //   next points to character after cpos
  private final ConstructObject parseNamedArg(ConstructConsumerState* state) const
  {
    readChar(state);
    consumeWhitespaceAndComments(state);
    if(state.cpos >= limit) {
      throw new ConstructCFamilyParseException
	(ErrorType.endedEarly, state.lineNumber, "input ended after '=' for named argument");
    }

    auto c = state.c;

    // Handle list
    if(c == '(') {

      readChar(state);
      auto listLineNumber = state.lineNumber;
      auto listArgs = appender!(ConstructObject[])();
      ConstructObject[const(char)[]] listNamedArgs;
      parseArgs(ArgContext.list, listArgs, listNamedArgs, state);
      
      return new ConstructList(listLineNumber, listArgs.data);

    }
    
    if(c == '{') {

      auto lineNumber = state.lineNumber;
      auto constructs = appender!(const(Construct)[])();
      parseConstructs(true, constructs, state);

      // Need to read the next character to normalize
      // the output state for this function
      readChar(state);
      return new ConstructBlock(lineNumber, constructs.data);
	
    }
    
    // Handle arg construct
    if(c == '/') {
      readChar(state);
      consumeWhitespaceAndComments(state);
      if(state.cpos >= limit) {
	throw new ConstructCFamilyParseException
	  (ErrorType.endedEarly, state.lineNumber, "ended inside a arg construct");
      }
      if(!isFirstCharacterOfName(state.c)) {
	throw new ConstructCFamilyParseException
	  (ErrorType.invalidChar, state.lineNumber,
	   format("expected a construct name but got '%s'", AsciiPrint(state.c)));
      }

      auto constructLineNumber = state.lineNumber;
      auto constructName = parseName(state);
      if(state.cpos >= limit) {
	throw new ConstructCFamilyParseException
	  (ErrorType.endedEarly, state.lineNumber, "input ended inside an argument-construct");
      }

      auto argConstructArgs = appender!(ConstructObject[])();
      ConstructObject[const(char)[]] argConstructNamedArgs = null;
      parseArgs(ArgContext.argConstruct, argConstructArgs, argConstructNamedArgs, state);
      return new Construct(constructLineNumber, constructName, argConstructArgs.data, argConstructNamedArgs);
    }

    // Quoted Strings
    if(c == '"') {

      auto stringLineNumber = state.lineNumber;
      auto next = state.next;
      while(true) {
	if(next >= limit) {
	  throw new ConstructCFamilyParseException
	    (ErrorType.endedEarly, state.lineNumber, "input ended inside a quoted string");
	}
	auto cpos = next;
	c = decodeUtf8(&next, limit);
	if(c == '\\') {
	  throw new ConstructCFamilyParseException
	    (ErrorType.notImplemented, state.lineNumber, "quoted string escapes not implemented");
	} else if(c == '"') {
	  auto name = state.next[0..cpos-state.next];
	  state.next = next;
	  readChar(state);
	  return new ConstructString(stringLineNumber, name);
	}
      }

    }

    // Unquoted Strings
    if(isFirstCharacterOfName(c)) {
      auto symbolLineNumber = state.lineNumber;
      auto symbol = parseName(state);
      consumeWhitespaceAndComments(state);
      if(state.cpos >= limit) {
	throw new ConstructCFamilyParseException
	  (ErrorType.endedEarly, state.lineNumber, "the last construct is not ended with a ';'");
      }
      
      return ConstructObject.fromUnquoted(symbolLineNumber, symbol);
    }
    
    if(c == ';') {
      throw new ConstructCFamilyParseException
	(ErrorType.constructEndedEarly, state.lineNumber, "construct ended before named parameter was given");
    }
    if(c == ',') {
      throw new ConstructCFamilyParseException
	(ErrorType.invalidChar, state.lineNumber, "commas are only allowed inside lists");
    }
      
    throw new ConstructCFamilyParseException
      (ErrorType.invalidChar, state.lineNumber,
       format("named construct arguments cannot start with '%s'", AsciiPrint(c)));
  }
  
  // InputState:
  //   cpos points to the first character
  //   c contains the first character
  //   next points to character after cpos
  // ReturnState:
  //   cpos points to character after whitespace and comments
  //   if(cpos < limit) {
  //      c contains character that cpos points to
  //      next points to character after cpos
  //   } else {
  //      no more input. do not use c or next
  //   }
  final void consumeWhitespaceAndComments(ConstructConsumerState* state) const
  {
    dchar c           = state.c;
    const(char)* cpos = state.cpos;
    const(char)* next = state.next;

    while(1) {

      // skip whitespace
      while(1) {
        if(c == '\n') {
          state.lineNumber++;
        } else if(c == ' ' || c == '\t' || c == '\r') {
	  
        } else {
	  state.cpos = cpos;
	  state.c = c;
	  state.next = next;
          if(c != '/') {
            return;
          }
          break;
        }
        if(next >= limit) {
          state.cpos = limit;
          return;
        }
	cpos = next;
        c = decodeUtf8(&next, limit);
      }

      // TODO: save the comments and attach them to the construct
      // maybe only save the comments that do not have a blank space
      // between the comment and the construct
      //
      if(next >= limit) {
        throw new ConstructCFamilyParseException(ErrorType.endedEarly, state.lineNumber, "input ended with a slash");
      }
      c = decodeUtf8(&next, limit);
      if(c == '/') {
          
        // skip till end of line
        while(1) {
          if(next >= limit) {
            state.cpos = limit;
            return;
          }
          c = decodeUtf8(&next, limit);
          if(c == '\n') {
            state.lineNumber++;
            break;
          }
        }
        // TODO: save the comment
          
      } else if(c == '*') {
          
        // skip till end of multiline comment
        bool lastCharWasStar = false;
        while(1) {
          if(next >= limit) {
            throw new ConstructCFamilyParseException(ErrorType.endedEarly, state.lineNumber,
                                              "multi-line comment was not ended");
          }
          c = decodeUtf8(&next, limit);
          if(c == '*') {
            lastCharWasStar = true;
            continue;
          }

          if(c == '/') {
            if(lastCharWasStar) {
              break;
            }
          } else if(c == '\n') {
	    state.lineNumber++;
	  }
          lastCharWasStar = false;
        }
	
      } else {
	// Must not be a comment
	// NOTE: state was already setup correctly
	return;
      }

      // Read the next character before checking for whitespace again
      cpos = next;
      c = decodeUtf8(&next, limit);
    }
  }

  // InputState:
  //   cpos points to first character of name
  //   next points to character after c
  //   Note: it is assumed that the caller already checked that
  //         the character pointed to by cpos is a valid first character
  //         for a name
  // OutputState:
  //   cpos points to character after name
  //   if(cpos < limit) {
  //     c is first character after name
  //     next points to character after c
  //   }
  private final const(char)[] parseName(ConstructConsumerState* state) const
  {
    auto next = state.next;
    while(next < limit) {
      auto cpos = next;
      auto c = decodeUtf8(&next, limit);
      if(!isConstructNameCharacter(c)) {
	auto name = state.cpos[0..cpos-state.cpos];
	state.cpos = cpos;
	state.c = c;
	state.next = next;
	return name;
      }
    }
    
    // End of input case (only need to set cpos)
    auto name = state.cpos[0..next-state.cpos];
    state.cpos = next;
    return name;
  }


  final void debugCurrentState(ConstructConsumerState* state) const {
    writefln("c    : '%s'", AsciiPrint(state.c));
    if(state.cpos) {
      if(state.cpos >= limit) {
	writefln("cpos : %s (limit)", state.cpos);
      } else {
	writefln("cpos : %s -> '%s'", state.cpos, AsciiPrint(*state.cpos));
      }
    } else {
      writefln("cpos : null", state.cpos);
    }
    if(state.next) {
      if(state.next >= limit) {
	writefln("next : %s (limit)", state.next);
      } else {
	writefln("next : %s -> '%s'", state.next, AsciiPrint(*state.next));
      }
    } else {
      writefln("next : null", state.next);
    }
    stdout.flush();
  }
}
const(Construct)[] parseConstructs(const(char)[] code)
{
  auto state = ConstructConsumerState(1, code.ptr);
  auto consumer = ConstructConsumer(code.ptr + code.length);
  return consumer.parseConstructs(&state);
}

unittest
{
  void test(const(char)[] code, Construct[] expected = null, size_t testLine = __LINE__)
  {
    writeln("---------------------------------------");
    writefln("[TEST] \"%s\"", AsciiPrintString(code));
    stdout.flush();
    auto constructs = parseConstructs(code);
    if(expected) {
      if(constructs.length != expected.length) {
	writefln("Expected: %s", expected);
	writefln("Actual  : %s", constructs);
	assert(0, format("Expected %s constructs but got %s (testline %s)",
                         expected.length, constructs.length, testLine));
      }
      foreach(i; 0..expected.length) {
	if(!constructs[i].equals(expected[i])) {
	  writefln("Expected: %s", expected[i]);
	  writefln("Actual  : %s", constructs[i]);
	  stdout.flush();
	  assert(0, format("Construct at index %s does not match (testline %s)", i, testLine));
	}
      }
    }
  }
  void testError(ErrorType expectedError, const(char)[] code, size_t testLine = __LINE__)
  {
    writeln("---------------------------------------");
    writefln("[TEST-ERROR] (%s) \"%s\"", expectedError, AsciiPrintString(code));
    stdout.flush();
    try {
      auto constructs = parseConstructs(code);
      assert(0, format("Expected exception '%s' but did not get one. (testline %s) Code=\"%s\"",
		       expectedError, testLine, AsciiPrintString(code)));
    } catch(ConstructCFamilyParseException e) {
      if(e.type != expectedError) {
	writefln("WrongException: %s", e);
	stdout.flush();
	assert(0, format("Expected error '%s' but got '%s' (testline %s)", expectedError, e.type, testLine));
      }
      writefln("[TEST-ERROR] Successfully got error: %s", e.msg);
    }
  }

  testError(ErrorType.endedEarly, "/");
  testError(ErrorType.invalidChar, "/+");

  test("//");
  test("//   _)(6545 ^58%& \n   // what\r\n // ^*^*^ ");
  test("/* \n%^&*( \n */ ");
  test("/* !!! **/");

  testError(ErrorType.endedEarly, "/*");
  testError(ErrorType.endedEarly, "/* *");
  testError(ErrorType.invalidChar, ".notAArg");
  testError(ErrorType.invalidChar, "[notAParm]");
  testError(ErrorType.invalidChar, "}");

  for(uint i = 0; i < 128; i++) {
    if(!isFirstCharacterOfName(i) &&
       i != ' ' && i != '\t' && i != '\r' && i != '\n' && i != '/') {
      char c = cast(char)i;
      testError(ErrorType.invalidChar, (cast(char*)&c)[0..1]);
    }
  }

  testError(ErrorType.endedEarly, "a");
  testError(ErrorType.endedEarly, "ab");
  testError(ErrorType.endedEarly, "abc");
  testError(ErrorType.endedEarly, "abc ");
  testError(ErrorType.endedEarly, "abc //comment");
  testError(ErrorType.endedEarly, "abc //comment\n");
  test("a;", [new Construct(1, "a")]);
  test("ab;", [new Construct(1, "ab")]);
  test("abasdfjkl ;", [new Construct(1, "abasdfjkl")]);
  test("abc; // comment", [new Construct(1, "abc")]);

  testError(ErrorType.invalidChar, "abc #");
  testError(ErrorType.endedEarly, "function myfunc");
  test("function myfunc;", [new Construct(1, "function", [new ConstructSymbol(1, "myfunc")])]);
  test("function /*comment*/ myfunc /* comment2 */;", [new Construct(1, "function", [new ConstructSymbol(1, "myfunc")])]);

  testError(ErrorType.invalidChar, "a (;");
  testError(ErrorType.invalidChar, "a )");
  testError(ErrorType.endedEarly, "a ()");
  test("a();", [new Construct(1, "a", [new ConstructList(1)])]);
  test("a ();", [new Construct(1, "a", [new ConstructList(1)])]);
  test("a () ;", [new Construct(1, "a", [new ConstructList(1)])]);

  testError(ErrorType.endedEarly, "a(first");
  testError(ErrorType.endedEarly, "a(first)");
  test("a (first);", [new Construct(1, "a", [new ConstructList(1, [new ConstructSymbol(1, "first")])])]);
  test("a ( first )  ;", [new Construct(1, "a", [new ConstructList(1, [new ConstructSymbol(1, "first")])])]);
  test("a /*comment*/(/*another*/first/*what*/)/*hey*/;", [new Construct(1, "a", [new ConstructList(1, [new ConstructSymbol(1, "first")])])]);

  test("a (first second);", [new Construct(1, "a", [new ConstructList(1, [new ConstructSymbol(1, "first"), new ConstructSymbol(1, "second")])])]);

  testError(ErrorType.invalidChar, "/a");
  testError(ErrorType.invalidChar, " / a ");
  testError(ErrorType.invalidChar, " / ab ");
  testError(ErrorType.invalidChar, " / /*comment*/ab ");

  testError(ErrorType.invalidChar, "a/;");
  testError(ErrorType.invalidChar, "abc/;");
  testError(ErrorType.endedEarly, "a/");
  testError(ErrorType.endedEarly, "abc/");

  test("abc/string;", [new Construct(1, "abc", [new Construct(1, "string")])]);
  test("abc (a/string);", [new Construct(1, "abc", [new ConstructList(1, [new ConstructSymbol(1, "a"), new Construct(1, "string")])])]);

  testError(ErrorType.unspecified, "a /sub {");
  testError(ErrorType.unspecified, "a /sub {}");

  test("a ;", [new Construct(1, "a")]);
  test("a {}", [new Construct(1, "a", [new ConstructBlock(1)])]);
  test("a {b;c;}", [new Construct(1, "a", [new ConstructBlock(1, [new Construct(1, "b"), new Construct(1, "c")])])]);
  //test("a ({b;c;}) ;", [new Construct(1, "a", [new ConstructList(1, [new Construct(1, "b"), new Construct(1, "c")])])]);
  //test("a ( ({b;c;}) ) ;", [new Construct(1, "a", [new ConstructList(1, [new ConstructList(1, [new Construct(1, "b"), new Construct(1, "c")])])])]);
  //test("a ({b;c;}) {}", [new Construct(1, "a", [new ConstructList(1, [new Construct(1, "b"), new Construct(1, "c")])])]);

  testError(ErrorType.invalidChar, "}");

  // Standard types
  test("a int;" , [new Construct(1, "a", [PrimitiveTypeEnum.int_.create(1) ])]);
  test("a uint;", [new Construct(1, "a", [PrimitiveTypeEnum.uint_.create(1)])]);
  
  // Quoted Strings
  testError(ErrorType.endedEarly, "a \"");
  testError(ErrorType.endedEarly, "a \" ");
  test("a \"a string!\";", [new Construct(1, "a", [new ConstructString(1, "a string!")])]);

  // List Breaks
  test("a (c,d,e);", [new Construct(1, "a", [new ConstructList(1, [
							       new ConstructSymbol(1, "c"),
							       new ListBreak(1),
							       new ConstructSymbol(1, "d"),
							       new ListBreak(1),
							       new ConstructSymbol(1, "e"),
							       ])])]);

  // Named Args
  testError(ErrorType.endedEarly, "a b=");
  testError(ErrorType.endedEarly, "a b =");
  testError(ErrorType.endedEarly, "a b= ");
  testError(ErrorType.endedEarly, "a b = ");

  testError(ErrorType.endedEarly, "a b=c");
  testError(ErrorType.endedEarly, "a b=c ");
  testError(ErrorType.endedEarly, "a b= c");
  testError(ErrorType.endedEarly, "a b =c");
  testError(ErrorType.endedEarly, "a b= c ");
  testError(ErrorType.endedEarly, "a b = c");
  testError(ErrorType.endedEarly, "a b = c ");

  testError(ErrorType.endedEarly, "a b=()");
  testError(ErrorType.endedEarly, "a b=(c d)");
  testError(ErrorType.endedEarly, "a b={}");
  testError(ErrorType.endedEarly, "a b=/");
  testError(ErrorType.endedEarly, "a b=/hey");
  testError(ErrorType.invalidChar, "a b=/!");
  testError(ErrorType.invalidChar, "a b=/\"");
  testError(ErrorType.invalidChar, "a b=/\"hello");
  testError(ErrorType.invalidChar, "a b=/\"\"");
  testError(ErrorType.invalidChar, "a b=/\"hello\"");
  
  testError(ErrorType.constructEndedEarly, "a b=;");

  testError(ErrorType.argsWithSameName, "a b=() b=");
  testError(ErrorType.argsWithSameName, "a b=() b=d;");
  
  test("a b=();", [new Construct(1, "a", null, ["b": new ConstructList(1)])]);
  test("a b=(c d);", [new Construct(1, "a", null, ["b": new ConstructList(1, [new ConstructSymbol(1, "c"), new ConstructSymbol(1, "d")])])]);
  test("a b=c;", [new Construct(1, "a", null, ["b": new ConstructSymbol(1, "c")])]);
  test("a b={};", [new Construct(1, "a", null, ["b": new ConstructBlock(1, null)])]);
  test("a b=/hey;", [new Construct(1, "a", null, ["b": new Construct(1, "hey")])]);

  // Escaped Construct Block
  testError(ErrorType.endedEarly, "a /{b;}");
  test("a /{b;};", [new Construct(1, "a", [new Construct(1, "b")])]);
  test("a /{b;} {}", [new Construct(1, "a", [new Construct(1, "b"), new ConstructBlock(1)])]);


  // Unescaped construct block
  testError(ErrorType.endedEarly, `a /b\`);
  test(`a /b\;`, [new Construct(1, "a", [new Construct(1, "b")])]);
}

