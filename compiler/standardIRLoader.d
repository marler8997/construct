import std.stdio  : writeln, writefln, stdout;
import std.array  : appender, Appender;
import std.file   : read, exists, mkdir, baseName, dirName, buildNormalizedPath;
import std.string : format;
import std.format : formattedWrite;

import utf8;
import construct.ir;
import backend : emit;
  
void usage(string program)
{
  writefln("Usage: %s <construct-file>", program.baseName);
}
int main(string[] args)
{
  string program = args[0];
  args = args[1..$];
    
  if(args.length <= 0) {
    usage(program);
    return 0;
  }
  if(args.length > 1) {
    usage(program);
    writeln("Error: too many command line arguments");
    return 1;
  }

  string constructFile = args[0];
  if(!exists(constructFile)) {
    writefln("Error: construct file '%s' does not exist", constructFile);
    return 1;
  }
  string outputDir = buildNormalizedPath(constructFile.dirName, "obj");
  if(!exists(outputDir)) {
    mkdir(outputDir);
  }
    
  writefln("[DEBUG] Loading constructs from '%s'...", constructFile);
  stdout.flush();

  auto constructCode = cast(string)read(constructFile);

  const(Construct)[] constructs;
  try {
    constructs = parseConstructs(constructCode);
  } catch(ConstructParseException e) {
    writefln("Error in %s (line %s): %s", constructFile, e.lineNumber, e.msg);
    return 1;
  }

  writeln("[DEBUG] Done loading constructs. Emitting code...");
  stdout.flush();

  {
    auto result = emit(outputDir, constructFile, constructs);
    if(result == 0) {
      writeln("[DEBUG] Done emitting code, Success.");
    }
    return result;
  }
}


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
  //expectedEnd,
}
class ConstructParseException : Exception
{
  ErrorType type;
  size_t lineNumber;
  this(ErrorType type, size_t lineNumber, string msg) {
    super(msg);
    this.type = type;
    this.lineNumber = lineNumber;
  }
}

void implement(size_t lineNumber, size_t codeLine = __LINE__)
{
  throw new ConstructParseException(ErrorType.notImplemented, lineNumber, format("parser not fully implemented (line %s)", codeLine));
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
  /*
  final void enforceEnd(ConstructConsumerState* state) const
  {
    if(state.next < limit) {
      throw new ConstructParseException(ErrorType.expectedEnd, state.lineNumber, "expected input to end but did not");
    }
  }
  */
  // InputState:
  //   next points to first character
  final Construct[] parseConstructs(ConstructConsumerState* state) const
  {
    auto constructs = appender!(Construct[])();
    parseConstructs(false, constructs, state);
    return constructs.data;
  }
  // InputState:
  //   next points to first character
  // OutputState:
  //   if(insideBlock) {
  //     next points to first character after '}'
  //   } else {
  //     no more state. all input should be processed
  //   }
  private final void parseConstructs(bool insideBlock, Appender!(Construct[]) constructs, ConstructConsumerState* state) const
  {
    while(true) {
      consumeWhitespaceAndComments(state);
      if(state.cpos >= limit) {
	if(insideBlock) {
	  throw new ConstructParseException
	    (ErrorType.invalidChar, state.lineNumber, "construct block not ended with '}'");
	}
        return;
      }

      if(state.c == '}') {
	if(insideBlock) {
	  return;
	}
	throw new ConstructParseException
	  (ErrorType.invalidChar, state.lineNumber, "mismatched curly-braces, got an extra close-brace '}'");
      }
      
      if(!isFirstCharacterOfName(state.c)) {
	// Make sure it's not a ParamConstruct
	if(state.c == '/') {
	  throw new ConstructParseException
	    (ErrorType.invalidChar, state.lineNumber,
	     "a construct can only have a '/' prefix if it's a parameter to another construct");
	}
	throw new ConstructParseException
	  (ErrorType.invalidChar, state.lineNumber,
	   format("expected a construct name but got '%s'", AsciiPrint(state.c)));
      }

      auto constructName = parseName(state);
      if(state.next >= limit) {
        throw new ConstructParseException
          (ErrorType.endedEarly, state.lineNumber, "the last construct is  not ended with a ';'");
      }

      auto params = appender!(ConstructObject[])();
      ConstructObject[string] namedParams = null;
      Construct[] constructBlock;
      // todo: the params and namedParams might
      //       need to be passed by reference
      parseParams(ParamContext.normalConstruct, params, namedParams, &constructBlock, state);
      constructs.put(new Construct(constructName, params.data, namedParams, constructBlock));
    }
  }

  enum ParamContext {
    normalConstruct,
    list,
    paramConstruct,
  }

  // InputState:
  //   next points to first character
  // OutputState:
  //   next pointer to character after construct
  private final void parseParams(ParamContext context,
				 Appender!(ConstructObject[]) params,
				 ConstructObject[string] namedParams,
				 Construct[]* constructBlock,
				 ConstructConsumerState* state) const
  {
    // Parse parameters
    while(true) {
      consumeWhitespaceAndComments(state);
      if(state.cpos >= limit) {
	throw new ConstructParseException
	  (ErrorType.endedEarly, state.lineNumber, "the last construct is not ended with a ';'");
      }

    NEXT_PARAM:
      auto c = state.c;
      if(c == ';') {
	if(context == ParamContext.list) {
	  throw new ConstructParseException
	    (ErrorType.invalidChar, state.lineNumber, "expected ')' to end param list but got ';'");
	}
	if(context == ParamContext.paramConstruct) {
	  // rewind
	  state.next = state.cpos;
	}
	return;
      }
      
      if(c == '{') {

	if(context == ParamContext.paramConstruct) {
	  throw new ConstructParseException
	    (ErrorType.unspecified, state.lineNumber, "I haven't decided how to handle construct blocks in param-constructs yet");
	}
	if(context == ParamContext.list) {
	  parseConstructs(true, cast(Appender!(Construct[]))params, state);
	  continue;
	}

	// context == ParamContext.normalConstruct
	auto blockBuilder = appender!(Construct[])();
	parseConstructs(true, blockBuilder, state);
	*constructBlock = blockBuilder.data;
	return;
	
      }

      // Handle lists
      if(c == '(') {
	
	auto listParams = appender!(ConstructObject[])();
	ConstructObject[string] listNamedParams = null;
	// todo: the params and namedParams might
	//       need to be passed by reference
	parseParams(ParamContext.list, listParams, listNamedParams, null, state);
	params.put(new ParamList(listParams.data, listNamedParams));
	continue;
	
      }

      // Handle param construct
      if(c == '/') {
	consumeWhitespaceAndComments(state);
	if(state.cpos >= limit) {
	  throw new ConstructParseException
	    (ErrorType.endedEarly, state.lineNumber, "ended inside a param construct");
	}

	if(!isFirstCharacterOfName(state.c)) {
	  throw new ConstructParseException
	    (ErrorType.invalidChar, state.lineNumber,
	     format("expected a construct name but got '%s'", AsciiPrint(state.c)));
	}

	auto constructName = parseName(state);
	if(state.next >= limit) {
	  throw new ConstructParseException
	    (ErrorType.endedEarly, state.lineNumber, "input ended inside a param construct");
	}

	auto paramConstructParams = appender!(ConstructObject[])();
	ConstructObject[string] paramConstructNamedParams = null;
	// todo: the params and namedParams might
	//       need to be passed by reference
	parseParams(ParamContext.paramConstruct, paramConstructParams, paramConstructNamedParams, null, state);
	params.put(new Construct(constructName, paramConstructParams.data, paramConstructNamedParams));
	continue;
      }
      
      if(c == '[') {
	implement(state.lineNumber);
      }

      // Quoted Strings
      if(c == '"') {
	
	auto next = state.next;
	while(true) {
	  if(next >= limit) {
	    throw new ConstructParseException
	      (ErrorType.endedEarly, state.lineNumber, "input ended inside a quoted string");
	  }
	  auto cpos = next;
	  c = decodeUtf8(&next, limit);
	  if(c == '\\') {
	    throw new ConstructParseException
	      (ErrorType.notImplemented, state.lineNumber, "quoted string escapes not implemented");
	  } else if(c == '"') {
	    params.put(new StringLiteral(state.next[0..cpos-state.next]));
	    state.next = next;
	    break;
	  }
	}
	continue;
      }

      // Unquoted Strings
      if(isFirstCharacterOfName(c)) {
	auto name = parseName(state);
	consumeWhitespaceAndComments(state);
	if(state.cpos >= limit) {
	  throw new ConstructParseException
	    (ErrorType.endedEarly, state.lineNumber, "the last construct is not ended with a ';'");
	}
	if(state.c != '=') {
	  params.put(cast(ConstructObject)ConstructObject.fromUnquoted(name));
	  goto NEXT_PARAM;
	}
	implement(state.lineNumber);
      }

      if(c == ')') {
	if(context == ParamContext.list) {
	  return;
	}
	if(context == ParamContext.paramConstruct) {
	  // rewind
	  state.next = state.cpos;
	  return;
	}
	goto BAD_PARAM_CHAR;
      }

      BAD_PARAM_CHAR:
      throw new ConstructParseException
	(ErrorType.invalidChar, state.lineNumber,
	 format("construct parameters cannot start with '%s'", AsciiPrint(c)));
    }
    assert(0, "CodeBug: this code should never execute");
  }
  
  // InputState:
  //   next points to first character
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
    auto next = state.next;
    dchar c;

    while(1) {
      // skip whitespace
      while(1) {
        if(next >= limit) {
          state.cpos = limit;
          return;
        }
        auto cpos = next;
        c = decodeUtf8(&next, limit);
        if(c == '\n') {
          state.lineNumber++;
        } else if(c == ' ' || c == '\t' || c == '\r') {
          // skip whitespace
        } else {
	  state.cpos = cpos;
	  state.c = c;
	  state.next = next;
          if(c != '/') {
            return;
          }
          break;
        }
      }

      // TODO: save the comments and attach them to the construct
      // maybe only save the comments that do not have a blank space
      // between the comment and the construct
      //
      if(next >= limit) {
        throw new ConstructParseException(ErrorType.endedEarly, state.lineNumber, "input ended with a slash");
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
            throw new ConstructParseException(ErrorType.endedEarly, state.lineNumber,
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
        //throw new ConstructParseException(ErrorType.invalidChar, state.lineNumber,
	//format("expected '/' or '*' after '/', but got '%s'", AsciiPrint(c)));
      }
    }
  }

  // InputState:
  //   c is the first character of the name
  //   cpos points to c
  //   next points to character after c
  //   Assumption: c is a valid first character of a name
  // OutputState:
  //   next pointer to character after name
  private final const(char)[] parseName(ConstructConsumerState* state) const
  {
    auto next = state.next;
    while(true) {
      if(next >= limit) {
        state.next = next;
        return state.cpos[0..next-state.cpos];
        break;
      }
      auto cpos = next;
      auto c = decodeUtf8(&next, limit);
      if(!isConstructNameCharacter(c)) {
        state.next = cpos;
        return state.cpos[0..cpos-state.cpos];
      }
    }
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
Construct[] parseConstructs(const(char)[] code)
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
    } catch(ConstructParseException e) {
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
  testError(ErrorType.invalidChar, ".notAParam");
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
  test("a;", [new Construct("a")]);
  test("ab;", [new Construct("ab")]);
  test("abasdfjkl ;", [new Construct("abasdfjkl")]);
  test("abc; // comment", [new Construct("abc")]);

  testError(ErrorType.invalidChar, "abc #");
  testError(ErrorType.endedEarly, "function myfunc");
  test("function myfunc;", [new Construct("function", [new StringLiteral("myfunc")])]);
  test("function /*comment*/ myfunc /* comment2 */;", [new Construct("function", [new StringLiteral("myfunc")])]);

  testError(ErrorType.invalidChar, "a (;");
  testError(ErrorType.invalidChar, "a )");
  testError(ErrorType.endedEarly, "a ()");
  test("a();", [new Construct("a", [new ParamList()])]);
  test("a ();", [new Construct("a", [new ParamList()])]);
  test("a () ;", [new Construct("a", [new ParamList()])]);

  testError(ErrorType.endedEarly, "a(first");
  testError(ErrorType.endedEarly, "a(first)");
  test("a (first);", [new Construct("a", [new ParamList([new StringLiteral("first")])])]);
  test("a ( first )  ;", [new Construct("a", [new ParamList([new StringLiteral("first")])])]);
  test("a /*comment*/(/*another*/first/*what*/)/*hey*/;", [new Construct("a", [new ParamList([new StringLiteral("first")])])]);

  test("a (first second);", [new Construct("a", [new ParamList([new StringLiteral("first"), new StringLiteral("second")])])]);

  testError(ErrorType.invalidChar, "/a");
  testError(ErrorType.invalidChar, " / a ");
  testError(ErrorType.invalidChar, " / ab ");
  testError(ErrorType.invalidChar, " / /*comment*/ab ");

  testError(ErrorType.invalidChar, "a/;");
  testError(ErrorType.invalidChar, "abc/;");
  testError(ErrorType.endedEarly, "a/");
  testError(ErrorType.endedEarly, "abc/");

  test("abc/string;", [new Construct("abc", [new Construct("string")])]);
  test("abc (a/string);", [new Construct("abc", [new ParamList([new StringLiteral("a"), new Construct("string")])])]);

  testError(ErrorType.unspecified, "a /sub {");
  testError(ErrorType.unspecified, "a /sub {}");

  test("a ;", [new Construct("a")]);
  test("a {}", [new Construct("a")]);
  test("a ({b;c;}) ;", [new Construct("a", [new ParamList([new Construct("b"), new Construct("c")])])]);
  test("a ( ({b;c;}) ) ;", [new Construct("a", [new ParamList([new ParamList([new Construct("b"), new Construct("c")])])])]);
  test("a ({b;c;}) {}", [new Construct("a", [new ParamList([new Construct("b"), new Construct("c")])])]);

  test("a {b;c;}", [new Construct("a", null, null, [new Construct("b"), new Construct("c")])]);

  // Standard types
  test("a int;", [new Construct("a", [StandardType.int_])]);
  test("a uint;", [new Construct("a", [StandardType.uint_])]);
  
  // Quoted Strings
  testError(ErrorType.endedEarly, "a \"");
  testError(ErrorType.endedEarly, "a \" ");
  test("a \"a string!\";", [new Construct("a", [new StringLiteral("a string!")])]);
}

