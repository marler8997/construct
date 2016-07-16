/*
Grammar
===========================================
construct ::= ( object | named-object)*
object ::= symbol | number | string | block | list | ';'
named-object ::= (symbol | number | string) '=' object
block ::= '{' construct '}'
list ::= '(' (construct | ',')* ')'
*/

module construct.standardParser;

import std.stdio  : writeln, writefln, stdout;
import std.array  : appender, Appender;
import std.file   : read, exists, mkdir, baseName, dirName, buildNormalizedPath;
import std.string : format;
import std.format : formattedWrite;
import std.conv   : to;

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
  unspecified,
  endedEarly,
  invalidChar,
  constructEndedEarly,
  argsWithSameName,
}
class ConstructCFamilyParseException : ConstructParseException
{
  ErrorType type;
  this(ErrorType type, size_t constructLineNumber, string msg, string codeFile = __FILE__, size_t codeLine = __LINE__) {
    super(constructLineNumber, msg, codeFile, codeLine);
    this.type = type;
  }
}

private bool isFirstCharacterOfSymbol(dchar c)
{
  if(c >= 'A') {
    if(c <= 'Z') return true;
    if(c < 'a') return c == '_';
    return c <= 'z';
  }
  return c == '-';
}
private bool isSymbolCharacter(dchar c)
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

const(ConstructObject)[] parse(T)(const(char)[] code) if(isConstructBuilder!(T))
{
  auto consumer = ConstructConsumer!T(code.ptr, code.ptr + code.length);
  return consumer.parse();
}


struct ConstructConsumer(T) if(isConstructBuilder!(T))
{
  const char* limit;
  const bool keepComments;

  // The fields that change
  struct State
  {
    size_t lineNumber;
    const(char)* next;
    const(char)* cpos;
    dchar c;
  }
  State state;

  this(const(char)* start, const char* limit, bool keepComments = false)
  {
    this.limit = limit;
    this.keepComments = keepComments;

    this.state.next = start;
    this.state.lineNumber = 1;
  }
  
  // InputState:
  //   next points to the first character
  // ReturnState:
  //   cpos points to next character
  //   if(cpos < limit) {
  //     c is character pointed to by cpos
  //     next points to next character
  //   }
  final void skipWhitespace()
  {
    dchar c;
    const(char)* cpos;
    const(char)* next = state.next;
    
    while(1) {
      if(next >= limit) {
        state.cpos = limit;
        return;
      }
      
      cpos = next;
      c = decodeUtf8(&next, limit);
      
      if(c == '\n') {
        state.lineNumber++;
      } else if(c != ' ' && c != '\t' && c != '\r') {
        state.cpos = cpos;
        state.c = c;
        state.next = next;
        return;
      }
    }
  }

  // InputState:
  //   next points to first character
  final const(ConstructObject)[] parse()
  {
    if(keepComments) {
      imp("keep comments");
    }
    T objects = T.init;
    parse(0, objects);
    return objects.done;
  }

  // InputState:
  //   next points to the character to read (note: could be at the limit)
  // OutputState:
  //   cpos points to chracter that next was pointing to
  //   if(cpos < limit) {
  //     c contains character pointed to by cpos
  //     next points to character after cpos
  //     Note: I think c must be '}' or ')'
  //   }
  private final void readChar()
  {
    state.cpos = state.next;
    if(state.next < limit) {
      state.c = decodeUtf8(&state.next, limit);
    }
  }

  // InputState:
  //   next points to first character
  //   context = '\0' (root), '}' (block), ')' (list)
  // OutputState:
  //   cpos points to next character
  //   if(cpos < limit) {
  //     c contains char pointed to by c
  //     next points to next char
  //   }
  private final void parse(char context, ref T objects)
  {
    
    while(true) {
      skipWhitespace();
      if(state.cpos >= limit) {
	break;
      }
      
    AT_NEXT_ARG:
      
      auto c = state.c;
      if(c == ';') {
        objects.put(new ObjectBreak(state.lineNumber));
        continue;
      }

      if(c == '{') {
	auto blockLineNumber = state.lineNumber;
	auto blockObjects = T.init;
	parse('}', blockObjects);
	objects.put(new ConstructBlock(blockLineNumber, blockObjects.done));
	continue;
      }

      if(c == '(') {
	auto listLineNumber = state.lineNumber;
	auto listObjects = T.init;
	parse(')', listObjects);
	objects.put(new ConstructList(listLineNumber, listObjects.done));
	continue;
      }

      if(c == ',') {
	if(context != ')') {
	  throw new ConstructCFamilyParseException
	    (ErrorType.invalidChar, state.lineNumber, "list breaks or commas ',', can only appear inside lists");
	}
	objects.put(new ListBreak(state.lineNumber));
	continue;
      }

      if(c == ')') {
	if(context == ')') {
	  return;
	}
	throw new ConstructCFamilyParseException
	  (ErrorType.invalidChar, state.lineNumber, "extra mismatched close parens ')'");
      }
      
      if(c == '}') {
	if(context == 0) {
	  throw new ConstructCFamilyParseException
	    (ErrorType.invalidChar, state.lineNumber, "extra mismatched close brace '}'");
	} else if(context == ')') {
	  throw new ConstructCFamilyParseException
	    (ErrorType.invalidChar, state.lineNumber, "expected close parens ')' to end list, but got close brace '}'");
	}
	return;
      }

      // Handle Comments
      if(c == '/') {
	auto commentLineNumber = state.lineNumber;
	auto comment = parseComment();
	if(keepComments) {
	  objects.put(new ConstructComment(commentLineNumber, comment));
	}
	continue;
      }
      
      // Quoted Strings
      if(c == '"') {
        auto stringLineNumber = state.lineNumber;
	auto string_ = parseQuoted();
	objects.put(new ConstructUtf8(stringLineNumber, string_));
	continue;
      }

      // Numbers
      if(c >= '0' && c <= '9') {
        auto numberLineNumber = state.lineNumber;
        auto numberString = parseNumber();
        objects.put(new ConstructUint(numberLineNumber, to!size_t(numberString)));
        state.next = state.cpos; // rewind
        continue;
      }

      if(isFirstCharacterOfSymbol(c)) {
        auto symbolLineNumber = state.lineNumber;
	auto symbol = parseSymbol();
	if(state.cpos >= limit) {
	  objects.put(ConstructObject.fromUnquoted(symbolLineNumber, symbol));
	  break;
	}
	state.next = state.cpos; // rewind
	skipWhitespace();
	if(state.cpos >= limit) {
	  objects.put(ConstructObject.fromUnquoted(symbolLineNumber, symbol));
	  break;
	}
        
	if(state.c == '=') {
          objects.put(new ConstructNamedObject(symbolLineNumber, symbol, parseNamedObject()));
	  state.next = state.cpos; // rewind
	  continue;
	}

	objects.put(ConstructObject.fromUnquoted(symbolLineNumber, symbol));
	// don't need to go to the beginning of the loop because we're already at the next character
	goto AT_NEXT_ARG;
      }
      
      throw new ConstructCFamilyParseException
	(ErrorType.invalidChar, state.lineNumber,
	 format("construct object cannot start with '%s'", AsciiPrint(c)));
    }

    if(context == 0) {
      return;
    }
    throw new ConstructCFamilyParseException
      (ErrorType.endedEarly, state.lineNumber,
       format("expected '%s' but reached end of input", context));
  }

  // InputState:
  //   next points to char after '='
  // OutputState:
  //   cpos points to char after named object
  //   if(cpos < limit) {
  //     c contains char pointed to by cpos
  //     next points to char after cpos
  //   }
  private final ConstructObject parseNamedObject()
  {
    while(true) {
      skipWhitespace();
      if(state.cpos >= limit) {
        throw new ConstructCFamilyParseException
          (ErrorType.endedEarly, state.lineNumber,
           "expected object after '=' but reached end of input");
      }
    
      auto c = state.c;
      if(c == ';') {
	readChar();
        return new ObjectBreak(state.lineNumber);
      }

      if(c == '{') {
        auto blockLineNumber = state.lineNumber;
        auto blockObjects = T.init;
        parse('}', blockObjects);
	readChar();
        return new ConstructBlock(blockLineNumber, blockObjects.done);
      }

      if(c == '(') {
        auto listLineNumber = state.lineNumber;
        auto listObjects = T.init;
        parse(')', listObjects);
	readChar();
        return new ConstructList(listLineNumber, listObjects.done);
      }

      // Handle Comments
      if(c == '/') {
        auto commentLineNumber = state.lineNumber;
        auto comment = parseComment();
        if(keepComments) {
          throw imp("keeping comments between '=' and named arg");
        }
        continue;
      }
      
      // Quoted Strings
      if(c == '"') {
        auto stringLineNumber = state.lineNumber;
	auto string_ = parseQuoted();
	readChar();
	return new ConstructUtf8(stringLineNumber, string_);
      }

      // Numbers
      if(c >= '0' && c <= '9') {
        auto numberLineNumber = state.lineNumber;
        auto numberString = parseNumber();
        return new ConstructUint(numberLineNumber, to!size_t(numberString));
      }

      if(isFirstCharacterOfSymbol(c)) {
        auto symbolLineNumber = state.lineNumber;
        auto symbol = parseSymbol();
        return ConstructObject.fromUnquoted(symbolLineNumber, symbol);
      }
      
      if(c == ',') {
        throw new ConstructCFamilyParseException
          (ErrorType.invalidChar, state.lineNumber, "commas ',' (or list breaks) can only appear inside lists");
      }
      throw new ConstructCFamilyParseException
        (ErrorType.invalidChar, state.lineNumber,
         format("named construct object cannot start with '%s'", AsciiPrint(c)));
    }
  }

  // InputState:
  //   next points to character after opening quote
  // OutputState:
  //   next points to character after closing quote
  private final const(char)[] parseQuoted()
  {
    auto next = state.next;
    while(true) {
      if(next >= limit) {
	throw new ConstructCFamilyParseException
	  (ErrorType.endedEarly, state.lineNumber, "input ended inside a quoted string");
      }
      auto cpos = next;
      auto c = decodeUtf8(&next, limit);
      if(c == '\\') {
	imp("quoted strings with escapes");
      } else if(c == '"') {
	auto string_ = state.next[0..cpos-state.next];
	state.next = next;
	return string_;
      }
    }
  }

  // InputState:
  //   cpos points to first digit
  //   next points to character after first digit
  // OutputState:
  //   cpos points to character after number
  //   if(cpos < limit) {
  //     c is first character after number
  //     next points to character after c
  //   }
  private final const(char)[] parseNumber()
  {
    auto start = state.cpos;
    auto next = state.next;
    while(true) {
      if(next >= limit) {
        state.cpos = limit;
        return start[0..limit-start];
      }
      auto cpos = next;
      auto c = decodeUtf8(&next, limit);
      if(c < '0' || c > '9') {
        state.cpos = cpos;
        state.c = c;
        return start[0..cpos-start];
      }
    }
  }
  
  // InputState:
  //   cpos points to first character of symbol
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
  private final const(char)[] parseSymbol()
  {
    auto next = state.next;
    while(next < limit) {
      auto cpos = next;
      auto c = decodeUtf8(&next, limit);
      if(!isSymbolCharacter(c)) {
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
  
  // InputState:
  //   cpos points to first character of comment '/'
  //   next points to next character
  // OutputState:
  //   next points to character after comment
  private final const(char)[] parseComment()
  {
    readChar();
    if(state.cpos >= limit) {
      throw new ConstructCFamilyParseException
	(ErrorType.endedEarly, state.lineNumber, "input ended wth '/'");
    }
    auto next = state.next;
    auto commentStart = next;
    if(state.c == '/') {
      // skip till end of line
      while(1) {
	if(next >= limit) {
	  state.next = limit;
	  return commentStart[0..next-commentStart];
	}
	auto c = decodeUtf8(&next, limit);
	if(c == '\n') {
	  state.lineNumber++;
	  state.next = next;
	  return commentStart[0..next-1-commentStart];
	}
      }
    }

    if(state.c == '*') {
      // skip till end of multiline comment
      bool lastCharWasStar = false;
      while(1) {
	if(next >= limit) {
	  throw new ConstructCFamilyParseException(ErrorType.endedEarly, state.lineNumber,
						   "multi-line comment was not ended");
	}
	auto c = decodeUtf8(&next, limit);
	if(c == '*') {
	  lastCharWasStar = true;
	  continue;
	}
	    
	if(c == '/') {
	  if(lastCharWasStar) {
	    state.next = next;
	    return commentStart[0..next-2-commentStart];
	  }
	} else if(c == '\n') {
	  state.lineNumber++;
	}
	lastCharWasStar = false;
      }
    }

    throw new ConstructCFamilyParseException
      (ErrorType.invalidChar, state.lineNumber, format
       ("expected '*' or '/' after '/', but got '%s'", AsciiPrint(state.c)));
  }

  final void debugCurrentState() const {
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

unittest
{
  void test(const(char)[] code, ConstructObject[] expected = null, size_t testLine = __LINE__)
  {
    writeln("---------------------------------------");
    writefln("[TEST] \"%s\"", AsciiPrintString(code));
    stdout.flush();
    auto codeTree = parse!(Appender!(const(ConstructObject)[]))(code);
    if(expected) {
      if(codeTree.length != expected.length) {
	writefln("Expected: %s", expected);
	writefln("Actual  : %s", codeTree);
	assert(0, format("Expected %s construct objects but got %s (testline %s)",
                         expected.length, codeTree.length, testLine));
      }
      foreach(i; 0..expected.length) {
	if(!codeTree[i].equals(expected[i])) {
	  writefln("Expected: %s", expected[i]);
	  writefln("Actual  : %s", codeTree[i]);
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
      auto codeTree = parse!(Appender!(const(ConstructObject)[]))(code);
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

  // helper functions
  ObjectBreak break_(size_t lineNumber = 1)
  {
    __gshared static cached = new ObjectBreak(1);
    return (lineNumber == 1) ? cached : new ObjectBreak(lineNumber);
  }
  ListBreak listBreak(size_t lineNumber = 1)
  {
    __gshared static cached = new ListBreak(1);
    return (lineNumber == 1) ? cached : new ListBreak(lineNumber);
  }
  ConstructBlock block(size_t lineNumber, ConstructObject[] objects ...)
  {
    return new ConstructBlock(lineNumber, objects);
  }
  ConstructList list(size_t lineNumber, ConstructObject[] objects ...)
  {
    return new ConstructList(lineNumber, objects);
  }
  ConstructSymbol symbol(size_t lineNumber, const(char)[] value)
  {
    return new ConstructSymbol(lineNumber, value);
  }
  alias prim = PrimitiveTypeEnum;
  PrimitiveType type(size_t lineNumber, PrimitiveTypeEnum typeEnum)
  {
    return new PrimitiveType(lineNumber, typeEnum);
  }
  ConstructString string_(size_t lineNumber, string value)
  {
    return new ConstructString(lineNumber, value);
  }
  ConstructNamedObject named(size_t lineNumber, string name, ConstructObject object)
  {
    return new ConstructNamedObject(lineNumber, name, object);
  }
  ConstructNull null_(size_t lineNumber)
  {
    return new ConstructNull(lineNumber);
  }

  
  test("", []);
  test(";", [break_]);
  test(" ;", [break_]);
  test("; ", [break_]);
  test("  ; ", [break_]);

  testError(ErrorType.invalidChar, "}");
  testError(ErrorType.invalidChar, ")");

  testError(ErrorType.endedEarly, "{");
  testError(ErrorType.endedEarly, "(");
  testError(ErrorType.invalidChar, "{)");
  testError(ErrorType.invalidChar, "(}");
  
  test("{}", [block(1)]);
  test("()", [list(1)]);
  test("{;}", [block(1, break_)]);
  test("(;)", [list(1, break_)]);
  test("(,)", [list(1, listBreak)]);
  
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
    if(!isFirstCharacterOfSymbol(i) &&
       i != ' ' && i != '\t' && i != '\r' && i != '\n' &&
       i != '/' && i != '(' && i != '{' && i != ';' && i != '"' &&
       !(i >= '0' && i <= '9')) {
      char c = cast(char)i;
      testError(ErrorType.invalidChar, (cast(char*)&c)[0..1]);
    }
  }

  test("a", [symbol(1, "a")]);
  test("ab", [symbol(1, "ab")]);
  test("abc", [symbol(1, "abc")]);
  test("abc ", [symbol(1, "abc")]);
  test("abc // comment", [symbol(1, "abc")]);
  test("abc // comment\n", [symbol(1, "abc")]);

  test("a;", [symbol(1, "a"), break_]);
  test("ab;", [symbol(1, "ab"), break_]);
  test("abasdfjkl ;", [symbol(1, "abasdfjkl"), break_]);
  test("abc; // comment", [symbol(1, "abc"), break_]);

  testError(ErrorType.invalidChar, "abc #");
  test("function myfunc", [symbol(1, "function"), symbol(1, "myfunc")]);
  test("function myfunc;", [symbol(1, "function"), symbol(1, "myfunc"), break_]);
  test("function /*comment*/ myfunc /* comment2 */;", [symbol(1, "function"), symbol(1, "myfunc"), break_]);

  testError(ErrorType.endedEarly, "a (;");
  testError(ErrorType.invalidChar, "a )");

  test("a ()", [symbol(1, "a"), list(1)]);
  test("a();"  , [symbol(1, "a"), list(1), break_]);
  test("a ();" , [symbol(1, "a"), list(1), break_]);
  test("a () ;", [symbol(1, "a"), list(1), break_]);

  testError(ErrorType.endedEarly, "a(first");
  test("a(first)", [symbol(1, "a"), list(1, [symbol(1, "first")])]);
  test("a(first)", [symbol(1, "a"), list(1, [symbol(1, "first")])]);
  test("a (first);", [symbol(1, "a"),list(1, symbol(1, "first")), break_]);
  test("a ( first )  ;", [symbol(1, "a"),list(1, symbol(1, "first")), break_]);
  test("a /*comment*/(/*another*/first/*what*/)/*hey*/;", [symbol(1, "a"),list(1, symbol(1, "first")), break_]);

  test("a (first second);", [symbol(1, "a"), list(1, symbol(1, "first"), symbol(1, "second")), break_]);

  testError(ErrorType.invalidChar, "/a");
  testError(ErrorType.invalidChar, "a/;");
  testError(ErrorType.endedEarly, "a/");
  testError(ErrorType.endedEarly, "abc/");
  testError(ErrorType.invalidChar, "abc/;");
  testError(ErrorType.invalidChar, "abc/a");
  testError(ErrorType.invalidChar, "abc/string");
  testError(ErrorType.invalidChar, "/ a ");
  testError(ErrorType.invalidChar, "//*\n*/");
  testError(ErrorType.endedEarly, "/*/");
  test("/**/");
  test("/***/");

  test("abc ();", [symbol(1, "abc"), list(1), break_]);
  test("abc (a def);", [symbol(1, "abc"), list(1, symbol(1, "a"), symbol(1, "def")), break_]);

  test("a ;", [symbol(1, "a"), break_]);
  test("a {}", [symbol(1, "a"), block(1)]);
  test("a {b;c;} ;", [symbol(1, "a"), block(1, symbol(1, "b"), break_, symbol(1, "c"), break_), break_]);
  test("a ({b;c;}) ;", [symbol(1, "a"), list(1, block(1, symbol(1, "b"), break_, symbol(1, "c"), break_)), break_]);
  test("a ( ({b;c;}) ) ;", [symbol(1, "a"), list(1, list(1, block(1, symbol(1, "b"), break_, symbol(1, "c"), break_))), break_]);
  test("a ({b;c;}) {}", [symbol(1, "a"), list(1, block(1, symbol(1, "b"), break_, symbol(1, "c"), break_)), block(1)]);
  // Primitive  types
  test("a int;" , [symbol(1, "a"), type(1, prim.int_), break_]);
  test("a uint;" , [symbol(1, "a"), type(1, prim.uint_), break_]);

  // Quoted Strings
  testError(ErrorType.endedEarly, "a \"");
  testError(ErrorType.endedEarly, "a \" ");
  test("a \"a string!\";", [symbol(1, "a"), string_(1, "a string!"), break_]);

  // List Breaks
  test("a (c,d,e);", [symbol(1, "a"), list(1, symbol(1, "c"), listBreak,
                                           symbol(1, "d"), listBreak,
                                           symbol(1, "e")), break_]);

  // Named Args
  testError(ErrorType.endedEarly, "a=");
  testError(ErrorType.endedEarly, "a= ");
  testError(ErrorType.endedEarly, "a =");
  testError(ErrorType.endedEarly, " a=");
  testError(ErrorType.endedEarly, "a = ");
  testError(ErrorType.endedEarly, " a =");
  testError(ErrorType.endedEarly, " a = ");
  testError(ErrorType.endedEarly, "a=//");
  testError(ErrorType.endedEarly, "a=/**/");
  testError(ErrorType.invalidChar, "a=,");

  test("a=;", [named(1, "a", break_)]);
  test("a =;", [named(1, "a", break_)]);
  test("a= ;", [named(1, "a", break_)]);
  test("a={}", [named(1, "a", block(1))]);
  test("a ={}", [named(1, "a", block(1))]);
  test("a= { } ", [named(1, "a", block(1))]);
  test("a = {b c} ", [named(1, "a", block(1, symbol(1, "b"), symbol(1, "c")))]);
  test("a = { b c } ", [named(1, "a", block(1, symbol(1, "b"), symbol(1, "c")))]);

  test("a=()", [named(1, "a", list(1))]);
  test("a =()", [named(1, "a", list(1))]);
  test("a= ( ) ", [named(1, "a", list(1))]);
  test("a = (b c) ", [named(1, "a", list(1, symbol(1, "b"), symbol(1, "c")))]);
  test("a = ( b c ) ", [named(1, "a", list(1, symbol(1, "b"), symbol(1, "c")))]);
  test("a = (b,c)", [named(1, "a", list(1, symbol(1, "b"), listBreak, symbol(1, "c")))]);

  test("a=/*comment*/;", [named(1, "a", break_)]);
  test("a=/*comment*/ ;", [named(1, "a", break_)]);
  test("a=/*comment*/{}", [named(1, "a", block(1))]);
  test("a=/*comment*/ {}", [named(1, "a", block(1))]);
  test("a=/*comment*/()", [named(1, "a", list(1))]);
  test("a=/*comment*/ ()", [named(1, "a", list(1))]);
  test("a=/*comment*/b", [named(1, "a", symbol(1, "b"))]);
  
  test("a=//comment\n;", [named(1, "a", break_(2))]);
  test("a=//comment\n ;", [named(1, "a", break_(2))]);
  test("a=//comment\n{}", [named(1, "a", block(2))]);
  test("a=//comment\n {}", [named(1, "a", block(2))]);
  test("a=//comment\n()", [named(1, "a", list(2))]);
  test("a=//comment\n ()", [named(1, "a", list(2))]);
  test("a=//comment\nb", [named(1, "a", symbol(2, "b"))]);
  
  testError(ErrorType.endedEarly, "a=\"");
  testError(ErrorType.endedEarly, "a=\" ");
  test("a=\"a string!\";", [named(1, "a", string_(1, "a string!")), break_]);





  /*
  testError(ErrorType.invalidChar, "a b=/!");
  testError(ErrorType.invalidChar, "a b=/\"");
  testError(ErrorType.invalidChar, "a b=/\"hello");
  testError(ErrorType.invalidChar, "a b=/\"\"");
  testError(ErrorType.invalidChar, "a b=/\"hello\"");
  
  testError(ErrorType.constructEndedEarly, "a b=;");

  testError(ErrorType.argsWithSameName, "a b=() b=");
  testError(ErrorType.argsWithSameName, "a b=() b=d;");
  
  test("a b=();", [new Construct(1, "a", null, ["b": list(1)])]);
  test("a b=(c d);", [new Construct(1, "a", null, ["b": list(1, [symbol(1, "c"), symbol(1, "d")])])]);
  test("a b=c;", [new Construct(1, "a", null, ["b": symbol(1, "c")])]);
  test("a b={};", [new Construct(1, "a", null, ["b": block(1, null)])]);
  test("a b=/hey;", [new Construct(1, "a", null, ["b": new Construct(1, "hey")])]);
  */

  // Regression Tests
  test("null", [null_(1)]);
  test("null;", [null_(1), break_]);
  test("thrownError = null;", [named(1, "thrownError", null_(1)), break_]);
  test("let thrownError = null;", [symbol(1, "let"), named(1, "thrownError", null_(1)), break_]);

  // Numbers
  test("0", [new ConstructUint(1, 0)]);
  test(" 0", [new ConstructUint(1, 0)]);
  test("0 ", [new ConstructUint(1, 0)]);
  test("1", [new ConstructUint(1, 1)]);
  test("2", [new ConstructUint(1, 2)]);
  test("3", [new ConstructUint(1, 3)]);
  test("4", [new ConstructUint(1, 4)]);
  test("5", [new ConstructUint(1, 5)]);
  test("6", [new ConstructUint(1, 6)]);
  test("7", [new ConstructUint(1, 7)]);
  test("8", [new ConstructUint(1, 8)]);
  test("9", [new ConstructUint(1, 9)]);
  test("10", [new ConstructUint(1, 10)]);
  test(" 10", [new ConstructUint(1, 10)]);
  test("10 ", [new ConstructUint(1, 10)]);

  test("a=0", [named(1, "a", new ConstructUint(1, 0))]);
  test("a=0 ", [named(1, "a", new ConstructUint(1, 0))]);
  test("a= 0", [named(1, "a", new ConstructUint(1, 0))]);
  test("a =0", [named(1, "a", new ConstructUint(1, 0))]);
  test(" a=0", [named(1, "a", new ConstructUint(1, 0))]);
  test("a= 0 ", [named(1, "a", new ConstructUint(1, 0))]);
  test("a =0 ", [named(1, "a", new ConstructUint(1, 0))]);
  test(" a=0 ", [named(1, "a", new ConstructUint(1, 0))]);
  test("a= 0", [named(1, "a", new ConstructUint(1, 0))]);
  test("a = 0", [named(1, "a", new ConstructUint(1, 0))]);
  test(" a= 0", [named(1, "a", new ConstructUint(1, 0))]);
  test("a =0", [named(1, "a", new ConstructUint(1, 0))]);
  test(" a =0", [named(1, "a", new ConstructUint(1, 0))]);
}

