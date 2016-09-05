/*
Grammar
===========================================
construct ::= ( nameSymbol | operatorSymbol | number | string | block | list | ';' )*
block ::= '{' construct '}'
list ::= '(' (construct | ',')* ')'
string ::= '"' chars '"'
symbolString ::= "'" nameSymbol
*/

module construct.standardParser;

import std.format : format, formattedWrite;
import std.array  : appender, Appender;
import std.file   : read, exists, mkdir;
import std.path   : baseName, dirName, buildNormalizedPath;
import std.conv   : to;
import construct.util;
import construct.parserCore;

import utf8;

version(unittest) {
  //version = VerboseTests;
  import std.stdio  : writeln, writefln, stdout;
}

// A wrapper that will print any character in a human readable format
// only using ascii characters
struct AsciiPrint
{
  dchar c;
  void toString(Sink)(scope Sink sink) const
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
  void toString(Sink)(scope Sink sink) const
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
  this(ErrorType type, size_t constructLineNumber, string msg,
       string codeFile = __FILE__, size_t codeLine = __LINE__) pure {
    super(constructLineNumber, msg, codeFile, codeLine);
    this.type = type;
  }
}

//
// How operators are handle currently
// -----------------------------------------------
// All valid operator characters that appear in sequence are considered
// one operator
//

enum CharFlags {
  // Flags
  validOperatorChar = 0x01,

  // Canned Values
  none = 0,
}
CharFlags lookupCharFlags(dchar c) pure
{
  return (c < charFlagsTable.length) ? charFlagsTable[c] : CharFlags.none;
}
immutable CharFlags[] charFlagsTable =
  [
   CharFlags.none, // 0 ''
   CharFlags.none, // 1 ''
   CharFlags.none, // 2 ''
   CharFlags.none, // 3 ''
   CharFlags.none, // 4 ''
   CharFlags.none, // 5 ''
   CharFlags.none, // 6 ''
   CharFlags.none, // 7 ''
   CharFlags.none, // 8 ''
   CharFlags.none, // 9 ''
   CharFlags.none, // 10 ''
   CharFlags.none, // 11 ''
   CharFlags.none, // 12 ''
   CharFlags.none, // 13 ''
   CharFlags.none, // 14 ''
   CharFlags.none, // 15 ''
   CharFlags.none, // 16 ''
   CharFlags.none, // 17 ''
   CharFlags.none, // 18 ''
   CharFlags.none, // 19 ''
   CharFlags.none, // 20 ''
   CharFlags.none, // 21 ''
   CharFlags.none, // 22 ''
   CharFlags.none, // 23 ''
   CharFlags.none, // 24 ''
   CharFlags.none, // 25 ''
   CharFlags.none, // 26 ''
   CharFlags.none, // 27 ''
   CharFlags.none, // 28 ''
   CharFlags.none, // 29 ''
   CharFlags.none, // 30 ''
   CharFlags.none, // 31 ''
   CharFlags.none, // 32 ' ' (space)
   CharFlags.validOperatorChar, // 33 '!'
   CharFlags.none, // 34 '"'
   CharFlags.validOperatorChar, // 35 '#'
   CharFlags.validOperatorChar, // 36 '$'
   CharFlags.validOperatorChar, // 37 '%'
   CharFlags.validOperatorChar, // 38 '&'
   CharFlags.none, // 39 '\''
   CharFlags.none, // 40 '('
   CharFlags.none, // 41 ')'
   CharFlags.validOperatorChar, // 42 '*'
   CharFlags.validOperatorChar, // 43 '+'
   CharFlags.none, // 44 ','
   CharFlags.validOperatorChar, // 45 '-'
   CharFlags.validOperatorChar, // 46 '.'
   CharFlags.validOperatorChar, // 47 '/'
   CharFlags.none, // 48 '0'
   CharFlags.none, // 49 '1'
   CharFlags.none, // 50 '2'
   CharFlags.none, // 51 '3'
   CharFlags.none, // 52 '4'
   CharFlags.none, // 53 '5'
   CharFlags.none, // 54 '6'
   CharFlags.none, // 55 '7'
   CharFlags.none, // 56 '8'
   CharFlags.none, // 57 '9'
   CharFlags.validOperatorChar, // 58 ':'
   CharFlags.none, // 59 ';'
   CharFlags.validOperatorChar, // 60 '<'
   CharFlags.validOperatorChar, // 61 '='
   CharFlags.validOperatorChar, // 62 '>'
   CharFlags.validOperatorChar, // 63 '?'
   CharFlags.validOperatorChar, // 64 '@'
   CharFlags.none, // 65 'A'
   CharFlags.none, // 66 'B'
   CharFlags.none, // 67 'C'
   CharFlags.none, // 68 'D'
   CharFlags.none, // 69 'E'
   CharFlags.none, // 70 'F'
   CharFlags.none, // 71 'G'
   CharFlags.none, // 72 'H'
   CharFlags.none, // 73 'I'
   CharFlags.none, // 74 'J'
   CharFlags.none, // 75 'K'
   CharFlags.none, // 76 'L'
   CharFlags.none, // 77 'M'
   CharFlags.none, // 78 'N'
   CharFlags.none, // 79 'O'
   CharFlags.none, // 80 'P'
   CharFlags.none, // 81 'Q'
   CharFlags.none, // 82 'R'
   CharFlags.none, // 83 'S'
   CharFlags.none, // 84 'T'
   CharFlags.none, // 85 'U'
   CharFlags.none, // 86 'V'
   CharFlags.none, // 87 'W'
   CharFlags.none, // 88 'X'
   CharFlags.none, // 89 'Y'
   CharFlags.none, // 90 'Z'
   CharFlags.none, // 91 '['
   CharFlags.none, // 92 '\\'
   CharFlags.none, // 93 ']'
   CharFlags.validOperatorChar, // 94 '^'
   CharFlags.none, // 95 '_'
   CharFlags.none, // 96 '`'
   CharFlags.none, // 97 'a'
   CharFlags.none, // 98 'b'
   CharFlags.none, // 99 'c'
   CharFlags.none, // 100 'd'
   CharFlags.none, // 101 'e'
   CharFlags.none, // 102 'f'
   CharFlags.none, // 103 'g'
   CharFlags.none, // 104 'h'
   CharFlags.none, // 105 'i'
   CharFlags.none, // 106 'j'
   CharFlags.none, // 107 'k'
   CharFlags.none, // 108 'l'
   CharFlags.none, // 109 'm'
   CharFlags.none, // 110 'n'
   CharFlags.none, // 111 'o'
   CharFlags.none, // 112 'p'
   CharFlags.none, // 113 'q'
   CharFlags.none, // 114 'r'
   CharFlags.none, // 115 's'
   CharFlags.none, // 116 't'
   CharFlags.none, // 117 'u'
   CharFlags.none, // 118 'v'
   CharFlags.none, // 119 'w'
   CharFlags.none, // 120 'x'
   CharFlags.none, // 121 'y'
   CharFlags.none, // 122 'z'
   CharFlags.none, // 123 '{'
   CharFlags.validOperatorChar, // 124 '|'
   CharFlags.none, // 125 '}'
   CharFlags.validOperatorChar, // 126 '~'
   CharFlags.validOperatorChar, // 127 '' (DEL)
   ];

private bool isFirstCharacterOfSymbol(dchar c) pure
{
  if(c >= 'A') {
    if(c <= 'Z') return true;
    if(c < 'a') return c == '_';
    return c <= 'z';
  }
  return false;
}
private bool isSymbolCharacter(dchar c) pure
{
  if(c >= 'A') {
    if(c <= 'Z') return true;
    if(c < 'a') return c == '_';
    return c <= 'z';
  }
  if(c < '0') {
    return false;
  }
  return (c <= '9');
}

const(ConstructObject)[] parse(T)(string code) pure if(isConstructBuilder!(T))
{
  auto consumer = ConstructConsumer!T(code.ptr, code.ptr + code.length);
  return consumer.parse();
}


struct ConstructConsumer(T) if(isConstructBuilder!(T))
{
  immutable(char)* limit;
  const bool keepComments;

  // The fields that change
  struct State
  {
    size_t lineNumber;
    immutable(char)* next;
    immutable(char)* cpos;
    dchar c;
  }
  State state;

  this(immutable(char)* start, immutable char* limit, bool keepComments = false) pure
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
  final void skipWhitespace() pure
  {
    dchar c;
    immutable(char)* cpos;
    immutable(char)* next = state.next;

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
  final const(ConstructObject)[] parse() pure
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
  private final void readChar() pure
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
  private final void parse(char context, ref T objects) pure
  {

    while(true) {
      skipWhitespace();
      if(state.cpos >= limit) {
	break;
      }

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

      // Handle Operator Symbols
      {
        auto charFlags = lookupCharFlags(c);
        if(charFlags & CharFlags.validOperatorChar) {
          auto opLineNumber = state.lineNumber;
          auto operator = parseOperator();
          objects.put(new ConstructSymbol(opLineNumber, operator));
          state.next = state.cpos; // rewind
          continue;
        }
      }

      // Quoted Strings
      if(c == '"') {
        auto stringLineNumber = state.lineNumber;
	auto string_ = parseQuoted();
	objects.put(new ConstructUtf8(stringLineNumber, string_));
	continue;
      }

      // Symbol Strings
      if(c == '\'') {
	auto stringLineNumber = state.lineNumber;
	if(state.next >= limit) {
	  throw new ConstructCFamilyParseException
	    (ErrorType.endedEarly, state.lineNumber, "expected symbol after ' but reached end of input");
	}
	readChar();
	if(!isFirstCharacterOfSymbol(state.c)) {
	  throw new ConstructCFamilyParseException
	    (ErrorType.invalidChar, state.lineNumber, format("expected symbol character after ' but got '%s'", AsciiPrint(state.c)));
	}
	auto symbol = parseSymbol();
	objects.put(new ConstructUtf8(stringLineNumber, symbol));
	state.next = state.cpos; // rewind
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
	  objects.put(new ConstructSymbol(symbolLineNumber, symbol));
	  break;
	}
	objects.put(new ConstructSymbol(symbolLineNumber, symbol));
	state.next = state.cpos; // rewind
	continue;
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
  //   next points to character after opening quote
  // OutputState:
  //   next points to character after closing quote
  private final string parseQuoted() pure
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
  private final string parseNumber() pure
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
  private final string parseSymbol() pure
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
  //   cpos points to first character of symbol
  //   next points to character after c
  //   Note: it is assumed that the caller already checked that
  //         the character pointed to by cpos is a valid operator character
  // OutputState:
  //   cpos points to character after operator
  //   if(cpos < limit) {
  //     c is first character after name
  //     next points to character after c
  //   }
  private final string parseOperator() pure
  {
    auto next = state.next;
    while(next < limit) {
      auto cpos = next;
      auto c = decodeUtf8(&next, limit);
      auto flags = lookupCharFlags(c);
      if(!(flags & CharFlags.validOperatorChar)) {
	auto operator = state.cpos[0..cpos-state.cpos];
	state.cpos = cpos;
	state.c = c;
	state.next = next;
	return operator;
      }
    }

    // End of input case (only need to set cpos)
    auto operator = state.cpos[0..next-state.cpos];
    state.cpos = next;
    return operator;
  }

  // InputState:
  //   cpos points to first character of comment '/'
  //   next points to next character
  // OutputState:
  //   next points to character after comment
  private final string parseComment() pure
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

  /*
  final void debugCurrentState() const pure {
    debug writefln("c    : '%s'", AsciiPrint(state.c));
    if(state.cpos) {
      if(state.cpos >= limit) {
        debug writefln("cpos : %s (limit)", state.cpos);
      } else {
        debug writefln("cpos : %s -> '%s'", state.cpos, AsciiPrint(*state.cpos));
      }
    } else {
      debug writefln("cpos : null", state.cpos);
    }
    if(state.next) {
      if(state.next >= limit) {
        debug writefln("next : %s (limit)", state.next);
      } else {
        debug writefln("next : %s -> '%s'", state.next, AsciiPrint(*state.next));
      }
    } else {
      debug writefln("next : null", state.next);
    }
    debug stdout.flush();
  }
  */
}

unittest
{
  void test(string code, ConstructObject[] expected = null, size_t testLine = __LINE__)
  {
    version(VerboseTests) {
      debug writeln("---------------------------------------");
      debug writefln("[TEST] \"%s\"", AsciiPrintString(code));
      stdout.flush();
    }
    auto codeTree = parse!(Appender!(const(ConstructObject)[]))(code);
    if(expected) {
      if(codeTree.length != expected.length) {
	debug writefln("Expected: %s", expected);
	debug writefln("Actual  : %s", codeTree);
	assert(0, format("Expected %s construct objects but got %s (testline %s)",
                         expected.length, codeTree.length, testLine));
      }
      foreach(i; 0..expected.length) {
	if(!codeTree[i].equals(expected[i])) {
	  debug writefln("Expected: %s", expected[i]);
	  debug writefln("Actual  : %s", codeTree[i]);
	  debug stdout.flush();
	  assert(0, format("Construct at index %s does not match (testline %s)", i, testLine));
	}
      }
    }
  }

  void testError(ErrorType expectedError, const(char)[] code, size_t testLine = __LINE__)
  {
    version(VerboseTests) {
      writeln("---------------------------------------");
      writefln("[TEST-ERROR] (%s) \"%s\"", expectedError, AsciiPrintString(code));
      stdout.flush();
    }
    try {
      auto codeTree = parse!(Appender!(const(ConstructObject)[]))(cast(string)code);
      assert(0, format("Expected exception '%s' but did not get one. (testline %s) Code=\"%s\"",
		       expectedError, testLine, AsciiPrintString(code)));
    } catch(ConstructCFamilyParseException e) {
      if(e.type != expectedError) {
	debug writefln("WrongException: %s", e);
	debug stdout.flush();
	assert(0, format("Expected error '%s' but got '%s' (testline %s)", expectedError, e.type, testLine));
      }
      version(VerboseTests) {
	writefln("[TEST-ERROR] Successfully got error: %s", e.msg);
      }
    }
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
  test(".notAArg", [symbol(1, "."), symbol(1, "notAArg")]);
  testError(ErrorType.invalidChar, "[notAParm]");
  testError(ErrorType.invalidChar, "}");

  for(uint i = 0; i < 128; i++) {
    auto charFlags = lookupCharFlags(i);
    auto isOperatorChar = (charFlags & CharFlags.validOperatorChar);
    if(!isFirstCharacterOfSymbol(i) && !isOperatorChar &&
       i != ' ' && i != '\t' && i != '\r' && i != '\n' && i != '\'' &&
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

  test("abc #", [symbol(1,"abc"),symbol(1,"#")]);
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
  testError(ErrorType.invalidChar, "//*\n)");
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
  test("a int;" , [symbol(1, "a"), symbol(1, "int"), break_]);
  test("a uint;" , [symbol(1, "a"), symbol(1, "uint"), break_]);

  // Quoted Strings
  testError(ErrorType.endedEarly, "a \"");
  testError(ErrorType.endedEarly, "a \" ");
  test("a \"a string!\";", [symbol(1, "a"), string_(1, "a string!"), break_]);

  // Symbol Strings
  testError(ErrorType.endedEarly, "'");
  {
    char[2] str;
    str[0] = '\'';
    foreach(i; 0..128) {
      if(!isFirstCharacterOfSymbol(i)) {
	str[1] = cast(char)i;
	testError(ErrorType.invalidChar, str);
      }
    }
  }
  test("'a", [string_(1, "a")]);
  test("'abc", [string_(1, "abc")]);
  test("'a ", [string_(1, "a")]);
  test("'abc ", [string_(1, "abc")]);
  test("'a //", [string_(1, "a")]);
  test("'a /**/", [string_(1, "a")]);
  test("'abc /**/", [string_(1, "abc")]);

  // List Breaks
  test("a (c,d,e);", [symbol(1, "a"), list(1, symbol(1, "c"), listBreak,
                                           symbol(1, "d"), listBreak,
                                           symbol(1, "e")), break_]);

  /+
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
  +/

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
  test("null", [symbol(1, "null")]);
  test("null;", [symbol(1, "null"), break_]);
  //test("thrownError = null;", [named(1, "thrownError", symbol(1, "null")), break_]);
  //test("let thrownError = null;", [symbol(1, "let"), named(1, "thrownError", symbol(1, "null")), break_]);

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

  // Test Operators
  test("a.b", [symbol(1,"a"),symbol(1,"."),symbol(1,"b")]);
  test("a=0", [symbol(1,"a"),symbol(1,"="),new ConstructUint(1,0)]);
  test("a = 0", [symbol(1,"a"),symbol(1,"="),new ConstructUint(1,0)]);

  // Test C Operators
  test(".",   [symbol(1, ".")]);
  test("+",   [symbol(1, "+")]);
  test("-",   [symbol(1, "-")]);
  test("*",   [symbol(1, "*")]);
  // test("/", [symbol(1, "/")]); TODO: FIX THIS CASE (problem with comments)
  test("%",   [symbol(1, "%")]);
  test("++",  [symbol(1, "++")]);
  test("--",  [symbol(1, "--")]);
  test("==",  [symbol(1, "==")]);
  test("!=",  [symbol(1, "!=")]);
  test(">",   [symbol(1, ">")]);
  test("<",   [symbol(1, "<")]);
  test(">=",  [symbol(1, ">=")]);
  test("<=",  [symbol(1, "<=")]);
  test("&&",  [symbol(1, "&&")]);
  test("||",  [symbol(1, "||")]);
  test("!",   [symbol(1, "!")]);
  test("&",   [symbol(1, "&")]);
  test("|",   [symbol(1, "|")]);
  test("^",   [symbol(1, "^")]);
  test("~",   [symbol(1, "~")]);
  test("<<",  [symbol(1, "<<")]);
  test(">>",  [symbol(1, ">>")]);
  test("=",   [symbol(1, "=")]);
  test("+=",  [symbol(1, "+=")]);
  test("-=",  [symbol(1, "-=")]);
  test("*=",  [symbol(1, "*=")]);
  //test("/=",  [symbol(1, "/=")]); TODO: FIX THIS CASE (problem with comments)
  test("%=",  [symbol(1, "%=")]);
  test("<<=", [symbol(1, "<<=")]);
  test(">>=", [symbol(1, ">>=")]);
  test("&=",  [symbol(1, "&=")]);
  test("^=",  [symbol(1, "^=")]);
  test("|=",  [symbol(1, "|=")]);
}

