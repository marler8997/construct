/*
Grammar
===========================================
construct ::= ( nameSymbol | operatorSymbol | number | string | codeInsidePairedOperators )*
codeInsidePairedSymbols ::= '{' construct '}' | '(' construct ')' | '[' construct '];
string ::= '"' chars '"'
symbolString ::= "'" nameSymbol
*/

module construct.standardParser;

import std.format : format, formattedWrite;
import std.array  : appender, Appender;
import std.file   : read, exists, mkdir;
import std.path   : baseName, dirName, buildNormalizedPath;
import std.conv   : to;
import std.bigint : BigInt;

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
      sink((&(cast(char)c))[0..1]);
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
  unknownEscapeChar,
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
enum CharOperatorKind : ubyte {
  none = 0,
  detached = 1,  // i.e. ",", "#"
  attachable = 2,// i.e. "<", "=", "!"
  leftPair = 3,  // i.e. "(", "{", "["
  rightPair = 4, // i.e. ")", "}", "]"
}
enum CharFlags : ubyte {
  none = 0x00,
  
  // Operator flags
  operatorKindMask = 0x07, // 0000 0111
  detachedOperator      = CharOperatorKind.detached, 
  attachableOperator    = CharOperatorKind.attachable,
  leftPairOperator      = CharOperatorKind.leftPair,
  rightPairOperator     = CharOperatorKind.rightPair
}
CharOperatorKind getOperatorKind(CharFlags flags) pure
{
  auto value = cast(CharOperatorKind)(flags & CharFlags.operatorKindMask);
  assert(value <= CharOperatorKind.max, "CodeBug: invalid character flags, operator flags are too big");
  return value;
}
char getRightOperator(char c) pure
{
  assert(c == '(' || c == '[' || c == '{', "code bug: getRightOperator got an invalid character");
  return (c == '(') ? ')' : cast(char)(c + 2);
}
DelimiterPair getDelimiterPair(char c) pure
{
  if(c == '{') return DelimiterPair.braces;
  if(c == '(') return DelimiterPair.parens;
  if(c == '[') return DelimiterPair.brackets;
  assert(0, format("CodeBug: character '%s' is not the start of a known delimiter pair", c));
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
   CharFlags.attachableOperator, // 33 '!'
   CharFlags.none, // 34 '"'
   CharFlags.detachedOperator, // 35 '#'
   CharFlags.detachedOperator, // 36 '$'
   CharFlags.detachedOperator, // 37 '%'
   CharFlags.attachableOperator, // 38 '&'
   CharFlags.none, // 39 '\''
   CharFlags.leftPairOperator, // 40 '('
   CharFlags.rightPairOperator, // 41 ')'
   CharFlags.detachedOperator, // 42 '*'
   CharFlags.attachableOperator, // 43 '+'
   CharFlags.detachedOperator, // 44 ','
   CharFlags.attachableOperator, // 45 '-'
   CharFlags.detachedOperator, // 46 '.'
   CharFlags.detachedOperator, // 47 '/'
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
   CharFlags.detachedOperator, // 58 ':'
   CharFlags.detachedOperator, // 59 ';'
   CharFlags.attachableOperator, // 60 '<'
   CharFlags.attachableOperator, // 61 '='
   CharFlags.attachableOperator, // 62 '>'
   CharFlags.detachedOperator, // 63 '?'
   CharFlags.detachedOperator, // 64 '@'
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
   CharFlags.leftPairOperator, // 91 '['
   CharFlags.none, // 92 '\\'
   CharFlags.rightPairOperator, // 93 ']'
   CharFlags.detachedOperator, // 94 '^'
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
   CharFlags.leftPairOperator, // 123 '{'
   CharFlags.attachableOperator, // 124 '|'
   CharFlags.rightPairOperator, // 125 '}'
   CharFlags.detachedOperator, // 126 '~'
   CharFlags.none, // 127 '' (DEL)
   ];

// TODO: add support for non-ascii symbols?
bool isValidSymbol(const(char)[] str) pure
{
  if(str.length == 0) {
    return false;
  }

  auto firstChar = str[0];

  //
  // Check if it is an operator symbol
  //
  auto firstCharFlags = lookupCharFlags(firstChar);
  auto firstCharOperatorKind = firstCharFlags.getOperatorKind();
  final switch(firstCharOperatorKind) {
  case CharOperatorKind.none:
    break;
  case CharOperatorKind.detached:
    return str.length == 1;
  case CharOperatorKind.attachable:
    foreach(c; str[1..$]) {
      if(lookupCharFlags(c).getOperatorKind() != CharOperatorKind.attachable) {
        return false; // starts with attachable operator chars, but then has a character that isn't one
      }
    }
    return true;
  case CharOperatorKind.leftPair:
    return str.length == 1;
  case CharOperatorKind.rightPair:
    return str.length == 1;
  }

  //
  // Check if it is a name symbol
  //
  if(isFirstCharacterOfSymbol(firstChar)) {
    foreach(c; str[1..$]) {
      if(!isSymbolCharacter(c)) {
        return false; // starts with name symbol but, then then has non-name-symbol chars
      }
    }
    return true; // it is a name symbol
  }

  return false; // the first character is not a valid symbol
}
unittest
{
  assert(isValidSymbol("a"));
  assert(isValidSymbol("abc"));
  assert(isValidSymbol("+"));
  assert(isValidSymbol("."));
  assert(!isValidSymbol("a."));
  assert(!isValidSymbol(".a"));
  assert(!isValidSymbol("abc."));
  assert(!isValidSymbol(".abc"));
}

// TODO: probably should make these next 2 functions use the character flags table
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

interface ConstructBuilder
{
  void put(const(ConstructObject)) pure;
  @property size_t currentIndex() pure;
  const(ConstructObject)[] trimFrom(size_t index) pure;
}

struct CustomAppender(T)
{
  private T[] buffer;
  private size_t count;
  void put(T object)
  {
    if(count >= buffer.length) {
      buffer.length *= 2;
    }
    buffer[count++] = object;
  }
  @property inout(T)[] data() inout
  {
    return buffer[0..count];
  }
  void shrinkTo(size_t newSize)
  {
    count = newSize;
  }
}

class AppenderConstructBuilder : ConstructBuilder
{
  // I get an assert in the compiler when I uncomment this
  //Appender!(const(ConstructObject)[]) appender;
  CustomAppender!(ConstructObject) appender;
  this(size_t initialSize) pure
  {
    appender.buffer = new ConstructObject[initialSize];
  }
  final void put(const(ConstructObject) object) pure
  {
    appender.put(object.unconst);
  }
  @property final size_t currentIndex() pure
  {
    return appender.data.length;
  }
  final const(ConstructObject)[] trimFrom(size_t index) pure
  {
    assert(index <= appender.data.length, "CodeBug: AppenderConstructBuilder.trimFrom");

    auto trimmedObjects = appender.data[index..$].dup;
    appender.shrinkTo(index);
    return trimmedObjects;
  }
}

const(ConstructObject)[] parse(string code) pure
{
  auto initialSize = code.length / 8; // assume an object about every 8 characters in the source
  if(initialSize < 32) {
    initialSize = 32; // minimum of 32
  }
  auto builder = new AppenderConstructBuilder(initialSize); 
  auto consumer = ConstructConsumer(builder, code.ptr, code.ptr+code.length);
  consumer.parse();
  return builder.appender.data;
}

struct ConstructConsumer
{
  ConstructBuilder builder;
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

  this(ConstructBuilder builder, immutable(char)* start, immutable char* limit, bool keepComments = false) pure
  {
    this.builder = builder;
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
  final void parse() pure
  {
    if(keepComments) {
      throw imp("keep comments");
    }
    parse(0);
  }

  // InputState:
  //   next points to first character
  //   context = '\0' (root), '}' (block)
  // OutputState:
  //   cpos points to next character
  //   if(cpos < limit) {
  //     c contains char pointed to by c
  //     next points to next char
  //   }
  private final void parse(char context) pure
  {
  NEXT_CHARACTER:
    while(true) {
      skipWhitespace();
      if(state.cpos >= limit) {
	break;
      }

      auto c = state.c;

      // Handle Comments
      if(c == '/') {
	auto commentLineNumber = state.lineNumber;
	auto comment = parseComment();
	if(keepComments) {
	  builder.put(new ConstructComment(commentLineNumber, comment));
	}
	continue;
      }

      // Quoted Strings
      if(c == '"') {
        auto stringLineNumber = state.lineNumber;
	auto string_ = parseQuoted();
	builder.put(new ConstructUtf8(stringLineNumber, string_));
	continue;
      }

      // Operator Characters
      auto charFlags = lookupCharFlags(c);
      auto operatorKind = charFlags.getOperatorKind();
      final switch(operatorKind) {
      case CharOperatorKind.none:
	break;
      case CharOperatorKind.detached: {
	  auto opLineNumber = state.lineNumber;
	  builder.put(new ConstructSymbol(opLineNumber, state.cpos[0..state.next-state.cpos]));
	}
	goto NEXT_CHARACTER;
      case CharOperatorKind.attachable: {
	  auto opLineNumber = state.lineNumber;
	  auto operator = parseOperator();
	  builder.put(new ConstructSymbol(opLineNumber, operator));
	  state.next = state.cpos; // rewind
	}
	goto NEXT_CHARACTER;
      case CharOperatorKind.leftPair: {
	  size_t saveIndex = builder.currentIndex();
	  parse((cast(char)c).getRightOperator());
          auto objects = builder.trimFrom(saveIndex);
          {
            ConstructDelimitedList list;
            if(c == '{') {
              list = new ConstructBlock(state.lineNumber, objects);
            } else if(c == '(') {
              list = new ConstructParenList(state.lineNumber, objects);
            } else if(c == '[') {
              list = new ConstructBracketList(state.lineNumber, objects);
            } else {
              assert(0, "codebug: unhandled leftPair operator");
            }
            builder.put(list);
          }
	}
	goto NEXT_CHARACTER;
      case CharOperatorKind.rightPair:
	if(context != c) {
	  if(context == 0) {
	    throw new ConstructCFamilyParseException
	      (ErrorType.invalidChar, state.lineNumber, format("extra close operator '%s'", c));
	  }
	  throw new ConstructCFamilyParseException
	    (ErrorType.invalidChar, state.lineNumber, format("expected close operator '%s', but got '%s'", context, c));
	}
	return;
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
	builder.put(new ConstructUtf8(stringLineNumber, symbol));
	state.next = state.cpos; // rewind
	continue;
      }

      // Numbers
      if(c >= '0' && c <= '9') {
        auto numberLineNumber = state.lineNumber;
        auto numberString = parseNumber();
        builder.put(new ConstructIntegerLiteral(numberLineNumber, to!BigInt(numberString)));
        state.next = state.cpos; // rewind
        continue;
      }

      if(isFirstCharacterOfSymbol(c)) {
        auto symbolLineNumber = state.lineNumber;
	auto symbol = parseSymbol();
	if(state.cpos >= limit) {
	  builder.put(new ConstructSymbol(symbolLineNumber, symbol));
	  break;
	}
	builder.put(new ConstructSymbol(symbolLineNumber, symbol));
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

  static string createUnescapedString(size_t lineNumber, string str, size_t escapeRemoveCount) pure
  {
    // Doesn't need to decode to UTF-8 because all escape sequences will be in 1-byte characters only
    char[] newString = new char[str.length - escapeRemoveCount];
    size_t newStringIndex = 0;
    immutable char* limit = str.ptr + str.length;
    for(auto next = str.ptr; next < limit; next++) {
      char c = *next;
      if(c == '\\') {
        assert(next < limit, "code bug in createUnescapedString");
        next++;
        c = *next;
        if(c == '\\') {
          c = '\\';
        } else if(c == '"') {
          c = '"';
        } else if(c == 'n') {
          c = '\n';
        } else if(c == 't') {
          c = '\t';
        } else {
          throw new ConstructCFamilyParseException(ErrorType.unknownEscapeChar, lineNumber, format
                                                   ("unknown escape character '%s' (0x%x)", c, c));
        }
      }
      if(newStringIndex >= newString.length) {
        throw new Exception(format("  string(%s) '%s', escapeRemoveCount %s, newStringIndex %s, c '%s'", str, str.length, escapeRemoveCount, newStringIndex, c));
      }
      newString[newStringIndex++] = c;
    }
    // sanity checks
    assert(newStringIndex == newString.length, "code bug in createUnescapedString");
    return cast(string)newString;
  }
  
  // InputState:
  //   next points to character after opening quote
  // OutputState:
  //   next points to character after closing quote
  private final string parseQuoted() pure
  {
    auto next = state.next;
    size_t escapeRemoveCount = 0;
    while(true) {
      if(next >= limit) {
	throw new ConstructCFamilyParseException
	  (ErrorType.endedEarly, state.lineNumber, "input ended inside a quoted string");
      }
      auto cpos = next;
      auto c = decodeUtf8(&next, limit);
      if(c == '\\') {
        if(next >= limit) {
          throw new ConstructCFamilyParseException
            (ErrorType.endedEarly, state.lineNumber, "input ended inside a quoted string");
        }
        next++;
        // Note: all escape characters will be 1-byte values so no need
        //       to decode the escape character as UTF-8
        escapeRemoveCount++;
      } else if(c == '"') {
        string string_ = state.next[0..cpos-state.next];
        if(escapeRemoveCount) {
          string_ = createUnescapedString(state.lineNumber, string_, escapeRemoveCount);
        }
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
      auto operatorKind = lookupCharFlags(c).getOperatorKind();
      if(operatorKind != CharOperatorKind.attachable) {
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
    auto codeTree = parse(code);
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

  void testError(ErrorType expectedError, const(char)[] code, size_t testLine = __LINE__)
  {
    version(VerboseTests) {
      writeln("---------------------------------------");
      writefln("[TEST-ERROR] (%s) \"%s\"", expectedError, AsciiPrintString(code));
      stdout.flush();
    }
    try {
      auto codeTree = parse(cast(string)code);
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
  testError(ErrorType.invalidChar, "]");
  testError(ErrorType.endedEarly, "{");
  testError(ErrorType.endedEarly, "(");
  testError(ErrorType.endedEarly, "[");

  testError(ErrorType.invalidChar, "{)");
  testError(ErrorType.invalidChar, "(}");

  test("{}", [block(1)]);
  test("()", [parenList(1)]);
  test("{;}", [block(1, break_)]);
  test("(;)", [parenList(1, break_)]);
  test("(,)", [parenList(1, symbol(1, ","))]);

  testError(ErrorType.endedEarly, "/");
  testError(ErrorType.invalidChar, "/+");

  test("//");
  test("//   _)(6545 ^58%& \n   // what\r\n // ^*^*^ ");
  test("/* \n%^&*( \n */ ");
  test("/* !!! **/");

  testError(ErrorType.endedEarly, "/*");
  testError(ErrorType.endedEarly, "/* *");
  test(".notAArg", [symbol(1, "."), symbol(1, "notAArg")]);
  test("[notAParm]", [bracketList(1, symbol(1, "notAParm"))]);
  testError(ErrorType.invalidChar, "}");

  for(uint i = 0; i < 128; i++) {
    auto charFlags = lookupCharFlags(i);
    auto operatorKind = charFlags.getOperatorKind();
    if(!isFirstCharacterOfSymbol(i) && operatorKind == CharOperatorKind.none &&
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

  test("a", [symbol(1, "a")]);
  test(" ()", [parenList(1)]);
  test("a ()", [symbol(1, "a"), parenList(1)]);
  test("a();"  , [symbol(1, "a"), parenList(1), break_]);
  test("a ();" , [symbol(1, "a"), parenList(1), break_]);
  test("a () ;", [symbol(1, "a"), parenList(1), break_]);

  testError(ErrorType.endedEarly, "a(first");
  test("a(first)", [symbol(1, "a"), parenList(1, symbol(1, "first"))]);
  test("a(first)", [symbol(1, "a"), parenList(1, symbol(1, "first"))]);
  test("a (first);", [symbol(1, "a"), parenList(1, symbol(1, "first")), break_]);
  test("a ( first )  ;", [symbol(1, "a"), parenList(1, symbol(1, "first")), break_]);
  test("a /*comment*/(/*another*/first/*what*/)/*hey*/;", [symbol(1, "a"), parenList(1, symbol(1, "first")), break_]);

  test("a (first second);", [symbol(1, "a"), parenList(1, symbol(1, "first"), symbol(1, "second")), break_]);

  testError(ErrorType.invalidChar, "/a");
  testError(ErrorType.invalidChar, "a/;");
  testError(ErrorType.endedEarly, "a/");
  testError(ErrorType.endedEarly, "abc/");
  testError(ErrorType.invalidChar, "abc/;");
  testError(ErrorType.invalidChar, "abc/a");
  testError(ErrorType.invalidChar, "abc/string");
  testError(ErrorType.invalidChar, "/ a ");
  testError(ErrorType.endedEarly, "/*/");
  testError(ErrorType.invalidChar, "//*\n)");
  test("/**/");
  test("/***/");
  
  test("abc ();", [symbol(1, "abc"), parenList(1), break_]);
  test("abc (a def);", [symbol(1, "abc"), parenList(1, symbol(1, "a"), symbol(1, "def")), break_]);

  test("a ;", [symbol(1, "a"), break_]);
  test("a {}", [symbol(1, "a"), block(1)]);
  test("a {b;c;} ;", [symbol(1, "a"), block(1, symbol(1, "b"), break_, symbol(1, "c"), break_), break_]);
  test("a ({b;c;}) ;", [symbol(1, "a"), parenList(1, block(1, symbol(1, "b"), break_, symbol(1, "c"), break_)), break_]);
  test("a ( ({b;c;}) ) ;", [symbol(1, "a"), parenList(1, parenList(1, block(1, symbol(1, "b"), break_, symbol(1, "c"), break_))), break_]);
  test("a ({b;c;}) {}", [symbol(1, "a"), parenList(1, block(1, symbol(1, "b"), break_, symbol(1, "c"), break_)), block(1)]);
  // Primitive  types
  test("a int;" , [symbol(1, "a"), symbol(1, "int"), break_]);
  test("a uint;" , [symbol(1, "a"), symbol(1, "uint"), break_]);

  // Quoted Strings
  testError(ErrorType.endedEarly, "a \"");
  testError(ErrorType.endedEarly, "a \" ");
  test("a \"a string!\";", [symbol(1, "a"), string_(1, "a string!"), break_]);
  test(`"another string with an escape character \\ "`, [string_(1, `another string with an escape character \ `)]);
  test(`"\\\"\t\n"`, [string_(1, "\\\"\t\n")]);
  testError(ErrorType.unknownEscapeChar, `"\c"`);
  testError(ErrorType.unknownEscapeChar, `"\d"`);
  testError(ErrorType.unknownEscapeChar, `"\1"`);

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
  test("a (c,d,e);", [symbol(1, "a"), parenList(1, symbol(1, "c"), symbol(1, ","),
                                                symbol(1, "d"), symbol(1, ","),
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
  test("0", [new ConstructIntegerLiteral(1, 0)]);
  test(" 0", [new ConstructIntegerLiteral(1, 0)]);
  test("0 ", [new ConstructIntegerLiteral(1, 0)]);
  test("1", [new ConstructIntegerLiteral(1, 1)]);
  test("2", [new ConstructIntegerLiteral(1, 2)]);
  test("3", [new ConstructIntegerLiteral(1, 3)]);
  test("4", [new ConstructIntegerLiteral(1, 4)]);
  test("5", [new ConstructIntegerLiteral(1, 5)]);
  test("6", [new ConstructIntegerLiteral(1, 6)]);
  test("7", [new ConstructIntegerLiteral(1, 7)]);
  test("8", [new ConstructIntegerLiteral(1, 8)]);
  test("9", [new ConstructIntegerLiteral(1, 9)]);
  test("10", [new ConstructIntegerLiteral(1, 10)]);
  test(" 10", [new ConstructIntegerLiteral(1, 10)]);
  test("10 ", [new ConstructIntegerLiteral(1, 10)]);

  // Test Operators
  test("a.b", [symbol(1,"a"),symbol(1,"."),symbol(1,"b")]);
  test("a=0", [symbol(1,"a"),symbol(1,"="),new ConstructIntegerLiteral(1,0)]);
  test("a = 0", [symbol(1,"a"),symbol(1,"="),new ConstructIntegerLiteral(1,0)]);

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
  //test("*=",  [symbol(1, "*=")]);
  //test("/=",  [symbol(1, "/=")]); TODO: FIX THIS CASE (problem with comments)
  //test("%=",  [symbol(1, "%=")]);
  test("<<=", [symbol(1, "<<=")]);
  test(">>=", [symbol(1, ">>=")]);
  test("&=",  [symbol(1, "&=")]);
  //test("^=",  [symbol(1, "^=")]);
  test("|=",  [symbol(1, "|=")]);
  testError(ErrorType.endedEarly, "=<=(");
  testError(ErrorType.invalidChar, "=<=)");
}

