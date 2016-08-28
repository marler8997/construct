module construct.patterns;

import std.format : format, formattedWrite;
import std.conv   : to;
import std.array  : appender, Appender;
import construct.logging;
import construct.ir;
import construct.processor : ConstructProcessor;

version(unittest) {
  import std.stdio;
}
/*
// defcon import (names string ...) requiresBreak=true;
// The 'import' construct takes one or more strings, followed by a semi-colon.
// It also returns nothing.
import-pattern ::= string+ ;

// the 'let' construct: grammar definition
let-pattern ::= symbol object ;?

// the 'message' construct grammar definition
message-pattern ::= anything* ;

// the 'try' construct grammar definition
try-pattern :: block ('catch' block)? ('finally' block)?

// the 'return' construct
return-patter :: value? ;

*/

enum CountType {
  one,
  optional,
  oneOrMore,
  many,
}
bool onlyOne(const CountType type) pure
{
  return type == CountType.one ||
    type == CountType.optional;
}
bool isLoop(const CountType type) pure
{
  return type == CountType.oneOrMore ||
    type == CountType.many;
}
bool isOptional(const CountType type) pure
{
  return type == CountType.optional ||
    type == CountType.many;
}
string codeTextPostfix(const CountType type) pure
{
  final switch(type) {
  case CountType.one      : return "";
  case CountType.optional : return "?";
  case CountType.oneOrMore: return "+";
  case CountType.many     : return "*";
  }
}

interface IMatcherVisitHandler
{
  void visit(const(AnyMatcher) matcher);
  void visit(const(SymbolMatcher) matcher);
  void visit(const(KeywordMatcher) matcher);
  void visit(const(PrimitiveTypeMatcher!ConstructString) matcher);
  void visit(const(PrimitiveTypeMatcher!ConstructBool) matcher);
  void visit(const(PrimitiveTypeMatcher!ConstructList) matcher);
  void visit(const(PrimitiveTypeMatcher!ObjectBreak) matcher);
  void visit(const(PrimitiveTypeMatcher!ConstructBlock) matcher);
  void visit(const(ConstructPattern) matcher);
}

interface Matcher {
  @property abstract string codeText() const pure;
  
  @property abstract bool matchesAnySymbolNext() const pure;
  @property abstract string processorValueType() const pure;
  @property abstract string processorOptionalValueType() const pure;

  abstract bool match(const(ConstructObject) obj) const pure;

  abstract void visit(IMatcherVisitHandler handler) const;

  alias anything       = AnyMatcher.instance;
  alias symbol         = SymbolMatcher.instance;
  alias string_        = PrimitiveTypeMatcher!ConstructString.instance;
  alias bool_          = PrimitiveTypeMatcher!ConstructBool.instance;
  alias list           = PrimitiveTypeMatcher!ConstructList.instance;
  alias constructBreak = PrimitiveTypeMatcher!ObjectBreak.instance;
  alias block          = PrimitiveTypeMatcher!ConstructBlock.instance;
}
class AnyMatcher : Matcher
{
  immutable static AnyMatcher instance = new immutable AnyMatcher();
  private this() immutable { }
  @property override string codeText() const pure { return "anything"; }
  @property override bool matchesAnySymbolNext() const pure { return false; }
  @property override string processorValueType() const pure { return "ConstructObject"; }
  @property override string processorOptionalValueType() const pure { return "ConstructObject"; }
  override bool match(const(ConstructObject) obj) const pure { return true; }
  final override void visit(IMatcherVisitHandler handler) const
  {
    handler.visit(this);
  }
}
class SymbolMatcher : Matcher
{
  immutable static SymbolMatcher instance = new immutable SymbolMatcher();
  private this() immutable { }
  @property override string codeText() const pure { return "symbol"; }
  @property override bool matchesAnySymbolNext() const pure { return true; }
  @property override string processorValueType() const pure { return "ConstructSymbol"; }
  @property override string processorOptionalValueType() const pure { return "ConstructSymbol"; }
  override bool match(const(ConstructObject) obj) const pure
  {
    throw new Exception("CodeBug: SymbolMatcher.match should never be called");
  }
  final override void visit(IMatcherVisitHandler handler) const
  {
    handler.visit(this);
  }
}

class KeywordMatcher : Matcher
{
  string keyword;
  string codeTextString;
  this(string keyword) pure immutable
  {
    this.keyword = keyword;
    this.codeTextString = format("\"%s\"", keyword);
  }
  @property override string codeText() const pure { return codeTextString; }
  @property override bool matchesAnySymbolNext() const pure { return false; }
  @property override string processorValueType() const pure
  {
    throw imp();
    //return T.processorValueType;
  }
  @property override string processorOptionalValueType() const pure
  {
    throw imp();
    //return T.processorOptionalValueType;
  }
  override bool match(const(ConstructObject) obj) const pure
  {
    throw imp();
  }
  final override void visit(IMatcherVisitHandler handler) const
  {
    handler.visit(this);
  }
}
class PrimitiveTypeMatcher(T) : Matcher if( !is(T == ConstructSymbol) )
{
  static immutable PrimitiveTypeMatcher!T instance =
    new immutable PrimitiveTypeMatcher!T();
  private this() immutable
  {
  }
  @property override string codeText() const pure { return T.staticTypeName; }
  @property override bool matchesAnySymbolNext() const pure { return false; }
  @property override string processorValueType() const pure
  {
    return T.processorValueType;
  }
  @property override string processorOptionalValueType() const pure
  {
    return T.processorOptionalValueType;
  }
  override bool match(const(ConstructObject) obj) const pure
  {
    return obj.as!T() !is null;
  }
  final override void visit(IMatcherVisitHandler handler) const
  {
    handler.visit(this);
  }
}
/*
class SubPatternMatcher : Matcher
{
  string codeTextString;
  PatternNode[] nodes;

  bool haveCachedProcessorValueType;
  string cachedProcessorValueType;

  bool haveCachedProcessorOptionalValueType;
  string cachedProcessorOptionalValueType;
  
  this(immutable(PatternNode)[] nodes) immutable
  {
    StringBuilder!512 builder;

    builder.append("(");
    foreach(i, node; nodes) {
      if(i > 0) builder.append(" ");
      builder.append(node.matcher.codeText);
      builder.append(node.countType.codeTextPostfix);
    }
    builder.append(")");
    this.codeTextString = builder.createString();
    this.nodes = nodes;
  }
  @property override string codeText() const pure { return codeTextString; }
  @property override bool matchesAnySymbolNext() const pure
  {
    return nodes.length > 0 && nodes[0].matcher.matchesAnySymbolNext;
  }
  @property override string processorValueType() const pure
  {
    throw imp();
    //    return T.processorValueType;
  }
  @property override string processorOptionalValueType() const pure
  {
    throw imp();
    //    return T.processorOptionalValueType;
  }
  override bool match(const(ConstructObject) obj) const pure
  {
    throw imp();
    //return obj.as!T() !is null;
  }
  final override void visit(IMatcherVisitHandler handler) const
  {
    handler.visit(this);
  }
}
*/

struct PatternNode
{
  static auto null_()
  {
    return immutable PatternNode(null, CountType.one, null);
  }

  const(char)[] name;
  CountType countType;
  Matcher matcher;

  this(const(char)[] name, CountType countType, immutable(Matcher) matcher) pure
  {
    this.name = name;
    this.countType = countType;
    this.matcher = matcher.unconst;
  }
  this(const(char)[] name, CountType countType, immutable(Matcher) matcher) pure immutable
  {
    this.name = cast(string)name;
    this.countType = countType;
    this.matcher = matcher;
  }
  void toString(scope void delegate(const(char)[]) sink) const
  {
    sink(matcher.codeText);
    sink(countType.codeTextPostfix);
  }
  /*
  void generateDInterface(scope void delegate(const(char)[]) sink) const
  {
    throw imp("generateDInterface");
  }
  */
  bool tryConsume(const(ConstructObject)[] objects, size_t* argIndex) const pure
  {
    auto index = *argIndex;
    if(index >= objects.length || !matcher.match(objects[index])) {
      return countType.isOptional;
    }
    index++;
    if(countType.onlyOne) {
      *argIndex = index;
      return true;
    }
    for(;;index++) {
      if(index >= objects.length || !matcher.match(objects[index])) {
	*argIndex = index;
	return true;
      }
    }
  }
}
struct Pattern {
  const(PatternNode)[] nodes;
  void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("pattern (");
    foreach(nodeIndex, node; nodes) {
      if(nodeIndex > 0) {
        sink(" ");
      }
      node.toString(sink);
    }
    sink(")");
  }
  bool tryConsume(const(ConstructObject)[] objects, size_t* argIndex) const pure
  {
    auto newIndex = *argIndex;
    foreach(node; nodes) {
      if(!node.tryConsume(objects, &newIndex)) {
	return false;
      }
    }
    *argIndex = newIndex;
    return true;
  }
  bool match(const(ConstructObject)[] objects) const pure
  {
    size_t index = 0;
    if(tryConsume(objects, &index)) {
      return index == objects.length;
    }
    return false;
  }
  
}

immutable emptyPattern = immutable Pattern();
immutable undefinedPattern = immutable Pattern();

//
// Note: defcon <symbol> ...
//       is almost equivalent to
//         let <symbol> construct ...
//       the problem comes when you are combining multiple defcons
//         letset <constructName> combineConstructs <constructName> construct ...
//


immutable setletPattern = immutable Pattern
  ([immutable PatternNode("name", CountType.one, Matcher.symbol),
    immutable PatternNode("value", CountType.one, Matcher.anything),
    immutable PatternNode("break_", CountType.optional, Matcher.constructBreak)]);


immutable oneSymbolPattern = immutable Pattern
  ([immutable PatternNode("symbol", CountType.one, Matcher.symbol)]);
immutable oneStringPattern = immutable Pattern
  ([immutable PatternNode("string_", CountType.one, Matcher.string_)]);


// Assumption: *itemIndex < items.length
CountType getCountType(const(ConstructObject)[] items, size_t* itemIndex) pure
{
  if(auto symbol = items[*itemIndex].asConstructSymbol){
    if(symbol.value == "optional") {
      (*itemIndex)++;
      return CountType.optional;
    } else if(symbol.value == "many") {
      (*itemIndex)++;
      return CountType.many;
    } else if(symbol.value == "oneOrMore") {
      (*itemIndex)++;
      return CountType.oneOrMore;
    } else if(symbol.value == "one") {
      (*itemIndex)++;
      return CountType.one;
    }
  }
  return CountType.one;
}

immutable(Pattern) compileBasicPattern(string code)
{
  import construct.parser : standardParser;
  const(ConstructObject)[] parsedObjects = standardParser!(Appender!(const(ConstructObject)[])).func(code);
  assert(parsedObjects.length == 1);
  auto list = parsedObjects[0].asConstructList;
  assert(list);
  return processBasicPattern(list);
}

immutable(Pattern) processBasicPattern(const(ConstructList) patternList)
{
  ConstructProcessor processor = ConstructProcessor(null);
  return processPattern(&processor, patternList);
}


immutable(Pattern) processPattern(ConstructProcessor* processor, const(ConstructList) patternList)
{
  //auto nodes = appender!(PatternNode[])();
  auto nodes = new immutable(PatternNode)[0];
  void appendNode(immutable(PatternNode) node)
  {
    nodes ~= node;
  }
  
  for(size_t itemIndex = 0;;) {
    if(itemIndex >= patternList.items.length) {
      break;
    }

    const(char)[] nodeNameString;
    {
      auto nodeName = processor.tryConsumeSymbol(patternList.items, &itemIndex);
      if(!nodeName) {
        throw processor.semanticError(patternList.items[itemIndex].lineNumber, format
                                      ("expected defcon parameter name to be a symbol but got %s",
                                       An(patternList.items[itemIndex].typeName)));
      }
      nodeNameString = (nodeName.value == "nameless") ? null : nodeName.value;
    }
    if(itemIndex >= patternList.items.length) {
      appendNode(immutable PatternNode(nodeNameString, CountType.one, Matcher.anything));
      break;
    }
    // Get count type
    CountType countType;
    {
      size_t newIndex = itemIndex;
      countType = getCountType(patternList.items, &newIndex);
      if(newIndex > itemIndex) {
        itemIndex = newIndex;
        if(itemIndex >= patternList.items.length) {
          appendNode(immutable PatternNode(nodeNameString, countType, Matcher.anything));
          break;
        }
      }
    }
    Matcher matcher;
    {
      auto object = processor.consumeValueAlreadyCheckedIndex(patternList.items, &itemIndex);
      if(object is null) {
	throw processor.semanticError(patternList.items[itemIndex].lineNumber,
				      "expected the expression to return a type but returned null");
      }
      if(object.isListBreak) {
        appendNode(immutable PatternNode(nodeNameString, countType, Matcher.anything));
	continue;
      }
      
      if(auto nodeType = object.asConstructType) {
	matcher = nodeType.matcher.unconst;
      } else if(auto subPattern = object.asConstructPattern) {
	matcher = subPattern.unconst;
      } else if(auto keywordString = object.asConstructString) {
	matcher = new immutable KeywordMatcher(keywordString.toUtf8).unconst;
      } else {
	throw processor.semanticError(object.lineNumber, format
				      ("expected the expression to return a type but returned %s",
				       An(object.typeName)));
      }
    }

    if(itemIndex >= patternList.items.length) {
      appendNode(immutable PatternNode(nodeNameString, countType, matcher.immutable_));
      break;
    }
    if(patternList.items[itemIndex].isListBreak) {
      appendNode(immutable PatternNode(nodeNameString, countType, matcher.immutable_));
      itemIndex++;
      continue;
    }

    appendNode(immutable PatternNode(nodeNameString, countType, matcher.immutable_));
    if(itemIndex >= patternList.items.length) {
      break;
    }
    {
      auto object = patternList.items[itemIndex];
      if(!object.isListBreak) {
	throw processor.semanticError(object.lineNumber, format
				      ("expected a list break ',' got %s", An(object.typeName)));
      }
    }
    itemIndex++;
  }

  return immutable Pattern(nodes);
  //return immutable Pattern(nodes.data);
  //return immutable Pattern(cast(immutable(PatternNode)[])nodes.data);
}


unittest
{
  //writeln(importPattern);
  //writeln(setletPattern);

  size_t assertEqualCount = 0;
  void assertEqual(immutable Pattern pattern, string patternString)
  {
    writefln("Testing pattern '%s'", patternString);
    assert(pattern == compileBasicPattern(patternString));
    assertEqualCount++;
  }
  
  struct StringPatternPair
  {
    string string_;
    immutable(PatternNode) pattern;
  }
  auto variants =
    [
     StringPatternPair("aName"                   , immutable PatternNode("aName", CountType.one, Matcher.anything)),
     StringPatternPair("aName one"               , immutable PatternNode("aName", CountType.one, Matcher.anything)),
     StringPatternPair("aName anything"          , immutable PatternNode("aName", CountType.one, Matcher.anything)),
     StringPatternPair("aName one anything"      , immutable PatternNode("aName", CountType.one, Matcher.anything)),

     StringPatternPair("nameless"                , immutable PatternNode(null, CountType.one, Matcher.anything)),
     StringPatternPair("nameless one"            , immutable PatternNode(null, CountType.one, Matcher.anything)),
     StringPatternPair("nameless anything"       , immutable PatternNode(null, CountType.one, Matcher.anything)),
     StringPatternPair("nameless one anything"   , immutable PatternNode(null, CountType.one, Matcher.anything)),

     StringPatternPair("aName optional"          , immutable PatternNode("aName", CountType.optional, Matcher.anything)),
     StringPatternPair("aName optional anything" , immutable PatternNode("aName", CountType.optional, Matcher.anything)),

     StringPatternPair("aName many"              , immutable PatternNode("aName", CountType.many, Matcher.anything)),
     StringPatternPair("aName many anything"     , immutable PatternNode("aName", CountType.many, Matcher.anything)),

     StringPatternPair("aName oneOrMore"         , immutable PatternNode("aName", CountType.oneOrMore, Matcher.anything)),
     StringPatternPair("aName oneOrMore anything", immutable PatternNode("aName", CountType.oneOrMore, Matcher.anything)),

     StringPatternPair("aName string"            , immutable PatternNode("aName", CountType.one, Matcher.string_)),
     StringPatternPair("aName one string"        , immutable PatternNode("aName", CountType.one, Matcher.string_)),

     StringPatternPair("nameless string"         , immutable PatternNode(null, CountType.one, Matcher.string_)),
     StringPatternPair("nameless one string"     , immutable PatternNode(null, CountType.one, Matcher.string_)),
     
     StringPatternPair("aName optional string"   , immutable PatternNode("aName", CountType.optional, Matcher.string_)),

     StringPatternPair("aName many string"       , immutable PatternNode("aName", CountType.many, Matcher.string_)),

     StringPatternPair("aName oneOrMore string"  , immutable PatternNode("aName", CountType.oneOrMore, Matcher.string_)),
     ];

  foreach(variant; variants) {
    auto pattern = immutable Pattern([variant.pattern]);
    assertEqual(pattern, "("~variant.string_~")");
    foreach(secondVariant; variants) {
      auto twoParamPattern = immutable Pattern([variant.pattern, secondVariant.pattern]);
      assertEqual(twoParamPattern, "("~variant.string_~", "~secondVariant.string_~")");
    }
  }

  writefln("Tested %s patterns", assertEqualCount);
}

// TODO: this should be defined in the standard library
void toString(N)(N number, scope void delegate(const(char)[]) sink)
{
  formattedWrite(sink, "%s", number);
}
