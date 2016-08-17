module construct.patterns;

import std.string : format;
import std.format : formattedWrite;
import std.conv   : to;
import std.array  : appender;
import construct.ir;

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
  plus,
  star,
}
bool onlyOne(const CountType type) pure
{
  return type == CountType.one ||
    type == CountType.optional;
}
bool isLoop(const CountType type) pure
{
  return type == CountType.plus ||
    type == CountType.star;
}
bool isOptional(const CountType type) pure
{
  return type == CountType.optional ||
    type == CountType.star;
}
string codeTextPostfix(const CountType type)
{
  final switch(type) {
  case CountType.one     : return "";
  case CountType.optional: return "?";
  case CountType.plus    : return "+";
  case CountType.star    : return "*";
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
  void visit(const(SubPatternMatcher) matcher);
}

class Matcher {
  string codeText;
  this(string codeText) immutable
  {
    this.codeText = codeText;
  }
  @property bool matchesAnySymbolNext() const pure { return false; }
  @property abstract string processorValueType() const pure;
  @property string processorOptionalValueType() const pure
  {
    return processorValueType();
  }
  abstract bool match(const(ConstructObject) obj) const;

  abstract void visit(IMatcherVisitHandler handler) const;

  alias anything    = AnyMatcher.instance;
  alias symbol      = SymbolMatcher.instance;
  alias string_     = PrimitiveTypeMatcher!ConstructString.instance;
  alias bool_       = PrimitiveTypeMatcher!ConstructBool.instance;
  alias list        = PrimitiveTypeMatcher!ConstructList.instance;
  alias objectBreak = PrimitiveTypeMatcher!ObjectBreak.instance;
  alias block       = PrimitiveTypeMatcher!ConstructBlock.instance;
}
class AnyMatcher : Matcher
{
  immutable static AnyMatcher instance = new immutable AnyMatcher();
  private this() immutable { super("anything"); }
  @property override string processorValueType() const pure { return "ConstructObject"; }
  override bool match(const(ConstructObject) obj) const { return true; }
  final override void visit(IMatcherVisitHandler handler) const
  {
    handler.visit(this);
  }
}

class SymbolMatcher : Matcher
{
  immutable static SymbolMatcher instance = new immutable SymbolMatcher();
  private this() immutable { super("symbol"); }
  @property override bool matchesAnySymbolNext() const pure { return true; }
  @property override string processorValueType() const pure { return "ConstructSymbol"; }
  override bool match(const(ConstructObject) obj) const
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
  this(string keyword) immutable
  {
    super(format("'%s'", keyword));
    this.keyword = keyword;
  }
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
  override bool match(const(ConstructObject) obj) const
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
    super(T.staticTypeName);
  }
  @property override string processorValueType() const pure
  {
    return T.processorValueType;
  }
  @property override string processorOptionalValueType() const pure
  {
    return T.processorOptionalValueType;
  }
  override bool match(const(ConstructObject) obj) const
  {
    return obj.as!T() !is null;
  }
  final override void visit(IMatcherVisitHandler handler) const
  {
    handler.visit(this);
  }
}

class SubPatternMatcher : Matcher
{
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
    super(builder.createString());
    this.nodes = nodes;
  }
  
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
  override bool match(const(ConstructObject) obj) const
  {
    throw imp();
    //return obj.as!T() !is null;
  }
  final override void visit(IMatcherVisitHandler handler) const
  {
    handler.visit(this);
  }
}

struct PatternNode
{
  static auto null_()
  {
    return immutable PatternNode(null, CountType.one, null);
  }

  const(char)[] name;
  CountType countType;
  Matcher matcher;
  this(const(char)[] name, CountType countType, immutable(Matcher) matcher)
  {
    this.name = name;
    this.countType = countType;
    this.matcher = matcher.unconst;
  }
  this(const(char)[] name, CountType countType, immutable(Matcher) matcher) immutable
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
  void generateDInterface(scope void delegate(const(char)[]) sink) const
  {
    throw imp("generateDInterface");
  }
  bool tryConsume(const(ConstructObject)[] objects, size_t* argIndex) const
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
    //sink(name);
    foreach(node; nodes) {
      sink(" ");
      node.toString(sink);
    }
  }
  bool tryConsume(const(ConstructObject)[] objects, size_t* argIndex) const
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
  bool match(const(ConstructObject)[] objects) const
  {
    size_t index = 0;
    if(tryConsume(objects, &index)) {
      return index == objects.length;
    }
    return false;
  }
}


immutable undefinedPattern = immutable Pattern();

immutable defconPattern1 = immutable Pattern
  ([immutable PatternNode("name", CountType.one, Matcher.symbol),
    immutable PatternNode("pattern", CountType.one, Matcher.list),
    immutable PatternNode(null, CountType.one, Matcher.objectBreak)]);
immutable defconPattern2 = immutable Pattern
  ([immutable PatternNode("name", CountType.one, Matcher.symbol),
    immutable PatternNode("pattern", CountType.one, Matcher.list),
    immutable PatternNode("implementation", CountType.one, Matcher.block)]);

immutable importPattern = immutable Pattern
  //([immutable PatternNode(CountType.plus, Matcher.string_),
  ([immutable PatternNode("name", CountType.one, Matcher.string_),
    immutable PatternNode(null, CountType.one , Matcher.objectBreak)]);

//
// Note: defcon <symbol> ...
//       is almost equivalent to
//         let <symbol> construct ...
//       the problem comes when you are combining multiple defcons
//         letset <constructName> combineConstructs <constructName> construct ...
//

//
// The 'pattern' construct constructs a pattern.
// it takes a list, and converts it to an internal pattern.
//
// Note: the '_' symbol is a special reserved symbol that means
//       the symbol is hidden.
//       defcon a (_ string)
//       {
//         // the string argument is hidden, it was consumed but can't be accessed
//       }
//       this is more useful for certain types such as keywords
//defcon try (tryBlock block, catchBlocks CatchInfo list itemPrefix="catch", finallyBlock block prefix="finally")
// (
//  tryBlock block,
//  catchClauses multi pattern(_ "catch", varName symbol, catchType type, catchBlock block),
//  finallyClause optional pattern(_ "finally", finallyBlock block)
//  )
immutable tryPattern = immutable Pattern
  ([immutable PatternNode("tryBlock", CountType.one, Matcher.block),
    immutable PatternNode("catchClauses", CountType.star, new immutable SubPatternMatcher
			  ([immutable PatternNode(null, CountType.one, new immutable KeywordMatcher("catch")),
			    immutable PatternNode("catchBlock", CountType.one, Matcher.block)])),
    immutable PatternNode("finallyClause", CountType.optional, new immutable SubPatternMatcher
			  ([immutable PatternNode(null, CountType.one, new immutable KeywordMatcher("finally")),
			    immutable PatternNode("finallyBlock", CountType.one, Matcher.block)]))]);

immutable setletPattern = immutable Pattern
  ([immutable PatternNode("name", CountType.one, Matcher.symbol),
    immutable PatternNode("value", CountType.one, Matcher.anything),
    immutable PatternNode("break_", CountType.optional, Matcher.objectBreak)]);


immutable oneSymbolPattern = immutable Pattern
  ([immutable PatternNode("symbol", CountType.one, Matcher.symbol)]);
immutable oneStringPattern = immutable Pattern
  ([immutable PatternNode("string_", CountType.one, Matcher.string_)]);




immutable(Pattern) processPattern(ConstructProcessor* processor, const(ConstructList) patternList)
{
  auto nodes = appender!(immutable(PatternNode)[])();
  
  for(size_t itemIndex = 0;;) {
    if(itemIndex >= patternList.items.length) {
      break;
    }

    auto nodeName = patternList.items[itemIndex].asConstructSymbol;
    if(!nodeName) {
      throw processor.semanticError(patternList.items[itemIndex].lineNumber, format
				    ("expected defcon parameter name to be a symbol but got %s",
				     An(patternList.items[itemIndex].typeName)));
    }
    itemIndex++;
    if(itemIndex >= patternList.items.length) {
      nodes.put(immutable PatternNode(nodeName.value, CountType.one, Matcher.anything));
      break;
    }

    ConstructType nodeType;
    {
      auto object = processor.consumeValueAlreadyCheckedIndex(patternList.items, &itemIndex);
      if(object is null) {
	throw processor.semanticError(patternList.items[itemIndex].lineNumber,
				      "expected the expression to return a type but returned null");
      }
      if(object.isListBreak) {
        nodes.put(immutable PatternNode(nodeName.value, CountType.one, Matcher.anything));
	continue;
      }
      nodeType = object.asConstructType.unconst;
      if(nodeType is null) {
	throw processor.semanticError(object.lineNumber, format
				      ("expected the expression to return a type but returned %s",
				       An(object.typeName)));
      }
    }

    if(itemIndex >= patternList.items.length) {
      nodes.put(immutable PatternNode(nodeName.value, CountType.one, nodeType.matcher));
      break;
    }
    if(patternList.items[itemIndex].isListBreak) {
      nodes.put(immutable PatternNode(nodeName.value, CountType.one, nodeType.matcher));
      itemIndex++;
      continue;
    }

    CountType countType;
    {
      auto object = patternList.items[itemIndex++];
      if(auto symbol = object.asConstructSymbol) {
	if(symbol.value == "optional") {
	  countType = CountType.optional;
	} else {
	  imp(format("pattern modifier '%s'", symbol.value));
	}
      } else {
	throw processor.semanticError(object.lineNumber, format
				      ("expected a symbol but got %s", An(object.typeName)));
      }
    }

    nodes.put(immutable PatternNode(nodeName.value, countType, nodeType.matcher));
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
  
  return immutable Pattern(nodes.data);
}

unittest
{
  writeln(importPattern);
  writeln(setletPattern);
  
  assert(importPattern.match([string_("hello"), break_()]));
  //assert(importPattern.match([string_("hello"), string_("world"), break_()]));
  //importConstruct([string_("hello")]);
  //importConstruct([string_("hello"), string_("world")]);



  
  
  //writefln("GenFunc (import): %s", generateConstructFunction(importPattern, ["names"]));
  

  
  //auto letPattern
    // the 'let' construct: grammar definition
    //let-pattern ::= symbol object break?

  // TODO: implement the pattern parser
}

// TODO: this should be defined in the standard library
void toString(N)(N number, scope void delegate(const(char)[]) sink)
{
  formattedWrite(sink, "%s", number);
}
