module construct.patterns;

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
bool onlyOne(const CountType type)
{
  return type == CountType.one ||
    type == CountType.optional;
}
bool isOptional(const CountType type)
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

alias MatchFunction = bool function(const(ConstructObject) obj);
struct Matcher {
  string codeText;
  MatchFunction func;
}
bool anyValueMatcherFunc(const(ConstructObject) obj)
{
  throw imp("anyValueMatcherFunc");
}
immutable anyValueMatcher = Matcher("anything", &anyValueMatcherFunc);
bool symbolMatcherFunc(const(ConstructObject) obj)
{
  // todo: handle the resolveSymbolRef case
  return obj.asConstructSymbol !is null;
}
immutable symbolMatcher = Matcher("symbol", &symbolMatcherFunc);
bool stringMatcherFunc(const(ConstructObject) obj)
{
  return obj.asConstructString !is null;
}
immutable stringMatcher = Matcher("string", &stringMatcherFunc);

bool objectBreakMatcherFunc(const(ConstructObject) obj)
{
  return obj.isObjectBreak;
}
immutable objectBreakMatcher = Matcher(";", &objectBreakMatcherFunc);
bool blockMatcherFunc(const(ConstructObject) obj)
{
  return obj.asConstructBlock !is null;
}
immutable blockMatcher = Matcher("block", &blockMatcherFunc);




struct PatternNode
{
  CountType countType;
  Matcher matcher;
  this(CountType countType, Matcher matcher)
  {
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
    if(index >= objects.length || !matcher.func(objects[index])) {
      return countType.isOptional;
    }
    index++;
    if(countType.onlyOne) {
      *argIndex = index;
      return true;
    }
    for(;;index++) {
      if(index >= objects.length || !matcher.func(objects[index])) {
	*argIndex = index;
	return true;
      }
    }
  }
}
struct Pattern {
  //const(char)[] name;
  const(PatternNode) nodes[];
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

immutable importPattern = immutable Pattern
  ([PatternNode(CountType.plus, stringMatcher),
    PatternNode(CountType.one, objectBreakMatcher)]);

immutable tryPattern = immutable Pattern
  ([PatternNode(CountType.one, blockMatcher),
    //	   PatternNode(CountType.star, 
    ]);

immutable letPattern = immutable Pattern
  ([PatternNode(CountType.one, symbolMatcher),
    PatternNode(CountType.one, anyValueMatcher),
    PatternNode(CountType.optional, objectBreakMatcher)]);

unittest
{
  writeln(importPattern);
  writeln(letPattern);
  
  assert(importPattern.match([string_("hello"), break_()]));
  assert(importPattern.match([string_("hello"), string_("world"), break_()]));


  //auto letPattern
    // the 'let' construct: grammar definition
    //let-pattern ::= symbol object break?

  // TODO: implement the pattern parser
}

