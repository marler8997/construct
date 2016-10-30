module construct.patterns;

import std.format : format, formattedWrite;
import std.conv   : to;
import std.array  : appender, Appender;
import construct.util;
import construct.logging;
import construct.parserCore;
import construct.standardParser : isValidSymbol;
import construct.backendCore : ConstructType, ConstructPattern, ConstructClass,
                               ConstructString, ConstructPointer, ConstructBool, ConstructNumber,
                               canBe, tryAs, IConstructContext, NoConstructContext,
                               PrimitiveType, KeywordType;
import construct.processor   : ConstructProcessor;

version(unittest) {
  import std.stdio;
  //version = VerboseTests;
}

enum CountType {
  one,
  optional,
  oneOrMore,
  many,
}
bool onlyOne(const CountType type) pure nothrow @nogc @safe
{
  return type == CountType.one ||
    type == CountType.optional;
}
bool isMultiple(const CountType type) pure nothrow @nogc @safe
{
  return type == CountType.oneOrMore ||
    type == CountType.many;
}
bool isOptional(const CountType type) pure nothrow @nogc @safe
{
  return type == CountType.optional ||
    type == CountType.many;
}

enum Raw { off, on }
struct PatternNode
{
  static auto null_()
  {
    return immutable PatternNode(null, CountType.one, Raw.off, null);
  }

  string name;
  CountType countType;
  Raw raw;
  immutable(ConstructType) type;

  this(string name, CountType countType, Raw raw, immutable(ConstructType) type) immutable pure
  {
    assert(name.length > 0);
    if(type.tryAsConstructSymbol) {
      if(countType != CountType.one) {
        throw new Exception(format("A symbol matcher cannot have count type %s", countType));
      }
    }
    this.name = name;
    this.countType = countType;
    this.raw = raw;
    this.type = type;
  }
  this(string name, CountType countType, Raw raw, immutable(ConstructType) type) pure
  {
    assert(name.length > 0);
    if(type.tryAsConstructSymbol) {
      if(countType != CountType.one) {
        throw new Exception(format("A symbol matcher cannot have count type %s", countType));
      }
    }
    this.name = name;
    this.countType = countType;
    this.raw = raw;
    this.type = type;
  }
  void toString(Sink)(Sink sink) const
  {
    string rawString = raw ? "raw " : "";
    if(countType == CountType.one) {
      formattedWrite(sink, "%s %s%s", name, rawString, type.typeEnum.definition.name);
    } else {
      formattedWrite(sink, "%s %s %s%s", name, countType, rawString, type.typeEnum.definition.name);
    }
  }
}

struct BranchesStructure(T)
{
  size_t branchCount;
  PatternTree!T.BranchesUnion dataOrBranches;
  this(size_t branchCount, PatternTree!T.BranchesUnion dataOrBranches) immutable
  {
    this.branchCount = branchCount;
    this.dataOrBranches = dataOrBranches;
  }
  this(T data) immutable
  {
    this.branchCount = 0;
    this.dataOrBranches.data = data;
  }
  this(immutable(PatternTree!T)[] branches) immutable
  {
    if(branches.length == 0) {
      throw new Exception("Use other constructor");
    }
    this.branchCount = branches.length;
    this.dataOrBranches.branchPtr = branches.ptr;
  }
  @property immutable(PatternTree!T)[] branches() immutable
  {
    return dataOrBranches.branchPtr[0..branchCount];
  }
}
struct PatternTree(T)
{
  union BranchesUnion {
    T data;
    immutable(PatternTree)* branchPtr;
  }

  immutable(PatternNode)[] nodes;
  size_t branchCount;
  BranchesUnion dataOrBranches;

  this(immutable(PatternNode)[] nodes, immutable(BranchesStructure!T) branchesStructure) immutable
  {
    this.nodes = nodes;
    this.branchCount = branchesStructure.branchCount;
    this.dataOrBranches = branchesStructure.dataOrBranches;
  }
  this(immutable(PatternNode)[] nodes, immutable(PatternTree)[] branches) immutable
  {
    this.nodes = nodes;
    if(branches.length == 0) {
      throw new Exception("branches cannot be empty, use the other constructor for that");
    }
    this.branchCount = branches.length;
    this.dataOrBranches.branchPtr = branches.ptr;

    // Check that the branches are not ambiguous
    /*
    foreach(branch; branches) {
      if(branch.nodes.length > 0) {
        if(branch.nodes[0].type.isAnySymbolType) {
          throw new Exception("invalid branch, could match a symbol next, creates ambiguity");
        }
      }
    }
    */
  }
  this(immutable(PatternNode)[] nodes, T data) immutable
  {
    this.nodes = nodes;
    this.dataOrBranches.data = data;
  }
  @property immutable(BranchesStructure!T) branchesStructure() immutable
  {
    return immutable BranchesStructure!T(branchCount, dataOrBranches);
  }
  bool equals(const(PatternTree!T) other, bool requireSameBranchOrder = true) const
  {
    if(nodes.length != other.nodes.length ||
       branchCount != other.branchCount) {
      return false;
    }
    if(branchCount == 0) {
      if(dataOrBranches.data != other.dataOrBranches.data) {
        return false;
      }
      return nodes == other.nodes;
    } else {
      if(nodes != other.nodes) {
        return false;
      }
      foreach(i; 0..branchCount) {
        if(this.dataOrBranches.branchPtr[i] != other.dataOrBranches.branchPtr[i]) {
          return false;
        }
      }
      return true;
    }
  }
  bool matchesNoObjects() const
  {
    foreach(node; nodes) {
      if(!node.countType.isOptional) {
        return false;
      }
    }
    foreach(i; 0..branchCount) {
      auto branch = dataOrBranches.branchPtr[i];
      if(branch.matchesNoObjects()) {
        return true;
      }
    }
    return false;
  }
  //void toString(Sink)(scope Sink sink) const
  void toString(scope void delegate(const(char)[]) sink) const
  {
    toString(sink, 0, 0);
  }

  //void toString(Sink)(scope Sink sink) const
  void toString(scope void delegate(const(char)[]) sink, size_t branchIndex, size_t level) const
  {
    //sink.writeLinePrefix(level);
    //formattedWrite(sink, "Pattern %s nodes %s branches\n", nodes.length, branchCount);

    foreach(nodeIndex, node; nodes) {
      sink.writeLinePrefix(level);
      //formattedWrite(sink, "NODE[%s]", nodeIndex);
      sink(node.name);
      sink(" ");
      sink(node.countType.to!string);
      sink(" ");
      node.type.toString(sink);
      sink("\n");
    }
    if(branchCount == 0) {
      sink.writeLinePrefix(level);
      sink("[LEAF]\n");
    } else {
      auto nextLevel = level+1;
      foreach(i; 0..branchCount) {
        auto branch = dataOrBranches.branchPtr[i];
        sink.writeLinePrefix(level);
        sink("|\n");
        sink.writeLinePrefix(level);
        formattedWrite(sink, "|---Branch %s\n", i);
        branch.toString(sink, i, nextLevel);
      }
    }
  }

}

void writeLinePrefix(Sink)(Sink sink, size_t level)
{
  foreach(i;0..level) {
    sink("|   ");
  }
}


//
// Note: defcon <symbol> ...
//       is almost equivalent to
//         let <symbol> construct ...
//       the problem comes when you are combining multiple defcons
//         letset <constructName> combineConstructs <constructName> construct ...
//
/*
immutable setletPattern =
  [immutable PatternNode("name", CountType.one, Raw.on, PrimitiveType.symbol),
   immutable PatternNode("value", CountType.one, Raw.off, PrimitiveType.anything),
   immutable PatternNode("break_", CountType.optional, Raw.on, PrimitiveType.constructBreak)];

immutable oneRawSymbolPattern =
  [immutable PatternNode("symbol", CountType.one, Raw.on, PrimitiveType.symbol)];
immutable oneStringPattern =
  [immutable PatternNode("string_", CountType.one, Raw.off, PrimitiveType.string_)];
*/

// Assumption: *itemIndex < items.length
CountType getCountType(const(ConstructObject)[] items, size_t* itemIndex) pure
{
  if(auto symbol = items[*itemIndex].tryAsConstructSymbol){
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

struct Pattern
{
  immutable(ConstructType) opType;
  PatternNode[] nodes;
}

Pattern compileBasicPattern(string code)
{
  import construct.parser : standardParser;
  const(ConstructObject)[] parsedObjects = standardParser!(Appender!(const(ConstructObject)[])).func(code);
  assert(parsedObjects.length == 1);
  auto list = parsedObjects[0].tryAsConstructList;
  assert(list);
  return processBasicPattern(list);
}
Pattern processBasicPattern(const(ConstructList) patternList)
{
  ConstructProcessor processor = ConstructProcessor(null);
  return processPattern(&processor, NoConstructContext.instance, patternList);
}

inout(ConstructType) getPatternType(ConstructProcessor* processor, inout(ConstructObject) typeObject)
{
  if(auto nodeType = typeObject.tryAsConstructType) {
    return nodeType;
  }

  if(auto keywordString = typeObject.tryAsConstructString) {
    auto utf8String = keywordString.toUtf8;
    if(!isValidSymbol(utf8String)) {
      throw processor.semanticError(typeObject.lineNumber, format
				    ("in order to use a keyword for a pattern type, the string must be a valid symbol, but \"%s\" is not",
				     utf8String));
    }
    return new inout KeywordType(typeObject.lineNumber, utf8String);
  }
  throw processor.semanticError(typeObject.lineNumber, format
				("expected the expression to return a type but returned %s",
				 An(typeObject.typeName)));
}

Pattern processPattern(ConstructProcessor* processor, const(IConstructContext) constructContext,
		       const(ConstructList) patternList)
{
  auto nodes = new PatternNode[0];
  void appendNode(PatternNode node)
  {
    nodes ~= node;
  }

  // TODO: add this variable and set it in Pattern
  ConstructType opType = null;

  for(size_t itemIndex = 0;;) {
    if(itemIndex >= patternList.items.length) {
      break;
    }

    string nodeNameString;
    {
      auto nodeName = processor.tryConsumeSymbol(constructContext, patternList.items, &itemIndex);
      if(!nodeName) {
        throw processor.semanticError(patternList.lineNumber, format
                                      ("expected pattern name to be a symbol but got %s",
                                       An(patternList.items[itemIndex].typeName)));
      }
      nodeNameString = nodeName.value;
    }

    if(nodeNameString == "this") {
      if(nodes.length > 0) {
        throw processor.semanticError(patternList.lineNumber, "only the first pattern node can be named 'this'");
      }
      if(itemIndex >= patternList.items.length) {
        opType = PrimitiveType.anything.unconst;
        break;
      }

      // Check for raw
      if(auto symbol = patternList.items[itemIndex].tryAsConstructSymbol) {
        if(symbol.value == "raw") {
	  throw processor.semanticError(symbol.lineNumber, "the 'raw' modifier is not valid on the 'this' pattern node");
        }
      }
      
      {
        auto object = processor.consumeValueAlreadyCheckedIndex(constructContext, patternList.items, &itemIndex);
        if(object is null) {
          throw processor.semanticError(patternList.items[itemIndex].lineNumber,
                                        "expected the expression to return a type but returned null");
        }
        if(object.isListBreak) {
          opType = PrimitiveType.anything.unconst;
          continue;
        }
        opType = getPatternType(processor, object).unconst;
        if(itemIndex >= patternList.items.length) {
          break;
        }
      }
      {
        auto object = patternList.items[itemIndex].unconst;
        if(!object.isListBreak) {
          throw processor.semanticError(object.lineNumber, format
                                        ("expected a list break ',' after this node, but got %s", An(object.typeName)));
        }
        itemIndex++;
      }
      continue;
    }

    if(itemIndex >= patternList.items.length) {
      appendNode(PatternNode(nodeNameString, CountType.one, Raw.off, PrimitiveType.anything));
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
          appendNode(PatternNode(nodeNameString, countType, Raw.off, PrimitiveType.anything));
          break;
        }
      }
    }

    // Check for raw
    Raw raw = Raw.off;
    if(auto symbol = patternList.items[itemIndex].tryAsConstructSymbol) {
      if(symbol.value == "raw") {
	raw = Raw.on;
        itemIndex++;
        if(itemIndex >= patternList.items.length) {
          appendNode(PatternNode(nodeNameString, CountType.one, raw, PrimitiveType.anything));
          break;
        }
      }
    }

    ConstructType type;
    {
      auto object = processor.consumeValueAlreadyCheckedIndex(constructContext, patternList.items, &itemIndex);
      if(object is null) {
	throw processor.semanticError(patternList.items[itemIndex].lineNumber,
				      "expected the expression to return a type but returned null");
      }
      if(object.isListBreak) {
        appendNode(PatternNode(nodeNameString, countType, raw, PrimitiveType.anything));
	continue;
      }

      type = getPatternType(processor, object).unconst;

      /*
      // Check that the pattern is valid so far
      if(type.isAnySymbolType) {
        if(countType != CountType.one) {
          throw processor.semanticError(object.lineNumber, format
                                        ("a symbol pattern node cannot have count type '%s'", countType));
        }
        if(nodes.length > 0) {
          auto previousCountType = nodes[$-1].countType;
          if(previousCountType != CountType.one) {
            throw processor.semanticError(object.lineNumber, format
                                          ("a symbol pattern node cannot be preceded by an object with count type '%s'",
                                           previousCountType));
          }
        }
      }
      */
    }

    if(itemIndex >= patternList.items.length) {
      appendNode(PatternNode(nodeNameString, countType, raw, type.immutable_));
      break;
    }
    if(patternList.items[itemIndex].isListBreak) {
      appendNode(PatternNode(nodeNameString, countType, raw, type.immutable_));
      itemIndex++;
      continue;
    }

    appendNode(PatternNode(nodeNameString, countType, raw, type.immutable_));
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

  return Pattern(opType.immutable_, nodes);
}

unittest
{
  import construct.processor : SemanticException;
  void testGoodPattern(string pattern)
  {
    compileBasicPattern(pattern);
  }
  void testBadPattern(string badPattern)
  {
    try {
      compileBasicPattern(badPattern);
      assert(0, format("expected pattern '%s' to throw an exception but it did not", badPattern));
    } catch(SemanticException e) {
      version(VerboseTests) {
        writefln("Got expected exception from pattern %s: %s", badPattern, e.msg);
      }
    }
  }
  testGoodPattern("(s           symbol)");
  testGoodPattern("(s one       symbol)");
  testBadPattern( "(s optional  symbol)");
  testBadPattern( "(s many      symbol)");
  testBadPattern( "(s oneOrMore symbol)");

  testGoodPattern("(x          , s symbol)");
  testGoodPattern("(x one      , s symbol)");
  testBadPattern( "(x optional , s symbol)");
  testBadPattern( "(x many     , s symbol)");
  testBadPattern( "(x oneOrMore, s symbol)");
}

unittest
{
  size_t assertEqualCount = 0;
  void assertEqual(string patternString, const(Pattern) pattern)
  {
    version(VerboseTests) {
      writefln("Testing pattern '%s'", patternString);
    }
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
     StringPatternPair("aName"                   , immutable PatternNode("aName", CountType.one, PrimitiveType.anything)),
     StringPatternPair("aName one"               , immutable PatternNode("aName", CountType.one, PrimitiveType.anything)),
     StringPatternPair("aName anything"          , immutable PatternNode("aName", CountType.one, PrimitiveType.anything)),
     StringPatternPair("aName one anything"      , immutable PatternNode("aName", CountType.one, PrimitiveType.anything)),

     StringPatternPair("_"                       , immutable PatternNode("_", CountType.one, PrimitiveType.anything)),
     StringPatternPair("_ one"                   , immutable PatternNode("_", CountType.one, PrimitiveType.anything)),
     StringPatternPair("_ anything"              , immutable PatternNode("_", CountType.one, PrimitiveType.anything)),
     StringPatternPair("_ one anything"          , immutable PatternNode("_", CountType.one, PrimitiveType.anything)),

     StringPatternPair("aName optional"          , immutable PatternNode("aName", CountType.optional, PrimitiveType.anything)),
     StringPatternPair("aName optional anything" , immutable PatternNode("aName", CountType.optional, PrimitiveType.anything)),

     StringPatternPair("aName many"              , immutable PatternNode("aName", CountType.many, PrimitiveType.anything)),
     StringPatternPair("aName many anything"     , immutable PatternNode("aName", CountType.many, PrimitiveType.anything)),

     StringPatternPair("aName oneOrMore"         , immutable PatternNode("aName", CountType.oneOrMore, PrimitiveType.anything)),
     StringPatternPair("aName oneOrMore anything", immutable PatternNode("aName", CountType.oneOrMore, PrimitiveType.anything)),

     StringPatternPair("aName string"            , immutable PatternNode("aName", CountType.one, PrimitiveType.string_)),
     StringPatternPair("aName one string"        , immutable PatternNode("aName", CountType.one, PrimitiveType.string_)),

     StringPatternPair("_ string"                , immutable PatternNode("_", CountType.one, PrimitiveType.string_)),
     StringPatternPair("_ one string"            , immutable PatternNode("_", CountType.one, PrimitiveType.string_)),

     StringPatternPair("aName optional string"   , immutable PatternNode("aName", CountType.optional, PrimitiveType.string_)),

     StringPatternPair("aName many string"       , immutable PatternNode("aName", CountType.many, PrimitiveType.string_)),

     StringPatternPair("aName oneOrMore string"  , immutable PatternNode("aName", CountType.oneOrMore, PrimitiveType.string_)),
     ];

  foreach(variant; variants) {
    auto patternNodes = [variant.pattern];
    assertEqual("("~variant.string_~")", immutable Pattern(null, patternNodes.immutable_));
    foreach(secondVariant; variants) {
      auto twoParamPatternNodes = immutable Pattern(null, [variant.pattern, secondVariant.pattern]);
      assertEqual("("~variant.string_~", "~secondVariant.string_~")", twoParamPatternNodes);
    }
  }

  //void assertEqual(immutable(PatternNode)[] patternNodes, string patternString)
  //assertEqual("(this string)", immutable PatternNode());

  version(VerboseTests) {
    writefln("Tested %s patterns", assertEqualCount);
  }
}

class ConstructPatternCombineException : ConstructException
{
  enum Reason {
    patternsAreTheSame,
    patternsAreAmbiguous,
  }
  Reason reason;

  this(Reason reason, string filename = __FILE__, LineNumber line = __LINE__) pure nothrow
  {
    this.reason = reason;
    string message;
    final switch(reason) {
    case Reason.patternsAreTheSame:
      message = "patterns cannot be combined because they are the same";
      break;
    case Reason.patternsAreAmbiguous:
      message = "patterns cannot be combined because they are the ambiguous";
      break;
    }
    super(message, filename, line);
  }
  static ConstructPatternCombineException same(string filename = __FILE__, LineNumber line = __LINE__)
  {
    return new ConstructPatternCombineException(Reason.patternsAreTheSame, filename, line);
  }
  static ConstructPatternCombineException ambiguous(string filename = __FILE__, LineNumber line = __LINE__)
  {
    return new ConstructPatternCombineException(Reason.patternsAreAmbiguous, filename, line);
  }
}
immutable(PatternTree!T) combinePatterns(T)(immutable(PatternTree!T) left, immutable(PatternTree!T) right) //pure
{
  auto newPatternNodes = appender!(immutable(PatternNode)[])();

  size_t leftNodeIndex = 0;
  size_t rightNodeIndex = 0;

  while(true) {
    if(leftNodeIndex >= left.nodes.length) {
      if(rightNodeIndex >= right.nodes.length) {

        return immutable PatternTree!T(newPatternNodes.data, combineBranches!T(left.branchesStructure, right.branchesStructure));

      } else {
        return immutable PatternTree!T
          (newPatternNodes.data, combineBranches(left.branchesStructure,
                                                 immutable BranchesStructure!T([immutable PatternTree!T(right.nodes[rightNodeIndex..$], right.branchesStructure)])));
      }
    }
    if(rightNodeIndex >= right.nodes.length) {
      return immutable PatternTree!T
        (newPatternNodes.data, combineBranches(immutable BranchesStructure!T([immutable PatternTree!T(left.nodes[leftNodeIndex..$], left.branchesStructure)]),
                                               right.branchesStructure));
    }

    auto leftNode = left.nodes[leftNodeIndex];
    auto rightNode = right.nodes[rightNodeIndex];
    if(leftNode != rightNode) {
      return immutable PatternTree!T(newPatternNodes.data,
                                     [immutable PatternTree!T(left.nodes[leftNodeIndex..$], left.branchesStructure),
                                      immutable PatternTree!T(right.nodes[rightNodeIndex..$], right.branchesStructure)]);
    }

    newPatternNodes.put(leftNode);
    leftNodeIndex++;
    rightNodeIndex++;
  }
}

enum CombineResultTag {
  patternBeginningNotCommon,
  ambiguous,
  combined,
}
struct TryCombineResult(T)
{
  CombineResultTag tag;
  immutable(PatternTree!T) tree;
}
// only combines if the pattern trees have common nodes
TryCombineResult!T tryCombine(T)(immutable(PatternTree!T) left, immutable(PatternTree!T) right)
{
  // Check if they have any common nodes
  if(left.nodes.length == 0) {
    if(right.matchesNoObjects()) {
      throw ConstructPatternCombineException.same;
    }

    throw imp();
  }
  if(right.nodes.length == 0) {
    throw imp();
  }

  auto leftNode = left.nodes[0];
  auto rightNode = right.nodes[0];
  if(leftNode.name != rightNode.name) {
    return TryCombineResult!T(CombineResultTag.patternBeginningNotCommon);
  }
  //if(left.nodes[0].name == right.nodes[0].name && left.nodes[0]
  throw imp();
}

immutable(BranchesStructure!T) combineBranches(T)(immutable(BranchesStructure!T) left, immutable(BranchesStructure!T) right)
{
  if(left.branchCount == 0) {
    if(right.branchCount == 0 ) {
      throw ConstructPatternCombineException.same;
    }
    foreach(branch; right.branches) {
      if(branch.matchesNoObjects) {
        throw ConstructPatternCombineException.same;
      }
    }
    return immutable BranchesStructure!T(immutable PatternTree!T(null, left)~right.branches);
  }
  if(right.branchCount == 0) {
    return immutable BranchesStructure!T(immutable PatternTree!T(null, right)~left.branches);
  }

  //
  // Check that there are no ambiguous branches between left and right
  //
  foreach(leftBranch; left.branches) {

    foreach(rightBranch; right.branches) {
      auto result = tryCombine(leftBranch, rightBranch);
      final switch(result.tag) {
      case CombineResultTag.patternBeginningNotCommon:
        break;
      case CombineResultTag.ambiguous:
        throw ConstructPatternCombineException.ambiguous;
      case CombineResultTag.combined:
        throw imp();
        goto DONE_WITH_BRANCH;
      }
    }

  DONE_WITH_BRANCH:

    throw imp();

  }

  throw imp();
}


unittest
{
  auto emptyPatternTree = immutable PatternTree!string(null, "my empty pattern");
  auto stringPatternTree = immutable PatternTree!string(compileBasicPattern("(str string)").nodes.immutable_, "string pattern data");
  auto numberPatternTree = immutable PatternTree!string(compileBasicPattern("(num number)").nodes.immutable_, "number pattern data");

  {
    try {
      combinePatterns(emptyPatternTree, emptyPatternTree);
      assert(0, "did not get expected exception");
    } catch(ConstructPatternCombineException e) {
      assert(e.reason == ConstructPatternCombineException.Reason.patternsAreTheSame);
    }
  }
  {
    auto combined1 = combinePatterns(emptyPatternTree, stringPatternTree);
    auto combined2 = combinePatterns(stringPatternTree, emptyPatternTree);
    auto expected = immutable PatternTree!string(null, [emptyPatternTree, stringPatternTree]);
    assert(combined1.equals(expected));
    assert(combined2.equals(expected));
  }
  {
    auto combined1 = combinePatterns(numberPatternTree, stringPatternTree);
    auto expected1 = immutable PatternTree!string(null, [numberPatternTree, stringPatternTree]);
    assert(combined1.equals(expected1));
    auto combined2 = combinePatterns(stringPatternTree, numberPatternTree);
    auto expected2 = immutable PatternTree!string(null, [stringPatternTree, numberPatternTree]);
    assert(combined2.equals(expected2));
  }
  /*
  {
    auto combined1 = combinePatterns(emptyPatternTree, numberPatternTree);
    assert(combined1.equals(immutable PatternTree!string(null, [emptyPatternTree, numberPatternTree])));
    {
      auto combined = combinePatterns(combined1, stringPatternTree);
      assert(combined.equals(immutable PatternTree!string(null, [emptyPatternTree, numberPatternTree, stringPatternTree])));
    }
    {
      auto combined = combinePatterns(stringPatternTree, combined1);
      assert(combined.equals(immutable PatternTree!string(null, [emptyPatternTree, numberPatternTree, stringPatternTree])));
    }
  }
  */
  logDev("Success");
}
