module construct.patterns;

import std.format : format, formattedWrite;
import std.conv   : to;
import std.array  : appender, Appender;
import construct.util;
import construct.logging;
import construct.parserCore;
import construct.backendCore : ConstructType, ConstructPattern, ConstructClass,
                               ConstructString, ConstructPointer, ConstructBool, ConstructNumber,
                               canBe, tryAs, IPrecedenceConsumer, DefaultPrecedenceConsumer;
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

interface IMatcherVisitHandler
{
  void visit(const(AnyMatcher) matcher);
  void visit(const(SymbolMatcher) matcher);
  void visit(const(KeywordMatcher) matcher);
  void visit(const(PrimitiveTypeMatcher!ConstructPointer) matcher);
  void visit(const(PrimitiveTypeMatcher!ConstructString) matcher);
  void visit(const(PrimitiveTypeMatcher!ConstructBool) matcher);
  void visit(const(PrimitiveTypeMatcher!ConstructNumber) matcher);
  void visit(const(PrimitiveTypeMatcher!ConstructUint) matcher);
  void visit(const(PrimitiveTypeMatcher!ConstructList) matcher);
  void visit(const(PrimitiveTypeMatcher!ObjectBreak) matcher);
  void visit(const(PrimitiveTypeMatcher!ConstructBlock) matcher);
  void visit(const(PrimitiveTypeMatcher!ConstructType) matcher);
  void visit(const(PrimitiveTypeMatcher!ConstructClass) matcher);
  void visit(const(TypedListMatcher) matcher);
  void visit(const(ConstructPattern) matcher);
}

interface Matcher {
  @property abstract string codeText() const pure;

  @property abstract bool isSymbolMatcher() const pure nothrow @nogc;
  @property abstract string processorValueType() const pure;
  @property abstract string processorOptionalValueType() const pure;

  abstract bool match(const(ConstructObject) obj) const pure;

  abstract void visit(IMatcherVisitHandler handler) const;

  alias anything       = AnyMatcher.instance;
  alias symbol         = SymbolMatcher.instance;
  alias pointer        = PrimitiveTypeMatcher!ConstructPointer.instance;
  alias string_        = PrimitiveTypeMatcher!ConstructString.instance;
  alias bool_          = PrimitiveTypeMatcher!ConstructBool.instance;
  alias number         = PrimitiveTypeMatcher!ConstructNumber.instance;
  alias uint_          = PrimitiveTypeMatcher!ConstructUint.instance;
  alias list           = PrimitiveTypeMatcher!ConstructList.instance;
  alias constructBreak = PrimitiveTypeMatcher!ObjectBreak.instance;
  alias block          = PrimitiveTypeMatcher!ConstructBlock.instance;
  alias class_         = PrimitiveTypeMatcher!ConstructClass.instance;
  alias type           = PrimitiveTypeMatcher!ConstructType.instance;
}
class AnyMatcher : Matcher
{
  immutable static AnyMatcher instance = new immutable AnyMatcher();
  private this() immutable { }
  @property override string codeText() const pure { return "anything"; }

  @property final override bool isSymbolMatcher() const pure nothrow @nogc { return false; }
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
  @property final override bool isSymbolMatcher() const pure nothrow @nogc { return true; }
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
  @property final override bool isSymbolMatcher() const pure nothrow @nogc { return false; }
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
  @property final override bool isSymbolMatcher() const pure nothrow @nogc { return false; }
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
    return obj.tryAs!T() !is null;
  }
  final override void visit(IMatcherVisitHandler handler) const
  {
    handler.visit(this);
  }
}
class TypedListMatcher : Matcher
{
  immutable(ConstructType) type;
  string cachedCodeText;
  this(immutable(ConstructType) type) pure immutable
  {
    this.type = type;
    this.cachedCodeText = format("listOf %s", type.matcher.codeText);
  }
  @property override string codeText() const pure { return cachedCodeText; }
  @property final override bool isSymbolMatcher() const pure nothrow @nogc { return false; }
  @property override string processorValueType() const pure
  {
    return "ConstructTypedList";
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
    auto list = obj.tryAsConstructList();
    return list && list.itemType.canBe(type);
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

  string name;
  CountType countType;
  Matcher matcher;

  this(string name, CountType countType, immutable(Matcher) matcher) pure
  {
    if(matcher.isSymbolMatcher) {
      if(countType != CountType.one) {
        throw new Exception(format("A symbol matcher cannot have count type %s", countType));
      }
    }
    this.name = name;
    this.countType = countType;
    this.matcher = matcher.unconst;
  }
  this(string name, CountType countType, immutable(Matcher) matcher) pure immutable
  {
    if(matcher.isSymbolMatcher) {
      if(countType != CountType.one) {
        throw new Exception(format("A symbol matcher cannot have count type %s", countType));
      }
    }
    this.name = cast(string)name;
    this.countType = countType;
    this.matcher = matcher;
  }
  void toString(Sink)(Sink sink) const
  {
    if(countType == CountType.one) {
      formattedWrite(sink, "%s %s", name, matcher.codeText);
    } else {
      formattedWrite(sink, "%s %s %s", name, countType, matcher.codeText);
    }
  }
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
    foreach(branch; branches) {
      if(branch.nodes.length > 0) {
        if(branch.nodes[0].matcher.isSymbolMatcher) {
          throw new Exception("invalid branch, could match a symbol next, creates ambiguity");
        }
      }
    }
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
      sink(node.matcher.codeText);
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
immutable setletPattern =
  [immutable PatternNode("name", CountType.one, Matcher.symbol),
   immutable PatternNode("value", CountType.one, Matcher.anything),
   immutable PatternNode("break_", CountType.optional, Matcher.constructBreak)];

immutable oneSymbolPattern =
  [immutable PatternNode("symbol", CountType.one, Matcher.symbol)];
immutable oneStringPattern =
  [immutable PatternNode("string_", CountType.one, Matcher.string_)];


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

immutable(PatternNode)[] compileBasicPattern(string code)
{
  import construct.parser : standardParser;
  const(ConstructObject)[] parsedObjects = standardParser!(Appender!(const(ConstructObject)[])).func(code);
  assert(parsedObjects.length == 1);
  auto list = parsedObjects[0].tryAsConstructList;
  assert(list);
  return processBasicPattern(list);
}
immutable(PatternNode)[] processBasicPattern(const(ConstructList) patternList)
{
  ConstructProcessor processor = ConstructProcessor(null);
  return processPattern(&processor, DefaultPrecedenceConsumer.instance, patternList);
}
immutable(PatternNode)[] processPattern(ConstructProcessor* processor, const(IPrecedenceConsumer) precedenceConsumer,
                                        const(ConstructList) patternList)
{
  auto nodes = new immutable(PatternNode)[0];
  //auto nodes = appender!(immutable(PatternNode)[])();
  //PatternNode[1024] nodeBuffer;
  //size_t nodeCount = 0;
  void appendNode(immutable(PatternNode) node)
  {
    /*
    if(nodeCount >= nodeBuffer.length) {
      throw new Exception("processPattern node buffer is too small");
    }
    nodeBuffer[nodeCount++] = node.unconst;
    */
    nodes ~= node;
  }

  for(size_t itemIndex = 0;;) {
    if(itemIndex >= patternList.items.length) {
      break;
    }

    string nodeNameString;
    {
      auto nodeName = processor.tryConsumeSymbol(precedenceConsumer, patternList.items, &itemIndex);
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
      auto object = processor.consumeValueAlreadyCheckedIndex(precedenceConsumer, patternList.items, &itemIndex);
      if(object is null) {
	throw processor.semanticError(patternList.items[itemIndex].lineNumber,
				      "expected the expression to return a type but returned null");
      }
      if(object.isListBreak) {
        appendNode(immutable PatternNode(nodeNameString, countType, Matcher.anything));
	continue;
      }

      if(auto nodeType = object.tryAsConstructType) {
	matcher = nodeType.matcher.unconst;
      } else if(auto subPattern = object.tryAsConstructPattern) {
	matcher = subPattern.unconst;
      } else if(auto keywordString = object.tryAsConstructString) {
	matcher = new immutable KeywordMatcher(keywordString.toUtf8).unconst;
      } else {
	throw processor.semanticError(object.lineNumber, format
				      ("expected the expression to return a type but returned %s",
				       An(object.typeName)));
      }

      // Check that the pattern is valid so far
      if(matcher.isSymbolMatcher) {
        if(countType != CountType.one) {
          throw processor.semanticError(object.lineNumber, format
                                        ("a symbol matcher cannot have count type '%s'", countType));
        }
        if(nodes.length > 0) {
          auto previousCountType = nodes[$-1].countType;
          if(previousCountType != CountType.one) {
            throw processor.semanticError(object.lineNumber, format
                                          ("a symbol matcher cannot be preceded by an object with count type '%s'",
                                           previousCountType));
          }
        }
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

  
  //return nodeBuffer[0..nodeCount].idup;
  return nodes;
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
  //writeln(importPattern);
  //writeln(setletPattern);

  size_t assertEqualCount = 0;
  void assertEqual(immutable(PatternNode)[] patternNodes, string patternString)
  {
    version(VerboseTests) {
      writefln("Testing pattern '%s'", patternString);
    }
    assert(patternNodes == compileBasicPattern(patternString));
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
    auto patternNodes = [variant.pattern];
    assertEqual(patternNodes, "("~variant.string_~")");
    foreach(secondVariant; variants) {
      auto twoParamPatternNodes = [variant.pattern, secondVariant.pattern];
      assertEqual(twoParamPatternNodes, "("~variant.string_~", "~secondVariant.string_~")");
    }
  }

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
    case Reason.patternsAreAmbiguous:
      message = "patterns cannot be combined because they are the ambiguous";
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
  auto stringPatternTree = immutable PatternTree!string(compileBasicPattern("(str string)"), "string pattern data");
  auto numberPatternTree = immutable PatternTree!string(compileBasicPattern("(num number)"), "number pattern data");

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
