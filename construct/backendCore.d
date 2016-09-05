module construct.backendCore;

import std.format : format, formattedWrite;
import std.stdio  : File, Exception, writeln, writefln, stdout;
import std.array  : Appender, appender;
import std.conv   : to;
import std.typecons : BitFlags;

import construct.util;
import construct.logging;
import construct.parserCore;
import construct.patterns   : PatternNode, Matcher, IMatcherVisitHandler, TypedListMatcher;
import construct.processor  : ConstructProcessor, Scope;

//version = VerboseTests;

enum PrimitiveTypeEnum {
  anything,

  void_,
  constructBreak,
  nullable,

  type,

  symbol,

  bool_,
  pointer,
  number,
  integer_,
  signed,
  int_,
  byte_,
  unsigned_,
  uint_,
  ubyte_,
  uni,

  construct,
  constructBlock,

  list,
  constructPattern,
  class_,

  array,
  string,
  ascii,
  unicode,
  utf8,

  lengthArray,
  lengthAscii,
  lengthUnicode,
  lengthUtf8,

  limitArray,
  limitAscii,
  limitUnicode,
  limitUtf8,
}

struct PrimitiveTypeDefinition
{
  string name;
  PrimitiveTypeEnum typeEnum;
  PrimitiveTypeEnum parentTypeEnum;
  immutable(Matcher) matcher;
  this(string name, PrimitiveTypeEnum typeEnum, PrimitiveTypeEnum parentTypeEnum , immutable(Matcher) matcher = null) pure
  {
    this.name = name;
    this.typeEnum = typeEnum;
    this.parentTypeEnum = parentTypeEnum;
    this.matcher = matcher;
  }
}
immutable PrimitiveTypeDefinition[] primitiveTypes =
  [
   // NOTE: the anything type must have itself as it's parent
   PrimitiveTypeDefinition("anything" , PrimitiveTypeEnum.anything , PrimitiveTypeEnum.anything, Matcher.anything),
   PrimitiveTypeDefinition("void"     , PrimitiveTypeEnum.void_    , PrimitiveTypeEnum.anything),
   PrimitiveTypeDefinition("constructBreak", PrimitiveTypeEnum.constructBreak, PrimitiveTypeEnum.anything, Matcher.constructBreak),
   PrimitiveTypeDefinition("nullable" , PrimitiveTypeEnum.nullable , PrimitiveTypeEnum.anything),

   PrimitiveTypeDefinition("type"     , PrimitiveTypeEnum.type     , PrimitiveTypeEnum.anything, Matcher.type),

   PrimitiveTypeDefinition("symbol"   , PrimitiveTypeEnum.symbol   , PrimitiveTypeEnum.anything, Matcher.symbol),

   PrimitiveTypeDefinition("bool"     , PrimitiveTypeEnum.bool_    , PrimitiveTypeEnum.anything, Matcher.bool_),

   PrimitiveTypeDefinition("pointer"  , PrimitiveTypeEnum.pointer  , PrimitiveTypeEnum.nullable, Matcher.pointer),
   PrimitiveTypeDefinition("number"   , PrimitiveTypeEnum.number   , PrimitiveTypeEnum.anything, Matcher.number),
   PrimitiveTypeDefinition("integer"  , PrimitiveTypeEnum.integer_ , PrimitiveTypeEnum.number),
   PrimitiveTypeDefinition("signed"   , PrimitiveTypeEnum.signed   , PrimitiveTypeEnum.integer_),
   PrimitiveTypeDefinition("int"      , PrimitiveTypeEnum.int_     , PrimitiveTypeEnum.signed),
   PrimitiveTypeDefinition("byte"     , PrimitiveTypeEnum.byte_    , PrimitiveTypeEnum.signed),
   PrimitiveTypeDefinition("unsigned" , PrimitiveTypeEnum.unsigned_, PrimitiveTypeEnum.integer_),
   PrimitiveTypeDefinition("uint"     , PrimitiveTypeEnum.uint_    , PrimitiveTypeEnum.unsigned_, Matcher.uint_),
   PrimitiveTypeDefinition("ubyte"    , PrimitiveTypeEnum.ubyte_   , PrimitiveTypeEnum.unsigned_),
   PrimitiveTypeDefinition("uni"      , PrimitiveTypeEnum.uni      , PrimitiveTypeEnum.anything),

   PrimitiveTypeDefinition("construct"     , PrimitiveTypeEnum.construct     , PrimitiveTypeEnum.nullable),
   PrimitiveTypeDefinition("constructBlock", PrimitiveTypeEnum.constructBlock, PrimitiveTypeEnum.nullable, Matcher.block),

   PrimitiveTypeDefinition("list"            , PrimitiveTypeEnum.list            , PrimitiveTypeEnum.anything, Matcher.list),
   PrimitiveTypeDefinition("constructPattern", PrimitiveTypeEnum.constructPattern, PrimitiveTypeEnum.list),
   PrimitiveTypeDefinition("class"           , PrimitiveTypeEnum.class_          , PrimitiveTypeEnum.nullable, Matcher.class_),

   PrimitiveTypeDefinition("array"    , PrimitiveTypeEnum.array    , PrimitiveTypeEnum.nullable),
   PrimitiveTypeDefinition("string"   , PrimitiveTypeEnum.string   , PrimitiveTypeEnum.array, Matcher.string_),
   PrimitiveTypeDefinition("ascii"    , PrimitiveTypeEnum.ascii    , PrimitiveTypeEnum.string),
   PrimitiveTypeDefinition("unicode"  , PrimitiveTypeEnum.unicode  , PrimitiveTypeEnum.string),
   PrimitiveTypeDefinition("utf8"     , PrimitiveTypeEnum.utf8     , PrimitiveTypeEnum.unicode),

   PrimitiveTypeDefinition("lengthArray"  , PrimitiveTypeEnum.lengthArray    , PrimitiveTypeEnum.array),
   PrimitiveTypeDefinition("lengthAscii"  , PrimitiveTypeEnum.lengthAscii    , PrimitiveTypeEnum.lengthArray),
   PrimitiveTypeDefinition("lengthUnicode", PrimitiveTypeEnum.lengthUnicode  , PrimitiveTypeEnum.lengthArray),
   PrimitiveTypeDefinition("lengthUtf8"   , PrimitiveTypeEnum.lengthUtf8     , PrimitiveTypeEnum.lengthUnicode),

   PrimitiveTypeDefinition("limitArray"  , PrimitiveTypeEnum.limitArray    , PrimitiveTypeEnum.array),
   PrimitiveTypeDefinition("limitAscii"  , PrimitiveTypeEnum.limitAscii    , PrimitiveTypeEnum.limitArray),
   PrimitiveTypeDefinition("limitUnicode", PrimitiveTypeEnum.limitUnicode  , PrimitiveTypeEnum.limitArray),
   PrimitiveTypeDefinition("limitUtf8"   , PrimitiveTypeEnum.limitUtf8     , PrimitiveTypeEnum.limitUnicode),
   ];

PrimitiveTypeDefinition definition(PrimitiveTypeEnum typeEnum) pure
{
  return primitiveTypes[cast(size_t)typeEnum];
}
bool canBe(PrimitiveTypeEnum typeEnum, PrimitiveTypeEnum canBeEnum) pure
{
  return typeEnum == canBeEnum ||
    (typeEnum != PrimitiveTypeEnum.anything && typeEnum.definition.parentTypeEnum.canBe(canBeEnum));
}
bool canBe(PrimitiveTypeEnum typeEnum, const(ConstructType) canBeType) pure
{
  return typeEnum.canBe(canBeType.asPrimitive());
}
PrimitiveTypeEnum commonType(PrimitiveTypeEnum a, PrimitiveTypeEnum b) pure
{
  if(a == PrimitiveTypeEnum.anything || b == PrimitiveTypeEnum.anything) {
    return PrimitiveTypeEnum.anything;
  }
  if(canBe(b, a)) {
    return a;
  }
  if(canBe(a, b)) {
    return b;
  }
  auto result = commonType(a, b.definition.parentTypeEnum);
  if(result != PrimitiveTypeEnum.anything) {
    return result;
  }
  return commonType(a.definition.parentTypeEnum, b);
}
unittest
{
  // Make sure every type enum has the correct definition in the primitive types table
  // And that every type inherits from the 'anything' type.
  foreach(primitiveTypeEnum; __traits(allMembers, PrimitiveTypeEnum)) {
    version(VerboseTests) {
      writefln("Checking primitive type '%s'", primitiveTypeEnum);
      stdout.flush();
    }
    auto index = cast(uint)mixin("PrimitiveTypeEnum."~primitiveTypeEnum);
    if(index >= primitiveTypes.length) {
      throw new Exception("The primitiveTypes table is missing values.");
    }
    auto typeDefinition = mixin("PrimitiveTypeEnum."~primitiveTypeEnum~".definition");
    if(typeDefinition.typeEnum != mixin("PrimitiveTypeEnum."~primitiveTypeEnum)) {
      throw new Exception(format("primitive type '%s' (value=%s) does not match entry in primitive type table '%s' at that index",
				 mixin("PrimitiveTypeEnum."~primitiveTypeEnum), index, typeDefinition.typeEnum));
    }

    // Check inheritahnce
    assert(mixin("PrimitiveTypeEnum."~primitiveTypeEnum~".canBe(PrimitiveTypeEnum.anything)"));
  }
}

inout(T) tryAs(T)(inout(ISymbolObject) obj)
{
  mixin(formattedString!(generateItemCode!(", atFirst ? \"\" : \"else \", item, item", const(string)[]))
        (ConstructClasses, "%sstatic if( is ( T == %s ) ) { return obj.tryAs%s; }\n")~
        q{else static assert(0, "the as(ISymbolObject) template does not support type: "~T.staticTypeName);});
}
inout(T) enforceAs(T)(inout(ISymbolObject) obj)
{
  auto value = tryAs!T(obj);
  if(!value) {
    throw new Exception(format("expected %s but got %s", An(T.staticTypeName), An(obj.typeName)));
  }
  return value;
}

interface ISymbolObject
{
  @property size_t getLineNumber() const pure;

  mixin(formattedString!(generateItemCode!(", item, item", const(string)[]))
        (ConstructClasses, "@property inout(%s) tryAs%s() inout pure;\n"));

  @property string typeName() const pure;
  bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const pure;
}

/*
class ConstructMode : ISymbolObject
{
  
}
*/

PrimitiveType create(PrimitiveTypeEnum typeEnum, size_t lineNumber)
{
  return new PrimitiveType(lineNumber, typeEnum);
}

//
// Initialize Precedence Table
//
shared static this()
{
  Precedence.precedenceTable =
    ["==":1,
     "+":2,
     "-":2,
     "*":3,
     "/":3,
     ".":4];
}
static class Precedence
{
  static bool greaterThan(const(char)[] left, const(char)[] right)
  {
    auto leftPrecedence = getPrecedence(left);
    auto rightPrecedence = getPrecedence(right);
    return leftPrecedence > rightPrecedence;
  }

  static size_t[string] precedenceTable;
  static size_t getPrecedence(const(char)[] opName)
  {
    auto value = precedenceTable.get(cast(string)opName, size_t.max);
    if(value == size_t.max) {
      throw new Exception(format("operator '%s' does not have a precedence set", opName));
    }
    return value;
  }
  /*
  static void setPrecedence(string opName, size_t value)
  {
    auto existingValue = precedenceTable.get(opName, size_t.max);
    if(existingValue != size_t.max) {
      throw new Exception(format("operator precedence of '%s' has been set more than once", opName));
    }
    precedenceTable[opName] = value;
  }
  */
}
interface IPrecedenceConsumer
{
  string getOperatorString() const;
}
class DefaultPrecedenceConsumer : IPrecedenceConsumer
{
  static immutable instance = new immutable DefaultPrecedenceConsumer();
  private this() immutable pure nothrow @nogc @safe {}

  string getOperatorString() const
  {
    return null;
  }
}

struct OpParam
{
  string name;
  immutable(Matcher) matcher;
}
alias ConstructHandler = const(ConstructObject) function
  (ConstructProcessor* processor, const(ConstructDefinition) definition, const(ConstructSymbol) constructSymbol, const(ConstructObject)[] args);
struct PatternHandler
{
  PatternNode[] patternNodes;
  ConstructHandler handler;
}
enum ConstructAttribute {
  // If true, then the implementation code can reach outside the
  // construct to find symbols in the scope of the caller.
  openScope = 1<<0,
  // If true, then everything that happens inside the code block will happen
  // in the scope of the caller (will not create a new scope)
  addSymbolsToParentScope   = 1<<1,
}
alias ConstructAttributes = BitFlags!ConstructAttribute;
class ConstructDefinition : ISymbolObject, IPrecedenceConsumer
{
  string name;
  const size_t lineNumber;
  const(char)[] filename;
  const ConstructAttributes attributes;
  const ConstructType evalTo;

  immutable(OpParam) opParam;

  this(string name, size_t lineNumber, const(char)[] filename, ConstructAttributes attributes, immutable(OpParam) opParam, ConstructType evalTo) pure
  {
    this.name = name;
    this.lineNumber = lineNumber;
    this.filename = filename;
    this.attributes = attributes;
    this.opParam = opParam;
    this.evalTo = evalTo;
  }
  this(string name, size_t lineNumber, string filename, ConstructAttributes attributes, immutable(OpParam) opParam, immutable ConstructType evalTo) pure immutable
  {
    this.name = name;
    this.lineNumber = 0;
    this.filename = filename;
    this.attributes = attributes;
    this.opParam = opParam;
    this.evalTo = evalTo;
  }

  enum staticTypeName = "construct";
  @property
  string typeName() const pure { return staticTypeName; }

  string getOperatorString() const
  {
    return (opParam.matcher is null) ? null : name;
  }

  @property final size_t getLineNumber() const pure { return lineNumber; }


  
  @property final inout(ConstructObject) tryAsConstructObject() inout pure
  {
    throw imp("ConstructDefinition tryAsConstructObject");
  }
  @property
  final inout(ConstructDefinition) tryAsConstructDefinition() inout pure
  {
    return this;
  }
  mixin(formattedString!(generateItemCode!(", item, item", const(string)[]))
        (ConstructClasses.filter("ConstructObject", "ConstructDefinition"),
         "@property final inout(%s) tryAs%s() inout pure { return null; }\n"));

  bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const pure
  {
    throw imp("ConstructDefinition equals");
  }

  abstract const(PatternHandler)[] getPatternHandlers() const;
  abstract const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
						   const(ConstructObject) opParam, const(ConstructObject)[] objects, size_t* index) const;

  const(ConstructObject) process2(ConstructProcessor* processor,
      const(ConstructSymbol) constructSymbol, const(ConstructObject)[] args) const
  {
    throw imp(format("construct '%s', process2", constructSymbol.value));
  }
}
class NoPatternConstruct : ConstructDefinition
{
  this(string name, size_t lineNumber, string filename, ConstructAttributes attributes, immutable ConstructType evalTo) pure immutable
  {
    super(name, lineNumber, filename, attributes, OpParam(), evalTo);
  }
  final override const(PatternHandler)[] getPatternHandlers() const { return null; }
}
class PatternConstructDefinition : ConstructDefinition
{
  immutable(PatternHandler)[] patternHandlers;
  this(string name, immutable(OpParam) opParam, immutable(PatternHandler)[] patternHandlers, size_t lineNumber,
       string filename, ConstructAttributes attributes, immutable ConstructType evalTo) pure immutable
  {
    super(name, lineNumber, filename, attributes, opParam, evalTo);
    this.patternHandlers = patternHandlers;
  }
  final override const(PatternHandler)[] getPatternHandlers() const { return patternHandlers; }
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
							 const(ConstructObject) opParam, const(ConstructObject)[] objects, size_t* index) const
  {
    throw new Exception("cannot call processNoPattern on a PatternConstructDefinition");
  }
}
class InternalPatternConstructDefinition : PatternConstructDefinition
{
  this(string name, immutable(OpParam) opParam, immutable(PatternHandler)[] patternHandlers,
       ConstructAttributes attributes, immutable ConstructType evalTo, LineNumber lineNumber = __LINE__, string filename = __FILE__) pure immutable
  {
    super(name, opParam, patternHandlers, lineNumber, filename, attributes, evalTo);
  }
}

abstract class ConstructType : ConstructObject
{
  this(size_t lineNumber) pure
  {
    super(lineNumber);
  }

  enum staticTypeName = "type";
  string typeName() const pure { return staticTypeName; }

  enum primitiveTypeEnum = PrimitiveTypeEnum.type;
  @property final override PrimitiveTypeEnum primitiveType() pure const { return primitiveTypeEnum; }

  enum processorValueType = "ConstructType";
  enum processorOptionalValueType = "ConstructType";

  @property abstract PrimitiveTypeEnum asPrimitive() const pure;
  @property final override inout(ConstructType) tryAsConstructType() inout { return this; }

  @property abstract immutable(Matcher) matcher() const pure;
}
class PrimitiveType : ConstructType
{
  PrimitiveTypeEnum typeEnum;
  this(size_t lineNumber, PrimitiveTypeEnum typeEnum) pure
  {
    super(lineNumber);
    this.typeEnum = typeEnum;
  }
  @property final override PrimitiveTypeEnum asPrimitive() const pure { return typeEnum; }
  override void toString(scope void delegate(const(char)[]) sink) const
  {
    typeEnum.to!string;
  }
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const pure
  {
    if(checkLineNumber && lineNumber != otherObj.getLineNumber) {
      return false;
    }
    auto other = cast(PrimitiveType)otherObj;
    return other && this.typeEnum == other.typeEnum;
  }
  @property override immutable(Matcher) matcher() const pure
  {
    auto matcher = typeEnum.definition.matcher;
    if(!matcher) {
      throw imp(format("matcher for '%s' is not configured yet", typeEnum.definition.name));
    }
    return matcher;
  }
}
class ConstructClassDefinition : ConstructType
{
  string name;
  Scope scope_;
  this(size_t lineNumber, string name, Scope scope_) pure
  {
    super(lineNumber);
    this.name = name;
    this.scope_ = scope_;
  }
  @property final override PrimitiveTypeEnum asPrimitive() const pure { return PrimitiveTypeEnum.class_; }
  override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink(name);
  }
  @property final override inout(ConstructClassDefinition) tryAsConstructClassDefinition() inout pure { return this; }
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const pure
  {
    if(checkLineNumber && lineNumber != otherObj.getLineNumber) {
      return false;
    }
    auto other = cast(ConstructClassDefinition)otherObj;
    return other && equals(cast(ConstructClassDefinition)otherObj);
  }
  final bool equals(const(ConstructClassDefinition) other) const pure
  {
    return this.name == other.name;
  }
  @property override immutable(Matcher) matcher() const pure
  {
    throw imp("ConstructClassDefinition.matcher()");
    /*
    auto matcher = typeEnum.definition.matcher;
    if(!matcher) {
      throw imp(format("matcher for '%s' is not configured yet", typeEnum.definition.name));
    }
    return matcher;
    */
  }
}
class ConstructClass : ConstructObject
{
  const(ConstructClassDefinition) classDef;
  this(size_t lineNumber, const(ConstructClassDefinition) classDef)
  {
    super(lineNumber);
    this.classDef = classDef;
  }

  enum primiviteTypeEnum = PrimitiveTypeEnum.class_;
  @property final override PrimitiveTypeEnum primitiveType() pure const { return primitiveTypeEnum; }

  enum staticTypeName = "class";
  final string typeName() const pure { return classDef.name; }

  enum processorValueType = "ConstructClass";
  enum processorOptionalValueType = "ConstructClass";

  @property final override inout(ConstructClass) tryAsConstructClass() inout pure { return this; }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    throw imp("ConstructClass.toString");
    //formattedWrite(sink, "%s", class);
  }
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const pure
  {
    if(checkLineNumber && lineNumber != otherObj.getLineNumber) {
      return false;
    }
    auto other = cast(ConstructClass)otherObj;
    return other && equals(other);
  }
  final bool equals(const(ConstructClass) otherClass) const pure
  {
    throw imp("ConstructClass.equals");
  }
}


class ConstructTypedListType : ConstructType
{
  const(ConstructType) itemType;
  this(size_t lineNumber, const(ConstructType) itemType) pure {
    super(lineNumber);
    this.itemType = itemType;
  }
  enum staticTypeName = "list";
  @property final override PrimitiveTypeEnum asPrimitive() const pure { return PrimitiveTypeEnum.list; }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("listOf ");
    itemType.toString(sink);
  }
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const pure
  {
    auto other = cast(ConstructTypedListType)otherObj;
    return other && equals(other, checkLineNumber);
  }
  final bool equals(const(ConstructTypedListType) other, bool checkLineNumber = true) const pure
  {
    if(checkLineNumber && lineNumber != other.getLineNumber) {
      return false;
    }
    return this.itemType.equals(other.itemType);
  }
  @property override immutable(Matcher) matcher() const pure
  {
    return new immutable TypedListMatcher(cast(immutable(ConstructType))itemType);
  }
}
class ConstructNull : ConstructObject
{
  this(size_t lineNumber) pure
  {
    super(lineNumber);
  }
  final string typeName() const pure
  {
    return "nullable";
  }
  @property final override PrimitiveTypeEnum primitiveType() pure const { return PrimitiveTypeEnum.nullable; }
  override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("null");
  }
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const pure
  {
    if(checkLineNumber && lineNumber != otherObj.getLineNumber) {
      return false;
    }
    return cast(ConstructNull)otherObj !is null;
  }
}
class ConstructBool : ConstructObject
{
  bool value;
  this(size_t lineNumber, bool value) pure
  {
    super(lineNumber);
    this.value = value;
  }
  enum primitiveTypeEnum = PrimitiveTypeEnum.bool_;
  enum staticTypeName = "bool";
  final string typeName() const pure { return staticTypeName; }
  enum processorValueType = "ConstructBool";
  enum processorOptionalValueType = "ConstructBool";
  @property final override PrimitiveTypeEnum primitiveType() pure const { return primitiveTypeEnum; }

  @property final override inout(ConstructBool) tryAsConstructBool() inout pure { return this; }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink(value ? "true" : "false");
  }
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const pure
  {
    if(checkLineNumber && lineNumber != otherObj.getLineNumber) {
      return false;
    }
    auto other = cast(ConstructBool)otherObj;
    return other && this.value == other.value;
  }
}
class ConstructReturn : ConstructObject
{
  const(ConstructObject) returnValue;
  this(size_t lineNumber, const(ConstructObject) returnValue) pure
  {
    super(lineNumber);
    this.returnValue = returnValue;
  }
  final string typeName() const pure { return "[return]"; }
  @property final override PrimitiveTypeEnum primitiveType() pure const { return PrimitiveTypeEnum.void_; }
  @property final override inout(ConstructReturn) tryAsConstructReturn() inout pure { return this; }
  override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("[return]");
  }
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const pure
  {
    if(checkLineNumber && lineNumber != otherObj.getLineNumber) {
      return false;
    }
    return cast(ConstructReturn)otherObj !is null;
  }
}
class ConstructNumber : ConstructObject
{
  this(size_t lineNumber) pure
  {
    super(lineNumber);
  }
  enum processorValueType = "ConstructNumber";
  enum processorOptionalValueType = "ConstructNumber";
  @property final override inout(ConstructNumber) tryAsConstructNumber() inout pure { return this; }

  abstract const(ConstructNumber) add(const(ConstructNumber) other) const pure;
  abstract const(ConstructNumber) multiply(const(ConstructNumber) other) const pure;
}
class ConstructPointer : ConstructObject
{
  void* pointer;
  this(size_t lineNumber, void* pointer) pure
  {
    super(lineNumber);
    this.pointer = pointer;
  }

  enum primitiveTypeEnum = PrimitiveTypeEnum.pointer;
  enum staticTypeName = "pointer";
  final string typeName() const pure { return staticTypeName; }
  enum processorValueType = "ConstructPointer";
  enum processorOptionalValueType = "ConstructPointer";
  @property final override PrimitiveTypeEnum primitiveType() pure const { return primitiveTypeEnum; }

  @property final override inout(ConstructPointer) tryAsConstructPointer() inout pure { return this; }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    formattedWrite(sink, "%s", pointer);
  }
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const pure
  {
    if(checkLineNumber && lineNumber != otherObj.getLineNumber) {
      return false;
    }
    auto other = cast(ConstructPointer)otherObj;
    return other && this.pointer == other.pointer;
  }
}
class ConstructString : ConstructObject
{
  this(size_t lineNumber) pure
  {
    super(lineNumber);
  }
  enum primitiveTypeEnum = PrimitiveTypeEnum.string;
  enum staticTypeName = "string";
  @property final override inout(ConstructString) tryAsConstructString() inout pure { return this; }
  enum processorValueType = "ConstructString";
  enum processorOptionalValueType = "ConstructString";

  abstract const(char)[] toUtf8() const pure;
  abstract size_t stringByteLength(PrimitiveTypeEnum type) const pure;
}
class ConstructPattern : ConstructObject, Matcher
{
  const(PatternNode)[] patternNodes;
  this(size_t lineNumber, const(PatternNode)[] patternNodes) pure
  {
    super(lineNumber);
    this.patternNodes = patternNodes;
  }
  this(size_t lineNumber, immutable(PatternNode)[] patternNodes) pure immutable
  {
    super(lineNumber);
    this.patternNodes = patternNodes;
  }
  final string typeName() const pure { return "pattern"; }
  @property final override PrimitiveTypeEnum primitiveType() pure const { return PrimitiveTypeEnum.constructPattern; }
  override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("<pattern.toString not implemented>");
  }
  @property final override inout(ConstructPattern) tryAsConstructPattern() inout pure { return this; }
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const pure
  {
    if(checkLineNumber && lineNumber != otherObj.getLineNumber) {
      return false;
    }
    throw imp("ConstructPattern.equals");
    //return cast(ConstructReturn)otherObj !is null;
  }

  //
  // Matcher Implementation
  //
  @property override string codeText() const pure { return "(a sub pattern...codeText not implemented)"; }
  @property final override bool isSymbolMatcher() const pure nothrow @nogc { return false; }
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


/*
class ConstructTypedList(T) : ConstructObject
{
  const(T)[] items;
  this(size_t lineNumber, const(T)[] items = null) pure
  {
    super(lineNumber);
    this.items = items;
  }
  enum staticTypeName = "listOf "~T.staticTypeName;
  final string typeName() const pure { return staticTypeName; }
  enum processorValueType = "ConstructTypedList!("~T.stringof~")";
  enum processorOptionalValueType = processorValueType;

  @property final override PrimitiveTypeEnum primitiveType() pure const { return PrimitiveTypeEnum.list; }
  @property final override inout(ConstructList) tryAsConstructList() inout pure { throw imp("ConstructTypedList.tryAsConstructList"); }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("(");
    if(items.length > 0) {
      items[0].toString(sink);
      foreach(item; items[1..$]) {
	sink(" ");
	item.toString(sink);
      }
    }
    sink(")");
  }

  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const pure
  {
    auto other = cast(ConstructTypedList!T)otherObj;
    return other && equals(other, checkLineNumber);
  }
  final bool equals(const(ConstructTypedList!T) other, bool checkLineNumber = true) const pure
  {
    if(checkLineNumber && lineNumber != other.lineNumber) {
      return false;
    }
    if(items.length != other.items.length) {
      return false;
    }
    foreach(i; 0..items.length) {
      if(!items[i].equals(other.items[i])) {
	return false;
      }
    }
    return true;
  }
}
*/
