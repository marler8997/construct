module construct.parserCore;

import std.format : format, formattedWrite;
import std.array  : Appender;
import std.bigint : BigInt;

import construct.util;
import construct.logging;
import construct.patterns    : PatternNode;
import construct.backendCore : PrimitiveTypeEnum, ConstructDefinition,
                               ConstructNumber, ConstructInteger, ConstructUnsigned, ConstructUint,
                               ConstructPredicate, ConstructBool, ConstructNullable, ConstructTuple,
                               ConstructType, KeywordType, ConstructOptionalValue, ConstructList,
                               definition, commonType, canBe,
                               ConstructPattern, ConstructClassDefinition, ConstructClass,
                               ConstructStatementMode, ConstructPatternNode,
                               ConstructReturn, ConstructPointer, ConstructArray, ConstructString;
import construct.processor   : ConstructProcessor;

enum ConstructClasses =
  ["ConstructObject",
   "ConstructDefinition",
   "ConstructType",
   "KeywordType",
   "ConstructReturn",
   "ConstructOptionalValue",
   "ConstructNullable",
   "ConstructPointer",
   "ConstructArray",
   "ConstructString",
   "ConstructUtf8",
   "ConstructSymbol",
   "ConstructPredicate",
   "ConstructBool",
   "ConstructNumber",
   "ConstructInteger",
   "ConstructUnsigned",
   "ConstructUint",
   "ConstructList",
   "ConstructDelimitedList",
   "ConstructBlock",
   "ConstructParenList",
   "ConstructBracketList",
   "ConstructTuple",
   "ConstructPattern",
   "ConstructClassDefinition",
   "ConstructClass",
   "ConstructStatementMode",
   "ConstructPatternNode"];

class ConstructException : Exception
{
  this(string msg, string file = __FILE__, size_t line = __LINE__) pure nothrow @nogc
  {
    super(msg, file, line);
  }
}
class ConstructParseException : Exception
{
  size_t constructLineNumber;
  this(size_t constructLineNumber, string msg, string codeFile = __FILE__,
       size_t codeLine = __LINE__) pure
  {
    super(msg, codeFile, codeLine);
    this.constructLineNumber = constructLineNumber;
  }
}
const(ConstructObject)[] done(Appender!(const(ConstructObject)[]) objects) pure
{
  return objects.data;
}
template isConstructBuilder(T)
{
  enum bool isConstructBuilder = is(typeof((inout int = 0) {
    T t = T.init;
    //put(t, ConstructObject.init);
    //const(ConstructObject)[] finished = done(t);
  }));
}

mixin template finalTypeNameMembers(string typeNameString)
{
  enum staticTypeName = typeNameString;
  @property final override string typeName() const pure nothrow @nogc @safe { return staticTypeName; }
}
mixin template virtualTypeNameMembers(string typeNameString)
{
  enum staticTypeName = typeNameString;
  @property override string typeName() const pure nothrow @nogc @safe { return staticTypeName; }
}

mixin template finalPrimitiveTypeMembers(PrimitiveTypeEnum primitiveTypeEnumValue)
{
  enum staticPrimitiveTypeEnum = primitiveTypeEnumValue;
  @property final override PrimitiveTypeEnum primitiveTypeEnum() const pure nothrow @nogc @safe { return staticPrimitiveTypeEnum; }
}
mixin template virtualPrimitiveTypeMembers(PrimitiveTypeEnum primitiveTypeEnumValue)
{
  enum staticPrimitiveTypeEnum = primitiveTypeEnumValue;
  @property override PrimitiveTypeEnum primitiveTypeEnum() const pure nothrow @nogc @safe { return staticPrimitiveTypeEnum; }
}
mixin template virtualEqualsMember(alias Class)
{
  // TODO: default checkLineNumber to false
  override bool equals(const(ConstructObject) otherObj, bool checkLineNumber = true) const pure
  {
    if(checkLineNumber && lineNumber != otherObj.lineNumber) {
      return false;
    }
    auto other = cast(Class)otherObj;
    return other && this.typedEquals(other);
  }
}
abstract class ConstructObject
{
  size_t lineNumber;
  this(size_t lineNumber) immutable pure nothrow @nogc @safe
  {
    this.lineNumber = lineNumber;
  }

  enum staticTypeName = "anything";
  @property abstract string typeName() const pure nothrow @nogc @safe;
  @property abstract immutable(ConstructType) type() const pure;

  enum staticPrimitiveTypeEnum = PrimitiveTypeEnum.anything;
  @property abstract PrimitiveTypeEnum primitiveTypeEnum() const pure nothrow @nogc @safe;

  abstract bool equals(const(ConstructObject) otherObj, bool checkLineNumber = true) const pure;
  abstract void toString(scope void delegate(const(char)[]) sink) const;

  final bool canBe(PrimitiveTypeEnum typeEnum) const pure
  {
    return primitiveTypeEnum.canBe(typeEnum);
  }
  final bool canBe(const(ConstructObject) object) const pure
  {
    return primitiveTypeEnum.canBe(object.primitiveTypeEnum);
  }
  final bool canBe(const(ConstructType) type) const pure
  {
    return primitiveTypeEnum.canBe(type.asPrimitive());
  }

  bool isSymbolOf(const(char)[] symbol) const pure { return false; }

  @property final inout(ConstructObject) tryAsConstructObject() inout pure { return this; }
  mixin(formattedString!(generateItemCode!(", item, item", const(string)[]))
        (ConstructClasses.filter("ConstructObject"),
         "@property inout(%s) tryAs%s() inout pure { return null; }\n"));
}
class ConstructComment : ConstructObject
{
  string value;
  this(size_t lineNumber, string value) pure
  {
    super(lineNumber);
    this.value = value;
  }

  mixin finalTypeNameMembers!"comment";
  mixin finalPrimitiveTypeMembers!(PrimitiveTypeEnum.comment);
  final @property override immutable(ConstructType) type() const pure
  {
    throw imp("ConstructComment.type");
  }

  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("/*");
    sink(value);
    sink("*/");
  }
  mixin virtualEqualsMember!ConstructComment;
  final bool typedEquals(const(ConstructComment) other) const pure
  {
    return this.value == other.value;
  }
}
class ConstructUtf8 : ConstructString
{
  string value;
  this(size_t lineNumber, string value) pure
  {
    super(lineNumber);
    this.value = value;
  }

  mixin finalTypeNameMembers!"utf8";
  mixin finalPrimitiveTypeMembers!(PrimitiveTypeEnum.utf8);
  @property override immutable(ConstructType) type() const pure
  {
    return PrimitiveType.utf8;
  }

  @property final override inout(ConstructUtf8) tryAsConstructUtf8() inout pure { return this; }
  @property override size_t length() const
  {
    return value.length;
  }

  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink(`"`);
    sink(value);
    sink(`"`);
  }
  mixin virtualEqualsMember!ConstructUtf8;
  final bool typedEquals(const(ConstructUtf8) other) const pure
  {
    return value == other.value;
  }

  override string toUtf8() const pure
  {
    return value;
  }
  override size_t stringByteLength(PrimitiveTypeEnum type) const pure
  {
    if(type == PrimitiveTypeEnum.utf8) {
      return value.length;
    }
    throw imp(format("ConstructUtf8 stringByteLength(%s)", type));
  }
}
class ConstructSymbol : ConstructObject
{
  string value;
  this(size_t lineNumber, string value) pure
  {
    super(lineNumber);
    this.value = value;
  }

  mixin finalTypeNameMembers!"symbol";
  mixin finalPrimitiveTypeMembers!(PrimitiveTypeEnum.symbol);
  @property override immutable(ConstructType) type() const pure
  {
    return PrimitiveType.symbol;
  }

  enum processorValueType = "ConstructSymbol";
  @property final override inout(ConstructSymbol) tryAsConstructSymbol() inout pure { return this; }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink(value);
  }
  mixin virtualEqualsMember!ConstructSymbol;
  final bool typedEquals(const(ConstructSymbol) other) const pure
  {
    return value == other.value;
  }
  final override bool isSymbolOf(const(char)[] symbol) const pure
  {
    return this.value == symbol;
  }
}
class ConstructIntegerLiteral : ConstructInteger
{
  this(size_t lineNumber, BigInt value) pure
  {
    super(lineNumber, value);
  }
  this(size_t lineNumber, size_t value) pure
  {
    super(lineNumber, BigInt(value));
  }

  mixin virtualTypeNameMembers!"integerLiteral";
  mixin virtualPrimitiveTypeMembers!(PrimitiveTypeEnum.integerLiteral);
  @property override immutable(ConstructType) type() const pure
  {
    return PrimitiveType.integerLiteral;
  }

  enum processorValueType = "ConstructIntegerLiteral";

  mixin virtualEqualsMember!ConstructIntegerLiteral;
  final bool typedEquals(const(ConstructIntegerLiteral) other) const pure
  {
    return value == other.value;
  }
  override const(ConstructNumber) createNegativeVersion(size_t lineNumber) const
  {
    return new ConstructIntegerLiteral(lineNumber, -value);
  }
  /*
  override const(ConstructNumber) add(const(ConstructNumber) other) const
  {
    if(auto otherIntegerLiteral = other.tryAsConstructIntegerLiteral) {
      return new ConstructIntegerLiteral(0, this.value + otherIntegerLiteral.value);
    }
    throw imp(format("add %s to %s", typeName, other.typeName));
  }
  override const(ConstructNumber) multiply(const(ConstructNumber) other) const
  {
    if(auto otherIntegerLiteral = other.tryAsConstructIntegerLiteral) {
      return new ConstructIntegerLiteral(0, value * otherIntegerLiteral.value);
    }
    throw imp(format("add %s to %s", typeName, other.typeName));
  }
  */
}
enum DelimiterPair {
  braces = 0,
  parens = 1,
  brackets = 2,
}
enum OpenMap = "{([";
enum CloseMap = "})]";
class ConstructDelimitedList : ConstructList
{
  DelimiterPair delimiterPair;
  protected this(size_t lineNumber, DelimiterPair delimiterPair, const(ConstructObject)[] objects) pure
  {
    super(lineNumber, objects);
    this.delimiterPair = delimiterPair;
  }

  enum processorValueType = "ConstructDelimitedList";
  @property final override inout(ConstructDelimitedList) tryAsConstructDelimitedList() inout pure { return this; }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink(OpenMap[cast(ubyte)delimiterPair..cast(ubyte)(delimiterPair+1)]);
    if(objects.length > 0) {
      objects[0].toString(sink);
      foreach(object; objects[1..$]) {
	sink(" ");
	object.toString(sink);
      }
    }
    sink(CloseMap[cast(ubyte)delimiterPair..cast(ubyte)(delimiterPair+1)]);
  }
  mixin virtualEqualsMember!ConstructDelimitedList;
  final bool typedEquals(const(ConstructDelimitedList) other) const pure
  {
    if(delimiterPair != other.delimiterPair) {
      return false;
    }
    if(objects.length != other.objects.length) {
      //writefln("[DEBUG] mismatched length");
      return false;
    }
    foreach(i; 0..objects.length) {
      if(!objects[i].equals(other.objects[i])) {
	//writefln("[DEBUG] mismatched item %s", i);
	return false;
      }
    }
    return true;
  }
}
class ConstructBlock : ConstructDelimitedList
{
  this(size_t lineNumber, const(ConstructObject)[] objects) pure
  {
    super(lineNumber, DelimiterPair.braces, objects);
  }

  mixin finalTypeNameMembers!"constructBlock";
  mixin finalPrimitiveTypeMembers!(PrimitiveTypeEnum.constructBlock);
  @property override immutable(ConstructType) type() const pure
  {
    return PrimitiveType.constructBlock;
  }

  enum processorValueType = "ConstructBlock";
  @property final override inout(ConstructBlock) tryAsConstructBlock() inout pure { return this; }
}
class ConstructParenList : ConstructDelimitedList
{
  this(size_t lineNumber, const(ConstructObject)[] objects) pure
  {
    super(lineNumber, DelimiterPair.braces, objects);
  }

  mixin finalTypeNameMembers!"parenList";
  mixin finalPrimitiveTypeMembers!(PrimitiveTypeEnum.parenList);
  @property override immutable(ConstructType) type() const pure
  {
    return PrimitiveType.parenList;
  }

  enum processorValueType = "ConstructParenList";
  @property final override inout(ConstructParenList) tryAsConstructParenList() inout pure { return this; }
}
class ConstructBracketList : ConstructDelimitedList
{
  this(size_t lineNumber, const(ConstructObject)[] objects) pure
  {
    super(lineNumber, DelimiterPair.braces, objects);
  }

  mixin finalTypeNameMembers!"constructBracketList";
  mixin finalPrimitiveTypeMembers!(PrimitiveTypeEnum.constructBracketList);
  @property override immutable(ConstructType) type() const pure
  {
    return PrimitiveType.bracketList;
  }

  enum processorValueType = "ConstructBracketList";
  @property final override inout(ConstructBracketList) tryAsConstructBracketList() inout pure { return this; }
}



version(unittest)
{
  import construct.backendCore : PrimitiveType;

  // helper functions to create construct objects for tests
  ConstructSymbol break_(size_t lineNumber = 1)
  {
    __gshared static cached = new ConstructSymbol(1, ";");
    return (lineNumber == 1) ? cached : new ConstructSymbol(lineNumber, ";");
  }
  ConstructBlock block(size_t lineNumber, ConstructObject[] objects ...)
  {
    return new ConstructBlock(lineNumber, objects);
  }
  ConstructParenList parenList(size_t lineNumber, ConstructObject[] objects ...)
  {
    return new ConstructParenList(lineNumber, objects);
  }
  ConstructBracketList bracketList(size_t lineNumber, ConstructObject[] objects ...)
  {
    return new ConstructBracketList(lineNumber, objects);
  }
  ConstructSymbol symbol(size_t lineNumber, string value)
  {
    return new ConstructSymbol(lineNumber, value);
  }
  alias prim = PrimitiveTypeEnum;
  immutable(PrimitiveType) type(size_t lineNumber, PrimitiveTypeEnum typeEnum)
  {
    auto typeObject = typeEnum.definition.typeObject;
    if(!typeObject) {
      throw imp(format("PrimitiveTypeEnum.%s typeObject is not configured", typeEnum));
    }
    return typeObject;
  }
  ConstructString string_(string value)
  {
    return string_(0, value);
  }
  ConstructString string_(size_t lineNumber, string value)
  {
    return new ConstructUtf8(lineNumber, value);
  }
}
