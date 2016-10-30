module construct.parserCore;

import std.format   : format, formattedWrite;
import std.array    : Appender;
import construct.util;
import construct.patterns    : PatternNode;
import construct.backendCore : PrimitiveTypeEnum, ConstructDefinition, ConstructNumber,
                               ConstructPredicate, ConstructBool, ConstructNullable,
                               ConstructType, ConstructOptionalValue, definition, commonType, canBe,
                               ConstructPattern, ConstructClassDefinition, ConstructClass,
                               ConstructReturn, ConstructPointer, ConstructArray, ConstructString;
import construct.processor   : ConstructProcessor;

enum ConstructClasses =
  ["ConstructObject",
   "ConstructDefinition",
   "ConstructType",
   "ConstructReturn",
   "ObjectBreak",
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
   "ConstructUint",
   "ConstructBlock",
   "ConstructList",
   "ConstructPattern",
   "ConstructClassDefinition",
   "ConstructClass"];

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

  @property
  final bool isObjectBreak() const pure
  {
    return typeid(this) is typeid(ObjectBreak);
  }
  @property
  final bool isListBreak() const pure
  {
    return typeid(this) is typeid(ListBreak);
  }

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
class ObjectBreak : ConstructObject
{
  this(size_t lineNumber) pure
  {
    super(lineNumber);
  }

  mixin finalTypeNameMembers!"constructBreak";
  mixin finalPrimitiveTypeMembers!(PrimitiveTypeEnum.constructBreak);

  enum processorValueType = null;
  enum processorOptionalValueType = "ObjectBreak";

  @property final override inout(ObjectBreak) tryAsObjectBreak() inout pure { return this; }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink(";");
  }
  mixin virtualEqualsMember!ObjectBreak;
  final bool typedEquals(const(ObjectBreak) other) const pure
  {
    return other !is null;
  }
}
class ListBreak : ConstructObject
{
  this(size_t lineNumber) pure
  {
    super(lineNumber);
  }

  mixin finalTypeNameMembers!"listBreak";
  mixin finalPrimitiveTypeMembers!(PrimitiveTypeEnum.anything);

  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink(",");
  }
  mixin virtualEqualsMember!ListBreak;
  final bool typedEquals(const(ListBreak) other) const pure
  {
    return other !is null;
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
}
class ConstructUint : ConstructNumber
{
  size_t value;
  this(size_t lineNumber, size_t value) pure
  {
    super(lineNumber);
    this.value = value;
  }

  mixin finalTypeNameMembers!"uint";
  mixin finalPrimitiveTypeMembers!(PrimitiveTypeEnum.uint_);

  enum processorValueType = "ConstructUint";
  enum processorOptionalValueType = "ConstructUint";

  @property final override inout(ConstructUint) tryAsConstructUint() inout pure { return this; }

  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    formattedWrite(sink, "%s", value);
  }
  mixin virtualEqualsMember!ConstructUint;
  final bool typedEquals(const(ConstructUint) other) const pure
  {
    return value == other.value;
  }
  final override const(ConstructNumber) add(const(ConstructNumber) other) const pure
  {
    if(auto otherUint = other.tryAsConstructUint) {
      return new ConstructUint(0, value + otherUint.value);
    }
    throw imp(format("add %s to %s", typeName, other.typeName));
  }
  final override const(ConstructNumber) multiply(const(ConstructNumber) other) const pure
  {
    if(auto otherUint = other.tryAsConstructUint) {
      return new ConstructUint(0, value * otherUint.value);
    }
    throw imp(format("add %s to %s", typeName, other.typeName));
  }
}
class ConstructBlock : ConstructObject
{
  const(ConstructObject)[] objects;
  this(size_t lineNumber, const(ConstructObject)[] objects = null) pure
  {
    super(lineNumber);
    this.objects = objects;
  }

  mixin finalTypeNameMembers!"constructBlock";
  mixin finalPrimitiveTypeMembers!(PrimitiveTypeEnum.constructBlock);

  enum processorValueType = "ConstructBlock";
  enum processorOptionalValueType = "ConstructBlock";
  @property final override inout(ConstructBlock) tryAsConstructBlock() inout pure { return this; }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("{");
    if(objects.length > 0) {
      objects[0].toString(sink);
      foreach(object; objects[1..$]) {
	sink(" ");
	object.toString(sink);
      }
    }
    sink("}");
  }
  mixin virtualEqualsMember!ConstructBlock;
  final bool typedEquals(const(ConstructBlock) other) const pure
  {
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
class ConstructList : ConstructObject
{
  const(ConstructObject)[] items;
  PrimitiveTypeEnum itemType;
  this(size_t lineNumber, const(ConstructObject)[] items = null) pure
  {
    super(lineNumber);
    this.items = items;

    // Get item type
    if(items.length == 0) {
      itemType = PrimitiveTypeEnum.anything;
    } else {
      itemType = items[0].primitiveTypeEnum;
      for(size_t i = 1; i < items.length; i++) {
	if(itemType == PrimitiveTypeEnum.anything) {
	  break;
	}
	auto item = items[i];
	itemType = commonType(itemType, item.primitiveTypeEnum);
      }
    }
  }

  mixin finalTypeNameMembers!"list";
  mixin finalPrimitiveTypeMembers!(PrimitiveTypeEnum.list);

  enum processorValueType = "ConstructList";
  enum processorOptionalValueType = "ConstructList";

  @property final override inout(ConstructList) tryAsConstructList() inout pure { return this; }
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
  mixin virtualEqualsMember!ConstructList;
  final bool typedEquals(const(ConstructList) other) const pure
  {
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

version(unittest)
{
  import construct.backendCore : PrimitiveType;

  // helper functions to create construct objects for tests
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
