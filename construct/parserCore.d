module construct.parserCore;

import std.format   : format, formattedWrite;
import std.array    : Appender;
import construct.util;
import construct.patterns    : PatternNode, Matcher;
import construct.backendCore : ISymbolObject, PrimitiveTypeEnum, ConstructDefinition, ConstructType,
                               ConstructNumber, ConstructBool, commonType, canBe,
                               ConstructPattern, ConstructClassDefinition, ConstructClass,
                               ConstructReturn, ConstructPointer, ConstructString;
import construct.processor   : ConstructProcessor;

enum ConstructClasses =
  ["ConstructObject",
   "ConstructDefinition",
   "ConstructType",
   "ConstructReturn",
   "ObjectBreak",
   "ConstructPointer",
   "ConstructString",
   "ConstructUtf8",
   "ConstructSymbol",
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
abstract class ConstructObject : ISymbolObject
{
  size_t lineNumber;
  @property final size_t getLineNumber() const pure { return lineNumber; }
  this(size_t lineNumber) pure immutable
  {
    this.lineNumber = lineNumber;
  }

  enum primitiveTypeEnum = PrimitiveTypeEnum.anything;
  enum staticTypeName = "anything";
  @property abstract PrimitiveTypeEnum primitiveType() pure const;

  abstract void toString(scope void delegate(const(char)[]) sink) const;

  final bool canBe(PrimitiveTypeEnum typeEnum) const pure
  {
    return primitiveType().canBe(typeEnum);
  }
  final bool canBe(const(ConstructObject) object) const pure
  {
    return primitiveType().canBe(object.primitiveType());
  }
  final bool canBe(const(ConstructType) type) const pure
  {
    return primitiveType().canBe(type.asPrimitive());
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
  enum staticTypeName = "comment";
  final string typeName() const pure
  {
    return "comment";
  }
  @property final override PrimitiveTypeEnum primitiveType() pure const { return PrimitiveTypeEnum.void_; }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("/*");
    sink(value);
    sink("*/");
  }
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const pure
  {
    if(checkLineNumber && lineNumber != otherObj.getLineNumber) {
      return false;
    }
    auto other = cast(ConstructComment)otherObj;
    return other && this.value == other.value;
  }
}
class ObjectBreak : ConstructObject
{
  this(size_t lineNumber) pure
  {
    super(lineNumber);
  }
  enum staticTypeName = "constructBreak";
  final string typeName() const pure { return staticTypeName; }
  enum processorValueType = null;
  enum processorOptionalValueType = "ObjectBreak";
  @property final override PrimitiveTypeEnum primitiveType() pure const { return PrimitiveTypeEnum.constructBreak; }
  @property final override inout(ObjectBreak) tryAsObjectBreak() inout pure { return this; }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink(";");
  }
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const pure
  {
    if(checkLineNumber && lineNumber != otherObj.getLineNumber) {
      return false;
    }
    return cast(ObjectBreak)otherObj !is null;
  }
}
class ListBreak : ConstructObject
{
  this(size_t lineNumber) pure
  {
    super(lineNumber);
  }
  enum staticTypeName = "[list-break]";
  final string typeName() const pure
  {
    return "[list-break]";
  }
  @property final override PrimitiveTypeEnum primitiveType() pure const { return PrimitiveTypeEnum.anything; }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink(",");
  }
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const pure
  {
    if(checkLineNumber && lineNumber != otherObj.getLineNumber) {
      return false;
    }
    return cast(ListBreak)otherObj !is null;
  }
  final bool equals(const(ListBreak) other, bool checkLineNumber = true) const pure
  {
    if(checkLineNumber && lineNumber != other.lineNumber) {
      return false;
    }
    return true;
  }
}
class ConstructUtf8 : ConstructString
{
  const(char)[] value;
  this(size_t lineNumber, const(char)[] value) pure
  {
    super(lineNumber);
    this.value = value;
  }
  enum primitiveTypeEnum = PrimitiveTypeEnum.utf8;
  enum staticTypeName = "utf8";
  final string typeName() const pure { return staticTypeName; }
  @property final override PrimitiveTypeEnum primitiveType() pure const { return primitiveTypeEnum; }

  @property final override inout(ConstructUtf8) tryAsConstructUtf8() inout pure { return this; }

  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink(`"`);
    sink(value);
    sink(`"`);
  }
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const pure
  {
    if(checkLineNumber && lineNumber != otherObj.getLineNumber) {
      return false;
    }
    auto other = cast(ConstructUtf8)otherObj;
    return other && this.value == other.value;
  }
  override const(char)[] toUtf8() const pure
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
  enum staticTypeName = "symbol";
  final string typeName() const pure
  {
    return "symbol";
  }
  enum processorValueType = "ConstructSymbol";
  @property final override PrimitiveTypeEnum primitiveType() pure const { return PrimitiveTypeEnum.symbol; }
  @property final override inout(ConstructSymbol) tryAsConstructSymbol() inout pure { return this; }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink(value);
  }
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const pure
  {
    if(checkLineNumber && lineNumber != otherObj.getLineNumber) {
      return false;
    }
    auto other = cast(ConstructSymbol)otherObj;
    return other && this.value == other.value;
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
  enum processorValueType = "ConstructUint";
  enum processorOptionalValueType = "ConstructUint";
  enum primitiveTypeEnum = PrimitiveTypeEnum.uint_;
  enum staticTypeName = "uint";
  final string typeName() const pure { return staticTypeName; }

  @property final override PrimitiveTypeEnum primitiveType() pure const { return PrimitiveTypeEnum.uint_; }
  @property final override inout(ConstructUint) tryAsConstructUint() inout pure { return this; }

  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    formattedWrite(sink, "%s", value);
  }
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const pure
  {
    if(checkLineNumber && lineNumber != otherObj.getLineNumber) {
      return false;
    }
    auto other = cast(ConstructUint)otherObj;
    return other && this.value == other.value;
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
  enum primitiveTypeEnum = PrimitiveTypeEnum.constructBlock;
  enum staticTypeName = "constructBlock";
  enum processorValueType = "ConstructBlock";
  enum processorOptionalValueType = "ConstructBlock";
  final string typeName() const pure
  {
    return "constructBlock";
  }
  @property final override PrimitiveTypeEnum primitiveType() pure const { return PrimitiveTypeEnum.constructBlock; }
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
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const pure
  {
    auto other = cast(ConstructBlock)otherObj;
    return other && equals(other, checkLineNumber);
  }
  final bool equals(const(ConstructBlock) other, bool checkLineNumber = true) const pure
  {
    if(checkLineNumber && lineNumber != other.lineNumber) {
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
      itemType = items[0].primitiveType;
      for(size_t i = 1; i < items.length; i++) {
	if(itemType == PrimitiveTypeEnum.anything) {
	  break;
	}
	auto item = items[i];
	itemType = commonType(itemType, item.primitiveType);
      }
    }
  }
  enum staticTypeName = "list";
  final string typeName() const pure
  {
    return "list";
  }
  enum processorValueType = "ConstructList";
  enum processorOptionalValueType = "ConstructList";

  @property final override PrimitiveTypeEnum primitiveType() pure const { return PrimitiveTypeEnum.list; }
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
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const pure
  {
    auto other = cast(ConstructList)otherObj;
    return other && equals(other, checkLineNumber);
  }
  final bool equals(const(ConstructList) other, bool checkLineNumber = true) const pure
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
  PrimitiveType type(size_t lineNumber, PrimitiveTypeEnum typeEnum)
  {
    return new PrimitiveType(lineNumber, typeEnum);
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
