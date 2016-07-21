module construct.ir;

import std.stdio  : File, Exception, writeln, writefln, stdout;
import std.array  : Appender, appender;
import std.string : format;
import std.format : formattedWrite;
import std.conv   : to;
import std.typecons : BitFlags;

import construct.patterns : Pattern;
import construct.processor : ConstructProcessor;

Exception imp(string feature = null, string file = __FILE__, size_t line = __LINE__) {
  Exception e;
  if(feature) {
    e = new Exception(feature~": not implemented", file, line);
  } else {
    e = new Exception("not implemented", file, line);
  }
  throw e;
  return e;
}

const(ConstructObject)[] done(Appender!(const(ConstructObject)[]) objects)
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

class ConstructException : Exception
{
  this(string msg, string file = __FILE__, size_t line = __LINE__)
  {
    super(msg, file, line);
  }
}

T singleton(T, A...)(A args)
{
  static T instance;
  if(!instance) {
    instance = new T(args);
  }
  return instance;
}

enum PrimitiveTypeEnum {
  anything,

  named,
  
  void_,
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
  this(string name, PrimitiveTypeEnum typeEnum, PrimitiveTypeEnum parentTypeEnum)
  {
    this.name = name;
    this.typeEnum = typeEnum;
    this.parentTypeEnum = parentTypeEnum;
  }
}
immutable PrimitiveTypeDefinition[] primitiveTypes =
  [
   // NOTE: the anything type must have itself as it's parent
   PrimitiveTypeDefinition("anything" , PrimitiveTypeEnum.anything , PrimitiveTypeEnum.anything),
   PrimitiveTypeDefinition("named"    , PrimitiveTypeEnum.named    , PrimitiveTypeEnum.anything),
   PrimitiveTypeDefinition("void"     , PrimitiveTypeEnum.void_    , PrimitiveTypeEnum.anything),
   PrimitiveTypeDefinition("nullable" , PrimitiveTypeEnum.nullable , PrimitiveTypeEnum.anything),

   PrimitiveTypeDefinition("type"     , PrimitiveTypeEnum.type     , PrimitiveTypeEnum.anything),

   PrimitiveTypeDefinition("symbol"   , PrimitiveTypeEnum.symbol   , PrimitiveTypeEnum.anything),
   
   PrimitiveTypeDefinition("bool"      , PrimitiveTypeEnum.bool_  , PrimitiveTypeEnum.anything),
   
   PrimitiveTypeDefinition("pointer"  , PrimitiveTypeEnum.pointer  , PrimitiveTypeEnum.nullable),
   PrimitiveTypeDefinition("number"   , PrimitiveTypeEnum.number   , PrimitiveTypeEnum.anything),
   PrimitiveTypeDefinition("integer"  , PrimitiveTypeEnum.integer_ , PrimitiveTypeEnum.number),
   PrimitiveTypeDefinition("signed"   , PrimitiveTypeEnum.signed   , PrimitiveTypeEnum.integer_),
   PrimitiveTypeDefinition("int"      , PrimitiveTypeEnum.int_     , PrimitiveTypeEnum.signed),
   PrimitiveTypeDefinition("byte"     , PrimitiveTypeEnum.byte_    , PrimitiveTypeEnum.signed),
   PrimitiveTypeDefinition("unsigned" , PrimitiveTypeEnum.unsigned_, PrimitiveTypeEnum.integer_),
   PrimitiveTypeDefinition("uint"     , PrimitiveTypeEnum.uint_    , PrimitiveTypeEnum.unsigned_),
   PrimitiveTypeDefinition("ubyte"    , PrimitiveTypeEnum.ubyte_   , PrimitiveTypeEnum.unsigned_),
   PrimitiveTypeDefinition("uni"      , PrimitiveTypeEnum.uni      , PrimitiveTypeEnum.anything),

   PrimitiveTypeDefinition("construct"     , PrimitiveTypeEnum.construct     , PrimitiveTypeEnum.nullable),
   PrimitiveTypeDefinition("constructBlock", PrimitiveTypeEnum.constructBlock, PrimitiveTypeEnum.nullable),

   PrimitiveTypeDefinition("list"     , PrimitiveTypeEnum.list     , PrimitiveTypeEnum.anything),

   PrimitiveTypeDefinition("array"    , PrimitiveTypeEnum.array    , PrimitiveTypeEnum.nullable),
   PrimitiveTypeDefinition("string"   , PrimitiveTypeEnum.string   , PrimitiveTypeEnum.array),
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

PrimitiveTypeDefinition definition(PrimitiveTypeEnum typeEnum)
{
  return primitiveTypes[cast(uint)typeEnum];
}
bool canBe(PrimitiveTypeEnum typeEnum, PrimitiveTypeEnum canBeEnum)
{
  return typeEnum == canBeEnum ||
    (typeEnum != PrimitiveTypeEnum.anything && typeEnum.definition.parentTypeEnum.canBe(canBeEnum));
}
bool canBe(PrimitiveTypeEnum typeEnum, ConstructType canBeType)
{
  return typeEnum.canBe(canBeType.asPrimitive());
}
PrimitiveTypeEnum commonType(PrimitiveTypeEnum a, PrimitiveTypeEnum b)
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

version(unittest)
{
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
  ConstructSymbol symbol(size_t lineNumber, const(char)[] value)
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
  ConstructNamedObject named(size_t lineNumber, string name, ConstructObject object)
  {
    return new ConstructNamedObject(lineNumber, name, object);
  }
  ConstructNull null_(size_t lineNumber)
  {
    return new ConstructNull(lineNumber);
  }
}

unittest
{
  // Make sure every type enum has the correct definition in the primitive types table
  // And that every type inherits from the 'anything' type.
  foreach(primitiveTypeEnum; __traits(allMembers, PrimitiveTypeEnum)) {
    writefln("Checking primitive type '%s'", primitiveTypeEnum);
    stdout.flush();
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


version(CheckConst) {
  const(T) unconst(T)(const(T) obj)
  {
    return obj;
  }
} else {
  T unconst(T)(const(T) obj)
  {
    return cast(T)obj;
  }
}

interface ISymbolObject
{
  @property size_t getLineNumber() const;
  
  @property inout(ConstructObject) asOneConstructObject() inout;

  @property inout(ConstructDefinition)  asConstructDefinition() inout;
  @property inout(ConstructType)        asConstructType() inout;
  @property inout(ConstructReturn)      asConstructReturn() inout;
  @property inout(ObjectBreak)          asObjectBreak() inout;
  @property inout(ConstructString)      asConstructString() inout;
  @property inout(ConstructUtf8)        asConstructUtf8() inout;
  @property inout(ConstructSymbol)      asConstructSymbol() inout;
  @property inout(ConstructBool)        asConstructBool() inout;
  @property inout(ConstructNumber)      asConstructNumber() inout;
  @property inout(ConstructUint)        asConstructUint() inout;
  @property inout(ConstructBlock)       asConstructBlock() inout;
  @property inout(ConstructList)        asConstructList() inout;
  @property inout(ConstructNamedObject) asNamedObject() inout;

  @property string typeName() const;

  bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const;

  const(ISymbolObject) member(const(char)[] memberName) const;
}

inout(T) as(T)(inout(ISymbolObject) obj)
{
  static if( is ( T == ConstructDefinition ) ) {
    return obj.asConstructDefinition;
  } else static if( is ( T == ConstructString ) ) {
    return obj.asConstructString;
  } else static if( is ( T == ConstructUtf8 ) ) {
    return obj.asConstructUtf8;
  } else static if( is ( T == ConstructSymbol ) ) {
    return obj.asConstructSymbol;
  } else static if( is ( T == ConstructBlock ) ) {
    return obj.asConstructBlock;
  } else static if( is ( T == ConstructList ) ) {
    return obj.asConstructList;
  } else static if( is ( T == ConstructType ) ) {
    return obj.asConstructType;
  } else static if( is ( T == ConstructReturn ) ) {
    return obj.asConstructReturn;
  } else static if( is ( T == ObjectBreak ) ) {
    return obj.asObjectBreak;
  } else static if( is ( T == ConstructBool ) ) {
    return obj.asConstructBool;
  } else static if( is ( T == ConstructNumber ) ) {
    return obj.asConstructNumber;
  } else static if( is ( T == ConstructUint ) ) {
    return obj.asConstructUint;
  } else static assert(0, "the as(ISymbolObject) template does not support type: "~T.staticTypeName);
}

inout(T) typedMember(T)(inout(ISymbolObject) obj, const(char)[] memberName)
{
  static if( is ( T == ConstructDefinition ) ) {
    throw imp();
  } else static if( is ( T == ConstructString ) ) {
    throw imp();
  } else static if( is ( T == ConstructUtf8 ) ) {
    throw imp();
  } else static if( is ( T == ConstructSymbol ) ) {
    throw imp();
  } else static if( is ( T == ConstructBlock ) ) {
    throw imp();
  } else static if( is ( T == ConstructList ) ) {
    throw imp();
  } else static if( is ( T == ConstructType ) ) {
    throw imp();
  } else static if( is ( T == ConstructReturn ) ) {
    throw imp();
  } else static if( is ( T == ObjectBreak ) ) {
    throw imp();
  } else static if( is ( T == ConstructBool ) ) {
    throw imp();
  } else static if( is ( T == ConstructNumber ) ) {
    throw imp();
  } else static if( is ( T == ConstructUint ) ) {
    throw imp();
  } else static assert(0, "the as(ISymbolObject) template does not support type: "~T.staticTypeName);
}


// Used to print a string with an 'a <string>' or 'an <string>' prefix depending
// on if it starts with a vowel.
// i.e. "thing"  => "a thing"
//      "object" => "an object"
struct An
{
  const(char)[] name;
  void toString(scope void delegate(const(char)[]) sink) const
  {
    if(name.length > 0) {
      auto c = name[0];
      if(c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u') {
	sink("an ");
      } else {
	sink("a ");
      }
      sink(name);
    }
  }
}
An an(T)(T enumValue) if( is( T == enum) )
{
  return An(to!string(enumValue));
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
struct ConstructParam
{
  const(char)[] name;
  const(ConstructType) type;
}
struct ConstructOptionalParam
{
  const(char)[] name;
  const(ConstructType) type;
  const(ConstructObject) defaultValue;
}
struct ProcessResult {
  size_t nextIndex;
  const(ConstructObject) object;
}
class ConstructDefinition : ISymbolObject
{
  const Pattern pattern;
  const size_t lineNumber;
  @property final size_t getLineNumber() const { return lineNumber; }

  const(char)[] filename;
  ConstructAttributes attributes;
  ConstructType evalTo;
  const(ConstructParam)[] requiredParams;
  const(ConstructOptionalParam)[] optionalParams;

  enum staticTypeName = "construct";
  @property
  string typeName() const { return "construct"; }
  
  this(Pattern pattern, size_t lineNumber, const(char)[] filename, ConstructAttributes attributes, ConstructType evalTo,
       const(ConstructParam)[] requiredParams, const(ConstructOptionalParam)[] optionalParams)
  {
    this.pattern = pattern;
    this.lineNumber = lineNumber;
    this.filename = filename;
    this.attributes = attributes;
    this.evalTo = evalTo;
    this.requiredParams = requiredParams;
    this.optionalParams = optionalParams;
  }

  @property final inout(ConstructObject) asOneConstructObject() inout
  {
    throw imp("ConstructDefinition asOneConstructObject");
  }
  
  final override const(ISymbolObject) member(const(char)[] memberName) const
  {
    return null;
  }

  @property
  final inout(ConstructDefinition) asConstructDefinition() inout
  {
    return this;
  }
  @property final inout(ConstructType)        asConstructType() inout { return null; }
  @property final inout(ConstructReturn)      asConstructReturn() inout { return null; }
  @property final inout(ObjectBreak)       asObjectBreak() inout { return null; }
  @property final inout(ConstructString)      asConstructString() inout { return null; }
  @property final inout(ConstructUtf8)        asConstructUtf8() inout { return null; }
  @property final inout(ConstructSymbol)      asConstructSymbol() inout { return null; }
  @property final inout(ConstructBool)        asConstructBool() inout { return null; }
  @property final inout(ConstructNumber)      asConstructNumber() inout { return null; }
  @property final inout(ConstructUint)        asConstructUint() inout { return null; }
  @property final inout(ConstructBlock)       asConstructBlock() inout { return null; }
  @property final inout(ConstructList)        asConstructList() inout { return null; }
  @property final inout(ConstructNamedObject) asNamedObject() inout { return null; }

  bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const
  {
    throw imp("ConstructDefinition equals");
  }
  abstract const(ConstructObject) process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                          const(ConstructObject)[] objects, size_t* argIndex) const;
}
class TypeDefinition : ISymbolObject
{
  const size_t lineNumber;
  @property final size_t getLineNumber() const { return lineNumber; }

  const(char)[] name;
  const(ConstructType) type;
  
  this(size_t lineNumber, const(char)[] name, ConstructType type)
  {
    this.lineNumber = lineNumber;
    this.name = name;
    this.type = type;
  }

  enum staticTypeName = "deftype";
  @property
  string typeName() const { return staticTypeName; }

  @property final inout(ConstructObject) asOneConstructObject() inout { return cast(inout(ConstructObject))type; }

  final override const(ISymbolObject) member(const(char)[] memberName) const
  {
    return null;
  }

  @property final inout(ConstructDefinition) asConstructDefinition() inout { return null; }
  @property final inout(ConstructType) asConstructType() inout
  {
    return cast(inout(ConstructType))type;
  }
  @property final inout(ConstructReturn)      asConstructReturn() inout { return null; }
  @property final inout(ObjectBreak)       asObjectBreak() inout { return null; }
  @property final inout(ConstructString)      asConstructString() inout { return null; }
  @property final inout(ConstructUtf8)        asConstructUtf8() inout { return null; }
  @property final inout(ConstructSymbol)      asConstructSymbol() inout { return null; }
  @property final inout(ConstructBool)        asConstructBool() inout { return null; }
  @property final inout(ConstructNumber)      asConstructNumber() inout { return null; }
  @property final inout(ConstructUint)        asConstructUint() inout { return null; }
  @property final inout(ConstructBlock)       asConstructBlock() inout { return null; }
  @property final inout(ConstructList)        asConstructList() inout { return null; }
  @property final inout(ConstructNamedObject) asNamedObject() inout { return null; }

  bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const
  {
    throw imp("TypeDefinition equals");
  }
  final void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("deftype ");
    sink(name);
  }
}

abstract class ConstructObject : ISymbolObject
{
  size_t lineNumber;
  @property final size_t getLineNumber() const { return lineNumber; }

  this(size_t lineNumber)
  {
    this.lineNumber = lineNumber;
  }

  enum primitiveTypeEnum = PrimitiveTypeEnum.anything;
  enum staticTypeName = "anything";
  @property abstract PrimitiveTypeEnum primitiveType() const;

  abstract void toString(scope void delegate(const(char)[]) sink) const;

  final bool canBe(PrimitiveTypeEnum typeEnum) const
  {
    return primitiveType().canBe(typeEnum);
  }
  final bool canBe(const(ConstructObject) object) const
  {
    return primitiveType().canBe(object.primitiveType());
  }
  final bool canBe(const(ConstructType) type) const
  {
    return primitiveType().canBe(type.asPrimitive());
  }

  
  @property
  final bool isObjectBreak() const
  {
    return typeid(this) is typeid(ObjectBreak);
  }
  @property
  final bool isListBreak() const
  {
    return typeid(this) is typeid(ListBreak);
  }

  override const(ISymbolObject) member(const(char)[] memberName) const
  {
    return null;
  }

  // True if the object returns an immediate value.
  // Used for the processOneConstruct method.
  //@property abstract bool isOneImmediateValue() const;
  
  @property final inout(ConstructObject) asOneConstructObject() inout { return this; }

  @property inout(ConstructDefinition)  asConstructDefinition() inout { return null; }
  @property inout(ConstructType)        asConstructType()       inout { return null; }
  @property inout(ConstructReturn)      asConstructReturn()     inout { return null; }
  @property inout(ObjectBreak)       asObjectBreak()      inout { return null; }
  @property inout(ConstructString)      asConstructString()     inout { return null; }
  @property inout(ConstructUtf8)        asConstructUtf8()       inout { return null; }
  @property inout(ConstructSymbol)      asConstructSymbol()     inout { return null; }
  @property inout(ConstructBool)        asConstructBool()       inout { return null; }
  @property inout(ConstructNumber)      asConstructNumber()     inout { return null; }
  @property inout(ConstructUint)        asConstructUint()       inout { return null; }
  @property inout(ConstructBlock)       asConstructBlock()      inout { return null; }
  @property inout(ConstructList)        asConstructList()       inout { return null; }
  @property inout(ConstructNamedObject) asNamedObject()         inout { return null; }

  const(ConstructObject) typedAs(const(ConstructType) type) const
  {
    throw imp(format("typedAs for %s", typeid(this)));
  }
  
  static ConstructObject fromUnquoted(size_t lineNumber, const(char)[] unquoted)
  {
    //
    // Value Keywords
    //
    if(unquoted == "null") {
      return new ConstructNull(lineNumber);
    }
    if(unquoted == "true" ) {
      return new ConstructBool(lineNumber, true);
    }
    if(unquoted == "false" ) {
      return new ConstructBool(lineNumber, false);
    }
    //
    // Type Keywords
    //
    foreach(primitiveType; primitiveTypes) {
      if(unquoted == primitiveType.name) {
	return new PrimitiveType(lineNumber, primitiveType.typeEnum);
      }
    }
    //
    // Normal Symbol
    //
    return new ConstructSymbol(lineNumber, unquoted);
  }
}


abstract class ConstructType : ConstructObject
{
  this(size_t lineNumber)
  {
    super(lineNumber);
  }
  enum staticTypeName = "type";
  enum primitiveTypeEnum = PrimitiveTypeEnum.type;
  @property final override PrimitiveTypeEnum primitiveType() const { return primitiveTypeEnum; }

  @property abstract PrimitiveTypeEnum asPrimitive() const;
  @property final override inout(ConstructType) asConstructType() inout { return this; }
  
  //@property bool isVoid() { return false; }
}

PrimitiveType create(PrimitiveTypeEnum typeEnum, size_t lineNumber)
{
  return new PrimitiveType(lineNumber, typeEnum);
}
class PrimitiveType : ConstructType
{
  PrimitiveTypeEnum typeEnum;
  this(size_t lineNumber, PrimitiveTypeEnum typeEnum)
  {
    super(lineNumber);
    this.typeEnum = typeEnum;
  }
  final string typeName() const
  {
    return to!string(typeEnum);
  }
  @property final override PrimitiveTypeEnum asPrimitive() const { return typeEnum; }
  override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink(to!string(typeEnum));
  }
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const
  {
    if(checkLineNumber && lineNumber != otherObj.getLineNumber) {
      return false;
    }
    auto other = cast(PrimitiveType)otherObj;
    return other && this.typeEnum == other.typeEnum;
  }
  /*
  final override bool isVoid()
  {
    return typeEnum == PrimitiveTypeEnum.void_;
  }
  */
}
class ConstructReferenceType : ConstructType
{
  const(char)[] constructName;
  this(size_t lineNumber, const(char)[] constructName) {
    super(lineNumber);
    this.constructName = constructName;
  }
  enum staticTypeName = "construct";
  final string typeName() const
  {
    return staticTypeName;
  }
  @property final override PrimitiveTypeEnum asPrimitive() const { return PrimitiveTypeEnum.construct; }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("(construct ");
    sink(constructName);
    sink(")");
  }
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const
  {
    auto other = cast(ConstructType)otherObj;
    return other && equals(other, checkLineNumber);
  }
  final bool equals(const(ConstructReferenceType) other, bool checkLineNumber = true) const
  {
    return this.constructName == other.constructName;
  }
  // TODO: create an abstract method that returns the default value
}
class ConstructTypedListType : ConstructType
{
  const(ConstructType) itemType;
  this(size_t lineNumber, const(ConstructType) itemType) {
    super(lineNumber);
    this.itemType = itemType;
  }
  enum staticTypeName = "list";
  final string typeName() const
  {
    return staticTypeName;
  }
  @property final override PrimitiveTypeEnum asPrimitive() const { return PrimitiveTypeEnum.list; }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("listOf ");
    itemType.toString(sink);
  }
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const
  {
    auto other = cast(ConstructTypedListType)otherObj;
    return other && equals(other, checkLineNumber);
  }
  final bool equals(const(ConstructTypedListType) other, bool checkLineNumber = true) const
  {
    if(checkLineNumber && lineNumber != other.getLineNumber) {
      return false;
    }
    return this.itemType == other.itemType;
  }
}

class ConstructReturn : ConstructObject
{
  const(ConstructObject) returnValue;
  this(size_t lineNumber, const(ConstructObject) returnValue)
  {
    super(lineNumber);
    this.returnValue = returnValue;
  }
  final string typeName() const { return "[return]"; }
  @property final override PrimitiveTypeEnum primitiveType() const { return PrimitiveTypeEnum.void_; }
  @property final override inout(ConstructReturn) asConstructReturn() inout { return this; }
  override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("[return]");
  }
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const
  {
    if(checkLineNumber && lineNumber != otherObj.getLineNumber) {
      return false;
    }
    return cast(ConstructReturn)otherObj !is null;
  }
}

/*
class ConstructVoid : ConstructObject
{
  this(size_t lineNumber)
  {
    super(lineNumber);
  }
  final string typeName() const { return "void"; }
  @property final override PrimitiveTypeEnum primitiveType() const { return PrimitiveTypeEnum.void_; }
  override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("void");
  }
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const
  {
    if(checkLineNumber && lineNumber != otherObj.getLineNumber) {
      return false;
    }
    return cast(ConstructVoid)otherObj !is null;
  }
}
*/
class ConstructNull : ConstructObject
{
  this(size_t lineNumber)
  {
    super(lineNumber);
  }
  final string typeName() const
  {
    return "nullable";
  }
  @property final override PrimitiveTypeEnum primitiveType() const { return PrimitiveTypeEnum.nullable; }
  override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("null");
  }
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const
  {
    if(checkLineNumber && lineNumber != otherObj.getLineNumber) {
      return false;
    }
    return cast(ConstructNull)otherObj !is null;
  }
}

class ConstructPointer : ConstructObject
{
  void* pointer;
  this(size_t lineNumber, void* pointer)
  {
    super(lineNumber);
    this.pointer = pointer;
  }

  enum primitiveTypeEnum = PrimitiveTypeEnum.pointer;
  enum staticTypeName = "pointer";
  final string typeName() const { return staticTypeName; }
  @property final override PrimitiveTypeEnum primitiveType() const { return primitiveTypeEnum; }
  
  //@property final override inout(ConstructPointer) asConstructPointer() inout { return this; }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    formattedWrite(sink, "%s", pointer);
  }
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const
  {
    if(checkLineNumber && lineNumber != otherObj.getLineNumber) {
      return false;
    }
    auto other = cast(ConstructPointer)otherObj;
    return other && this.pointer == other.pointer;
  }
}

class ConstructBool : ConstructObject
{
  bool value;
  this(size_t lineNumber, bool value)
  {
    super(lineNumber);
    this.value = value;
  }
  enum primitiveTypeEnum = PrimitiveTypeEnum.bool_;
  enum staticTypeName = "bool";
  final string typeName() const { return staticTypeName; }
  @property final override PrimitiveTypeEnum primitiveType() const { return primitiveTypeEnum; }
  
  @property final override inout(ConstructBool) asConstructBool() inout { return this; }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink(value ? "true" : "false");
  }
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const
  {
    if(checkLineNumber && lineNumber != otherObj.getLineNumber) {
      return false;
    }
    auto other = cast(ConstructBool)otherObj;
    return other && this.value == other.value;
  }
}


class ConstructNumber : ConstructObject
{
  this(size_t lineNumber)
  {
    super(lineNumber);
  }
  @property final override inout(ConstructNumber) asConstructNumber() inout { return this; }

  abstract const(ConstructNumber) add(const(ConstructNumber) other) const;

  
}

class ConstructUint : ConstructNumber
{
  size_t value;
  this(size_t lineNumber, size_t value)
  {
    super(lineNumber);
    this.value = value;
  }
  enum primitiveTypeEnum = PrimitiveTypeEnum.uint_;
  enum staticTypeName = "uint";
  final string typeName() const
  {
    return "uint";
  }
  @property final override PrimitiveTypeEnum primitiveType() const { return PrimitiveTypeEnum.uint_; }
  @property final override inout(ConstructUint) asConstructUint() inout { return this; }

  /*
  const(ConstructObject) typedAs(const(ConstructType) type) const
  {
    //if(type
  }
  */
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    formattedWrite(sink, "%s", value);
  }
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const
  {
    if(checkLineNumber && lineNumber != otherObj.getLineNumber) {
      return false;
    }
    auto other = cast(ConstructUint)otherObj;
    return other && this.value == other.value;
  }
  final override const(ConstructNumber) add(const(ConstructNumber) other) const
  {
    if(auto otherUint = other.asConstructUint) {
      return new ConstructUint(0, value + otherUint.value);
    }
    throw imp(format("add %s to %s", typeName, other.typeName));
  }
}


// A construct string is explicitly a string.  It cannot be a symbol.
class ConstructString : ConstructObject
{
  this(size_t lineNumber)
  {
    super(lineNumber);
  }
  enum primitiveTypeEnum = PrimitiveTypeEnum.string;
  enum staticTypeName = "string";
  @property final override inout(ConstructString) asConstructString() inout { return this; }

  abstract const(char)[] toUtf8() const;
  abstract size_t stringByteLength(PrimitiveTypeEnum type);
}
class ConstructUtf8 : ConstructString
{
  const(char)[] value;
  this(size_t lineNumber, const(char)[] value)
  {
    super(lineNumber);
    this.value = value;
  }
  enum primitiveTypeEnum = PrimitiveTypeEnum.utf8;
  enum staticTypeName = "utf8";
  final string typeName() const { return staticTypeName; }
  @property final override PrimitiveTypeEnum primitiveType() const { return primitiveTypeEnum; }
  
  @property final override inout(ConstructUtf8) asConstructUtf8() inout { return this; }
  
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink(`"`);
    sink(value);
    sink(`"`);
  }
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const
  {
    if(checkLineNumber && lineNumber != otherObj.getLineNumber) {
      return false;
    }
    auto other = cast(ConstructUtf8)otherObj;
    return other && this.value == other.value;
  }
  final override const(ISymbolObject) member(const(char)[] memberName) const
  {
    if(memberName == "length") {
      return new ConstructUint(0, value.length);
    }
    return null;
  }
  override const(char)[] toUtf8() const
  {
    return value;
  }
  override size_t stringByteLength(PrimitiveTypeEnum type)
  {
    if(type == PrimitiveTypeEnum.utf8) {
      return value.length;
    }
    throw imp(format("ConstructUtf8 stringByteLength(%s)", type));
  }
}

// ConstructSymbolOrString: Could be a symbol or a string?
// ConstructSymbol: is only a symbol (a construct name or variable name, etc)
// ConstructString: is only a string (cannot be a symbol).
// May or may not amend the syntax to support this.
// Quoted/Singlequotes strings are always strings (not symbols).
/*
class ConstructSymbolOrString : ConstructObject
{
  const(char)[] value;
  this(size_t lineNumber, const(char)[] value)
  {
    super(lineNumber);
    this.value = value;
  }
  enum staticTypeName = "symbolOrString";
  override string typeName() const
  {
    return "symbolOrString";
  }
  override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink(value);
  }
  override bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const
  {
    if(checkLineNumber && lineNumber != otherObj.getLineNumber) {
      return false;
    }
    auto other = cast(ConstructSymbolOrString)otherObj;
    return other && this.value == other.value;
  }
}
*/
class ConstructSymbol : ConstructObject
{
  const(char)[] value;
  this(size_t lineNumber, const(char)[] value)
  {
    super(lineNumber);
    this.value = value;
  }
  enum staticTypeName = "symbol";
  final string typeName() const
  {
    return "symbol";
  }
  @property final override PrimitiveTypeEnum primitiveType() const { return PrimitiveTypeEnum.symbol; }
  @property final override inout(ConstructSymbol) asConstructSymbol() inout { return this; }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink(value);
  }
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const
  {
    if(checkLineNumber && lineNumber != otherObj.getLineNumber) {
      return false;
    }
    auto other = cast(ConstructSymbol)otherObj;
    return other && this.value == other.value;
  }
}
/*
class ConstructSymbolRef : ConstructObject
{
  const ConstructSymbol symbol;
  this(size_t lineNumber, const(ConstructSymbol) symbol)
  {
    super(lineNumber);
    this.symbol = symbol;
  }
  enum staticTypeName = "symbolRef";
  @property final string typeName() const { return staticTypeName; }

  @property final override PrimitiveTypeEnum primitiveType() const { return PrimitiveTypeEnum.symbolRef; }
  @property final override inout(ConstructSymbolRef) asConstructSymbolRef() inout { return this; }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("ref ");
    sink(symbol.value);
  }
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const
  {
    if(checkLineNumber && lineNumber != otherObj.getLineNumber) {
      return false;
    }
    auto other = cast(ConstructSymbolRef)otherObj;
    return other && this.symbol == other.symbol;
  }
}
*/
class ConstructComment : ConstructObject
{
  const(char)[] value;
  this(size_t lineNumber, const(char)[] value)
  {
    super(lineNumber);
    this.value = value;
  }
  enum staticTypeName = "comment";
  final string typeName() const
  {
    return "comment";
  }
  @property final override PrimitiveTypeEnum primitiveType() const { return PrimitiveTypeEnum.void_; }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("/*");
    sink(value);
    sink("*/");
  }
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const
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
  this(size_t lineNumber)
  {
    super(lineNumber);
  }
  enum staticTypeName = "[object-break]";
  final string typeName() const
  {
    return "[object-break]";
  }
  @property final override PrimitiveTypeEnum primitiveType() const { return PrimitiveTypeEnum.anything; }
  @property final override inout(ObjectBreak) asObjectBreak() inout { return this; }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink(";");
  }
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const
  {
    if(checkLineNumber && lineNumber != otherObj.getLineNumber) {
      return false;
    }
    return cast(ObjectBreak)otherObj !is null;
  }
}
class ListBreak : ConstructObject
{
  this(size_t lineNumber)
  {
    super(lineNumber);
  }
  enum staticTypeName = "[list-break]";
  final string typeName() const
  {
    return "[list-break]";
  }
  @property final override PrimitiveTypeEnum primitiveType() const { return PrimitiveTypeEnum.anything; }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink(",");
  }
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const
  {
    if(checkLineNumber && lineNumber != otherObj.getLineNumber) {
      return false;
    }
    return cast(ListBreak)otherObj !is null;
  }
  final bool equals(const(ListBreak) other, bool checkLineNumber = true) const
  {
    if(checkLineNumber && lineNumber != other.lineNumber) {
      return false;
    }
    return true;
  }
}

class ConstructList : ConstructObject
{
  const(ConstructObject)[] items;
  PrimitiveTypeEnum itemType;
  this(size_t lineNumber, const(ConstructObject)[] items = null)
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
  final string typeName() const
  {
    return "list";
  }
  @property final override PrimitiveTypeEnum primitiveType() const { return PrimitiveTypeEnum.list; }
  @property final override inout(ConstructList) asConstructList() inout { return this; }
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
  
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const
  {
    auto other = cast(ConstructList)otherObj;
    return other && equals(other, checkLineNumber);
  }
  final bool equals(const(ConstructList) other, bool checkLineNumber = true) const
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


class ConstructBlock : ConstructObject
{
  const(ConstructObject)[] objects;
  this(size_t lineNumber, const(ConstructObject)[] objects = null)
  {
    super(lineNumber);
    this.objects = objects;
  }
  enum primitiveTypeEnum = PrimitiveTypeEnum.constructBlock;
  enum staticTypeName = "constructBlock";
  final string typeName() const
  {
    return "constructBlock";
  }
  @property final override PrimitiveTypeEnum primitiveType() const { return PrimitiveTypeEnum.constructBlock; }
  @property final override inout(ConstructBlock) asConstructBlock() inout { return this; }
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
  
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const
  {
    auto other = cast(ConstructBlock)otherObj;
    return other && equals(other, checkLineNumber);
  }
  final bool equals(const(ConstructBlock) other, bool checkLineNumber = true) const
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

class ConstructNamedObject : ConstructObject
{
  const(char)[] name;
  const(ConstructObject) object;
  this(size_t lineNumber, const(char)[] name, const(ConstructObject) object)
  {
    super(lineNumber);
    this.name = name;
    this.object = object;
  }
  //enum primitiveTypeEnum = PrimitiveTypeEnum.constructBlock;
  enum staticTypeName = "named-object";
  final string typeName() const
  {
    return "named-object";
  }
  @property final override PrimitiveTypeEnum primitiveType() const { return PrimitiveTypeEnum.named; }
  @property final override inout(ConstructNamedObject) asNamedObject() inout { return this; }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink(name);
    sink("=");
    object.toString(sink);
  }
  
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const
  {
    auto other = cast(ConstructNamedObject)otherObj;
    return other && equals(other, checkLineNumber);
  }
  final bool equals(const(ConstructNamedObject) other, bool checkLineNumber = true) const
  {
    if(checkLineNumber && lineNumber != other.lineNumber) {
      return false;
    }
    return name == other.name && object.equals(other.object);
  }
}
