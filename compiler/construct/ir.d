module construct.ir;

import std.stdio  : File, Exception, writeln, writefln, stdout;
import std.array  : Appender, appender;
import std.string : format;
import std.format : formattedWrite;
import std.conv   : to;

import construct.processor : ProcessorState;

class ConstructException : Exception
{
  this(string msg, string file = __FILE__, size_t line = __LINE__)
  {
    super(msg, file, line);
  }
}


T singleton(T)()
{
  static T instance;
  if(!instance) {
    instance = new T();
  }
  return instance;
}

enum PrimitiveTypeEnum {
  anything,
  void_,
  nullable,
  
  type,

  symbol, // Used for construct names (think of more)

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
  if(PrimitiveType primitiveType = cast(PrimitiveType)canBeType) {
    return typeEnum.canBe(primitiveType.typeEnum);
  }
  throw new Exception("canBe non primitive type not implemented");
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

abstract class ConstructObject
{
  size_t lineNumber;
  this(size_t lineNumber)
  {
    this.lineNumber = lineNumber;
  }
  @property
  enum primitiveTypeEnum = PrimitiveTypeEnum.anything;
  enum staticTypeName = "anything";
  abstract string typeName() const;
  abstract void toString(scope void delegate(const(char)[]) sink) const;
  abstract bool equals(const(ConstructObject) otherObj, bool checkLineNumber = true) const;

  abstract bool canBe(ConstructType canBeType);

  
  abstract BackendObject toBackendObject() const;
  
  @property
  final bool isListBreak() const
  {
    return !(cast(ListBreak)this is null);
  }

  static ConstructObject fromUnquoted(size_t lineNumber, const(char)[] unquoted)
  {
    if(unquoted == "null") {
      return new ConstructNull(lineNumber);
    }
    if(unquoted == "true" ) {
      return new ConstructBool(lineNumber, true);
    }
    if(unquoted == "false" ) {
      return new ConstructBool(lineNumber, false);
    }
    foreach(primitiveType; primitiveTypes) {
      if(unquoted == primitiveType.name) {
	return new PrimitiveType(lineNumber, primitiveType.typeEnum);
      }
    }
    return new ConstructSymbol(lineNumber, unquoted);
  }
  static PrimitiveType lookupType(size_t lineNumber, const(char)[] name) {
    foreach(primitiveType; primitiveTypes) {
      if(name == primitiveType.name) {
	return new PrimitiveType(lineNumber, primitiveType.typeEnum);
      }
    }
    return null;
  }

  
}
abstract class BackendObject : ConstructObject
{
  this(size_t lineNumber)
  {
    super(lineNumber);
  }
  final override BackendObject toBackendObject() const
  {
    return cast(BackendObject)this;
  }
}


abstract class ConstructType : BackendObject
{
  this(size_t lineNumber)
  {
    super(lineNumber);
  }
  enum staticTypeName = "type";
  enum primitiveTypeEnum = PrimitiveTypeEnum.type;
  abstract bool canBe(PrimitiveTypeEnum canBeEnum);

  override bool canBe(ConstructType canBeType)
  {
    return PrimitiveTypeEnum.type.canBe(canBeType);
  }

  
  @property
  bool isVoid() { return false; }
}

PrimitiveType create(PrimitiveTypeEnum typeEnum, size_t lineNumber)
{
  return new PrimitiveType(lineNumber, typeEnum);
}
class PrimitiveType : ConstructType {
  PrimitiveTypeEnum typeEnum;
  this(size_t lineNumber, PrimitiveTypeEnum typeEnum)
  {
    super(lineNumber);
    this.typeEnum = typeEnum;
  }
  override string typeName() const
  {
    return to!string(typeEnum);
  }
  override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink(to!string(typeEnum));
  }
  override bool equals(const(ConstructObject) otherObj, bool checkLineNumber = true) const
  {
    if(checkLineNumber && lineNumber != otherObj.lineNumber) {
      return false;
    }
    auto other = cast(PrimitiveType)otherObj;
    return other && this.typeEnum == other.typeEnum;
  }
  override bool canBe(PrimitiveTypeEnum canBeEnum)
  {
    return typeEnum.canBe(canBeEnum);
  }
  final override bool isVoid()
  {
    return typeEnum == PrimitiveTypeEnum.void_;
  }
}
class ConstructReferenceType : ConstructType {
  const(char)[] constructName;
  this(size_t lineNumber, const(char)[] constructName) {
    super(lineNumber);
    this.constructName = constructName;
  }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("(construct ");
    sink(constructName);
    sink(")");
  }
  final override bool equals(const(ConstructObject) otherObj, bool checkLineNumber = true) const
  {
    auto other = cast(ConstructType)otherObj;
    return other && equals(other, checkLineNumber);
  }
  final bool equals(const(ConstructReferenceType) other, bool checkLineNumber = true) const
  {
    return this.constructName == other.constructName;
  }
  override bool canBe(ConstructType canBeType)
  {
    return PrimitiveTypeEnum.construct.canBe(canBeType);
  }
  // TODO: create an abstract method that returns the default value
}

class ConstructNull : BackendObject
{
  this(size_t lineNumber)
  {
    super(lineNumber);
  }
  override string typeName() const
  {
    return "nullable";
  }
  override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("null");
  }
  override bool equals(const(ConstructObject) otherObj, bool checkLineNumber = true) const
  {
    if(checkLineNumber && lineNumber != otherObj.lineNumber) {
      return false;
    }
    return cast(ConstructNull)otherObj !is null;
  }
  override bool canBe(ConstructType canBeType)
  {
    return PrimitiveTypeEnum.nullable.canBe(canBeType);
  }
}

class ConstructBool : BackendObject
{
  bool value;
  this(size_t lineNumber, bool value)
  {
    super(lineNumber);
    this.value = value;
  }
  enum primitiveTypeEnum = PrimitiveTypeEnum.bool_;
  enum staticTypeName = "bool";
  final override string typeName() const
  {
    return "bool";
  }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink(value ? "true" : "false");
  }
  final override bool equals(const(ConstructObject) otherObj, bool checkLineNumber = true) const
  {
    if(checkLineNumber && lineNumber != otherObj.lineNumber) {
      return false;
    }
    auto other = cast(ConstructBool)otherObj;
    return other && this.value == other.value;
  }
  override bool canBe(ConstructType canBeType)
  {
    return PrimitiveTypeEnum.bool_.canBe(canBeType);
  }
}


// A construct string is explicitly a string.  It cannot be a symbol.
class ConstructString : BackendObject
{
  const(char)[] value;
  this(size_t lineNumber, const(char)[] value)
  {
    super(lineNumber);
    this.value = value;
  }
  enum primitiveTypeEnum = PrimitiveTypeEnum.string;
  enum staticTypeName = "string";
  final override string typeName() const
  {
    return "string";
  }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink(`"`);
    sink(value);
    sink(`"`);
  }
  final override bool equals(const(ConstructObject) otherObj, bool checkLineNumber = true) const
  {
    if(checkLineNumber && lineNumber != otherObj.lineNumber) {
      return false;
    }
    auto other = cast(ConstructString)otherObj;
    return other && this.value == other.value;
  }
  override bool canBe(ConstructType canBeType)
  {
    return PrimitiveTypeEnum.string.canBe(canBeType);
  }
}

// ConstructSymbolOrString: Could be a symbol or a string?
// ConstructSymbol: is only a symbol (a construct name or variable name, etc)
// ConstructString: is only a string (cannot be a symbol).
// May or may not amend the syntax to support this.
// Quoted/Singlequotes strings are always strings (not symbols).
/*
class ConstructSymbolOrString : BackendObject
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
  override bool equals(const(ConstructObject) otherObj, bool checkLineNumber = true) const
  {
    if(checkLineNumber && lineNumber != otherObj.lineNumber) {
      return false;
    }
    auto other = cast(ConstructSymbolOrString)otherObj;
    return other && this.value == other.value;
  }
}
*/
class ConstructSymbol : BackendObject
{
  const(char)[] value;
  this(size_t lineNumber, const(char)[] value)
  {
    super(lineNumber);
    this.value = value;
  }
  enum staticTypeName = "symbol";
  final override string typeName() const
  {
    return "symbol";
  }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink(value);
  }
  final override bool equals(const(ConstructObject) otherObj, bool checkLineNumber = true) const
  {
    if(checkLineNumber && lineNumber != otherObj.lineNumber) {
      return false;
    }
    auto other = cast(ConstructSymbol)otherObj;
    return other && this.value == other.value;
  }
  override bool canBe(ConstructType canBeType)
  {
    return PrimitiveTypeEnum.symbol.canBe(canBeType);
  }
}




class LongLiteral : BackendObject
{
  long value;
  this(size_t lineNumber, long value)
  {
    super(lineNumber);
    this.value = value;
  }
  final override string typeName() const
  {
    return "long";
  }
  override void toString(scope void delegate(const(char)[]) sink) const
  {
    formattedWrite(sink, "%s", value);
  }
  override bool equals(const(ConstructObject) otherObj, bool checkLineNumber = true) const
  {
    if(checkLineNumber && lineNumber != otherObj.lineNumber) {
      return false;
    }
    auto other = cast(LongLiteral)otherObj;
    return other && this.value == other.value;
  }
  override bool canBe(ConstructType canBeType)
  {
    return PrimitiveTypeEnum.uint_.canBe(canBeType);
  }
}

class ListBreak : BackendObject
{
  /*
  public static immutable ListBreak instance = new immutable ListBreak();
  private this() immutable {
  }
  */
  this(size_t lineNumber)
  {
    super(lineNumber);
  }
  enum staticTypeName = "[list-break]";
  final override string typeName() const
  {
    return "[list-break]";
  }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink(",");
  }
  final override bool equals(const(ConstructObject) otherObj, bool checkLineNumber = true) const
  {
    if(checkLineNumber && lineNumber != otherObj.lineNumber) {
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
  override bool canBe(ConstructType canBeType)
  {
    return PrimitiveTypeEnum.anything.canBe(canBeType);
  }
}

// TODO: Should a list acceped named arguments?
alias ConstructList = ConstructListImpl!ConstructObject;
class ConstructListImpl(T) : T
{
  const(T)[] items;
  //ConstructObject[const(char)[]] namedItems;
  this(size_t lineNumber, const(T)[] items = null)//, ConstructObject[const(char)[]] namedItems = null
  {
    super(lineNumber);
    this.items = items;
    //this.namedItems = namedItems;
  }
  enum staticTypeName = "list";
  final override string typeName() const
  {
    return "list";
  }
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
    // TODO: implement printing namedItems
    //foreach(item; items) {
    //}
    //sink(name);
    sink(")");
  }
  static if ( is( T == ConstructObject) ) {
    final override BackendObject toBackendObject() const
    {
      auto backendItems = new BackendObject[items.length];
      foreach(i; 0..items.length) {
	backendItems[i] = items[i].toBackendObject();
      }
      return new ConstructListImpl!BackendObject(lineNumber, backendItems);
    }
  }
  
  final override bool equals(const(ConstructObject) otherObj, bool checkLineNumber = true) const
  {
    auto other = cast(ConstructListImpl!T)otherObj;
    return other && equals(other, checkLineNumber);
  }
  final bool equals(const(ConstructListImpl!T) other, bool checkLineNumber = true) const
  {
    if(checkLineNumber && lineNumber != other.lineNumber) {
      return false;
    }
    if(items.length != other.items.length) {
      //writefln("[DEBUG] mismatched length");
      return false;
    }
    foreach(i; 0..items.length) {
      if(!items[i].equals(other.items[i])) {
	//writefln("[DEBUG] mismatched item %s", i);
	return false;
      }
    }
    return true;
  }
  override bool canBe(ConstructType canBeType)
  {
    throw new Exception("not implemented");
  }
}


class ConstructBlock : ConstructObject
{
  const(Construct)[] constructs;
  this(size_t lineNumber, const(Construct)[] constructs = null)
  {
    super(lineNumber);
    this.constructs = constructs;
  }
  enum primitiveTypeEnum = PrimitiveTypeEnum.constructBlock;
  enum staticTypeName = "construct-block";
  final override string typeName() const
  {
    return "construct-block";
  }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("{");
    if(constructs.length > 0) {
      constructs[0].toString(sink);
      foreach(item; constructs[1..$]) {
	sink(" ");
	item.toString(sink);
      }
    }
    sink("}");
  }
  final override BackendObject toBackendObject() const
  {
    throw new Exception("not implemented");
    /*
    auto backendItems = new BackendObject[items.length];
    foreach(i; 0..items.length) {
      backendItems[i] = items[i].toBackendObject();
    }
    return new ConstructListImpl!BackendObject(lineNumber, backendItems);
    */
  }
  
  final override bool equals(const(ConstructObject) otherObj, bool checkLineNumber = true) const
  {
    auto other = cast(ConstructBlock)otherObj;
    return other && equals(other, checkLineNumber);
  }
  final bool equals(const(ConstructBlock) other, bool checkLineNumber = true) const
  {
    if(checkLineNumber && lineNumber != other.lineNumber) {
      return false;
    }
    if(constructs.length != other.constructs.length) {
      //writefln("[DEBUG] mismatched length");
      return false;
    }
    foreach(i; 0..constructs.length) {
      if(!constructs[i].equals(other.constructs[i])) {
	//writefln("[DEBUG] mismatched item %s", i);
	return false;
      }
    }
    return true;
  }
  override bool canBe(ConstructType canBeType)
  {
    return PrimitiveTypeEnum.constructBlock.canBe(canBeType);
  }
}




			     
alias Construct = ConstructImpl!ConstructObject;
class ConstructImpl(T) : T
{
  const(char)[] name;
  const(T)[] args;
  T[const(char)[]] namedArgs;

  static if( is(T == ConstructObject) ) {
    this(size_t lineNumber, const(char)[] name,
	 const(T)[] args = null, T[const(char)[]] namedArgs = null)
    {
      super(lineNumber);
      this.name = name;
      this.args = args;;
      this.namedArgs = namedArgs;
    }
    final override BackendObject toBackendObject() const
    {
      throw new Exception("cannot call toBackendObject on a Construct");
    }
  } else static if( is(T == BackendObject) ) {
    int function(ProcessorState*, const(ConstructImpl!BackendObject)) backendFunc;
    this(size_t lineNumber, int function(ProcessorState*, const(ConstructImpl!BackendObject)) backendFunc,
	 const(char)[] name, const(BackendObject)[] args = null, BackendObject[const(char)[]] namedArgs = null)
      {
	super(lineNumber);
	this.backendFunc = backendFunc;
	this.name = name;
	this.args = args;;
	this.namedArgs = namedArgs;
      }
  }
  final override string typeName() const
  {
    return "construct";
  }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("{Construct ");
    sink(name);
    if(args.length > 0) {
      foreach(arg; args) {
	sink(" ");
	arg.toString(sink);
      }
    }
    if(namedArgs.length) {
      
      foreach(keyValue; namedArgs.byKeyValue) {
	sink(keyValue.key);
	sink("=");
	keyValue.value.toString(sink);
      }
    }
    sink("}");
  }
  final override bool equals(const(ConstructObject) otherObj, bool checkLineNumber = true) const
  {
    auto other = cast(ConstructImpl!T)otherObj;
    return other && equals(other, checkLineNumber);
  }
  final bool equals(const(ConstructImpl!T) other, bool checkLineNumber = true) const
  {
    if(checkLineNumber && lineNumber != other.lineNumber) {
      return false;
    }
    if(args.length != other.args.length ||
       namedArgs.length != other.namedArgs.length ||
       name != other.name) {
      return false;
    }
    foreach(i; 0..args.length) {
      if(!args[i].equals(other.args[i])) {
	return false;
      }
    }
    if(namedArgs.length) {
      foreach(nameObj; namedArgs.byKeyValue) {
	auto otherArg = other.namedArgs.get(nameObj.key, null);
	if(!otherArg || !nameObj.value.equals(otherArg)) {
	  //writefln("[DEBUG] named param '%s' does not match", nameObj.key);
	  return false;
	}
      }
    }
    return true;
  }
  override bool canBe(ConstructType canBeType)
  {
    return PrimitiveTypeEnum.construct.canBe(canBeType);
  }
}
