module construct.ir;

import std.stdio  : File, Exception, writeln, writefln, stdout;
import std.array  : Appender, appender;
import std.string : format;
import std.format : formattedWrite;
import std.conv   : to;

import construct.backendIR : BackendConstruct;

enum PrimitiveTypeEnum {
  anything,
  
  type,

  symbol, // Used for construct names (think of more)

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
   PrimitiveTypeDefinition("type"     , PrimitiveTypeEnum.type     , PrimitiveTypeEnum.anything),
   PrimitiveTypeDefinition("type"     , PrimitiveTypeEnum.symbol   , PrimitiveTypeEnum.anything),
   PrimitiveTypeDefinition("pointer"  , PrimitiveTypeEnum.pointer  , PrimitiveTypeEnum.anything),
   PrimitiveTypeDefinition("number"   , PrimitiveTypeEnum.number   , PrimitiveTypeEnum.anything),
   PrimitiveTypeDefinition("integer"  , PrimitiveTypeEnum.integer_ , PrimitiveTypeEnum.number),
   PrimitiveTypeDefinition("signed"   , PrimitiveTypeEnum.signed   , PrimitiveTypeEnum.integer_),
   PrimitiveTypeDefinition("int"      , PrimitiveTypeEnum.int_     , PrimitiveTypeEnum.signed),
   PrimitiveTypeDefinition("byte"     , PrimitiveTypeEnum.byte_    , PrimitiveTypeEnum.signed),
   PrimitiveTypeDefinition("unsigned" , PrimitiveTypeEnum.unsigned_, PrimitiveTypeEnum.integer_),
   PrimitiveTypeDefinition("uint"     , PrimitiveTypeEnum.uint_    , PrimitiveTypeEnum.unsigned_),
   PrimitiveTypeDefinition("ubyte"    , PrimitiveTypeEnum.ubyte_   , PrimitiveTypeEnum.unsigned_),
   PrimitiveTypeDefinition("uni"      , PrimitiveTypeEnum.uni      , PrimitiveTypeEnum.anything),

   PrimitiveTypeDefinition("array"    , PrimitiveTypeEnum.array    , PrimitiveTypeEnum.anything),
   PrimitiveTypeDefinition("string"   , PrimitiveTypeEnum.string   , PrimitiveTypeEnum.array),
   PrimitiveTypeDefinition("ascii"    , PrimitiveTypeEnum.ascii    , PrimitiveTypeEnum.string),
   PrimitiveTypeDefinition("unicode"  , PrimitiveTypeEnum.unicode  , PrimitiveTypeEnum.string),
   PrimitiveTypeDefinition("utf8"     , PrimitiveTypeEnum.utf8     , PrimitiveTypeEnum.utf8),

   PrimitiveTypeDefinition("lengthArray"  , PrimitiveTypeEnum.lengthArray    , PrimitiveTypeEnum.array),
   PrimitiveTypeDefinition("lengthAscii"  , PrimitiveTypeEnum.lengthAscii    , PrimitiveTypeEnum.lengthArray),
   PrimitiveTypeDefinition("lengthUnicode", PrimitiveTypeEnum.lengthUnicode  , PrimitiveTypeEnum.lengthArray),
   PrimitiveTypeDefinition("lengthUtf8"   , PrimitiveTypeEnum.lengthUtf8     , PrimitiveTypeEnum.lengthUnicode),

   PrimitiveTypeDefinition("limitArray"  , PrimitiveTypeEnum.limitArray    , PrimitiveTypeEnum.array),
   PrimitiveTypeDefinition("limitAscii"  , PrimitiveTypeEnum.limitAscii    , PrimitiveTypeEnum.limitArray),
   PrimitiveTypeDefinition("limitUnicode", PrimitiveTypeEnum.limitUnicode  , PrimitiveTypeEnum.limitArray),
   PrimitiveTypeDefinition("limitUtf8"   , PrimitiveTypeEnum.limitUtf8     , PrimitiveTypeEnum.limitUnicode),
   ];
  

abstract class ConstructObject
{
  size_t lineNumber;
  this(size_t lineNumber)
  {
    this.lineNumber = lineNumber;
  }
  abstract void toString(scope void delegate(const(char)[]) sink) const;
  abstract bool equals(const(ConstructObject) otherObj, bool checkLineNumber = true) const;

  abstract BackendObject toBackendObject() const;
  
  @property
  final bool isListBreak() const
  {
    return !(cast(ListBreak)this is null);
  }

  static ConstructObject fromUnquoted(size_t lineNumber, const(char)[] unquoted)
  {
    foreach(primitiveType; primitiveTypes) {
      if(unquoted == primitiveType.name) {
	return new PrimitiveType(lineNumber, primitiveType.typeEnum);
      }
    }
    return new ConstructString(lineNumber, unquoted);
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


abstract class Type : BackendObject
{
  this(size_t lineNumber)
  {
    super(lineNumber);
  }
}

PrimitiveType create(PrimitiveTypeEnum typeEnum, size_t lineNumber)
{
  return new PrimitiveType(lineNumber, typeEnum);
}
class PrimitiveType : Type {
  PrimitiveTypeEnum typeEnum;
  this(size_t lineNumber, PrimitiveTypeEnum typeEnum)
  {
    super(lineNumber);
    this.typeEnum = typeEnum;
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
}
class ConstructType : Type {
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
  final bool equals(const(ConstructType) other, bool checkLineNumber = true) const
  {
    return this.constructName == other.constructName;
  }
}

class ConstructString : BackendObject
{
  const(char)[] value;
  alias value this;
  this(size_t lineNumber, const(char)[] value)
  {
    super(lineNumber);
    this.value = value;
  }
  override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("\"");
    sink(value);
    sink("\"");
  }
  override bool equals(const(ConstructObject) otherObj, bool checkLineNumber = true) const
  {
    if(checkLineNumber && lineNumber != otherObj.lineNumber) {
      return false;
    }
    auto other = cast(ConstructString)otherObj;
    return other && this.value == other.value;
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
    int function(ProcessingData*, const(BackendConstruct)) backendFunc;
    this(size_t lineNumber, int function(ProcessingData*, const(BackendConstruct)) backendFunc,
	 const(char)[] name, const(BackendObject)[] args = null, BackendObject[const(char)[]] namedArgs = null)
      {
	super(lineNumber);
	this.backendFunc = backendFunc;
	this.name = name;
	this.args = args;;
	this.namedArgs = namedArgs;
      }
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
}

struct BackendConstructBuilder
{
  Appender!(BackendObject[]) args;
  BackendObject[const(char)[]] namedArgs;
}

struct ConstructParam
{
  const(char)[] name;
  const(Type) type;
}
struct ConstructOptionalParam
{
  const(char)[] name;
  const(Type) type;
  const(ConstructObject) defaultValue;
}
struct ConstructDefinition
{
  const(char)[] name;
  const(ConstructParam)[] requiredParams = void;
  const(ConstructOptionalParam)[] optionalParams = void;
  
  int function(ConstructDefinition*, const(Construct), Appender!(BackendConstruct[]) ) processor;
  int function(ConstructDefinition*, const(Construct), const(Construct), ref BackendConstructBuilder) childProcessor;
  int function(ProcessingData*, const(BackendConstruct)) backendFunc;

  this(const(char)[] name, const(ConstructParam)[] requiredParams,
       const(ConstructOptionalParam)[] optionalParams,
       int function(ConstructDefinition*,const(Construct), Appender!(BackendConstruct[]) ) processor,
       int function(ConstructDefinition*, const(Construct), const(Construct), ref BackendConstructBuilder) childProcessor)
  {
    this.name = name;
    this.requiredParams = requiredParams;
    this.optionalParams = optionalParams;
    this.processor = processor;
    this.childProcessor = childProcessor;
  }
  this(string name, int function(ConstructDefinition*, const(Construct), Appender!(BackendConstruct[])) processor,
       int function(ConstructDefinition*, const(Construct), const(Construct), ref BackendConstructBuilder) childProcessor) immutable
  {
    this.name = name;
    this.requiredParams = null;
    this.optionalParams = null;
    this.processor = processor;
    this.childProcessor = childProcessor;
  }
  
}

struct ProcessingData {
  ConstructDefinition[const(char)[]] constructMap;

  int executeBackend(const(BackendConstruct)[] constructs)
  {
    stdout.flush();
    foreach(construct; constructs) {
      int error = executeBackend(construct);
      if(error) {
	return error;
      }
    }
    return 0;
  }
  int executeBackend(const(BackendConstruct) construct)
  {
    writefln("[BACKEND] processing construct '%s'...", construct.name);
    //stdout.flush();
    
    // TODO: I'm not sure I should keep looking up constructs
    //       using the name
    auto definition = constructMap.get(construct.name, ConstructDefinition());
    if(definition.name == null) {
      writefln("Error: backend found unknown construct '%s'", construct.name);
      return 1;
    }
    if(!definition.backendFunc) {
      writefln("Error: the '%s' construct is not meant to be executed by the backend, it should have been removed by the processor but wasn't for some reason.", construct.name);
      return 1;
    }
    return definition.backendFunc(&this, construct);
  }
}
