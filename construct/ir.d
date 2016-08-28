module construct.ir;

import std.format : format, formattedWrite;
import std.stdio  : File, Exception, writeln, writefln, stdout;
import std.array  : Appender, appender;
import std.conv   : to;
import std.typecons : BitFlags;
import construct.patterns : Pattern, Matcher, IMatcherVisitHandler;
import construct.processor : ConstructProcessor;

//version = VerboseTests;

////////////////////////////////////////////////////////////////////////////////
// Utility Functions
////////////////////////////////////////////////////////////////////////////////
alias LineNumber = size_t;
alias StringSink = scope void delegate(const(char)[]);
alias PureStringSink = scope void delegate(const(char)[]) pure;

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
  immutable(T) immutable_(T)(T obj)// if( !is(immutable T U == T U) )
  {
    return cast(immutable(T))obj;
  }
}
Exception imp(string feature = null, string file = __FILE__, size_t line = __LINE__) pure {
  Exception e;
  if(feature) {
    e = new Exception(feature~": not implemented", file, line);
  } else {
    e = new Exception("not implemented", file, line);
  }
  throw e;
  return e;
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
An an(T)(T enumValue) pure if( is( T == enum) )
{
  return An(to!string(enumValue));
}
struct StringBuilder(size_t MaxSize)
{
  char[MaxSize] buffer;
  size_t offset;
  void append(const(char)[] s)
  {
    assert(offset + s.length <= MaxSize);
    buffer[offset..offset+s.length] = s;
    offset += s.length;
  }
  string createString()
  {
    return buffer[0..offset].idup;
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

class ConstructException : Exception
{
  this(string msg, string file = __FILE__, size_t line = __LINE__) pure
  {
    super(msg, file, line);
  }
}

version(ChangeTemporarily) {
  T singleton(T, A...)(A args) 
  {
    static T instance;
    if(!instance) {
      instance = new T(args);
    }
    return instance;
  }
} else {
  T singleton(T, A...)(A args) pure
  {
    return new T(args);
  }
}

enum PrimitiveTypeEnum {
  anything,

  named,
  
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
  this(string name, PrimitiveTypeEnum typeEnum, PrimitiveTypeEnum parentTypeEnum, immutable(Matcher) matcher = null) pure
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
   PrimitiveTypeDefinition("named"    , PrimitiveTypeEnum.named    , PrimitiveTypeEnum.anything),
   PrimitiveTypeDefinition("void"     , PrimitiveTypeEnum.void_    , PrimitiveTypeEnum.anything),
   PrimitiveTypeDefinition("constructBreak", PrimitiveTypeEnum.constructBreak, PrimitiveTypeEnum.anything, Matcher.constructBreak),
   PrimitiveTypeDefinition("nullable" , PrimitiveTypeEnum.nullable , PrimitiveTypeEnum.anything),

   PrimitiveTypeDefinition("type"     , PrimitiveTypeEnum.type     , PrimitiveTypeEnum.anything),

   PrimitiveTypeDefinition("symbol"   , PrimitiveTypeEnum.symbol   , PrimitiveTypeEnum.anything, Matcher.symbol),
   
   PrimitiveTypeDefinition("bool"     , PrimitiveTypeEnum.bool_    , PrimitiveTypeEnum.anything, Matcher.bool_),
   
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
   PrimitiveTypeDefinition("constructBlock", PrimitiveTypeEnum.constructBlock, PrimitiveTypeEnum.nullable, Matcher.block),

   PrimitiveTypeDefinition("list"            , PrimitiveTypeEnum.list            , PrimitiveTypeEnum.anything, Matcher.list),
   PrimitiveTypeDefinition("constructPattern", PrimitiveTypeEnum.constructPattern, PrimitiveTypeEnum.list),

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
  return primitiveTypes[cast(uint)typeEnum];
}
bool canBe(PrimitiveTypeEnum typeEnum, PrimitiveTypeEnum canBeEnum) pure
{
  return typeEnum == canBeEnum ||
    (typeEnum != PrimitiveTypeEnum.anything && typeEnum.definition.parentTypeEnum.canBe(canBeEnum));
}
bool canBe(PrimitiveTypeEnum typeEnum, ConstructType canBeType) pure
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

abstract class ConstructType : ConstructObject
{
  this(size_t lineNumber) pure
  {
    super(lineNumber);
  }
  /*
  this(size_t lineNumber) immutable
  {
    super(lineNumber);
  }
  */
  enum staticTypeName = "type";
  enum primitiveTypeEnum = PrimitiveTypeEnum.type;
  @property final override PrimitiveTypeEnum primitiveType() pure const { return primitiveTypeEnum; }

  @property abstract PrimitiveTypeEnum asPrimitive() const pure;
  @property final override inout(ConstructType) asConstructType() inout { return this; }

  @property abstract immutable(Matcher) matcher() const pure;
}

PrimitiveType create(PrimitiveTypeEnum typeEnum, size_t lineNumber)
{
  return new PrimitiveType(lineNumber, typeEnum);
}
class PrimitiveType : ConstructType
{
  PrimitiveTypeEnum typeEnum;
  this(size_t lineNumber, PrimitiveTypeEnum typeEnum) pure
  {
    super(lineNumber);
    this.typeEnum = typeEnum;
  }
  /*
  this(size_t lineNumber, PrimitiveTypeEnum typeEnum) immutable
  {
    super(lineNumber);
    this.typeEnum = typeEnum;
    }*/
  final string typeName() const pure
  {
    return "primitiveType";
  }
  @property final override PrimitiveTypeEnum asPrimitive() const pure { return typeEnum; }
  override void toString(scope void delegate(const(char)[]) sink) const
  {
    //typeEnum.enumValueString;
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
    if(!typeEnum.definition.matcher) {
      throw imp(format("matcher for '%s' not configured in primitive type definition", typeName));
    }
    return typeEnum.definition.matcher;
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
  this(size_t lineNumber, const(char)[] constructName) pure {
    super(lineNumber);
    this.constructName = constructName;
  }
  enum staticTypeName = "construct";
  final string typeName() const pure
  {
    return staticTypeName;
  }
  @property final override PrimitiveTypeEnum asPrimitive() const pure { return PrimitiveTypeEnum.construct; }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("(construct ");
    sink(constructName);
    sink(")");
  }
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const pure
  {
    auto other = cast(ConstructType)otherObj;
    return other && equals(other, checkLineNumber);
  }
  final bool equals(const(ConstructReferenceType) other, bool checkLineNumber = true) const pure
  {
    return this.constructName == other.constructName;
  }
  @property override immutable(Matcher) matcher() const pure
  {
    throw imp("ConstructReferencdType.matcher");
  }
  // TODO: create an abstract method that returns the default value
}
class ConstructTypedListType : ConstructType
{
  const(ConstructType) itemType;
  this(size_t lineNumber, const(ConstructType) itemType) pure {
    super(lineNumber);
    this.itemType = itemType;
  }
  enum staticTypeName = "list";
  final string typeName() const pure
  {
    return staticTypeName;
  }
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
    throw imp("ConstructTypedList.matcher");
  }
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

interface ISymbolObject
{
  @property size_t getLineNumber() const pure;
  
  @property inout(ConstructObject) asOneConstructObject() inout pure;

  @property inout(ConstructDefinition)  asConstructDefinition() inout pure;
  @property inout(ConstructType)        asConstructType() inout pure;
  @property inout(ConstructReturn)      asConstructReturn() inout pure;
  @property inout(ObjectBreak)          asObjectBreak() inout pure;
  @property inout(ConstructString)      asConstructString() inout pure;
  @property inout(ConstructUtf8)        asConstructUtf8() inout pure;
  @property inout(ConstructSymbol)      asConstructSymbol() inout pure;
  @property inout(ConstructBool)        asConstructBool() inout pure;
  @property inout(ConstructNumber)      asConstructNumber() inout pure;
  @property inout(ConstructUint)        asConstructUint() inout pure;
  @property inout(ConstructBlock)       asConstructBlock() inout pure;
  @property inout(ConstructList)        asConstructList() inout pure;
  @property inout(ConstructPattern)     asConstructPattern() inout pure;
  @property inout(ConstructNamedObject) asNamedObject() inout pure;

  @property string typeName() const pure;

  bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const pure;

  const(ISymbolObject) member(const(char)[] memberName) const pure;
}


// TODO: need to make a tryAs, this one should throw an exception if
//       the object is not the expected type
inout(T) as(T)(inout(ISymbolObject) obj)
{
  static if( is ( T == ConstructDefinition ) ) {
    return obj.asConstructDefinition;
  } else static if( is ( T == ConstructObject ) ) {
    throw imp();
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
  } else static if( is ( T == ConstructPattern ) ) {
    return obj.asConstructPattern;
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
inout(T) as(T)(inout(ConstructObject) obj)
{
  static if( is ( T == ConstructDefinition ) ) {
    return obj.asConstructDefinition;
  } else static if( is ( T == ConstructObject ) ) {
    return obj;
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
  } else static if( is ( T == ConstructPattern ) ) {
    return obj.asConstructPattern;
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
alias ConstructHandler = const(ConstructObject) function
  (ConstructProcessor* processor, const(ConstructDefinition) definition, const(ConstructSymbol) constructSymbol, const(ConstructObject)[] args);

struct PatternHandler
{
  Pattern pattern;
  ConstructHandler handler;
}

class ConstructDefinition : ISymbolObject
{
  const size_t lineNumber;
  const(char)[] filename;
  const ConstructAttributes attributes;
  const ConstructType evalTo;

  this(size_t lineNumber, const(char)[] filename, ConstructAttributes attributes, ConstructType evalTo) pure
  {
    this.lineNumber = lineNumber;
    this.filename = filename;
    this.attributes = attributes;
    this.evalTo = evalTo;
  }
  this(size_t lineNumber, string filename, ConstructAttributes attributes, immutable ConstructType evalTo) pure immutable
  {
    this.lineNumber = 0;
    this.filename = filename;
    this.attributes = attributes;
    this.evalTo = evalTo;
  }

  enum staticTypeName = "construct";
  @property
  string typeName() const pure { return staticTypeName; }
  

  @property final size_t getLineNumber() const pure { return lineNumber; }
  @property final inout(ConstructObject) asOneConstructObject() inout pure
  {
    throw imp("ConstructDefinition asOneConstructObject");
  }
  
  final override const(ISymbolObject) member(const(char)[] memberName) const pure
  {
    return null;
  }

  @property
  final inout(ConstructDefinition) asConstructDefinition() inout pure
   {
    return this;
  }
  @property final inout(ConstructType)        asConstructType() inout pure { return null; }
  @property final inout(ConstructReturn)      asConstructReturn() inout pure { return null; }
  @property final inout(ObjectBreak)       asObjectBreak() inout pure { return null; }
  @property final inout(ConstructString)      asConstructString() inout pure { return null; }
  @property final inout(ConstructUtf8)        asConstructUtf8() inout pure { return null; }
  @property final inout(ConstructSymbol)      asConstructSymbol() inout pure { return null; }
  @property final inout(ConstructBool)        asConstructBool() inout pure { return null; }
  @property final inout(ConstructNumber)      asConstructNumber() inout pure { return null; }
  @property final inout(ConstructUint)        asConstructUint() inout pure { return null; }
  @property final inout(ConstructBlock)       asConstructBlock() inout pure { return null; }
  @property final inout(ConstructList)        asConstructList() inout pure { return null; }
  @property final inout(ConstructPattern)     asConstructPattern() inout pure { return null; }
  @property final inout(ConstructNamedObject) asNamedObject() inout pure { return null; }

  bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const pure
  {
    throw imp("ConstructDefinition equals");
  }

  abstract const(PatternHandler)[] getPatternHandlers() const;
  abstract const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol, const(ConstructObject)[] objects, size_t* index) const;

  
  //abstract const(ConstructObject) processNoPattern(ConstructProcessor* processor,
  //const(ConstructSymbol) constructSymbol, const(ConstructObject)[] objects, size_t* argIndex) const;

  //  abstract const(ConstructObject) process2(ConstructProcessor* processor,
  //      const(ConstructSymbol) constructSymbol, const(ConstructObject)[] args) const;
  const(ConstructObject) process2(ConstructProcessor* processor,
      const(ConstructSymbol) constructSymbol, const(ConstructObject)[] args) const
  {
    throw imp(format("construct '%s', process2", constructSymbol.value));
  }
}
class NoPatternConstruct : ConstructDefinition
{
  this(size_t lineNumber, string filename, ConstructAttributes attributes, immutable ConstructType evalTo) pure immutable
  {
    super(lineNumber, filename, attributes, evalTo);
  }
  final override const(PatternHandler)[] getPatternHandlers() const { return null; }
}
class PatternConstructDefinition : ConstructDefinition
{
  immutable(PatternHandler)[] patternHandlers;
  this(immutable(PatternHandler)[] patternHandlers, size_t lineNumber, string filename, ConstructAttributes attributes, immutable ConstructType evalTo) pure immutable
  {
    super(lineNumber, filename, attributes, evalTo);
    this.patternHandlers = patternHandlers;
  }
  final override const(PatternHandler)[] getPatternHandlers() const { return patternHandlers; }
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor,
      const(ConstructSymbol) constructSymbol, const(ConstructObject)[] objects, size_t* index) const
  {
    throw new Exception("cannot call processNoPattern on a PatternConstructDefinition");
  }
}
class InternalPatternConstructDefinition : PatternConstructDefinition
{
  this(immutable(PatternHandler)[] patternHandlers, ConstructAttributes attributes, immutable ConstructType evalTo, LineNumber lineNumber = __LINE__, string filename = __FILE__) pure immutable
  {
    super(patternHandlers, lineNumber, filename, attributes, evalTo);
  }
}

class TypeDefinition : ISymbolObject
{
  const size_t lineNumber;
  @property final size_t getLineNumber() const pure { return lineNumber; }

  const(char)[] name;
  const(ConstructType) type;
  
  this(size_t lineNumber, const(char)[] name, ConstructType type) pure
  {
    this.lineNumber = lineNumber;
    this.name = name;
    this.type = type;
  }

  enum staticTypeName = "deftype";
  @property
  string typeName() const pure { return staticTypeName; }

  @property final inout(ConstructObject) asOneConstructObject() inout pure { return cast(inout(ConstructObject))type; }

  final override const(ISymbolObject) member(const(char)[] memberName) const pure
  {
    return null;
  }

  @property final inout(ConstructDefinition) asConstructDefinition() inout pure { return null; }
  @property final inout(ConstructType) asConstructType() inout pure
  {
    return cast(inout(ConstructType))type;
  }
  @property final inout(ConstructReturn)      asConstructReturn() inout pure { return null; }
  @property final inout(ObjectBreak)       asObjectBreak() inout pure { return null; }
  @property final inout(ConstructString)      asConstructString() inout pure { return null; }
  @property final inout(ConstructUtf8)        asConstructUtf8() inout pure { return null; }
  @property final inout(ConstructSymbol)      asConstructSymbol() inout pure { return null; }
  @property final inout(ConstructBool)        asConstructBool() inout pure { return null; }
  @property final inout(ConstructNumber)      asConstructNumber() inout pure { return null; }
  @property final inout(ConstructUint)        asConstructUint() inout pure { return null; }
  @property final inout(ConstructBlock)       asConstructBlock() inout pure { return null; }
  @property final inout(ConstructList)        asConstructList() inout pure { return null; }
  @property final inout(ConstructPattern)     asConstructPattern() inout pure { return null; }
  @property final inout(ConstructNamedObject) asNamedObject() inout pure { return null; }

  bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const pure
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
  @property final size_t getLineNumber() const pure { return lineNumber; }
  /*
  this(size_t lineNumber) pure
  {
    this.lineNumber = lineNumber;
  }
  */
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

  override const(ISymbolObject) member(const(char)[] memberName) const pure
  {
    return null;
  }

  // True if the object returns an immediate value.
  // Used for the processOneConstruct method.
  //@property abstract bool isOneImmediateValue() const;
  
  @property final inout(ConstructObject) asOneConstructObject() inout pure { return this; }

  @property inout(ConstructDefinition)  asConstructDefinition() inout pure { return null; }
  @property inout(ConstructType)        asConstructType()       inout pure { return null; }
  @property inout(ConstructReturn)      asConstructReturn()     inout pure { return null; }
  @property inout(ObjectBreak)          asObjectBreak()         inout pure { return null; }
  @property inout(ConstructString)      asConstructString()     inout pure { return null; }
  @property inout(ConstructUtf8)        asConstructUtf8()       inout pure { return null; }
  @property inout(ConstructSymbol)      asConstructSymbol()     inout pure { return null; }
  @property inout(ConstructBool)        asConstructBool()       inout pure { return null; }
  @property inout(ConstructNumber)      asConstructNumber()     inout pure { return null; }
  @property inout(ConstructUint)        asConstructUint()       inout pure { return null; }
  @property inout(ConstructBlock)       asConstructBlock()      inout pure { return null; }
  @property inout(ConstructList)        asConstructList()       inout pure { return null; }
  @property inout(ConstructPattern)     asConstructPattern()    inout pure { return null; }
  @property inout(ConstructNamedObject) asNamedObject()         inout pure { return null; }

  const(ConstructObject) typedAs(const(ConstructType) type) const pure
  {
    throw imp(format("typedAs for %s", typeid(this)));
  }

  /*
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
  */
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
  @property final override inout(ConstructReturn) asConstructReturn() inout pure { return this; }
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

class ConstructPattern : ConstructObject, Matcher
{
  const(Pattern) pattern;
  this(size_t lineNumber, const(Pattern) pattern) pure
  {
    super(lineNumber);
    this.pattern = pattern;
  }
  this(size_t lineNumber, const(Pattern) pattern) pure immutable
  {
    super(lineNumber);
    this.pattern = cast(immutable(Pattern))pattern;
  }
  final string typeName() const pure { return "pattern"; }
  @property final override PrimitiveTypeEnum primitiveType() pure const { return PrimitiveTypeEnum.constructPattern; }
  override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("<pattern.toString not implemented>");
  }
  @property final override inout(ConstructPattern) asConstructPattern() inout pure { return this; }
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
  @property override bool matchesAnySymbolNext() const pure
  {
    return pattern.nodes.length > 0 && pattern.nodes[0].matcher.matchesAnySymbolNext;
  }
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
class ConstructVoid : ConstructObject
{
  this(size_t lineNumber)
  {
    super(lineNumber);
  }
  final string typeName() const pure { return "void"; }
  @property final override PrimitiveTypeEnum primitiveType() pure const { return PrimitiveTypeEnum.void_; }
  override void toString(scope void delegate(const(char)[]) pure @nogc sink) const
  {
    sink("void");
  }
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const pure
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
  @property final override PrimitiveTypeEnum primitiveType() pure const { return primitiveTypeEnum; }
  
  //@property final override inout(ConstructPointer) asConstructPointer() inout pure { return this; }
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
  
  @property final override inout(ConstructBool) asConstructBool() inout pure { return this; }
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


class ConstructNumber : ConstructObject
{
  this(size_t lineNumber) pure
  {
    super(lineNumber);
  }
  @property final override inout(ConstructNumber) asConstructNumber() inout pure { return this; }

  abstract const(ConstructNumber) add(const(ConstructNumber) other) const pure;
}

class ConstructUint : ConstructNumber
{
  size_t value;
  this(size_t lineNumber, size_t value) pure
  {
    super(lineNumber);
    this.value = value;
  }
  enum primitiveTypeEnum = PrimitiveTypeEnum.uint_;
  enum staticTypeName = "uint";
  final string typeName() const pure
  {
    return "uint";
  }
  @property final override PrimitiveTypeEnum primitiveType() pure const { return PrimitiveTypeEnum.uint_; }
  @property final override inout(ConstructUint) asConstructUint() inout pure { return this; }

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
    if(auto otherUint = other.asConstructUint) {
      return new ConstructUint(0, value + otherUint.value);
    }
    throw imp(format("add %s to %s", typeName, other.typeName));
  }
}


// A construct string is explicitly a string.  It cannot be a symbol.
class ConstructString : ConstructObject
{
  this(size_t lineNumber) pure
  {
    super(lineNumber);
  }
  enum primitiveTypeEnum = PrimitiveTypeEnum.string;
  enum staticTypeName = "string";
  @property final override inout(ConstructString) asConstructString() inout pure { return this; }
  enum processorValueType = "ConstructString";
  enum processorOptionalValueType = "ConstructString";

  abstract const(char)[] toUtf8() const pure;
  abstract size_t stringByteLength(PrimitiveTypeEnum type) pure;
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
  
  @property final override inout(ConstructUtf8) asConstructUtf8() inout pure { return this; }
  
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
  final override const(ISymbolObject) member(const(char)[] memberName) const pure
  {
    if(memberName == "length") {
      return new ConstructUint(0, value.length);
    }
    return null;
  }
  override const(char)[] toUtf8() const pure
  {
    return value;
  }
  override size_t stringByteLength(PrimitiveTypeEnum type) pure
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
  override string typeName() const pure
  {
    return "symbolOrString";
  }
  override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink(value);
  }
  override bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const pure
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
  this(size_t lineNumber, const(char)[] value) pure
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
  @property final override inout(ConstructSymbol) asConstructSymbol() inout pure { return this; }
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
  @property final string typeName() const pure { return staticTypeName; }

  @property final override PrimitiveTypeEnum primitiveType() pure const { return PrimitiveTypeEnum.symbolRef; }
  @property final override inout(ConstructSymbolRef) asConstructSymbolRef() inout pure { return this; }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("ref ");
    sink(symbol.value);
  }
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const pure
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
  this(size_t lineNumber, const(char)[] value) pure
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
  @property final override inout(ObjectBreak) asObjectBreak() inout pure { return this; }
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
  @property final override inout(ConstructList) asConstructList() inout pure { return this; }
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
  @property final override inout(ConstructBlock) asConstructBlock() inout pure { return this; }
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

class ConstructNamedObject : ConstructObject
{
  const(char)[] name;
  const(ConstructObject) object;
  this(size_t lineNumber, const(char)[] name, const(ConstructObject) object) pure
  {
    super(lineNumber);
    this.name = name;
    this.object = object;
  }
  //enum primitiveTypeEnum = PrimitiveTypeEnum.constructBlock;
  enum staticTypeName = "named-object";
  final string typeName() const pure
  {
    return "named-object";
  }
  @property final override PrimitiveTypeEnum primitiveType() pure const { return PrimitiveTypeEnum.named; }
  @property final override inout(ConstructNamedObject) asNamedObject() inout pure { return this; }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink(name);
    sink("=");
    object.toString(sink);
  }
  
  final bool equals(const(ISymbolObject) otherObj, bool checkLineNumber = true) const pure
  {
    auto other = cast(ConstructNamedObject)otherObj;
    return other && equals(other, checkLineNumber);
  }
  final bool equals(const(ConstructNamedObject) other, bool checkLineNumber = true) const pure
  {
    if(checkLineNumber && lineNumber != other.lineNumber) {
      return false;
    }
    return name == other.name && object.equals(other.object);
  }
}
