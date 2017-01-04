module construct.backendCore;

import std.format : format, formattedWrite;
import std.stdio  : File, Exception, writeln, writefln, stdout;
import std.array  : Appender, appender;
import std.conv   : to;
import std.typecons : BitFlags;

import construct.util;
import construct.logging;
import construct.parserCore;
import construct.patterns   : PatternNode, PatternTree, Pattern;
import construct.processor  : ConstructProcessor, Scope;
import construct.primitives : DefaultStatementModeConstruct;

//version = VerboseTests;

enum PrimitiveTypeEnum {
  anything,

  predicate,
  nullable,
  optionalValue,
  comment,

  constructBreak,

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
  string_,
  ascii,
  unicode,
  utf8,
  /*
  lengthArray,
  lengthAscii,
  lengthUnicode,
  lengthUtf8,

  limitArray,
  limitAscii,
  limitUnicode,
  limitUtf8,
  */

  statementMode,
}

struct PrimitiveTypeDefinition
{
  // The name should also be the symbol that is mapped to the type definition
  // in the symbol table
  string name;

  PrimitiveTypeEnum typeEnum;
  PrimitiveTypeEnum parentTypeEnum;
  immutable(PrimitiveType) typeObject;
  string internalClassName;

  this(string name, PrimitiveTypeEnum typeEnum, PrimitiveTypeEnum parentTypeEnum,
       immutable(PrimitiveType) typeObject = null, string internalClassName = null) pure
  {
    this.name = name;
    this.typeEnum = typeEnum;
    this.parentTypeEnum = parentTypeEnum;
    this.typeObject = typeObject;
    this.internalClassName = internalClassName;
  }
}
immutable PrimitiveTypeDefinition[] primitiveTypes =
  [
   // NOTE: the anything type must have itself as it's parent
   PrimitiveTypeDefinition("anything" , PrimitiveTypeEnum.anything , PrimitiveTypeEnum.anything, PrimitiveType.anything, "ConstructObject"),
   PrimitiveTypeDefinition("predicate", PrimitiveTypeEnum.predicate, PrimitiveTypeEnum.anything, PrimitiveType.predicate, "ConstructPredicate"),
   PrimitiveTypeDefinition("nullable" , PrimitiveTypeEnum.nullable , PrimitiveTypeEnum.predicate, PrimitiveType.nullable, "ConstructNullable"),
   PrimitiveTypeDefinition("optionalValue", PrimitiveTypeEnum.optionalValue, PrimitiveTypeEnum.anything, PrimitiveType.optionalValue, "ConstructOptionalValue"),
   //PrimitiveTypeDefinition("void"     , PrimitiveTypeEnum.void_    , PrimitiveTypeEnum.anything),
   PrimitiveTypeDefinition("comment"  , PrimitiveTypeEnum.comment  , PrimitiveTypeEnum.anything),

   PrimitiveTypeDefinition("constructBreak", PrimitiveTypeEnum.constructBreak, PrimitiveTypeEnum.anything, PrimitiveType.constructBreak, "ObjectBreak"),

   PrimitiveTypeDefinition("type"     , PrimitiveTypeEnum.type     , PrimitiveTypeEnum.anything, PrimitiveType.type    , "ConstructType"),

   PrimitiveTypeDefinition("symbol"   , PrimitiveTypeEnum.symbol   , PrimitiveTypeEnum.anything, PrimitiveType.symbol  , "ConstructSymbol"),

   PrimitiveTypeDefinition("bool"     , PrimitiveTypeEnum.bool_    , PrimitiveTypeEnum.predicate, PrimitiveType.bool_   , "ConstructBool"),

   PrimitiveTypeDefinition("pointer"  , PrimitiveTypeEnum.pointer  , PrimitiveTypeEnum.nullable, PrimitiveType.pointer , "ConstructPointer"),
   PrimitiveTypeDefinition("number"   , PrimitiveTypeEnum.number   , PrimitiveTypeEnum.anything, PrimitiveType.number  , "ConstructNumber"),
   PrimitiveTypeDefinition("integer"  , PrimitiveTypeEnum.integer_ , PrimitiveTypeEnum.number  ),
   PrimitiveTypeDefinition("signed"   , PrimitiveTypeEnum.signed   , PrimitiveTypeEnum.integer_),
   PrimitiveTypeDefinition("int"      , PrimitiveTypeEnum.int_     , PrimitiveTypeEnum.signed),
   PrimitiveTypeDefinition("byte"     , PrimitiveTypeEnum.byte_    , PrimitiveTypeEnum.signed),
   PrimitiveTypeDefinition("unsigned" , PrimitiveTypeEnum.unsigned_, PrimitiveTypeEnum.integer_),
   PrimitiveTypeDefinition("uint"     , PrimitiveTypeEnum.uint_    , PrimitiveTypeEnum.unsigned_, PrimitiveType.uint_  , "ConstructUint"),
   PrimitiveTypeDefinition("ubyte"    , PrimitiveTypeEnum.ubyte_   , PrimitiveTypeEnum.unsigned_),
   PrimitiveTypeDefinition("uni"      , PrimitiveTypeEnum.uni      , PrimitiveTypeEnum.anything),

   PrimitiveTypeDefinition("construct"     , PrimitiveTypeEnum.construct     , PrimitiveTypeEnum.anything, null, "ConstructDefinition"),
   PrimitiveTypeDefinition("constructBlock", PrimitiveTypeEnum.constructBlock, PrimitiveTypeEnum.anything, PrimitiveType.constructBlock, "ConstructBlock"),

   PrimitiveTypeDefinition("list"            , PrimitiveTypeEnum.list        , PrimitiveTypeEnum.anything, PrimitiveType.list, "ConstructList"),
   // A constructPattern is a type and a list
   PrimitiveTypeDefinition("constructPattern", PrimitiveTypeEnum.constructPattern, PrimitiveTypeEnum.type, null, "ConstructPattern"),
   PrimitiveTypeDefinition("class"           , PrimitiveTypeEnum.class_          , PrimitiveTypeEnum.nullable, PrimitiveType.class_, "ConstructClass"),

   // An array is a contiguous sequence of elements.
   // An array is not the same as a pointer.
   // A pointer can "point" to an array, but that would be "pointer to an array", not an "array" itself.
   //
   // Note: For now, an array will always carry with it, it's length. There may be a case to also have
   //       an array type that does not know it's length. There may also be a 3rd type of array, which
   //       is an array that has no length but has a terminating element (a special element that marks
   //       the end of the array).  Then there could be a 4th type that both knows it's length and has
   //       a terminating element.
   //
   //         1. array
   //         2. arrayWithLength
   //         3. arrayWithTerminatingElement
   //         4. arrayWithLengthAndTerminatingElement
   //
   //       If it becomes clear that these cases should have their own types, I may need to reorganize the
   //       types.
   //
   PrimitiveTypeDefinition("array"    , PrimitiveTypeEnum.array    , PrimitiveTypeEnum.anything),
   // A string is defined as an array of characters.
   // Note: see ConstructArray for the definition of an "array".
   // The bit-width of each element in the string array is unsepcified.
   // It may be 8 bits, or 16, or 32, or any other width.
   //
   // Example child types:
   //   A ConstructUtf8 object is a ConstructString where the element size is
   //   8 bits and the characters are encoded as UTF8.
   //
   PrimitiveTypeDefinition("string"   , PrimitiveTypeEnum.string_  , PrimitiveTypeEnum.array, PrimitiveType.string_, "ConstructString"),
   PrimitiveTypeDefinition("ascii"    , PrimitiveTypeEnum.ascii    , PrimitiveTypeEnum.string_),
   PrimitiveTypeDefinition("unicode"  , PrimitiveTypeEnum.unicode  , PrimitiveTypeEnum.string_),
   PrimitiveTypeDefinition("utf8"     , PrimitiveTypeEnum.utf8     , PrimitiveTypeEnum.unicode, PrimitiveType.utf8, "ConstructUtf8"),
   /*
   PrimitiveTypeDefinition("lengthArray"  , PrimitiveTypeEnum.lengthArray    , PrimitiveTypeEnum.array),
   PrimitiveTypeDefinition("lengthAscii"  , PrimitiveTypeEnum.lengthAscii    , PrimitiveTypeEnum.lengthArray),
   PrimitiveTypeDefinition("lengthUnicode", PrimitiveTypeEnum.lengthUnicode  , PrimitiveTypeEnum.lengthArray),
   PrimitiveTypeDefinition("lengthUtf8"   , PrimitiveTypeEnum.lengthUtf8     , PrimitiveTypeEnum.lengthUnicode),

   PrimitiveTypeDefinition("limitArray"  , PrimitiveTypeEnum.limitArray    , PrimitiveTypeEnum.array),
   PrimitiveTypeDefinition("limitAscii"  , PrimitiveTypeEnum.limitAscii    , PrimitiveTypeEnum.limitArray),
   PrimitiveTypeDefinition("limitUnicode", PrimitiveTypeEnum.limitUnicode  , PrimitiveTypeEnum.limitArray),
   PrimitiveTypeDefinition("limitUtf8"   , PrimitiveTypeEnum.limitUtf8     , PrimitiveTypeEnum.limitUnicode),
   */
   PrimitiveTypeDefinition("statementMode", PrimitiveTypeEnum.statementMode, PrimitiveTypeEnum.anything, PrimitiveType.statementMode, "ConstructStatementMode"),
   ];

PrimitiveTypeDefinition definition(PrimitiveTypeEnum typeEnum) pure nothrow @nogc @safe
{
  return primitiveTypes[cast(size_t)typeEnum];
}
immutable(PrimitiveTypeDefinition)* definitionRef(PrimitiveTypeEnum typeEnum) pure nothrow @nogc @safe
{
  return &primitiveTypes[cast(size_t)typeEnum];
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

inout(T) tryAs(T)(inout(ConstructObject) obj)
{
  mixin(formattedString!(generateItemCode!(", atFirst ? \"\" : \"else \", item, item", const(string)[]))
        (ConstructClasses, "%sstatic if( is ( T == %s ) ) { return obj.tryAs%s; }\n")~
        q{else static assert(0, "the as(ConstructObject) template does not support type: "~T.stringof);});
}
inout(T) enforceAs(T)(inout(ConstructObject) obj)
{
  auto value = tryAs!T(obj);
  if(!value) {
    throw new Exception(format("expected %s but got %s", An(T.staticTypeName), An(obj.typeName)));
  }
  return value;
}

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
interface IConstructContext
{
  bool hasOperatorPatterns() const;
  string getConstructName() const;
}
class NoConstructContext : IConstructContext
{
  static immutable instance = new immutable NoConstructContext();
  private this() immutable pure nothrow @nogc @safe {}
  bool hasOperatorPatterns() const { return false; }
  string getConstructName() const
  {
    return null;
  }
}

struct ConstructResult
{
  ConstructObject object;
  enum Action {
    none, return_
  }
  Action action;
  const(ConstructResult) withNoReturn() const
  {
    if(action == Action.return_) {
      return const ConstructResult(object);
    }
    return this;
  }
  @property uint lineNumber() const pure nothrow @nogc @safe
  {
    return object ? object.lineNumber : 0;
  }
  @property bool noAction() const pure nothrow @nogc @safe { return this.action == Action.none; }
  @property bool isReturn() const pure nothrow @nogc @safe { return this.action == Action.return_; }
  @property bool hasAction() const pure nothrow @nogc @safe { return this.action != Action.none; }
}

union ObjectOrSize
{
  const(ConstructObject) obj;
  size_t size;
  const(ConstructResult.Action) action;
  this(const(ConstructObject) obj)
  {
    this.obj = obj;
  }
  this(size_t size)
  {
    this.size = size;
  }
  this(const(ConstructResult.Action) action)
  {
    this.action = action;
  }
}
// The ConstructHandler uses a format for it's ConstructObject[] args array based
// on the pattern nodes. If a pattern node count type is:
//   case "one"      : the matching ConstructObject should NEVER be null
//   case "optional" : the matching ConstructObject will be NULL if the value was not present, and non-null if it was present
//   case "many" and "oneOrMore": the number of values, followed by those values
alias ConstructHandler = const(ConstructResult) function
  (ConstructProcessor* processor, const(ConstructDefinition) definition, const(ConstructSymbol) constructSymbol,
   Object handlerObject, const(PatternNode)[] patternNodes, const(ObjectOrSize)[] args);

struct PatternHandler
{
  ConstructHandler handler;
  Object handlerObject;
  PatternNode[] patternNodes;
}

struct OpPatternHandlers
{
  ConstructType opType;
  PatternHandler[] patternHandlers;
}
struct OpPatternTree
{
  ConstructType opType;
  PatternTree!ConstructHandler patternTree;
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
class ConstructDefinition : ConstructObject, IConstructContext
{
  string name;
  const(char)[] filename;
  const ConstructAttributes attributes;
  const ConstructType evalTo;

  this(string name, size_t lineNumber, const(char)[] filename, ConstructAttributes attributes, ConstructType evalTo) pure
  {
    super(lineNumber);
    this.name = name;
    this.filename = filename;
    this.attributes = attributes;
    this.evalTo = evalTo;
  }
  this(string name, size_t lineNumber, string filename, ConstructAttributes attributes, immutable ConstructType evalTo) pure immutable
  {
    super(lineNumber);
    this.name = name;
    this.filename = filename;
    this.attributes = attributes;
    this.evalTo = evalTo;
  }

  mixin finalTypeNameMembers!"construct";
  mixin finalPrimitiveTypeMembers!(PrimitiveTypeEnum.construct);

  @property abstract bool hasOperatorPatterns() const;
  string getConstructName() const
  {
    return name;
  }

  @property abstract inout(NoPatternConstruct) tryAsNoPatternConstruct() inout pure nothrow @nogc @safe;

  @property
  final override inout(ConstructDefinition) tryAsConstructDefinition() inout pure
  {
    return this;
  }
  mixin virtualEqualsMember!ConstructDefinition;
  final bool typedEquals(const(ConstructDefinition) other) const pure
  {
    throw imp("ConstructDefinition equals");
  }
  abstract const(PatternHandler)[] getPatternHandlers(const(ConstructObject) op) const;

  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("[construct-definition, toString not implemented]");
    //throw imp("ConstructDefinition.toString");
  }
}
class NoPatternConstruct : ConstructDefinition
{
  this(string name, size_t lineNumber, string filename, ConstructAttributes attributes, immutable ConstructType evalTo) pure immutable
  {
    super(name, lineNumber, filename, attributes, evalTo);
  }
  @property final override inout(NoPatternConstruct) tryAsNoPatternConstruct() inout pure nothrow @nogc @safe { return this; }
  @property final override bool hasOperatorPatterns() const { return false; }
  final override const(PatternHandler)[] getPatternHandlers(const(ConstructObject) op) const { return null; }
  abstract const(ConstructResult) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
						   const(ConstructObject) opParam, const(ConstructObject)[] objects, size_t* index) const;
}

enum TypeMatch {
  not, exact, subtype,
}

class PatternConstructDefinition : ConstructDefinition
{
  immutable(PatternHandler)[] noOpPatternHandlers;
  immutable(OpPatternHandlers)[] opPatternHandlers;
  this(string name, immutable(PatternHandler)[] noOpPatternHandlers,
       immutable(OpPatternHandlers)[] opPatternHandlers, size_t lineNumber,
       string filename, ConstructAttributes attributes, immutable ConstructType evalTo) immutable pure
  {
    super(name, lineNumber, filename, attributes, evalTo);
    this.noOpPatternHandlers = noOpPatternHandlers;
    this.opPatternHandlers = opPatternHandlers;
  }
  @property final override inout(NoPatternConstruct) tryAsNoPatternConstruct() inout pure nothrow @nogc @safe { return null; }
  @property final override bool hasOperatorPatterns() const { return opPatternHandlers !is null; }
  final override const(PatternHandler)[] getPatternHandlers(const(ConstructObject) op) const
  {
    if(!op) {
      return noOpPatternHandlers;
    }
    if(opPatternHandlers.length == 0) {
      return null;
    }

    //
    // Find the best match for the operator object
    //
    OpPatternHandlers bestMatch = OpPatternHandlers(null, null);
    foreach(opPatternHandler; opPatternHandlers) {
      final switch(opPatternHandler.opType.matchValue(op)) {
      case TypeMatch.not:
        break;
      case TypeMatch.exact:
        return opPatternHandler.patternHandlers;
      case TypeMatch.subtype:
        if(bestMatch.opType !is null) {
          throw imp("the operator object is a sub type for multiple patterns, need to implement code to select the best match");
        }
        bestMatch = opPatternHandler.unconst;
        break;
      }
    }
    return bestMatch.patternHandlers;
  }
  final immutable(PatternConstructDefinition) combine(immutable(PatternHandler) newHandler)
  {
    return new immutable PatternConstructDefinition(name, noOpPatternHandlers~newHandler, opPatternHandlers,
                                                    lineNumber, cast(string)filename, attributes, cast(immutable(ConstructType))evalTo);
  }
}
class InternalPatternConstructDefinition : PatternConstructDefinition
{
  this(string name, immutable(PatternHandler)[] noOpPatternHandlers, immutable(OpPatternHandlers)[] opPatternHandlers,
       ConstructAttributes attributes, immutable ConstructType evalTo, LineNumber lineNumber = __LINE__, string filename = __FILE__) pure immutable
  {
    super(name, noOpPatternHandlers, opPatternHandlers, lineNumber, filename, attributes, evalTo);
  }
}

abstract class ConstructType : ConstructObject
{
  this(size_t lineNumber) pure
  {
    super(lineNumber);
  }

  mixin virtualTypeNameMembers!"type";
  mixin virtualPrimitiveTypeMembers!(PrimitiveTypeEnum.type);

  @property abstract string tryAsKeyword() const pure;

  // Write the construct code that creates this type inside a pattern
  abstract void writePattern(PureStringSink sink) const;
  abstract void writeInternalFactoryCode(PureStringSink sink) const;

  // TODO: now that I've added 'optional' as a type, I should
  //       be able to make these into one function
  //@property abstract string internalValueClassIfRequired() const pure;
  //@property abstract string internalValueClassIfOptional() const pure;
  @property abstract string internalValueClass() const pure;

  @property abstract PrimitiveTypeEnum asPrimitive() const pure;
  @property final override inout(ConstructType) tryAsConstructType() inout { return this; }

  abstract bool supportsValue(const(ConstructObject) obj) const pure;
  abstract TypeMatch matchValue(const(ConstructObject) obj) const;
}
class PrimitiveType : ConstructType
{
  PrimitiveTypeEnum typeEnum;
  private this(size_t lineNumber, PrimitiveTypeEnum typeEnum) pure
  {
    super(lineNumber);
    this.typeEnum = typeEnum;
  }

  @property final override string tryAsKeyword() const pure { return null; }
  final override void writePattern(PureStringSink sink) const
  {
    // Every primitive type should be in the symbol table
    // with their name mapped to it
    sink(typeEnum.definition.name);
  }
  final override void writeInternalFactoryCode(PureStringSink sink) const
  {
    sink("PrimitiveType.");
    sink(typeEnum.to!string);
  }
  /*
  @property final override string internalValueClassIfRequired() const pure
  {
    auto name = typeEnum.definition().internalClassName;
    if(!name) {
      throw imp(format("type enum '%s' has no internalClassName configured", typeEnum));
    }
    return name;
  }
  @property final override string internalValueClassIfOptional() const pure
  {
    auto name = typeEnum.definition().internalClassName;
    if(!name) {
      throw imp(format("type enum '%s' has no internalClassName configured", typeEnum));
    }
    return name;
  }
  */
  @property final override string internalValueClass() const pure
  {
    auto name = typeEnum.definition().internalClassName;
    if(!name) {
      throw imp(format("type enum '%s' has no internalClassName configured", typeEnum));
    }
    return name;
  }

  @property final override PrimitiveTypeEnum asPrimitive() const pure { return typeEnum; }

  final override bool supportsValue(const(ConstructObject) obj) const pure
  {
    return obj.primitiveTypeEnum.canBe(typeEnum);
  }
  final override TypeMatch matchValue(const(ConstructObject) obj) const
  {
    if(obj.primitiveTypeEnum == typeEnum) {
      return TypeMatch.exact;
    }
    if(obj.primitiveTypeEnum.canBe(typeEnum)) {
      return TypeMatch.subtype;
    }
    return TypeMatch.not;
  }
  override void toString(scope void delegate(const(char)[]) sink) const
  {
    typeEnum.to!string;
  }
  mixin virtualEqualsMember!PrimitiveType;
  final bool typedEquals(const(PrimitiveType) other) const pure
  {
    return other && this.typeEnum == other.typeEnum;
  }

  static immutable predicate      = new PrimitiveType(0, PrimitiveTypeEnum.predicate);
  static immutable bool_          = new PrimitiveType(0, PrimitiveTypeEnum.bool_);
  static immutable number         = new PrimitiveType(0, PrimitiveTypeEnum.number);
  static immutable uint_          = new PrimitiveType(0, PrimitiveTypeEnum.uint_);
  static immutable string_        = new PrimitiveType(0, PrimitiveTypeEnum.string_);
  static immutable utf8           = new PrimitiveType(0, PrimitiveTypeEnum.utf8);
  static immutable symbol         = new PrimitiveType(0, PrimitiveTypeEnum.symbol);
  static immutable anything       = new PrimitiveType(0, PrimitiveTypeEnum.anything);
  static immutable nullable       = new PrimitiveType(0, PrimitiveTypeEnum.nullable);
  static immutable optionalValue  = new PrimitiveType(0, PrimitiveTypeEnum.optionalValue);
  static immutable type           = new PrimitiveType(0, PrimitiveTypeEnum.type);
  static immutable constructBlock = new PrimitiveType(0, PrimitiveTypeEnum.constructBlock);
  static immutable pointer        = new PrimitiveType(0, PrimitiveTypeEnum.pointer);
  static immutable list           = new PrimitiveType(0, PrimitiveTypeEnum.list);
  static immutable class_         = new PrimitiveType(0, PrimitiveTypeEnum.class_);
  static immutable constructBreak = new PrimitiveType(0, PrimitiveTypeEnum.constructBreak);
  static immutable statementMode  = new PrimitiveType(0, PrimitiveTypeEnum.statementMode);
}

class KeywordType : ConstructType
{
  string keyword;
  this(size_t lineNumber, string keyword) inout pure
  {
    super(lineNumber);
    // TODO: I could validate that the keywors is a valid symbol,
    //       or use another constructor that passes in a symbol that's
    //       already been verified to be valid
    this.keyword = keyword;
  }
  @property final override string tryAsKeyword() const pure { return keyword; }
  final override void writePattern(PureStringSink sink) const
  {
    // A keyword type will be selected by the pattern compiler
    // if a string is used for the pattern type
    formattedWrite(sink, "\"%s\"", keyword);

    // NOTE: if I decide to keep the 'symbolString syntax, I could use that instead
  }
  final override void writeInternalFactoryCode(PureStringSink sink) const
  {
    sink("new immutable KeywordType(\"");
    sink(keyword);
    sink("\")");
  }
  //@property final override string internalValueClassIfRequired() const pure { return null;  }
  //@property final override string internalValueClassIfOptional() const pure { return "ConstructSymbol";  }
  @property final override string internalValueClass() const pure { return "ConstructSymbol";  }

  @property final override PrimitiveTypeEnum asPrimitive() const pure { return PrimitiveTypeEnum.symbol; }
  final override bool supportsValue(const(ConstructObject) obj) const pure
  {
    auto symbol = obj.tryAsConstructSymbol;
    return symbol && symbol.value == keyword;
  }
  final override TypeMatch matchValue(const(ConstructObject) obj) const
  {
    throw imp();
  }

  override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("\"");
    sink(keyword);
    sink("\"");
  }
  mixin virtualEqualsMember!KeywordType;
  final bool typedEquals(const(KeywordType) other) const pure
  {
    return this.keyword == other.keyword;
  }
}
class ConstructPattern : ConstructType
{
  immutable(ConstructType) opType;
  const(PatternNode)[] patternNodes;
  this(size_t lineNumber, Pattern pattern) pure
  {
    super(lineNumber);
    this.opType = pattern.opType;
    this.patternNodes = pattern.nodes;
  }
  this(size_t lineNumber, immutable(Pattern) pattern) pure immutable
  {
    super(lineNumber);
    this.opType = pattern.opType;
    this.patternNodes = pattern.nodes;
  }

  mixin finalTypeNameMembers!"pattern";
  mixin finalPrimitiveTypeMembers!(PrimitiveTypeEnum.constructPattern);

  @property final override inout(ConstructPattern) tryAsConstructPattern() inout pure { return this; }

  @property final override string tryAsKeyword() const pure { return null; }
  final override void writePattern(PureStringSink sink) const
  {
    throw imp();
  }
  final override void writeInternalFactoryCode(PureStringSink sink) const
  {
    throw imp();
  }
  //@property final override string internalValueClassIfRequired() const pure { return "ConstructPattern";  }
  //@property final override string internalValueClassIfOptional() const pure { return "ConstructPattern";  }
  @property final override string internalValueClass() const pure { return "ConstructPattern";  }

  @property final override PrimitiveTypeEnum asPrimitive() const pure { return PrimitiveTypeEnum.constructPattern; }
  final override bool supportsValue(const(ConstructObject) obj) const pure
  {
    throw imp();
  }
  final override TypeMatch matchValue(const(ConstructObject) obj) const
  {
    throw imp();
  }
  override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("(a pattern...toString not implemented)");
  }
  mixin virtualEqualsMember!ConstructPattern;
  final bool typedEquals(const(ConstructPattern) other) const pure
  {
    return this.opType.equals(other.opType) && this.patternNodes == other.patternNodes;
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
  @property final override string tryAsKeyword() const pure { return null; }
  final override void writePattern(PureStringSink sink) const
  {
    throw imp();
  }
  final override void writeInternalFactoryCode(PureStringSink sink) const
  {
    throw imp();
  }
  //@property final override string internalValueClassIfRequired() const pure { return "ConstructClass";  }
  //@property final override string internalValueClassIfOptional() const pure { return "ConstructClass";  }
  @property final override string internalValueClass() const pure { return "ConstructClass";  }
  @property final override PrimitiveTypeEnum asPrimitive() const pure { return PrimitiveTypeEnum.class_; }
  override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink(name);
  }
  @property final override inout(ConstructClassDefinition) tryAsConstructClassDefinition() inout pure { return this; }

  final override bool supportsValue(const(ConstructObject) obj) const pure
  {
    throw imp();
  }
  final override TypeMatch matchValue(const(ConstructObject) obj) const
  {
    throw imp();
  }

  mixin virtualEqualsMember!ConstructClassDefinition;
  final bool typedEquals(const(ConstructClassDefinition) other) const pure
  {
    return this.name == other.name;
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
  @property final override string tryAsKeyword() const pure { return null; }
  final override void writePattern(PureStringSink sink) const
  {
    throw imp();
  }
  final override void writeInternalFactoryCode(PureStringSink sink) const
  {
    throw imp();
  }
  //@property final override string internalValueClassIfRequired() const pure { return "ConstructList";  }
  //@property final override string internalValueClassIfOptional() const pure { return "ConstructList";  }
  @property final override string internalValueClass() const pure { return "ConstructList";  }
  @property final override PrimitiveTypeEnum asPrimitive() const pure { return PrimitiveTypeEnum.list; }
  final override bool supportsValue(const(ConstructObject) obj) const pure
  {
    throw imp();
  }
  final override TypeMatch matchValue(const(ConstructObject) obj) const
  {
    throw imp();
  }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("listOf ");
    itemType.toString(sink);
  }
  mixin virtualEqualsMember!ConstructTypedListType;
  final bool typedEquals(const(ConstructTypedListType) other) const pure
  {
    return this.itemType.equals(other.itemType);
  }
}
class ConstructOptionalOf : ConstructType
{
  ConstructType ofType;
  this(size_t lineNumber, ConstructType ofType) pure
  {
    super(lineNumber);
    this.ofType = ofType;
  }

  @property final override string tryAsKeyword() const pure { return null; }
  final override void writePattern(PureStringSink sink) const
  {
    sink("optional ");
    ofType.writePattern(sink);
  }
  final override void writeInternalFactoryCode(PureStringSink sink) const
  {
    sink("new ConstructOptionalOf(0, ");
    ofType.writeInternalFactoryCode(sink);
    sink(")");
  }
  @property final override string internalValueClass() const pure
  {
    return "ConstructOptionalValue";
  }

  @property final override PrimitiveTypeEnum asPrimitive() const pure { return PrimitiveTypeEnum.optionalValue; }

  final override bool supportsValue(const(ConstructObject) obj) const pure
  {
    throw imp();
    //return obj.primitiveTypeEnum.canBe(typeEnum);
  }
  final override TypeMatch matchValue(const(ConstructObject) obj) const
  {
    throw imp();
    /*
    if(obj.primitiveTypeEnum == typeEnum) {
      return TypeMatch.exact;
    }
    if(obj.primitiveTypeEnum.canBe(typeEnum)) {
      return TypeMatch.subtype;
    }
    return TypeMatch.not;
    */
  }
  override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("optional ");
    ofType.toString(sink);
  }
  mixin virtualEqualsMember!ConstructOptionalOf;
  final bool typedEquals(const(ConstructOptionalOf) other) const pure
  {
    return other && this.ofType.equals(other.ofType);
  }
}

class ConstructVoid : ConstructNullable
{
  this(size_t lineNumber) pure
  {
    super(lineNumber);
  }

  mixin finalTypeNameMembers!"nullable";
  mixin finalPrimitiveTypeMembers!(PrimitiveTypeEnum.nullable);

  @property final override bool isNull() const { return true; }

  override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("void");
  }
  mixin virtualEqualsMember!ConstructVoid;
  final bool typedEquals(const(ConstructVoid) other) const pure
  {
    return other !is null;
  }
}
class ConstructPredicate : ConstructObject
{
  this(size_t lineNumber) pure
  {
    super(lineNumber);
  }

  mixin virtualTypeNameMembers!"predicate";
  mixin virtualPrimitiveTypeMembers!(PrimitiveTypeEnum.predicate);

  @property final override inout(ConstructPredicate) tryAsConstructPredicate() inout pure { return this; }
  @property abstract bool isTrue() const;
}

class ConstructNullable : ConstructPredicate
{
  this(size_t lineNumber) pure
  {
    super(lineNumber);
  }

  mixin virtualTypeNameMembers!"nullable";
  mixin virtualPrimitiveTypeMembers!(PrimitiveTypeEnum.nullable);

  @property final override inout(ConstructNullable) tryAsConstructNullable() inout pure { return this; }
  @property final override bool isTrue() const { return !isNull; }
  @property abstract bool isNull() const;
}

class ConstructClass : ConstructNullable
{
  const(ConstructClassDefinition) classDef;
  this(size_t lineNumber, const(ConstructClassDefinition) classDef)
  {
    super(lineNumber);
    this.classDef = classDef;
  }

  enum staticTypeName = "class";
  final override string typeName() const pure { return classDef.name; }

  mixin finalPrimitiveTypeMembers!(PrimitiveTypeEnum.class_);

  enum processorValueType = "ConstructClass";

  @property final override inout(ConstructClass) tryAsConstructClass() inout pure { return this; }
  @property final override bool isNull() const { throw imp("ConstructClass.isNull"); }

  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    throw imp("ConstructClass.toString");
    //formattedWrite(sink, "%s", class);
  }
  mixin virtualEqualsMember!ConstructClass;
  final bool typedEquals(const(ConstructClass) other) const pure
  {
    throw imp("ConstructClass.equals");
  }
}

// Note: I could make this a predicate, but maybe not..we'll see
// Note: with dynamic types, after a program checks whether
//       this value is present, it can automatically get typed
//       to it's actual value.
class ConstructOptionalValue : ConstructObject
{
  static immutable(ConstructOptionalValue) Null = new ConstructOptionalValue(0, null);

  const(ConstructObject) value;
  this(size_t lineNumber, const(ConstructObject) value) pure
  {
    super(lineNumber);
    this.value = value;
  }

  // NOTE: type name will not be complete, adding a typeToString
  //       abstract method would allow an easy way to get the type string
  mixin finalTypeNameMembers!"optionalValue";
  mixin finalPrimitiveTypeMembers!(PrimitiveTypeEnum.optionalValue);

  @property final override inout(ConstructOptionalValue) tryAsConstructOptionalValue() inout pure { return this; }

  override void toString(scope void delegate(const(char)[]) sink) const
  {
    if(value) {
      sink("has-value: ");
      value.toString(sink);
    } else {
      sink("<no-value>");
    }
  }
  mixin virtualEqualsMember!ConstructOptionalValue;
  final bool typedEquals(const(ConstructOptionalValue) other) const pure
  {
    return value.equals(other.value);
  }
}

class ConstructNull : ConstructNullable
{
  this(size_t lineNumber) pure
  {
    super(lineNumber);
  }

  mixin finalTypeNameMembers!"nullable";
  mixin finalPrimitiveTypeMembers!(PrimitiveTypeEnum.nullable);

  @property final override bool isNull() const { return true; }

  override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("null");
  }
  mixin virtualEqualsMember!ConstructNull;
  final bool typedEquals(const(ConstructNull) other) const pure
  {
    return other !is null;
  }
}
class ConstructBool : ConstructPredicate
{
  static immutable(ConstructBool) false_ = new ConstructBool(0, false);
  static immutable(ConstructBool) true_ = new ConstructBool(0, true);

  bool value;
  this(size_t lineNumber, bool value) pure
  {
    super(lineNumber);
    this.value = value;
  }

  mixin finalTypeNameMembers!"bool";
  mixin finalPrimitiveTypeMembers!(PrimitiveTypeEnum.bool_);

  enum processorValueType = "ConstructBool";

  @property final override bool isTrue() const { return value; }
  @property final override inout(ConstructBool) tryAsConstructBool() inout pure { return this; }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink(value ? "true" : "false");
  }
  mixin virtualEqualsMember!ConstructBool;
  final bool typedEquals(const(ConstructBool) other) const pure
  {
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

  mixin finalTypeNameMembers!"[return]";
  mixin finalPrimitiveTypeMembers!(PrimitiveTypeEnum.anything);

  @property final override inout(ConstructReturn) tryAsConstructReturn() inout pure { return this; }
  override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("[return]");
  }
  final override bool equals(const(ConstructObject) otherObj, bool checkLineNumber = true) const pure
  {
    if(checkLineNumber && lineNumber != otherObj.lineNumber) {
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
  @property final override inout(ConstructNumber) tryAsConstructNumber() inout pure { return this; }

  abstract const(ConstructNumber) add(const(ConstructNumber) other) const pure;
  abstract const(ConstructNumber) multiply(const(ConstructNumber) other) const pure;
}
class ConstructPointer : ConstructNullable
{
  void* pointer;
  this(size_t lineNumber, void* pointer) pure
  {
    super(lineNumber);
    this.pointer = pointer;
  }

  mixin finalTypeNameMembers!"pointer";
  mixin finalPrimitiveTypeMembers!(PrimitiveTypeEnum.pointer);

  enum processorValueType = "ConstructPointer";

  @property final override inout(ConstructPointer) tryAsConstructPointer() inout pure { return this; }
  @property final override bool isNull() const { return pointer is null; }

  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    formattedWrite(sink, "%s", pointer);
  }
  mixin virtualEqualsMember!ConstructPointer;
  final bool typedEquals(const(ConstructPointer) other) const pure
  {
    return other && this.pointer == other.pointer;
  }
}

class ConstructArray : ConstructObject
{
  this(size_t lineNumber) pure
  {
    super(lineNumber);
  }
  enum staticTypeName = "array";
  enum staticPrimitiveTypeEnum = PrimitiveTypeEnum.array;

  enum processorValueType = "ConstructArray";

  //@property final override inout(ConstructString) tryAsConstructString() inout pure { return this; }

  @property abstract size_t length() const;
}

class ConstructString : ConstructArray
{
  this(size_t lineNumber) pure
  {
    super(lineNumber);
  }
  enum staticTypeName = "string";
  enum staticPrimitiveTypeEnum = PrimitiveTypeEnum.string_;

  enum processorValueType = "ConstructString";

  @property final override inout(ConstructString) tryAsConstructString() inout pure { return this; }

  abstract string toUtf8() const pure;
  abstract size_t stringByteLength(PrimitiveTypeEnum type) const pure;
}
class ConstructStatementMode : ConstructObject
{
  static immutable(ConstructStatementMode) default_ = new immutable ConstructStatementMode
    (0, DefaultStatementModeConstruct.definition);
  
  const(PatternConstructDefinition) handlerConstruct;
  this(size_t lineNumber, const(PatternConstructDefinition) handlerConstruct) pure
  {
    super(lineNumber);
    this.handlerConstruct = handlerConstruct;
  }
  this(size_t lineNumber, immutable(PatternConstructDefinition) handlerConstruct) pure immutable
  {
    super(lineNumber);
    this.handlerConstruct = handlerConstruct;
  }
  enum staticTypeName = "statementMode";
  enum staticPrimitiveTypeEnum = PrimitiveTypeEnum.statementMode;

  mixin finalTypeNameMembers!"statementMode";
  mixin finalPrimitiveTypeMembers!(PrimitiveTypeEnum.statementMode);

  enum processorValueType = "ConstructStatementMode";

  @property final override inout(ConstructStatementMode) tryAsConstructStatementMode() inout pure { return this; }

  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    throw imp();
  }
  mixin virtualEqualsMember!ConstructStatementMode;
  final bool typedEquals(const(ConstructStatementMode) other) const pure
  {
    throw imp();
    /*
    return other &&
      this.sourceVar == other.sourceVar &&
      this.resultVar == other.resultVar &&
      this.handlerBlock == other.handlerBlock;
    */
  }
}

