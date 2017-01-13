module construct.primitives;

import core.stdc.stdlib : malloc;
import std.format : sformat, format, formattedWrite;
import std.array  : replace;
import std.conv   : to;
import std.bigint : BigInt;

import construct.util;
import construct.logging;
import construct.parserCore;
import construct.backendCore;
import construct.patterns;
import construct.processor;

import backend : loadConstructBackend, importBackendPackage, loadBackendType;

//
// Basic Primitives with no need for a pattern because they have no parameters
//
// These primitives are required to create other patterns.
// Their patterns cannot be created from parsing construct pattern string
// because these patterns are required to create other patterns.
//
const(ConstructResult) falseHandler(ConstructProcessor* processor, const(ConstructDefinition) def, const(ConstructSymbol) constructSymbol,
				    Object handlerObject, const(PatternNode)[] patternNodes, const(ObjectOrSize)[] objects) pure
{
  return ConstructResult(new ConstructBool(constructSymbol.lineNumber, false));
}
immutable falseConstructDefinition = new immutable InternalPatternConstructDefinition
  ("false", [immutable PatternHandler(&falseHandler, null, null)], null, ConstructAttributes.init, null);
const(ConstructResult) trueHandler(ConstructProcessor* processor, const(ConstructDefinition) def, const(ConstructSymbol) constructSymbol,
                                   Object handlerObject, const(PatternNode)[] patternNodes, const(ObjectOrSize)[] objects) pure
{
  return ConstructResult(new ConstructBool(constructSymbol.lineNumber, true));
}
immutable trueConstructDefinition = new immutable InternalPatternConstructDefinition
  ("true", [immutable PatternHandler(&trueHandler, null, null)], null, ConstructAttributes.init, null);
const(ConstructResult) nullHandler(ConstructProcessor* processor, const(ConstructDefinition) def, const(ConstructSymbol) constructSymbol,
                                   Object handlerObject, const(PatternNode)[] patternNodes, const(ObjectOrSize)[] objects) pure
{
  return ConstructResult(new ConstructNull(constructSymbol.lineNumber));
}
immutable nullConstructDefinition = new immutable InternalPatternConstructDefinition
  ("null", [immutable PatternHandler(&nullHandler, null, null)], null, ConstructAttributes.init, null);

const(ConstructResult) commaHandler(ConstructProcessor* processor, const(ConstructDefinition) def, const(ConstructSymbol) constructSymbol,
                                   Object handlerObject, const(PatternNode)[] patternNodes, const(ObjectOrSize)[] objects) pure
{
  return const ConstructResult(constructSymbol);
}
immutable commaConstructDefinition = new immutable InternalPatternConstructDefinition
  (",", [immutable PatternHandler(&commaHandler, null, null)], null, ConstructAttributes.init, null);

immutable oneTypePattern = [immutable PatternNode("type", CountType.one, PatternNodeFlags.none, PrimitiveType.type)];
/*
struct ListOfConstruct
{
  private static const(ConstructResult) handler(ConstructProcessor* processor, const(ConstructDefinition) def,
                                                const(ConstructSymbol) constructSymbol, Object handlerObject,
                                                const(PatternNode)[] patternNodes, const(ObjectOrSize)[] objects) pure

  {
    assert(objects.length == 1);
    auto type = objects[0].obj.tryAsConstructType;
    assert(type);
    return ConstructResult(new ConstructTypedListType(constructSymbol.lineNumber, type));
  }
  static immutable definition = new immutable InternalPatternConstructDefinition
    ("listOf", [immutable PatternHandler(&handler, null, oneTypePattern)], null, ConstructAttributes.init, null);
}
*/
// I think this has to be defined early because it is used to process constructs
class DefaultStatementModeConstruct
{
  public static const(ConstructResult) handle(ConstructProcessor* processor, const(ConstructObject) object, const(ConstructResult.Action) action) pure
  {
    if(object) {
      if(action != ConstructResult.Action.return_) {
        throw processor.semanticError(object.lineNumber, format
                                      ("unhandled statement value (type %s)", object.typeName));
      }
    }
    return ConstructResult(null);
  }
  private static const(ConstructResult) handler(ConstructProcessor* processor, const(ConstructDefinition) def, const(ConstructSymbol) constructSymbol,
					 Object handlerObject, const(PatternNode)[] patternNodes, const(ObjectOrSize)[] objects) pure
  {
    assert(objects.length == 3);
    assert(objects[0].size == 0);
    return handle(processor, objects[1].obj, objects[2].action);
  }
  static immutable definition = new immutable InternalPatternConstructDefinition
    ("defaultStatementMode", [immutable PatternHandler(&handler, null, null)], null, ConstructAttributes.init, null);
}
//mixin(formattedString!(generatePatternConstructCode)(ConstructName("defaultStatementMode"), "(source, result)", q{
//  throw imp();
//}));

//
// Functions to generate D code for processing constructs
//
inout(char)[] firstLetterToUpper(inout(char)[] name) pure
{
  if(name[0] >= 'a' && name[0] <= 'z') {
    return cast(inout(char)[])((cast(char)(name[0] - 'a' + 'A')) ~ name[1..$]);
  }
  return name;
}
size_t patternNodeArgCount(const(Pattern) pattern) pure
{
  size_t count = 0;
  if(pattern.opType !is null) {
    count++;
  }
  foreach(node; pattern.nodes) {
    string typeName = node.type.internalValueClass;
    if(typeName && node.name != "_") {
      count++;
    }
  }
  return count;
}
void genConstructHandlerSignature(PureStringSink sink, const(char)[] funcName, const(Pattern) pattern) pure
{
  sink("private static const(ConstructResult) ");
  sink(funcName);
  sink("(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol");

  if(pattern.opType !is null) {
    formattedWrite(sink, ", const(%s) this_", pattern.opType.internalValueClass);
  }

  foreach(node; pattern.nodes) {
    string typeName = node.type.internalValueClass;
    if(typeName && node.name != "_") {
      sink(", const(");
      sink(typeName);
      if(!node.countType.onlyOne) {
        sink(")[] ");
	} else {
        sink(") ");
      }
      sink(node.name);
    }
  }
  sink(")");
}


void sanityCheckConstructArgs(const(PatternNode)[] patternNodes, const(ObjectOrSize)[] args)
{
  //logDev("------------- sanityCheckConstructArgs --------------");
  
  size_t argumentShift = 0;
  size_t argumentIndex = 0;
  foreach(node; patternNodes) {
    //logDev("Checking node %s", node);
    if(node.name == "_") {
      continue; // nameless pattern nodes don't get arguments
    }
    
    if(node.countType.isMultiple) {
      size_t nodeSizeOffset = argumentShift + argumentIndex;
      //slogDev("nodeSizeOffset is %s", nodeSizeOffset);
      if(nodeSizeOffset >= args.length) {
        logError("CodeBug: not enough arguments for construct pattern, dumping pattern...");
        printPatternArguments(patternNodes, args);
        throw new Exception("sanityCheckConstructArgs: not enough arguments for this pattern(1)");
      }
      size_t nodeObjectCount = args[nodeSizeOffset].size;
      argumentShift += nodeObjectCount;
    }
    argumentIndex++;
  }
  if(argumentShift + argumentIndex != args.length) {
    logError("CodeBug: expected %d argument(s) for construct pattern but got %d, dumping pattern...", argumentShift+argumentIndex, args.length);
    printPatternArguments(patternNodes, args);
    throw new Exception("sanityCheckConstructArgs: not enough arguments for this pattern(2)");
  }
}

const(T)[] patternNodeArguments(T)(bool oneOrMore, size_t* runtimeArgOffset,
                                   size_t patternNodeArgumentIndex, const(ObjectOrSize)[] args) if (is (T == class))
{
  size_t nodeSizeOffset = *runtimeArgOffset + patternNodeArgumentIndex;
  assert(nodeSizeOffset < args.length); // should have already been checked by sanityCheckConstructArgs
  size_t nodeObjectCount = args[nodeSizeOffset].size;
  if(nodeObjectCount > 0) {
    *runtimeArgOffset += nodeObjectCount;
    assert(nodeSizeOffset + nodeObjectCount < args.length); // should have already been checked by sanityCheckConstructArgs
    return cast(const(T)[])args[nodeSizeOffset+1..nodeSizeOffset+1+nodeObjectCount];
  }
  return null;
}

void generateHandlerThunkFunction(PureStringSink sink, const(char)[] linePrefix, const(char)[] name,
                                  const(char)[] handlerFunctionName, const(Pattern) pattern) pure
{
  sink(linePrefix);
  sink("private static const(ConstructResult) ");
  sink(name);
  sink("(ConstructProcessor* processor");
  sink(", const(ConstructDefinition) constructDefinition");
  sink(", const(ConstructSymbol) constructSymbol");
  sink(", Object handlerObject");
  sink(", const(PatternNode)[] patternNodes");
  sink(", const(ObjectOrSize)[] args)\n");
  sink(linePrefix);
  sink("{\n");

  //sink(linePrefix);
  //sink("    assert(args.length == ");
  //formattedWrite(sink, "%s", patternNodeArgCount(pattern));
  //sink(");\n");
  sink(linePrefix);
  if(pattern.opType is null) {
    sink("    sanityCheckConstructArgs(patternNodes, args);\n");
  } else {
    sink("    sanityCheckConstructArgs(patternNodes, args[1..$]); /*skip the operator object*/\n");
  }

  sink(linePrefix);
  sink("    size_t runtimeArgOffset = 0;\n");

  sink(linePrefix);
  sink("    return ");
  sink(handlerFunctionName);
  sink("(processor, constructSymbol");

  size_t argIndex = 0;
  if(pattern.opType !is null) {
    sink(",\n");
    sink(linePrefix);
    formattedWrite(sink, "        args[runtimeArgOffset+%s].obj.enforceAs!%s",
		   argIndex, pattern.opType.internalValueClass);
    argIndex++;
  }

  foreach(node; pattern.nodes) {
    string typeName = node.type.internalValueClass;
    if(typeName && node.name != "_") {
      sink(",\n");
      sink(linePrefix);
      if(node.countType == CountType.one) {
        formattedWrite(sink, "        args[runtimeArgOffset+%s].obj.enforceAs!%s", argIndex, typeName);
      } else if(node.countType == CountType.optional) {
        formattedWrite(sink, "        (args[runtimeArgOffset+%s].obj is null) ? null : args[%s].obj.enforceAs!%s", argIndex, argIndex, typeName);
      } else {
        formattedWrite(sink, "        patternNodeArguments!(%s)(%s, &runtimeArgOffset, %s, args)",
                       typeName, node.countType == CountType.oneOrMore, argIndex);
      }
      argIndex++;
    }
  }

  sink(");\n");
  sink(linePrefix);
  sink("}\n");
}
struct ConstructPatternHandler
{
  string patternCode;
  immutable(Pattern) compiledPattern;
  string handlerCode;
  static ConstructPatternHandler fromDataStructure(string pattern)(string handlerCode)
  {
    mixin("static immutable Pattern patternDataStructure = "~pattern~";");
    return ConstructPatternHandler(pattern, patternDataStructure, handlerCode);
  }
  static ConstructPatternHandler fromPattern(string patternCode, string handlerCode)
  {
    return ConstructPatternHandler("compileBasicPattern(\""~patternCode.replace(`"`,`\"`)~"\")",
				   compileBasicPattern(patternCode).immutable_, handlerCode);
  }
}
ConstructPatternHandler[][immutable(ConstructType)] createTypeMap(ConstructPatternHandler[] patternHandlers)
{
  ConstructPatternHandler[][immutable(ConstructType)] map;
  foreach(patternHandler; patternHandlers) {
    map[patternHandler.compiledPattern.opType] ~= patternHandler;
  }
  return map;
}
struct ConstructName
{
  string symbol;
  string name;
  this(string symbolAndName)
  {
    this.symbol = symbolAndName;
    this.name = symbolAndName;
  }
  this(string symbol, string name)
  {
    this.symbol = symbol;
    this.name = name;
  }
}
void generateConstructCode(PureStringSink sink, const(ConstructName) constructSymbolAndName,
                           ConstructPatternHandler[] allPatternHandlers)
{
  auto handlerTypeMap = createTypeMap(allPatternHandlers);

  auto constructName = constructSymbolAndName.name;
  auto constructNameUpperCase = constructName.firstLetterToUpper();

  sink("struct ");
  sink(constructNameUpperCase);
  sink("Construct\n");
  sink("{\n");

  size_t noTypeIndex;
  {
    size_t opPatternIndex = 0;
    foreach(opType; handlerTypeMap.keys) {
      if(!opType) {
        noTypeIndex = opPatternIndex;
      }
      auto opPatternHandlers = handlerTypeMap[opType];
      foreach(handlerIndex, opPatternHandler; opPatternHandlers) {
        formattedWrite(sink, "    private static immutable pattern_%s_%s = %s;",
                       opPatternIndex, handlerIndex, opPatternHandler.patternCode);
      }
      opPatternIndex++;
    }
  }

  char[64] handlerFuncNameBuffer;
  char[64] thunkFuncNameBuffer;
  {
    size_t opPatternIndex = 0;
    foreach(opType; handlerTypeMap.keys) {
      auto opPatternHandlers = handlerTypeMap[opType];
      foreach(handlerIndex, patternHandler; opPatternHandlers) {
        auto handlerFuncName = sformat(handlerFuncNameBuffer, "handler_%s_%s", opPatternIndex, handlerIndex);
        auto thunkFuncName   = sformat(thunkFuncNameBuffer  , "thunkToHandler_%s_%s", opPatternIndex, handlerIndex);

        sink("\n    ");
        genConstructHandlerSignature(sink, handlerFuncName, patternHandler.compiledPattern);
        sink("\n");
        sink("    {");
        sink(patternHandler.handlerCode);
        sink("    }\n");

        //
        // Generate Thunk Function
        //
        generateHandlerThunkFunction(sink, "    ", thunkFuncName, handlerFuncName,
                                     patternHandler.compiledPattern);
      }
      opPatternIndex++;
    }
  }
  sink("    static immutable definition = new immutable InternalPatternConstructDefinition\n");
  formattedWrite(sink, "        (\"%s\", [", constructSymbolAndName.symbol);
  foreach(handlerIndex, patternHandler; allPatternHandlers) {
    if(!patternHandler.compiledPattern.opType) {
      if(handlerIndex > 0) {
	sink(",\n          ");
      }
      formattedWrite(sink, "immutable PatternHandler(&thunkToHandler_%s_%s, null, pattern_%s_%s.nodes)",
                     noTypeIndex, handlerIndex, noTypeIndex, handlerIndex);
    }
  }
  sink("], [");
  {
    size_t opPatternIndex = 0;
    foreach(opType; handlerTypeMap.keys) {
      if(opType) {
        if(opPatternIndex > 0) {
          sink(",\n          ");
        }
        sink("immutable OpPatternHandlers(");
        opType.writeInternalFactoryCode(sink);
        sink(", [");
        auto opPatternHandlers = handlerTypeMap[opType];
        foreach(handlerIndex, patternHandler; opPatternHandlers) {
          if(handlerIndex > 0) {
            sink(",\n          ");
          }
          formattedWrite(sink, "immutable PatternHandler(&thunkToHandler_%s_%s, null, pattern_%s_%s.nodes)",
                         opPatternIndex, handlerIndex, opPatternIndex, handlerIndex);
        }
        sink("])");
      }
      opPatternIndex++;
    }
  }
  sink("], ConstructAttributes.init, null);\n");
  sink("}\n");
}


mixin(formattedString!(generateConstructCode)
      (ConstructName("pattern"), [ConstructPatternHandler.fromDataStructure!
				  (q{immutable Pattern(null, [immutable PatternNode("patternStart", CountType.one, PatternNodeFlags.raw, KeywordType.openParens),
							      immutable PatternNode("patternObjects", CountType.many, PatternNodeFlags.none, PrimitiveType.anything),
							      immutable PatternNode("_", CountType.one, PatternNodeFlags.raw, KeywordType.closeParens),])})(q{
   return ConstructResult(new ConstructPattern(patternStart.lineNumber, processPattern(processor, definition, patternObjects, patternStart.lineNumber)));
})]));

//
// Functions to generate D code for constructs
// The constructs that use these functions require the constructs
// that are defined above this in order to create construt patterns.
//
void generatePatternConstructCode(PureStringSink sink, const(ConstructName) symbolAndName, string patternCode, string handlerCode)
{
  generateConstructCode(sink, symbolAndName, [ConstructPatternHandler.fromPattern(patternCode, handlerCode)]);
}

//
// More extended primitives
// These primitives have patterns that rely on the basic primitives
mixin(formattedString!(generatePatternConstructCode)(ConstructName("let"), "(name raw symbol, value, break_ optional raw \";\")", q{
  processor.addSymbol(name.value, value);
  return const ConstructResult((break_ is null) ? value : null);
}));
mixin(formattedString!(generatePatternConstructCode)(ConstructName("set"), "(name raw symbol, value, break_ optional raw \";\")", q{
  processor.setSymbol(name.lineNumber, name.value, value);
  return const ConstructResult((break_ is null) ? value : null);
}));
mixin(formattedString!(generatePatternConstructCode)(ConstructName("letset"), "(name raw symbol, value, break_ optional raw \";\")", q{
  processor.letSetSymbol(name.value, value);
  return const ConstructResult((break_ is null) ? value : null);
}));
mixin(formattedString!(generatePatternConstructCode)(ConstructName("import"), `(isRelative optional raw "relative", name string)`, q{
  //processor.findAndImport(constructSymbol.lineNumber, name.toUtf8);
  processor.import_(constructSymbol.lineNumber, isRelative !is null, name.toUtf8);
  return ConstructResult(null);
}));
mixin(formattedString!(generatePatternConstructCode)(ConstructName("exec"), "(code constructBlock)", q{
  // TODO: should the return action be swallowed or propogate up?
  throw imp("exec construct");
  //return processor.processBlock(code.objects).withNoReturn()?;
}));

mixin(formattedString!(generateConstructCode)
      (ConstructName("defcon"), [ConstructPatternHandler.fromPattern
				 ("(name raw symbol, patternList raw parenList,  _ raw \";\")", q{
    ConstructAttributes attributes;

    auto processedPattern = processPattern(processor, definition, patternList.objects, patternList.lineNumber);
    throw imp("backend constructs");
    /+
     auto backendFunc = loadConstructBackend(name.value);
     if(backendFunc == null) {
     throw processor.semanticError(name.lineNumber, format
     ("the backend does not implement construct '%s'", name));
     }
     processor.addSymbol(name.value, new FunctionConstructDefinition
     (/*Pattern(nodes), */constructSymbol.lineNumber, processor.currentFile.name, attributes, null, requiredParams, null, backendFunc));
     return null;
     +/
  }), ConstructPatternHandler.fromPattern
				 ("(name raw symbol, patternList parenList, implementation constructBlock)", q{

    ConstructAttributes attributes;
    auto processedPattern = processPattern(processor, definition, patternList.objects, patternList.lineNumber);

    //
    // Check if current scope has a construct with the same name
    //
    auto existing = processor.tryLookupSymbolAndScope(name.value);
    if(existing.entryList.first) {

      if(existing.entryList.moreCount) {
        throw imp("a symbol with multiple entries at the same scope");
      }

      if(existing.symbols is processor.currentScope.symbols) {
        throw imp("add new construct pattern and block to existing one");
        //existing.addBlockConstruct(constructSymbol.lineNumber, processor.currentFile.name, processedPattern, implementation);
      } else {

        if(auto existingConstruct = existing.entryList.first.tryAsConstructDefinition) {
          PatternConstructDefinition patternConstruct = cast(PatternConstructDefinition)existingConstruct;
          if(!patternConstruct) {
            throw imp("combining with non-pattern construct");
          }

          if(processedPattern.opType) {
            throw imp();
            //processor.addSymbol(name.value, patternConstruct.combine(immutable OpPatternHandlers(processedPattern.opType,
            //pattern));
          } else {
            processor.addSymbol(name.value, patternConstruct.combine
                                (immutable PatternHandler(&handleConstructWithBlock, implementation.immutable_,
                                                          processedPattern.nodes.immutable_)));
          }

        } else {
          addNewConstruct(processor, cast(string)name.value, constructSymbol.lineNumber,
                          processedPattern, implementation);
        }

      }

    } else {
      addNewConstruct(processor, cast(string)name.value, constructSymbol.lineNumber,
                      processedPattern, implementation);
    }
    return ConstructResult(null);
})]));

private void addNewConstruct(ConstructProcessor* processor, string name, size_t lineNumber,
			     Pattern processedPattern, const(ConstructBlock) implementation)
{
  ConstructAttributes attributes;

  immutable(PatternHandler)[] patternHandlerArray =
    [immutable PatternHandler(&handleConstructWithBlock, implementation.immutable_, processedPattern.nodes.immutable_)];

  immutable(PatternHandler)[] noOpPatternHandlers;
  immutable(OpPatternHandlers)[] opPatternHandlers;
  if(processedPattern.opType) {
    noOpPatternHandlers = null;
    opPatternHandlers   = [immutable OpPatternHandlers(processedPattern.opType, patternHandlerArray)];
  } else {
    noOpPatternHandlers = patternHandlerArray;
    opPatternHandlers   = null;
  }
  processor.addSymbol(name, new immutable PatternConstructDefinition
                      (name, noOpPatternHandlers, opPatternHandlers,
                       lineNumber, cast(string)processor.currentFile.relativeName,
                       attributes, null));
}

mixin(formattedString!(generateConstructCode)
      (ConstructName("toSymbol"), [ConstructPatternHandler.fromPattern("(string_ string)", "return ConstructResult(new ConstructSymbol(string_.lineNumber, string_.toUtf8()));")]));

mixin(formattedString!(generatePatternConstructCode)(ConstructName("symbolRef"), "(name raw symbol)", q{
   return const ConstructResult(name);
}));

mixin(formattedString!(generatePatternConstructCode)
      // TODO: probably want to require '(' ')' or raw parenList for the condition
      (ConstructName("if"), "(condition predicate, trueCase constructBlock)", q{
  if(condition.isTrue) {
    processor.pushScope(trueCase.lineNumber, ScopeType.block, true);
    scope(exit) { processor.popScope(); }
    return processor.processBlock(trueCase.objects);
  } else {
    return ConstructResult(null);
  }
}));
/*
immutable ifConstructDefinition = new immutable FunctionConstructDefinition
  ("if", __LINE__, __FILE__, ConstructAttributes.init, null, &ifConstructHandler);
const(ConstructObject) ifConstructHandler(ConstructProcessor* processor,
                                          const(ConstructDefinition) definition,
                                          const(ConstructSymbol) constructSymbol,
                                          const(ConstructObject)[] objects, size_t* argIndex)
{
  auto condition = processor.consumeTypedValue!ConstructBool(definition, constructSymbol, objects, argIndex);
  auto trueCase = processor.consumeTypedValue!ConstructBlock(definition, constructSymbol, objects, argIndex);
  ConstructObject resultObject = null;
  if(condition.value) {
    processor.pushScope(trueCase.lineNumber, ScopeType.block, true);
    scope(exit) { processor.popScope(); }
    return processor.processBlock(trueCase.objects).unconst;
  } else {
    return null;
  }
}
*/


mixin(formattedString!(generatePatternConstructCode)(ConstructName("importBackendPackage"),
     //"(packageName raw symbol, limitList optional pattern(_ \":\", includeSymbolList many raw symbol), _ raw \";\")", q{
     "(packageName raw symbol, _ raw \";\")", q{
      importBackendPackage(processor, packageName.value, null);//limitList.includeSymbolList);
      return ConstructResult(null);
}));
mixin(formattedString!(generatePatternConstructCode)(ConstructName("loadBackendType"), "(typeName raw symbol, _ raw \";\")", q{
      auto backendType = loadBackendType(typeName.value);
      processor.addSymbol(typeName.value, backendType);
      return ConstructResult(null);
}));
/*
mixin(formattedString!(generatePatternConstructCode)(ConstructName("makeTypeFrom"), "(parentType type)", q{
      return ConstructResult(new immutable ConstructUserDefinedType(constructSymbol.lineNumber, parentType.immutable_).unconst);
}));
*/
mixin(formattedString!(generatePatternConstructCode)(ConstructName("deftype"), "(typeName raw symbol, _ raw \"inheritFrom\", parentType type, _ raw \";\")", q{
      processor.addSymbol(typeName.value, new immutable ConstructUserDefinedType(typeName.lineNumber, typeName.value, parentType.immutable_));
      return ConstructResult(null);
}));
/*
mixin(formattedString!(generatePatternConstructCode)(ConstructName("typeof"), "(obj)", q{
  return obj.
}));
*/
mixin(formattedString!(generatePatternConstructCode)(ConstructName("isA"), "(this, typeToCheck type)", q{
      return ConstructResult(new ConstructBool(constructSymbol.lineNumber, this_.canBe(typeToCheck)));
}));


//
// Operators
//
mixin(formattedString!(generatePatternConstructCode)(ConstructName("+", "Plus"), "(this number, right number)", q{
      return const ConstructResult(this_.add(right));
}));
mixin(formattedString!(generatePatternConstructCode)(ConstructName("*", "Multiply"), "(this number, right number)", q{
      return const ConstructResult(this_.multiply(right));
}));
mixin(formattedString!(generatePatternConstructCode)(ConstructName("==", "Equals"), "(this, right)", q{
      return const ConstructResult(new ConstructBool(constructSymbol.lineNumber, this_.equals(right, false)));
}));
mixin(formattedString!(generatePatternConstructCode)(ConstructName("!=", "NotEquals"), "(this, right)", q{
      return const ConstructResult(new ConstructBool(constructSymbol.lineNumber, !this_.equals(right, false)));
}));


mixin(formattedString!(generatePatternConstructCode)(ConstructName("assert"), "(predicate bool)", q{
      if(!predicate.value) {
	throw new ConstructAssertException(constructSymbol.lineNumber, processor, "assertion failed");
      }
      return ConstructResult(null);
}));

mixin(formattedString!(generatePatternConstructCode)(ConstructName("throw"), "(message string, _ raw \";\")", q{
    throw new ConstructThrownException(constructSymbol.lineNumber, processor, message.toUtf8());
}));
/*
mixin(formattedString!(generatePatternConstructCode)(ConstructName("throw"), "(messageObjects oneOrMore string, _ raw \";\")", q{
  auto messageBuilder = appender!(char[])(256);
  foreach(messageObject; messageObjects.length) {
    messageObject.toString(messageBuilder.put);
  }
  throw new ConstructThrownException(constructSymbol.lineNumber, processor, cast(string)messageBuilder.data);
}));
*/
mixin(formattedString!(generatePatternConstructCode)(ConstructName("return"), "(value, _ raw \";\")", q{
  return const ConstructResult(value, ConstructResult.Action.return_);
}));

mixin(formattedString!(generatePatternConstructCode)(ConstructName("not"), "(value predicate)", q{
      return const ConstructResult(new ConstructBool(constructSymbol.lineNumber, !value.isTrue));
}));
/*
// NOTE: should probably be a "dotted" keyword operator
mixin(formattedString!(generatePatternConstructCode)(ConstructName("itemType"), "(this list)", q{
    auto typeObject = this_.itemType.definition.typeObject;
    if(!typeObject) {
      throw imp(format("PrimitiveType %s has no type object configured", this_.itemType));
    }
    return const ConstructResult(typeObject);
}));
*/

//
// String Operations
//

mixin(formattedString!(generatePatternConstructCode)(ConstructName("byteLength"), "(this string, type optional type)", q{
      auto primitiveType = (type is null) ? PrimitiveTypeEnum.utf8 : type.asPrimitive;
      return const ConstructResult(new ConstructUint(constructSymbol.lineNumber, BigInt(this_.stringByteLength(primitiveType))));
}));
mixin(formattedString!(generatePatternConstructCode)(ConstructName("getUtf8Byte"),
						     "(str utf8, byteIndex unsigned)", q{
  if(byteIndex.value >= str.value.length) {
    throw new ConstructIndexOutOfRangeException(constructSymbol.lineNumber, processor, byteIndex.value);
  }
  return const ConstructResult(new ConstructUbyte(constructSymbol.lineNumber, BigInt(cast(ubyte)str.value[byteIndex.value.to!size_t])));
}));
mixin(formattedString!(generatePatternConstructCode)(ConstructName("lastIndexOf"), "(this utf8, stringToFind utf8)", q{
      //logDev("lastIndexOf '%s' '%s'", this_.value, stringToFind.value);
      BigInt result = void;
      if(this_.value.length >= stringToFind.value.length) {
        for(size_t i = this_.value.length - stringToFind.length; ; i--) {
          if(this_.value[i..i+stringToFind.length] == stringToFind.value) {
            result = BigInt(i);
            break;
          }
          if(i == 0) {
            result = BigInt(-1);
            break;
          }
        }
      }
      return const ConstructResult(new ConstructInteger(constructSymbol.lineNumber, result));
}));
mixin(formattedString!(generateConstructCode)
      (ConstructName("strcpy"), [ConstructPatternHandler.fromPattern("(dest pointer, src string, type type)",q{
    if(type.asPrimitive == PrimitiveTypeEnum.utf8) {
      auto srcString = src.toUtf8;
      (cast(char*)dest.pointer)[0..srcString.length] = srcString[];
    } else {
      throw imp(format("strcpy pointer (type=%s)", type));
    }
    return ConstructResult(null);
}), ConstructPatternHandler.fromPattern("(dest string, src string, type type)", q{
    throw imp("strcpy string");
})]));
// TODO: fix this so it works with integer literals
mixin(formattedString!(generatePatternConstructCode)(ConstructName("slice"), "(this string, start unsigned, limit unsigned)", q{
      throw imp("slice");
}));



//
// The try construct
//
//
// The 'pattern' construct constructs a pattern.
// it takes a list, and converts it to an internal pattern.
//
// Note: the '_' symbol is a special reserved symbol that means
//       the symbol is hidden.
//       defcon a (_ string)
//       {
//         // the string argument is hidden, it was consumed but can't be accessed
//       }
//       this is more useful for certain types such as keywords
//defcon try (tryBlock block, catchBlocks CatchInfo list itemPrefix="catch", finallyBlock block prefix="finally")
// (
//  tryBlock block,
//  catchClauses multi pattern(_ "catch", varName symbol, catchType type, catchBlock block),
//  finallyClause optional pattern(_ "finally", finallyBlock block)
//  )
//version(UsePatternDataStructures) {
/*
  immutable tryPattern = immutable Pattern
    ([immutable PatternNode("tryBlock", CountType.one, Matcher.block),
      immutable PatternNode("catchClauses", CountType.many, false, new immutable ConstructPattern(0, Pattern
			    ([immutable PatternNode(null, CountType.one, false, new immutable KeywordMatcher("catch")),
			      immutable PatternNode("catchBlock", CountType.one, false, Matcher.block)]))),
      immutable PatternNode("finallyClause", CountType.optional, new immutable ConstructPattern(0, Pattern
			    ([immutable PatternNode(null, CountType.one, false, new immutable KeywordMatcher("finally")),
			      immutable PatternNode("finallyBlock", CountType.one, false, Matcher.block)])))]);
*/
/*
} else {
  immutable tryPattern = compileBasicPattern(`
(tryBlock constructBlock,
 catchClauses many pattern(_ "catch", catchBlock constructBlock),
 finallyClause optional pattern(_ "finally", finallyBlock constructBlock))`);
 }*/
/*
immutable tryFunc = immutable FuncAndPattern("handleTry", tryPattern);
mixin(tryFunc.genFunc()~q{
{
  auto code = processor.consumeTypedValue!ConstructBlock(constructSymbol, objects, argIndex);

  ConstructSymbol catchSymbol = null;
  ConstructBlock catchBlock = null;
  ConstructBlock finallyBlock = null;

  while(*argIndex < objects.length) {
    auto nextObject = objects[*argIndex];
    if(auto symbol = nextObject.tryAsConstructSymbol) {
      if(symbol.value == "catch") {
	if(catchBlock) {
	  throw imp("multiple catch blocks");
	}
	(*argIndex)++;
	catchSymbol = processor.consumeSymbol(constructSymbol, objects, argIndex).unconst;
	catchBlock = processor.consumeTypedValue!ConstructBlock(constructSymbol, objects, argIndex).unconst;
	continue;
      }

      if(symbol.value == "finally") {
	throw imp("try-finally");
      }
    }
    break;
  }

  try {
    return processor.processBlock(code.objects);
  } catch(ConstructException e) {
    processor.pushScope(catchBlock.lineNumber, ScopeType.block, true);
    scope(exit) { processor.popScope(); }

    if(catchSymbol) {
      processor.addSymbol(catchSymbol.value, new ConstructUtf8(0, e.msg));
    }
    return processor.processBlock(catchBlock.objects);
  }
}});
mixin(genConstructPatternThunk("tryThunk", tryFunc));
immutable tryConstructDefinition = new immutable InternalPatternConstructDefinition
  ([immutable PatternHandler(tryPattern, &tryThunk)], ConstructAttributes.init, null);
*/

immutable tryConstructDefinition = new immutable FunctionConstructDefinition
  ("try", __LINE__, __FILE__, ConstructAttributes.init, null, &tryConstructHandler);
const(ConstructResult) tryConstructHandler(ConstructProcessor* processor,
                                           const(ConstructDefinition) definition,
                                           const(ConstructSymbol) constructSymbol,
                                           const(ConstructObject)[] objects, size_t* argIndex)
{
  auto code = processor.consumeTypedValue!ConstructBlock(definition, constructSymbol, objects, argIndex);

  ConstructSymbol catchSymbol = null;
  ConstructBlock catchBlock = null;
  ConstructBlock finallyBlock = null;

  while(*argIndex < objects.length) {
    auto nextObject = objects[*argIndex];
    if(auto symbol = nextObject.tryAsConstructSymbol) {
      if(symbol.value == "catch") {
        if(catchBlock) {
          throw imp("multiple catch blocks");
        }
        (*argIndex)++;
        catchSymbol = processor.consumeSymbol(definition, constructSymbol, objects, argIndex).unconst;
        catchBlock = processor.consumeTypedValue!ConstructBlock(definition, constructSymbol, objects, argIndex).unconst;
        continue;
      }

      if(symbol.value == "finally") {
        throw imp("try-finally");
      }
    }
    break;
  }

  try {
    return processor.processBlock(code.objects);
  } catch(ConstructException e) {
    processor.pushScope(catchBlock.lineNumber, ScopeType.block, true);
    scope(exit) { processor.popScope(); }

    if(catchSymbol) {
      processor.addSymbol(catchSymbol.value, new ConstructUtf8(0, e.msg));
    }
    return processor.processBlock(catchBlock.objects);
  }
}

mixin(formattedString!(generatePatternConstructCode)(ConstructName("addSymbolsToCaller"), "(code constructBlock)", q{
    Scope* addSymbolsTo;
    if(processor.currentScope.type == ScopeType.defcon) {
      addSymbolsTo = &processor.scopeStack.data[processor.scopeStack.data.length-1];
    } else {
      throw imp("non root level addSymbolsToCaller");
    }

    processor.pushScope(code.lineNumber, ScopeType.block, true);
    scope(exit) { processor.popScope(); }
    processor.currentScope.addSymbolsTo = addSymbolsTo;

    return processor.processBlock(code.objects);
}));

//
// Foreach
//
mixin(formattedString!(generatePatternConstructCode)(ConstructName("foreach"), "(loopArgs raw parenList, loopCode constructBlock)", q{

  //
  // Parse the foreach iteration list
  //
  string indexVar = null;
  string itemVar;
  ConstructList list;
  {
    size_t listIndex = 0;
    if(listIndex >= loopArgs.objects.length) {
      throw processor.semanticError(constructSymbol.lineNumber, "foreach iteration list is incomplete");
    }
    itemVar = processor.consumeSymbol(definition, constructSymbol, loopArgs.objects, &listIndex).value;

    if(listIndex >= loopArgs.objects.length) {
      throw processor.semanticError(constructSymbol.lineNumber, "foreach iteration list is incomplete");
    }
    auto next = loopArgs.objects[listIndex++].unconst;
    if(next.isSymbolOf(",")) {
      indexVar = itemVar;
      itemVar = processor.consumeSymbol(definition, constructSymbol, loopArgs.objects, &listIndex).value;
      if(listIndex >= loopArgs.objects.length) {
        throw processor.semanticError(constructSymbol.lineNumber, "foreach iteration list is incomplete");
      }
      next = loopArgs.objects[listIndex++].unconst;
    }

    if(auto inSymbol = next.tryAsConstructSymbol) {
    } else {
      throw processor.semanticError(constructSymbol.lineNumber, format
                                    ("foreach expected an 'in' symbol but got %s: %s",
                                     An(next.typeName), next));
    }

    if(listIndex >= loopArgs.objects.length) {
      throw processor.semanticError(constructSymbol.lineNumber, "foreach iteration list is incomplete");
    }
    auto result = processor.consumeValueAlreadyCheckedIndex(definition, loopArgs.objects, &listIndex);
    if(result.hasAction) {
      throw imp("foreach list has an action!");
    }
    if(result.object is null) {
      throw processor.semanticError(constructSymbol.lineNumber, "foreach iteration list got a void expression");
    }
    if(listIndex < loopArgs.objects.length) {
      throw processor.semanticError(constructSymbol.lineNumber, "foreach iteration list has too many items");
    }

    // TODO: I don't want this matching block or bracket lists
    list = result.object.tryAsConstructList.unconst;
    if(!list) {
      throw processor.semanticError(constructSymbol.lineNumber, format
                                    ("foreach requires a list but got %s", An(result.object.typeName)));
    }
    // Make sure it wasn't a block or bracket list
    if(result.object.tryAsConstructBlock || result.object.tryAsConstructBracketList) {
      throw processor.semanticError(constructSymbol.lineNumber, format
                                    ("foreach requires a list but got %s", An(result.object.typeName)));
    }
  }

  if(list.objects.length > 0) {
    processor.pushScope(loopCode.lineNumber, ScopeType.block, true);
    ConstructUint indexObject = null;
    scope(exit) { processor.popScope(); }
    if(indexVar) {
      indexObject = new ConstructUint(loopArgs.lineNumber, BigInt(0));
      processor.addSymbol(indexVar, indexObject);
    }
    processor.addSymbol(itemVar, list.objects[0]);

    auto firstResult = processor.processBlock(loopCode.objects);
    if(firstResult.isReturn) {
      return firstResult;
    }
    foreach(listObject; list.objects[1..$]) {
      processor.setSymbol(constructSymbol.lineNumber, itemVar, listObject);
      if(indexObject) {
        indexObject.value++;
      }
      auto result = processor.processBlock(loopCode.objects);
      if(result.isReturn) {
        return result;
      }
    }
  }

  return ConstructResult(null);
    }));
/*
      immutable foreachConstructDefinition = new immutable FunctionConstructDefinition
  ("foreach", __LINE__, __FILE__, ConstructAttributes.init, null, &foreachConstructHandler);
const(ConstructResult) foreachConstructHandler(ConstructProcessor* processor,
                                           const(ConstructDefinition) definition,
                                           const(ConstructSymbol) constructSymbol,
                                           const(ConstructObject)[] objects, size_t* argIndex)
{
  auto forEachArgs = processor.consumeTypedValue!ConstructList(definition, constructSymbol, objects, argIndex);
  auto forEachCode = processor.consumeTypedValue!ConstructBlock(definition, constructSymbol, objects, argIndex);

  //
  // Parse the foreach iteration list
  //
  string indexVar = null;
  string itemVar;
  ConstructList list;

  {
    size_t listIndex = 0;
    if(listIndex >= forEachArgs.items.length) {
      throw processor.semanticError(constructSymbol.lineNumber, "foreach iteration list is incomplete");
    }
    itemVar = processor.consumeSymbol(definition, constructSymbol, forEachArgs.items, &listIndex).value;

    if(listIndex >= forEachArgs.items.length) {
      throw processor.semanticError(constructSymbol.lineNumber, "foreach iteration list is incomplete");
    }
    auto next = forEachArgs.items[listIndex++].unconst;
    if(next.isListBreak) {
      indexVar = itemVar;
      itemVar = processor.consumeSymbol(definition, constructSymbol, forEachArgs.items, &listIndex).value;
      if(listIndex >= forEachArgs.items.length) {
        throw processor.semanticError(constructSymbol.lineNumber, "foreach iteration list is incomplete");
      }
      next = forEachArgs.items[listIndex++].unconst;
    }

    if(auto inSymbol = next.tryAsConstructSymbol) {
    } else {
      throw processor.semanticError(constructSymbol.lineNumber, format
                                    ("foreach expected an 'in' symbol but got %s: %s",
                                     An(next.typeName), next));
    }

    if(listIndex >= forEachArgs.items.length) {
      throw processor.semanticError(constructSymbol.lineNumber, "foreach iteration list is incomplete");
    }
    auto result = processor.consumeValueAlreadyCheckedIndex(definition, forEachArgs.items, &listIndex);
    if(result.hasAction) {
      throw imp("foreach list has an action!");
    }
    if(result.object is null) {
      throw processor.semanticError(constructSymbol.lineNumber, "foreach iteration list got a void expression");
    }
    if(listIndex < forEachArgs.items.length) {
      throw processor.semanticError(constructSymbol.lineNumber, "foreach iteration list has too many items");
    }

    list = result.object.tryAsConstructList.unconst;
    if(!list) {
      throw processor.semanticError(constructSymbol.lineNumber, format
                                    ("foreach requires a list but got %s", An(result.object.typeName)));
    }
  }

  if(list.items.length > 0) {
    processor.pushScope(forEachCode.lineNumber, ScopeType.block, true);
    ConstructUint indexObject = null;
    scope(exit) { processor.popScope(); }
    if(indexVar) {
      indexObject = new ConstructUint(forEachArgs.lineNumber, BigInt(0));
      processor.addSymbol(indexVar, indexObject);
    }
    processor.addSymbol(itemVar, list.items[0]);

    auto firstResult = processor.processBlock(forEachCode.objects);
    if(firstResult.isReturn) {
      return firstResult;
    }
    foreach(listObject; list.items[1..$]) {
      processor.setSymbol(constructSymbol.lineNumber, itemVar, listObject);
      if(indexObject) {
        indexObject.value++;
      }
      auto result = processor.processBlock(forEachCode.objects);
      if(result.isReturn) {
        return result;
      }
    }
  }

  return ConstructResult(null);
}
  */

//
// Modes
//
//(name symbol, handlerClause pattern(_ "handler", source raw symbol, result raw symbol, handlerBlock constructBlock))
/*
mixin(formattedString!(generatePatternConstructCode)(ConstructName("defStatementMode"), q{
      (name raw symbol, source raw symbol, result raw symbol, handlerBlock constructBlock)
}, q{
      throw imp("defStatementMode");
      //return new ConstructPointer(constructSymbol.lineNumber, malloc(size.value));
}));
*/
mixin(formattedString!(generatePatternConstructCode)(ConstructName("createStatementMode"),
						     "(source raw symbol, result raw symbol, handlerBlock constructBlock)", q{
  ConstructAttributes attributes;
  return const ConstructResult(new ConstructStatementMode(constructSymbol.lineNumber, new immutable PatternConstructDefinition
                                                          (null, [immutable PatternHandler
                                                                  (&handleConstructWithBlock, handlerBlock.immutable_,
                                                                   [immutable PatternNode(source.value, CountType.optional, PatternNodeFlags.none, PrimitiveType.anything),
                                                                    immutable PatternNode(result.value, CountType.optional, PatternNodeFlags.none, PrimitiveType.anything),
                                                                    immutable PatternNode("action", CountType.optional, PatternNodeFlags.none, PrimitiveType.anything)])],
                                                           null, constructSymbol.lineNumber, cast(string)processor.currentFile.relativeName, attributes, null)));
}));
mixin(formattedString!(generatePatternConstructCode)(ConstructName("getStatementMode"), "()", q{
      return const ConstructResult(processor.statementMode);
}));
mixin(formattedString!(generatePatternConstructCode)(ConstructName("setStatementMode"), "(mode statementMode)", q{
      processor.setStatementMode(mode);
      return const ConstructResult(null);
}));

mixin(formattedString!(generatePatternConstructCode)(ConstructName("getPatternMode"), "()", q{
      return const ConstructResult(new ConstructUtf8(constructSymbol.lineNumber, processor.patternMode.to!string));
}));

mixin(formattedString!(generatePatternConstructCode)(ConstructName("setPatternMode"), "(name raw symbol)", q{
  PatternMode mode;
  if(name.value == "firstMatch") {
    mode = PatternMode.firstMatch;
  } else {
    throw processor.semanticError(definition.lineNumber, format("unknown pattern node '%s'", name.value));
  }
  processor.setPatternMode(mode);
  return const ConstructResult(null);
}));

//
// Memory Primitives
//
mixin(formattedString!(generatePatternConstructCode)(ConstructName("malloc"), "(size uint)", q{
      return const ConstructResult(new ConstructPointer(constructSymbol.lineNumber, malloc(size.value.to!uint)));
}));

//
// Classes
//
//(name raw symbol , inheritClause optional pattern(_ :, baseClass symbol)
mixin(formattedString!(generatePatternConstructCode)(ConstructName("class"), "(name raw symbol, definition constructBlock)", q{

      Scope classDefScope;
      {
        processor.pushScope(definition.lineNumber, ScopeType.classDef, true);
        scope(exit) { processor.popScope(); }
        auto result = processor.processBlock(definition.objects).unconst;
        if(result.hasAction) {
          throw imp("class block with an action!");
        }
        if(result.object !is null) {
          throw processor.semanticError(definition.lineNumber, "class block cannot return a value");
        }
        classDefScope = processor.currentScope;
      }

      //
      // Add all the '.' member symbols to the current scope
      //
      foreach(symbolEntryPair; classDefScope.symbols.byKeyValue) {
        auto symbolName = symbolEntryPair.key;
        foreach(symbolEntry; symbolEntryPair.value.each) {

          logDev("need to add class member symbol '%s' to current scope", symbolName);
        }
      }
      //throw imp("adding class members to outer scope as dot operator constructs");

      processor.addSymbol(name.value, new ConstructClassDefinition
                          (constructSymbol.lineNumber, cast(string)name.value, classDefScope));
      return ConstructResult(null);
}));
mixin(formattedString!(generatePatternConstructCode)(ConstructName("new"), "(className symbol)", q{
      auto classDef = processor.lookupSymbol!ConstructClassDefinition(className);
      return const ConstructResult(new ConstructClass(constructSymbol.lineNumber, classDef));
}));

mixin(formattedString!(generatePatternConstructCode)(ConstructName("-", "Negative"), "(value integer)", q{
      return const ConstructResult(value.createNegativeVersion(constructSymbol.lineNumber));
}));
      
mixin(formattedString!(generateConstructCode)
      (ConstructName(".", "Dot"),
       [
        ConstructPatternHandler.fromPattern("(this optionalValue, _ raw \"isPresent\")",q{
            // TODO: return a side affect that affects the type of the symbol
            //       that was passed in.
            return const ConstructResult((this_.value is null) ? ConstructBool.false_ : ConstructBool.true_);
          }),
	ConstructPatternHandler.fromPattern("(this optionalValue, _ raw \"value\")",q{
	    return const ConstructResult(this_.value);
	  }),
        //ConstructPatternHandler.fromPattern("(symbol symbol)", "return symbol;"),
        //ConstructPatternHandler.fromPattern("(string_ string)", "return new ConstructSymbol(string_.lineNumber, string_.toUtf8());")

        ]));
/*
mixin(formattedString!(generatePatternConstructCode)(ConstructName(".", "Dot"), "(instance class)", "()", q{
      // Execute one construct inside the class definition scope
      processor.pushScope(instance.classDef.scope_);
      scope(exit) { processor.popScope(); }

      //result = consumeValueAlreadyCheckedIndex
      //(DefaultPrecedenceConsumer.instance, objects, &index).unconst;
      //!!

    }));
*/
/+
mixin(formattedString!(generatePatternConstructCode)(ConstructName(".", "Dot"), "(instance class)", "(member symbol)", q{
      /*
      foreach(entry; instance.classDef.scope_.symbols.byKeyValue) {
        logDev("class '%s' entry '%s:%s'", instance.classDef.name, entry.key, entry.value);
      }
      */
      auto symbolEntryList = instance.classDef.scope_.symbols.get(member.value, SymbolEntryList());
      if(!symbolEntryList.first) {
        throw processor.semanticError(member.lineNumber, format("class '%s' does not have member '%s'", instance.classDef.name, member.value));
      }
      if(symbolEntryList.moreCount > 0) {
        throw new Exception("multiple class members with the same symbol");
      }
      auto memberObject = symbolEntryList.first;
      if(auto constructDefinition = memberObject.tryAsConstructDefinition) {
        // Execute the construct, however, need to add all the class members
        // to the scope, along with the 'this' parameter

        throw imp("construct class member");
      }
      throw imp(format("%s class member", An(memberObject.typeName)));
}));
+/

//
// Debug Constructs
//
mixin(formattedString!(generatePatternConstructCode)(ConstructName("dumpScopeStack"), "()", q{
      processor.printScopeStack();
      return ConstructResult(null);
}));

//
// File System Operations
//

// TODO: I want to make a filePath type. Maybe it will inherit from the string type?
//
mixin(formattedString!(generatePatternConstructCode)(ConstructName("currentSourceFileString"), "()", q{
      // TODO: Instead of returning ConstructUtf8, return a FilePath type or something
      return const ConstructResult(new ConstructUtf8(constructSymbol.lineNumber, processor.currentFile.absoluteName));
}));
/*
mixin(formattedString!(generatePatternConstructCode)(ConstructName("buildPath"), "(pathParts oneOrMore string)", q{
      
      throw imp("buildPath");
}));
mixin(formattedString!(generatePatternConstructCode)(ConstructName("directoryOf"), "(filePath string)", q{
      enum pathSeparator = '\\';
      auto filePathUtf8 = filePath.toUtf8;
      if(filePathUtf8.length > 0) {
        for(auto i = filePathUtf8.length-1; ;i--) {
          if(filePathUtf8[i] == pathSeparator) {
            return const ConstructResult(new ConstructUtf8(constructSymbol.lineNumber, filePathUtf8[0..i]));
          }
          if(i == 0) {
            break;
          }
        }
      }
      return const ConstructResult(new ConstructUtf8(constructSymbol.lineNumber, ""));
}));
mixin(formattedString!(generatePatternConstructCode)(ConstructName("currentSourceFile"), "()", q{
      // TODO: Instead of returning ConstructUtf8, return a FilePath type or something
      return const ConstructResult(new ConstructUtf8(constructSymbol.lineNumber, processor.currentFile.absoluteName));
}));
mixin(formattedString!(generatePatternConstructCode)(ConstructName("mkdir"), "(filePath string)", q{
      throw imp("mkdir");
}));
*/
