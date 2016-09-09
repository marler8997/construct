module construct.primitives;

import core.stdc.stdlib : malloc;
import std.format : sformat, format, formattedWrite;
import std.array  : replace;
import std.conv   : to;

import construct.util;
import construct.logging;
import construct.parserCore;
import construct.backendCore;
import construct.patterns;
import construct.processor;

import backend : loadConstructBackend;

//
// Basic Primitives with no need for a pattern because they have no parameters
//
// These primitives are required to create other patterns.
// Their patterns cannot be created from parsing construct pattern string
// because these patterns are required to create other patterns.
//
const(ConstructObject) falseHandler(ConstructProcessor* processor, const(ConstructDefinition) def, const(ConstructSymbol) constructSymbol,
				    const(PatternNode)[] patternNodes, const(ConstructObject)[] objects) pure
{
  return new ConstructBool(constructSymbol.lineNumber, false);
}
immutable falseConstructDefinition = new immutable InternalPatternConstructDefinition
  ("false", [immutable PatternHandler(&falseHandler, null)], null, ConstructAttributes.init, null);
const(ConstructObject) trueHandler(ConstructProcessor* processor, const(ConstructDefinition) def, const(ConstructSymbol) constructSymbol,
				    const(PatternNode)[] patternNodes, const(ConstructObject)[] objects) pure
{
  return new ConstructBool(constructSymbol.lineNumber, true);
}
immutable trueConstructDefinition = new immutable InternalPatternConstructDefinition
  ("true", [immutable PatternHandler(&trueHandler, null)], null, ConstructAttributes.init, null);
const(ConstructObject) nullHandler(ConstructProcessor* processor, const(ConstructDefinition) def, const(ConstructSymbol) constructSymbol,
				    const(PatternNode)[] patternNodes, const(ConstructObject)[] objects) pure
{
  return new ConstructNull(constructSymbol.lineNumber);
}
immutable nullConstructDefinition = new immutable InternalPatternConstructDefinition
  ("null", [immutable PatternHandler(&nullHandler, null)], null, ConstructAttributes.init, null);

immutable oneTypePattern = [immutable PatternNode("type", CountType.one, PrimitiveType.type)];
struct ListOfConstruct
{
  private static const(ConstructObject) handler(ConstructProcessor* processor, const(ConstructDefinition) def,
                                                const(ConstructSymbol) constructSymbol, const(PatternNode)[] patternNodes,
						const(ConstructObject)[] objects) pure
  {
    assert(objects.length == 1);
    auto type = objects[0].tryAsConstructType;
    assert(type);
    return new ConstructTypedListType(constructSymbol.lineNumber, type);
  }
  static immutable definition = new immutable InternalPatternConstructDefinition
    ("listOf", [immutable PatternHandler(&handler, oneTypePattern)], null, ConstructAttributes.init, null);
}

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
    string typeName = null;
    if(node.countType == CountType.optional) {
      typeName = node.type.internalValueClassIfOptional;
    } else {
      typeName = node.type.internalValueClassIfRequired;
    }
    if(typeName && node.name != "_") {
      count++;
    }
  }
  return count;
}
void genConstructHandlerSignature(PureStringSink sink, const(char)[] funcName, const(Pattern) pattern) pure
{
  sink("private static const(ConstructObject) ");
  sink(funcName);
  sink("(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol");

  if(pattern.opType !is null) {
    formattedWrite(sink, ", const(%s) this_", pattern.opType.internalValueClassIfRequired);
  }

  foreach(node; pattern.nodes) {
    string typeName = null;
    if(node.countType == CountType.optional) {
      typeName = node.type.internalValueClassIfOptional;
    } else {
      typeName = node.type.internalValueClassIfRequired;
    }
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
void generateHandlerThunkFunction(PureStringSink sink, const(char)[] linePrefix, const(char)[] name,
                                  const(char)[] handlerFunctionName, const(Pattern) pattern) pure
{
  sink(linePrefix);
  sink("private static const(ConstructObject) ");
  sink(name);
  sink("(ConstructProcessor* processor");
  sink(", const(ConstructDefinition) constructDefinition");
  sink(", const(ConstructSymbol) constructSymbol");
  sink(", const(PatternNode)[] patternNodes");
  sink(", const(ConstructObject)[] args)\n");
  sink(linePrefix);
  sink("{\n");

  sink(linePrefix);
  sink("    assert(args.length == ");
  formattedWrite(sink, "%s", patternNodeArgCount(pattern));
  sink(");\n");

  sink(linePrefix);
  sink("    return ");
  sink(handlerFunctionName);
  sink("(processor, constructSymbol");

  size_t argIndex = 0;
  if(pattern.opType !is null) {
    sink(",\n");
    sink(linePrefix);
    formattedWrite(sink, "        args[%s] ? args[%s].enforceAs!%s : null",
		   argIndex, argIndex, pattern.opType.internalValueClassIfRequired);
    argIndex++;
  }

  foreach(node; pattern.nodes) {
    string typeName = null;
    if(node.countType == CountType.optional) {
      typeName = node.type.internalValueClassIfOptional;
    } else {
      typeName = node.type.internalValueClassIfRequired;
    }
    if(typeName && node.name != "_") {
      sink(",\n");
      sink(linePrefix);
      formattedWrite(sink, "        args[%s] ? args[%s].enforceAs!%s : null", argIndex, argIndex, typeName);
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
      formattedWrite(sink, "immutable PatternHandler(&thunkToHandler_%s_%s, pattern_%s_%s.nodes)",
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
          formattedWrite(sink, "immutable PatternHandler(&thunkToHandler_%s_%s, pattern_%s_%s.nodes)",
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
				  (q{immutable Pattern(null, [immutable PatternNode("patternList", CountType.one, PrimitiveType.list)])})(q{
    return new ConstructPattern(patternList.lineNumber, processPattern(processor, definition, patternList));
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
mixin(formattedString!(generatePatternConstructCode)(ConstructName("let"), "(name symbol, value, break_ optional constructBreak)", q{
  processor.addSymbol(name.value, value);
  return (break_ is null) ? value : null;
}));
mixin(formattedString!(generatePatternConstructCode)(ConstructName("set"), "(name symbol, value, break_ optional constructBreak)", q{
  processor.setSymbol(name.lineNumber, name.value, value);
  return (break_ is null) ? value : null;
}));
mixin(formattedString!(generatePatternConstructCode)(ConstructName("letSet"), "(name symbol, value, break_ optional constructBreak)", q{
  processor.letSetSymbol(name.value, value);
  return (break_ is null) ? value : null;
}));
mixin(formattedString!(generatePatternConstructCode)(ConstructName("import"), `(isRelative optional "relative", name string)`, q{
  //processor.findAndImport(constructSymbol.lineNumber, name.toUtf8);
  processor.import_(constructSymbol.lineNumber, isRelative !is null, name.toUtf8);
  return null;
}));
mixin(formattedString!(generatePatternConstructCode)(ConstructName("exec"), "(code constructBlock)", q{
  return processor.processBlock(code.objects);
}));

mixin(formattedString!(generateConstructCode)
      (ConstructName("defcon"), [ConstructPatternHandler.fromPattern("(name symbol, patternList list, _ constructBreak)", q{
    ConstructAttributes attributes;

    auto processedPattern = processPattern(processor, definition, patternList);
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
}), ConstructPatternHandler.fromPattern("(name symbol, patternList list, implementation constructBlock)", q{

    ConstructAttributes attributes;
    auto processedPattern = processPattern(processor, definition, patternList);

    //
    // Check if current scope has a construct with the same name
    //
    auto existing = processor.tryLookupSymbolAndScope(name.value);
    if(existing.entryList.first) {

      if(existing.symbols is processor.currentScope.symbols) {
        throw imp("add new construct pattern and block to existin one");
        //existing.addBlockConstruct(constructSymbol.lineNumber, processor.currentFile.name, processedPattern, implementation);
      } else {
        throw imp("combine old add new construct to current scope");
      }

    } else {
      immutable(PatternHandler)[] patternHandlerArray =
        [immutable PatternHandler(&handleConstructWithBlock, processedPattern.nodes.immutable_)];


      immutable(PatternHandler)[] noOpPatternHandlers;
      immutable(OpPatternHandlers)[] opPatternHandlers;
      if(processedPattern.opType) {
        noOpPatternHandlers = null;
        opPatternHandlers   = [immutable OpPatternHandlers(processedPattern.opType, patternHandlerArray)];
      } else {
        noOpPatternHandlers = patternHandlerArray;
        opPatternHandlers   = null;
      }
      processor.addSymbol(name.value, new immutable ConstructWithBlockDefinition
                          (cast(string)name.value, noOpPatternHandlers, opPatternHandlers,
                           constructSymbol.lineNumber, cast(string)processor.currentFile.relativeName,
                           attributes, null, cast(immutable(ConstructBlock))implementation));
    }
    return null;
})]));

mixin(formattedString!(generatePatternConstructCode)
      (ConstructName("if"), "(condition predicate, trueCase constructBlock)", q{
  if(condition.isTrue) {
    processor.pushScope(trueCase.lineNumber, ScopeType.block, true);
    scope(exit) { processor.popScope(); }
    return processor.processBlock(trueCase.objects);
  } else {
    return null;
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
//
// Operators
//
mixin(formattedString!(generatePatternConstructCode)(ConstructName("+", "Plus"), "(this number, right number)", q{
      return this_.add(right);
}));
mixin(formattedString!(generatePatternConstructCode)(ConstructName("*", "Multiply"), "(this number, right number)", q{
      return this_.multiply(right);
}));
mixin(formattedString!(generatePatternConstructCode)(ConstructName("==", "Equals"), "(this, right)", q{
      return new ConstructBool(constructSymbol.lineNumber, this_.equals(right, false));
}));


mixin(formattedString!(generatePatternConstructCode)(ConstructName("assert"), "(predicate bool)", q{
      if(!predicate.value) {
	throw new ConstructAssertException(constructSymbol.lineNumber, processor, "assertion failed");
      }
      return null;
}));
mixin(formattedString!(generatePatternConstructCode)(ConstructName("throw"), "(message string)", q{
    throw new ConstructThrownException(constructSymbol.lineNumber, processor, message.toUtf8());
}));


mixin(formattedString!(generatePatternConstructCode)(ConstructName("not"), "(value predicate)", q{
      return new ConstructBool(constructSymbol.lineNumber, !value.isTrue);
}));
// NOTE: should probably be a "dotted" keyword operator
mixin(formattedString!(generatePatternConstructCode)(ConstructName("itemType"), "(this list)", q{
    auto typeObject = this_.itemType.definition.typeObject;
    if(!typeObject) {
      throw imp(format("PrimitiveType %s has no type object configured", this_.itemType));
    }
    return typeObject;
}));

mixin(formattedString!(generateConstructCode)
      (ConstructName("toSymbol"), [ConstructPatternHandler.fromPattern("(symbol symbol)", "return symbol;"),
				   ConstructPatternHandler.fromPattern("(string_ string)", "return new ConstructSymbol(string_.lineNumber, string_.toUtf8());")]));

//
// String Operations
//
mixin(formattedString!(generatePatternConstructCode)(ConstructName("byteLength"), "(this string, type optional type)", q{
      auto primitiveType = (type is null) ? PrimitiveTypeEnum.utf8 : type.asPrimitive;
      return new ConstructUint(constructSymbol.lineNumber, this_.stringByteLength(primitiveType));
}));
mixin(formattedString!(generateConstructCode)
      (ConstructName("strcpy"), [ConstructPatternHandler.fromPattern("(dest pointer, src string, type type)",q{
    if(type.asPrimitive == PrimitiveTypeEnum.utf8) {
      auto srcString = src.toUtf8;
      (cast(char*)dest.pointer)[0..srcString.length] = srcString[];
    } else {
      throw imp(format("strcpy pointer (type=%s)", type));
    }
    return null;
}), ConstructPatternHandler.fromPattern("(dest string, src string, type type)", q{
    throw imp("strcpy string");
})]));



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
      immutable PatternNode("catchClauses", CountType.many, new immutable ConstructPattern(0, Pattern
			    ([immutable PatternNode(null, CountType.one, new immutable KeywordMatcher("catch")),
			      immutable PatternNode("catchBlock", CountType.one, Matcher.block)]))),
      immutable PatternNode("finallyClause", CountType.optional, new immutable ConstructPattern(0, Pattern
			    ([immutable PatternNode(null, CountType.one, new immutable KeywordMatcher("finally")),
			      immutable PatternNode("finallyBlock", CountType.one, Matcher.block)])))]);
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
const(ConstructObject) tryConstructHandler(ConstructProcessor* processor,
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
immutable foreachConstructDefinition = new immutable FunctionConstructDefinition
  ("foreach", __LINE__, __FILE__, ConstructAttributes.init, null, &foreachConstructHandler);
const(ConstructObject) foreachConstructHandler(ConstructProcessor* processor,
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
    auto object = processor.consumeValueAlreadyCheckedIndex(definition, forEachArgs.items, &listIndex);
    if(object is null) {
      throw processor.semanticError(constructSymbol.lineNumber, "foreach iteration list got a void expression");
    }
    if(listIndex < forEachArgs.items.length) {
      throw processor.semanticError(constructSymbol.lineNumber, "foreach iteration list has too many items");
    }

    list = object.tryAsConstructList.unconst;
    if(!list) {
      throw processor.semanticError(constructSymbol.lineNumber, format
                                    ("foreach requires a list but got %s", An(object.typeName)));
    }
  }

  if(list.items.length > 0) {
    processor.pushScope(forEachCode.lineNumber, ScopeType.block, true);
    ConstructUint indexObject = null;
    scope(exit) { processor.popScope(); }
    if(indexVar) {
      indexObject = new ConstructUint(forEachArgs.lineNumber, 0);
      processor.addSymbol(indexVar, indexObject);
    }
    processor.addSymbol(itemVar, list.items[0]);

    auto firstResult = processor.processBlock(forEachCode.objects);
    if(firstResult) {
      if(auto return_ = firstResult.tryAsConstructReturn) {
        return firstResult;
      }
    }
    foreach(listObject; list.items[1..$]) {
      processor.setSymbol(constructSymbol.lineNumber, itemVar, listObject);
      if(indexObject) {
        indexObject.value++;
      }
      auto result = processor.processBlock(forEachCode.objects);
      if(result) {
        if(auto return_ = result.tryAsConstructReturn) {
          return result;
        }
      }
    }
  }

  return null;
}

//
// Modes
//
//(name symbol, handlerClause pattern(_ "handler", source symbol, result symbol, handlerBlock constructBlock))
mixin(formattedString!(generatePatternConstructCode)(ConstructName("createMode"), q{
      (name symbol, source symbol, result symbol, handlerBlock constructBlock)
}, q{
      throw imp("createMode");
      //return new ConstructPointer(constructSymbol.lineNumber, malloc(size.value));
}));

//
// Memory Primitives
//
mixin(formattedString!(generatePatternConstructCode)(ConstructName("malloc"), "(size uint)", q{
      return new ConstructPointer(constructSymbol.lineNumber, malloc(size.value));
}));

//
// Classes
//
//(name symbol , inheritClause optional pattern(_ :, baseClass symbol)
mixin(formattedString!(generatePatternConstructCode)(ConstructName("class"), "(name symbol, definition constructBlock)", q{

      Scope classDefScope;
      {
        processor.pushScope(definition.lineNumber, ScopeType.classDef, true);
        scope(exit) { processor.popScope(); }
        auto result = processor.processBlock(definition.objects).unconst;
        if(result !is null) {
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
      throw imp("adding class members to outer scope as dot operator constructs");

      processor.addSymbol(name.value, new ConstructClassDefinition
                          (constructSymbol.lineNumber, cast(string)name.value, classDefScope));
      return null;
}));
mixin(formattedString!(generatePatternConstructCode)(ConstructName("new"), "(className symbol)", q{
      auto classDef = processor.lookupSymbol!ConstructClassDefinition(className);
      return new ConstructClass(constructSymbol.lineNumber, classDef);
}));

/*
mixin(formattedString!(generateConstructCode)
      (ConstructName("."), [ConstructPatternHandler.fromPattern("(symbol symbol)", "return symbol;"),
			    ConstructPatternHandler.fromPattern("(string_ string)", "return new ConstructSymbol(string_.lineNumber, string_.toUtf8());")]));
*/
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
      return null;
}));
