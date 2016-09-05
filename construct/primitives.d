module construct.primitives;

import core.stdc.stdlib : malloc;
import std.format : sformat, format, formattedWrite;
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
const(ConstructObject) falseHandler(ConstructProcessor* processor, const(ConstructDefinition) def, const(ConstructSymbol) constructSymbol, const(ConstructObject)[] objects) pure
{
  return new ConstructBool(constructSymbol.lineNumber, false);
}
immutable falseConstructDefinition = new immutable InternalPatternConstructDefinition
  ("false", OpParam(), [immutable PatternHandler(null, &falseHandler)], ConstructAttributes.init, null);
const(ConstructObject) trueHandler(ConstructProcessor* processor, const(ConstructDefinition) def, const(ConstructSymbol) constructSymbol, const(ConstructObject)[] objects) pure
{
  return new ConstructBool(constructSymbol.lineNumber, true);
}
immutable trueConstructDefinition = new immutable InternalPatternConstructDefinition
  ("true", OpParam(), [immutable PatternHandler(null, &trueHandler)], ConstructAttributes.init, null);
const(ConstructObject) nullHandler(ConstructProcessor* processor, const(ConstructDefinition) def, const(ConstructSymbol) constructSymbol, const(ConstructObject)[] objects) pure
{
  return new ConstructNull(constructSymbol.lineNumber);
}
immutable nullConstructDefinition = new immutable InternalPatternConstructDefinition
  ("null", OpParam(), [immutable PatternHandler(null, &nullHandler)], ConstructAttributes.init, null);
string generatePrimitiveTypeConstruct(PrimitiveTypeEnum type) pure
{
  auto name = type.definition.name;
  auto enumName = type.to!string;
  return format(q{
    const(ConstructObject) %sHandler(ConstructProcessor* processor, const(ConstructDefinition) def, const(ConstructSymbol) constructSymbol, const(ConstructObject)[] objects) pure {
      return new PrimitiveType(constructSymbol.lineNumber, PrimitiveTypeEnum.%s);
    }
    immutable %sConstructDefinition = new immutable InternalPatternConstructDefinition
      ("%s", OpParam(), [immutable PatternHandler(null, &%sHandler)], ConstructAttributes.init, null);
    }, name, enumName, name, name, name);
}
mixin(generatePrimitiveTypeConstruct(PrimitiveTypeEnum.anything));
mixin(generatePrimitiveTypeConstruct(PrimitiveTypeEnum.constructBreak));
mixin(generatePrimitiveTypeConstruct(PrimitiveTypeEnum.list));
mixin(generatePrimitiveTypeConstruct(PrimitiveTypeEnum.bool_));
mixin(generatePrimitiveTypeConstruct(PrimitiveTypeEnum.number));
mixin(generatePrimitiveTypeConstruct(PrimitiveTypeEnum.uint_));
mixin(generatePrimitiveTypeConstruct(PrimitiveTypeEnum.symbol));
mixin(generatePrimitiveTypeConstruct(PrimitiveTypeEnum.string));
mixin(generatePrimitiveTypeConstruct(PrimitiveTypeEnum.pointer));
mixin(generatePrimitiveTypeConstruct(PrimitiveTypeEnum.type));
mixin(generatePrimitiveTypeConstruct(PrimitiveTypeEnum.constructBlock));
mixin(generatePrimitiveTypeConstruct(PrimitiveTypeEnum.class_));

immutable oneTypePattern = [immutable PatternNode("type", CountType.one, Matcher.type)];
struct ListOfConstruct
{
  private static const(ConstructObject) handler(ConstructProcessor* processor, const(ConstructDefinition) def,
                                                const(ConstructSymbol) constructSymbol, const(ConstructObject)[] objects) pure
  {
    assert(objects.length == 1);
    auto type = objects[0].tryAsConstructType;
    assert(type);
    return new ConstructTypedListType(constructSymbol.lineNumber, type);
  }
  static immutable definition = new immutable InternalPatternConstructDefinition
    ("listOf", OpParam(), [immutable PatternHandler(oneTypePattern, &handler)], ConstructAttributes.init, null);
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
size_t patternNodeArgCount(const(OpParam) opParam, const(PatternNode)[] patternNodes) pure
{
  size_t count = 0;
  if(opParam.name && opParam.name != "nameless") {
    count++;
  }
  foreach(node; patternNodes) {
    string typeName = null;
    if(node.countType == CountType.optional) {
      typeName = node.matcher.processorOptionalValueType;
    } else {
      typeName = node.matcher.processorValueType;
    }

    if(typeName && typeName != "nameless") {
      count++;
    }
  }
  return count;
}
void genConstructHandlerSignature(PureStringSink sink, const(char)[] funcName,
                                  const(OpParam) opParam, const(PatternNode)[] patternNodes) pure
{
  sink("private static const(ConstructObject) ");
  sink(funcName);
  sink("(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol");

  if(opParam.name && opParam.name != "nameless") {
    formattedWrite(sink, ", const(%s) %s", opParam.matcher.processorValueType, opParam.name);
  }

  foreach(node; patternNodes) {
    string typeName = null;
    if(node.countType == CountType.optional) {
      typeName = node.matcher.processorOptionalValueType;
    } else {
      typeName = node.matcher.processorValueType;
    }

    if(typeName && typeName != "nameless") {
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
                                  const(char)[] handlerFunctionName, const(OpParam) opParam, const(PatternNode)[] patternNodes) pure
{
  sink(linePrefix);
  sink("private static const(ConstructObject) ");
  sink(name);
  sink("(ConstructProcessor* processor");
  sink(", const(ConstructDefinition) constructDefinition");
  sink(", const(ConstructSymbol) constructSymbol");
  sink(", const(ConstructObject)[] args)\n");
  sink(linePrefix);
  sink("{\n");

  sink(linePrefix);
  sink("    assert(args.length == ");
  formattedWrite(sink, "%s", patternNodeArgCount(opParam, patternNodes));
  sink(");\n");

  sink(linePrefix);
  sink("    return ");
  sink(handlerFunctionName);
  sink("(processor, constructSymbol");

  size_t argIndex = 0;
  if(opParam.name && opParam.name != "nameless") {
    sink(",\n");
    sink(linePrefix);
    formattedWrite(sink, "        args[%s] ? args[%s].enforceAs!%s : null", argIndex, argIndex, opParam.matcher.processorValueType);
    argIndex++;
  }

  foreach(node; patternNodes) {
    string typeName = null;
    if(node.countType == CountType.optional) {
      typeName = node.matcher.processorOptionalValueType;
    } else {
      typeName = node.matcher.processorValueType;
    }

    if(typeName && typeName != "nameless") {
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

struct OpParamPattern
{
  string code;
  immutable(OpParam) param;
  this(string code) immutable
  {
    this.code = code;
    if(code) {
      auto patternNodes = compileBasicPattern(code);
      if(patternNodes.length == 0) {
        throw new Exception(format("The OpParam code '%s' cannot be empty", code));
      }
      if(patternNodes.length > 1) {
        throw new Exception(format("The OpParam code '%s' can only have 1 parameter, but it has %s", code, patternNodes.length));
      }
      auto patternNode = patternNodes[0];
      if(patternNode.countType != CountType.one) {
        throw new Exception(format("The code '%s' cannot have a count type of '%s'", code, patternNode.countType));
      }
      this.param = OpParam(patternNode.name, patternNode.matcher);
    }
  }
}
struct ConstructPatternHandler
{
  string patternCode;
  immutable(PatternNode)[] compiledPattern;
  string handlerCode;
  static ConstructPatternHandler fromDataStructure(string pattern)(string handlerCode)
  {
    mixin("static immutable PatternNode[] patternDataStructure = "~pattern~";");
    return ConstructPatternHandler(pattern, patternDataStructure, handlerCode);
  }
  static ConstructPatternHandler fromPattern(string patternCode, string handlerCode)
  {
    return ConstructPatternHandler("compileBasicPattern(\""~patternCode~"\")", compileBasicPattern(patternCode), handlerCode);
  }
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
                           const(OpParamPattern) opParam, ConstructPatternHandler[] patternHandlers)
{
  auto constructName = constructSymbolAndName.name;
  auto constructNameUpperCase = constructName.firstLetterToUpper();

  sink("struct ");
  sink(constructNameUpperCase);
  sink("Construct\n");
  sink("{\n");

  foreach(i, patternHandler; patternHandlers) {
    formattedWrite(sink, "    private immutable pattern%s = %s;", i, patternHandler.patternCode);
  }

  char[64] handlerFuncNameBuffer;
  char[64] thunkFuncNameBuffer;
  foreach(i, patternHandler; patternHandlers) {
    auto handlerFuncName = sformat(handlerFuncNameBuffer, "handler%s", i);
    auto thunkFuncName   = sformat(thunkFuncNameBuffer  , "thunkToHandler%s", i);

    sink("\n    ");
    genConstructHandlerSignature(sink, handlerFuncName, opParam.param, patternHandler.compiledPattern);
    sink("\n");
    sink("    {");
    sink(patternHandler.handlerCode);
    sink("    }\n");

    //
    // Generate Thunk Function
    //
    generateHandlerThunkFunction(sink, "    ", thunkFuncName, handlerFuncName,
                                 opParam.param, patternHandler.compiledPattern);
  }
  sink("    static immutable definition = new immutable InternalPatternConstructDefinition\n");
  sink("        (");
  formattedWrite(sink, "\"%s\", ", constructSymbolAndName.symbol);
  if(opParam.code) {
    formattedWrite(sink, "(immutable OpParamPattern(\"%s\")).param", opParam.code);
  } else {
    sink("OpParam()");
  }
  sink(", [");
  foreach(i, patternHandler; patternHandlers) {
    if(i > 0) {
      sink(",\n          ");
    }
    formattedWrite(sink, "immutable PatternHandler(pattern%s, &thunkToHandler%s)", i, i);
  }
  sink("], ConstructAttributes.init, null);\n");
  sink("}\n");
}

mixin(formattedString!(generateConstructCode)
      (ConstructName("pattern"), OpParamPattern(), [ConstructPatternHandler.fromDataStructure!
                                                    (q{[immutable PatternNode("patternList", CountType.one, Matcher.list)]})(q{
    return new ConstructPattern(patternList.lineNumber, processPattern(processor, definition, patternList));
})]));

//
// Functions to generate D code for constructs
// The constructs that use these functions require the constructs
// that are defined above this in order to create construt patterns.
//
void generatePatternConstructCode(PureStringSink sink, const(ConstructName) symbolAndName, string opParamCode, string patternCode, string handlerCode)
{
  generateConstructCode(sink, symbolAndName, immutable OpParamPattern(opParamCode), [ConstructPatternHandler.fromPattern(patternCode, handlerCode)]);
}

//
// More extended primitives
// These primitives have patterns that rely on the basic primitives
mixin(formattedString!(generatePatternConstructCode)(ConstructName("let"), null, "(name symbol, value, break_ optional constructBreak)", q{
  processor.addSymbol(name.value, value);
  return (break_ is null) ? value : null;
}));
mixin(formattedString!(generatePatternConstructCode)(ConstructName("set"), null, "(name symbol, value, break_ optional constructBreak)", q{
  processor.setSymbol(name.lineNumber, name.value, value);
  return (break_ is null) ? value : null;
}));
mixin(formattedString!(generatePatternConstructCode)(ConstructName("letSet"), null, "(name symbol, value, break_ optional constructBreak)", q{
  processor.letSetSymbol(name.value, value);
  return (break_ is null) ? value : null;
}));
mixin(formattedString!(generatePatternConstructCode)(ConstructName("import"), null, "(name string)", q{
  processor.findAndImport(constructSymbol.lineNumber, name.toUtf8);
  return null;
}));

mixin(formattedString!(generatePatternConstructCode)(ConstructName("exec"), null, "(code constructBlock)", q{
  return processor.processBlock(code.objects);
}));

mixin(formattedString!(generateConstructCode)
      (ConstructName("defcon"), OpParamPattern(), [ConstructPatternHandler.fromPattern("(name symbol, patternList list, nameless constructBreak)", q{
    ConstructAttributes attributes;

    //const(ConstructParam)[] requiredParams = parseRequiredParams(processor, pattern);
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
      processor.addSymbol(name.value, new immutable ConstructWithBlockDefinition
                          (cast(string)name.value, processedPattern, constructSymbol.lineNumber, cast(string)processor.currentFile.name, attributes, null,
                           cast(immutable(ConstructBlock))implementation));
    }
    return null;
})]));


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

//
// Operators
//
mixin(formattedString!(generatePatternConstructCode)(ConstructName("+", "Plus"), "(left number)", "(right number)", q{
      return left.add(right);
}));
mixin(formattedString!(generatePatternConstructCode)(ConstructName("*", "Multiply"), "(left number)", "(right number)", q{
      return left.multiply(right);
}));
mixin(formattedString!(generatePatternConstructCode)(ConstructName("==", "Equals"), "(left)", "(right)", q{
      return new ConstructBool(constructSymbol.lineNumber, left.equals(right, false));
}));


mixin(formattedString!(generatePatternConstructCode)(ConstructName("assert"), null, "(predicate bool)", q{
      if(!predicate.value) {
	throw new ConstructAssertException(constructSymbol.lineNumber, processor, "assertion failed");
      }
      return null;
}));

mixin(formattedString!(generatePatternConstructCode)(ConstructName("not"), null, "(value bool)", q{
      return new ConstructBool(constructSymbol.lineNumber, !value.value);
}));
// NOTE: should probably be a "dotted" keyword operator
mixin(formattedString!(generatePatternConstructCode)(ConstructName("itemType"), "(list list)", "()", q{
      return new PrimitiveType(constructSymbol.lineNumber, list.itemType);
}));

mixin(formattedString!(generateConstructCode)
      (ConstructName("toSymbol"), OpParamPattern(), [ConstructPatternHandler.fromPattern("(symbol symbol)", "return symbol;"),
						     ConstructPatternHandler.fromPattern("(string_ string)", "return new ConstructSymbol(string_.lineNumber, string_.toUtf8());")]));


//
// String Operations
//
mixin(formattedString!(generatePatternConstructCode)(ConstructName("byteLength"), "(string_ string)", "(type optional type)", q{
      auto primitiveType = (type is null) ? PrimitiveTypeEnum.utf8 : type.asPrimitive;
      return new ConstructUint(constructSymbol.lineNumber, string_.stringByteLength(primitiveType));
}));
mixin(formattedString!(generateConstructCode)
      (ConstructName("strcpy"), OpParamPattern(), [ConstructPatternHandler.fromPattern("(dest pointer, src string, type type)",q{
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
 catchClauses many pattern(nameless "catch", catchBlock constructBlock),
 finallyClause optional pattern(nameless "finally", finallyBlock constructBlock))`);
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

mixin(formattedString!(generatePatternConstructCode)(ConstructName("addSymbolsToCaller"), null, "(code constructBlock)", q{
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
mixin(formattedString!(generatePatternConstructCode)(ConstructName("defmode"), null, "(name symbol, source symbol, result symbol, handlerBlock constructBlock)", q{
      throw imp("defmode");
      //return new ConstructPointer(constructSymbol.lineNumber, malloc(size.value));
}));

//
// Memory Primitives
//
mixin(formattedString!(generatePatternConstructCode)(ConstructName("malloc"), null, "(size uint)", q{
      return new ConstructPointer(constructSymbol.lineNumber, malloc(size.value));
}));

//
// Classes
//
//(name symbol , inheritClause optional pattern(nameless :, baseClass symbol)
mixin(formattedString!(generatePatternConstructCode)(ConstructName("class"), null, "(name symbol, definition constructBlock)", q{

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
mixin(formattedString!(generatePatternConstructCode)(ConstructName("new"), null, "(className symbol)", q{
      auto classDef = processor.lookupSymbol!ConstructClassDefinition(className);
      return new ConstructClass(constructSymbol.lineNumber, classDef);
}));
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
mixin(formattedString!(generatePatternConstructCode)(ConstructName("dumpScopeStack"), null, "()", q{
      processor.printScopeStack();
      return null;
}));
