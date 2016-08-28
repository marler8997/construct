module construct.primitives;

import std.format : sformat, format, formattedWrite;
import std.conv   : to;

import construct.logging;
import construct.ir;
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
  ([immutable PatternHandler(emptyPattern, &falseHandler)], ConstructAttributes.init, null);
const(ConstructObject) trueHandler(ConstructProcessor* processor, const(ConstructDefinition) def, const(ConstructSymbol) constructSymbol, const(ConstructObject)[] objects) pure
{
  return new ConstructBool(constructSymbol.lineNumber, true);
}
immutable trueConstructDefinition = new immutable InternalPatternConstructDefinition
  ([immutable PatternHandler(emptyPattern, &trueHandler)], ConstructAttributes.init, null);
const(ConstructObject) nullHandler(ConstructProcessor* processor, const(ConstructDefinition) def, const(ConstructSymbol) constructSymbol, const(ConstructObject)[] objects) pure
{
  return new ConstructNull(constructSymbol.lineNumber);
}
immutable nullConstructDefinition = new immutable InternalPatternConstructDefinition
  ([immutable PatternHandler(emptyPattern, &nullHandler)], ConstructAttributes.init, null);
string generatePrimitiveTypeConstruct(PrimitiveTypeEnum type) pure
{
  auto name = type.definition.name;
  auto enumName = type.to!string;
  return format(q{
    const(ConstructObject) %sHandler(ConstructProcessor* processor, const(ConstructDefinition) def, const(ConstructSymbol) constructSymbol, const(ConstructObject)[] objects) pure {
      return new PrimitiveType(constructSymbol.lineNumber, PrimitiveTypeEnum.%s);
    }
    immutable %sConstructDefinition = new immutable InternalPatternConstructDefinition
      ([immutable PatternHandler(emptyPattern, &%sHandler)], ConstructAttributes.init, null);
    }, name, enumName, name, name);
}
mixin(generatePrimitiveTypeConstruct(PrimitiveTypeEnum.anything));
mixin(generatePrimitiveTypeConstruct(PrimitiveTypeEnum.constructBreak));
mixin(generatePrimitiveTypeConstruct(PrimitiveTypeEnum.list));
mixin(generatePrimitiveTypeConstruct(PrimitiveTypeEnum.bool_));
mixin(generatePrimitiveTypeConstruct(PrimitiveTypeEnum.symbol));
mixin(generatePrimitiveTypeConstruct(PrimitiveTypeEnum.string));
mixin(generatePrimitiveTypeConstruct(PrimitiveTypeEnum.constructBlock));


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
string formattedString(alias formatter, size_t maxLength = 4096, Args...)(Args args)
{
  StringBuilder!maxLength builder;
  formatter(&builder.append, args);
  return builder.createString();
}
size_t patternNodeArgCount(Pattern pattern) pure
{
  size_t count = 0;
  foreach(node; pattern.nodes) {
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
void genConstructHandlerSignature(PureStringSink sink, const(char)[] funcName, Pattern pattern) pure
{
  sink("private static const(ConstructObject) ");
  sink(funcName);
  sink("(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol");
  
  foreach(node; pattern.nodes) {
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
void generateHandlerThunkFunction(PureStringSink sink, const(char)[] linePrefix, const(char)[] name, const(char)[] handlerFunctionName, Pattern pattern) pure
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
  formattedWrite(sink, "%s", patternNodeArgCount(pattern));
  sink(");\n");

  sink(linePrefix);
  sink("    return ");
  sink(handlerFunctionName);
  sink("(processor, constructSymbol");
  
  size_t argIndex = 0;
  foreach(node; pattern.nodes) {
    string typeName = null;
    if(node.countType == CountType.optional) {
      typeName = node.matcher.processorOptionalValueType;
    } else {
      typeName = node.matcher.processorValueType;
    }

    if(typeName && typeName != "nameless") {
      sink(",\n");
      sink(linePrefix);
      formattedWrite(sink, "        args[%s] ? args[%s].as!%s : null", argIndex, argIndex, typeName);
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
  Pattern compiledPattern;
  string handlerCode;
  static ConstructPatternHandler fromDataStructure(string pattern)(string handlerCode)
  {
    mixin("static patternDataStructure = "~pattern~";");
    return ConstructPatternHandler(pattern, patternDataStructure, handlerCode);
  }
  static ConstructPatternHandler fromPattern(string patternCode, string handlerCode)
  {
    return ConstructPatternHandler("compileBasicPattern(\""~patternCode~"\")", compileBasicPattern(patternCode), handlerCode);
  }
}
void generateConstructCode(PureStringSink sink, const(char)[] constructName, ConstructPatternHandler[] patternHandlers)
{
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
    
    sink("    ");
    genConstructHandlerSignature(sink, handlerFuncName, patternHandler.compiledPattern);
    sink("\n");
    sink("    {");
    sink(patternHandler.handlerCode);
    sink("    }\n");

    //
    // Generate Thunk Function
    //
    generateHandlerThunkFunction(sink, "    ", thunkFuncName, handlerFuncName, patternHandler.compiledPattern);
  }
  sink("    static immutable definition = new immutable InternalPatternConstructDefinition\n");
  sink("        ([");
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
      ("pattern", [ConstructPatternHandler.fromDataStructure!(q{
          immutable Pattern([immutable PatternNode("patternList", CountType.one, Matcher.list)])
      })(q{
          return new ConstructPattern(patternList.lineNumber, processPattern(processor, patternList));
      })]));

//
// Functions to generate D code for constructs
// The constructs that use these functions require the constructs
// that are defined above this in order to create construt patterns.
//
void generatePatternConstructCode(PureStringSink sink, string name, string patternCode, string handlerCode)
{
  generateConstructCode(sink, name, [ConstructPatternHandler.fromPattern(patternCode, handlerCode)]);
}

//
// More extended primitives
// These primitives have patterns that rely on the basic primitives
mixin(formattedString!(generatePatternConstructCode)("let", "(name symbol, value, break_ optional constructBreak)", q{
  processor.addSymbol(name.value, value);
  return (break_ is null) ? value : null;
}));
mixin(formattedString!(generatePatternConstructCode)("set", "(name symbol, value, break_ optional constructBreak)", q{
  processor.setSymbol(name.lineNumber, name.value, value);
  return (break_ is null) ? value : null;
}));
mixin(formattedString!(generatePatternConstructCode)("letSet", "(name symbol, value, break_ optional constructBreak)", q{
  processor.letSetSymbol(name.value, value);
  return (break_ is null) ? value : null;
}));
mixin(formattedString!(generatePatternConstructCode)("import", "(name string)", q{
  processor.findAndImport(constructSymbol.lineNumber, name.toUtf8);
  return null;
}));

   
mixin(formattedString!(generateConstructCode)
      ("defcon", [ConstructPatternHandler.fromPattern("(name symbol, patternList list, nameless constructBreak)", q{
    ConstructAttributes attributes;

    //const(ConstructParam)[] requiredParams = parseRequiredParams(processor, pattern);
    auto processedPattern = processPattern(processor, patternList);

    throw imp("backend constructs");
    /+
     auto nodes = new PatternNode[requiredParams.length];
     foreach(i, param; requiredParams) {
     nodes[i] = PatternNode(null, CountType.one, param.type.matcher);
     }
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
    auto processedPattern = processPattern(processor, patternList);
    /*
      const(ConstructParam)[] requiredParams = parseRequiredParams(processor, pattern);
      auto nodes = new PatternNode[requiredParams.length];
      foreach(i, param; requiredParams) {
      if(param.type is null) {
      nodes[i] = PatternNode(null, CountType.one, Matcher.anything);
      } else {
      nodes[i] = PatternNode(null, CountType.one, param.type.matcher);
      }
      }
    */
    processor.addSymbol(name.value, new immutable ConstructWithBlockDefinition
                        (processedPattern, constructSymbol.lineNumber, cast(string)processor.currentFile.name, attributes, null,
                         cast(immutable(ConstructBlock))implementation));
    return null;
})]));


immutable ifConstructDefinition = new immutable FunctionConstructDefinition
  (__LINE__, __FILE__, ConstructAttributes.init, null, &ifConstructHandler);
const(ConstructObject) ifConstructHandler(ConstructProcessor* processor,
                                          const(ConstructDefinition) definition,
                                          const(ConstructSymbol) constructSymbol,
                                          const(ConstructObject)[] objects, size_t* argIndex)
{
  auto condition = processor.consumeTypedValue!ConstructBool(constructSymbol, objects, argIndex);
  auto trueCase = processor.consumeTypedValue!ConstructBlock(constructSymbol, objects, argIndex);
  ConstructObject resultObject = null;
  if(condition.value) {
    processor.pushScope(trueCase.lineNumber, ScopeType.block, true);
    scope(exit) { processor.popScope(); }
    return processor.processBlock(trueCase.objects).unconst;
  } else {
    return null;
  }
}

mixin(formattedString!(generatePatternConstructCode)("equals", "(left,right)", q{
      return new ConstructBool(constructSymbol.lineNumber, left.equals(right, false));
}));
mixin(formattedString!(generatePatternConstructCode)("not", "(value bool)", q{
      return new ConstructBool(constructSymbol.lineNumber, !value.value);
}));

mixin(formattedString!(generateConstructCode)
      ("toSymbol", [ConstructPatternHandler.fromPattern("(symbol symbol)", "return symbol;"),
                    ConstructPatternHandler.fromPattern("(string_ string)", "return new ConstructSymbol(string_.lineNumber, string_.toUtf8());")]));

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
  immutable tryPattern = immutable Pattern
    ([immutable PatternNode("tryBlock", CountType.one, Matcher.block),
      immutable PatternNode("catchClauses", CountType.many, new immutable ConstructPattern(0, Pattern
			    ([immutable PatternNode(null, CountType.one, new immutable KeywordMatcher("catch")),
			      immutable PatternNode("catchBlock", CountType.one, Matcher.block)]))),
      immutable PatternNode("finallyClause", CountType.optional, new immutable ConstructPattern(0, Pattern
			    ([immutable PatternNode(null, CountType.one, new immutable KeywordMatcher("finally")),
			      immutable PatternNode("finallyBlock", CountType.one, Matcher.block)])))]);
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
    if(auto symbol = nextObject.asConstructSymbol) {
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
  (__LINE__, __FILE__, ConstructAttributes.init, null, &tryConstructHandler);
const(ConstructObject) tryConstructHandler(ConstructProcessor* processor,
                                           const(ConstructDefinition) definition,
                                           const(ConstructSymbol) constructSymbol,
                                           const(ConstructObject)[] objects, size_t* argIndex)
{
  auto code = processor.consumeTypedValue!ConstructBlock(constructSymbol, objects, argIndex);

  ConstructSymbol catchSymbol = null;
  ConstructBlock catchBlock = null;
  ConstructBlock finallyBlock = null;

  while(*argIndex < objects.length) {
    auto nextObject = objects[*argIndex];
    if(auto symbol = nextObject.asConstructSymbol) {
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
}


mixin(formattedString!(generatePatternConstructCode)("addSymbolsToCaller", "(code constructBlock)", q{
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
  (__LINE__, __FILE__, ConstructAttributes.init, null, &foreachConstructHandler);
const(ConstructObject) foreachConstructHandler(ConstructProcessor* processor,
                                           const(ConstructDefinition) definition,
                                           const(ConstructSymbol) constructSymbol,
                                           const(ConstructObject)[] objects, size_t* argIndex)
{
  auto forEachArgs = processor.consumeTypedValue!ConstructList(constructSymbol, objects, argIndex);
  auto forEachCode = processor.consumeTypedValue!ConstructBlock(constructSymbol, objects, argIndex);

  //
  // Parse the foreach iteration list
  //
  const(char)[] indexVar = null;
  const(char)[] itemVar;
  ConstructList list;

  {
    size_t listIndex = 0;
    if(listIndex >= forEachArgs.items.length) {
      throw processor.semanticError(constructSymbol.lineNumber, "foreach iteration list is incomplete");
    }
    itemVar = processor.consumeSymbol(constructSymbol, forEachArgs.items, &listIndex).value;

    if(listIndex >= forEachArgs.items.length) {
      throw processor.semanticError(constructSymbol.lineNumber, "foreach iteration list is incomplete");
    }
    auto next = forEachArgs.items[listIndex++].unconst;
    if(next.isListBreak) {
      indexVar = itemVar;
      itemVar = processor.consumeSymbol(constructSymbol, forEachArgs.items, &listIndex).value;
      if(listIndex >= forEachArgs.items.length) {
        throw processor.semanticError(constructSymbol.lineNumber, "foreach iteration list is incomplete");
      }
      next = forEachArgs.items[listIndex++].unconst;
    }

    if(auto inSymbol = next.asConstructSymbol) {
    } else {
      throw processor.semanticError(constructSymbol.lineNumber, format
                                    ("foreach expected an 'in' symbol but got %s: %s",
                                     An(next.typeName), next));
    }

    if(listIndex >= forEachArgs.items.length) {
      throw processor.semanticError(constructSymbol.lineNumber, "foreach iteration list is incomplete");
    }
    auto object = processor.consumeValueAlreadyCheckedIndex(forEachArgs.items, &listIndex);
    if(object is null) {
      throw processor.semanticError(constructSymbol.lineNumber, "foreach iteration list got a void expression");
    }
    if(listIndex < forEachArgs.items.length) {
      throw processor.semanticError(constructSymbol.lineNumber, "foreach iteration list has too many items");
    }

    list = object.asConstructList.unconst;
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
      if(auto return_ = firstResult.asConstructReturn) {
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
        if(auto return_ = result.asConstructReturn) {
          return result;
        }
      }
    }
  }

  return null;
}


