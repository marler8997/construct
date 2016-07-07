module construct.processor;

import std.file   : exists, buildNormalizedPath, read;
import std.path   : setExtension, stripExtension;
import std.array  : Appender, appender, isDynamicArray, ElementEncodingType;
import std.string : startsWith, format;

import construct.ir;
import construct.parser : SourceFile, ConstructParser, standardParser, ConstructParseException;

import backend    : loadConstructBackend, loadBackendType;

bool verbose;
private enum LogLevel {
  debug_,
  info,
  warning,
  error,
}
private immutable string[] logLevelPrefix =
  [
   LogLevel.debug_  : "[DEBUG] ",
   LogLevel.info    : "[INFO] ",
   LogLevel.warning : "[WARNING] ",
   LogLevel.error   : "[ERROR] ",
   ];
private void log(A...)(LogLevel level, in char[] fmt, A args)
{
  import std.stdio : writefln, stdout;
  if(verbose || (level > LogLevel.info)) {
    writefln("[PROCESSOR] " ~ logLevelPrefix[level] ~ fmt, args);
    stdout.flush();
  }
}
private void logDebug(A...)(in char[] fmt, A args)
{
  log(LogLevel.debug_, fmt, args);
}
private void logInfo(A...)(in char[] fmt, A args)
{
  log(LogLevel.info, fmt, args);
}
private void logWarning(A...)(in char[] fmt, A args)
{
  log(LogLevel.warning, fmt, args);
}
private void logError(A...)(in char[] fmt, A args)
{
  log(LogLevel.error, fmt, args);
}


class ConstructThrownException : ConstructException
{
  this(size_t lineNumber, ConstructProcessor* processor, string msg)
  {
    super(msg, cast(string)processor.currentFile.name, lineNumber);
  }
}

class SemanticException: ConstructException
{
  Appender!(SemanticException[]) errorList;
  this(size_t lineNumber, ConstructProcessor* processor, string msg)
  {
    super(msg, cast(string)processor.currentFile.name, lineNumber);
  }
}
class InvalidConstructException: SemanticException
{
  const(ConstructObject) obj;
  this(const(ConstructObject) obj, ConstructProcessor* processor, string msg)
  {
    super(obj.lineNumber, processor, msg);
    this.obj = obj;
  }
}

struct ImportPath
{
  const(char)[] path;
  const(char)[] prefix;
}
class ImportFile
{
  const char[] importName;
  SourceFile sourceFile;
  const(char)[] code;

  OneOrMore!ISymbolObject[const(char)[]] publicSymbols;

  this(const(char)[] importName, SourceFile sourceFile, const(char)[] code)
  {
    this.importName = importName;
    this.sourceFile = sourceFile;
    this.code = code;
  }
}

enum ScopeType {
  file, defcon, block,
}

struct OneOrMore(T)
{
  T first;
  size_t moreCount;
  T[] buffer = void;
  this(T first)
  {
    this.first = first;
  }

  @property
  Range each()
  {
    return Range(this, 0);
  }
  struct Range
  {
    OneOrMore oneOrMore;
    size_t state;
    this(OneOrMore oneOrMore, size_t state)
    {
      this.oneOrMore = oneOrMore;
      this.state = state;
    }
    @property
    bool empty() {
      //writefln("state is %s, empty is %s", state, state > oneOrMore.moreCount);
      return state > oneOrMore.moreCount;
    }
    @property
    T front()
    {
      return (state == 0) ? oneOrMore.first : oneOrMore.buffer[state-1];
    }
    void popFront()
    {
      state++;
    }
  }
}
const(T) symbolAs(T)(OneOrMore!ISymbolObject entries)
{
  foreach(entry; entries.each()) {
    if(auto value = entry.as!T()) {
      return value;
    }
  }
  return null;
}


struct Scope
{
  size_t startLineNumber;

  ScopeType type;
  
  // If true, then symbols inside the scope can see symbols outside the scope
  bool open;
  // If true, the symbols defined in this scope will be added to the parent scope
  bool addSymbolsToParentScope;

  OneOrMore!ISymbolObject[const(char)[]] symbols;
}

struct ConstructProcessor
{
  ImportPath[] importPaths;
  ImportFile[const(char)[]] importMap;

  SourceFile currentFile;

  Appender!(Scope[]) scopeStack;
  Scope currentScope;

  this(ImportPath[] importPaths)
  {
    this.importPaths = importPaths;
    currentScope.symbols = 
      ["import"    : OneOrMore!ISymbolObject(singleton!ImportConstructDefinition()),
       "defcon"    : OneOrMore!ISymbolObject(singleton!DefconConstructDefinition()),
       "deftype"   : OneOrMore!ISymbolObject(singleton!DeftypeConstructDefinition()),
       "exec"      : OneOrMore!ISymbolObject(singleton!ExecConstructDefinition()),
       "throw"     : OneOrMore!ISymbolObject(singleton!ThrowConstructDefinition()),
       "try"       : OneOrMore!ISymbolObject(singleton!TryConstructDefinition()),
       "let"       : OneOrMore!ISymbolObject(singleton!LetConstructDefinition()),
       "set"       : OneOrMore!ISymbolObject(singleton!SetConstructDefinition()),
       "if"        : OneOrMore!ISymbolObject(singleton!IfConstructDefinition()),
       "equals"    : OneOrMore!ISymbolObject(singleton!EqualsConstructDefinition()),
       "listOf"    : OneOrMore!ISymbolObject(singleton!ListOfConstructDefinition()),
       "return"    : OneOrMore!ISymbolObject(singleton!ReturnConstructDefinition()),
       "semantics" : OneOrMore!ISymbolObject(singleton!SemanticsConstructDefinition()),
       ];
  }

  void pushScope(size_t startLineNumber, ScopeType type, bool openScope, bool addSymbolsToParent)
  {
    scopeStack.put(currentScope);
    currentScope = Scope(startLineNumber, type, openScope, addSymbolsToParent);
  }
  void popScope()
  {
    auto scopeStackLength = scopeStack.data.length;
    if(scopeStackLength == 0) {
      throw new Exception("Attempted to pop the scope stack when there are no scopes on the stack");
    }
    currentScope = scopeStack.data[scopeStackLength-1];
    scopeStack.shrinkTo(scopeStackLength-1);
  }

  final void findAndImport(size_t importLineNumber, const(char)[] importName)
  {
    auto imported = importMap.get(importName, null);
    if(imported) {
      logDebug("%s was already imported", importName);
      return;
    }

    SourceFile importFile;
    
    // Find the import file
    logInfo("searching for '%s'...", importName);
    foreach(importPath; importPaths) {
      auto nameSubPath = importName;
      if(importPath.prefix.length) {
        if(importName.length < importPath.prefix.length + 1 ||
           !importName.startsWith(importPath.prefix) ||
           importName[importPath.prefix.length] != '.') {
          continue;
        }
        nameSubPath = importName[importPath.prefix.length+1..$];
      }

      importFile.name = buildNormalizedPath(importPath.path, nameSubPath.setExtension(".con"));
      ConstructParser parser;
      if(exists(importFile.name)) {
        parser = standardParser!(Appender!(const(ConstructObject)[]));
        logInfo("    FOUND AT: %s", importFile.name);
      } else {
        logInfo("NOT FOUND AT: %s", importFile.name);

        importFile.name = importFile.name.stripExtension;
        if(exists(importFile.name)) {
          parser = standardParser!(Appender!(const(ConstructObject)[]));
          logInfo("    FOUND AT: %s", importFile.name);
        } else {
          logInfo("NOT FOUND AT: %s", importFile.name);
        }
      }
	  
      if(parser.name) {
        auto importCode = cast(const(char)[])read(importFile.name);
	importFile.parsedObjects = parser.func(importCode);

        // Have to do this before processing the import source
        // so that it doesn't try to load the same import twice
        auto importFileEntry = new ImportFile(importName, importFile, importCode);
        importMap[importName] = importFileEntry;
        logDebug("Added import '%s' to map", importName);
        pushScope(1, ScopeType.file, false, false);
        scope(exit) {
          importFileEntry.publicSymbols = currentScope.symbols;
          popScope();
        }
        auto result = process(importFile.parsedObjects);
	return;
      }
    }

    throw new SemanticException(importLineNumber, &this, format("import '%s' not found", importName));
  }

  void addSymbol(const(char)[] symbol, const(ISymbolObject) object)
  {
    auto entry = currentScope.symbols.get(symbol, OneOrMore!ISymbolObject(null));
    if(!entry.first) {
      currentScope.symbols[symbol] = OneOrMore!ISymbolObject(unconst(object));
    } else {
      throw imp("multiple symbol objects with the same name");
    }
  }

  void setSymbol(size_t setLineNumber, const(char)[] symbol, const(ISymbolObject) object)
  {

    auto scope_ = &currentScope;
    auto scopeIndex = scopeStack.data.length;
    while(true) {
      auto entry = scope_.symbols.get(symbol, OneOrMore!ISymbolObject(null));
      if(entry.first) {
        if(entry.moreCount) {
          imp("symbol with multiple entries");
        }
        scope_.symbols[symbol] = OneOrMore!ISymbolObject(unconst(object));
        return;
      }

      if(scopeIndex == 0) {
        throw new SemanticException(setLineNumber, &this, format("Cannot set symbol '%s' because it doesn't exist", symbol));
      }
      scopeIndex--;
      scope_ = &scopeStack.data[scopeIndex];
    }
  }

  const(T) lookupSymbol(T)(const(ConstructSymbol) symbol)
  {
    return lookupSymbol!T(symbol.lineNumber, symbol.value);
  }
  const(T) lookupSymbol(T)(size_t lineNumber, const(char)[] symbol)
  {
    // Search current scope
    OneOrMore!ISymbolObject match = currentScope.symbols.get(symbol, OneOrMore!ISymbolObject(null));
    if(match.first) {
      if(auto object = match.symbolAs!T) {
        return object;
      }
    }

    
    // Search parent scopes
    foreach_reverse(scope_; scopeStack.data) {
      auto anotherMatch = scope_.symbols.get(symbol, OneOrMore!ISymbolObject(null));
      if(anotherMatch.first) {
        if(auto object = anotherMatch.symbolAs!T) {
          return object;
        }
        if(!match.first) {
          match = anotherMatch;
        }
      }
    }

    // Search other files
    imp("import from other files");
    //foreach(fileAndCode; importMap.byKeyValue) {
    //}


    if(match.first) {
      throw semanticError(lineNumber, format
                          ("symbol '%s' of type '%s' cannot be converted to %s", symbol, match.first.typeName, An(T.staticTypeName)));
    }

    throw semanticError(lineNumber, format
                        ("%s '%s' does not exist", T.staticTypeName, symbol));
  }


  OneOrMore!ISymbolObject tryLookupSymbol(const(char)[] symbol)
  {
    // Search current scope
    {
      OneOrMore!ISymbolObject match = currentScope.symbols.get(symbol, OneOrMore!ISymbolObject(null));
      if(match.first) {
	return match;
      }
    }

    // Search parent scopes
    foreach_reverse(scope_; scopeStack.data) {
      auto match = scope_.symbols.get(symbol, OneOrMore!ISymbolObject(null));
      if(match.first) {
	return match;
      }
    }

    // Search other files
    foreach(fileKeyValue; importMap.byKeyValue) {
      auto importFile = fileKeyValue.value;
      auto match = importFile.publicSymbols.get(symbol, OneOrMore!ISymbolObject(null));
      if(match.first) {
        return match;
      }
    }

    return OneOrMore!ISymbolObject(null);
  }


  void printScopeStack()
  {
    imp("printScopeStack");
    /*
    import std.stdio : writefln, stdout;
    auto prefix = "";
    size_t scopeNumber = 0;
    foreach(scope_; scopeStack.data) {
      writefln("%sScope %s (%s symbols)", prefix, scopeNumber, scope_.symbols.length);
      prefix ~= "  ";
      scopeNumber++;
    }
    writefln("%sScope %s (%s symbols)", prefix, scopeNumber, currentScope.symbols.length);
    stdout.flush();
    */
  }


  auto semanticError(size_t lineNumber, string msg)
  {
    return new SemanticException(lineNumber, &this, msg);
  }
  auto endedInsideConstruct(const(ConstructSymbol) constructSymbol)
  {
    if(currentScope.type == ScopeType.file) {
      return new SemanticException(constructSymbol.lineNumber, &this,
                                   format("file ended inside construct '%s'", constructSymbol.value));
    } else {
      return new SemanticException(constructSymbol.lineNumber, &this,
                                   format("construct '%s' was not terminated", constructSymbol.value));
    }
  }
  
  void process(SourceFile sourceFile)
  {
    auto previousFile = currentFile;
    currentFile = sourceFile;
    scope(exit) { currentFile = previousFile; }

    pushScope(1, ScopeType.file, false, false);
    scope(exit) { popScope(); }
    
    process(sourceFile.parsedObjects);
  }
  const(ConstructObject) process(const(ConstructObject)[] objects)
  {
    size_t index = 0;
    while(1) {
      auto result = processOneConstruct(objects, index);
      if(result.nextIndex >= objects.length) {
	return result.object;
      }
      index = result.nextIndex;
    }
  }
  
  // TODO: maybe allow caller to pass an expected type
  ProcessResult processOneConstruct(const(ConstructObject)[] objects, size_t index)
  {
    if(index >= objects.length) {
      return ProcessResult(index, null);
    }

    auto object = objects[index++];
    if(object.isOneImmediateValue) {
      return ProcessResult(index, object);
    }
    
    if(auto symbol = object.asConstructSymbol) {

      logDebug("looking up symbol '%s'...", symbol.value);
      auto symbolEntries = tryLookupSymbol(symbol.value);
      if(!symbolEntries.first) {
	throw semanticError(symbol.lineNumber, format("symbol '%s' does not exist", symbol.value));
      }
      if(symbolEntries.moreCount > 0) {
	throw semanticError(symbol.lineNumber, format("symbol '%s' has multiple entries at the same scope", symbol.value));
      }
      
      if(auto definition = symbolEntries.first.asConstructDefinition) {
	logDebug("processing '%s' construct", symbol.value);
	return definition.process(&this, symbol, objects, index);
      }

      return ProcessResult(index, symbolEntries.first.asOneConstructObject);
    }

    if(auto block = object.asConstructBlock) {
      
      pushScope(block.lineNumber, ScopeType.block, true, false);
      scope(exit) { popScope(); }
      return ProcessResult(index, process(block.objects));
    }

    throw semanticError(object.lineNumber, format("processOneConstruct does not handle type %s", object.typeName));
  }

  // If obj is a symbol, will resolve obj to whatever value it represents
  inout(ISymbolObject) resolveSymbol(inout(ISymbolObject) obj)
  {
    auto result = unconst(obj);
    while(true) {
      if(auto symbol = result.asConstructSymbol) {
	auto entry = tryLookupSymbol(symbol.value);
	if(!entry.first) {
	  throw new SemanticException(obj.getLineNumber, &this, format("symbol '%s' has not been bound", symbol.value));
	}
	if(entry.moreCount > 0) {
	  throw new SemanticException(obj.getLineNumber, &this, format("symbol '%s' has multiple values", symbol.value));
	}
	result = entry.first;
      } else {
	return cast(inout(ISymbolObject))result;
      }
    }
  }
  
  const(T) resolveTo(T)(const(ConstructObject) obj)
  {
    if(auto value = obj.as!T) {
      return value;
    }
    //logDebug("%s is not %s", obj.typeName, T.messageTypeName);

    if(auto symbol = obj.asConstructSymbol) {
      if(auto symbolObject = lookupSymbol!T(symbol)) {
	return symbolObject;
      }
      throw new SemanticException(obj.lineNumber, &this, format("symbol '%s' has not been bound", symbol.value));
    }

    throw imp(format("resolveTo!%s(%s)", T.staticTypeName, obj.typeName));
    /*
    static if( !is( T == Construct ) ) {
      if(Construct construct = cast(Construct)obj) {
	//logDebug("will attempt to convert construct '%s' to '%s'", construct.name, T.staticTypeName);
	auto definition = state.tryFindConstruct(construct.name);
	if(!definition) {
	  throw new Exception(format("argument construct '%s' on line %s is not defined, if this construct is defined later, then this has not been implemented yet", construct.name, construct.lineNumber));
	}
	return evalTo!T(state, definition, construct);
      }
    }
    
    throw new SemanticException(obj.lineNumber, state, format("expected a(n) '%s' but got a(n) '%s'", T.staticTypeName, obj.typeName));
    */
  }
  
}

unittest
{
  ConstructProcessor processor = ConstructProcessor();

  void testLookup(T)(T expected, const(char)[] symbol)
  {
    assert(expected == processor.lookupSymbol!T(1, symbol));
    auto entry = processor.tryLookupSymbol(symbol);
    assert(entry.moreCount == 0);
    assert(expected == entry.first);
  }
  

  auto valueObj = new ConstructString(1, "mysymbol string value");
  processor.addSymbol("mysymbol", valueObj);

  testLookup(valueObj, "mysymbol");

  processor.pushScope(2, ScopeType.block, true, false);
  testLookup(valueObj, "mysymbol");
  processor.popScope();

  testLookup(valueObj, "mysymbol");
  /*
  //
  // Test openScope
  //
  processor.pushScope(2, true, false);
  assert(valueObj == processor.tryLookupSymbol("mysymbol"));
  processor.popScope();
  
  //
  // Test addSymbolsToParentScope
  //
  processor.pushScope(3, false, true);
  assert(!processor.tryLookupSymbol("mysymbol"));

  auto goToParentObj = new ConstructString(1, "the value of goToParent");
  processor.addSymbol("goToParent", goToParentObj);
  assert(!processor.tryLookupSymbol("goToParent"));
  processor.popScope();
  assert(valueObj == processor.tryLookupSymbol("mysymbol"));
  assert(goToParentObj == processor.tryLookupSymbol("goToParent"));
  */
}

alias ProcessorFunc = ProcessResult function(ConstructProcessor* processor,
                                             const(ConstructDefinition) definition,
                                             const(ConstructSymbol) constructSymbol,
                                             const(ConstructObject)[] objects, size_t argIndex);
  
class FunctionConstructDefinition : ConstructDefinition
{
  ProcessorFunc func;
  this(size_t lineNumber, const(char)[] name, ConstructAttributes attributes,
       ConstructType evalTo, const(ConstructParam)[] requiredParams,
       const(ConstructOptionalParam)[] optionalParams, ProcessorFunc func)
  {
    super(lineNumber, name, attributes, evalTo, requiredParams, optionalParams);
    this.func = func;
  }
  final override ProcessResult process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                       const(ConstructObject)[] objects, size_t argIndex) const
  {
    return func(processor, this, constructSymbol, objects, argIndex);
  }
}

class ConstructWithBlockDefinition : ConstructDefinition
{
  const ConstructBlock block;
  this(size_t lineNumber, const(char)[] name, ConstructAttributes attributes,
       ConstructType evalTo, const(ConstructParam)[] requiredParams,
       const(ConstructOptionalParam)[] optionalParams, const(ConstructBlock) block)
  {
    super(lineNumber, name, attributes, evalTo, requiredParams, optionalParams);
    this.block = block;
  }
  final override ProcessResult process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                       const(ConstructObject)[] objects, size_t argIndex) const
  {
    logDebug("entering scope for construct '%s' defined on line %s (%s required args)",
             name, lineNumber, requiredParams.length);
    processor.pushScope(lineNumber, ScopeType.defcon,
                        cast(bool)(attributes & ConstructAttribute.openScope),
                        cast(bool)(attributes & ConstructAttribute.addSymbolsToParentScope));
    scope(exit) {processor.popScope();}

    //
    // Parse parameters
    //
    if(argIndex + requiredParams.length >= objects.length) {
      throw processor.endedInsideConstruct(constructSymbol);
    }
    foreach(i, requiredParam; requiredParams) {
      if(requiredParam.type) {
	auto arg = objects[argIndex+i];
	if(!arg.canBe(requiredParam.type)) {
	  throw processor.semanticError(arg.lineNumber, format
					("Expected argument %s to be %s, but is %s", (i+1),
					 An(requiredParam.type.typeName), An(arg.typeName)));
	}
	processor.addSymbol(requiredParam.name, arg);
      }
    }
    /*
    {
      auto arg = objects[requiredParams.length];
      if(!arg.isObjectBreak) {
	throw processor.semanticError(arg.lineNumber, format("Expected ';' but got %s", An(arg.typeName)));
      }
    }
    */

    return ProcessResult(argIndex + requiredParams.length, processor.process(block.objects));
  }
}

class ImportConstructDefinition : ConstructDefinition
{
  this()
  {
    super(0, "import", ConstructAttributes.init, null, null, null);
  }
  final override ProcessResult process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                       const(ConstructObject)[] objects, size_t argIndex) const
  {
    while(true) {
      if(argIndex >= objects.length) {
        throw processor.endedInsideConstruct(constructSymbol);
      }
      auto object = objects[argIndex++];
      if(object.isObjectBreak) {
        break;
      } else if(auto string_ = object.asConstructString) {
        processor.findAndImport(constructSymbol.lineNumber, string_.value);
      } else {
        throw processor.semanticError(constructSymbol.lineNumber, format
                                      ("the import construct expects a string but got %s",
                                       An(object.typeName)));
      }
    }

    // TODO: maybe import should return a code block?
    return ProcessResult(argIndex, null);
  }
}


const(T) nextArg(T)(ConstructProcessor* processor, const(ConstructDefinition) construct,
             const(ConstructObject)[] objects, size_t* argIndex)
{
  if(*argIndex >= objects.length) {
    // TODO: get line number
    throw processor.semanticError(0, format("construct '%s' expects %s but reached the end of input", construct.name, An(T.staticTypeName)));
  }

  auto object = objects[*argIndex];
  auto casted = object.as!T;
  if(!casted) {
    throw processor.semanticError(object.lineNumber, format("construct '%s' expects %s but got %s", construct.name, An(T.staticTypeName), An(object.typeName)));
  }
  (*argIndex)++;
  return casted;
}

const(ConstructParam)[] parseRequiredParams(ConstructProcessor* processor, const(ConstructList) list)
{
  auto params = appender!(const(ConstructParam)[])();
  for(size_t itemIndex = 0;;) {
    if(itemIndex >= list.items.length) {
      return params.data;
    }

    auto paramName = list.items[itemIndex].asConstructSymbol;
    if(!paramName) {
      throw processor.semanticError(list.items[itemIndex].lineNumber, format
				    ("expected defcon parameter name to be a symbol but got %s",
				     An(list.items[itemIndex].typeName)));
    }
    itemIndex++;
    if(itemIndex >= list.items.length) {
      params.put(ConstructParam(paramName.value));
      break;
    }

    ConstructType paramType;
    {
      auto result = processor.processOneConstruct(list.items, itemIndex);
      if(result.object is null) {
	throw processor.semanticError(list.items[itemIndex].lineNumber,
				      "expected the expression to return a type but returned null");
      }
      itemIndex = result.nextIndex;
      if(result.object.isListBreak) {
	params.put(ConstructParam(paramName.value));
	continue;
      }
      paramType = result.object.asConstructType.unconst;
      if(paramType is null) {
	throw processor.semanticError(list.items[itemIndex].lineNumber, format
				      ("expected the expression to return a type but returned %s",
				       An(result.object.typeName)));
      }
    }

    if(itemIndex >= list.items.length) {
      params.put(ConstructParam(paramName.value, paramType));
      break;
    }
    if(list.items[itemIndex].isListBreak) {
      params.put(ConstructParam(paramName.value));
      itemIndex++;
      continue;
    }

    imp("a third value for a defcon parameter");
  }

  return params.data;
}

class DefconConstructDefinition : ConstructDefinition
{
  this() { super(0, "defcon", ConstructAttributes.init, null, null, null); }
  final override ProcessResult process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                       const(ConstructObject)[] objects, size_t argIndex) const
  {
    auto lineNumber = objects[argIndex-1].lineNumber;
    auto constructName = nextArg!ConstructSymbol(processor, this, objects, &argIndex).value;

    ConstructBlock implementation = null;
    ConstructAttributes attributes;

    const(ConstructParam)[] requiredParams = null;

    //
    // Parse defcon information
    //
    while(true) {
      if(argIndex >= objects.length) {
        throw processor.endedInsideConstruct(constructSymbol);
      }
      auto object = objects[argIndex++];
      if(object.isObjectBreak) {
        break;
      } else if(auto block = object.asConstructBlock) {
	implementation = unconst(block);
	break;
      } else if(auto list = object.asConstructList) {
	if(requiredParams == null) {
	  requiredParams = parseRequiredParams(processor, list);
	} else {
	  imp("defcon construct optional param list");
	}
      } else if(auto named = object.asNamedObject) {
	imp("defcon named objects");
      } else {
        imp("defcon construct");
      }
    }

    //
    // Create the construct definition
    //
    if(implementation) {
      processor.addSymbol(constructName, new ConstructWithBlockDefinition
                          (lineNumber, constructName, attributes, null, requiredParams, null, implementation));
    } else {
      auto backendFunc = loadConstructBackend(constructName);
      if(backendFunc == null) {
        throw processor.semanticError(lineNumber, format
                                      ("the backend does not implement construct '%s'", constructName));
      }
      processor.addSymbol(constructName, new FunctionConstructDefinition
                          (lineNumber, constructName, attributes, null, requiredParams, null, backendFunc));
    }
    
    return ProcessResult(argIndex, null);
  }
}

class DeftypeConstructDefinition : ConstructDefinition
{
  this() { super(0, "defcon", ConstructAttributes.init, null, null, null); }
  final override ProcessResult process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                       const(ConstructObject)[] objects, size_t argIndex) const
  {
    auto lineNumber = objects[argIndex-1].lineNumber;
    auto typeName = nextArg!ConstructSymbol(processor, this, objects, &argIndex).value;

    ConstructType type;
    
    while(true) {
      if(argIndex >= objects.length) {
        throw processor.endedInsideConstruct(constructSymbol);
      }
      auto object = objects[argIndex++];
      if(object.isObjectBreak) {
        break;
      } else {
        imp("deftype construct");
      }
    }

    if(!type) {
      type = loadBackendType(typeName);
      if(!type) {
	throw processor.semanticError(lineNumber, format("backend does not support type '%s'", typeName));
      }
    }

    processor.addSymbol(typeName, new TypeDefinition(lineNumber, typeName, type));
    return ProcessResult(argIndex, null);
  }
}
class ExecConstructDefinition : ConstructDefinition
{
  this() { super(0, "exec", ConstructAttributes.init, null, null, null); }
  final override ProcessResult process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                       const(ConstructObject)[] objects, size_t argIndex) const
  {
    // Note: should be a subset of the Catch construct
    throw imp("exec construct");
  }
}
class ThrowConstructDefinition : ConstructDefinition
{
  this() { super(0, "throw", ConstructAttributes.init, null, null, null); }
  final override ProcessResult process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                       const(ConstructObject)[] objects, size_t argIndex) const
  {
    if(argIndex >= objects.length) {
      throw processor.endedInsideConstruct(constructSymbol);
    }
    auto message = processor.resolveTo!ConstructString(objects[argIndex++]);
    throw new ConstructThrownException(constructSymbol.lineNumber, processor, cast(string)message.value);
  }
}
class TryConstructDefinition : ConstructDefinition
{
  this() { super(0, "try", ConstructAttributes.init, null, null, null); }
  final override ProcessResult process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                       const(ConstructObject)[] objects, size_t argIndex) const
  {
    if(argIndex >= objects.length) {
      throw processor.endedInsideConstruct(constructSymbol);
    }
    auto code = processor.resolveTo!ConstructBlock(objects[argIndex++]);


    ConstructSymbol catchSymbol = null;
    ConstructBlock catchBlock = null;
    ConstructBlock finallyBlock = null;

    for(;argIndex < objects.length; argIndex++)
    {
      auto next = objects[argIndex];
      if(auto symbol = next.asConstructSymbol)
      {
	if(symbol.value == "catch") {
	  argIndex++;
	  if(argIndex + 1 >= objects.length) {
	    throw processor.endedInsideConstruct(constructSymbol);
	  }
	  catchSymbol = unconst(objects[argIndex].asConstructSymbol);
	  if(!catchSymbol) {
	    throw processor.semanticError(objects[argIndex].lineNumber, format
					  ("expected a symbol but got %s", An(objects[argIndex].typeName)));
	  }
	  catchBlock = unconst(processor.resolveTo!ConstructBlock(objects[argIndex + 1]));
	  argIndex++;
	  
	} else if(symbol.value == "finally") {
	  throw imp("try-finally");
	} else {
	  break;
	}
      } else {
	break;
      }
    }

    try {
      auto resultObject = processor.process(code.objects);
      return ProcessResult(argIndex, resultObject);
    } catch(ConstructException e) {
      processor.pushScope(catchBlock.lineNumber, ScopeType.block, true, false);
      scope(exit) { processor.popScope(); }

      if(catchSymbol) {
	processor.addSymbol(catchSymbol.value, new ConstructString(0, e.msg));
      }
      auto resultObject = processor.process(catchBlock.objects);
      return ProcessResult(argIndex, resultObject);
    }
  }
}
class LetConstructDefinition : ConstructDefinition
{
  this() { super(0, "let", ConstructAttributes.init, null, null, null); }
  final override ProcessResult process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                       const(ConstructObject)[] objects, size_t argIndex) const
  {
    while(true) {
      if(argIndex >= objects.length) {
        throw processor.endedInsideConstruct(constructSymbol);
      }
      auto object = objects[argIndex++];
      if(object.isObjectBreak) {
        break;
      } else if(auto namedObject = object.asNamedObject) {
        processor.addSymbol(namedObject.name, namedObject.object);
      } else {
        throw processor.semanticError(object.lineNumber, format("the let construct doesn't support type '%s'",
                                                                object.typeName));
      }
    }
    return ProcessResult(argIndex, null);
  }
}
class SetConstructDefinition : ConstructDefinition
{
  this() { super(0, "set", ConstructAttributes.init, null, null, null); }
  final override ProcessResult process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                       const(ConstructObject)[] objects, size_t argIndex) const
  {
    while(true) {
      if(argIndex >= objects.length) {
        throw processor.endedInsideConstruct(constructSymbol);
      }
      auto object = objects[argIndex++];
      if(object.isObjectBreak) {
        break;
      } else if(auto namedObject = object.asNamedObject) {
        processor.setSymbol(constructSymbol.lineNumber, namedObject.name, processor.resolveSymbol(namedObject.object));
      } else {
        throw processor.semanticError(object.lineNumber, format("the let construct doesn't support type '%s'",
                                                                object.typeName));
      }
    }
    return ProcessResult(argIndex, null);
  }
}

class IfConstructDefinition : ConstructDefinition
{
  this() { super(0, "if", ConstructAttributes.init, null, null, null); }
  final override ProcessResult process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                       const(ConstructObject)[] objects, size_t argIndex) const
  {
    ConstructBool condition;
    {
      auto result = processor.processOneConstruct(objects, argIndex);
      if(result.object is null) {
	throw processor.semanticError(constructSymbol.lineNumber,
				      "the if construct expects a bool but the expression has no result");
      }
      argIndex = result.nextIndex;
      condition = unconst(processor.resolveTo!ConstructBool(result.object));
    }


    if(argIndex >= objects.length) {
      throw processor.endedInsideConstruct(constructSymbol);
    }
    const ConstructBlock trueCase = processor.resolveTo!ConstructBlock(objects[argIndex++]);
    ConstructObject resultObject = null;
    if(condition.value) {
      processor.pushScope(trueCase.lineNumber, ScopeType.block, true, false);
      scope(exit) { processor.popScope(); }

      resultObject = unconst(processor.process(trueCase.objects));
    }

    return ProcessResult(argIndex, resultObject);
  }
}


class EqualsConstructDefinition : ConstructDefinition
{
  this() { super(0, "equals", ConstructAttributes.init, new PrimitiveType(0, PrimitiveTypeEnum.bool_), null, null); }
  final override ProcessResult process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                       const(ConstructObject)[] objects, size_t argIndex) const
  {
    if(argIndex + 1 >= objects.length) {
      throw processor.endedInsideConstruct(constructSymbol);
    }

    auto left  = processor.resolveSymbol(objects[argIndex  ]);
    auto right = processor.resolveSymbol(objects[argIndex+1]);
    
    auto result = left.equals(right, false);
    logDebug("equals %s %s -> %s", objects[argIndex], objects[argIndex+1], result);
    return ProcessResult(argIndex+2, new ConstructBool(constructSymbol.lineNumber,  result));
  }
}
class ListOfConstructDefinition : ConstructDefinition
{
  this() { super(0, "listOf", ConstructAttributes.init, new PrimitiveType(0, PrimitiveTypeEnum.list), null, null); }
  final override ProcessResult process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                       const(ConstructObject)[] objects, size_t argIndex) const
  {
    auto result = processor.processOneConstruct(objects, argIndex);
    if(result.object is null) {
      throw processor.semanticError(constructSymbol.lineNumber,
				    "the listOf construct expects a type but the following expression has no result");
    }
    return ProcessResult(result.nextIndex, unconst(processor.resolveTo!ConstructType(result.object)));
  }
}
class ReturnConstructDefinition : ConstructDefinition
{
  this() { super(0, "return", ConstructAttributes.init, new PrimitiveType(0, PrimitiveTypeEnum.anything), null, null); }
  final override ProcessResult process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                       const(ConstructObject)[] objects, size_t argIndex) const
  {
    throw imp("return construct");
  }
}
class StructConstructDefinition : ConstructDefinition
{
  this() { super(0, "struct", ConstructAttributes.init, null, null, null); }
  final override ProcessResult process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                       const(ConstructObject)[] objects, size_t argIndex) const
  {
    throw imp("struct construct");
  }
}
class SemanticsConstructDefinition : ConstructDefinition
{
  this() { super(0, "semantics", ConstructAttributes.init, null, null, null); }
  final override ProcessResult process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                       const(ConstructObject)[] objects, size_t argIndex) const
  {
    throw imp("semantics construct");
  }
}
