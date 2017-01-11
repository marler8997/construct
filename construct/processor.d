module construct.processor;

import std.file   : exists, read;
import std.format : format;
import std.path   : setExtension, stripExtension, buildNormalizedPath, absolutePath, dirName;
import std.array  : Appender, appender, isDynamicArray, ElementEncodingType;
import std.string : startsWith, indexOf;
import std.conv   : to, ConvOverflowException;
import std.format : formattedWrite;
import std.bigint : BigInt;

import construct.util;
import construct.logging;
import construct.patterns;
import construct.parserCore;
import construct.backendCore;
import construct.parser : SourceFile, ConstructParser, standardParser, ConstructParseException;
import construct.primitives;

import backend    : loadConstructBackend, loadBackendType;

class ConstructThrownException : ConstructException
{
  this(size_t lineNumber, ConstructProcessor* processor, string msg) pure
  {
    super(msg, cast(string)processor.currentFile.relativeName, lineNumber);
  }
}

class SemanticException: ConstructException
{
  Appender!(SemanticException[]) errorList;
  this(size_t lineNumber, const(ConstructProcessor)* processor, string msg) pure
  {
    const(char)[] filename;
    if(processor.currentConstruct && processor.currentConstruct.filename.length > 0) {
      filename = processor.currentConstruct.filename;
    } else {
      filename = processor.currentFile.relativeName;
    }
    super(msg, cast(string)filename, lineNumber);
  }
}
class InvalidConstructException: SemanticException
{
  const(ConstructObject) obj;
  this(const(ConstructObject) obj, const(ConstructProcessor)* processor, string msg) pure
  {
    super(obj.lineNumber, processor, msg);
    this.obj = obj;
  }
}
string getFilenameForException(const(ConstructProcessor)* processor) pure
{
  if(!processor) {
    return null;
  } else if(processor.currentConstruct && processor.currentConstruct.filename.length > 0) {
    return cast(string)processor.currentConstruct.filename;
  } else {
    return cast(string)processor.currentFile.relativeName;
  }
}
class ConstructAssertException: ConstructException
{
  Appender!(SemanticException[]) errorList;
  this(size_t lineNumber, const(ConstructProcessor)* processor, string msg) pure
  {
    super(msg, getFilenameForException(processor), lineNumber);
  }
}
class ConstructIndexOutOfRangeException : ConstructException
{
  this(size_t lineNumber, const(ConstructProcessor)* processor, BigInt index)
  {
    super(format("index %s is out of range", index), getFilenameForException(processor), lineNumber);
  }
}

size_t toIndex(BigInt index)
{
  try {
    return index.to!size_t;
  } catch(ConvOverflowException) {
    throw new ConstructIndexOutOfRangeException(0, null, index);
  }
}


alias SymbolEntryList = OneOrMore!ConstructObject;
alias SymbolTable = SymbolEntryList[string];

enum PatternMode
{
  firstMatch,
}

struct ImportPath
{
  const(char)[] relativePath; // should be normalized
  const(char)[] absolutePath; // should be normalized
  const(char)[] prefix;
}
class ImportFile
{
  string importName;

  SourceFile sourceFile;
  SymbolTable publicSymbols;

  this(string importName, SourceFile sourceFile) pure
  {
    this.importName = importName;
    this.sourceFile = sourceFile;
  }
}

enum ScopeType {
  file, defcon, block, classDef
}

struct OneOrMore(T)
{
  T first;
  size_t moreCount;
  T[] buffer = void;
  this(T first) pure
  {
    this.first = first;
  }

  @property
  Range each() const
  {
    return Range(this, 0);
  }
  struct Range
  {
    const OneOrMore oneOrMore;
    size_t state;
    this(const OneOrMore oneOrMore, size_t state)
    {
      this.oneOrMore = oneOrMore;
      this.state = state;
    }
    @property
    bool empty() const {
      //writefln("state is %s, empty is %s", state, state > oneOrMore.moreCount);
      return state > oneOrMore.moreCount;
    }
    @property
    T front() const
    {
      return (state == 0) ? cast(T)oneOrMore.first : cast(T)oneOrMore.buffer[state-1];
    }
    void popFront()
    {
      state++;
    }
  }
}
const(T) symbolAs(T)(SymbolEntryList entries)
{
  foreach(entry; entries.each()) {
    if(auto value = entry.tryAs!T()) {
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
  //bool addSymbolsToParentScope;
  Scope* addSymbolsTo;

  SymbolTable symbols;

  ConstructStatementMode statementModeToRestoreOnPop;
}

struct ScopeAndEntryList
{
  ScopeType scopeType;
  SymbolTable symbols;
  SymbolEntryList entryList;
}

struct ConstructProcessor
{
  ImportPath[] importPaths;

  ConstructStatementMode statementMode;
  PatternMode patternMode;

  // key is the absolute normalized filename of the import file
  // NOTE: every file that is imported should be in this map
  ImportFile[string] importAbsoluteFilenameMap;

  // key is the import name, should be path/path/.../name
  ImportFile[string] importMap;

  SourceFile currentFile;

  Appender!(Scope[]) scopeStack;
  Scope currentScope;

  ConstructDefinition currentConstruct;

  this(ImportPath[] importPaths) pure
  {
    this.importPaths = importPaths;

    statementMode = ConstructStatementMode.default_.unconst;
    
    // TODO: implement 'letset' which adds a symbol if it does not exist, or updates
    //       it if it does exist
    currentScope.symbols =
      [
       //
       // Keyword Values
       //
       "false"         : SymbolEntryList(falseConstructDefinition.unconst),
       "true"          : SymbolEntryList(trueConstructDefinition.unconst),
       "null"          : SymbolEntryList(nullConstructDefinition.unconst),

       //
       // Basic Constructs
       //
       "pattern"       : SymbolEntryList(PatternConstruct.definition.unconst),
       ];

    addPrimitiveType(PrimitiveType.predicate);
    addPrimitiveType(PrimitiveType.optionalValue);
    addPrimitiveType(PrimitiveType.bool_);
    addPrimitiveType(PrimitiveType.number);
    addPrimitiveType(PrimitiveType.integer);
    addPrimitiveType(PrimitiveType.integerLiteral);
    addPrimitiveType(PrimitiveType.unsigned_);
    addPrimitiveType(PrimitiveType.uint_);
    addPrimitiveType(PrimitiveType.string_);
    addPrimitiveType(PrimitiveType.utf8);
    addPrimitiveType(PrimitiveType.symbol);
    addPrimitiveType(PrimitiveType.anything);
    addPrimitiveType(PrimitiveType.nullable);
    addPrimitiveType(PrimitiveType.type);
    addPrimitiveType(PrimitiveType.constructBlock);
    addPrimitiveType(PrimitiveType.pointer);
    addPrimitiveType(PrimitiveType.list);
    //addPrimitiveType(PrimitiveType.class_);
    addPrimitiveType(PrimitiveType.constructBreak);
    addPrimitiveType(PrimitiveType.statementMode);
  }
  private void addPrimitiveType(const(PrimitiveType) primitiveType) pure
  {
    currentScope.symbols[primitiveType.typeEnum.definition.name] = SymbolEntryList(primitiveType.unconst);
  }
  private void addConstruct(const(ConstructDefinition) definition)
  {
    auto entry = currentScope.symbols.get(definition.name, SymbolEntryList(null));
    if(entry.first) {
      throw imp(format("construct '%s' has multiple entries '%s' (type=%s) and '%s'", definition.name,
		       entry.first, entry.first.typeName, definition));
    } else {
      currentScope.symbols[definition.name] = SymbolEntryList(definition.unconst);
    }
  }
  void loadExtendedConstructs()
  {
    addConstruct(ImportConstruct.definition);
    //addConstruct(RelativeImportConstruct.definition);
    addConstruct(DefconConstruct.definition);
    addConstruct(SymbolRefConstruct.definition);
    addConstruct(ToSymbolConstruct.definition);
    //currentScope.symbols["deftype"]   = SymbolEntryList(singleton!DeftypeConstructDefinition());
    addConstruct(ExecConstruct.definition);
    addConstruct(ThrowConstruct.definition);
    addConstruct(ReturnConstruct.definition);
    addConstruct(AssertConstruct.definition);
    addConstruct(tryConstructDefinition);
    addConstruct(LetConstruct.definition);
    addConstruct(SetConstruct.definition);
    addConstruct(LetsetConstruct.definition);
    addConstruct(IfConstruct.definition);
    addConstruct(NotConstruct.definition);
    addConstruct(foreachConstructDefinition);
    addConstruct(ListOfConstruct.definition);
    //currentScope.symbols["typeOf"]    = SymbolEntryList(singleton!TypeOfConstructDefinition());
    addConstruct(ItemTypeConstruct.definition);
    //currentScope.symbols["typed"]     = SymbolEntryList(singleton!TypedConstructDefinition());
    //currentScope.symbols["memcpy"]    = SymbolEntryList(singleton!MemcpyConstructDefinition());
    //currentScope.symbols["return"]    = SymbolEntryList(singleton!ReturnConstructDefinition());
    addConstruct(AddSymbolsToCallerConstruct.definition);

    //
    // Types
    //
    addConstruct(ImportBackendPackageConstruct.definition);
    addConstruct(LoadBackendTypeConstruct.definition);
    //addConstruct(MakeTypeFromConstruct.definition);
    addConstruct(DeftypeConstruct.definition);
    //addConstruct(TypeofConstruct.definition);
    addConstruct(IsAConstruct.definition);

    //
    // Operators
    //
    addConstruct(NegativeConstruct.definition);
    addConstruct(DotConstruct.definition);
    addConstruct(PlusConstruct.definition);
    addConstruct(MultiplyConstruct.definition);
    addConstruct(EqualsConstruct.definition);
    addConstruct(NotEqualsConstruct.definition);

    //
    // String Operations
    //
    addConstruct(ByteLengthConstruct.definition);
    addConstruct(GetUtf8ByteConstruct.definition);
    //addConstruct(StringIndexOperatorConstruct.definition);
    addConstruct(LastIndexOfConstruct.definition);
    addConstruct(StrcpyConstruct.definition);
    addConstruct(SliceConstruct.definition);

    //
    // Modes
    //
    //addConstruct(DefStatementModeConstruct.definition);
    //addConstruct(GetStatementModeConstruct.definition);
    addConstruct(CreateStatementModeConstruct.definition);
    addConstruct(SetStatementModeConstruct.definition);
    addConstruct(GetPatternModeConstruct.definition);
    addConstruct(SetPatternModeConstruct.definition);

    //
    // Memory
    //
    addConstruct(MallocConstruct.definition);

    //
    // Classes
    //
    addConstruct(ClassConstruct.definition);
    addConstruct(NewConstruct.definition);

    //
    // Debug Constructs
    //
    //currentScope.symbols["semantics"] = SymbolEntryList(singleton!SemanticsConstructDefinition());
    addConstruct(DumpScopeStackConstruct.definition);

    //
    // File System Operations
    //
    addConstruct(CurrentSourceFileStringConstruct.definition);
    /*
    addConstruct(BuildPathConstruct.definition);
    addConstruct(DirectoryOfConstruct.definition);
    addConstruct(CurrentSourceFileConstruct.definition);
    addConstruct(MkdirConstruct.definition);
    */
  }

  void setPatternMode(PatternMode mode)
  {
    this.patternMode = mode;
  }
  
  void pushScope(size_t startLineNumber, ScopeType type, bool openScope) pure
  {
    scopeStack.put(currentScope);
    currentScope = Scope(startLineNumber, type, openScope);
  }
  void pushScope(Scope existingScope) pure
  {
    scopeStack.put(currentScope);
    currentScope = existingScope;
  }
  void popScope() pure
  {
    auto scopeStackLength = scopeStack.data.length;
    if(scopeStackLength == 0) {
      throw new Exception("Attempted to pop the scope stack when there are no scopes on the stack");
    }
    // restore mode
    currentScope = scopeStack.data[scopeStackLength-1];
    if(currentScope.statementModeToRestoreOnPop) {
      pureLogDev("popScope: restoring statement mode");
      statementMode = currentScope.statementModeToRestoreOnPop;
      currentScope.statementModeToRestoreOnPop = null;
    }
    scopeStack.shrinkTo(scopeStackLength-1);
  }
  void setStatementMode(const(ConstructStatementMode) mode)
  {
    if(currentScope.statementModeToRestoreOnPop is null) {
      currentScope.statementModeToRestoreOnPop = statementMode.unconst;
    }
    statementMode = mode.unconst;
  }

  // Assumption: sourceFile.relativeName
  final void import_(size_t importLineNumber, bool relative, string importName)
  {
    ConstructParser parser;
    SourceFile importSourceFile;
    if(relative) {
      importSourceFile.relativeName = buildNormalizedPath(currentFile.relativeName.dirName, importName).setExtension(".con");
      importSourceFile.absoluteName = buildNormalizedPath(importSourceFile.relativeName.absolutePath);
      auto importEntry = importAbsoluteFilenameMap.get(importSourceFile.absoluteName, null);
      if(importEntry) {
        logDebug("relative import \"%s\" has already been imported (%s)", importName, importSourceFile.absoluteName);
        return;
      }
      parser = standardParser!(Appender!(const(ConstructObject)[]));
    } else {

      if(auto importEntry = importMap.get(importName, null)) {
        if(importEntry) {
          logDebug("\"%s\" was already imported", importName);
          return;
        }
      }

      // Find the import file
      // loop will set parser if it finds the file
      logInfo("searching for import '%s'...", importName);
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

        importSourceFile.relativeName = buildNormalizedPath(importPath.relativePath, nameSubPath.setExtension(".con"));
        if(exists(importSourceFile.relativeName)) {
          parser = standardParser!(Appender!(const(ConstructObject)[]));
          logInfo("    FOUND AT: %s", importSourceFile.relativeName);
          break;
        } else {
          logInfo("NOT FOUND AT: %s", importSourceFile.relativeName);

          importSourceFile.relativeName = importSourceFile.relativeName.stripExtension;
          if(exists(importSourceFile.relativeName)) {
            parser = standardParser!(Appender!(const(ConstructObject)[]));
            logInfo("    FOUND AT: %s", importSourceFile.relativeName);
            break;
          } else {
            logInfo("NOT FOUND AT: %s", importSourceFile.relativeName);
          }
        }
      }

      if(!parser.name) {
        throw semanticError(importLineNumber, format("import \"%s\" not found", importName));
      }

      importSourceFile.absoluteName = buildNormalizedPath(importSourceFile.relativeName.absolutePath);

      // check if the filename has already been imported using a relative import
      if(auto importEntry = importAbsoluteFilenameMap.get(importSourceFile.absoluteName, null)) {
        if(importEntry.importName == null) {
          importEntry.importName = importName;
        } else if(importEntry.importName != importName) {
          throw new Exception(format("file imported with different names '%s' and '%s'", importEntry.importName, importName));
        }
        logInfo("import '%s' has already been imported somewhere else, but must have been a relative import", importName);
        importMap[importName] = importEntry;
        return;
      }
    }


    //
    // The file has not been imported yet
    //
    importSourceFile.source = cast(string)read(importSourceFile.relativeName);
    importSourceFile.parsedObjects = parser.func(importSourceFile.source);

    auto importFileEntry = new ImportFile(importName, importSourceFile);

    // Important to add the import entries before processing the import file
    if(!relative) {
      importMap[importName] = importFileEntry;
      logDebug("Added '%s' to import map", importName);
    }
    importAbsoluteFilenameMap[importSourceFile.absoluteName] = importFileEntry;
    logDebug("Added '%s' to import absolute filename map", importSourceFile.absoluteName);

    //
    // Process the import file
    //
    logDebug("Processing import file %s...", importSourceFile.relativeName);
    importFileEntry.publicSymbols = process(importSourceFile);
    logDebug("Done processing import file %s...", importSourceFile.relativeName);
  }

  void addSymbol(string symbol, const(ConstructObject) object) pure
  {
    Scope* addTo;
    if(currentScope.addSymbolsTo) {
      addTo = currentScope.addSymbolsTo;
    } else {
      addTo = &currentScope;
    }

    auto entry = addTo.symbols.get(cast(string)symbol, SymbolEntryList(null));
    if(!entry.first) {
      addTo.symbols[symbol] = SymbolEntryList(object.unconst);
      pureLogDebug("Added symbol '%s' to current scope", symbol);
    } else {
      throw imp(format("multiple symbol objects with the same name (trying to add symbol '%s')", symbol));
    }
  }

  void setSymbol(size_t setLineNumber, const(char)[] symbol, const(ConstructObject) object) pure
  {
    auto scope_ = &currentScope;
    auto scopeIndex = scopeStack.data.length;
    while(true) {
      auto entry = scope_.symbols.get(cast(string)symbol, SymbolEntryList(null));
      if(entry.first) {
        if(entry.moreCount) {
          imp("symbol with multiple entries");
        }
        scope_.symbols[symbol] = SymbolEntryList(object.unconst);
        return;
      }

      if(scopeIndex == 0) {
        throw new SemanticException(setLineNumber, &this, format("Cannot set symbol '%s' because it doesn't exist", symbol));
      }
      scopeIndex--;
      scope_ = &scopeStack.data[scopeIndex];
    }
  }
  void letSetSymbol(const(char)[] symbol, const(ConstructObject) object) pure
  {
    auto entry = currentScope.symbols.get(cast(string)symbol, SymbolEntryList(null));
    if(entry.first) {
      if(entry.moreCount) {
        throw imp("symbol with multiple entries");
      }
      currentScope.symbols[symbol] = SymbolEntryList(object.unconst);
    } else {
      currentScope.symbols[symbol] = SymbolEntryList(object.unconst);
    }
  }

  const(T) lookupSymbol(T)(const(ConstructSymbol) symbol)
  {
    return lookupSymbol!T(symbol.lineNumber, symbol.value);
  }
  const(T) lookupSymbol(T)(size_t lineNumber, const(char)[] symbol)
  {
    // Search current scope
    SymbolEntryList match = currentScope.symbols.get(cast(string)symbol, SymbolEntryList(null));
    if(match.first) {
      if(auto object = match.symbolAs!T) {
        return object;
      }
    }

    // Search parent scopes
    foreach_reverse(scope_; scopeStack.data) {
      auto anotherMatch = scope_.symbols.get(cast(string)symbol, SymbolEntryList(null));
      if(anotherMatch.first) {
        if(auto object = anotherMatch.symbolAs!T) {
          return object;
        }
      }
    }

    // Search other files
    printScopeStack();
    imp(format("import from other files (symbol=%s)", symbol));
    //foreach(fileAndCode; importMap.byKeyValue) {
    //}


    if(match.first) {
      throw semanticError(lineNumber, format
                          ("symbol '%s' of type '%s' cannot be converted to %s", symbol, match.first.typeName, An(T.staticTypeName)));
    }

    throw semanticError(lineNumber, format
                        ("%s '%s' does not exist", T.staticTypeName, symbol));
  }

  // Cannot be const because it returns a non-const reference to the scope symbol table
  ScopeAndEntryList tryLookupSymbolAndScope(const(char)[] symbol) pure
  {
    // Search current scope
    {
      auto match = currentScope.symbols.get(cast(string)symbol, SymbolEntryList(null));
      if(match.first) {
        return ScopeAndEntryList(currentScope.type, currentScope.symbols, match.unconst);
      }
    }

    // Search parent scopes
    foreach_reverse(scope_; scopeStack.data) {
      auto match = scope_.symbols.get(cast(string)symbol, SymbolEntryList(null));
      if(match.first) {
        return ScopeAndEntryList(scope_.type, scope_.symbols, match.unconst);
      }
    }

    // Search other files
    if(!__ctfe) {
      foreach(fileKeyValue; importAbsoluteFilenameMap.byKeyValue) {
	auto importFile = fileKeyValue.value;
	auto match = importFile.publicSymbols.get(cast(string)symbol, SymbolEntryList(null));
	if(match.first) {
          return ScopeAndEntryList(ScopeType.file, importFile.publicSymbols, match.unconst);
	}
      }
    }

    return ScopeAndEntryList(ScopeType.file, null, SymbolEntryList());
  }

  const(SymbolEntryList) tryLookupSymbol(const(char)[] symbol) const pure
  {
    return this.unconst.tryLookupSymbolAndScope(symbol).entryList;
  }


  void printScopeStack() pure
  {
    import std.stdio : writefln, stdout;
    debug {
      auto prefix = "";
      size_t scopeNumber = 0;
      foreach(scope_; scopeStack.data) {
        writefln("%sScope %s (type=%s %s symbols)", prefix, scopeNumber, scope_.type, scope_.symbols.length);
        prefix ~= "  ";
        foreach(symbol; scope_.symbols.byKeyValue) {
          writefln("%s%s : %s", prefix, symbol.key, symbol.value);
        }
        prefix ~= "  ";
        scopeNumber++;
      }
      writefln("%sScope %s (type=%s %s symbols)", prefix, scopeNumber, currentScope.type, currentScope.symbols.length);
      prefix ~= "  ";
      foreach(symbol; currentScope.symbols.byKeyValue) {
        writefln("%s%s : %s", prefix, symbol.key, symbol.value);
      }
      stdout.flush();
    }
  }


  auto semanticError(size_t lineNumber, string msg) const
  {
    return new SemanticException(lineNumber, &this, msg);
  }
  auto constructError(const(ConstructSymbol) constructSymbol, string msg) const
  {
    return new SemanticException(constructSymbol.lineNumber, &this, msg);
  }
  auto endedInsideConstruct(const(ConstructSymbol) constructSymbol) const pure
  {
    if(currentScope.type == ScopeType.file) {
      return new SemanticException(constructSymbol.lineNumber, &this,
                                   format("file ended inside construct '%s'", constructSymbol.value));
    } else {
      return new SemanticException(constructSymbol.lineNumber, &this,
                                   format("construct '%s' was not terminated", constructSymbol.value));
    }
  }

  // Returns the public symbols
  SymbolTable process(SourceFile sourceFile)
  {
    auto previousFile = currentFile;
    currentFile = sourceFile;
    scope(exit) { currentFile = previousFile; }

    pushScope(1, ScopeType.file, false);
    try {
      auto result = processBlock(sourceFile.parsedObjects);
      if(result.isReturn) {
        throw semanticError(result.lineNumber, "can only return inside a construct");
      }
      if(result.hasAction) {
        throw imp("a root level action construct");
      }
      if(result.object) {
        throw semanticError(result.lineNumber, "unhandled expression at root level");
      }
      return currentScope.symbols;
    } finally {
      popScope();
    }
  }
  const(ConstructResult) processConstructImplementationBlock(const(ConstructObject)[] objects, const(ConstructDefinition) constructDefinition)
  {
    ConstructDefinition previousConstruct = currentConstruct;
    currentConstruct = constructDefinition.unconst;
    scope(exit) {
      currentConstruct = previousConstruct;
    }
    // Remove return action
    return processBlock(objects).withNoReturn();
  }
  const(ConstructResult) processBlock(const(ConstructObject)[] objects)
  {
    size_t index = 0;
    size_t lineNumber = 0;
    while(index < objects.length) {
      ConstructResult result;

      auto rawObject = objects[index];
      if(auto block = rawObject.tryAsConstructBlock) {
        pushScope(block.lineNumber, ScopeType.block, true);
        scope(exit) { popScope(); }
        result = processBlock(block.objects).unconst;
        index++;
      } else {
        lineNumber = rawObject.lineNumber;
        result = consumeValueAlreadyCheckedIndex
          (NoConstructContext.instance, objects, &index).unconst;
      }

      if(statementMode is ConstructStatementMode.default_) {
	// Optimization
	DefaultStatementModeConstruct.handle(&this, result.object, result.action);
      } else {
	auto saveStatementMode = statementMode;
	statementMode = ConstructStatementMode.default_.unconst;
	scope(exit) {
	  statementMode = saveStatementMode;
	}
	ObjectOrSize[3] args;
	args[0] = ObjectOrSize(0);
	args[1] = ObjectOrSize(result.object);
	args[2] = ObjectOrSize(result.action);
	auto statementHandler = saveStatementMode.handlerConstruct.noOpPatternHandlers[0];
	auto statementHandlerResult = statementHandler.handler(&this, saveStatementMode.handlerConstruct,
							       null,
							       statementHandler.handlerObject.unconst,
							       statementHandler.patternNodes, args);
      }

      if(result.isReturn) {
        return result;
      }
    }
    return ConstructResult(null);
  }

  const(ConstructSymbol) consumeSymbol(const(IConstructContext) constructContext,
                                       const(ConstructSymbol) context, const(ConstructObject)[] objects, size_t* index)
  {
    auto symbol = tryConsumeSymbol(constructContext, objects, index);
    if(!symbol) {
      throw semanticError(context.lineNumber, format
                          ("construct '%s' expected a symbol", context.value));
    }
    return symbol;
  }
  const(ConstructSymbol) tryConsumeSymbol(const(IConstructContext) constructContext,
                                          const(ConstructObject)[] objects, size_t* index)
  {
    size_t nextIndex = *index;
    if(nextIndex >= objects.length) {
      return null;
    }
    auto object = objects[nextIndex++];
    auto symbol = object.tryAsConstructSymbol;
    if(!symbol) {
      return null;
    }

    // todo: remove the 'sym' keyword
    if(symbol.value != "sym") {
      *index = nextIndex;
      return symbol;
    }

    if(nextIndex >= objects.length) {
      //return null;
      throw semanticError(symbol.lineNumber, "no objects followed the 'sym' keyword");
    }

    auto result = consumeValueAlreadyCheckedIndex(constructContext, objects, &nextIndex).unconst;
    if(result.hasAction) {
      throw imp("tryConsumeSymbol, result.hasAction is true!");
    }
    if(!result.object) {
      throw semanticError(symbol.lineNumber, "The 'sym' keyword expects an expression that evaluates to a symbol or string but evaulated to null");
    }
    if(auto resolvedSymbol = result.object.tryAsConstructSymbol) {
      *index = nextIndex;
      return resolvedSymbol;
    }
    if(auto resolvedString = result.object.tryAsConstructString) {
      *index = nextIndex;
      return new ConstructSymbol(resolvedString.lineNumber, resolvedString.toUtf8());
    }
    throw semanticError(symbol.lineNumber, format("The 'sym' keyword expects an expression that evaluates to a symbol or string but evaulated to %s", result.object.typeName));
  }

  // appendArg format:
  // If a pattern node count type is:
  //   case "one"      : appendArg with a ConstructObject, should NEVER be null
  //   case "optional" : appendArg with ConstructObject, will be null if node value was not present
  //   case "many" and "oneOrMore": appendArg with a count, and then that many arguments
  bool matchPattern(size_t stackSize)(const(ConstructSymbol) constructSymbol, const(IConstructContext) constructContext,
                                      const(PatternNode)[] patternNodes, const(ConstructObject)[] objects, size_t* index,
                                      ref PatternMatchStorage!stackSize argumentBuilder)
  {
  PATTERN_NODE_LOOP:
    foreach(nodeIndex,node; patternNodes) {
      //logDev("  construct '%s' matching pattern node %d '%s' of type '%s' (*index=%s, object.length=%s)", constructSymbol.value, nodeIndex, node.name, node, *index, objects.length);
      size_t nextIndex = *index;
      if(nextIndex >= objects.length) {
        if(node.name == "_") {
          if(!node.countType.isOptional) {
	    //logDev("  matchPattern: not a match 1");
            return false;
          }
        } else {
          if(node.countType == CountType.optional) {
            argumentBuilder.put(ObjectOrSize(null));
          } else if(node.countType == CountType.many) {
            argumentBuilder.put(ObjectOrSize(0));
          } else {
	    //logDev("  matchPattern: not a match 2");
            return false;
          }
        }
        continue;
      }

      ConstructObject value;
      if(node.raw) {
	value = objects[nextIndex++].unconst;
	// handle the special macro construct
	if(auto symbol = value.tryAsConstructSymbol) {
	  if(symbol.value == "macro") {
	    auto result = consumeValueAlreadyCheckedIndex(NoConstructContext.instance, objects, &nextIndex).unconst;
            if(result.hasAction) {
              throw imp("matchPattern, result has an action(1)!");
            }
            value = result.object;
	  }
	}
      } else {
	auto result = consumeValueAlreadyCheckedIndex(constructContext, objects, &nextIndex).unconst;
        if(result.hasAction) {
          throw imp("matchPattern, result has an action(2)!");
        }
        value = result.object;
      }
      if(value is null) {
	throw imp(format("value is null while matching '%s' value for construct '%s'", node, constructSymbol.value));
      }

      // todo: handle returns

      if(!node.type.supportsValue(value)) {
        // NOTE: this block same as the *index >= objects.length block
        if(node.name == "_") {
          if(!node.countType.isOptional) {
            //logDev("  matchPattern: not a match 4");
            return false; // not a match
          }
        } else {
          if(node.countType == CountType.optional) {
            argumentBuilder.put(ObjectOrSize(null));
          } else if(node.countType == CountType.many) {
            argumentBuilder.put(ObjectOrSize(0));
          } else {
            //logDev("  matchPattern: not a match 3 (node is '%s' object is '%s' (type=%s))", node, value, value.typeName);
            return false; // not a match
          }
        }
        continue PATTERN_NODE_LOOP;
      }

      //logDev("    matcher '%s' matched '%s'", node, value);
      *index = nextIndex; // consume the object

      if(node.countType == CountType.one) {
        if(node.name != "_") {
          argumentBuilder.put(ObjectOrSize(value));
        }
        continue PATTERN_NODE_LOOP;
      }
      if(node.countType == CountType.optional) {
        if(node.name != "_") {
          //appendArg(new ConstructOptionalValue(value.lineNumber, value));
          argumentBuilder.put(ObjectOrSize(value));
        }
        continue PATTERN_NODE_LOOP;
      }

      ObjectOrSize* sizeReference = null;
      if(node.name != "_") {
        sizeReference = argumentBuilder.getReferenceAndMoveNext();
      }
      size_t nodeArgumentCount = 0;
      while(true) {
        argumentBuilder.put(ObjectOrSize(value));
        nodeArgumentCount++;

        if(nextIndex >= objects.length) {
          break;
        }
        
        if(node.raw) {
          value = objects[nextIndex++].unconst;
          // handle the special macro construct
          if(auto symbol = value.tryAsConstructSymbol) {
            if(symbol.value == "macro") {
              auto result = consumeValueAlreadyCheckedIndex(NoConstructContext.instance, objects, &nextIndex).unconst;
              if(result.hasAction) {
                throw imp("matchPattern, result has an action(3)!");
              }
              value = result.object;
            }
          }
        } else {
          auto result = consumeValueAlreadyCheckedIndex(constructContext, objects, &nextIndex).unconst;
          if(result.hasAction) {
            throw imp("matchPattern, result has an action(4)!");
          }
          value = result.object;
        }
        
        if(value is null) {
          // TODO: we should save the value that was consumed so that it
          //       does not need to be processed again
          break;
        }

        if(!node.type.supportsValue(value)) {
          break;
        }
        //logDev("    matcher '%s' matched '%s'", node, value);
        *index = nextIndex; // consume the object
      }
      if(node.name != "_") {
        sizeReference.size = nodeArgumentCount;
      }
    }
    return true; // match
  }


  struct PatternMatchStorage(size_t stackSize)
  {
    ObjectOrSize[stackSize] stackBuffer;
    size_t totalSize;
    void put(const(ObjectOrSize) objOrSize)
    {
      if(totalSize >= stackSize) {
        throw imp("PatternMatchStorage not big enough");
      }
      stackBuffer[totalSize++] = objOrSize;
    }
    ObjectOrSize* getReferenceAndMoveNext()
    {
      if(totalSize >= stackSize) {
        throw imp("PatternMatchStorage not big enough");
      }
      ObjectOrSize* next = &stackBuffer[totalSize];
      totalSize++;
      return next;
    }
    @property ObjectOrSize[] data()
    {
      return stackBuffer[0..totalSize];
    }
  }

  const(ConstructResult) processPatternConstruct(const(ConstructSymbol) constructSymbol, const(ConstructDefinition) con, bool hasOperatorObject,
                                                 const(ConstructObject) opParam, const(PatternHandler)[] patternHandlers, const(ConstructObject)[] objects, size_t* index)

  {
    //enum STACK_STORAGE_COUNT = 512;
    enum STACK_STORAGE_COUNT = 10; // use 10 for development

    PatternMatchStorage!STACK_STORAGE_COUNT args;

    size_t appendStartIndex;
    if(hasOperatorObject) {
      args.put(ObjectOrSize(opParam));
      appendStartIndex = 1;
    } else {
      appendStartIndex = 0;
    }
    //logDev("construct '%s' is %s", constructSymbol.value, (con.opParam.matcher is null) ? "NULL" : "NOT NULL");

    PatternHandler matched = PatternHandler();
    // go in reverse order because most recent constructs will be at the
    // end of the list
    foreach_reverse(patternHandlerIndex, patternHandler; patternHandlers) {
      size_t nextIndex;
      nextIndex = *index;
      args.totalSize = appendStartIndex;
      //logDev("Attempting to match pattern %d for construct '%s'...", patternHandlerIndex, constructSymbol.value);
      if(matchPattern!STACK_STORAGE_COUNT(constructSymbol, con, patternHandler.patternNodes, objects, &nextIndex, args)) {
	matched = patternHandler.unconst;
        *index = nextIndex;
        break;
      }
    }

    if(matched.handler == null) {
      throw semanticError(constructSymbol.lineNumber, format("construct '%s' had no pattern that matched the following contruct objects", constructSymbol.value));
    }

    //
    // Print the arguments
    //
    /*
    logDev("Construct '%s':", constructSymbol.value);
    if(hasOperatorObject) {
      printPatternArguments(matched.patternNodes, args.data[1..$]);
    } else {
      printPatternArguments(matched.patternNodes, args.data);
    }
    logDev("-----------");
    */

    return matched.handler(&this, con, constructSymbol, matched.handlerObject, matched.patternNodes, args.data);
  }


  const(ConstructResult) consumeValue(const(IConstructContext) constructContext, const(ConstructSymbol) constructSymbol,
                                      const(ConstructObject)[] objects, size_t* index)
  {
    if(*index >= objects.length) {
      throw endedInsideConstruct(constructSymbol);
    }
    return consumeValueAlreadyCheckedIndex(constructContext, objects, index);
  }

  // TODO: this function needs to know the current precedence.
  //       Construct Precedence (very low)
  //       The '+' operator precedence (higher than construct precendence)
  ///      The '*' operator precedence (higher than '+' operator precedence)
  //       The '.' operator precedence (higher than '*' operator precedence
  //
  // These operator precedences should be defined in the construct code. Maybe like this:
  //
  //   operatorPrecedence * higherThan +
  //   operatorPrecedence . higherThan *
  //
  // Assumption: *index < objects.length
  const(ConstructResult) consumeValueAlreadyCheckedIndex(const(IConstructContext) constructContext,
                                                         const(ConstructObject)[] objects, size_t* index)
  {
    assert(*index < objects.length);

    //
    // Get the next object, if it is a construct, then process it, and use that object
    //
    auto result = ConstructResult(objects[*index].unconst);
    (*index)++;
    if(auto symbol = result.object.tryAsConstructSymbol.unconst) {

      logDebug("looking up symbol '%s' on line %s...", symbol.value, symbol.lineNumber);
      auto symbolEntries = tryLookupSymbol(symbol.value);
      if(!symbolEntries.first) {
        throw semanticError(symbol.lineNumber, format("symbol '%s' does not exist", symbol.value));
      }
      if(symbolEntries.moreCount > 0) {
        throw semanticError(symbol.lineNumber, format("symbol '%s' has multiple entries at the same scope", symbol.value));
      }

      if(auto definition = symbolEntries.first.tryAsConstructDefinition) {
        //logDev("(inside construct '%s') processing '%s'...", precedence.getOperatorString(), symbol.value);
        if(auto noPatternConstruct = definition.tryAsNoPatternConstruct()) {
          result = noPatternConstruct.processNoPattern(&this, symbol, null, objects, index).unconst;
        } else {
          auto patternHandlers = definition.getPatternHandlers(null);
          if(patternHandlers is null) {
            throw semanticError(symbol.lineNumber, format
                                ("the %s construct is missing an object to operate on (it only has 'this' patterns)", symbol.value));
          }
          result = processPatternConstruct(symbol, definition, false, null, patternHandlers, objects, index).unconst;
        }
        //logDev("(inside construct '%s') done processing '%s'...", precedence.getOperatorString(), symbol.value);

      } else {
        result.object = symbolEntries.first.tryAsConstructObject.unconst;
      }
    }

    //
    // Check if the next object is a construct that could
    // consume this value with a highier precedence
    //
    while(*index < objects.length) {
      auto nextObject = objects[*index];
      auto symbol = nextObject.tryAsConstructSymbol;
      if(!symbol) {
        //logDev("  next object is not a symbol");
	break; // object is not a symbol
      }

      //logDev("looking up symbol '%s' on line %s to see if it is an operation with higher precedence...", symbol.value, symbol.lineNumber);
      auto symbolEntries = tryLookupSymbol(symbol.value);
      //logDev("symbol '%s' entry is %s", symbol.value, symbolEntries);
      if(!symbolEntries.first) {
	//logDev("  '%s' does not have a value in the symbol table", symbol.value);
	break; // symbol does not exist
      }

      if(symbolEntries.moreCount > 0) {
	throw semanticError(symbol.lineNumber, format("symbol '%s' has multiple entries at the same scope", symbol.value));
      }

      //logDev("Checking if symbol '%s' is an operator construct", symbol.value);
      auto definition = symbolEntries.first.tryAsConstructDefinition;
      if(!definition) {
	//logDev("symbol '%s' is not a construct", symbol.value);
	break; // symbol is not a construct
      }
      if(!definition.hasOperatorPatterns()) {
	//logDev("construct '%s' is not an operator construct", symbol.value);
	break; // symbol is not an operator construct
      }

      // Check precedence
      if(constructContext.hasOperatorPatterns()) {
        auto currentOpString = constructContext.getConstructName();
        if(currentOpString) {
          if(!Precedence.greaterThan(symbol.value, currentOpString)) {
            //logDev("operator construct '%s' precedence %s is not higher then the current operator '%s' precedence %s", symbol.value, Precedence.getPrecedence(symbol.value), currentOpString, Precedence.getPrecedence(symbol.value));
            break; // precedence is not high enough
          }
          //logDev("Operator construct '%s' is higher precedence than '%s'", definition.name, currentOpString);
        }
      }

      // Check if it supports this object
      auto patternHandlers = definition.getPatternHandlers(result.object);
      if(!patternHandlers) {
	//logDev("construct '%s' is not an operator construct", symbol.value);
	break; // this construct operator does not support this object
      }

      (*index)++;
      //logDev("--> %s is an operator construct with a higher precedence", symbol.value);
      result = processPatternConstruct(symbol, definition, true, result.object, patternHandlers, objects, index).unconst;
      //logDev("<-- %s is done being processed", symbol.value);
    }

    return result;
  }


  const(T) consumeTypedValue(T)(const(ConstructDefinition) constructDefinition, const(ConstructSymbol) constructSymbol,
                                const(ConstructObject)[] objects, size_t* argIndex) if( !is( T == ConstructSymbol) )
  {
    auto result = consumeValue(constructDefinition, constructSymbol, objects, argIndex);
    if(result.hasAction) {
      throw semanticError(constructSymbol.lineNumber, format
                          ("the %s construct expects %s but the resulting construct produced a %s action", constructSymbol.value, An(T.staticTypeName), result.action));
    }
    if(result.object is null) {
      throw semanticError(constructSymbol.lineNumber, format
                          ("the %s construct expects %s but got no value", constructSymbol.value, An(T.staticTypeName)));
    }
    auto value = result.object.tryAs!T;
    if(!value) {
      throw semanticError(constructSymbol.lineNumber, format
                          ("the %s construct expects %s but got %s",
                           constructSymbol.value, An(T.staticTypeName), An(result.object.typeName)));
    }
    return value;
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


  auto valueObj = new ConstructUtf8(1, "mysymbol string value");
  processor.addSymbol("mysymbol", valueObj);

  testLookup(valueObj, "mysymbol");

  processor.pushScope(2, ScopeType.block, true);
  testLookup(valueObj, "mysymbol");
  processor.popScope();

  testLookup(valueObj, "mysymbol");

  /*
  //
  // Test openScope
  //
  processor.pushScope(2, true);
  assert(valueObj == processor.tryLookupSymbol("mysymbol"));
  processor.popScope();

  //
  // Test addSymbolsToParentScope
  //
  processor.pushScope(3, false);
  assert(!processor.tryLookupSymbol("mysymbol"));

  auto goToParentObj = new ConstructString(1, "the value of goToParent");
  processor.addSymbol("goToParent", goToParentObj);
  assert(!processor.tryLookupSymbol("goToParent"));
  processor.popScope();
  assert(valueObj == processor.tryLookupSymbol("mysymbol"));
  assert(goToParentObj == processor.tryLookupSymbol("goToParent"));
  */
}

alias ProcessorFunc = const(ConstructResult) function(ConstructProcessor* processor,
                                                      const(ConstructDefinition) definition,
                                                      const(ConstructSymbol) constructSymbol,
                                                      const(ConstructObject)[] objects, size_t* argIndex);
class FunctionConstructDefinition : NoPatternConstruct
{
  ProcessorFunc func;
  this(string name, size_t lineNumber, string filename, ConstructAttributes attributes, immutable ConstructType evalTo, ProcessorFunc func) pure immutable
  {
    super(name, lineNumber, filename, attributes, evalTo);
    this.func = func;
  }
  final override const(ConstructResult) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
							 const(ConstructObject) opParam, const(ConstructObject)[] objects, size_t* index) const
  {
    return func(processor, this, constructSymbol, objects, index);
  }
}
const(ConstructResult) handleConstructWithBlock
(ConstructProcessor* processor, const(ConstructDefinition) definition, const(ConstructSymbol) constructSymbol,
 Object handlerObject, const(PatternNode)[] patternNodes, const(ObjectOrSize)[] args)
{
  auto block = cast(const(ConstructBlock))handlerObject;
  assert(block, "code bug: handlerObject is not a ConstructBlock");

  processor.pushScope(block.lineNumber, ScopeType.defcon, true);
  scope(exit) { processor.popScope(); }

  // Prepare to call the construct by adding the objects that were setup
  //  by the matchPattern function, to the symbol table.
  size_t argIndex = 0;
  foreach(patternNode; patternNodes) {
    if(patternNode.name == "_") {
      continue;
    }
    if(argIndex >= args.length) {
      if(patternNode.countType.isOptional) {
        processor.addSymbol(patternNode.name, ConstructOptionalValue.Null);
        continue;
      }
      throw new Exception("code bug: handleConstructWithBlock expected more arguments then were returned by matchPattern");
    }
    
    auto arg = args[argIndex++];
    if(patternNode.countType == CountType.one) {
      if(!arg.obj) {
        throw new Exception("code bug: handleConstructWithBlock expects all patternNodes of count type 'one' to have a non-null object, but matchPattern set the object associated with the node to null");
      }
      processor.addSymbol(patternNode.name, arg.obj);
    } else if(patternNode.countType == CountType.optional) {
      if(arg.obj) {
        processor.addSymbol(patternNode.name, new ConstructOptionalValue(arg.obj.lineNumber, arg.obj));
      } else {
        processor.addSymbol(patternNode.name, ConstructOptionalValue.Null);
      }
    } else {
      if(arg.size == 0) {
        // sanity check
        if(patternNode.countType == CountType.oneOrMore) {
          throw new Exception("code bug: a oneOrMore pattern node has 0 objects");
        }
        // TODO: Maybe create a static ConstructList.NULL?
        processor.addSymbol(patternNode.name, new ConstructList(0, null));
      } else {
        // sanity check
        if(argIndex + arg.size > args.length) {
          throw new Exception("code bug: pattern node argument size is bigger than the given arguments");
        }
        // TODO: should I just pass in the type of the pattern node?
        processor.addSymbol(patternNode.name, new ConstructList(0, cast(const(ConstructObject)[])args[argIndex..argIndex+arg.size]));
        argIndex += arg.size;
      }
    }
  }
  if(argIndex < args.length) {
    throw new Exception("code bug: handleConstructWithBlock did not use all the arguments that were returned by matchPattern");
  }

  return processor.processConstructImplementationBlock(block.objects, definition);
}
/*
class ConstructWithBlockDefinition : PatternConstructDefinition
{
  const ConstructBlock block;
  this(string name, immutable(PatternHandler)[] noOpPatternHandlers,
       immutable(OpPatternHandlers)[] opPatternHandlers, size_t lineNumber, string filename, ConstructAttributes attributes,
       immutable(ConstructType) evalTo, immutable(ConstructBlock) block) immutable pure
  {
    // TODO: how to support OpParam
    super(name, noOpPatternHandlers, opPatternHandlers, lineNumber, filename, attributes, evalTo);
    this.block = block;
  }
}
*/

/+
class DeftypeConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, null, null, null); }
  /*
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
						   const(ConstructObject)[] objects, size_t* argIndex) const
  {
    auto typeName = processor.consumeSymbol(constructSymbol, objects, argIndex).value;
    ConstructType type;

    while(true) {
      auto object = processor.consumeValue(constructSymbol, objects, argIndex);
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
    return null;
  }
  */
}

/*
class CallerScopeConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    //auto forwardList =
    auto code = processor.consumeTypedValue!ConstructBlock(constructSymbol, objects, argIndex);

    if(processor.currentScope.type == ScopeType.defcon) {
      auto saveScope = processor.currentScope;
      processor.popScope();
      scope(exit) { processor.pushScope(saveScope); }

      return processor.processBlock(code.objects);
    } else {
      processor.printScopeStack();
      throw imp("callerScope construct when currentScope is not defcon type");
    }
  }
}
*/
class TypeOfConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, new PrimitiveType(0, PrimitiveTypeEnum.type), null, null); }
  /*
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    auto object = processor.consumeValue(constructSymbol, objects, argIndex);
    if(object is null) {
      throw processor.semanticError(constructSymbol.lineNumber, "the typeOf construct expects a value but got void");
    }
    return new PrimitiveType(0, object.primitiveType);
  }
  */
}
class TypedConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, null, null, null); }
  /*
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    auto type = processor.consumeTypedValue!ConstructType(constructSymbol, objects, argIndex);
    auto value = processor.consumeValue(constructSymbol, objects, argIndex);
    return value.typedAs(type);
  }
  */
}
/*
class StringAllocConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    auto type = processor.consumeTypedValue!ConstructType(constructSymbol, objects, &argIndex).unconst;
    auto size = processor.consumeTypedValue!ConstructUint(constructSymbol, objects, &argIndex).unconst;
    logDev("stringalloc type=%s", type);
    if(!type.asPrimitive.canBe(PrimitiveTypeEnum.string)) {
      throw processor.constructError(constructSymbol, format
				     ("expected a string type but got %s", an(type.asPrimitive)));
    }
    if(type.asPrimitive == PrimitiveTypeEnum.string) {

      // default string type is utf8 encoded
      //return new ConstructString(
      throw imp("stringAlloc type string");
    //} else if(type.asPrimitiveType == PrimitiveTypeEnum.utf8) {

    } else {
      throw imp(format("stringAlloc type %s", type));
    }
  }
}
*/
+/
void printPatternArguments(const(PatternNode)[] patternNodes, const(ObjectOrSize)[] args)
{
  logDev("--- Pattern Arguments -----------------------------");
  size_t argIndex = 0;
  foreach(patternIndex, patternNode; patternNodes) {
    if(patternNode.name == "_") {
      // No arguments are saved for nameless pattern nodes
      logDev("  [%s] _", patternIndex);
      continue;
    }
    if(argIndex >= args.length) {
      if(patternNode.countType.isOptional) {
        logDev("  [%s] <null>", patternIndex);
        continue;
      }
      throw new Exception("code bug: printPatternArguments: the pattern expected more arguments than were given");
    }
    if(patternNode.countType == CountType.one) {
      auto arg = args[argIndex++];
      if(!arg.obj) {
        throw new Exception("code bug: printPatternArguments expects all patternNodes of count type 'one' to have a non-null object");
      }
      logDev("  [%s] %s (%s)", patternIndex, arg.obj, arg.obj.typeName);
    } else if(patternNode.countType == CountType.optional) {
      auto arg = args[argIndex++];
      if(arg.obj) {
        logDev("  [%s] %s (%s)", patternIndex, arg.obj, arg.obj.typeName);
      } else {
        logDev("  [%s] <null>", patternIndex);
      }
    } else {
      logDev("  [%s] %s objects:", argIndex, args.length);
      size_t nodeObjectCount = args[argIndex++].size;
      if(patternNode.countType == CountType.oneOrMore) {
        if(nodeObjectCount == 0) {
          throw new Exception("code bug: printPatternArguments expects all patternNodes of count type 'oneOrMore' to have at least one object");
        }
      }
      for(size_t i = 0; i < nodeObjectCount; i++) {
        if(argIndex >= args.length) {
          throw new Exception(format("code bug: printPatternArguments: the pattern node indicates it has matched %s arguments but not enough arguments were provided", nodeObjectCount));
        }
        auto arg = args[argIndex++];
        if(!arg.obj) {
          throw new Exception(format("code bug: printPatternArguments: one of the objects in a patternNode of count type %s was unexpectedly null", patternNode.countType));
        }
        logDev("    - %s (%s)", arg.obj, arg.obj.typeName);
      }
    }
  }

  if(argIndex != args.length) {
    logDev("  CODE_BUG: pattern recognized %d arguments but there are %d", argIndex, args.length);
    /*
    for(;argIndex < args.length; argIndex++) {
      logDev("  - %s (%s)", args[argIndex].obj, args[argIndex].obj.typeName);
    }
    */
  }
}

  
