module construct.processor;

import std.file   : exists, read;
import std.format : format;
import std.path   : setExtension, stripExtension, buildNormalizedPath;
import std.array  : Appender, appender, isDynamicArray, ElementEncodingType;
import std.string : startsWith, indexOf;
import std.conv   : to;
import std.format : formattedWrite/*, format*/;

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
    super(msg, cast(string)processor.currentFile.name, lineNumber);
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
      filename = processor.currentFile.name;
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
class ConstructAssertException: ConstructException
{
  Appender!(SemanticException[]) errorList;
  this(size_t lineNumber, const(ConstructProcessor)* processor, string msg) pure
  {
    const(char)[] filename;
    if(processor.currentConstruct && processor.currentConstruct.filename.length > 0) {
      filename = processor.currentConstruct.filename;
    } else {
      filename = processor.currentFile.name;
    }
    super(msg, cast(string)filename, lineNumber);
  }
}

alias SymbolEntryList = OneOrMore!ISymbolObject;
alias SymbolTable = SymbolEntryList[string];

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

  SymbolTable publicSymbols;

  this(const(char)[] importName, SourceFile sourceFile, const(char)[] code) pure
  {
    this.importName = importName;
    this.sourceFile = sourceFile;
    this.code = code;
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
  ImportFile[const(char)[]] importMap;

  SourceFile currentFile;

  Appender!(Scope[]) scopeStack;
  Scope currentScope;

  ConstructDefinition currentConstruct;

  this(ImportPath[] importPaths) pure
  {
    this.importPaths = importPaths;
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
       "constructBreak": SymbolEntryList(constructBreakConstructDefinition.unconst),
       //
       // Types
       //
       "anything"      : SymbolEntryList(anythingConstructDefinition.unconst),
       "list"          : SymbolEntryList(listConstructDefinition.unconst),
       "bool"          : SymbolEntryList(boolConstructDefinition.unconst),
       "number"        : SymbolEntryList(numberConstructDefinition.unconst),
       "uint"          : SymbolEntryList(uintConstructDefinition.unconst),
       "symbol"        : SymbolEntryList(symbolConstructDefinition.unconst),
       "string"        : SymbolEntryList(stringConstructDefinition.unconst),
       "pointer"       : SymbolEntryList(pointerConstructDefinition.unconst),
       "type"          : SymbolEntryList(typeConstructDefinition.unconst),
       "constructBlock": SymbolEntryList(constructBlockConstructDefinition.unconst),
       "class"         : SymbolEntryList(classConstructDefinition.unconst),
       //
       // Basic Constructs
       //
       "pattern"       : SymbolEntryList(PatternConstruct.definition.unconst),
       ];
  }

  private void addConstruct(const(ConstructDefinition) definition)
  {
    currentScope.symbols[definition.name] = SymbolEntryList(definition.unconst);
  }
  void loadExtendedConstructs()
  {
    addConstruct(ImportConstruct.definition);
    addConstruct(DefconConstruct.definition);
    addConstruct(ToSymbolConstruct.definition);
    //currentScope.symbols["deftype"]   = SymbolEntryList(singleton!DeftypeConstructDefinition());
    addConstruct(ExecConstruct.definition);
    //addConstruct(ThrowConstruct.definition);
    addConstruct(AssertConstruct.definition);
    addConstruct(tryConstructDefinition);
    addConstruct(LetConstruct.definition);
    addConstruct(SetConstruct.definition);
    addConstruct(LetSetConstruct.definition);
    addConstruct(ifConstructDefinition);
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
    // Operators
    //
    //addConstruct(DotConstruct.definition);
    addConstruct(PlusConstruct.definition);
    addConstruct(MultiplyConstruct.definition);
    addConstruct(EqualsConstruct.definition);

    //
    // String Operations
    //
    addConstruct(ByteLengthConstruct.definition);
    addConstruct(StrcpyConstruct.definition);

    //
    // Modes
    //
    addConstruct(DefmodeConstruct.definition);
    
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
        auto importCode = cast(string)read(importFile.name);
	importFile.parsedObjects = parser.func(importCode);

        // Have to do this before processing the import source
        // so that it doesn't try to load the same import twice
        auto importFileEntry = new ImportFile(importName, importFile, importCode);
        importMap[importName] = importFileEntry;
        logDebug("Added import '%s' to map", importName);
        importFileEntry.publicSymbols = process(importFile);
	return;
      }
    }

    throw new SemanticException(importLineNumber, &this, format("import '%s' not found", importName));
  }

  void addSymbol(string symbol, const(ISymbolObject) object) pure
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

  void setSymbol(size_t setLineNumber, const(char)[] symbol, const(ISymbolObject) object) pure
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
  void letSetSymbol(const(char)[] symbol, const(ISymbolObject) object) pure
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
      foreach(fileKeyValue; importMap.byKeyValue) {
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
      if(result) {
        if(auto return_ = result.tryAsConstructReturn) {
          throw semanticError(result.lineNumber, "can only return inside a construct");
        }
        throw semanticError(result.lineNumber, "unhandled expression at root level");
      }
      return currentScope.symbols;
    } finally {
      popScope();
    }
  }
  const(ConstructObject) processConstructImplementationBlock(const(ConstructObject)[] objects, const(ConstructDefinition) constructDefinition)
  {
    ConstructDefinition previousConstruct = currentConstruct;
    currentConstruct = constructDefinition.unconst;
    scope(exit) {
      currentConstruct = previousConstruct;
    }
    auto result = processBlock(objects);
    if(result) {
      if(auto return_ = result.tryAsConstructReturn) {
        return return_.returnValue;
      }
    }
    return result;
  }
  const(ConstructObject) processBlock(const(ConstructObject)[] objects)
  {
    size_t index = 0;
    size_t lineNumber = 0;
    while(index < objects.length) {
      ConstructObject result;

      auto rawObject = objects[index];
      if(auto block = rawObject.tryAsConstructBlock) {
        pushScope(block.lineNumber, ScopeType.block, true);
        scope(exit) { popScope(); }
        result = processBlock(block.objects).unconst;
        index++;
      } else {
        lineNumber = rawObject.lineNumber;
        result = consumeValueAlreadyCheckedIndex
          (DefaultPrecedenceConsumer.instance, objects, &index).unconst;
      }

      if(result) {
        if(auto return_ = result.tryAsConstructReturn) {
          return return_;
        }
        throw semanticError(result.lineNumber, format
                            ("unhandled statement value (type %s)", result.typeName));
      }
    }
    return null;
  }

  const(ConstructSymbol) consumeSymbol(const(IPrecedenceConsumer) precedenceConsumer,
                                       const(ConstructSymbol) context, const(ConstructObject)[] objects, size_t* index)
  {
    auto symbol = tryConsumeSymbol(precedenceConsumer, objects, index);
    if(!symbol) {
      throw semanticError(context.lineNumber, format
                          ("construct '%s' expected a symbol", context.value));
    }
    return symbol;
  }
  const(ConstructSymbol) tryConsumeSymbol(const(IPrecedenceConsumer) precedenceConsumer,
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

    if(symbol.value != "sym") {
      *index = nextIndex;
      return symbol;
    }

    if(nextIndex >= objects.length) {
      //return null;
      throw semanticError(symbol.lineNumber, "no objects followed the 'sym' keyword");
    }

    auto result = consumeValueAlreadyCheckedIndex(precedenceConsumer, objects, &nextIndex).unconst;
    if(!result) {
      throw semanticError(symbol.lineNumber, "The 'sym' keyword expects an expression that evaluates to a symbol or string but evaulated to null");
    }
    if(auto resolvedSymbol = result.tryAsConstructSymbol) {
      *index = nextIndex;
      return resolvedSymbol;
    }
    if(auto resolvedString = result.tryAsConstructString) {
      *index = nextIndex;
      return new ConstructSymbol(resolvedString.lineNumber, resolvedString.toUtf8());
    }
    throw semanticError(symbol.lineNumber, format("The 'sym' keyword expects an expression that evaluates to a symbol or string but evaulated to %s", result.typeName));
  }

  bool matchPattern(const(ConstructSymbol) constructSymbol, const(IPrecedenceConsumer) precedenceConsumer,
                    const(PatternNode)[] patternNodes, const(ConstructObject)[] objects, size_t* index,
                    void delegate(const(ConstructObject) arg) pure appendArg)
  {
  PATTERN_NODE_LOOP:
    foreach(nodeIndex,node; patternNodes) {
      //logDev("  matching pattern node %d '%s' of type '%s'", nodeIndex, node.name, node);
      if(*index >= objects.length) {

        if(!node.countType.isOptional) {
          //logDev("  did not match");
          return false; // not a match
          /*
          throw semanticError(constructSymbol.lineNumber, format("construct '%s' did not get enough objects, expected at least one '%s' next", constructSymbol.value, node.matcher.codeText));
          */
        }
        if(node.matcher.processorOptionalValueType && node.name) {
          appendArg(null);
        }
        continue;
      }

      // Detect the symbol case
      if(node.matcher.isSymbolMatcher) {

        if(!cast(SymbolMatcher)node.matcher) {
          throw imp();
        }
        if(node.countType != CountType.one) {
          throw imp(format("  matching symbol with count type %s", node.countType));
        }
        auto symbol = tryConsumeSymbol(precedenceConsumer, objects, index);
        if(symbol is null) {
          return false; // not a match
        }
        if(node.name) {
          appendArg(symbol);
        }
        continue;
      }

      //logDev("  consuming value...");
      auto value = consumeValueAlreadyCheckedIndex(precedenceConsumer, objects, index);
      if(value is null) {
        throw imp("value is null");
      }
      // todo: handle returns

      if(!node.matcher.match(value)) {
        if(!node.countType.isOptional) {
          //logDev("  did not match");
          return false; // not a match
          //throw semanticError(constructSymbol.lineNumber, format("construct '%s' expected %s but got %s", constructSymbol.value, An(node.matcher.codeText), An(value.typeName)));
        }
        if(node.name) {
          appendArg(null);
        }
        continue PATTERN_NODE_LOOP;
      }
      //logDev("    matcher '%s' matched '%s'", node.matcher.codeText, value);

      if(node.countType == CountType.one) {
        if(node.name) {
          appendArg(value);
        }
        continue PATTERN_NODE_LOOP;
      }
      if(node.countType == CountType.optional) {
        if(node.matcher.processorOptionalValueType && node.name) {
          appendArg(value);
        }
        continue PATTERN_NODE_LOOP;
      }
      imp(format("count type %s", node.countType));
    }
    return true; // match
  }

  const(ConstructObject) processPatternConstruct(const(ConstructSymbol) constructSymbol, const(ConstructDefinition) con,
                                                 const(PatternHandler)[] patternHandlers, const(ConstructObject) opParam, const(ConstructObject)[] objects, size_t* index)
  {
    ConstructObject[256] constructArgs;
    size_t constructArgCount = 0;
    ConstructObject[256] constructObjectBuffer;
    size_t constructObjectOffset = 0;

    void appendArg(const(ConstructObject) arg) pure
    {
      constructArgs[constructArgCount++] = arg.unconst;
    }

    if(con.opParam.matcher !is null) {
      if(!opParam) {
	throw new Exception(format("CodeBug: processPatternConstruct got an operator construct '%s' but the opParam is null", constructSymbol.value));
      }
      appendArg(opParam);
    }
    //logDev("construct '%s' is %s", constructSymbol.value, (con.opParam.matcher is null) ? "NULL" : "NOT NULL");

    ConstructHandler handler = null;
    foreach(patternHandlerIndex, patternHandler; patternHandlers) {
      size_t nextIndex;
      nextIndex = *index;
      constructArgCount = 0;
      if(opParam) {
	appendArg(opParam);
      }
      //logDev("Attempting to match pattern %d for construct '%s'...", patternHandlerIndex, constructSymbol.value);
      if(matchPattern(constructSymbol, con, patternHandler.patternNodes, objects, &nextIndex, &appendArg)) {
        handler = patternHandler.handler;
        *index = nextIndex;
        break;
      }
    }

    if(handler == null) {
      throw semanticError(constructSymbol.lineNumber, format("construct '%s' had no pattern that matched the following contruct objects", constructSymbol.value));
    }

    //
    // Print the arguments
    //
    /*
    logDev("Setup %d argument(s) for construct '%s':", constructArgCount, constructSymbol.value);
    foreach(i, arg; constructArgs[0..constructArgCount]) {
      logDev("  [%s] %s (%s)", i, arg, arg.typeName);
    }
    */
    return handler(&this, con, constructSymbol, constructArgs[0..constructArgCount]);
  }

  const(ConstructObject) consumeValue(const(IPrecedenceConsumer) precedence, const(ConstructSymbol) constructSymbol,
                                      const(ConstructObject)[] objects, size_t* index)
  {
    if(*index >= objects.length) {
      throw endedInsideConstruct(constructSymbol);
    }
    return consumeValueAlreadyCheckedIndex(precedence, objects, index);
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
  const(ConstructObject) consumeValueAlreadyCheckedIndex(const(IPrecedenceConsumer) precedence,
                                                         const(ConstructObject)[] objects, size_t* index)
  {
    assert(*index < objects.length);

    //
    // Get the next object, if it is a construct, then process it, and use that object
    //
    auto object = objects[*index].unconst;
    (*index)++;
    if(auto symbol = object.tryAsConstructSymbol.unconst) {

      logDebug("looking up symbol '%s' on line %s...", symbol.value, symbol.lineNumber);
      auto symbolEntries = tryLookupSymbol(symbol.value);
      if(!symbolEntries.first) {
        throw semanticError(symbol.lineNumber, format("symbol '%s' does not exist", symbol.value));
      }
      if(symbolEntries.moreCount > 0) {
        throw semanticError(symbol.lineNumber, format("symbol '%s' has multiple entries at the same scope", symbol.value));
      }

      if(auto definition = symbolEntries.first.tryAsConstructDefinition) {
        if(definition.opParam.matcher !is null) {
          throw semanticError(symbol.lineNumber, format
                              ("the %s construct is an operator and expects %s before it, but did not get one", symbol.value, An(definition.opParam.matcher.codeText)));
        }

        //logDev("(inside construct '%s') processing '%s'...", precedence.getOperatorString(), symbol.value);
        auto patternHandlers = definition.getPatternHandlers();
        if(patternHandlers is null) {
          object = definition.processNoPattern(&this, symbol, null, objects, index).unconst;
        } else {
          object = processPatternConstruct(symbol, definition, patternHandlers, null, objects, index).unconst;
        }
        //logDev("(inside construct '%s') done processing '%s'...", precedence.getOperatorString(), symbol.value);

      } else {
        object = symbolEntries.first.tryAsConstructObject.unconst;
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
	break; // object is not a symbol
      }

      logDebug("looking up symbol '%s' on line %s to see if it is an operation with higher precedence...", symbol.value, symbol.lineNumber);
      auto symbolEntries = tryLookupSymbol(symbol.value);
      //logDev("symbol '%s' entry is %s", symbol.value, symbolEntries);
      if(!symbolEntries.first) {
	logDebug("  '%s' does not have a value in the symbol table", symbol.value);
	break; // symbol does not exist
      }

      if(symbolEntries.moreCount > 0) {
	throw semanticError(symbol.lineNumber, format("symbol '%s' has multiple entries at the same scope", symbol.value));
      }

      //logDev("Checking if symbol '%s' is an operator construct", symbol.value);
      auto definition = symbolEntries.first.tryAsConstructDefinition;
      if(!definition) {
	logDebug("symbol '%s' is not a construct", symbol.value);
	break; // symbol is not a construct
      }
      if(definition.opParam.matcher is null) {
	logDebug("construct '%s' is not an operator construct", symbol.value);
	break; // symbol is not an operator construct
      }

      auto currentOpString = precedence.getOperatorString();
      if(currentOpString) {
	if(!Precedence.greaterThan(symbol.value, currentOpString)) {
	  logDebug("operator construct '%s' precedence %s is not higher then the current operator '%s' precedence %s",
                   symbol.value, Precedence.getPrecedence(symbol.value),
                   currentOpString, Precedence.getPrecedence(symbol.value));
	  break; // precedence is not high enough
	}
	logDebug("Operator '%s' is higher precedence than '%s'", definition.getOperatorString, currentOpString);
      }

      (*index)++;
      //logDev("--> %s is an operator construct with a higher precedence", symbol.value);
      auto patternHandlers = definition.getPatternHandlers();
      if(patternHandlers is null) {
	object = definition.processNoPattern(&this, symbol, object, objects, index).unconst;
      } else {
	object = processPatternConstruct(symbol, definition, patternHandlers, object, objects, index).unconst;
      }
      //logDev("<-- %s is done being processed", symbol.value);
    }

    return object;
  }


  const(T) consumeTypedValue(T)(const(IPrecedenceConsumer) precedence, const(ConstructSymbol) constructSymbol,
                                const(ConstructObject)[] objects, size_t* argIndex) if( !is( T == ConstructSymbol) )
  {
    auto object = consumeValue(precedence, constructSymbol, objects, argIndex);
    if(object is null) {
      throw semanticError(constructSymbol.lineNumber, format
                          ("the %s construct expects %s but got no value", constructSymbol.value, An(T.staticTypeName)));
    }
    auto value = object.tryAs!T;
    if(!value) {
      throw semanticError(constructSymbol.lineNumber, format
                          ("the %s construct expects %s but got %s",
                           constructSymbol.value, An(T.staticTypeName), An(object.typeName)));
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

alias ProcessorFunc = const(ConstructObject) function(ConstructProcessor* processor,
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
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
							 const(ConstructObject) opParam, const(ConstructObject)[] objects, size_t* index) const
  {
    return func(processor, this, constructSymbol, objects, index);
  }
}
const(ConstructObject) handleConstructWithBlock
(ConstructProcessor* processor, const(ConstructDefinition) definition, const(ConstructSymbol) constructSymbol, const(ConstructObject)[] args)
{
  auto withBlock = cast(ConstructWithBlockDefinition)definition;
  auto patternNodes = withBlock.getPatternHandlers()[0].patternNodes;

  //logDev("pattern is '%s'", pattern);
  if(args.length != patternNodes.length) {
    throw new Exception(format("CodeBug: handleConstructWithBlock expected %s args for it's pattern, but got %s", patternNodes.length, args.length));
  }

  processor.pushScope(withBlock.block.lineNumber, ScopeType.defcon, true);
  scope(exit) { processor.popScope(); }

  foreach(i; 0..args.length) {
    auto arg = args[i];
    auto patternNode = patternNodes[i];

    if(patternNode.name.length > 0 && patternNode.name != "nameless") {
      processor.addSymbol(patternNode.name, arg);
    }
  }

  return processor.processConstructImplementationBlock(withBlock.block.objects, withBlock);
}
class ConstructWithBlockDefinition : PatternConstructDefinition
{
  const ConstructBlock block;
  this(string name, immutable(PatternNode)[] patternNodes, size_t lineNumber, string filename, ConstructAttributes attributes,
       immutable(ConstructType) evalTo, immutable(ConstructBlock) block) immutable pure
  {
    // TODO: how to support OpParam
    super(name, OpParam(), [immutable PatternHandler(patternNodes, &handleConstructWithBlock)], lineNumber, filename, attributes, evalTo);
    this.block = block;
  }
}

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
  /*
class ThrowConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    auto message = processor.consumeTypedValue!ConstructString(constructSymbol, objects, argIndex);
    throw new ConstructThrownException(constructSymbol.lineNumber, processor, cast(string)message.toUtf8());
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
