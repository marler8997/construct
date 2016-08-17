module construct.processor;

import std.file   : exists, read;
import std.path   : setExtension, stripExtension, buildNormalizedPath;
import std.array  : Appender, appender, isDynamicArray, ElementEncodingType;
import std.string : startsWith, format, indexOf;
import std.conv   : to;
import std.format : formattedWrite;

import core.stdc.stdlib : malloc, free;

import construct.patterns;
import construct.ir;
import construct.parser : SourceFile, ConstructParser, standardParser, ConstructParseException;

import backend    : loadConstructBackend, loadBackendType;

bool verbose;
private enum LogLevel {
  debug_,
  info,
  dev,
  warning,
  error,
}
private immutable string[] logLevelPrefix =
  [
   LogLevel.debug_  : "[DEBUG] ",
   LogLevel.info    : "[INFO] ",
   LogLevel.dev     : "[DEV] ",
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
void logDebug(A...)(in char[] fmt, A args)
{
  log(LogLevel.debug_, fmt, args);
}
void logInfo(A...)(in char[] fmt, A args)
{
  log(LogLevel.info, fmt, args);
}
void logDev(A...)(in char[] fmt, A args)
{
  log(LogLevel.dev, fmt, args);
}
void logWarning(A...)(in char[] fmt, A args)
{
  log(LogLevel.warning, fmt, args);
}
void logError(A...)(in char[] fmt, A args)
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
  this(size_t lineNumber, const(ConstructProcessor)* processor, string msg)
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
  this(const(ConstructObject) obj, const(ConstructProcessor)* processor, string msg)
  {
    super(obj.lineNumber, processor, msg);
    this.obj = obj;
  }
}

alias SymbolEntryList = OneOrMore!ISymbolObject;
alias SymbolTable = SymbolEntryList[const(char)[]];

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
    if(auto value = entry.as!T()) {
      return value;
    }
  }
  return null;
}

inout(char)[] splitAtDot(inout(char)[] symbol, inout(char)[]* rest)
{
  foreach(i, c; symbol) {
    if(c == '.') {
      *rest = symbol[i+1..$];
      return symbol[0..i];
    }
  }
  *rest = null;
  return symbol;
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

struct ConstructProcessor
{
  ImportPath[] importPaths;
  ImportFile[const(char)[]] importMap;

  SourceFile currentFile;

  Appender!(Scope[]) scopeStack;
  Scope currentScope;

  ConstructDefinition currentConstruct;

  this(ImportPath[] importPaths)
  {
    this.importPaths = importPaths;
    // TODO: implement 'letset' which adds a symbol if it does not exist, or updates
    //       it if it does exist
    currentScope.symbols = 
      ["import"     : SymbolEntryList(importConstructDefinition.unconst),
       "defcon"     : SymbolEntryList(defconConstructDefinition.unconst), // TODO: support without unconst
       "deftype"    : SymbolEntryList(singleton!DeftypeConstructDefinition()),
       "pattern"    : SymbolEntryList(patternConstructDefinition.unconst),
       "toSymbol"   : SymbolEntryList(toSymbolConstructDefinition.unconst),
       //"callerScope": SymbolEntryList(singleton!CallerScopeConstructDefinition()),
       "addSymbolsToCaller": SymbolEntryList(singleton!AddSymbolsToCallerConstructDefinition()),
       "exec"       : SymbolEntryList(singleton!ExecConstructDefinition()),
       "throw"      : SymbolEntryList(singleton!ThrowConstructDefinition()),
       "try"        : SymbolEntryList(singleton!TryConstructDefinition()),
       "let"        : SymbolEntryList(letConstructDefinition.unconst),
       "set"        : SymbolEntryList(setConstructDefinition.unconst),
       "letset"     : SymbolEntryList(letSetConstructDefinition.unconst),
       "if"         : SymbolEntryList(singleton!IfConstructDefinition()),
       "equals"     : SymbolEntryList(singleton!EqualsConstructDefinition()),
       "listOf"     : SymbolEntryList(singleton!ListOfConstructDefinition()),
       "typeOf"     : SymbolEntryList(singleton!TypeOfConstructDefinition()),
       "itemType"   : SymbolEntryList(singleton!ItemTypeConstructDefinition()),
       "typed"      : SymbolEntryList(singleton!TypedConstructDefinition()),
       "foreach"    : SymbolEntryList(singleton!ForEachConstructDefinition()),
       "malloc"     : SymbolEntryList(singleton!MallocConstructDefinition()),
       //"stringAlloc": SymbolEntryList(singleton!StringAllocConstructDefinition()),
       "stringByteLength": SymbolEntryList(singleton!StringByteLengthConstructDefinition()),
       "strcpy"     : SymbolEntryList(singleton!StrcpyConstructDefinition()),
       "memcpy"     : SymbolEntryList(singleton!MemcpyConstructDefinition()),
       "not"        : SymbolEntryList(singleton!NotConstructDefinition()),
       "add"        : SymbolEntryList(singleton!AddConstructDefinition()),
       "return"     : SymbolEntryList(singleton!ReturnConstructDefinition()),

       //
       // Classes
       //
       "defclass"   : SymbolEntryList(singleton!DefclassConstructDefinition()),

       
       "semantics"  : SymbolEntryList(singleton!SemanticsConstructDefinition()),
       "dumpScopeStack" : SymbolEntryList(singleton!DumpScopeStackConstructDefinition()),
       ];
  }

  void pushScope(size_t startLineNumber, ScopeType type, bool openScope)
  {
    scopeStack.put(currentScope);
    currentScope = Scope(startLineNumber, type, openScope);
  }
  void pushScope(Scope existingScope)
  {
    scopeStack.put(currentScope);
    currentScope = existingScope;
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
        importFileEntry.publicSymbols = process(importFile);
	return;
      }
    }

    throw new SemanticException(importLineNumber, &this, format("import '%s' not found", importName));
  }

  void addSymbol(const(char)[] symbol, const(ISymbolObject) object)
  {
    Scope* addTo;
    if(currentScope.addSymbolsTo) {
      addTo = currentScope.addSymbolsTo;
    } else {
      addTo = &currentScope;
    }

    auto entry = addTo.symbols.get(symbol, SymbolEntryList(null));
    if(!entry.first) {
      addTo.symbols[symbol] = SymbolEntryList(object.unconst);
      logDebug("Added symbol '%s' to current scope", symbol);
    } else {
      throw imp(format("multiple symbol objects with the same name (trying to add symbol '%s')", symbol));
    }
  }

  void setSymbol(size_t setLineNumber, const(char)[] symbol, const(ISymbolObject) object)
  {
    auto scope_ = &currentScope;
    auto scopeIndex = scopeStack.data.length;
    while(true) {
      auto entry = scope_.symbols.get(symbol, SymbolEntryList(null));
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
  void letSetSymbol(const(char)[] symbol, const(ISymbolObject) object)
  {
    auto entry = currentScope.symbols.get(symbol, SymbolEntryList(null));
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
  const(T) lookupSymbol(T)(size_t lineNumber, const(char)[] fullSymbol)
  {
    const(char)[] rest;
    auto symbol = fullSymbol.splitAtDot(&rest);

    // Search current scope
    SymbolEntryList match = currentScope.symbols.get(symbol, SymbolEntryList(null));
    if(match.first) {
      if(rest.length == 0) {
        if(auto object = match.symbolAs!T) {
          return object;
        }
      } else {
        foreach(entry; match.each()) {
          if(auto matchMember = entry.typedMember!T(rest)) {
            return matchMember;
          }
        }
      }
    }

    
    // Search parent scopes
    foreach_reverse(scope_; scopeStack.data) {
      auto anotherMatch = scope_.symbols.get(symbol, SymbolEntryList(null));
      if(anotherMatch.first) {
        if(rest.length == 0) {
          if(auto object = anotherMatch.symbolAs!T) {
            return object;
          }
        } else {
          foreach(entry; match.each()) {
            if(auto matchMember = entry.typedMember!T(rest)) {
              return matchMember;
            }
          }
        }
        if(!match.first) {
          match = anotherMatch;
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

  
  const(SymbolEntryList) tryLookupSymbol(const(char)[] fullSymbol) const
  {
    const(char)[] rest;
    auto symbol = fullSymbol.splitAtDot(&rest);
    
    // Search current scope
    {
      auto match = currentScope.symbols.get(symbol, SymbolEntryList(null));
      if(match.first) {
        if(rest.length == 0) {
          return match;
        } else {
          // TODO: need to return multiple entries if they all have the correct member
          foreach(entry; match.each()) {
            if(auto matchMember = entry.member(rest)) {
              return SymbolEntryList(matchMember.unconst);
            }
          }
        }
      }
    }

    // Search parent scopes
    foreach_reverse(scope_; scopeStack.data) {
      auto match = scope_.symbols.get(symbol, SymbolEntryList(null));
      if(match.first) {
        if(rest.length == 0) {
          return match;
        } else {
          // TODO: need to return multiple entries if they all have the correct member
          foreach(entry; match.each()) {
            if(auto matchMember = entry.member(rest)) {
              return SymbolEntryList(matchMember.unconst);
            }
          }
        }
      }
    }

    // Search other files
    foreach(fileKeyValue; importMap.byKeyValue) {
      auto importFile = fileKeyValue.value;
      auto match = importFile.publicSymbols.get(symbol, SymbolEntryList(null));
      if(match.first) {
        if(rest.length == 0) {
          return match;
        } else {
          // TODO: need to return multiple entries if they all have the correct member
          foreach(entry; match.each()) {
            if(auto matchMember = entry.member(rest)) {
              return SymbolEntryList(matchMember.unconst);
            }
          }
        }
      }
    }

    return SymbolEntryList(null);
  }


  void printScopeStack()
  {
    import std.stdio : writefln, stdout;
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


  auto semanticError(size_t lineNumber, string msg) const
  {
    return new SemanticException(lineNumber, &this, msg);
  }
  auto constructError(const(ConstructSymbol) constructSymbol, string msg) const
  {
    return new SemanticException(constructSymbol.lineNumber, &this, msg);
  }
  auto endedInsideConstruct(const(ConstructSymbol) constructSymbol) const
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
        if(auto return_ = result.asConstructReturn) {
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
      if(auto return_ = result.asConstructReturn) {
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
      if(auto block = rawObject.asConstructBlock) {
        pushScope(block.lineNumber, ScopeType.block, true);
        scope(exit) { popScope(); }
        result = processBlock(block.objects).unconst;
        index++;
      } else {
        lineNumber = rawObject.lineNumber;
        result = consumeValueAlreadyCheckedIndex(objects, &index).unconst;
      }
      
      if(result) {
        if(auto return_ = result.asConstructReturn) {
          return return_;
        }
        throw semanticError(result.lineNumber, format
                            ("unhandled statement value (type %s)", result.typeName));
      }
    }
    return null;
  }

  const(ConstructSymbol) consumeSymbol(const(ConstructSymbol) context, const(ConstructObject)[] objects, size_t* index)
  {
    auto symbol = tryConsumeSymbol(context, objects, index);
    if(!symbol) {
      throw semanticError(context.lineNumber, format
                          ("construct '%s' expected a symbol", context.value));
    }
    return symbol;
  }
  const(ConstructSymbol) tryConsumeSymbol(const(ConstructSymbol) context, const(ConstructObject)[] objects, size_t* index)
  {
    size_t nextIndex = *index;
    if(nextIndex >= objects.length) {
      //throw endedInsideConstruct(context);
      return null;
    }
    auto object = objects[nextIndex++];
    auto symbol = object.asConstructSymbol;
    if(!symbol) {
      //throw semanticError(context.lineNumber, format
      //("construct '%s' expected a symbol but got %s", context.value, An(object.typeName)));
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
    
    auto result = consumeValueAlreadyCheckedIndex(objects, &nextIndex).unconst;
    if(!result) {
      throw semanticError(symbol.lineNumber, "The 'sym' keyword expects an expression that evaluates to a symbol or string but evaulated to null");
    }
    if(auto resolvedSymbol = result.asConstructSymbol) {
      *index = nextIndex;
      return resolvedSymbol;
    }
    if(auto resolvedString = result.asConstructString) {
      *index = nextIndex;
      return new ConstructSymbol(resolvedString.lineNumber, resolvedString.toUtf8());
    }
    throw semanticError(symbol.lineNumber, format("The 'sym' keyword expects an expression that evaluates to a symbol or string but evaulated to %s", result.typeName));
  }

  /*
  inout(PatternNode) nextRequired(inout(PatternNode)[] nodes)
  {
    foreach(node; nodes) {
      if(!node.countType.isOptional) {
	return node;
      }
    }
    return PatternNode.null_;
  }
  */

  bool matchPattern(const(ConstructSymbol) constructSymbol, Pattern pattern,
                    const(ConstructObject)[] objects, size_t* index,
                    void delegate(const(ConstructObject) arg) appendArg)
  {
    
  PATTERN_NODE_LOOP:
    foreach(nodeIndex,node; pattern.nodes) {
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
      if(node.matcher.matchesAnySymbolNext) {

        if(!cast(SymbolMatcher)node.matcher) {
          throw imp();
        }
        if(node.countType != CountType.one) {
          throw imp(format("  matching symbol with count type %s", node.countType));
        }
        auto symbol = tryConsumeSymbol(constructSymbol, objects, index);
        if(symbol is null) {
          return false; // not a match
        }
        if(node.name) {
          appendArg(symbol);
        }
        continue;
      }

      //logDev("  consuming value...");
      auto value = consumeValueAlreadyCheckedIndex(objects, index);
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

  
  const(ConstructObject) processConstruct(ConstructSymbol constructSymbol,
      const(ConstructDefinition) con, const(ConstructObject)[] objects, size_t* index)
  {
    if(con.patternHandlers.length == 0) {
      return con.processNoPattern(&this, constructSymbol, objects, index);
    }

    ConstructObject[256] constructArgs;
    size_t constructArgCount = 0;
    ConstructObject[256] constructObjectBuffer;
    size_t constructObjectOffset = 0;

    void appendArg(const(ConstructObject) arg)
    {
      constructArgs[constructArgCount++] = arg.unconst;
    }

    ConstructHandler handler = null;
    foreach(patternHandlerIndex, patternHandler; con.patternHandlers) {
      size_t nextIndex;
      nextIndex = *index;
      constructArgCount = 0;
      //logDev("Attempting to match pattern %d for construct '%s'...", patternHandlerIndex, constructSymbol.value);
      if(matchPattern(constructSymbol, patternHandler.pattern, objects, &nextIndex, &appendArg)) {
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
    logDev("Setup %d argument(s):", constructArgCount);
    foreach(i, arg; constructArgs[0..constructArgCount]) {
      logDev("  [%s] %s (%s)", i, arg, arg.typeName);
    }
    */
    
    return handler(&this, constructSymbol, constructArgs[0..constructArgCount]);
  }

  

  //
  // Object consumeValue(Object[] objects, ref index)
  // {
  //   while(true) {
  //     auto object = objects[index++];
  //     if(object.isSymbol) {
  //       auto symbolEntry = object.lookup();
  //       if(symbolEntry.isEmpty) error "symbol does not exist";
  //       if(symbolEntry.hasMulitipleObjects)
  //         error "ambiguous symbol, multiple entries in the same scope";
  //       object = symbolEntry.firstObject;
  //       if(object.isConstructDefinition) {
  //         return processConstruct(oject, ref index);
  //       }
  //     }
  //     return object;
  //   }
  // }
  //
  const(ConstructObject) consumeValue(const(ConstructSymbol) constructSymbol, const(ConstructObject)[] objects, size_t* index)
  {
    if(*index >= objects.length) {
      throw endedInsideConstruct(constructSymbol);
    }
    return consumeValueAlreadyCheckedIndex(objects, index);
  }
  // Assumption: *index < objects.length
  const(ConstructObject) consumeValueAlreadyCheckedIndex(const(ConstructObject)[] objects, size_t* index)
  {
    assert(*index < objects.length);

    auto object = objects[*index];
    (*index)++;
    if(auto symbol = object.asConstructSymbol.unconst) {

      while(true) {
        logDebug("looking up symbol '%s'...", symbol.value);
        auto symbolEntries = tryLookupSymbol(symbol.value);
        if(!symbolEntries.first) {
          throw semanticError(symbol.lineNumber, format("symbol '%s' does not exist", symbol.value));
        }
        if(symbolEntries.moreCount > 0) {
          throw semanticError(symbol.lineNumber, format("symbol '%s' has multiple entries at the same scope", symbol.value));
        }

        if(auto definition = symbolEntries.first.asConstructDefinition) {
	  return processConstruct(symbol, definition, objects, index);
        }

        return symbolEntries.first.asOneConstructObject;
      }
    } else {
      return object;
    }
  }

  const(T) consumeTypedValue(T)(const(ConstructSymbol) constructSymbol, const(ConstructObject)[] objects, size_t* argIndex) if( !is( T == ConstructSymbol) )
  {
    auto object = consumeValue(constructSymbol, objects, argIndex);
    if(object is null) {
      throw semanticError(constructSymbol.lineNumber, format
                          ("the %s construct expects %s but got no value", constructSymbol.value, An(T.staticTypeName)));
    }
    auto value = object.as!T;
    if(!value) {
      throw semanticError(constructSymbol.lineNumber, format
                          ("the %s construct expects %s but got %s",
                           constructSymbol.value, An(T.staticTypeName), An(object.typeName)));
    }
    return value;
  }

  version(comment) {
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

class FunctionConstructDefinition : ConstructDefinition
{
  ProcessorFunc func;
  this(size_t lineNumber, const(char)[] filename,
       ConstructAttributes attributes, ConstructType evalTo,
       const(ConstructParam)[] requiredParams,
       const(ConstructOptionalParam)[] optionalParams, ProcessorFunc func)
  {
    super(lineNumber, filename, attributes,
	  evalTo, requiredParams, optionalParams);
    this.func = func;
  }
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
						   const(ConstructObject)[] objects, size_t* argIndex) const
  {
    return func(processor, this, constructSymbol, objects, argIndex);
  }
}

class ConstructWithBlockDefinition : ConstructDefinition
{
  const ConstructBlock block;
  this(const Pattern pattern, size_t lineNumber, const(char)[] filename, ConstructAttributes attributes,
       ConstructType evalTo, const(ConstructParam)[] requiredParams,
       const(ConstructOptionalParam)[] optionalParams, const(ConstructBlock) block)
  {
    super(lineNumber, filename, attributes,
	  evalTo, requiredParams, optionalParams);
    this.block = block;
  }
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
						   const(ConstructObject)[] objects, size_t* argIndex) const
  {
    //throw new Exception("ConstructWithBlockDefinition.processNoPattern deprecated");

    logDebug("entering scope for construct '%s' defined on line %s",
             constructSymbol.value, lineNumber);
    processor.pushScope(lineNumber, ScopeType.defcon,
                        cast(bool)(attributes & ConstructAttribute.openScope));
    scope(exit) {processor.popScope();}

    //
    // Parse parameters
    //
    foreach(i, requiredParam; requiredParams) {
      //if(argIndex >= objects.length) {
      //throw processor.endedInsideConstruct(constructSymbol);
      //}

      //logDev("parsing parameter %s '%s'...", i, requiredParam.name);
      if(requiredParam.type) {
        ConstructObject arg;
        if(requiredParam.type.asPrimitive == PrimitiveTypeEnum.symbol) {
          //logDev("getting arg as symbol...");
          arg = processor.consumeSymbol(constructSymbol, objects, argIndex).unconst;
        } else if(requiredParam.type.asPrimitive == PrimitiveTypeEnum.symbol) {
          //logDev("getting arg as block...");
          arg = processor.consumeTypedValue!ConstructBlock(constructSymbol, objects, argIndex).unconst;
        } else {
          arg = processor.consumeValue(constructSymbol, objects, argIndex).unconst;
          if(arg is null) {
            throw processor.constructError(constructSymbol, format
                                           ("expected %s for argument %s but got void",
                                            An(to!string(requiredParam.type.asPrimitive)), i));
          }
        }
        
	if(!arg.canBe(requiredParam.type)) {
	  throw processor.semanticError(arg.lineNumber, format
					("Expected argument %s to be %s, but is %s", (i+1),
					 An(requiredParam.type.typeName), An(arg.typeName)));
	}
	processor.addSymbol(requiredParam.name, arg);
      } else {
        auto object = processor.consumeValue(constructSymbol, objects, argIndex);
	processor.addSymbol(requiredParam.name, object);
      }
    }
    return processor.processConstructImplementationBlock(block.objects, this);

  }

}





struct FuncAndPattern {
  string funcName;
  Pattern pattern;

  string genFunc() const pure
  {
    //writefln("argNames = %s", argNames);

    StringBuilder!512 builder;

    builder.append("const(ConstructObject) ");
    builder.append(funcName);
    builder.append("(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol");
  
    foreach(node; pattern.nodes) {
      string typeName = null;
      if(node.countType == CountType.optional) {
	typeName = node.matcher.processorOptionalValueType;
      } else {
	typeName = node.matcher.processorValueType;
      }
    
      if(typeName) {
	builder.append(", const(");
	builder.append(typeName);
	if(!node.countType.onlyOne) {
	  builder.append(")[] ");
	} else {
	  builder.append(") ");
	}
        builder.append(node.name);
      }
    }
    builder.append(")");
    return builder.createString();
  }
}
string genConstructPatternThunk(string name, const FuncAndPattern patternFunc) pure
{
  StringBuilder!1024 builder;

  builder.append("const(ConstructObject) ");
  builder.append(name);
  builder.append("(ConstructProcessor* processor");
  builder.append(", const(ConstructSymbol) constructSymbol");
  builder.append(", const(ConstructObject)[] args)\n");
  builder.append("{\n");
  
  builder.append("  assert(args.length == ");
  auto assertCountOffset = builder.offset;
  builder.append("            );\n");

  builder.append("  return ");
  builder.append(patternFunc.funcName);
  builder.append("(processor, constructSymbol");
  
  size_t argIndex = 0;
  foreach(node; patternFunc.pattern.nodes) {
    string typeName = null;
    if(node.countType == CountType.optional) {
      typeName = node.matcher.processorOptionalValueType;
    } else {
      typeName = node.matcher.processorValueType;
    }

    if(typeName) {
      formattedWrite(&builder.append, ",\n    args[%s] ? args[%s].as!%s : null", argIndex, argIndex, typeName);
      argIndex++;
    }
  }

  builder.append(");\n");
  builder.append("}\n");
  auto codeSize = builder.offset;
  builder.offset = assertCountOffset;
  formattedWrite(&builder.append, "%s", argIndex);
  builder.offset = codeSize;
  
  return builder.createString();
}

immutable importFunc = immutable FuncAndPattern("handleImport", importPattern);
mixin(importFunc.genFunc()~q{
{
  processor.findAndImport(constructSymbol.lineNumber, name.toUtf8);
  //foreach(name; names) {
  //processor.findAndImport(constructSymbol.lineNumber, name.toUtf8);
  //}
  return null;
}});
mixin(genConstructPatternThunk("importThunk", importFunc));
immutable importConstructDefinition = new immutable InternalImplementedConstruct
  ([immutable PatternHandler(importPattern, &importThunk)], ConstructAttributes.init, null);
   
/*
class ImportConstructDefinition : ConstructDefinition
{
  this()
  {
    super(0, null, ConstructAttributes.init, null, null, null);
  }
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
						   const(ConstructObject)[] objects, size_t* argIndex) const
  {
    while(true) {
      auto object = processor.consumeValue(constructSymbol, objects, argIndex);
      if(object.isObjectBreak) {
        break;
      } else if(auto string_ = object.asConstructString) {
        processor.findAndImport(constructSymbol.lineNumber, string_.toUtf8());
      } else {
        throw processor.semanticError(constructSymbol.lineNumber, format
                                      ("the import construct expects a string but got %s",
                                       An(object.typeName)));
      }
    }
    return null;
  }
}
*/


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
      auto object = processor.consumeValueAlreadyCheckedIndex(list.items, &itemIndex);
      if(object is null) {
	throw processor.semanticError(list.items[itemIndex].lineNumber,
				      "expected the expression to return a type but returned null");
      }
      if(object.isListBreak) {
	params.put(ConstructParam(paramName.value));
	continue;
      }
      paramType = object.asConstructType.unconst;
      if(paramType is null) {
	throw processor.semanticError(list.items[itemIndex].lineNumber, format
				      ("expected the expression to return a type but returned %s",
				       An(object.typeName)));
      }
    }

    if(itemIndex >= list.items.length) {
      params.put(ConstructParam(paramName.value, paramType));
      break;
    }
    if(list.items[itemIndex].isListBreak) {
      params.put(ConstructParam(paramName.value, paramType));
      itemIndex++;
      continue;
    }

    imp("a third value for a defcon parameter");
  }

  return params.data;
}



immutable defconFunc1 = immutable FuncAndPattern("handleDefcon1", defconPattern1);
mixin(defconFunc1.genFunc()~q{
{
  ConstructAttributes attributes;
  const(ConstructParam)[] requiredParams = parseRequiredParams(processor, pattern);
  /*
  auto nodes = new PatternNode[requiredParams.length];
  foreach(i, param; requiredParams) {
    nodes[i] = PatternNode(null, CountType.one, param.type.matcher);
  }
  */
  auto backendFunc = loadConstructBackend(name.value);
  if(backendFunc == null) {
    throw processor.semanticError(name.lineNumber, format
                                  ("the backend does not implement construct '%s'", name));
  }
  processor.addSymbol(name.value, new FunctionConstructDefinition
                      (/*Pattern(nodes), */constructSymbol.lineNumber, processor.currentFile.name, attributes, null, requiredParams, null, backendFunc));
  return null;
}});

immutable defconFunc2 = immutable FuncAndPattern("handleDefcon2", defconPattern2);
mixin(defconFunc2.genFunc()~q{
{
  ConstructAttributes attributes;
  const(ConstructParam)[] requiredParams = parseRequiredParams(processor, pattern);
  auto nodes = new PatternNode[requiredParams.length];
  foreach(i, param; requiredParams) {
    if(param.type is null) {
      nodes[i] = PatternNode(null, CountType.one, Matcher.anything);
    } else {
      nodes[i] = PatternNode(null, CountType.one, param.type.matcher);
    }
  }
  
  processor.addSymbol(name.value, new ConstructWithBlockDefinition
                      (Pattern(nodes), constructSymbol.lineNumber, processor.currentFile.name, attributes, null, requiredParams, null, implementation));
  return null;
}});

mixin(genConstructPatternThunk("defconThunk1", defconFunc1));
mixin(genConstructPatternThunk("defconThunk2", defconFunc2));
immutable defconConstructDefinition = new immutable InternalImplementedConstruct
  ([immutable PatternHandler(defconPattern1, &defconThunk1),
    immutable PatternHandler(defconPattern2, &defconThunk2)],
   ConstructAttributes.init, null);
/*
class DefconConstructDefinition : ConstructDefinition
{
  this() { super(defconPattern, 0, null, ConstructAttributes.init, null, null, null); this.handler = &defconThunk; }
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
						   const(ConstructObject)[] objects, size_t* argIndex) const
  {
    auto constructName = processor.consumeSymbol(constructSymbol, objects, argIndex).value;

    ConstructBlock implementation = null;
    ConstructAttributes attributes;

    const(ConstructParam)[] requiredParams = null;

    //
    // Parse defcon information
    //
    while(true) {
      auto object = processor.consumeValue(constructSymbol, objects, argIndex);
      if(object.isObjectBreak) {
        break;
      } else if(auto block = object.asConstructBlock) {
	implementation = block.unconst;
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
        imp(format("defcon construct type %s", object.typeName));
      }
    }

    //
    // Create the construct definition
    //
    if(implementation) {
      processor.addSymbol(constructName, new ConstructWithBlockDefinition
                          (lineNumber, processor.currentFile.name, attributes, null, requiredParams, null, implementation));
    } else {
      auto backendFunc = loadConstructBackend(constructName);
      if(backendFunc == null) {
        throw processor.semanticError(lineNumber, format
                                      ("the backend does not implement construct '%s'", constructName));
      }
      processor.addSymbol(constructName, new FunctionConstructDefinition
                          (lineNumber, processor.currentFile.name, attributes, null, requiredParams, null, backendFunc));
    }
    return null;
  }
}
*/


immutable toSymbolFunc1 = immutable FuncAndPattern("handleToSymbol1", oneSymbolPattern);
mixin(toSymbolFunc1.genFunc()~q{
{
  return symbol;
}});
immutable toSymbolFunc2 = immutable FuncAndPattern("handleToSymbol2", oneStringPattern);
mixin(toSymbolFunc2.genFunc()~q{
{
  return new ConstructSymbol(string_.lineNumber, string_.toUtf8());
}});

mixin(genConstructPatternThunk("toSymbolThunk1", toSymbolFunc1));
mixin(genConstructPatternThunk("toSymbolThunk2", toSymbolFunc2));
immutable toSymbolConstructDefinition = new immutable InternalImplementedConstruct
  ([immutable PatternHandler(oneSymbolPattern, &toSymbolThunk1),
    immutable PatternHandler(oneStringPattern, &toSymbolThunk2)],
   ConstructAttributes.init, null);

class DeftypeConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, null, null, null); }
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
}



immutable patternConstructPattern = immutable Pattern
  ([immutable PatternNode("patternList", CountType.one, Matcher.list)]);
immutable patternFunc = immutable FuncAndPattern("handlePattern", patternConstructPattern);
mixin(patternFunc.genFunc()~q{
{
  return new ConstructPattern(patternList.lineNumber, processPattern(processor, patternList));
}});
mixin(genConstructPatternThunk("patternThunk", patternFunc));
immutable patternConstructDefinition = new immutable InternalImplementedConstruct
  ([immutable PatternHandler(patternConstructPattern, &patternThunk)], ConstructAttributes.init, null);
   

class AddSymbolsToCallerConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    auto code = processor.consumeTypedValue!ConstructBlock(constructSymbol, objects, argIndex);

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
  }
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
class ExecConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    auto code = processor.consumeTypedValue!ConstructBlock(constructSymbol, objects, argIndex);
    return processor.processBlock(code.objects);
  }
}
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
class TryConstructDefinition : ConstructDefinition
{
  this() { super(/*tryPattern, */0, null, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    auto code = processor.consumeTypedValue!ConstructBlock(constructSymbol, objects, argIndex);


    ConstructSymbol catchSymbol = null;
    ConstructBlock catchBlock = null;
    ConstructBlock finallyBlock = null;

    while(*argIndex < objects.length)
    {
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
}


immutable letFunc = immutable FuncAndPattern("handleLet", setletPattern);
mixin(letFunc.genFunc()~q{
{
  processor.addSymbol(name.value, value);
  return (break_ is null) ? value : null;
}});
//pragma(msg, genConstructPatternThunk("letThunk2", letFunc));
mixin(genConstructPatternThunk("letThunk", letFunc));
immutable letConstructDefinition = new immutable InternalImplementedConstruct
  ([immutable PatternHandler(setletPattern, &letThunk)], ConstructAttributes.init, null);
/*
class LetConstructDefinition : ConstructDefinition
{
  this() { super(letPattern, 0, null, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    auto symbol = processor.consumeSymbol(constructSymbol, objects, argIndex).value;
    auto value = processor.consumeValue(constructSymbol, objects, argIndex);
    if(value is null) {
      throw processor.semanticError(constructSymbol.lineNumber, "let construct does not work if the source expression evaluates to null");
    }
    processor.addSymbol(symbol, value);

    if(*argIndex < objects.length) {
      if(objects[*argIndex].isObjectBreak) {
        (*argIndex)++;
        return null;
      }
    }
    return value;
  }
}
*/

immutable setFunc = immutable FuncAndPattern("handleSet", setletPattern);
mixin(setFunc.genFunc()~q{
{
  processor.setSymbol(name.lineNumber, name.value, value);
  return (break_ is null) ? value : null;
}});
//pragma(msg, genConstructPatternThunk("setThunk2", setFunc));
mixin(genConstructPatternThunk("setThunk", setFunc));
immutable setConstructDefinition = new immutable InternalImplementedConstruct
  ([immutable PatternHandler(setletPattern, &setThunk)], ConstructAttributes.init, null);


immutable letSetFunc = immutable FuncAndPattern("handleLetSet", setletPattern);
mixin(letSetFunc.genFunc()~q{
{
  processor.letSetSymbol(name.value, value);
  return (break_ is null) ? value : null;
}});
mixin(genConstructPatternThunk("letSetThunk", letSetFunc));
immutable letSetConstructDefinition = new immutable InternalImplementedConstruct
  ([immutable PatternHandler(setletPattern, &letSetThunk)], ConstructAttributes.init, null);
/*
class LetSetConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    auto symbol = processor.consumeSymbol(constructSymbol, objects, argIndex).value;
    auto value = processor.consumeValue(constructSymbol, objects, argIndex);
    if(value is null) {
      throw processor.semanticError(constructSymbol.lineNumber, "let construct does not work if the source expression evaluates to null");
    }
    processor.letSetSymbol(symbol, value);

    if(*argIndex < objects.length) {
      if(objects[*argIndex].isObjectBreak) {
        (*argIndex)++;
        return null;
      }
    }
    return value;
  }
}
*/

class IfConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
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
}


class EqualsConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, new PrimitiveType(0, PrimitiveTypeEnum.bool_), null, null); }
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    auto left  = processor.consumeValue(constructSymbol, objects, argIndex);
    if(left is null) {
      throw processor.constructError(constructSymbol, "the equals construct did not get a value for the left expression");
    }
    auto right = processor.consumeValue(constructSymbol, objects, argIndex);
    if(right is null) {
      throw processor.constructError(constructSymbol, "the equals construct did not get a value for the right expression");
    }
    auto result = left.equals(right, false);
    return new ConstructBool(constructSymbol.lineNumber,  result);
  }
}
class ListOfConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, new PrimitiveType(0, PrimitiveTypeEnum.list), null, null); }
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    auto type = processor.consumeTypedValue!ConstructType(constructSymbol, objects, argIndex);
    return new ConstructTypedListType(constructSymbol.lineNumber, type);
  }
}
class TypeOfConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, new PrimitiveType(0, PrimitiveTypeEnum.type), null, null); }
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    auto object = processor.consumeValue(constructSymbol, objects, argIndex);
    if(object is null) {
      throw processor.semanticError(constructSymbol.lineNumber, "the typeOf construct expects a value but got void");
    }
    return new PrimitiveType(0, object.primitiveType);
  }
}
class ItemTypeConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, new PrimitiveType(0, PrimitiveTypeEnum.type), null, null); }
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    auto object = processor.consumeTypedValue!ConstructList(constructSymbol, objects, argIndex);
    //logDev("list type is %s", object.itemType);
    return new PrimitiveType(0, object.itemType);
  }
}
class TypedConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    auto type = processor.consumeTypedValue!ConstructType(constructSymbol, objects, argIndex);
    auto value = processor.consumeValue(constructSymbol, objects, argIndex);
    return value.typedAs(type);
  }
}
class ForEachConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
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
}
class MallocConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    auto length = processor.consumeTypedValue!ConstructUint(constructSymbol, objects, argIndex);
    return new ConstructPointer(constructSymbol.lineNumber, malloc(length.value));
  }
}
class StringByteLengthConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, new PrimitiveType(0, PrimitiveTypeEnum.uint_), null, null); }
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    auto stringValue = processor.consumeTypedValue!ConstructString(constructSymbol, objects, argIndex).unconst;
    auto type = processor.consumeTypedValue!ConstructType(constructSymbol, objects, argIndex).unconst;
    return new ConstructUint(0, stringValue.stringByteLength(type.asPrimitive));
  }
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
class StrcpyConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    //auto dest   = processor.consumeTypedValue!ConstructString(constructSymbol, objects, &argIndex).unconst;
    //auto source = processor.consumeTypedValue!ConstructString(constructSymbol, objects, &argIndex).unconst;
    //auto type = processor.consumeTypedValue!ConstructType(constructSymbol, objects, &argIndex).unconst;
    
    throw imp("strcpy");
  }
}
class MemcpyConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    throw imp("memcpy");
  }
}
class NotConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, new PrimitiveType(0, PrimitiveTypeEnum.bool_), null, null); }
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    auto object = processor.consumeValue(constructSymbol, objects, argIndex);
    if(object is null) {
      throw processor.semanticError(constructSymbol.lineNumber, "the not construct expects an expression that returns a bool but it returned nothing");
    }
    auto bool_ = object.asConstructBool;
    if(!bool_) {
      throw processor.semanticError(constructSymbol.lineNumber, format("the add construct expects an expression that returns a bool but it returned %s", object.typeName));    }
    return new ConstructBool(0, !bool_.value);
  }
}
class AddConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    auto leftNumber  = processor.consumeTypedValue!ConstructNumber(constructSymbol, objects, argIndex);
    auto rightNumber = processor.consumeTypedValue!ConstructNumber(constructSymbol, objects, argIndex);
    return leftNumber.add(rightNumber);
  }
}
class ReturnConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, new PrimitiveType(0, PrimitiveTypeEnum.anything), null, null); }
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    auto next = processor.consumeValue(constructSymbol, objects, argIndex);
    ConstructObject returnValue = null;
    if(!next.isObjectBreak) {
      returnValue = next.unconst;
      processor.consumeTypedValue!ObjectBreak(constructSymbol, objects, argIndex);
    }
    return new ConstructReturn(constructSymbol.lineNumber, returnValue);
  }
}
class StructConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    throw imp("struct construct");
  }
}



class DefclassConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    throw imp("defclass");
  }
}


class SemanticsConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    throw imp("semantics construct");
  }
}
class DumpScopeStackConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) processNoPattern(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    processor.printScopeStack();
    return null;
  }
}
