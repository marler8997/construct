module construct.processor;
/*
The Process Loop
--------------------

while(true) {
  if(index >= objects.length) {
    return null;
  }
  object = objects[index++];

  if(object.type == symbol) {
    object = object.resolveSymbol;
    if(object is construct) {
      processConstruct()
      continue;
    }
  }

}
 */
import std.file   : exists, buildNormalizedPath, read;
import std.path   : setExtension, stripExtension;
import std.array  : Appender, appender, isDynamicArray, ElementEncodingType;
import std.string : startsWith, format, indexOf;
import std.conv   : to;

import core.stdc.stdlib : malloc, free;

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
  bool addSymbolsToParentScope;

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
      ["import"     : SymbolEntryList(singleton!ImportConstructDefinition()),
       "defcon"     : SymbolEntryList(singleton!DefconConstructDefinition()),
       "deftype"    : SymbolEntryList(singleton!DeftypeConstructDefinition()),
       "toSymbolRef": SymbolEntryList(singleton!ToSymbolRefConstructDefinition()),
       "exec"       : SymbolEntryList(singleton!ExecConstructDefinition()),
       "throw"      : SymbolEntryList(singleton!ThrowConstructDefinition()),
       "try"        : SymbolEntryList(singleton!TryConstructDefinition()),
       "let"        : SymbolEntryList(singleton!LetConstructDefinition()),
       "set"        : SymbolEntryList(singleton!SetConstructDefinition()),
       "letset"     : SymbolEntryList(singleton!LetSetConstructDefinition()),
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
       "semantics"  : SymbolEntryList(singleton!SemanticsConstructDefinition()),
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
        importFileEntry.publicSymbols = process(importFile);
	return;
      }
    }

    throw new SemanticException(importLineNumber, &this, format("import '%s' not found", importName));
  }

  void addSymbol(const(char)[] symbol, const(ISymbolObject) object)
  {
    auto entry = currentScope.symbols.get(symbol, SymbolEntryList(null));
    if(!entry.first) {
      currentScope.symbols[symbol] = SymbolEntryList(object.unconst);
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

  
  SymbolEntryList tryLookupSymbol(const(char)[] fullSymbol)
  {
    const(char)[] rest;
    auto symbol = fullSymbol.splitAtDot(&rest);
    
    // Search current scope
    {
      SymbolEntryList match = currentScope.symbols.get(symbol, SymbolEntryList(null));
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
      writefln("%sScope %s (%s symbols)", prefix, scopeNumber, scope_.symbols.length);
      prefix ~= "  ";
      foreach(symbol; scope_.symbols.byKeyValue) {
        writefln("%s%s : %s", prefix, symbol.key, symbol.value);
      }
      prefix ~= "  ";
      scopeNumber++;
    }
    writefln("%sScope %s (%s symbols)", prefix, scopeNumber, currentScope.symbols.length);
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

    pushScope(1, ScopeType.file, false, false);
    try {
      process(sourceFile.parsedObjects, null);
      return currentScope.symbols;
    } finally {
      popScope();
    }
  }
  const(ConstructObject) process(const(ConstructObject)[] objects, const(ConstructDefinition) insideConstruct)
  {
    ConstructDefinition previousConstruct;
    if(insideConstruct) {
      previousConstruct = currentConstruct;
      currentConstruct = insideConstruct.unconst;
    }
    scope(exit) {
      if(insideConstruct) {
        currentConstruct = previousConstruct;
      }
    }
    
    size_t index = 0;
    size_t lineNumber = 0;
    while(index < objects.length) {
      lineNumber = objects[index].lineNumber;
      auto object = consumeValueAlreadyCheckedIndex(objects, &index);
      if(object !is null) {
        if(auto block = object.asConstructBlock) {
          pushScope(block.lineNumber, ScopeType.block, true, false);
          scope(exit) { popScope(); }
          return process(block.objects, null);
        } else {
          //if(object.lineNumber) {
          //lineNumber = result.object.lineNumber;
          //}
          throw semanticError(lineNumber, format("unhandled statement value (type %s)", object.typeName));
        }
      }
    }
    return null;
  }

  const(ConstructSymbol) consumeSymbol(const(ConstructSymbol) context, const(ConstructObject)[] objects, size_t* index)
  {
    if(*index >= objects.length) {
      throw endedInsideConstruct(context);
    }
    auto object = objects[*index];
    (*index)++;
    if(auto symbol = object.asConstructSymbol) {
      if(symbol.value == "resolveSymbolRef") {
        if(*index >= objects.length) {
          throw endedInsideConstruct(context);
        }
        auto symbolRefObject = objects[*index];
        (*index)++;
        auto symbolRefSymbol = symbolRefObject.asConstructSymbol;
        if(!symbolRefSymbol) {
          throw semanticError(context.lineNumber, format
                              ("resolveSymbolRef requires a symbol ref but got %s", An(symbolRefObject.typeName)));
        }

        //logDev("going to lookup '%s'", symbolRefSymbol.value);
        return lookupSymbol!ConstructSymbol(symbolRefSymbol);
      }
      return symbol;
    } else {
      throw semanticError(context.lineNumber, format
                          ("construct %s expected a symbol but got %s", context.value, An(object.typeName)));
    }
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
          logDebug("processing '%s' construct", symbol.value);
          return definition.process(&this, symbol, objects, index);
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

alias ProcessorFunc = const(ConstructObject) function(ConstructProcessor* processor,
                                                      const(ConstructDefinition) definition,
                                                      const(ConstructSymbol) constructSymbol,
                                                      const(ConstructObject)[] objects, size_t* argIndex);

class FunctionConstructDefinition : ConstructDefinition
{
  ProcessorFunc func;
  this(size_t lineNumber, const(char)[] filename, ConstructAttributes attributes,
       ConstructType evalTo, const(ConstructParam)[] requiredParams,
       const(ConstructOptionalParam)[] optionalParams, ProcessorFunc func)
  {
    super(lineNumber, filename, attributes, evalTo, requiredParams, optionalParams);
    this.func = func;
  }
  final override const(ConstructObject) process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    return func(processor, this, constructSymbol, objects, argIndex);
  }
}

class ConstructWithBlockDefinition : ConstructDefinition
{
  const ConstructBlock block;
  this(size_t lineNumber, const(char)[] filename, ConstructAttributes attributes,
       ConstructType evalTo, const(ConstructParam)[] requiredParams,
       const(ConstructOptionalParam)[] optionalParams, const(ConstructBlock) block)
  {
    super(lineNumber, filename, attributes, evalTo, requiredParams, optionalParams);
    this.block = block;
  }
  final override const(ConstructObject) process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    logDebug("entering scope for construct '%s' defined on line %s (%s required args)",
             constructSymbol.value, lineNumber, requiredParams.length);
    processor.pushScope(lineNumber, ScopeType.defcon,
                        cast(bool)(attributes & ConstructAttribute.openScope),
                        cast(bool)(attributes & ConstructAttribute.addSymbolsToParentScope));
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
    return processor.process(block.objects, this);
  }
}

class ImportConstructDefinition : ConstructDefinition
{
  this()
  {
    super(0, null, ConstructAttributes.init, null, null, null);
  }
  final override const(ConstructObject) process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
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

/*
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


class DefconConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
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

class DeftypeConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
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
class ToSymbolRefConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    return processor.consumeSymbol(constructSymbol, objects, argIndex);
  }
}
class ExecConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    auto code = processor.consumeTypedValue!ConstructBlock(constructSymbol, objects, argIndex);
    return processor.process(code.objects, null);
  }
}
class ThrowConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    auto message = processor.consumeTypedValue!ConstructString(constructSymbol, objects, argIndex);
    throw new ConstructThrownException(constructSymbol.lineNumber, processor, cast(string)message.toUtf8());
  }
}
class TryConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
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
      auto resultObject = processor.process(code.objects, null);
      return null;
    } catch(ConstructException e) {
      processor.pushScope(catchBlock.lineNumber, ScopeType.block, true, false);
      scope(exit) { processor.popScope(); }

      if(catchSymbol) {
	processor.addSymbol(catchSymbol.value, new ConstructUtf8(0, e.msg));
      }
      return processor.process(catchBlock.objects, null);
    }
  }
}
class LetConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
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
class SetConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    auto symbol = processor.consumeSymbol(constructSymbol, objects, argIndex).value;
    auto value = processor.consumeValue(constructSymbol, objects, argIndex);
    if(value is null) {
      throw processor.semanticError(constructSymbol.lineNumber, "let construct does not work if the source expression evaluates to null");
    }
    processor.setSymbol(constructSymbol.lineNumber, symbol, value);

    if(*argIndex < objects.length) {
      if(objects[*argIndex].isObjectBreak) {
        (*argIndex)++;
        return null;
      }
    }
    return value;
  }
}
class LetSetConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
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

class IfConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    auto condition = processor.consumeTypedValue!ConstructBool(constructSymbol, objects, argIndex);
    auto trueCase = processor.consumeTypedValue!ConstructBlock(constructSymbol, objects, argIndex);
    ConstructObject resultObject = null;
    if(condition.value) {
      processor.pushScope(trueCase.lineNumber, ScopeType.block, true, false);
      scope(exit) { processor.popScope(); }
      return processor.process(trueCase.objects, null).unconst;
    } else {
      return null;
    }
  }
}


class EqualsConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, new PrimitiveType(0, PrimitiveTypeEnum.bool_), null, null); }
  final override const(ConstructObject) process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
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
  final override const(ConstructObject) process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    auto type = processor.consumeTypedValue!ConstructType(constructSymbol, objects, argIndex);
    return new ConstructTypedListType(constructSymbol.lineNumber, type);
  }
}
class TypeOfConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, new PrimitiveType(0, PrimitiveTypeEnum.type), null, null); }
  final override const(ConstructObject) process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
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
  final override const(ConstructObject) process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
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
  final override const(ConstructObject) process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    auto type = processor.consumeTypedValue!ConstructType(constructSymbol, objects, argIndex);
    //auto result = 
    throw imp("typed construct");
  }
}
class ForEachConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
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
      processor.pushScope(forEachCode.lineNumber, ScopeType.block, true, false);
      ConstructUint indexObject = null;
      scope(exit) { processor.popScope(); }
      if(indexVar) {
        indexObject = new ConstructUint(forEachArgs.lineNumber, 0);
        processor.addSymbol(indexVar, indexObject);
      }
      processor.addSymbol(itemVar, list.items[0]);
      
      processor.process(forEachCode.objects, null);
      foreach(listObject; list.items[1..$]) {
        processor.setSymbol(constructSymbol.lineNumber, itemVar, listObject);
        if(indexObject) {
          indexObject.value++;
        }
        processor.process(forEachCode.objects, null);
      }
    }

    return null;
  }
}
class MallocConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    auto length = processor.consumeTypedValue!ConstructUint(constructSymbol, objects, argIndex);
    return new ConstructPointer(constructSymbol.lineNumber, malloc(length.value));
  }
}
class StringByteLengthConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, new PrimitiveType(0, PrimitiveTypeEnum.uint_), null, null); }
  final override const(ConstructObject) process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
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
  final override const(ConstructObject) process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
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
  final override const(ConstructObject) process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
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
  final override const(ConstructObject) process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    throw imp("memcpy");
  }
}
class NotConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, new PrimitiveType(0, PrimitiveTypeEnum.bool_), null, null); }
  final override const(ConstructObject) process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
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
  final override const(ConstructObject) process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
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
  final override const(ConstructObject) process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    throw imp("return construct");
  }
}
class StructConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    throw imp("struct construct");
  }
}
class SemanticsConstructDefinition : ConstructDefinition
{
  this() { super(0, null, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) process(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol,
                                                const(ConstructObject)[] objects, size_t* argIndex) const
  {
    throw imp("semantics construct");
  }
}
