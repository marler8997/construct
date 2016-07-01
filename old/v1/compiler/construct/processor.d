module construct.processor;

import std.file   : exists, buildNormalizedPath, read;
import std.path   : setExtension, stripExtension;
import std.array  : Appender, appender, isDynamicArray, ElementEncodingType;
import std.string : startsWith, format;
import std.typecons : BitFlags;

import construct.ir;
import construct.backendIR;
import construct.parser : SourceFile, ConstructParser, cfamilyParser, ConstructParseException;

import backend    : loadConstructBackend;

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
  this(const(Construct) throwConstruct, ProcessorState* state, string msg)
  {
    super(msg, cast(string)state.currentFile.name, throwConstruct.lineNumber);
  }
}

class SemanticException: ConstructException
{
  Appender!(SemanticException[]) errorList;
  this(size_t lineNumber, ProcessorState* state, string msg)
  {
    super(msg, cast(string)state.currentFile.name, lineNumber);
  }
}
class InvalidConstructException: SemanticException
{
  const(ConstructObject) obj;
  this(const(ConstructObject) obj, ProcessorState* state, string msg)
  {
    super(obj.lineNumber, state, msg);
    this.obj = obj;
  }
}

void enforceArgCount(const(Construct) construct, ProcessorState* state, size_t count)
{
  if(construct.args.length != count) {
    throw new InvalidConstructException
      (construct, state, format("construct '%s' requires %s argument(s) but have %s",
                                construct.name, count, construct.args.length));
  }
}
void enforceNoUnrecognizedArgs(const(Construct) construct, ProcessorState* state, size_t recognizedArgCount)
{
  if(construct.namedArgs.length != recognizedArgCount) {
    throw new InvalidConstructException
      (construct, state, format("construct '%s' has unrecognized named arguments", construct.name));
  }
}
void enforceNoNamedArgs(const(Construct) construct, ProcessorState* state)
{
  if(construct.namedArgs.length != 0) {
    throw new InvalidConstructException
      (construct, state, format("construct '%s' does not supported named arguments but got %s",
                                construct.name, construct.namedArgs.length));
  }
}
T getArg(T,K)(const(K) construct, ProcessorState* state, size_t index) if( is( T : ConstructObject ) && is( K == Construct ) || is( K == BackendConstruct) )
{
  if(index >= construct.args.length) {
    throw new InvalidConstructException
      (construct, state, format("construct '%s' requires more arguments, expected '%s' at index %s but only have %s parameters",
                                construct.name, T.staticTypeName, index, construct.args.length));
  }
  auto casted = cast(T)construct.args[index];
  if(!casted) {
    throw new InvalidConstructException
      (construct, state, format("construct '%s' expects a(n) '%s' parameter at index %s, but got '%s'",
                                construct.name, T.staticTypeName, index, construct.args[index].typeName));
  }
  return casted;
}
T getArg(T)(const(Construct) construct, ProcessorState* state, const(char)[] name) if( is( T : ConstructObject ) )
{
  auto value = construct.namedParams.get(name, null);
  if(!value) {
    return null;
  }
  auto casted = cast(T)value;
  if(!casted) {
    throw new InvalidConstructException
      (construct, state, format("construct '%s' expects named parameter '%s' to be a(n) '%s', but it is a(n) '%s'",
                                construct.name, name, T.staticTypeName, value.typeName));
  }
  return casted;
}
T getItem(T)(const(ConstructList) list, ProcessorState* state, const(Construct) parentConstruct, size_t listIndex, size_t index) if( is( T : ConstructObject ) )
{
  if(index >= list.items.length) {
    throw new InvalidConstructException
      (list, state, format("in construct '%s', the list at index %s requires more arguments, expected '%s' at index %s but only have %s parameters",
                           parentConstruct.name, listIndex, T.staticTypeName, index, list.items.length));
  }
  auto casted = cast(T)list.items[index];
  if(!casted) {
    throw new InvalidConstructException
      (list, state, format("in construct '%s', the list at index %s expects a(n) '%s' parameter at index %s, but got '%s'",
                           parentConstruct.name, listIndex, T.staticTypeName, index, list.items[index].typeName));
  }
  return casted;
}




struct ImportPath
{
  const(char)[] path;
  const(char)[] prefix;
}
class ImportFile
{
  const char[] importName;
  SourceFile sourceFile = void;
  this(const(char)[] importName, SourceFile sourceFile)
  {
    this.importName = importName;
    this.sourceFile = sourceFile;
  }
}
struct Scope
{
  size_t startLineNumber;
  // If true, then symbols inside the scope can see symbols outside the scope
  bool open;
  // If true, the symbols defined in this scope will be added to the parent scope
  bool addSymbolsToParentScope;
  ConstructObject[const(char)[]] symbols;
  ConstructDefinition[const(char)[]] constructMap;
  
  Appender!(const(Construct)[])[const(char)[]] waitingForConstructDefinition;
}

struct ProcessorState
{
  ImportPath[] paths;
  ImportFile[const(char)[]] importMap;

  SourceFile currentFile;
  ConstructDefinition context;

  Appender!(Scope[]) scopeStack;
  Scope currentScope;
  void pushScope(size_t startLineNumber, bool openScope, bool addSymbolsToParent)
  {
    scopeStack.put(currentScope);
    currentScope = Scope(startLineNumber, openScope, addSymbolsToParent);
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
  
  this(ImportPath[] paths)
  {
    this.paths = paths;
    currentScope.constructMap =
      ["import"    : singleton!ImportConstructDefinition(),
       "defcon"    : singleton!DefconConstructDefinition(),
       "exec"      : singleton!ExecConstructDefinition(),
       "throw"     : singleton!ThrowConstructDefinition(),
       "catch"     : singleton!CatchConstructDefinition(),
       "let"       : singleton!LetConstructDefinition(),
       "set"       : singleton!SetConstructDefinition(),
       "if"        : singleton!IfConstructDefinition(),
       "equals"    : singleton!EqualsConstructDefinition(),
       "return"    : singleton!ReturnConstructDefinition(),
       "construct" : singleton!ConstructConstructDefinition(),
       "semantics" : singleton!SemanticsConstructDefinition(),
       ];
  }

  final void findAndImport(const(Construct) construct)
  {
    if(construct.namedArgs.length) {
      throw new Exception("The 'import' construct does not have any named parameters");
    }
    foreach(arg; construct.args) {
      auto importSymbol = cast(ConstructSymbol)arg;
      if(!importSymbol) {
	throw new InvalidConstructException(construct, &this, format
					    ("The 'import' construct does not accept '%s' parameters", arg.typeName));
      }
      
      findAndImport(importSymbol.lineNumber, importSymbol.value);
    }
  }
  final void findAndImport(size_t importLineNumber, const(char)[] importName)
  {
    auto imported = importMap.get(importName, null);
    if(!imported) {
      logDebug("%s was already imported", importName);
      return;
    }

    SourceFile importFile;
    
    // Find the import file
    logInfo("searching for '%s'...", importName);
    foreach(path; paths) {
      auto nameSubPath = importName;
      if(path.prefix.length) {
        if(importName.length < path.prefix.length + 1 ||
           !importName.startsWith(path.prefix) ||
           importName[path.prefix.length] != '.') {
          continue;
        }
        nameSubPath = importName[path.prefix.length+1..$];
      }

      importFile.name = buildNormalizedPath(path.path, nameSubPath.setExtension(".con"));
      ConstructParser parser;
      if(exists(importFile.name)) {
        parser = cfamilyParser;
        logInfo("    FOUND AT: %s", importFile.name);
      } else {
        logInfo("NOT FOUND AT: %s", importFile.name);

        importFile.name = importFile.name.stripExtension;
        if(exists(importFile.name)) {
          parser = cfamilyParser;
          logInfo("    FOUND AT: %s", importFile.name);
        } else {
          logInfo("NOT FOUND AT: %s", importFile.name);
        }
      }
	  
      if(parser.name) {
        auto importCode = cast(const(char)[])read(importFile.name);
	importFile.constructs = parser.func(importCode);

        // Have to do this before processing the import source
        // so that it doesn't try to load the same import twice
        importMap[importName] = new ImportFile(importName, importFile);

        processConstructs(&this, importFile);
	return;
      }
    }

    throw new SemanticException(importLineNumber, &this, format("import '%s' not found", importName));
  }

  void addConstruct(const(char)[] constructName, const(ConstructDefinition) definition, bool forceAddToThisScope = false) {
    void add(size_t scopeIndex, Scope* scope_)
    {
      scope_.constructMap[constructName] = cast(ConstructDefinition) definition;

      // Process any construts waiting for this one
      while(true) {
	auto waitingForList = scope_.waitingForConstructDefinition.get(constructName, appender!(const(Construct)[])());
	if(waitingForList.data.length > 0) {
	  if(scope_ != &currentScope) {
	    throw new Exception("not implemented"); // need to process constructs in another scope
	  }
	  foreach(waitingFor; waitingForList.data) {
	    logDebug("processing forward reference construct for '%s'...", constructName);
	    definition.process(&this, waitingFor);
	  }
	  scope_.waitingForConstructDefinition.remove(constructName);
	}
	if(scope_ == &currentScope) {
	  break;
	}
	scopeIndex++;
	if(scopeIndex >= scopeStack.data.length) {
	  scope_ = &currentScope;
	} else {
	  scope_ = &scopeStack.data[scopeIndex];
	}
      }
    }

    size_t scopeIndex = scopeStack.data.length;
    if(forceAddToThisScope || !currentScope.addSymbolsToParentScope) {
      auto existing = currentScope.constructMap.get(constructName, null);
      if(existing) {
	throw new SemanticException(definition.lineNumber, &this, format("construt '%s' has been defined twice in the same scope", constructName));
      }
      add(scopeIndex, &currentScope);
      return;
    }
    
    while(true) {
      if(scopeIndex == 0) {
	// The symbol will disappear (added to the ether)
	logDebug("construct '%s' is added to the ether (will disappear)", constructName);
	return;
      }
      scopeIndex--;
      auto scope_ = &scopeStack.data[scopeIndex];
      if(!scope_.addSymbolsToParentScope) {
	auto existing = scope_.constructMap.get(constructName, null);
	if(existing) {
	  throw new SemanticException(definition.lineNumber, &this, format("construct '%s' has been defined twice in the same scope", constructName));
	}
	add(scopeIndex, scope_);
	return;
      }
    }
  }


  void setSymbol(const(char)[] symbol, const(ConstructObject) obj) {
    auto scope_ = &currentScope;
    auto scopeIndex = scopeStack.data.length;
    while(true) {
      auto oldValue = scope_.symbols.get(symbol, null);
      if(oldValue) {
        scope_.symbols[symbol] = cast(ConstructObject)obj;
        return;
      }
      if(scopeIndex == 0) {
        throw new SemanticException(obj.lineNumber, &this, format("Cannot set symbol '%s' because it doesn't exist", symbol));
      }
      scopeIndex--;
      scope_ = &scopeStack.data[scopeIndex];
    }
    
  }
  void addSymbol(const(char)[] symbol, const(ConstructObject) obj, bool forceAddToThisScope = false) {
    /*
    writefln("Before adding symbol '%s' on line %s", symbol, obj.lineNumber);
    printScopeStack();
    
    scope(exit) {
      writefln("After adding symbol '%s' on line %s", symbol, obj.lineNumber);
      printScopeStack();
    }
    */
    
    if(forceAddToThisScope || !currentScope.addSymbolsToParentScope) {
      auto existing = currentScope.symbols.get(symbol, null);
      if(existing) {
	  throw new SemanticException(obj.lineNumber, &this, format("symbol '%s' has been defined twice in the same scope", symbol));
      }
      //logDebug("Added symbol '%s' to currentScope", symbol);
      currentScope.symbols[symbol] = cast(ConstructObject)obj;
      return;
    }
    
    size_t scopeIndex = scopeStack.data.length;
    while(true) {
      if(scopeIndex == 0) {
	// The symbol will disappear (added to the ether)
	logDebug("symbol '%s' is added to the ether (will disappear)", symbol);
	return;
      }
      scopeIndex--;
      auto scope_ = &scopeStack.data[scopeIndex];
      if(!scope_.addSymbolsToParentScope) {
	auto existing = scope_.symbols.get(symbol, null);
	if(existing) {
	  throw new SemanticException(obj.lineNumber, &this, format("symbol '%s' has been defined twice in the same scope", symbol));
	}
	//logDebug("Added symbol '%s' to a parent scope", symbol);
	scope_.symbols[symbol] = cast(ConstructObject)obj;
	return;
      }
    }
  }

  ConstructDefinition tryFindConstruct(const(char)[] constructName)
  {
    {
      auto obj = currentScope.constructMap.get(constructName, null);
      if(obj) {
        return obj;
      }
    }
    foreach_reverse(scope_; scopeStack.data) {
      auto obj = scope_.constructMap.get(constructName, null);
      if(obj) {
        return obj;
      }
    }
    return null;
  }
  
  ConstructObject tryFindSymbol(const(char)[] symbol)
  {
    {
      auto obj = currentScope.symbols.get(symbol, null);
      if(obj) {
        return obj;
      }
      if(!currentScope.open) {
        return null;
      }
    }

    foreach_reverse(scope_; scopeStack.data) {
      auto obj = scope_.symbols.get(symbol, null);
      if(obj) {
        return obj;
      }
      if(!scope_.open) {
        return null;
      }
    }
    return null;
  }

  void printScopeStack()
  {
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
  }
}

unittest
{
  // Test scope stack works
  ProcessorState state = ProcessorState();

  auto valueObj = new ConstructString(1, "mysymbol string value");
  state.addSymbol("mysymbol", valueObj);

  assert(valueObj == state.tryFindSymbol("mysymbol"));

  state.pushScope(2, false, false);
  assert(!state.tryFindSymbol("mysymbol"));
  
  state.popScope();

  assert(valueObj == state.tryFindSymbol("mysymbol"));

  //
  // Test openScope
  //
  state.pushScope(2, true, false);
  assert(valueObj == state.tryFindSymbol("mysymbol"));
  state.popScope();
  
  //
  // Test addSymbolsToParentScope
  //
  state.pushScope(3, false, true);
  assert(!state.tryFindSymbol("mysymbol"));

  auto goToParentObj = new ConstructString(1, "the value of goToParent");
  state.addSymbol("goToParent", goToParentObj);
  assert(!state.tryFindSymbol("goToParent"));
  state.popScope();
  assert(valueObj == state.tryFindSymbol("mysymbol"));
  assert(goToParentObj == state.tryFindSymbol("goToParent"));
}

/*
// Accepts ConstructSymbol or ConstructSymbolOrString
const(char)[] resolveToSymbol(ProcessorState* state, ConstructSymbolOrString obj)
{
  {
    ConstructString string_ = cast(ConstructString)obj;
    if(string_) {
      throw new InvalidConstructException(obj, state, "expected a symbol but got a string");
    }
  }
  return obj.value;
}
*/

/*
// Only accepts construct strings
const(char)[] resolveToString(ProcessorState* state, ConstructSymbolOrString obj)
{
  //logDebug("resolveToString ( %s \"%s\")", obj.typeName, obj.value);
  {
    auto string_ = cast(ConstructString)obj;
    if(string_) {
      return string_.value;
    }
  }

  auto symbolObject = state.tryFindSymbol(obj.value);
  if(!symbolObject) {
    throw new SemanticException(obj.lineNumber, state, format("Symbol '%s' has not been bound", obj.value));
  }

  auto symbolOrString = cast(ConstructSymbolOrString)symbolObject;
  if(!symbolOrString) {
    throw new SemanticException(obj.lineNumber, state, format("Expected a string but symbol '%s' is a(n) %s", obj.value, symbolObject.typeName));
  }
  
  return resolveToString(state, symbolOrString);
}
*/

T resolveSymbolTo(T)(ProcessorState* state, const(ConstructSymbol) obj)
{
  auto symbolObject = state.tryFindSymbol(obj.value);
  if(!symbolObject) {
    throw new SemanticException(obj.lineNumber, state, format("Symbol '%s' has not been bound", obj.value));
  }

  auto value = cast(T)symbolObject;
  if(!value) {
    throw new SemanticException(obj.lineNumber, state, format("expected a(n) '%s' but symbol '%s' is a(n) %s", T.staticTypeName, obj.value, symbolObject.typeName));
  }
  return value;
}

T evalTo(T)(ProcessorState* state, ConstructDefinition definition, const(Construct) construct) if( is ( T : ConstructObject ) )
{
  //logDebug("attempting to convert construct '%s' to %s", construct.name, T.staticTypeName);
  if(definition.evalTo) {
    if(definition.evalTo.isVoid) {
      throw new SemanticException(construct.lineNumber, state, format
				  ("construct '%s' does not evaluate to any type, let alone a(n) '%s'",
				   construct.name, T.staticTypeName));
    }
    if(!T.primitiveTypeEnum.canBe(definition.evalTo)) {
      throw new SemanticException(construct.lineNumber, state, format
				  ("construct '%s' does not evaluate to a(n) '%s', it evaluates to a(n) '%s'",
				   construct.name, T.staticTypeName, definition.evalTo.typeName));
    }
  }

  auto returnValue = definition.process(state, construct);
  if(returnValue is null) {
    throw new SemanticException(construct.lineNumber, state, format
				("expected construct '%s' to return a(n) '%s' but did not return any value",
				 construct.name, T.staticTypeName));
  }
  //logDebug("need to resolve '%s' to '%s'", returnValue.typeName, T.staticTypeName);
  return resolveTo!T(state, returnValue);
}

T resolveTo(T)(ProcessorState* state, const(ConstructObject) obj)
{
  {
    T value = cast(T)obj;
    if(value) {
      return value;
    }
    logDebug("%s is not a(n) %s", obj.typeName, T.staticTypeName);
  }

  ConstructSymbol symbol = cast(ConstructSymbol)obj;
  if(!symbol) {

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
  }
  
  auto symbolObject = state.tryFindSymbol(symbol.value);
  if(!symbolObject) {
    //state.printScopeStack();
    throw new SemanticException(obj.lineNumber, state, format("symbol '%s' has not been bound", symbol.value));
  }

  //logDebug("found symbol '%s'!", symbol.value);
  auto value = cast(T)symbolObject;
  if(!value) {
    throw new SemanticException(obj.lineNumber, state, format("expected a '%s' but symbol '%s' is a(n) %s", T.staticTypeName, symbol.value, symbolObject.typeName));
  }
  return value;
}

T resolveArg(T,K)(const(K) construct, ProcessorState* state, size_t index) if( is( T : ConstructObject ) && is( K == Construct ) || is( K == BackendConstruct) )
{
  if(index >= construct.args.length) {
    throw new InvalidConstructException
      (construct, state, format("construct '%s' requires more arguments, expected '%s' at index %s but only have %s arguments",
                                construct.name, T.staticTypeName, index, construct.args.length));
  }
  return resolveTo!T(state, construct.args[index]);
}
void enforceHasArg(const(Construct) construct, ProcessorState* state, size_t index)
{
  if(index >= construct.args.length) {
    throw new InvalidConstructException
      (construct, state, format("not enough arguments for construct '%s' (only have %s argument(s))",
                                construct.name, construct.args.length));
  }
}



struct ConstructParam
{
  const(char)[] name;
  const(ConstructType) type;
}
struct ConstructOptionalParam
{
  const(char)[] name;
  const(ConstructType) type;
  const(ConstructObject) defaultValue;
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

class ConstructDefinition
{
  const size_t lineNumber;
  ConstructAttributes attributes;
  ConstructType evalTo;
  const(ConstructParam)[] requiredParams;
  const(ConstructOptionalParam)[] optionalParams;
  
  this(size_t lineNumber, ConstructAttributes attributes, ConstructType evalTo,
       const(ConstructParam)[] requiredParams,
       const(ConstructOptionalParam)[] optionalParams)
  {
    this.lineNumber = lineNumber;
    this.attributes = attributes;
    this.evalTo = evalTo;
    this.requiredParams = requiredParams;
    this.optionalParams = optionalParams;
  }

  abstract const(ConstructObject) process(ProcessorState* state, const(Construct) construct) const;
  /*
  void processIgnoreReturn(ProcessorState* state, const(Construct) construct) const
  {
    throw new Exception("processIgnoreReturn not implemented");
  }
  void processWithReturn(ProcessorState* state, const(Construct) construct) const
  {
    throw new Exception("processWithReturn not implemented");
  }
  */
}

alias ProcessorFunc = ConstructObject function(ProcessorState* state, const(ConstructDefinition) definition, const(Construct) construct);
class FunctionConstructDefinition : ConstructDefinition
{
  ProcessorFunc func;
  this(size_t lineNumber, ConstructAttributes attributes,
       ConstructType evalTo, const(ConstructParam)[] requiredParams,
       const(ConstructOptionalParam)[] optionalParams, ProcessorFunc func)
  {
    super(lineNumber, attributes, evalTo, requiredParams, optionalParams);
    this.func = func;
  }
  final override const(ConstructObject) process(ProcessorState* state, const(Construct) construct) const
  {
    return func(state, this, construct);
  }
}

class ConstructWithBlockDefinition : ConstructDefinition
{
  const ConstructBlock block;
  this(size_t lineNumber, ConstructAttributes attributes,
       ConstructType evalTo, const(ConstructParam)[] requiredParams,
       const(ConstructOptionalParam)[] optionalParams, const(ConstructBlock) block)
  {
    super(lineNumber, attributes, evalTo, requiredParams, optionalParams);
    this.block = block;
  }
  final override const(ConstructObject) process(ProcessorState* state, const(Construct) construct) const
  {
    auto saveContext = state.context;
    state.context = cast(ConstructDefinition)this;
    scope(exit) { state.context = saveContext; }

    logDebug("entering scope for construct '%s' defined on line %s (%s required args)",
             construct.name, lineNumber, requiredParams.length);
    state.pushScope(construct.lineNumber,
		    cast(bool)(attributes & ConstructAttribute.openScope),
		    cast(bool)(attributes & ConstructAttribute.addSymbolsToParentScope));
    scope(exit) {state.popScope();}

    construct.enforceArgCount(state, requiredParams.length);
    foreach(i, requiredParam; requiredParams) {
      state.addSymbol(requiredParam.name, construct.args[i], true);
    }

    if(construct.namedArgs.length) {
      throw new Exception("not implemented");
    }

    return processConstructs(state, block.constructs);
  }
}

void processConstructs(ProcessorState* state, SourceFile sourceFile)
{
  auto previousFile = state.currentFile;
  state.currentFile = sourceFile;
  scope(exit) { state.currentFile = previousFile; }

  processConstructs(state, sourceFile.constructs);
}
const(ConstructObject) processConstructs(ProcessorState* state, const(Construct)[] constructs)
{
  ConstructObject returnObject = null;
  foreach(i, construct; constructs) {
    auto definition = state.tryFindConstruct(construct.name);
    if(!definition) {
      auto referencedBy = state.currentScope.waitingForConstructDefinition.get(construct.name, appender!(const(Construct)[])());
      if(referencedBy.data.length > 0) {
	referencedBy.put(construct);
      } else {
	referencedBy.put(construct);
	state.currentScope.waitingForConstructDefinition[construct.name] = referencedBy;
      }
      continue;
    }
    if(definition == singleton!ReturnConstructDefinition()) {
      returnObject = cast(ConstructObject)definition.process(state, construct);
      if(definition.evalTo) {
	if(!returnObject.canBe(definition.evalTo)) {
	  throw new SemanticException(construct.lineNumber, state, format
				      ("construct '%s' should only return '%s', but it returned '%s'",
				       construct.name, definition.evalTo.typeName, returnObject.typeName));
	}
      }
    } else {
      definition.process(state, construct);
    }
  }

  if(state.currentScope.waitingForConstructDefinition.length > 0) {
    auto exception = new SemanticException(__LINE__, state, "multiple errors");
    foreach(waitingForList; state.currentScope.waitingForConstructDefinition.byKeyValue) {
      auto message = format("undefined construct '%s'", waitingForList.key);
      foreach(waitingFor; waitingForList.value.data) {
	exception.errorList.put(new SemanticException(waitingFor.lineNumber, state, message));
      }
    }
    throw exception;
  }

  return returnObject;
}

class ImportConstructDefinition : ConstructDefinition
{
  this()
  {
    super(0, ConstructAttributes.init, null, null, null);
  }
  final override const(ConstructObject) process(ProcessorState* state, const(Construct) construct) const
  {
    // TODO: maybe import will return a code block?
    state.findAndImport(construct);
    return null;
  }
}


// Returns null if it's a construct that has not been defined yet
private ConstructType resolveType(ProcessorState* state, const(ConstructObject) obj)
{
  {
    ConstructType type = cast(ConstructType)obj;
    if(type) {
      return type;
    }
  }
  /*
  {
    Construct construct = cast(Construct)obj;
    if(construct) {
      auto definition = state.tryFindConstruct(construct.name);
      if(!definitions) {
	return null;
      }
      throw new Exception("not implemented");
    }
  }
  */
  {
    logError("unable to convert '%s' to a type", obj.typeName);
    return null;
  }
}

class DefconConstructDefinition : ConstructDefinition
{
  this() { super(0, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) process(ProcessorState* state, const(Construct) construct) const
  {
    auto constructName = construct.getArg!ConstructSymbol(state, 0).value;

    //
    // Handle Named Arguments
    //
    ConstructAttributes attributes;
    ConstructType evalTo = null;
    {
      size_t namedArgsRecognized = 0;
      // Handle evalTo
      {
        auto evalToObj = construct.namedArgs.get("evalTo", null);
        if(evalToObj) {
          namedArgsRecognized++;
          evalTo = resolveTo!ConstructType(state, evalToObj);
        }
      }
      // Handle noscope
      {
        auto addSymbolsToParentScopeObj = construct.namedArgs.get("addSymbolsToParentScope", null);
        if(addSymbolsToParentScopeObj) {
	  if(addSymbolsToParentScopeObj) {
	    namedArgsRecognized++;
	    if(resolveTo!ConstructBool(state, addSymbolsToParentScopeObj).value) {
	      attributes |= ConstructAttribute.addSymbolsToParentScope;
	    }
	  }
        }
      }
      // Handle openScope
      {
        auto openScopeObj = construct.namedArgs.get("openScope", null);
        if(openScopeObj) {
          namedArgsRecognized++;
          if(resolveTo!ConstructBool(state, openScopeObj).value) {
	    attributes |= ConstructAttribute.openScope;
	  }
        }
      }
      

      if(namedArgsRecognized < construct.namedArgs.length) {
        auto unrecognized = "";
        foreach(name; construct.namedArgs.byKey) {
          if(
             name != "evalTo" &&
             name != "noscope" &&
             name != "openScope")
            {
              if(unrecognized.length > 0) {
                unrecognized ~= ", ";
              }
              unrecognized ~= name;
          }
        }
        throw new InvalidConstructException(construct, state, format("the defcon '%s' construct contains unrecognized named arguments: %s", constructName, unrecognized));
      }
    }
    if((attributes & ConstructAttribute.openScope) &&
       (attributes & ConstructAttribute.addSymbolsToParentScope)) {
      throw new InvalidConstructException(construct, state, format("the defcon '%s' construct cannot have both openScope and addSymbolsToParentScope attributes set to true", constructName));
    }

    //
    // Handle unnamed args
    //
    size_t nextArgIndex = 1;
    auto requiredParams = appender!(const(ConstructParam)[])();
    if(nextArgIndex < construct.args.length)
    {
      auto paramListObj = cast(ConstructList)construct.args[nextArgIndex];
      if(paramListObj)
      {
        auto thisListIndex = nextArgIndex;
        nextArgIndex++;
	auto paramList = paramListObj.items;
	size_t listIndex = 0;
	for(; listIndex < paramList.length; listIndex++) {
          
	  // Param name
	  auto paramNameSymbol = paramListObj.getItem!ConstructSymbol
	    (state, construct, thisListIndex, listIndex).value;
	  listIndex++;
	  if(listIndex >= paramList.length) {
	    requiredParams.put(ConstructParam(paramNameSymbol));
	    break;
	  }

	  // Param type
	  ConstructType paramType;
	  {
	    auto nextArg = paramList[listIndex];
	    if(nextArg.isListBreak) {
	      requiredParams.put(ConstructParam(paramNameSymbol));
	      continue;
	    }
	    paramType = resolveType(state, nextArg);
	    if(!paramType) {
	      throw new Exception(format("Could not resolve '%s' to type", nextArg.typeName));
	    }
            requiredParams.put(ConstructParam(paramNameSymbol, paramType));
	    listIndex++;
	    if(listIndex >= paramList.length) {
	      break;
	    }
	  }

	  auto nextArg = paramList[listIndex];
	  if(!nextArg.isListBreak) {
	    throw new InvalidConstructException
	      (nextArg, state, format("expected a ',' but got '%s' of type '%s'", nextArg, nextArg.typeName));
	  }
	}
      }
    }
    
    //
    // Process Optional Parameters
    //
    auto optionalParams = appender!(const(ConstructOptionalParam)[])();
    if(nextArgIndex < construct.args.length)
    {
      ConstructList paramListObj = cast(ConstructList)construct.args[nextArgIndex];
      if(paramListObj) {
        auto thisListIndex = nextArgIndex;
        nextArgIndex++;

        auto paramList = paramListObj.items;
        size_t listIndex = 0;

        for(; listIndex < paramList.length; listIndex++) {

          // Param name
          auto paramNameSymbol = paramListObj.getItem!ConstructSymbol
            (state, construct, thisListIndex, listIndex).value;
          listIndex++;
          if(listIndex >= paramList.length) {
            optionalParams.put(ConstructOptionalParam(paramNameSymbol));
            break;
          }

          // Param type
          ConstructType paramType;
          {
            auto nextArg = paramList[listIndex];
            if(nextArg.isListBreak) {
              optionalParams.put(ConstructOptionalParam(paramNameSymbol));
              continue;
            }
            paramType = resolveType(state, nextArg);
            if(!paramType) {
              throw new Exception(format("Could not resolve '%s' to type", nextArg));
            }
            listIndex++;
            if(listIndex >= paramList.length) {
              optionalParams.put(ConstructOptionalParam(paramNameSymbol, paramType));
              break;
            }
          }

          // Default Value
          {
            auto nextArg = paramList[listIndex];
            if(nextArg.isListBreak) {
              optionalParams.put(ConstructOptionalParam(paramNameSymbol, paramType));
              continue;
            }
            optionalParams.put(ConstructOptionalParam(paramNameSymbol, paramType, nextArg));
          }
          listIndex++;
          if(listIndex >= paramList.length) {
            break;
          }

          auto nextArg = paramList[listIndex];
          if(!nextArg.isListBreak) {
            throw new InvalidConstructException
              (nextArg, state, format("expected a ',' but got '%s' of type '%s'", nextArg, nextArg.typeName));
          }
	    
        }
      }
    }

    // Get implementation if there is one
    if(nextArgIndex < construct.args.length) {
      auto implementation = construct.getArg!ConstructBlock(state, nextArgIndex);
      nextArgIndex++;
      state.addConstruct(constructName, new ConstructWithBlockDefinition
			 (construct.lineNumber, attributes, evalTo,
                          requiredParams.data, optionalParams.data, implementation));
      
      if(nextArgIndex < construct.args.length) {
        throw new InvalidConstructException(construct, state, "the defcon has too many arguments");
      }

    } else {
      
      auto backendFunc = loadConstructBackend(constructName);
      if(backendFunc == null) {
	throw new InvalidConstructException(construct, state, format
					    ("failed to load construct '%s' from the backend", constructName));
      }
      state.addConstruct(constructName, new FunctionConstructDefinition
			 (construct.lineNumber, attributes, evalTo,
                          requiredParams.data, optionalParams.data, backendFunc));
    }
    return null; // defcon doesn't return anything right now
  }
}

class ExecConstructDefinition : ConstructDefinition
{
  this() { super(0, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) process(ProcessorState* state, const(Construct) construct) const
  {
    bool addSymbolsToParentScope;
    bool addSymbolsToParentScopeOldValue;
    size_t recognizedNamedArgs = 0;
    {
      auto addSymbolsToParentScopeObj = construct.namedArgs.get("addSymbolsToParentScope", null);
      if(addSymbolsToParentScopeObj) {
	if(addSymbolsToParentScopeObj) {
	  recognizedNamedArgs++;
	  addSymbolsToParentScope = resolveTo!ConstructBool(state, addSymbolsToParentScopeObj).value;
	}
      }
    }
    
    if(addSymbolsToParentScope) {
      addSymbolsToParentScopeOldValue = state.currentScope.addSymbolsToParentScope;
      state.currentScope.addSymbolsToParentScope = true;
      logDebug("exec addSymbolsToParentScope (old=%s, current=true)", addSymbolsToParentScopeOldValue);
    }
    scope(exit) {
      if(addSymbolsToParentScope) {
	state.currentScope.addSymbolsToParentScope = addSymbolsToParentScopeOldValue;
      }
    }

    construct.enforceNoUnrecognizedArgs(state, recognizedNamedArgs);
    construct.enforceArgCount(state, 1);
    auto codeBlock = resolveTo!ConstructBlock(state, construct.args[0]);
    return processConstructs(state, codeBlock.constructs);
  }
}


class ThrowConstructDefinition : ConstructDefinition
{
  this() { super(0, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) process(ProcessorState* state, const(Construct) construct) const
  {
    construct.enforceArgCount(state, 1);
    auto message = construct.resolveArg!ConstructString(state, 0);
    throw new ConstructThrownException(construct, state, cast(string)message.value);
  }
}


class CatchConstructDefinition : ConstructDefinition
{
  this() { super(0, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) process(ProcessorState* state, const(Construct) construct) const
  {
    //
    // Named Args
    //
    bool addSymbolsToParentScope;
    bool addSymbolsToParentScopeOldValue;
    size_t recognizedNamedArgs = 0;
    {
      auto addSymbolsToParentScopeObj = construct.namedArgs.get("addSymbolsToParentScope", null);
      if(addSymbolsToParentScopeObj) {
	if(addSymbolsToParentScopeObj) {
	  recognizedNamedArgs++;
	  addSymbolsToParentScope = resolveTo!ConstructBool(state, addSymbolsToParentScopeObj).value;
	}
      }
    }

    if(addSymbolsToParentScope) {
      addSymbolsToParentScopeOldValue = state.currentScope.addSymbolsToParentScope;
      state.currentScope.addSymbolsToParentScope = true;
    }
    scope(exit) {
      if(addSymbolsToParentScope) {
	state.currentScope.addSymbolsToParentScope = addSymbolsToParentScopeOldValue;
      }
    }
    construct.enforceNoUnrecognizedArgs(state, recognizedNamedArgs);


    //
    // Args
    //
    auto nextArgIndex = 0;
    auto codeBlock  = construct.resolveArg!ConstructBlock(state, nextArgIndex++);
    construct.enforceHasArg(state, nextArgIndex);
    const(char)[] errorVarName = null;
    if(auto catchArgList = cast(ConstructList)construct.args[nextArgIndex]) {
      auto thisListIndex = nextArgIndex;
      nextArgIndex++;

      if(catchArgList.items.length > 0) {
        // Param name
        errorVarName = catchArgList.getItem!ConstructSymbol
          (state, construct, thisListIndex, 0).value;

        if(catchArgList.items.length > 1) {
          throw new Exception("not implemented");
          /*
          // Param type
          ConstructType paramType;
          {
          auto nextArg = paramList[listIndex];
          if(nextArg.isListBreak) {
          requiredParams.put(ConstructParam(paramNameSymbol));
          continue;
          }
          paramType = resolveType(state, nextArg);
          if(!paramType) {
            throw new Exception(format("Could not resolve '%s' to type", nextArg.typeName));
          }
          listIndex++;
          if(listIndex >= paramList.length) {
            requiredParams.put(ConstructParam(paramNameSymbol, paramType));
            break;
          }
          }
        */
        }
      }
    }
    
    auto catchBlock = construct.resolveArg!ConstructBlock(state, nextArgIndex);

    string errorMessage = null;
    try {
      return processConstructs(state, codeBlock.constructs);
    } catch(ConstructException e) {
      errorMessage = e.msg;
    }

    state.pushScope(catchBlock.lineNumber, true, false);
    scope(exit) { state.popScope(); }

    if(errorVarName) {
      state.addSymbol(errorVarName, new ConstructString(0, errorMessage));
    }
    
    return processConstructs(state, catchBlock.constructs);
  }
}

class LetConstructDefinition : ConstructDefinition
{
  this() { super(0, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) process(ProcessorState* state, const(Construct) construct) const
  {
    construct.enforceArgCount(state, 0);
    foreach(namedArg; construct.namedArgs.byKeyValue) {
      state.addSymbol(namedArg.key, namedArg.value);
    }
    return null; // doesn't return anything right now
  }
}
class SetConstructDefinition : ConstructDefinition
{
  this() { super(0, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) process(ProcessorState* state, const(Construct) construct) const
  {
    construct.enforceArgCount(state, 0);
    foreach(namedArg; construct.namedArgs.byKeyValue) {
      state.setSymbol(namedArg.key, namedArg.value);
    }
    return null; // doesn't return anything right now
  }
}

class IfConstructDefinition : ConstructDefinition
{
  this() { super(0, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) process(ProcessorState* state, const(Construct) construct) const
  {
    construct.enforceHasArg(state, 1);
    auto truth    = resolveTo!ConstructBool(state, construct.args[0]);
    auto trueCase = resolveTo!ConstructBlock(state, construct.args[1]);
    if(construct.args.length > 2) {
      throw new Exception("if construct with more args not implemented");
    }

    if(truth.value) {
      state.pushScope(trueCase.lineNumber, true, false);
      scope(exit) { state.popScope(); }
      auto result = processConstructs(state, trueCase.constructs);
      // ignore result for now
    }
    return null; // doesn't return anything right now
  }
}
class EqualsConstructDefinition : ConstructDefinition
{
  this() { super(0, ConstructAttributes.init, new PrimitiveType(0, PrimitiveTypeEnum.bool_), null, null); }
  final override const(ConstructObject) process(ProcessorState* state, const(Construct) construct) const
  {
    construct.enforceArgCount(state, 2);
    return new ConstructBool(construct.lineNumber, construct.args[0].equals(construct.args[1]));
  }
}
class ReturnConstructDefinition : ConstructDefinition
{
  this() { super(0, ConstructAttributes.init, new PrimitiveType(0, PrimitiveTypeEnum.anything), null, null); }
  final override const(ConstructObject) process(ProcessorState* state, const(Construct) construct) const
  {
    construct.enforceArgCount(state, 1);
    //return resolveTo!ConstructObject(construct.args[0]);
    // NOTE: not sure if I should be able to return a symbol.
    //       if it does return a symbol, might not be a good
    //       idea to return a symbol defined inside the construct?
    return construct.args[0];
  }
}

class ConstructConstructDefinition : ConstructDefinition
{
  this() { super(0, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) process(ProcessorState* state, const(Construct) construct) const
  {
    /*
      child.enforceNoNamedArgs();
      child.enforceArgCount(1);
      auto nameObj = child.getArg!ConstructString(0);
      builder.args.put(cast(ConstructType)child);
      return 0;
    */
    throw new Exception("not implemented");
  }
}

class StructConstructDefinition : ConstructDefinition
{
  this() { super(0, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) process(ProcessorState* state, const(Construct) construct) const
  {
    throw new Exception("not implemented");
  }
}

class SemanticsConstructDefinition : ConstructDefinition
{
  this() { super(0, ConstructAttributes.init, null, null, null); }
  final override const(ConstructObject) process(ProcessorState* state, const(Construct) construct) const
  {
    throw new Exception("not implemented");
    // Not implemented yet
    // Note: this construct will go away after processing
    //  return 0;
  }
}
