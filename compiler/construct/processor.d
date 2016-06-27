module construct.processor;

import std.stdio  : writeln, writefln, stdout;
import std.file   : exists, buildNormalizedPath, read;
import std.path   : setExtension, stripExtension;
import std.array  : Appender, appender;
import std.string : startsWith, format;

import construct.ir;
import construct.backendIR;
import construct.parser : ConstructParser, cfamilyParser, ConstructParseException;

import backend : loadConstructBackend;


class TypedConstructList(T) if( is( T : ConstructObject ) )
{
  // null item represents listBreak
  const(T)[] items;
  this(const(ConstructObject)[] items = null)
  {
    this.items = items;
  }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("(");
    if(items.length > 0) {
      items[0].toString(sink);
      foreach(item; items[1..$]) {
	sink(" ");
	item.toString(sink);
      }
    }
    sink(")");
  }
  final override bool equals(const(ConstructObject) otherObj) const
  {
    auto other = cast(TypedConstructList!T)otherObj;
    return other && equals(other);
  }
  final bool equals(const(TypedConstructList!T) other) const
  {
    if(items.length != other.items.length) {
      return false;
    }
    foreach(i; 0..items.length) {
      if(!items[i].equals(other.items[i])) {
	return false;
      }
    }
    return true;
  }
  final T getItem(T)(size_t i) const
  {
    if(i >= items.length) {
      throw new Exception(format("ConstructList does not have an item at index %s", i));
    }
    return items[i];
  }
}

class InvalidConstructException: Exception
{
  const(ConstructObject) obj;
  this(const(ConstructObject) obj, string msg)
  {
    super(msg, cast(string)globalCurrentFilename, obj.lineNumber);
    this.obj = obj;
  }
}

void enforceArgCount(const(Construct) construct, size_t count)
{
  if(construct.args.length != count) {
    throw new InvalidConstructException
      (construct, format("construct '%s' requires %s parameters but have %s",
                         construct.name, count, construct.args.length));
  }
}
void enforceNoNamedArgs(const(Construct) construct)
{
  if(construct.namedArgs.length != 0) {
    throw new InvalidConstructException
      (construct, format("construct '%s' does not supported named arguments but got %s",
                         construct.name, construct.namedArgs.length));
  }
}
T getArg(T,K)(const(K) construct, size_t index) if( is( T : ConstructObject ) && is( K == Construct ) || is( K == BackendConstruct) )
{
  if(index >= construct.args.length) {
    throw new InvalidConstructException
      (construct, format("construct '%s' requires more parameters, expected '%s' at index %s but only have %s parameters",
                         construct.name, T.stringof, index, construct.args.length));
  }
  auto casted = cast(T)construct.args[index];
  if(!casted) {
    throw new InvalidConstructException
      (construct, format("construct '%s' expects a(n) '%s' parameter at index %s, but got '%s'",
                         construct.name, T.stringof, index, typeid(construct.args[index])));
  }
  return casted;
}
T getArg(T)(const(Construct) construct, const(char)[] name) if( is( T : ConstructObject ) )
{
  auto value = construct.namedParams.get(name, null);
  if(!value) {
    return null;
  }
  auto casted = cast(T)value;
  if(!casted) {
    throw new InvalidConstructException
      (construct, format("construct '%s' expects named parameter '%s' to be a(n) '%s', but it is a(n) '%s'",
                         construct.name, name, T.stringof, typeid(value)));
  }
  return casted;
}
T getItem(T)(const(ConstructList) list, const(Construct) parentConstruct, size_t listIndex, size_t index) if( is( T : ConstructObject ) )
{
  if(index >= list.items.length) {
    throw new InvalidConstructException
      (list, format("in construct '%s', the list at index %s requires more parameters, expected '%s' at index %s but only have %s parameters",
                    parentConstruct.name, listIndex, T.stringof, index, list.items.length));
  }
  auto casted = cast(T)list.items[index];
  if(!casted) {
    throw new InvalidConstructException
      (list, format("in construct '%s', the list at index %s expects a(n) '%s' parameter at index %s, but got '%s'",
                    parentConstruct.name, listIndex, T.stringof, index, typeid(list.items[index])));
  }
  return casted;
}





struct ImportPath
{
  const(char)[] path;
  const(char)[] prefix;
}
struct ImportedFile
{
  const(char)[] name;
  const(char)[] filename = void;
  const(Construct)[] source = void;
  const(BackendConstruct)[] processed = void;
}
struct Importer
{
  ImportPath[] paths;
  ImportedFile[const(char)[]] importMap;

  this(ImportPath[] paths)
  {
    this.paths = paths;
  }
  int handler(const(Construct) construct, Appender!(BackendConstruct[]) processed)
  {
    if(construct.namedArgs.length) {
      throw new Exception("The 'import' construct does not have any named parameters");
    }
    foreach(arg; construct.args) {
      ConstructString string_ = cast(ConstructString)arg;
      if(string_) {
	int result = findAndImport(string_, processed);
	if(result) return result; // fail
      } else  {
	writefln("Error: The 'import' construct does not accept '%s' parameters", typeid(arg));
	return 1; // fail
      }
    }
    return 0;
  }
  final int findAndImport(const(char)[] importName, Appender!(BackendConstruct[]) processed)
  {
    auto imported = importMap.get(importName, ImportedFile());
    if(!(imported.name is null)) {
      writefln("[PROCESSOR] %s was already imported", importName);
      return 0;
    } else {
      // Find the import file
      writefln("[PROCESSOR] searching for '%s'...", importName);
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

	auto importFile = buildNormalizedPath(path.path, nameSubPath.setExtension(".con"));
	ConstructParser parser;
	if(exists(importFile)) {
	  parser = cfamilyParser;
	  writefln("[PROCESSOR]   FOUND AT: %s", importFile);
	} else {
	  writefln("[PROCESSOR]   NOT FOUND AT: %s", importFile);

	  importFile = importFile.stripExtension;
	  if(exists(importFile)) {
	    parser = cfamilyParser;
	    writefln("[PROCESSOR]   FOUND AT: %s", importFile);
	  } else {
	    writefln("[PROCESSOR]   NOT FOUND AT: %s", importFile);
	  }
	}
	  
	if(parser.name) {
	  auto importCode = cast(const(char)[])read(importFile);
	  const(Construct)[] importSource;
	  try {
	    importSource = parser.func(importCode);
	  } catch(ConstructParseException e) {
	    writefln("Error in %s (line %s): %s", importFile, e.lineNumber, e.msg);
	    return 1;
	  }

	  // Have to do this before processing the import source
	  // so that it doesn't try to load the same import twice
	  importMap[importName] = ImportedFile(importName, importFile, importSource);
	  
	  auto importProcessed = appender!(BackendConstruct[])();
          int result = processConstructs(importFile, importSource, importProcessed);
	  if(result) {
	    writefln("Failed to process import file '%s'", importFile);
	    return 1;
	  }

	  importMap[importName].processed = importProcessed.data;
	  return 0;
	}
      }
    }
    
    writefln("Error: Could not find import '%s'", importName);
    return 1; // fail
  }
}


// unsafe if multithreaded
private __gshared const(char)[] globalCurrentFilename;
private __gshared Importer* globalImporter;
/*
struct ConstructHandler
{
  int function(void* arg, const(Construct) sourceConstruct, Appender!(Construct[]) processed) handler;
  void* arg;
}
*/
private __gshared ConstructDefinition[const(char)[]]* constructMap;

private auto importDefinition    = immutable ConstructDefinition("import"   , &importProcessor, null);
private auto defconDefinition    = immutable ConstructDefinition("defcon"   , &defconProcessor, null);
private auto constructDefinition = immutable ConstructDefinition("construct", &constructProcessor, &constructChildProcessor);
private auto structDefinition    = immutable ConstructDefinition("struct"   , &structProcessor, &structChildProcessor);
private auto semanticsDefinition = immutable ConstructDefinition("semantics", &semanticsProcessor, null);

//
// Performs semantic analysis and transformations
// on the constructs
//
BackendConstruct[] processConstructs(const(char)[] filename, ConstructDefinition[const(char)[]]* _constructMap,
			      Importer* importer, const(Construct)[] source)
{
  // Setup Global Data (unsafe if multithreaded)
  globalImporter = importer;
  constructMap = _constructMap;
  (*constructMap)["import"] = importDefinition;
  (*constructMap)["defcon"] = defconDefinition;
  (*constructMap)["construct"] = constructDefinition;
  (*constructMap)["semantics"] = semanticsDefinition;

  auto processed = appender!(BackendConstruct[])();
  auto result = processConstructs(filename, source, processed);
  return result ? null : processed.data;
}
private int processConstructs(const(char)[] filename, const(Construct)[] source, Appender!(BackendConstruct[]) processed)
{
  const(char)[] previousFilename = globalCurrentFilename;
  globalCurrentFilename = filename;
  scope(exit) { globalCurrentFilename = previousFilename; }
  
  foreach(sourceConstruct; source) {
    auto result = processConstruct(sourceConstruct, processed);
    if(result) return result; // fail
  }
  
  return 0; // success
}

private int processConstruct(const(Construct) sourceConstruct, Appender!(BackendConstruct[]) processed)
{
  auto definition = constructMap.get(sourceConstruct.name, ConstructDefinition());
  if(definition.name == null) {
    writefln("Error: unknown construct '%s', are you missing an import?", sourceConstruct.name);
    return 1;
  }
  return definition.processor(&definition, sourceConstruct, processed);
}
int importProcessor(ConstructDefinition* definition, const(Construct) construct, Appender!(BackendConstruct[]) processed)
{
  return globalImporter.handler(construct, processed);
}

// Returns null if it's a construct that has not been defined yet
private Type resolveType(const(ConstructObject) obj)
{
  {
    Type type = cast(Type)obj;
    if(type) {
      return type;
    }
  }
  {
    Construct construct = cast(Construct)obj;
    if(construct) {
      auto definition = constructMap.get(construct.name, ConstructDefinition());
      if(!definition.name) {
	return null;
      }
      throw new Exception("not implemented");
    }
  }
  {
    writefln("Error: unable to convert '%s' to a construct type", typeid(obj));
    return null;
  }
}

private int defconProcessor(ConstructDefinition* definition, const(Construct) construct, Appender!(BackendConstruct[]) processed)
{
  auto constructNameObj = construct.getArg!ConstructString(0);
  const size_t indexOfParamList = 1;
  auto paramListObj = construct.getArg!ConstructList(indexOfParamList);
  /*
  if(paramListObj.namedArgs.length) {
    writefln("Error: the parameter list for a construct definition cannot have named parameters");
    return 1; // fail
  }
  */
  
  // Process the construct definition parameters
  auto requiredParams = appender!(const(ConstructParam)[])();
  auto optionalParams = appender!(const(ConstructOptionalParam)[])();
  {
    auto paramList = paramListObj.items;
    size_t listIndex = 0;

    for(; listIndex < paramList.length; listIndex++) {

      // Param name
      auto paramName = paramListObj.getItem!ConstructString(construct, indexOfParamList, listIndex);
      listIndex++;
      if(listIndex >= paramList.length) {
	requiredParams.put(ConstructParam(paramName));
	break;
      }

      // Param type
      Type paramType;
      {
	auto nextArg = paramList[listIndex];
	if(nextArg.isListBreak) {
	  requiredParams.put(ConstructParam(paramName));
	  continue;
	}
	paramType = resolveType(nextArg);
	if(!paramType) {
	  throw new Exception(format("Could not resolve '%s' to type", nextArg));
	}
	listIndex++;
	if(listIndex >= paramList.length) {
	  requiredParams.put(ConstructParam(paramName, paramType));
	  break;
	}
      }

      // Default Value
      {
	auto nextArg = paramList[listIndex];
	if(nextArg.isListBreak) {
	  requiredParams.put(ConstructParam(paramName, paramType));
	  continue;
	}

	throw new Exception("construct arg default value not implemented");
      }
    }
    bool enteredOptionalParams = false;
  }

  auto constructName = constructNameObj.value;

  // Evaluate
  {
    auto evaluateToObj = construct.namedArgs.get("evaluateTo", null);
    if(evaluateToObj) {
      writefln("construct '%s', evaluateTo not implemented", constructName);
      return 1;
    }
  }

  // Check if the construct already exists
  if(constructName in *constructMap) {
    writefln("Error: construct '%s' has been defined twice", constructName);
    return 1;
  }

  auto constructDefinition = ConstructDefinition(constructName, requiredParams.data,
						 optionalParams.data, &defaultProcessor, &defaultChildProcessor);
  {
    auto result = loadConstructBackend(&constructDefinition);
    if(result) {
      return result;
    }
    if(!constructDefinition.backendFunc) {
      writefln("Error: there is a code bug in the backend.  It indicates it loaded the '%s' construct but it didn't set the backend function.", constructName);
      return 1;
    }
  }

  (*constructMap)[constructName] = constructDefinition;
  writefln("[PROCESSOR] processor loaded the '%s' construct", constructName);
  return 0;
}
private int constructProcessor(ConstructDefinition* definition, const(Construct) construct, Appender!(BackendConstruct[]) processed)
{
  throw new Exception("not implemented");
}
private int constructChildProcessor(ConstructDefinition* definition, const(Construct) parent,
                                    const(Construct) child, ref BackendConstructBuilder builder)
{
  child.enforceNoNamedArgs();
  child.enforceArgCount(1);
  auto nameObj = child.getArg!ConstructString(0);
  builder.args.put(cast(ConstructType)child);
  return 0;
}

private int structProcessor(ConstructDefinition* definition, const(Construct) construct, Appender!(BackendConstruct[]) processed)
{
  throw new Exception("not implemented");
}
private int structChildProcessor(ConstructDefinition* definition, const(Construct) parent,
                                 const(Construct) child, ref BackendConstructBuilder builder)
{
  throw new Exception("not implemented");
}

private int semanticsProcessor(ConstructDefinition* definition, const(Construct) construct, Appender!(BackendConstruct[]) processed)
{
  // Not implemented yet
  // Note: this construct will go away after processing
  return 0;
}

private int defaultProcessor(ConstructDefinition* definition, const(Construct) construct, Appender!(BackendConstruct[]) processed)
{
  //writefln("[PROCESSOR] processing the '%s' construct", construct.name);

  // TODO: Validate the construct
  auto builder = BackendConstructBuilder();
  foreach(arg; construct.args) {
    auto childConstruct = cast(Construct)arg;
    if(childConstruct) {
      auto childDefinition = constructMap.get(childConstruct.name, ConstructDefinition());
      if(childDefinition.name == null) {
        writefln("Error: unknown construct '%s', are you missing an import?", childConstruct.name);
        return 1;
      }
      if(!definition.childProcessor) {
        writefln("Error: construct '%s', cannot be used inside another construct", childConstruct.name);
        return 1;
      }
      auto result = definition.childProcessor(&childDefinition, construct, childConstruct, builder);
      if(result) {
        return result;
      }
    } else {
      builder.args.put(arg.toBackendObject());
    }
  }

  processed.put(new BackendConstruct(construct.lineNumber, definition.backendFunc,
				     construct.name, builder.args.data, builder.namedArgs));
  return 0;
}

private int defaultChildProcessor(ConstructDefinition* definition, const(Construct) parent,
                                  const(Construct) child, ref BackendConstructBuilder builder)
{
  writefln("[PROCESSOR] need to process construct '%s'", definition.name);
  throw new Exception("not implemented");
}

