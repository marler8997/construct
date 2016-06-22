import std.stdio : writeln, writefln, stdout;
import std.array : appender, Appender;
import std.file  : read, exists, mkdir, baseName, dirName, buildNormalizedPath;
import std.json;

import construct.ir;
import backend : emit;
  
void usage(string program)
{
  writefln("Usage: %s <json-file>", program.baseName);
}
int main(string[] args)
{
  string program = args[0];
  args = args[1..$];
    
  if(args.length <= 0) {
    usage(program);
    return 0;
  }
  if(args.length > 1) {
    usage(program);
    writeln("Error: too many command line arguments");
    return 1;
  }

  string jsonFile = args[0];
  if(!exists(jsonFile)) {
    writefln("Error: json file '%s' does not exist", jsonFile);
    return 1;
  }
  string outputDir = buildNormalizedPath(jsonFile.dirName, "obj");
  if(!exists(outputDir)) {
    mkdir(outputDir);
  }
    
  writefln("[DEBUG] Loading constructs from '%s'...", jsonFile);
  stdout.flush();
  auto jsonString = cast(string)read(jsonFile);

  JSONValue[string] json;
  try {
    auto jsonRoot = parseJSON(jsonString);
    if(jsonRoot.type != JSON_TYPE.OBJECT) {
      throw new JSONException("root value must be an object");
    }
    json = jsonRoot.object;
  } catch(JSONException e) {
    writefln("Invalid JSON in file %s: %s", jsonFile, e.msg);
    return 1;
  }

  // Convert the JSON to constructs
  JSONValue[] constructsJson;
  {
    auto constructsJsonObj = json.get("constructs", JSONValue(null));
    // TODO: may want to handle if constructs is actually null differently
    if(constructsJsonObj.isNull) {
      writefln("Error in '%s': root object is missing the 'constructs' property", jsonFile);
      return 1;
    }
    if(constructsJsonObj.type != JSON_TYPE.ARRAY) {
      writefln("Error in '%s': constructs must be an ARRAY but it is a %s", jsonFile, constructsJsonObj.type);
      return 1;
    }
    constructsJson = constructsJsonObj.array;
  }

  auto constructs = new Construct[constructsJson.length];
  
  foreach(i, constructValue; constructsJson) {
    if(constructValue.type != JSON_TYPE.OBJECT) {
      writefln("Error in '%s': all constructs must be of type OBJECT, but construct at index %s of type %s", jsonFile, i, constructValue.type);
      return 1;
    }
    auto constructJson = constructValue.object;

    //
    // Check Type
    //
    {
      auto typeJson = constructJson.get("type", JSONValue(null));
      if(typeJson.isNull) {
	writefln("Error in '%s': construct object at index %s is missing the 'type' property", jsonFile, i);
	return 1;
      }
      if(typeJson.type != JSON_TYPE.STRING) {
	writefln("Error in '%s': expected a STRING for construct object 'type' property but got a %s", jsonFile, typeJson.type);
	return 1;
      }
      if(typeJson.str != "Construct") {
	writefln("Error in '%s': expected 'construct' for construct object property 'type' but got a '%s'", jsonFile, typeJson.str);
	return 1;
      }
    }

    auto construct = toConstruct(constructJson);
    if(!construct) {
      return 1; // fail
    }
    constructs[i] = cast(Construct)construct;
  }
  
  writeln("[DEBUG] Done loading constructs. Emitting code...");
  stdout.flush();

  {
    auto result = emit(outputDir, jsonFile, constructs);
    if(result == 0) {
      writeln("[DEBUG] Done emitting code, Success.");
    }
    return result;
  }
}



const(ConstructObject)[] toConstructObjects(JSONValue[] values)
{
  ConstructObject[] objects = new ConstructObject[values.length];
  foreach(i; 0..values.length) {
    objects[i] = cast(ConstructObject)toConstructObject(values[i]);
    if(!objects[i]) {
      return null; // Fail
    }
  }
  return objects;
}
const(ConstructObject) toConstructObject(JSONValue valueJson)
{
  if(valueJson.type == JSON_TYPE.STRING) {
    return new StringLiteral(valueJson.str);
  }
  if(valueJson.type == JSON_TYPE.INTEGER) {
    return new LongLiteral(valueJson.integer);
  }
  if(valueJson.type == JSON_TYPE.OBJECT) {
    JSONValue[string] object = valueJson.object;
    auto typeJson = object.get("type", JSONValue(null));
    if(typeJson.isNull) {
      // TODO: better error message
      writeln("An object is missing the 'type' property");
      return null; // fail
    }
    if(typeJson.type != JSON_TYPE.STRING) {
      // TODO: better error message
      writefln("An object 'type' property should be STRING but is %s", typeJson.type);
      return null; // fail
    }
    auto typeString = typeJson.str;
    if(typeString == "Construct") {
      return toConstruct(object);
    } else if(typeString == "StandardType") {
      return toStandardType(object);
    } else {
      writefln("Error: unknown object type '%s'", typeJson.str);
      return null; // fail
    }
  }
  
  writefln("Error: unhandled json type '%s'", valueJson.type);
  return null; // fail
}

const(Construct) toConstruct(JSONValue[string] json)
{
  string name;
  {
    auto nameJson = json.get("name", JSONValue(null));
    if(nameJson.isNull) {
      // TODO: better error message (file name and line number at least)
      writefln("Error: construct object is missing the 'name' property");
      return null; // fail
    }
    if(nameJson.type != JSON_TYPE.STRING) {
      // TODO: better error message (file name and line number at least)
      writefln("Error: expected the 'name' property to be a STRING but is a(n) %s", nameJson.type);
      return null; // fail
    }
    name = nameJson.str;
  }
    
  //
  // Get Args
  //
  const(ConstructObject)[] params = null;
  {
    auto argsJson = json.get("args", JSONValue(null));
    if(!argsJson.isNull) {
      if(argsJson.type != JSON_TYPE.ARRAY) {
	// TODO: better error message (file name and line number at least)
	writefln("Error: expected the 'args' property to be an ARRAY but is a(n) %s", argsJson.type);
	return null; // fail
      }
      params = toConstructObjects(argsJson.array);
      if(!params) {
	return null; // Fail
      }
    }
  }
  return new Construct(name, params);
}

immutable(StandardType) toStandardType(JSONValue[string] json)
{
  string name;
  {
    auto nameJson = json.get("name", JSONValue(null));
    if(nameJson.isNull) {
      // TODO: better error message (file name and line number at least)
      writefln("Error: StandardType object is missing the 'name' property");
      return null; // fail
    }
    if(nameJson.type != JSON_TYPE.STRING) {
      // TODO: better error message (file name and line number at least)
      writefln("Error: expected the 'name' property to be a STRING but is a(n) %s", nameJson.type);
      return null; // fail
    }
    name = nameJson.str;
  }
  switch(name) {
  case "int": return StandardType.int_;
  default:
    writefln("Error: unknown StandardType '%s'", name);
    return null; // fail
  }
}
