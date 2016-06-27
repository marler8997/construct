module construct.jsonParser;

import std.stdio  : writeln, writefln, stdout;
import std.array  : appender, Appender;
import std.file   : read, exists, mkdir, baseName, dirName, buildNormalizedPath;
import std.string : format;
import std.json;

import construct.ir;
import construct.parser : ConstructParseException;

const(Construct)[] parseConstructs(const(char)[] code)
{
  JSONValue[] constructsJson;
  try {
    auto jsonRoot = parseJSON(code);
    if(jsonRoot.type != JSON_TYPE.ARRAY) {
      throw new ConstructParseException
	(0, format("root value must be an ARRAY but got a(n) %s", jsonRoot.type));
    }
    constructsJson = jsonRoot.array;
  } catch(JSONException e) {
    throw new ConstructParseException(e.line, e.msg);
  }

  Construct[] constructs = new Construct[constructsJson.length];
  
  foreach(i, jsonValue; constructsJson) {
    if(jsonValue.type != JSON_TYPE.OBJECT) {
      throw new ConstructParseException
	(0, format("all JSON values in the root array must be objects, but got a(n) %s at index %s", jsonValue.type, i));
    }
    auto constructJson = jsonValue.object;

    /*
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
    */

    constructs[i] = cast(Construct)toConstruct(constructJson);
  }
  return constructs;
}



ConstructObject[] toConstructObjects(JSONValue[] values)
{
  ConstructObject[] objects = new ConstructObject[values.length];
  foreach(i; 0..values.length) {
    objects[i] = cast(ConstructObject)toConstructObject(values[i]);
  }
  return objects;
}
ConstructObject toConstructObject(JSONValue valueJson)
{
  if(valueJson.type == JSON_TYPE.STRING) {
    return ConstructObject.fromUnquoted(0, valueJson.str);
  }
  if(valueJson.type == JSON_TYPE.INTEGER) {
    return new LongLiteral(0, valueJson.integer);
  }
  if(valueJson.type == JSON_TYPE.ARRAY) {
    return new ConstructList(0, toConstructObjects(valueJson.array));
  }
  if(valueJson.type == JSON_TYPE.OBJECT) {
    JSONValue[string] object = valueJson.object;
    auto typeJson = object.get("type", JSONValue(null));
    if(typeJson.isNull) {
      // TODO: better error message
      throw new ConstructParseException
	(0, "An object is missing the 'type' property");
    }
    if(typeJson.type != JSON_TYPE.STRING) {
      // TODO: better error message
      throw new ConstructParseException
	(0, format("An object 'type' property should be STRING but is %s", typeJson.type));
    }
    auto typeString = typeJson.str;
    if(typeString == "construct") {
      return toConstruct(object);
    } else if(typeString == "type") {
      return toPrimitiveType(object);
    } else {
      throw new ConstructParseException
	(0, format("unknown object type '%s'", typeJson.str));
    }
  }
  
  throw new ConstructParseException
    (0, format("unhandled json type '%s'", valueJson.type));
}

Construct toConstruct(JSONValue[string] json)
{
  string name;
  {
    auto nameJson = json.get("name", JSONValue(null));
    if(nameJson.isNull) {
      // TODO: better error message (file name and line number at least)
      throw new ConstructParseException
	(0, "construct object is missing the 'name' property");
    }
    if(nameJson.type != JSON_TYPE.STRING) {
      // TODO: better error message (file name and line number at least)
      throw new ConstructParseException
	(0, format("expected the 'name' property to be a STRING but is a(n) %s", nameJson.type));
    }
    name = nameJson.str;
  }
    
  //
  // Get Args
  //
  const(ConstructObject)[] args = null;
  {
    auto argsJson = json.get("args", JSONValue(null));
    if(!argsJson.isNull) {
      if(argsJson.type != JSON_TYPE.ARRAY) {
	// TODO: better error message (file name and line number at least)
	throw new ConstructParseException
	  (0, format("expected the 'args' property to be an ARRAY but is a(n) %s", argsJson.type));
      }
      args = toConstructObjects(argsJson.array);
    }
  }
  return new Construct(0, name, args);
}

PrimitiveType toPrimitiveType(JSONValue[string] json)
{
  string name;
  {
    auto nameJson = json.get("name", JSONValue(null));
    if(nameJson.isNull) {
      // TODO: better error message (file name and line number at least)
      throw new ConstructParseException
	(0, "PrimitiveType object is missing the 'name' property");
    }
    if(nameJson.type != JSON_TYPE.STRING) {
      // TODO: better error message (file name and line number at least)
      throw new ConstructParseException
	(0, format("expected the 'name' property to be a STRING but is a(n) %s", nameJson.type));
    }
    name = nameJson.str;
  }
  auto type = ConstructObject.lookupType(0, name);
  if(!type) {
    throw new ConstructParseException(0, format("unknown PrimitiveType '%s'", name));
  }
  return type;
}
