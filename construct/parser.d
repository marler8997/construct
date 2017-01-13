module construct.parser;

import std.path : extension;

import construct.parserCore;
static import construct.standardParser;
//static import construct.jsonParser;

struct SourceFile
{
  string relativeName; // should be normalized
  string absoluteName; // should be normalized
  string source;
  const(ConstructObject)[] parsedObjects = void;
}

struct ConstructParser {
  string name;
  const(ConstructObject)[] function(string code) pure func;
}

auto standardParser = immutable ConstructParser("Standard", &construct.standardParser.parse);

ConstructParser getParser(T)(const(char)[] filename) if(isConstructBuilder!T)
{
  auto extension = filename.extension;
  if(extension.length == 0|| extension == ".con") {
    return standardParser;
  }
  if(extension == ".json") {
    throw new Exception("json parser not implemented");
    //return ConstructParser("Json", &construct.jsonParser.parse);
  }
  return ConstructParser();
}
