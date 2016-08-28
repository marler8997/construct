module construct.parser;

import std.path : extension;

import construct.ir;
static import construct.standardParser;
//static import construct.jsonParser;

class ConstructParseException : Exception
{
  size_t constructLineNumber;
  this(size_t constructLineNumber, string msg, string codeFile = __FILE__,
       size_t codeLine = __LINE__) pure
  {
    super(msg, codeFile, codeLine);
    this.constructLineNumber = constructLineNumber;
  }
}
struct SourceFile
{
  const(char)[] name;
  const(ConstructObject)[] parsedObjects = void;
}

struct ConstructParser {
  string name;
  const(ConstructObject)[] function(const(char)[] code) pure func;
}

template standardParser(T) {
  immutable(ConstructParser) standardParser = immutable ConstructParser("Standard", &construct.standardParser.parse!T);
}

ConstructParser getParser(T)(const(char)[] filename) if(isConstructBuilder!T)
{
  auto extension = filename.extension;
  if(extension.length == 0|| extension == ".con") {
    return standardParser!T;
  }
  if(extension == ".json") {
    throw new Exception("json parser not implemented");
    //return ConstructParser("Json", &construct.jsonParser.parse);
  }
  return ConstructParser();
}
