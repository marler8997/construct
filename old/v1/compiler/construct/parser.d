module construct.parser;

import std.path : extension;

import construct.ir;
static import construct.cfamilyparser;
static import construct.jsonParser;

class ConstructParseException : Exception
{
  size_t constructLineNumber;
  this(size_t constructLineNumber, string msg, string codeFile = __FILE__, size_t codeLine = __LINE__) {
    super(msg, codeFile, codeLine);
    this.constructLineNumber = constructLineNumber;
  }
}
struct SourceFile
{
  const(char)[] name;
  const(Construct)[] constructs = void;
}

struct ConstructParser {
  string name;
  const(Construct)[] function(const(char)[] code) func;
}

immutable(ConstructParser) cfamilyParser = immutable ConstructParser("CFamily", &construct.cfamilyparser.parseConstructs);

ConstructParser getParser(const(char)[] filename)
{
  auto extension = filename.extension;
  if(extension.length == 0|| extension == ".con") {
    return cfamilyParser;
  }
  if(extension == ".json") {
    return ConstructParser("Json", &construct.jsonParser.parseConstructs);
  }
  return ConstructParser();
}
