module backend;

import std.stdio : writeln, writefln, File, stdout;
import std.file  : setExtension, baseName, buildNormalizedPath;
import std.conv  : to;

import construct.ir;

__gshared File output;
__gshared int function(const(Construct) construct)[const(char)[]] handlerMap;

int emit(string outputDir, string irLoaderFile, const(Construct)[] code)
{
  string outputFilename =
    buildNormalizedPath(outputDir, irLoaderFile.baseName.setExtension(".d"));

  writefln("OutputFile : %s", outputFilename);
  output = File(outputFilename, "w");

  //
  // Load the CORE constructs
  //
  handlerMap["import"]   = &importHandler;
  
  return emit(code);
}
int emit(const(Construct)[] constructs)
{
  foreach(construct; constructs) {
    int error = emit(construct);
    if(error) {
      return error;
    }
  }
  return 0;
}
int emit(const(Construct) construct)
{
  auto handler = handlerMap.get(construct.name, null);
  if(handler == null) {
    writefln("Error: unknown construct '%s'", construct.name);
    return 1;
  }
  return handler(construct);
}

int importHandler(const(Construct) construct)
{
  writeln("[DEBUG] got import!");
  foreach(param; construct.params) {
    auto literal = cast(StringLiteral)param;
    if(!literal) {
      writefln("Error: the 'import' construct can only have string literal parameters");
      return 1;
    }
    if(literal.value == "std.io") {
      writefln("[DEBUG] importing std.io");
      output.writeln("import std.stdio : write;");
      handlerMap["write"] = &writeHandler;
    } else if(literal.value == "std.procedural") {
      writefln("[DEBUG] importing std.procedural");
      handlerMap["function"] = &functionHandler;
      handlerMap["return"] = &returnHandler;
    } else {
      writefln("Error: unknown import '%s'", literal.value);
      return 1;
    }
  }
  return 0;
}

string toD(StandardTypeEnum type)
{
  final switch(type) {
  case StandardTypeEnum.int_: return "ptrdiff_t";
  case StandardTypeEnum.uint_: return "size_t";
  }
}

//
// std.procedural
//
int functionHandler(const(Construct) construct)
{
  auto functionName = construct.getParam!StringLiteral(0);
  auto returnType = construct.getParam!StandardType(1);
  output.writefln("%s %s()", returnType.type.toD(), functionName.value);
  output.writeln("{");
  foreach(param; construct.params[2..$]) {
    auto subConstruct = cast(Construct)param;
    if(!subConstruct) {
      writeln("Error: the 'main' construct only accepts Constructor parameters");
      return 1;
    }
    int error = emit(subConstruct);
    if(error) {
      return error;
    }
  }
  output.writeln("}");
  return 0;
}

const(char)[] toDCode(const(ConstructObject) obj)
{
  {
    StringLiteral lit = cast(StringLiteral)obj;
    if(lit) {
      return lit.value;
    }
  }
  {
    LongLiteral lit = cast(LongLiteral)obj;
    if(lit) {
      return to!string(lit.value);
    }
  }
  writefln("Error: toDCode does not handle type '%s'", typeid(obj));
  return null;
}

int returnHandler(const(Construct) construct)
{
  construct.enforceParamCount(1);
  auto code = construct.params[0].toDCode();
  if(!code) {
    return 1; // fail
  }
  output.writefln("return %s;", code);
  return 0;
}
/*
int mainHandler(Construct construct)
{
  writeln("int main(string[] args)");
  writeln("{");
  foreach(param; construct.params) {
    auto subConstruct = cast(Construct)param;
    if(!subConstruct) {
      writeln("Error: the 'main' construct only accepts Constructor parameters");
      return 1;
    }
    int error = emit(subConstruct);
    if(error) {
      return error;
    }
  }
  writeln("}");
  return 0;
}
*/
int writeHandler(const(Construct) construct)
{
  string dMethod = "write";
  /*
  string dMethod;
  if(construct.appendNewline) {
    dMethod = "write";
  } else {
    dMethod = "writeln":
  }
  */
  output.writef("%s(", dMethod);
  foreach(i, param; construct.params) {
    if(i > 0) {
      output.write(", ");
    }
    
    auto literal = cast(StringLiteral)param;
    if(!literal) {
      writeln("Error: the 'write' construct only accepts StringLiteral paramaeters");
      return 1;
    }
    output.write('"', literal.value, '"');
  }
  output.writeln(");");
  
  return 0;
}
