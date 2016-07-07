module backend;

import std.stdio  : write, writeln, writefln, File, stdout;
import std.file   : mkdir, exists, setExtension, baseName;
import std.path   : dirName, buildNormalizedPath;
import std.string : format;
import std.conv   : to;

import construct.ir;
import construct.processor : ConstructProcessor, ProcessResult, ProcessorFunc, ConstructDefinition;

ProcessorFunc loadConstructBackend(const(char)[] name)
{
  if(name == "message") {
    //return singleton!MessageHandler();
    return &messageHandler;
  }
  if(name == "openFile") {
    return &openFileHandler;
  }
  return null;
}
ConstructType loadBackendType(const(char)[] name)
{
  if(name == "systemString") {
    return singleton!PrimitiveType(0, PrimitiveTypeEnum.utf8);
  }
  if(name == "FileName") {
    return singleton!PrimitiveType(0, PrimitiveTypeEnum.utf8);
  }
  return null;
}
ProcessResult messageHandler(ConstructProcessor* processor,
                             const(ConstructDefinition) definition,
                             const(ConstructSymbol) constructSymbol,
                             const(ConstructObject)[] objects, size_t argIndex)
{
  while(true) {
    if(argIndex >= objects.length) {
      throw processor.endedInsideConstruct(constructSymbol);
    }
    auto object = objects[argIndex++];
    if(object.isObjectBreak) {
      break;
    } else {
      write(processor.resolveTo!ConstructString(object).value);
    }
  }
  writeln();
  stdout.flush();
  return ProcessResult(argIndex, null);
}
ProcessResult openFileHandler(ConstructProcessor* processor,
			      const(ConstructDefinition) definition,
			      const(ConstructSymbol) constructSymbol,
			      const(ConstructObject)[] objects, size_t argIndex)
{
  throw imp("openFileHandler");
}
