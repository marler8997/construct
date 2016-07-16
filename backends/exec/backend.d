module backend;

import std.stdio  : write, writef, writeln, writefln, File, stdout;
import std.file   : mkdir, exists, setExtension, baseName;
import std.path   : dirName, buildNormalizedPath;
import std.string : format;
import std.conv   : to;

import construct.ir;
import construct.processor : ConstructProcessor, ProcessResult, ProcessorFunc, ConstructDefinition, logDev;

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
  if(name == "string") {
    return singleton!PrimitiveType(0, PrimitiveTypeEnum.utf8);
  }
  if(name == "systemString") {
    return singleton!PrimitiveType(0, PrimitiveTypeEnum.utf8);
  }
  if(name == "FileName") {
    return singleton!PrimitiveType(0, PrimitiveTypeEnum.utf8);
  }
  return null;
}
const(ConstructObject) messageHandler(ConstructProcessor* processor,
                                      const(ConstructDefinition) definition,
                                      const(ConstructSymbol) constructSymbol,
                                      const(ConstructObject)[] objects, size_t* argIndex)
{
  while(true) {
    auto object = processor.consumeValue(constructSymbol, objects, argIndex);
    if(object is null) {
      throw processor.semanticError(constructSymbol.lineNumber, "the message construct does not handle void statements");
    }
    if(object.isObjectBreak) {
      break;
    } else if(auto string_ = object.asConstructUtf8) {
      write(string_.value);
    } else if(auto number = object.asConstructUint) {
      writef("%s", number.value);
    } else if(auto bool_ = object.asConstructBool) {
      write(bool_.value ? "true" : "false");
    } else {
      throw imp(format("message printing object of type %s", object.typeName));
    }
  }
  writeln();
  stdout.flush();
  return null;
}
const(ConstructObject) openFileHandler(ConstructProcessor* processor,
                                       const(ConstructDefinition) definition,
                                       const(ConstructSymbol) constructSymbol,
                                       const(ConstructObject)[] objects, size_t* argIndex)
{
  throw imp("openFileHandler");
}
