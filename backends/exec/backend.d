module backend;

import std.format : format;
import std.stdio  : write, writef, writeln, writefln, File, stdout;
import std.file   : mkdir, exists;
import std.path   : dirName, setExtension, baseName, buildNormalizedPath;
import std.conv   : to;

import construct.ir;
import construct.processor : ConstructProcessor, ProcessResult, ProcessorFunc,
                             ConstructDefinition, FunctionConstructDefinition, logDev;

void loadBackendConstructs(ConstructProcessor* processor) pure
{
  processor.addSymbol("message", new immutable FunctionConstructDefinition
                      (0, cast(string)processor.currentFile.name, ConstructAttributes.init, null, &messageHandler));
}
ProcessorFunc loadConstructBackend(const(char)[] name) pure
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
ConstructType loadBackendType(const(char)[] name) pure
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
  // wrap in debug to retain purity
  debug {
    while(true) {
      auto object = processor.consumeValue(constructSymbol, objects, argIndex);
      if(object is null) {
        throw processor.semanticError(constructSymbol.lineNumber, "the message construct does not handle void statements");
      }
      if(object.isObjectBreak) {
        break;
      } else if(auto string_ = object.asConstructString) {
        write(string_.toUtf8());
      } else if(auto number = object.asConstructUint) {
        writef("%s", number.value);
      } else if(auto bool_ = object.asConstructBool) {
        write(bool_.value ? "true" : "false");
      } else if(auto symbol = object.asConstructSymbol) {
        write("symbol:");
        write(symbol.value);
      } else {
        throw imp(format("message printing object of type %s", object.typeName));
      }
    }
    writeln();
    stdout.flush();
  }
  return null;
}
const(ConstructObject) openFileHandler(ConstructProcessor* processor,
                                       const(ConstructDefinition) definition,
                                       const(ConstructSymbol) constructSymbol,
                                       const(ConstructObject)[] objects, size_t* argIndex) pure
{
  throw imp("openFileHandler");
}
