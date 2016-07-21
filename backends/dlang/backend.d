module backend;

import std.stdio  : write, writef, writeln, writefln, File, stdout;
import std.file   : mkdir, exists, setExtension, baseName;
import std.path   : dirName, buildNormalizedPath;
import std.string : format;
import std.conv   : to;

import construct.ir;
import construct.processor;
//import construct.processor : ConstructProcessor, ProcessResult, ProcessorFunc, ConstructDefinition, logDev;

ProcessorFunc loadConstructBackend(const(char)[] name)
{
  if(name == "message") {
    //return singleton!MessageHandler();
    return &messageHandler;
  }
  if(name == "mainFunction") {
    return &mainFunctionHandler;
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
const(ConstructObject) mainFunctionHandler(ConstructProcessor* processor,
                                           const(ConstructDefinition) definition,
                                           const(ConstructSymbol) constructSymbol,
                                           const(ConstructObject)[] objects, size_t* argIndex)
{
  auto argsSymbol = processor.consumeSymbol(constructSymbol, objects, argIndex);
  auto code = processor.consumeTypedValue!ConstructBlock(constructSymbol, objects, argIndex);
  
  // somehow need to make the args symbol visibile to the emitted code
  emitBlock(processor, code.objects);
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



const(ConstructObject) emitBlock(ConstructProcessor* processor, const(ConstructObject)[] objects)
{
  size_t index = 0;
  size_t lineNumber = 0;
  while(index < objects.length) {
    ConstructObject result;
      
    auto rawObject = objects[index];
    if(auto block = rawObject.asConstructBlock) {
      throw imp("emitting sub block");
      //pushScope(block.lineNumber, ScopeType.block, true);
      //scope(exit) { popScope(); }
      //result = emitBlock(block.objects).unconst;
      //index++;
    } else {
      lineNumber = rawObject.lineNumber;
      result = emitValueAlreadyCheckedIndex(processor, objects, &index).unconst;
    }
      
    if(result) {
      if(auto return_ = result.asConstructReturn) {
        return return_;
      }
      throw new ConstructException(format("unhandled statement value (type %s)", result.typeName));
                                   
    }
  }
  return null;
}
// Assumption: *index < objects.length
const(ConstructObject) emitValueAlreadyCheckedIndex(ConstructProcessor* processor,
                                                    const(ConstructObject)[] objects, size_t* index)
{
  assert(*index < objects.length);
    
  auto object = objects[*index];
  (*index)++;
  if(auto symbol = object.asConstructSymbol.unconst) {

    while(true) {
      logDebug("looking up symbol '%s'...", symbol.value);
      auto symbolEntries = processor.tryLookupSymbol(symbol.value);
      if(!symbolEntries.first) {
        throw new ConstructException(format("symbol '%s' does not exist", symbol.value));
      }
      if(symbolEntries.moreCount > 0) {
        throw processor.semanticError(symbol.lineNumber, format
                                      ("symbol '%s' has multiple entries at the same scope", symbol.value));
      }

      if(auto definition = symbolEntries.first.asConstructDefinition) {
        //logDebug("processing '%s' construct", symbol.value);
        //return definition.process(&this, symbol, objects, index);
        throw imp(format("emitting construct (name=%s)", symbol.value));
      }

      return symbolEntries.first.asOneConstructObject;
    }
  } else {
    return object;
  }
}
