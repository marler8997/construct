module backend;

import std.format : format;
import std.stdio  : write, writef, writeln, writefln, File, stdout;
import std.file   : mkdir, exists;
import std.path   : dirName, setExtension, baseName, buildNormalizedPath;
import std.conv   : to;

import construct.util;
import construct.parserCore;
import construct.backendCore : ConstructType, ConstructResult, PrimitiveType, ConstructDefinition, ConstructAttributes,
                               PrimitiveTypeEnum, ConstructOptionalValue;
import construct.processor : ConstructProcessor, ProcessorFunc,
                             FunctionConstructDefinition, logDev;

void loadBackendConstructs(ConstructProcessor* processor) pure
{
  processor.addSymbol("message", new immutable FunctionConstructDefinition
                      ("message", 0, "[internal]", ConstructAttributes.init, null, &messageHandler));
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
ConstructType loadBackendType(const(char)[] name)
{
  /*
  if(name == "systemString") {
    return singleton!PrimitiveType(0, PrimitiveTypeEnum.utf8);
  }
  if(name == "FileName") {
    return singleton!PrimitiveType(0, PrimitiveTypeEnum.utf8);
  }
  */
  return null;
}
const(ConstructResult) messageHandler(ConstructProcessor* processor,
                                      const(ConstructDefinition) definition,
                                      const(ConstructSymbol) constructSymbol,
                                      const(ConstructObject)[] objects, size_t* argIndex)
{
  // wrap in debug to retain purity
  debug {
  OBJECT_LOOP:
    while(true) {

      auto result = processor.consumeValue(definition, constructSymbol, objects, argIndex).unconst;
      if(result.hasAction) {
        throw processor.semanticError(constructSymbol.lineNumber, "the message construct does not handle action constructs");
      }
      ConstructObject object = result.object.unconst;
      
      while(true) {
        if(object is null) {
          throw processor.semanticError(constructSymbol.lineNumber, "the message construct does not handle void statements");
        }
        if(auto optionalValue = object.tryAsConstructOptionalValue) {
          object = optionalValue.value.unconst;
          if(object is null) {
            write("<optional-no-value>");
            continue OBJECT_LOOP;
          }
        }
        break;
      }

      if(object.isObjectBreak) {
        break;
      } else if(auto string_ = object.tryAsConstructString) {
        write(string_.toUtf8());
      } else if(auto number = object.tryAsConstructUint) {
        writef("%s", number.value);
      } else if(auto bool_ = object.tryAsConstructBool) {
        write(bool_.value ? "true" : "false");
      } else if(auto symbol = object.tryAsConstructSymbol) {
        write("symbol:");
        write(symbol.value);
      } else {
        {
          auto nullable = object.tryAsConstructNullable;
          if(nullable && nullable.isNull) {
            write("<null>");
            continue;
          }
        }

        throw imp(format("message printing object of type %s", object.typeName));
      }
    }
    writeln();
    stdout.flush();
  }
  return ConstructResult(null);
}
const(ConstructResult) openFileHandler(ConstructProcessor* processor,
                                       const(ConstructDefinition) definition,
                                       const(ConstructSymbol) constructSymbol,
                                       const(ConstructObject)[] objects, size_t* argIndex) pure
{
  throw imp("openFileHandler");
}
