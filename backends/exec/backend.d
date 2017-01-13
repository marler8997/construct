module backend;

import std.format : format;
import std.stdio  : write, writef, writeln, writefln, File, stdout;
import std.file   : mkdir, exists;
import std.path   : dirName, setExtension, baseName, buildNormalizedPath;
import std.conv   : to;

import construct.util;
import construct.parserCore;
import construct.backendCore : ConstructType, ConstructResult, PrimitiveType, ConstructUserDefinedType,
                               ConstructDefinition, ConstructAttributes,
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

void importBackendPackage(ConstructProcessor* processor, const(char)[] packageName, const(char)[][] limitList)
{
  throw new Exception(format("this backend does not have package '%s'", packageName));
}

immutable(ConstructType) loadBackendType(const(char)[] name)
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
    while(true) {
      size_t nextIndex = *argIndex;
      if(nextIndex > objects.length) {
        throw processor.semanticError(constructSymbol.lineNumber, "message construct missing the ending ';'");
      }
      if(objects[nextIndex].isSymbolOf(";")) {
        // message construct done
        (*argIndex)++;
        break;
      }
      auto result = processor.consumeValue(definition, constructSymbol, objects, argIndex).unconst;
      if(result.hasAction) {
        throw processor.semanticError(constructSymbol.lineNumber, "the message construct does not handle action constructs");
      }
      printObject(processor, constructSymbol, result.object.unconst);
    }
    writeln();
    stdout.flush();
  }
  return ConstructResult(null);
}
static void printObject(ConstructProcessor* processor, const(ConstructSymbol) constructSymbol, ConstructObject object)
{
  while(true) {
    if(object is null) {
      throw processor.semanticError(constructSymbol.lineNumber, "the message construct does not handle void statements");
    }
    if(auto optionalValue = object.tryAsConstructOptionalValue) {
      object = optionalValue.value.unconst;
      if(object is null) {
        write("<optional-no-value>");
        return;
      }
    }
    break;
  }

  if(auto string_ = object.tryAsConstructString) {
    write(string_.toUtf8());
  } else if(auto integer = object.tryAsConstructInteger) {
    writef("%s", integer.value);
  } else if(auto bool_ = object.tryAsConstructBool) {
    write(bool_.value ? "true" : "false");
  } else if(auto symbol = object.tryAsConstructSymbol) {
    write("symbol:");
    write(symbol.value);
  } else if(auto tuple = object.tryAsConstructTuple) {
    write("tuple(");
    bool atFirst = true;
    foreach(tupleItem; tuple.objects) {
      if(atFirst) atFirst = false; else write(" ");
      printObject(processor, constructSymbol, tupleItem.unconst);
    }
    write(")");
  } else {
    {
      auto nullable = object.tryAsConstructNullable;
      if(nullable && nullable.isNull) {
        write("<null>");
        return;
      }
    }

    throw imp(format("message printing object of type %s", object.typeName));
  }
}
const(ConstructResult) openFileHandler(ConstructProcessor* processor,
                                       const(ConstructDefinition) definition,
                                       const(ConstructSymbol) constructSymbol,
                                       const(ConstructObject)[] objects, size_t* argIndex) pure
{
  throw imp("openFileHandler");
}
