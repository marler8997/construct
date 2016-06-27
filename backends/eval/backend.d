module backend;

import std.stdio  : writeln, writefln, File, stdout;
import std.file   : mkdir, exists, setExtension, baseName;
import std.path   : dirName, buildNormalizedPath;
import std.string : format;
import std.conv   : to;

import construct.ir;
import construct.backendIR;
import construct.processor : getArg, getItem, enforceArgCount;

int loadConstructBackend(ConstructDefinition* construct)
{
  if(construct.name == "message") {
    construct.backendFunc = &messageHandler;
  } else if(construct.name == "function") {
    construct.backendFunc = &functionHandler;
  } else if(construct.name == "return") {
    construct.backendFunc = &returnHandler;
  } else if(construct.name == "write") {
    construct.backendFunc = &writeHandler;
  } else {
    writefln("Error: the eval backend does not implement the '%s' construct", construct.name);
    return 1;
  }
  return 0;
}

int executeBackend(const(char)[]sourceFile, ProcessingData* processingData, const(BackendConstruct)[] constructs)
{
  return processingData.executeBackend(constructs);
}

int messageHandler(ProcessingData* processingData, const(BackendConstruct) obj)
{
  writefln("[BACKEND-EVAL] Message: %s", obj.getArg!ConstructString(0));
  return 0;
}
int functionHandler(ProcessingData* processingData, const(BackendConstruct) obj)
{
  writefln("[BACKEND-EVAL] TODO: implement function construct: %s", obj.getArg!ConstructString(0));
  return 0;
}
int returnHandler(ProcessingData* processingData, const(BackendConstruct) obj)
{
  writeln("[BACKEND-EVAL] TODO: implement return construct");
  return 0;
}
int writeHandler(ProcessingData* processingData, const(BackendConstruct) obj)
{
  writeln("[BACKEND-EVAL] TODO: implement write construct");
  return 0;
}
