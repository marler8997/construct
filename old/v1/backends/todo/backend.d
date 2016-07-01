module backend;

import std.stdio  : writeln, writefln, File, stdout;
import std.file   : mkdir, exists, setExtension, baseName;
import std.path   : dirName, buildNormalizedPath;
import std.string : format;
import std.conv   : to;

import construct.ir;
import construct.processor : getArg, getItem, enforceArgCount;

int loadConstructBackend(ConstructDefinition* construct)
{
  if(construct.name == "item") {
    construct.backendFunc = &itemHandler;
  } else if(construct.name == "todo") {
    construct.backendFunc = &todoHandler;
  } else {
    writefln("Error: the todo backend does not implement the '%s' construct", construct.name);
    return 1;
  }
  return 0;
}

//File output;
int executeBackend(const(char)[]sourceFile, ProcessingData* processingData, const(BackendObject)[] constructs)
{
  /*
  string outputDir = buildNormalizedPath(sourceFile.dirName, "obj");
  if(!exists(outputDir)) {
    mkdir(outputDir);
  }
  string outputFilename = buildNormalizedPath
    (outputDir, sourceFile.baseName.setExtension(".d"));
    
  writefln("[DSOURCE-BACKEND] OutputFile : %s", outputFilename);
  stdout.flush();
  output = File(outputFilename, "w");
  */
  return processingData.executeBackend(constructs);
}

int todoHandler(ProcessingData* processingData, const(BackendObject) construct)
{
  writeln("got a todo");
  return 0;
}
int itemHandler(ProcessingData* processingData, const(BackendObject) construct)
{
  writeln("got an item");
  return 0;
}
