//
// conc = Construct Compiler
//
module construct.loader;

import std.stdio  : writeln, writefln, stdout, File;
import std.array  : appender, Appender;
import std.file   : read, exists, mkdir, baseName, dirName, buildNormalizedPath;
import std.path   : setExtension;
import std.string : format;
import std.format : formattedWrite;

import utf8;
import construct.ir;
import construct.backendIR;
import construct.parser : getParser, ConstructParseException;
import construct.processor : ImportPath, Importer, processConstructs, InvalidConstructException;

import backend : executeBackend;

void usage(string program)
{
  writefln("Usage: %s <construct-file>", program.baseName);
}
int main(string[] args)
{
  string program = args[0];
  args = args[1..$];
    
  if(args.length <= 0) {
    usage(program);
    return 0;
  }
  if(args.length > 1) {
    usage(program);
    writeln("Error: too many command line arguments");
    return 1;
  }

  string constructFile = args[0];
  if(!exists(constructFile)) {
    writefln("Error: construct file '%s' does not exist", constructFile);
    return 1;
  }

  //
  // Find the correct loader based on the file extension
  //
  auto parser = getParser(constructFile);
  if(!parser.name) {
    writefln("Error: could not find a parser for file '%s'", constructFile);
    return 1; // fail
  }

  
  writefln("[CONC] Loading constructs from '%s'...", constructFile);
  stdout.flush();

  auto constructCode = cast(string)read(constructFile);

  const(Construct)[] sourceConstructs;
  try {
    sourceConstructs = parser.func(constructCode);
  } catch(ConstructParseException e) {
    if(e.lineNumber) {
      writefln("Error in %s (line %s): %s", constructFile, e.lineNumber, e.msg);
    } else {
      writefln("Error in %s: %s", constructFile, e.msg);
    }
    return 1;
  }

  Importer importer = Importer([ImportPath(buildNormalizedPath("..","std"), "std")]);
  
  writeln("[CONC] Done loading constructs. Processing them...");
  stdout.flush();
  BackendConstruct[] processedConstructs;
  ProcessingData processingData;
  {
    try {
      processedConstructs = processConstructs
        (constructFile, &processingData.constructMap, &importer, sourceConstructs);
      if(!processedConstructs) {
        return 1; // fail
      }
    } catch(InvalidConstructException e) {
      writefln("Error in '%s' line %s: %s", e.file, e.line, e.msg);
      return 1; // fail
    }
  }

  writefln("[CONC] Loaded '%s' constructs", processingData.constructMap.length);
  
  writeln("[CONC] Done processing constructs. Passing them to the backend...");
  stdout.flush();
  {
    int result;
    try {
      result = executeBackend(constructFile, &processingData, processedConstructs);
    } catch(InvalidConstructException e) {
      if(e.line) {
	writefln("Error in '%s' line %s: %s", e.file, e.line, e.msg);
      } else {
	writefln("Error in '%s': %s", e.file, e.msg);
      }
      return 1; // fail
    }
    
    if(result == 0) {
      writeln("Success");
    } else {
      writeln("FAIL");
    }
    return result;
  }
}
