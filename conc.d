//
// conc = Construct Compiler
//
import std.stdio  : writeln, writefln, stdout, File;
import std.getopt : getopt;
import std.array  : appender, Appender;
import std.file   : read, exists, mkdir;
import std.path   : setExtension, baseName, dirName, buildNormalizedPath, absolutePath;
import std.string : format;
import std.format : formattedWrite;

import utf8;
import backend : loadBackendConstructs;
import construct.logging;
import construct.parserCore : isConstructBuilder, ConstructObject, ConstructException;
import construct.parser     : SourceFile, getParser, ConstructParseException;
import construct.processor  : ImportPath, ConstructProcessor, SemanticException;

void usage(string program)
{
  writefln("Usage: %s <source-file>", program.baseName);
}
int main(string[] args)
{
  string program = args[0];
  string[] importPathStrings;
  getopt(args,
	 "v|verbose", &verbose,
         "I", &importPathStrings);
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

  // Check command line arguments
  debug {
  } else {
    if(verbose) {
      writeln("Error: cannot enable verbose logging because program was not compiled in debug mode");
      return 1;
    }
  }

  SourceFile constructFile;
  {
    auto sourceFileString = args[0];
    if(!exists(sourceFileString)) {
      writefln("Error: construct file '%s' does not exist", sourceFileString);
      return 1;
    }
    auto normalizedSourceFile = buildNormalizedPath(sourceFileString);
    logDebug("source '%s' normalized to '%s'", sourceFileString, normalizedSourceFile);
    auto absoluteNormalizedSourceFile = normalizedSourceFile.absolutePath.buildNormalizedPath;
    constructFile = SourceFile(normalizedSourceFile, absoluteNormalizedSourceFile);
  }

  static if(isConstructBuilder!(Appender!(const(ConstructObject)[]))) {

  } else static assert(0, "appender is not a construct builder");

  auto parser = getParser!(Appender!(const(ConstructObject)[]))(constructFile.relativeName);
  if(!parser.name) {
    writefln("Error: could not find a parser for file '%s'", constructFile.relativeName);
    return 1; // fail
  }

  //writefln("[CONC] Loading constructs from '%s'...", constructFile.name);
  //stdout.flush();
  auto constructCode = cast(string)read(constructFile.relativeName);
  try {
    constructFile.parsedObjects = parser.func(constructCode);
  } catch(ConstructParseException e) {
    if(e.constructLineNumber) {
      writefln("%s(%s): ParseError: %s", constructFile.relativeName, e.constructLineNumber, e.msg);
    } else {
      writefln("%s: ParseError: %s", constructFile.relativeName, e.msg);
    }
    return 1;
  }

  ImportPath[] importPaths = new ImportPath[2 + importPathStrings.length];
  importPaths[0] = ImportPath(constructFile.relativeName.dirName,
                              constructFile.absoluteName.dirName);
  {
    string libraryPath = buildNormalizedPath(__FILE_FULL_PATH__.dirName, "lib");
    importPaths[1] = ImportPath(libraryPath, libraryPath);
  }
  foreach(i, importPathString; importPathStrings) {
    if(!exists(importPathString)) {
      writefln("Error: import path '%s' does not exist", importPathString);
      return 1;
    }
    auto normalizedPath = buildNormalizedPath(importPathString);
    logDebug("import path '%s' normalized to '%s'", importPathString, normalizedPath);
    auto absoluteNormalizedPath = normalizedPath.absolutePath.buildNormalizedPath;
    importPaths[2 + i] = ImportPath(normalizedPath, absoluteNormalizedPath);
  }
  
  ConstructProcessor processor = ConstructProcessor(importPaths);
  processor.loadExtendedConstructs();
  loadBackendConstructs(&processor);
  //writeln("[CONC] Done loading constructs. Processing them...");
  //stdout.flush();
  try {
    processor.process(constructFile);
  } catch(SemanticException e) {
    writeln(); // blank line
    if(e.errorList.data.length) {
      foreach(other; e.errorList.data) {
	writefln("%s(%s): SemanticError: %s", other.file, other.line, other.msg);
      }
    } else {
      writefln("%s(%s): SemanticError: %s", e.file, e.line, e.msg);
    }
    //writeln(e);
    return 1; // fail
  } catch(ConstructParseException e) {
    writeln(); // blank line
    if(e.constructLineNumber) {
      writefln("%s(%s): ParseError: %s", constructFile.relativeName, e.constructLineNumber, e.msg);
    } else {
      writefln("%s: ParseError: %s", constructFile.relativeName, e.msg);
    }
    return 1;
  } catch(ConstructException e) {
    writeln(); // blank line
    writeln(e);
    return 1; // fail
  }
  return 0;
}
