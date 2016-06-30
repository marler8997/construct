//
// conc = Construct Compiler
//
import std.stdio  : writeln, writefln, stdout, File;
import std.getopt : getopt;
import std.array  : appender, Appender;
import std.file   : read, exists, mkdir, baseName, dirName, buildNormalizedPath;
import std.path   : setExtension;
import std.string : format;
import std.format : formattedWrite;

import utf8;
import construct.ir;
import construct.backendIR;
import construct.parser : SourceFile, getParser, ConstructParseException;
import construct.processor : ProcessorState, ImportPath, processConstructs, SemanticException, verbose;

void usage(string program)
{
  writefln("Usage: %s <source-file>", program.baseName);
}
int main(string[] args)
{
  string program = args[0];

  getopt(args,
	 "v|verbose", &verbose);
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

  SourceFile constructFile = SourceFile(args[0]);
  if(!exists(constructFile.name)) {
    writefln("Error: construct file '%s' does not exist", constructFile.name);
    return 1;
  }

  auto parser = getParser(constructFile.name);
  if(!parser.name) {
    writefln("Error: could not find a parser for file '%s'", constructFile.name);
    return 1; // fail
  }
  
  //writefln("[CONC] Loading constructs from '%s'...", constructFile.name);
  //stdout.flush();
  auto constructCode = cast(string)read(constructFile.name);
  try {
    constructFile.constructs = parser.func(constructCode);
  } catch(ConstructParseException e) {
    if(e.constructLineNumber) {
      writefln("%s(%s): ParseError: %s", constructFile.name, e.constructLineNumber, e.msg);
    } else {
      writefln("%s: ParseError: %s", constructFile.name, e.msg);
    }
    return 1;
  }

  ProcessorState processorState = ProcessorState([ImportPath(buildNormalizedPath("..","std"), "std")]);

  //writeln("[CONC] Done loading constructs. Processing them...");
  //stdout.flush();
  try {
    processConstructs(&processorState, constructFile);
  } catch(SemanticException e) {
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
    if(e.constructLineNumber) {
      writefln("%s(%s): ParseError: %s", constructFile.name, e.constructLineNumber, e.msg);
    } else {
      writefln("%s: ParseError: %s", constructFile.name, e.msg);
    }
    return 1;
  } catch(Exception e) {
    stdout.flush();
    writeln(e);
    return 1; // fail
  }
  
  return 0;
}
