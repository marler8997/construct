module backend;

import std.stdio : writeln, writefln, File;
import std.file  : setExtension;

import construct.ir;

int emit(string irLoaderFile, ConstructCode* code)
{
  string outputFilename = irLoaderFile.setExtension(".con.txt");

  writefln("OutputFile : %s", outputFilename);
  File output = File(outputFilename, "w");
  output.writefln("// ir loaded from '%s'", irLoaderFile);
  output.writefln("// %s constructs", code.constructs.data.length);
  foreach(construct; code.constructs.data) {
    output.write(construct.name);
    output.write('(');
    foreach(param; construct.params) {
      param.write(output);
    }
    output.write(");");
    output.writeln();
  }
  return 0;
}
