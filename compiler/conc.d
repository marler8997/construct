//
// The Construct Compiler
//
import std.stdio     : writeln, writefln, File;
import std.getopt    : getopt;
import std.exception : ErrnoException;

void usage()
{
  writeln("conc [options] <construct-file>");
}
int main(string[] args)
{
  getopt(args);

  args = args[1..$]; // Remove program argument
  if(args.length == 0) {
    usage();
    return 0;
  }

  if(args.length > 1) {
    writeln("Error: too many arguments");
    return 1;
  }

  auto sourceFilename = args[0];

  File sourceFile;
  try {
    sourceFile = File(sourceFilename, "r");
  } catch(ErrnoException e) {
    writefln("Error: file '%s' does not exist", sourceFilename);
    return 1;
  }

  foreach(line; sourceFile.byLine) {
    writeln(line);
  }
  
  return 0;
}
