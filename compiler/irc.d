//
// Intermediate Represesentation Compiler
//
import std.stdio     : writeln, writefln, stdout, File;
import std.file      : getcwd, exists, dirName, extension, buildNormalizedPath;
import std.getopt    : getopt;
import std.exception : ErrnoException;
import std.string    : format;
import std.process   : spawnShell, wait;

__gshared string cwd;
__gshared string compilerPath;
__gshared string constructPath;
__gshared string constructBackendsPath;


void usage()
{
  writeln("irc [options] <ir-loader> <backend>");
}
int main(string[] args)
{
  getopt(args);

  args = args[1..$]; // Remove program argument
  if(args.length == 0) {
    usage();
    return 0;
  }

  if(args.length < 2) {
    usage();
    writeln();
    writeln("Error: not enough command line arguments");
    return 1;
  }
  if(args.length > 2) {
    usage();
    writeln();
    writeln("Error: too many command line arguments");
    return 1;
  }
  string irLoader = args[0];
  string backend = args[1];

  //
  // Setup paths where to find things
  //
  cwd = getcwd();
  //writefln("[DEBUG] cwd           : %s", cwd);

  compilerPath = cwd;
  constructPath = compilerPath.dirName;
  //writefln("[DEBUG] constructPath : %s", constructPath);

  constructBackendsPath = buildNormalizedPath(constructPath, "backends");
  if(!exists(constructBackendsPath)) {
    writefln("Error: construct path '%s' seems to be incorrect because there is no 'backends' directory",
             constructPath);
    return 1;
  }

  //
  // Find ir loader
  //
  if(!exists(irLoader)) {
    writefln("Error: ir-loader '%s' does not exist", irLoader);
    return 1;
  }

  string runtimeIRFile = null;
  {
    auto ext = irLoader.extension();
    if(ext == ".con") {
      runtimeIRFile = irLoader;
      irLoader = buildNormalizedPath(compilerPath, "standardIRLoader.d");
    } else if(irLoader.extension() == ".json") {
      runtimeIRFile = irLoader;
      irLoader = buildNormalizedPath(compilerPath, "jsonIRLoader.d");
    }
  }
  
  //
  // Backend search algorithm
  //
  if(!exists(backend)) {
    string standardBackend = buildNormalizedPath(constructBackendsPath, backend);
    if(exists(standardBackend)) {
      backend = standardBackend;
    } else {
      writefln("Error: could not find backend '%s'", backend);
      return 1;
    }
  }
  string backendCode = buildNormalizedPath(backend, "backend.d");
  if(!exists(backendCode)) {
    writefln("Error: backend directory '%s' does not contain a 'backend.d' file", backend);
    return 1;
  }
  
  return exec(format("rdmd -I%s %s %s", backend, irLoader,
		     (runtimeIRFile is null) ? "" : runtimeIRFile ));
}
int exec(string command)
{
  writefln("Executing: %s", command);
  stdout.flush();

  auto pid = spawnShell(command);
  return wait(pid);
}
