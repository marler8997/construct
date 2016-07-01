module backend;

import std.stdio  : write, writeln, writefln, File, stdout;
import std.file   : mkdir, exists, setExtension, baseName;
import std.path   : dirName, buildNormalizedPath;
import std.string : format;
import std.conv   : to;

import construct.ir;
import construct.backendIR;
import construct.processor : getArg, getItem, enforceArgCount, enforceNoNamedArgs, ProcessorState, ConstructDefinition, ProcessorFunc, resolveTo;

ProcessorFunc loadConstructBackend(const(char)[] name)
{
  if(name == "message") {
    //return singleton!MessageHandler();
    return &messageHandler;
  }
  return null;
}
ConstructObject messageHandler(ProcessorState* state, const(ConstructDefinition) definition, const(Construct) construct)
{
  construct.enforceNoNamedArgs(state);
  foreach(arg; construct.args) {
    write(resolveTo!ConstructString(state, arg).value);
  }
  writeln();
  stdout.flush();
  return null;
}
