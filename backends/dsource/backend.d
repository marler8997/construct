module backend;

import std.stdio  : writeln, writefln, File, stdout;
import std.file   : setExtension, baseName, buildNormalizedPath;
import std.string : format;
import std.conv   : to;

import construct.ir;
import construct.processor : getArg, getItem, enforceArgCount;

Exception implement(string message)
{
  return new Exception("not implemented: " ~ message);
}

enum string defaultExtension = ".d";
int loadConstructBackend(ConstructDefinition* construct)
{
  if(construct.name == "function") {
    construct.emitter = &functionEmitter;
  } else if(construct.name == "return") {
    construct.emitter = &returnEmitter;
  } else if(construct.name == "write") {
    construct.emitter = &writeEmitter;
  } else {
    writefln("Error: the dsource backend does not implement the '%s' construct", construct.name);
    return 1;
  }
  return 0;
}
string toD(PrimitiveTypeEnum type)
{
  switch(type) {
  case PrimitiveTypeEnum.int_: return "ptrdiff_t";
  case PrimitiveTypeEnum.uint_: return "size_t";
  case PrimitiveTypeEnum.utf8: return "char[]";
  default:
    throw new EmitException(format("toD for type '%s' is not implemented", type));
  }
}
const(char)[] toD(ConstructString lit)
{
  return lit.value;
}
const(char)[] toD(LongLiteral lit)
{
  return to!string(lit.value);
}
const(char)[] toD(const(ConstructObject) obj)
{
  {
    PrimitiveType primitiveType = cast(PrimitiveType)obj;
    if(primitiveType) {
      return primitiveType.toD;
    }
  }
  {
    ConstructString lit = cast(ConstructString)obj;
    if(lit) {
      return lit.toD;
    }
  }
  {
    LongLiteral lit = cast(LongLiteral)obj;
    if(lit) {
      return lit.toD;
    }
  }
  writefln("Error: toD does not handle type '%s'", typeid(obj));
  return null;
}

class EmitException : Exception
{
  this(string msg) {
    super(msg);
  }
}
const(char)[] toD(const(Type) type)
{
  {
    PrimitiveType primitiveType = cast(PrimitiveType)type;
    if(primitiveType) {
      return primitiveType.typeEnum.toD;
    }
  }
  /*
  {
    Construct construct = cast(Construct)type;
    if(construct) {
      if(construct.name == 'array') {
	return new ArrayType(
      } else if(construct.name == 'const') {
	
      } else {
	writefln("Error: toDeclType does not handle construct '%s'", construct.name);
	return null;
      }
    }
  }
  */
  throw new EmitException
    (format("Error: toDeclType does not handle type '%s'", typeid(type)));
}

//
// std.procedural
//
int functionEmitter(Emitter* emitter, const(Construct) construct)
{
  auto functionNameConstruct = construct.getArg!ConstructString(0);
  auto returnTypeConstruct = construct.getArg!PrimitiveType(1);
  emitter.output.writef("%s %s(", returnTypeConstruct.toD, functionNameConstruct.toD);

  const size_t constructIndexOfArgs = 2;
  auto args = construct.getArg!ConstructList(constructIndexOfArgs);
  for(auto i = 0; i < args.items.length; i++) {
    if(args.items[i].isListBreak) {
      emitter.output.write(", ");
    } else {
      auto argName = args.getItem!ConstructString(construct, constructIndexOfArgs, i);
      i++;
      if(i >= args.items.length) {
	writefln("Error: function argument has no type");
	return 1; // error
      }
      auto argType = args.getItem!Type(construct, constructIndexOfArgs, i);
      emitter.output.writef("%s %s", argType.toD, argName.toD);
    }
  }

  emitter.output.writeln(")");
  emitter.output.writeln("{");

  foreach(arg; construct.args[3..$]) {
    auto subConstruct = cast(Construct)arg;
    if(!subConstruct) {
      writefln("Error: the 'function' construct expected a construct parameter but got '%s'", typeid(arg));
      return 1;
    }
    int error = emitter.emit(subConstruct);
    if(error) {
      return error;
    }
  }
  emitter.output.writeln("}");
  return 0;
}

int returnEmitter(Emitter* emitter, const(Construct) construct)
{
  construct.enforceArgCount(1);
  auto code = construct.args[0].toD;
  if(!code) {
    return 1; // fail
  }
  emitter.output.writefln("return %s;", code);
  return 0;
}
int writeEmitter(Emitter* emitter, const(Construct) construct)
{
  string dMethod = "write";
  /*
  string dMethod;
  if(construct.appendNewline) {
    dMethod = "write";
  } else {
    dMethod = "writeln":
  }
  */
  emitter.output.writef("%s(", dMethod);
  foreach(i, arg; construct.args) {
    if(i > 0) {
      emitter.output.write(", ");
    }
    
    auto literal = cast(ConstructString)arg;
    if(!literal) {
      writeln("Error: the 'write' construct only accepts ConstructString arguments");
      return 1;
    }
    emitter.output.write('"', literal.value, '"');
  }
  emitter.output.writeln(");");
  
  return 0;
}
