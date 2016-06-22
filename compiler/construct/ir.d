module construct.ir;

mixin template IRLoaderEntryPoint(string irLoaderFile = __FILE__)
{
  import std.stdio : writeln, writefln, stdout;
  import std.file  : exists, mkdir, baseName, dirName, buildNormalizedPath;
  import std.array : appender;
  import backend : emit;
  
  void usage(string program)
  {
    writefln("Usage: %s", program.baseName);
  }
  int main(string[] args)
  {
    auto code = appender!(Construct[])();

    string program = args[0];
    args = args[1..$];
    
    if(args.length > 0) {
      usage(program);
      writeln();
      writeln("Error: too many command line arguments");
      return 1;
    }

    string outputDir = buildNormalizedPath(irLoaderFile.dirName, "obj");
    if(!exists(outputDir)) {
      mkdir(outputDir);
    }
    
    writefln("[DEBUG] Loading constructs from '%s'...", irLoaderFile);
    stdout.flush();
    loadCode(code);
    writeln("[DEBUG] Done loading constructs. Emitting code...");
    stdout.flush();
    {
      auto result = emit(outputDir, irLoaderFile, code);
      if(result == 0) {
        writeln("[DEBUG] Done emitting code, Success.");
      }
      return result;
    }
  }
}

import std.stdio  : File, Exception;
import std.array  : Appender, appender;
import std.string : format;
import std.format : formattedWrite;
import std.conv   : to;

class InvalidConstructException: Exception
{
  this(string msg)
  {
    super(msg);
  }
}

/*
struct ConstructCode
{
  Appender!(Construct[]) constructs;
  this() {
    constructs = appender!(Construct[])();
  }
  void put(Construct construct)
  {
    constructs.put(construct);
  }
}
*/

class ConstructObject {
  abstract void toString(scope void delegate(const(char)[]) sink) const;
  abstract bool equals(const(ConstructObject) otherObj) const;

  static const(ConstructObject) fromUnquoted(const(char)[] unquoted)
  {
    if(unquoted == "int") {
      return StandardType.int_;
    }
    if(unquoted == "uint") {
      return StandardType.uint_;
    }
    return new StringLiteral(unquoted);
  }
}

class ConstructType {
}

enum StandardTypeEnum {
  int_,
  uint_,
}
class StandardType : ConstructObject {
  StandardTypeEnum type;
  this(StandardTypeEnum type) immutable {
    this.type = type;
  }
  immutable static StandardType int_ =
    new immutable(StandardType)(StandardTypeEnum.int_);
  immutable static StandardType uint_ =
    new immutable(StandardType)(StandardTypeEnum.uint_);
  override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink(to!string(type));
  }
  override bool equals(const(ConstructObject) otherObj) const
  {
    auto other = cast(StandardType)otherObj;
    return other && this.type == other.type;
  }
}


class StringLiteral : ConstructObject
{
  const(char)[] value;
  alias value this;
  this(const(char)[] value) {
    this.value = value;
  }
  override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("\"");
    sink(value);
    sink("\"");
  }
  override bool equals(const(ConstructObject) otherObj) const
  {
    auto other = cast(StringLiteral)otherObj;
    return other && this.value == other.value;
  }
}
class LongLiteral : ConstructObject
{
  long value;
  this(long value) {
    this.value = value;
  }
  override void toString(scope void delegate(const(char)[]) sink) const
  {
    formattedWrite(sink, "%s", value);
  }
  override bool equals(const(ConstructObject) otherObj) const
  {
    auto other = cast(LongLiteral)otherObj;
    return other && this.value == other.value;
  }
}
  
class ParamList : ConstructObject
{
  const(ConstructObject)[] params;
  ConstructObject[string] namedParams;
  this(const(ConstructObject)[] params = null, ConstructObject[string] namedParams = null)
  {
    this.params = params;
    this.namedParams = namedParams;
  }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("(");
    if(params.length > 0) {
      params[0].toString(sink);
      foreach(param; params[1..$]) {
	sink(" ");
	param.toString(sink);
      }
    }
    // TODO: implement printing namedParams
    //foreach(param; params) {
    //}
    //sink(name);
    sink(")");
  }
  final override bool equals(const(ConstructObject) otherObj) const
  {
    auto other = cast(ParamList)otherObj;
    return other && equals(other);
  }
  final bool equals(const(ParamList) other) const
  {
    if(params.length != other.params.length) {
      return false;
    }
    foreach(i; 0..params.length) {
      if(!params[i].equals(other.params[i])) {
	return false;
      }
    }
    return true;
  }
}  
class Construct : ConstructObject
{
  const(char)[] name;
  const(ConstructObject)[] params;
  ConstructObject[string] namedParams;
  const(Construct)[] constructBlock;
  this(const(char)[] name, const(ConstructObject)[] params = null,
       ConstructObject[string] namedParams = null, const(Construct)[] constructBlock = null)
  {
    this.name = name;
    this.params = params;
    this.namedParams = namedParams;
    this.constructBlock = constructBlock;
  }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("Construct '");
    sink(name);
    sink("' ");
    if(params.length > 0) {
      params[0].toString(sink);
      foreach(param; params[1..$]) {
	sink(" ");
	param.toString(sink);
      }
    }
  }
  final override bool equals(const(ConstructObject) otherObj) const
  {
    auto other = cast(Construct)otherObj;
    return other && equals(other);
  }
  final bool equals(const(Construct) other) const
  {
    if(params.length != other.params.length ||
       namedParams.length != other.namedParams.length ||
       constructBlock.length != other.constructBlock.length ||
       name != other.name) {
      return false;
    }
    foreach(i; 0..params.length) {
      if(!params[i].equals(other.params[i])) {
	return false;
      }
    }
    if(namedParams || other.namedParams) {
      throw new Exception("namedParams equality not implemented");
    }
    if(constructBlock || other.constructBlock) {
      foreach(i; 0..constructBlock.length) {
	if(!constructBlock[i].equals(other.constructBlock[i])) {
	  return false;
	}
      }
    }
    return true;
  }
  final void enforceParamCount(size_t count) const
  {
    if(params.length != count) {
      throw new InvalidConstructException
	(format("Construct '%s' requires %s parameters but have %s", name, count, params.length));
    }
  }
  final T getParam(T)(size_t i) const if( is( T : ConstructObject ) )
  {
    if(i >= params.length) {
      throw new InvalidConstructException
	(format("Construct '%s' requires more parameters, expected '%s' at index %s but only have %s parameters", name, T.stringof, i, params.length));
    }
    auto casted = cast(T)params[i];
    if(!casted) {
      throw new InvalidConstructException
	(format("Construct '%s' expects a '%s' parameter at index %s, but got '%s'", name, T.stringof, i, typeid(params[i])));
    }
    return casted;
  }
}
