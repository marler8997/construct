module construct.util;

import std.format : formattedWrite;

alias LineNumber = size_t;
alias StringSink = scope void delegate(const(char)[]);
alias PureStringSink = scope void delegate(const(char)[]) pure;

T unconst(T)(const(T) obj)
{
  return cast(T)obj;
}
immutable(T) immutable_(T)(T obj)
{
  return cast(immutable(T))obj;
}

Exception imp(string feature = null, string file = __FILE__, size_t line = __LINE__) pure {
  Exception e;
  if(feature) {
    e = new Exception(feature~": not implemented", file, line);
  } else {
    e = new Exception("not implemented", file, line);
  }
  //throw e;
  return e;
}

T singleton(T, A...)(A args)
{
  static T instance;
  if(!instance) {
    instance = new T(args);
  }
  return instance;
}

// Used to print a string with an 'a <string>' or 'an <string>' prefix depending
// on if it starts with a vowel.
// i.e. "thing"  => "a thing"
//      "object" => "an object"
struct An
{
  const(char)[] name;
  void toString(scope void delegate(const(char)[]) sink) const
  {
    if(name.length > 0) {
      auto c = name[0];
      if(c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u') {
	sink("an ");
      } else {
	sink("a ");
      }
      sink(name);
    }
  }
}
An an(T)(T enumValue) pure if( is( T == enum) )
{
  return An(to!string(enumValue));
}

struct StringBuilder(size_t MaxSize)
{
  char[MaxSize] buffer;
  size_t offset;
  void append(const(char)[] s) pure
  {
    assert(offset + s.length <= MaxSize);
    buffer[offset..offset+s.length] = s;
    offset += s.length;
  }
  string createString()
  {
    return buffer[0..offset].idup;
  }
}
string formattedString(alias formatter, size_t maxLength = 8192, Args...)(Args args)
{
  StringBuilder!maxLength builder;
  formatter(&builder.append, args);
  return builder.createString();
}
void generateItemCode(string formatArgs, T)(PureStringSink sink, T list, string format)
{
  bool atFirst = true;
  foreach(item; list) {
    mixin("formattedWrite(sink, format"~formatArgs~");");
    atFirst = false;
  }
}

inout(T)[] filter(T)(inout(T)[] original, inout(T)[] filterValues...) pure nothrow
{
  size_t length = 0;
  T[] newArray = new T[original.length];
  foreach(item; original) {
    bool remove = false;
    foreach(filterValue; filterValues) {
      if(item == filterValue) {
        remove = true;
        break;
      }
    }
    if(!remove) {
      newArray[length++] = item;
    }
  }
  return cast(inout(T)[])newArray[0..length];
}
unittest
{
  assert(["a","b"].filter("a") == ["b"]);
  assert(["a","b","c"].filter("a", "c") == ["b"]);
}
