import utf8;

version(unittest) {
  import std.stdio;
}

class ParseException : Exception
{
  this(string msg, string file = __FILE__,
       uint line = __LINE__, Throwable next = null) pure nothrow @nogc @safe
  {
    super(msg, file, line, next);
  }
}
/*
class BadCharacterException : ParseException
{
  this(string msg = null, string file = __FILE__,
       uint line = __LINE__, Throwable next = null) pure nothrow @nogc @safe
  {
    super((msg == null) ? "input ended early" : msg, file, line, next);
  }
}
*/
class InputEndedEarlyException : ParseException
{
  this(string msg = null, string file = __FILE__,
       uint line = __LINE__, Throwable next = null) pure nothrow @nogc @safe
  {
    super((msg == null) ? "input ended early" : msg, file, line, next);
  }
}  
class TooMuchInputException : ParseException
{
  this(string msg = null, string file = __FILE__,
       uint line = __LINE__, Throwable next = null) pure nothrow @nogc @safe
  {
    super((msg == null) ? "too much input" : msg, file, line, next);
  }
}  


struct NumberBuilder(T)
{
  T value;
  /*
  static T zero() {
    return 0;
  }
  */
}

template unsignedParser(T,N)
{
  N parse(T t)
  {
    auto c = t.consumeRequiredChar();
    if(c == '0') {
      t.expectEnd();
      return 0;
    }
    if(c < '1' || c > '9') {
      
    }
    
    NumberBuilder!N number;
    return number.value;
  }
}


struct LimitPointerParser
{
  const(char)* next;
  const(char)* limit;
  this(const(char)[] str)
  {
    this.next = str.ptr;
    this.limit = str.ptr + str.length;
  }
  void expectEnd()
  {
    if(next < limit) {
      throw new TooMuchInputException();
    }
  }
  dchar consumeRequiredChar()
  {
    if(next >= limit) {
      throw new InputEndedEarlyException();
    }
    return decodeUtf8(&next, limit);
  }
  
}

unittest
{
  {
    alias parser = unsignedParser!(LimitPointerParser,size_t).parse;
    void expectFail(E)(string input)
    {
      try {
	auto value = parser(LimitPointerParser(input));
	assert(0, "expected exception but did not get one");
      } catch(E e) {
	writefln("Got expected error: %s", e.msg);
      }
    }

    expectFail!InputEndedEarlyException("");

    assert(0 == parser(LimitPointerParser("0")));
    assert(1 == parser(LimitPointerParser("1")));

    
  }
}
