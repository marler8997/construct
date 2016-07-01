import std.stdio;

//
// Utf8
//
class Utf8Exception : Exception {
  this(string msg) {
    super(msg);
  }
}

enum invalidEndMessage = "input ended with invalid UTF-8 character";

// This method assumes that utf8 points to at least one character
// and that the first non-valid pointer is at the limit pointer
// (this means that utf8 < limit)
dchar decodeUtf8(const(char)** utf8InOut, const char* limit)
{
  auto utf8 = *utf8InOut;
  dchar c = *utf8;
  utf8++;
  if((c & 0x80) == 0) {
    *utf8InOut = utf8;
    return c;
  }

  if((c & 0x20) == 0) {
    if(utf8 >= limit) throw new Utf8Exception(invalidEndMessage);
    utf8++;
    *utf8InOut = utf8;
    return ((c << 6) & 0x7C0) | (*(utf8 - 1) & 0x3F);
  }

  throw new Exception("utf8 not fully implemented");
}

version(unittest_utf8) unittest
{
  mixin(scopedTest!("utf8"));

  void testDecodeUtf8(string s, dchar[] expectedChars...) {
    dchar decoded;
    auto start = s.ptr;
    auto limit = s.ptr + s.length;

    foreach(expected; expectedChars) {
      if(start >= limit) {
	writefln("Expected more decoded utf8 chars but input ended");
	assert(0);
      }
      auto saveStart = start;
      decoded = decodeUtf8(start, limit);
      if(decoded != expected) {
	writefln("Expected '%s' 0x%x but decoded '%s' 0x%x",
		 expected, expected, decoded, decoded);
	assert(0);
      }
      start = saveStart;
      decoded = bjoernDecodeUtf8(start, limit);
      if(decoded != expected) {
	writefln("Expected '%s' 0x%x but decoded '%s' 0x%x",
		 expected, expected, decoded, decoded);
	assert(0);
      }
      debug writefln("decodeUtf8('%s')", decoded);
    }
  }

  testDecodeUtf8("\u0000", 0x0000);
  testDecodeUtf8("\u0001", 0x0001);

  testDecodeUtf8("\u00a9", 0xa9);
  testDecodeUtf8("\u00b1", 0xb1);
  testDecodeUtf8("\u02c2", 0x02c2);
}
