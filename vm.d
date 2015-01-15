import std.stdio : write, writef, writeln, writefln;
import std.string : format;
import std.traits : isIntegral, isSigned;
import std.typetuple : TypeTuple;
import std.conv : to;

version(BigEndian) {
  pragma(msg, "Current system is BigEndian");
} else version(LittleEndian) {
  pragma(msg, "Current system is LittleEndian");
} else static assert(0, NoEndianMessage);

enum MAX_INSTRUCTION_LENGTH = 64;

//
// TODO: see if there's a way to setup the VM instructions and have the instruction set automatically generated
// How: Define every instruction in terms of it's arguments and variants, then count how many variations there are to determine how
//      many bits of information are necessary to encode it.
//
/+
The assembly language should be high level.  Why?  This is so that information is not lost.
The assembly will aim to contain all the pertinent information necessary to re-compile the assembly to other assembly languages.
High level constructs will only be added when the construct contains more information then the low level construct and will
help optimize the assembly when it is recompiled to other assembly languages.
I need to study assembly languages to find a very nice theoretical one.

I'm going to make different kinds of assembly targets:

   1. Target:
      - Compile for this machine only (programs that are compiled on the target computer)
      - Compile for the general VM ()
   This language can use a different paradigm.  Every machine is required to have a compiler to use the language and the compiler makes as many optimizations as it can for that particular machine.

+/

//
// Program File
// ushort versionMajor
// ushort versionMinor
// size_t recommendedStackLength
// size_t sectionHeaderLength
// {
//   byte sectionBypte (SectionType enum)
//   size_t offset
//   size_t length
// }
//
// Decision: should the sections be fixed (code/data/exports) or variable (code/code/data/code/exports/data/code...)
//
enum SectionType : byte
{
  code, data, /*exports*/
}
struct Section
{
  SectionType type;
  size_t offset;
  size_t length;
}
struct ProgramHeader
{
  ushort versionMajor;
  ushort versionMinor;
  size_t recommendedStackLength;
  Section[] sections;
}

/+
Instruction Set

Decision: how many registers? infinite?

Decision: instruction encoding?
    Note: Having a variable size instruction allows extensions without changing the encoding

Standard: Function argument name should describe what the argument represents, not what type it is

Note: This instruction set assumes that a word/address/register are all the same size.

 Name   | Size Group    | Assignable | Purpose
---------------------------------------
reg     | registerIndex | Yes        | A register
reg^    | registerIndex | Yes        | A register pointing to a memory address
word    | word          | No         | A value
^word   | word          | Yes        | A pointer to a value
^^word  | word          | Yes        | An address to memory that has an address to memory

Note: The type name can be read from left to right:
 reg: a register
 ^word: a pointer to a word
 ^^word: a pointer to a pointer to a word
 reg^: a register pointing to a word

TypeGroup
----------------------------------------
4_common_assignables | reg, ^word, reg^, ^word

Note: 4_common_assignables could include more like reg^^ or ^^^word, however, it is designed to have the 4 most likely 4_common_assignables types so that the argument type can be represented by 2 bits.

Note: even if the function signature says a certain type, it does not mean that the final instruction will encode the entire type.  For example, if you have Copy(register0, 0), then the final instruction would likely not include an entire word to represent the value of 0.

 VM constructs as functions
 Signature                          | Description
---------------------------------------------
Copy(reg dest, reg src)             | reg[dest] = reg[src]
Copy(reg dest, reg^ src)            | reg[dest] = *(reg[src])
Copy(reg dest, word value)          | reg[dest] = value
Copy(reg dest, ^word src)           | reg[dest] = *src
Copy(reg dest, ^^word src)          | reg[dest] = **src

Copy(^word dest, reg src)           | *dest = reg[src]
Copy(^word dest, reg^ src)          | *dest = *(reg[src])
Copy(^word dest, word value)        | *dest = value
Copy(^word dest, ^word src)         | *dest = *src
Copy(^word dest, ^^word src)        | *dest = **src

Copy(reg^ dest, reg src)            | *(reg[dest]) = reg[src]
Copy(reg^ dest, reg^ src)           | *(reg[dest]) = *(reg[src])
Copy(reg^ dest, word value)         | *(reg[dest]) = value
Copy(reg^ dest, ^word src)          | *(reg[dest]) = *src
Copy(reg^ dest, ^^word src)         | *(reg[dest]) = **src

Copy(^^word dest, reg src)          | **dest = reg[src]
Copy(^^word dest, reg^ src)         | **dest = *(reg[src])
Copy(^^word dest, word value)       | **dest = value
Copy(^^word dest, ^word src)        | **dest = *src
Copy(^^word dest, ^^word src)       | **dest = **src
+/
/+

Encoding: 2 instructions for Copy, one for all the signatures without a word, and the other for the signatures with a word.  

Copy(4_common_assignables dest, 4_common_assignables src)
    needs 7 bits: 3 bits for byte width, 2 bits for dest type, 2 bits for src type
Copy(4_common_assignables dest, word value)
    needs 5 bits: 3 bits for byte width, 2 bits for dest type
+/

/++
 Returns: The decoded width. If the encoded value is e, then the decoded value
          is 2^e.
Encoded       Decoded     (bits)
----------------------------------------
0  0b000        1 byte       8 bits
1  0b001        2 bytes     16 bits
2  0b010        4 bytes     32 bits
3  0b011        8 bytes     64 bits
4  0b100       16 bytes    128 bits
5  0b101       32 bytes    256 bits
6  0b110       64 bytes    512 bits
7  0b111      128 bytes   1024 bits
+/
static immutable ubyte[] decodeWidthMap =
  [1,
   2,
   4,
   8,
   16,
   32,
   64,
   128];
ubyte decodeWidth(ubyte encodedWidth) in { assert(encodedWidth <= 7); } body
{
  return decodeWidthMap[encodedWidth];
}
ubyte encodeWidth(ubyte encodedWidth) {
  switch(encodedWidth) {
  case   1: return 0;
  case   2: return 1;
  case   4: return 2;
  case   8: return 3;
  case  16: return 4;
  case  32: return 5;
  case  64: return 6;
  case 128: return 7;
  default:
    throw new Exception(format("invalid width %d, expected 1, 2, 4, 8, 16, 32, 64 or 128", encodedWidth));
  }
}
unittest
{
  foreach(decoded; decodeWidthMap) {
    assert(decoded == decodeWidth(encodeWidth(decoded)));
  }
}



enum EncodedRegister : ushort
{
  ip      = 0x0000, // instruction pointer
  sp      = 0x0001, // stack pointer
  gpStart = 0x0002, // start of general purpose registers
}
enum EncodedInstruction : ubyte
{
  halt = 0,
  copyFromAssignable = 1,
  copyFromImmediate  = 2,
}
enum AssignableType : ubyte
{
  reg    = 0,
  pword  = 1,
  regp   = 2,
  ppword = 3
}
immutable assignableTypeStringMap = ["reg", "^word", "reg^", "^^word"];
string toString(AssignableType t)
{
  return assignableTypeStringMap[t];
}

struct reg
{
  static ip = reg(EncodedRegister.ip);
  static sp = reg(EncodedRegister.sp);
  static reg gp(ushort index) {
    return reg(cast(ushort)(EncodedRegister.gpStart + index));
  }

  ushort index;
}
struct regp
{
  static ip = regp(EncodedRegister.ip);
  static sp = regp(EncodedRegister.sp);
  static regp gp(ushort index) {
    return regp(cast(ushort)(EncodedRegister.gpStart + index));
  }

  ushort index;
}

//
// A word is the number of bits to store an address to memory
// A word is also the size of the registers
//
template vm(alias wordSize) {

static if(wordSize == 1) {
  alias uword = ubyte;
  alias sword = byte;
  enum encodedWordWidth = 0;
  // Loop through the widths using the encodedWidth
  //immutable ubyte[] typedWidths = [1];
  void setFromEncodedWidth(void* address, ubyte encodedWidth, uword value) in { assert(encodedWidth <= encodedWordWidth); } body
  {
    *cast(ubyte*)address = cast(ubyte)value;
  }
  uword dereference(void* address, ubyte encodedWidth) in { assert(encodedWidth <= encodedWordWidth); } body
  {
    return *cast(ubyte*)address;
  }
  uword maskFromEncodedWidth(ubyte encodedWidth) in { assert(encodedWidth <= encodedWordWidth); } body
  {
    return 0xFF;
  }
  sword negativeMask(ubyte byteLength) in { assert(byteLength > 0 && byteLength < wordSize); } body
  {
    assert(0, "byte words cannot have negative masks");
  }
} else static if(wordSize == 2) {
  alias uword = ushort;
  alias sword = short;
  enum encodedWordWidth = 1;
  // Loop through the widths using the encodedWidth
  //immutable ubyte[] typedWidths = [1, 2];
  void setFromEncodedWidth(void* address, ubyte encodedWidth, uword value) in { assert(encodedWidth <= encodedWordWidth); } body
  {
    final switch(encodedWidth) {
    case 0: *cast(ubyte*) address = cast(ubyte) value; return;
    case 1: *cast(ushort*)address = cast(ushort)value; return;
    }
  }
  uword dereference(void* address, ubyte encodedWidth) in { assert(encodedWidth <= encodedWordWidth); } body
  {
    final switch(encodedWidth) {
    case 0: return *cast(ubyte*)address;
    case 1: return *cast(ushort*)address;
    }
  }
  uword maskFromEncodedWidth(ubyte encodedWidth) in { assert(encodedWidth <= encodedWordWidth); } body
  {
    final switch(encodedWidth) {
    case 0: return 0x00FF;
    case 1: return 0xFFFF;
    }
  }
  sword negativeMask(ubyte byteLength) in { assert(byteLength > 0 && byteLength < wordSize); } body
  {
    return cast(sword)0xFF00;
  }
} else static if(wordSize == 4) {
  alias uword = uint;
  alias sword = int;
  enum encodedWordWidth = 2;
  // Loop through the widths using the encodedWidth
  //immutable ubyte[] typedWidths = [1, 2, 4];
  void setFromEncodedWidth(void* address, ubyte encodedWidth, uword value) in { assert(encodedWidth <= encodedWordWidth); } body
  {
    final switch(encodedWidth) {
    case 0: *cast(ubyte*) address = cast(ubyte) value; return;
    case 1: *cast(ushort*)address = cast(ushort)value; return;
    case 2: *cast(uint*)  address = cast(uint)  value; return;
    }
  }
  uword dereference(void* address, ubyte encodedWidth) in { assert(encodedWidth <= encodedWordWidth); } body
  {
    final switch(encodedWidth) {
    case 0: return *cast(ubyte*)address;
    case 1: return *cast(ushort*)address;
    case 2: return *cast(uint*)address;
    }
  }
  uword maskFromEncodedWidth(ubyte encodedWidth) in { assert(encodedWidth <= encodedWordWidth); } body
  {
    final switch(encodedWidth) {
    case 0: return 0x000000FF;
    case 1: return 0x0000FFFF;
    case 2: return 0xFFFFFFFF;
    }
  }
  sword negativeMask(ubyte byteLength) in { assert(byteLength > 0 && byteLength < wordSize); } body
  {
    final switch(byteLength) {
    case 1:return cast(sword)0xFF000000;
    case 2:return cast(sword)0xFFFF0000;
    case 3:return cast(sword)0xFFFFFF00;
    }
  }
} else static if(wordSize == 8) {
  alias uword = ulong;
  alias sword = long;
  enum encodedWordWidth = 3;
  // Loop through the widths using the encodedWidth
  //immutable ubyte[] typedWidths = [1, 2, 4, 8]; 
  void setFromEncodedWidth(void* address, ubyte encodedWidth, uword value) in { assert(encodedWidth <= encodedWordWidth); } body
  {
    final switch(encodedWidth) {
    case 0: *cast(ubyte*) address = cast(ubyte) value; return;
    case 1: *cast(ushort*)address = cast(ushort)value; return;
    case 2: *cast(uint*)  address = cast(uint)  value; return;
    case 3: *cast(ulong*) address = cast(ulong) value; return;
    }
  }
  uword dereference(void* address, ubyte encodedWidth) in { assert(encodedWidth <= encodedWordWidth); } body
  {
    final switch(encodedWidth) {
    case 0: return *cast(ubyte*)address;
    case 1: return *cast(ushort*)address;
    case 2: return *cast(uint*)address;
    case 3: return *cast(ulong*)address;
    }
  }
  uword maskFromEncodedWidth(ubyte encodedWidth) in { assert(encodedWidth <= encodedWordWidth); } body
  {
    final switch(encodedWidth) {
    case 0: return 0x00000000000000FF;
    case 1: return 0x000000000000FFFF;
    case 2: return 0x00000000FFFFFFFF;
    case 3: return 0xFFFFFFFFFFFFFFFF;
    }
  }
  sword negativeMask(ubyte byteLength) in { assert(byteLength > 0 && byteLength < wordSize); } body
  {
    final switch(byteLength) {
    case 1:return cast(sword)0xFF00000000000000;
    case 2:return cast(sword)0xFFFF000000000000;
    case 3:return cast(sword)0xFFFFFF0000000000;
    case 4:return cast(sword)0xFFFFFFFF00000000;
    case 5:return cast(sword)0xFFFFFFFFFF000000;
    case 6:return cast(sword)0xFFFFFFFFFFFF0000;
    case 7:return cast(sword)0xFFFFFFFFFFFFFF00;
    }
  }
} else static assert(0, format("currently no uword/sword type could be determined for wordSize %s", wordSize));

pragma(msg, format("uword=%s, sword=%s", uword.stringof, sword.stringof));

alias word = uword;
enum isWord(T) = is ( T == uword ) || is ( T == sword );
unittest
{
  assert(isWord!uword);
  assert(isWord!sword);
}

struct pword
{
  uword addr;
}
struct ppword
{
  uword addr;
}

version(unittest)
{
  alias intTypes = TypeTuple!(uword,sword,ubyte,byte,ushort,short,uint,int,ulong,long,size_t);
}

/+
// Decodes in BigEndian order
word decodeWord(ubyte* code)
{
  version(BigEndian) {
    return *cast(word*)code;
  } else version(LittleEndian) {
    word value = 0;
    ubyte shift = (word.sizeof - 1) * 8;
    while(true) {
      value |= (cast(word)*code) << shift;
      if(shift == 0) break;
      shift -= 8;
      code++;
    }
    return value;
  } else static assert(0, NoEndianMessage);
}
// Emits in BigEndian order
void emitWord(ubyte* code, word value)
{
  version(BigEndian) {
    *cast(word*)code = value;
  } else version(LittleEndian) {
    ubyte shift = (word.sizeof - 1) * 8;
    while(true) {
      *code = cast(ubyte)(value >> shift);
      if(shift == 0) break;
      shift -= 8;
      code++;
    }
  } else static assert(0, NoEndianMessage);
}
unittest
{
  ubyte[word.sizeof] buffer;

  auto testValues = [word.min, word.max, 0, 1, word.max / 2, 10, 64, 512];
  foreach(testValue; testValues) {
    emitWord(buffer.ptr, testValue);
    assert(testValue == decodeWord(buffer.ptr));
  }
}
+/

bool packedIsUnsigned(ubyte packedInfo) {
  return (packedInfo & 0x80) == 0;
}
enum wordPackedLengthMask = 0x7F;
ubyte wordPackedLength(ubyte packedInfo) {
  return packedInfo & wordPackedLengthMask;
}


alias decodePackedUword = decodePackedAddress;
// Packed word/sword:
//     first byte: sryy_yxxx (s = signed, 2^(yy_y) = dataLength, 2^(xxx) = encodeLength)
// Note: This language encodes values in BigEndian order
// Decodes in BigEndian order
uword decodePackedAddress(ubyte* code)
{
  auto byteLength = *code;
  if(byteLength == 0) return 0;

  if(byteLength > uword.sizeof) {
    throw new Exception(format("Encountered invalid length %s for word(%s)", byteLength, uword.stringof));
  }

  uword value = 0;
  auto shift = (byteLength-1)*8;
  while(true) {
    code++;
    value |= cast(uword)*code << shift;
    if(shift == 0) return value;
    shift -= 8;
  }
}
sword decodePackedSword(ubyte* code)
{
  return cast(sword)decodePackedWord(code);
}
uword decodePackedWord(ubyte* code)
{
  //writefln("  decodePackedWord (first byte is 0x%x)", *code);
  auto byteLength = *code & wordPackedLengthMask;
  if(byteLength == 0) return 0;
  
  if(byteLength > sword.sizeof) {
    throw new Exception(format("Encountered invalid length %s for sword(%s)", byteLength, sword.stringof));
  }

  code++;
  sword value = 0;

  if((*code & 0x80) == 0x80) {
    if(byteLength < sword.sizeof) {
      value |= negativeMask(cast(ubyte)(sword.sizeof - byteLength));
      //writefln("    value 0x%x (after mask)", value);
    }
  }

  auto shift = (byteLength-1)*8;
  while(true) {
    //writefln("    value 0x%x", value);
    value |= cast(sword)*code << shift;
    if(shift == 0) return value;
    code++;
    shift -= 8;
  }
}

ubyte* emitPackedWord(T)(ubyte* code, T value) if(isWord!T)
{
  static if(isSigned!T) {
    return emitPackedSword(code, value);
  } else {
    return emitPackedUword(code, value);
  }
}
alias emitPackedAddress = emitPackedUword;
// Emits in BigEndian order
ubyte* emitPackedUword(ubyte* code, uword value)
{
  if(value == 0) {
    *code = 0;
    return code + 1;
  }

  ubyte byteLength = 1;
  for(uword i = value >> 8; i != 0; i >>= 8) {
    byteLength++;
  }

  *code = byteLength;
  code++;
  auto shift = (byteLength-1) * 8;
  while(true) {
    *code = cast(ubyte)(value >> shift);
    code++;
    if(shift == 0) break;
    shift -= 8;
  }
  return code;
}
// Emits in BigEndian order
ubyte* emitPackedSword(ubyte* code, sword value)
{
  if(value == 0) {
    *code = 0x80;
    return code + 1;
  }

  if(value > 0) {
    ubyte byteLength = 1;
    for(sword i = value; i > 0x7F; i>>=8) {
      byteLength++;
    }

    *code = 0x80 | byteLength;
    code++;
    auto shift = (byteLength-1) * 8;
    while(true) {
      *code = cast(ubyte)(value >> shift);
      code++;
      if(shift == 0) break;
      shift -= 8;
    }
    return code;
  } else {
    ubyte byteLength = 1;
    for(sword i = value; i < -0x80; i>>=8) {
      byteLength++;
    }

    *code = 0x80 | byteLength;
    code++;
    auto shift = (byteLength-1) * 8;
    while(true) {
      *code = cast(ubyte)(value >> shift);
      code++;
      if(shift == 0) break;
      shift -= 8;
    }
    return code;
  }
}

string packedString(ubyte* p) {
  auto length = p[0] & wordPackedLengthMask;
  if(length == 0) return "0 bytes";
  string s;
  if((p[0] & 0x80) == 0x80) {
    s = format("(signed,%s) %s bytes 0x", ((p[1] & 0x80) == 0x80) ? "negative" : "positive", length);
  } else {
    s = format("(unsigned) %s bytes 0x", length);
  }
		  
  foreach(b; p[1..1+length]) {
    s ~= format("%02x", b);
  }
  return s;
}
unittest
{
  ubyte[wordSize+1] buffer;

  void testUword(T)(T value)
  {
    //writefln("Testing word %s %s (0x%x)", T.stringof, value, value);
    auto byteLength = emitPackedUword(buffer.ptr, value) - buffer.ptr;
    assert(value == decodePackedAddress(buffer.ptr));
  }
  void testSword(T)(T value)
  {
    //writefln("Testing sword %s %s (0x%x)", T.stringof, value, value);
    auto byteLength = emitPackedSword(buffer.ptr, value) - buffer.ptr;
    //writefln("    buffer %s", packedString(buffer.ptr));
    assert(value == decodePackedSword(buffer.ptr), format("expected %s got %s", value, decodePackedWord(buffer.ptr)));
  }
  {
    sword max;
    static if(sword.max - 1 > 5000) {
      max = 5000;
    } else {
      max = sword.max - 1;
    }
    sword min;
    static if(sword.min + 1 < -5000) {
      min = 5000;
    } else {
      min = sword.min + 1;
    }
    foreach(sword i; min..max+1) {
      testSword(i);
    }
  }
  foreach(T; intTypes) {
    static if(isSigned!T) {
      static if(T.sizeof <= sword.sizeof) {
	testSword(cast(T)0);
	testSword(cast(T)-1);
	testSword(T.min);
	testSword(cast(sword)(T.min - 1));
	testSword(cast(sword)(T.min + 1));
	testSword(cast(sword)(T.min/2));
	testSword(T.max);
	testSword(cast(sword)(T.max - 1));
	testSword(cast(sword)(T.max + 1));
	testSword(cast(sword)(T.max/2));
      }
    } else {
      static if(T.sizeof <= uword.sizeof) {
	testUword(T.min);
	testUword(cast(uword)(T.min/2));
	testUword(T.max);
	testUword(cast(uword)(T.max/2));
      }
    }
  }

  testUword(cast(uword)1);
  testUword(cast(uword)2);
  testUword(cast(uword)3);
  testUword(cast(uword)10);
  testUword(cast(uword)255);
  testUword(cast(uword)256);
  testUword(cast(uword)512);

  testSword(cast(sword)1);
  testSword(cast(sword)2);
  testSword(cast(sword)3);
  testSword(cast(sword)10);
  testSword(cast(sword)255);
  testSword(cast(sword)256);
  testSword(cast(sword)512);

  testSword(cast(sword)-1);
  testSword(cast(sword)-2);
  testSword(cast(sword)-10);
  testSword(cast(sword)-127);
  testSword(cast(sword)-128);
  testSword(cast(sword)-129);
  testSword(cast(sword)-255);
}

//
// Emit Functions
//

/** Emits the recommended code padding (padded by zeros) */
ubyte* emitCodePadding(ubyte* code)
{
  code[0..MAX_INSTRUCTION_LENGTH] = 0;
  return code + MAX_INSTRUCTION_LENGTH;
}
ubyte* emitHalt(ubyte* code)
{
  *code = 0;
  return code + 1;
}
ubyte* emitCopy(ubyte* code, reg dest, reg src, ubyte byteWidth)
{
  *(code    ) = 0b0000_0001;
  *(code + 1) = cast(ubyte)(0x80 | encodeWidth(byteWidth) << 4 | (AssignableType.reg << 2) | AssignableType.reg);
  *(code + 2) =            dest.index >> 8;
  *(code + 3) = cast(ubyte)dest.index     ;
  *(code + 4) =            src.index >> 8;
  *(code + 5) = cast(ubyte) src.index     ;
  return code + 6;
}
ubyte* emitCopy(ubyte* code, reg dest, pword src, ubyte byteWidth)
{
  *(code    ) = 0b0000_0001;
  *(code + 1) = cast(ubyte)(0x80 | encodeWidth(byteWidth) << 4 | (AssignableType.reg << 2) | AssignableType.pword);
  *(code + 2) =            dest.index >> 8;
  *(code + 3) = cast(ubyte)dest.index     ;
  return emitPackedUword(code + 4, src.addr);
}

ubyte* emitCopy(T)(ubyte* code, reg dest, T src, ubyte byteWidth) if(isWord!T)
{
  *(code    ) = 0b0000_0001;
  *(code + 1) = cast(ubyte)(encodeWidth(byteWidth) << 4 | (AssignableType.reg << 2));
  *(code + 2) =            dest.index >> 8;
  *(code + 3) = cast(ubyte)dest.index     ;
  return emitPackedWord(code + 4, src);
}
ubyte* emitCopy(T)(ubyte* code, pword dest, T src, ubyte byteWidth) if(isWord!T)
{
  *(code    ) = 0b0000_0001;
  *(code + 1) = cast(ubyte)(encodeWidth(byteWidth) << 4 | (AssignableType.pword << 2));
  code = emitPackedAddress(code + 2, dest.addr);
  return emitPackedWord(code, src);
}


interface IRegisterManager
{
  word[] createRegisters();
  word[] requestMoreRegisters(size_t newCount);
}
enum NoEndianMessage = "Neither BigEndian and LittleEndian are defined";
struct NativeMemory
{
  ubyte[] memory;
  ubyte* stackBase;
  this(ubyte[] memory)
  {
    this.memory = memory;
    this.stackBase = memory.ptr;
  }
/+
  void store(T)(uword address, T value)
  {
    *cast(T*)(memory.ptr + address) = value;
  }
  T load(T)(uword address)
  {
    return *cast(T*)(memory.ptr + address);
  }
+/
  void store(const uword address, const ubyte encodedWidth, const uword value)
  {
    setFromEncodedWidth(memory.ptr + address, encodedWidth, value);
  }
  uword load(const uword address, const ubyte encodedWidth)
  {
    return dereference(memory.ptr + address, encodedWidth);
  }
}
unittest
{
  NativeMemory memory = NativeMemory(new ubyte[1024]);

  /+
  foreach(i; 0..16) {
    memory.memory[i] = 0x56;
    assert(0x56 == memory.loadByte(i));
  }
  foreach(i; 0..16) {
    version(BigEndian) {
      memory.memory[i  ] = 0x12;
      memory.memory[i+1] = 0x34;
    } else version(LittleEndian) {
      memory.memory[i  ] = 0x34;
      memory.memory[i+1] = 0x12;
    } else static assert(0, NoEndianMessage);
    assert(0x1234 == memory.loadShort(i));
  }
  foreach(i; 0..16) {
    version(BigEndian) {
      memory.memory[i  ] = 0x12;
      memory.memory[i+1] = 0x34;
      memory.memory[i+2] = 0x56;
      memory.memory[i+3] = 0x78;
    } else version(LittleEndian) {
      memory.memory[i  ] = 0x78;
      memory.memory[i+1] = 0x56;
      memory.memory[i+2] = 0x34;
      memory.memory[i+3] = 0x12;
    } else static assert(0, NoEndianMessage);
    assert(0x12345678 == memory.loadInt(i));
  }
  foreach(i; 0..16) {
    version(BigEndian) {
      memory.memory[i  ] = 0x12;
      memory.memory[i+1] = 0x34;
      memory.memory[i+2] = 0x56;
      memory.memory[i+3] = 0x78;
      memory.memory[i+4] = 0x9A;
      memory.memory[i+5] = 0xBC;
      memory.memory[i+6] = 0xDE;
      memory.memory[i+7] = 0xF0;
    } else version(LittleEndian) {
      memory.memory[i  ] = 0xF0;
      memory.memory[i+1] = 0xDE;
      memory.memory[i+2] = 0xBC;
      memory.memory[i+3] = 0x9A;
      memory.memory[i+4] = 0x78;
      memory.memory[i+5] = 0x56;
      memory.memory[i+6] = 0x34;
      memory.memory[i+7] = 0x12;
    } else static assert(0, NoEndianMessage);
    assert(0x123456789ABCDEF0 == memory.loadLong(i));
  }
  +/

/+
  foreach(T; intTypes) {
    T expected = 0;
    foreach(i; 0..T.sizeof) {
      expected = cast(T)(expected << 8);
      expected |= cast(T)(i+1);
    }
    foreach(uword i; 0..16) {
      memory.store(i, T.min);
      assert(T.min == memory.load!T(i));

      memory.store(i, T.max);
      assert(T.max == memory.load!T(i));

      memory.store(i, cast(T)0);
      assert(0 == memory.load!T(i), format("%s (i=%s) expected 0 actual %s", T.stringof, i, memory.load!T(i)));

      memory.store(i, T.init);
      assert(T.init == memory.load!T(i), format("%s (i=%s) expected 0 actual %s", T.stringof, i, memory.load!T(i)));

      version(BigEndian) {
	foreach(ubyte j; 0..T.sizeof) {
	  memory.memory[cast(size_t)(i+j)] = cast(ubyte)(j+1);
	}
      } else version(LittleEndian) {
	foreach(ubyte j; 0..T.sizeof) {
	  memory.memory[cast(size_t)(i+T.sizeof - j - 1)] =
	    cast(ubyte)(j+1);
	}
      } else static assert(0, NoEndianMessage);

      assert(expected == memory.load!T(i));
    }
  }
+/
}
interface IErrorHandler
{
  Exception ipOutOfRange(ubyte* codeBase, ubyte* codeLimit, ubyte* ip);
  Exception notEnoughRegisters(size_t neededRegisterCount);
  Exception codeEndedInsideInstruction(ubyte* ip, ubyte* codeLimit, size_t length);
}


enum VmType { processor, disassembler }

int runVirtualMachine(alias type = VmType.processor,M)(IRegisterManager registerManager, M memory,
		      IErrorHandler errorHandler, ubyte[] code)
{
  assert(code.length > MAX_INSTRUCTION_LENGTH, format("The code isn't padded with %s 0s", MAX_INSTRUCTION_LENGTH));
  for(auto i = 1; i <= MAX_INSTRUCTION_LENGTH; i++) {
    assert(code[$-i] == 0, format("The code isn't padded with %s 0s", MAX_INSTRUCTION_LENGTH));
  }

  ubyte b;

  auto registers = registerManager.createRegisters();

  //auto sp = memory.stackBase; // stack pointer

  auto codeBase = code.ptr;
  auto codeLimit = codeBase + code.length - MAX_INSTRUCTION_LENGTH;
  auto nativeIP = codeBase; // instruction pointer. the nativeIP is a native pointer
                            // to the native memory containing the code
                            // The virtualIP is the (nativeIP - codeBase).  Instructions
                            // will be operating on the virtualIP but in reality will be
                            // affecting the nativeIP

  void printRegisters()
  {/+
    writefln("ip      : 0x%0"~to!string(wordSize*2)~"x (%s)", cast(uword)(nativeIP-codeBase), cast(uword)(nativeIP-codeBase));
    //writefln("sp      : 0x%0"~to!string(wordSize*2)~"x (%s)", sp, cast(word)sp);
    foreach(i, register; registers) {
      writefln("gp(%3s) : 0x%0"~to!string(wordSize*2)~"x (%s)", i, cast(uword)register, cast(uword)register);
    }+/
  }
  void debugInstruction(string instruction)
  {
    debug writeln("INSTRUCTION: ", instruction);
  }
  void checkRegister(size_t index)
  {
    if(index >= registers.length) {
      registers = registerManager.requestMoreRegisters(index + 1);
      if(index >= registers.length) {
	throw errorHandler.notEnoughRegisters(index + 1);
      }
    }
  }
/+
  void enforceCodeLongEnoughForInstruction(size_t instructionLength)
  {
    if(ip + instructionLength > codeLimit) {
      throw errorHandler.codeEndedInsideInstruction(ip, codeLimit, instructionLength);
    }
    writefln("[DEBUG] instruction bytes %s", ip[0..instructionLength]);
  }
+/
/+
  // Note: source should return size_t type
  void setRegisterValue(string source)(size_t dest, ubyte byteWidth)
  {
    switch(dest) {
    case EncodedRegister.ip:
      mixin("ip = codeBase + ("~source~");");
      debug writefln("[DEBUG] set ip to %s", ip);
      return;
    case EncodedRegister.sp:
      mixin("sp = cast(ubyte*)("~source~");");
      debug writefln("[DEBUG] set sp to %s", sp);
      return;
    default: // general purpose registers
      dest -= EncodedRegister.gpStart;
      checkRegister(dest);
      mixin("registers[dest] = ("~source~");");
      debug writefln("[DEBUG] set gp(%s) to %s", dest, registers[dest]);
      return;
    }
  }
+/
  static if(type == VmType.disassembler) {
    string registerName(ushort idx)
    {
      switch(idx) {
      case EncodedRegister.ip: return "ip";
      case EncodedRegister.sp: return "sp";
      default:
	idx -= EncodedRegister.gpStart;
	return "gp("~to!string(idx)~")";
      }
    }
  }
  void setRegister(ushort dest, uword raw, uword mask)
  {
    switch(dest) {
    case EncodedRegister.ip:
      uword virtualIP = cast(uword)(nativeIP - codeBase);
      virtualIP = (virtualIP & (~mask)) | (raw & mask);
      debug writefln("[DEBUG] set ip to %s", virtualIP);
      nativeIP = codeBase + virtualIP;
      return;
    case EncodedRegister.sp:
      assert(0, "setting sp register not implemented");
      //sp = cast(ubyte*)raw;
      //debug writefln("[DEBUG] set sp to %s", sp);
      //return;
    default: // general purpose registers
      dest -= EncodedRegister.gpStart;
      checkRegister(dest);
      registers[dest] = (registers[dest] & (~mask)) | (raw & mask);
      debug writefln("[DEBUG] set gp(%s) to 0x%x (%s)", dest, registers[dest], registers[dest]);
      return;
    }
  }

  uword getRegisterValue(size_t index) {
    switch(index) {
    case EncodedRegister.ip: return cast(uword)(nativeIP - codeBase);
    case EncodedRegister.sp: assert(0, "sp register not implemented"); //return cast(uword)sp;
    default:
      index -= EncodedRegister.gpStart;
      checkRegister(index);
      return registers[index];
    }
  }
  ubyte readIP(size_t offset = 0)()
  {
    return *(nativeIP+offset);
  }
  ubyte rtReadIP(size_t offset = 0) // rt = runtime
  {
    return *(nativeIP+offset);
  }
  ushort registerIndexFromIP(ptrdiff_t offset)()
  {
    return *(nativeIP+offset) << 8 | *(nativeIP+offset+1);
  }
  ushort rtRegisterIndexFromIP(ptrdiff_t offset) // rt = runtime
  {
    return *(nativeIP+offset) << 8 | *(nativeIP+offset+1);
  }


 VM_LOOP:
  while(true) {
    
    if(nativeIP < codeBase || nativeIP >= codeLimit) {
      throw errorHandler.ipOutOfRange(codeBase, codeLimit, nativeIP);
    }
    
    b = readIP();
    printRegisters();
    debug writefln("[DEBUG] next instruction at %s is %02x %02x %02x %02x %02x %02x...",
	     cast(uword)(nativeIP - codeBase), readIP!0, readIP!1, readIP!2, readIP!3, readIP!4, readIP!5);

    debug assert(((b>>4) & 0xF0) == 0); // Check that the shift shifted in 0's only
    final switch(b>>4) {

    case 0b0000: // 0b0000_xxxx

      final switch(b & 0x0F) {
      case 0b0000: // 0b0000_0000
	debugInstruction("HALT");

	static if(type == VmType.disassembler) {
	  writeln("halt");
	}

	break VM_LOOP;

      case 0b0001: // 0b0000_0001 (COPY)
	{
	  nativeIP++;

	  b = readIP();
	  ubyte encodedByteWidth = (b >> 4) & 0x07;
	  auto destType  = cast(AssignableType)((b >> 2) & 0x03);

	  if(b & 0x80) { // COPY from assignable type

	    auto srcType = cast(AssignableType)( b & 0x03);
	    final switch(destType) {
	    case AssignableType.reg:
	  
	      final switch(srcType) {
	      case AssignableType.reg:
	    
		debugInstruction("COPY reg reg");
		nativeIP += 5; // set IP before executing instruction

		static if(type == VmType.processor) {
		  setRegister(registerIndexFromIP!(-4), getRegisterValue(registerIndexFromIP!(-2)),
			      maskFromEncodedWidth(encodedByteWidth));
		} else static if(type == VmType.disassembler) {
		  writefln("copy %s %s", registerName(registerIndexFromIP!(-4)),
			   registerName(registerIndexFromIP!(-2)));
		}
	    
		break;
	      case AssignableType.pword:

		debugInstruction("COPY reg ^word");
		auto dstReg = registerIndexFromIP!1;
		nativeIP += 3;
		auto addr = decodePackedAddress(nativeIP);
		nativeIP += 1 + wordPackedLength(*nativeIP);

		static if(type == VmType.processor) {
		  setRegister(dstReg, memory.load(addr, encodedByteWidth), maskFromEncodedWidth(encodedByteWidth));
		} else static if(type == VmType.disassembler) {
		  writefln("copy %s ^(0x%x)", registerName(dstReg), addr);
		}

		break;
	      case AssignableType.regp:
		assert(0, "implement");
		break;
	      case AssignableType.ppword:
		assert(0, "implement");
		break;
	      }
	      break;
	    case AssignableType.pword:
	      final switch(srcType) {
	      case AssignableType.reg:
		assert(0, "implement");
		break;
	      case AssignableType.pword:
		assert(0, "implement");
		break;
	      case AssignableType.regp:
		assert(0, "implement");
		break;
	      case AssignableType.ppword:
		assert(0, "implement");
		break;
	      }
	      break;
	    case AssignableType.regp:
	      final switch(srcType) {
	      case AssignableType.reg:
		assert(0, "implement");
		break;
	      case AssignableType.pword:
		assert(0, "implement");
		break;
	      case AssignableType.regp:
		assert(0, "implement");
		break;
	      case AssignableType.ppword:
		assert(0, "implement");
		break;
	      }
	      break;
	    case AssignableType.ppword:
	      final switch(srcType) {
	      case AssignableType.reg:
		assert(0, "implement");
		break;
	      case AssignableType.pword:
		assert(0, "implement");
		break;
	      case AssignableType.regp:
		assert(0, "implement");
		break;
	      case AssignableType.ppword:
		assert(0, "implement");
		break;
	      }
	      break;
	    }
	    
	  } else { // COPY from immediate

	    final switch(destType) {
	    case AssignableType.reg:
	  
	      debugInstruction("COPY reg word");
	      auto dstReg = registerIndexFromIP!1;
	      nativeIP += 3;
	      auto rawValue = decodePackedWord(nativeIP);
	      nativeIP += 1 + wordPackedLength(*nativeIP);

	      static if(type == VmType.processor) {
		setRegister(dstReg, rawValue, maskFromEncodedWidth(encodedByteWidth));
	      } else static if(type == VmType.disassembler) {
		writefln("copy %s %s", registerName(dstReg), rawValue);
	      }
	    
	      break;
	    case AssignableType.pword:

	      debugInstruction("COPY ^word word");
	      nativeIP++;
	      auto dstAddr = decodePackedAddress(nativeIP);
	      nativeIP += 1 + wordPackedLength(*nativeIP);
	      auto rawValue = decodePackedWord(nativeIP);
	      nativeIP += 1 + wordPackedLength(*nativeIP);

	      static if(type == VmType.processor) {
		memory.store(dstAddr, encodedByteWidth, rawValue);
	      } else static if(type == VmType.disassembler) {
		writefln("copy ^(0x%x) %s", dstAddr, rawValue);
	      }


	      break;
	    case AssignableType.regp:
	      assert(0, "implement");
	      break;
	    case AssignableType.ppword:
	      assert(0, "implement");
	      break;
	    }

	  }
	}

	break;
      case 0b0010:
	assert(0, "implement");
	break;
      case 0b0011:
	assert(0, "implement");
	break;
      case 0b0100:
	assert(0, "implement");
	break;
      case 0b0101:
	assert(0, "implement");
	break;
      case 0b0110:
	assert(0, "implement");
	break;
      case 0b0111:
	assert(0, "implement");
	break;
      case 0b1000:
	assert(0, "implement");
	break;
      case 0b1001:
	assert(0, "implement");
	break;
      case 0b1010:
	assert(0, "implement");
	break;
      case 0b1011:
	assert(0, "implement");
	break;
      case 0b1100:
	assert(0, "implement");
	break;
      case 0b1101:
	assert(0, "implement");
	break;
      case 0b1110:
	assert(0, "implement");
	break;
      case 0b1111:
	assert(0, "implement");
	break;
      }

      break;
    case 0b0001: // 0b0001_xxxx
    case 0b0010: // 0b0010_xxxx
    case 0b0011: // 0b0011_xxxx
    case 0b0100: // 0b0100_xxxx
    case 0b0101: // 0b0101_xxxx
    case 0b0110: // 0b0110_xxxx
    case 0b0111: // 0b0111_xxxx
    case 0b1000: // 0b1000_xxxx
    case 0b1001: // 0b1001_xxxx
    case 0b1010: // 0b1010_xxxx
    case 0b1011: // 0b1011_xxxx
    case 0b1100: // 0b1100_xxxx
    case 0b1101: // 0b1101_xxxx
    case 0b1110: // 0b1110_xxxx
    case 0b1111: // 0b1111_xxxx
      throw new Exception(format("unknown instruction 0b%b(0x%x)(%s)", b, b, b));
    }
  }

  return 0;
}

class FourRegisters : IRegisterManager
{
  word[4] registers = void;
  void zero()
  {
    registers[] = 0;
  }
  word[] createRegisters()
  {
    return registers;
  }
  word[] requestMoreRegisters(size_t newSize)
  {
    writefln("Error: code is using a register index out of range '%s'", newSize - 1);
    return null;
  }
}
class DefaultErrorHandler : IErrorHandler
{
  Exception ipOutOfRange(ubyte* codeBase, ubyte* codeLimit, ubyte* ip)
  {
    writefln("Error: instruction pointer '%s' is out of range [%s-%s]", ip, codeBase, codeLimit-1);
    return null;
  }
  Exception notEnoughRegisters(size_t neededRegisterCount)
  {
    writefln("Error: not enough registers, need %s", neededRegisterCount);
    return null;
  }
  Exception codeEndedInsideInstruction(ubyte* ip, ubyte* codeLimit, size_t length)
  {
    writefln("Error: code ended inside instruction (required %s, actual %s) %s", length, codeLimit-ip, ip[0..codeLimit-ip]);
    return null;
  }
}


version(unittest)
{
  void basicRegressionTests()
  {
    ubyte[512] codeBuffer = void;
    ubyte* code;

    FourRegisters registers = new FourRegisters();
    NativeMemory memory = NativeMemory(new ubyte[512]);
    auto errorHandler = new DefaultErrorHandler();

    writefln("[TEST] vm!%s Copy(reg,word) & Copy(reg,reg)", wordSize);
    uword[] testValues = [uword.max, uword.max / 2, uword.max / 3, 0, sword.max];
    foreach(ubyte encodedWidth; 0..encodedWordWidth+1) {
      auto width = decodeWidth(encodedWidth);
      foreach(testValue; testValues) {
	testValue &= maskFromEncodedWidth(encodeWidth(width));

	code = codeBuffer.ptr;
	code = emitCopy(code, reg.gp(0), testValue, width);
	code = emitCopy(code, reg.gp(2), reg.gp(0), width);
	code = emitHalt(code);
	code = emitCodePadding(code);
	registers.zero();
	runVirtualMachine(registers, memory, errorHandler,
			  codeBuffer[0..code-codeBuffer.ptr]);
	runVirtualMachine!(VmType.disassembler)(registers, memory, errorHandler,
						codeBuffer[0..code-codeBuffer.ptr]);
	assert(registers.registers[0] == testValue);
	assert(registers.registers[2] == testValue);
      }
    }

    writefln("[TEST] vm!%s Copy(^word,word) & Copy(reg,^word)", wordSize);
    foreach(ubyte encodedWidth; 0..encodedWordWidth+1) {
      auto width = decodeWidth(encodedWidth);
      uword mask = maskFromEncodedWidth(encodedWidth);
      uword testValue = cast(uword)0x123456789ABCDEF0 & mask;
      writefln("[TEST] vm!%s width %s testValue %s 0x%x", wordSize, width, testValue, testValue);
      //memory.store(1, encodedWidth, testValue);

      code = codeBuffer.ptr;
      code = emitCopy(code, pword(1), testValue, width);
      code = emitCopy(code, reg.gp(0), pword(1), width);
      code = emitHalt(code);
      code = emitCodePadding(code);
      registers.zero();
      runVirtualMachine(registers, memory, errorHandler,
			codeBuffer[0..code-codeBuffer.ptr]);
      runVirtualMachine!(VmType.disassembler)(registers, memory, errorHandler,
			codeBuffer[0..code-codeBuffer.ptr]);

      assert(memory.load(1, encodedWidth) == testValue);
      assert((registers.registers[0] & mask) == testValue);
    }



/+
    // Check Disassembler
    code = codeBuffer.ptr;
    code = emitCopy(code, reg.gp(0), cast(uword)56, 1);
    code = emitCopy(code, reg.gp(2), reg.gp(0), 1);
    code = emitCopy(code, pword(1), cast(uword)45, 1);
    code = emitCopy(code, reg.gp(0), pword(1), 1);
    code = emitHalt(code);
    code = emitCodePadding(code);
    registers.zero();
    runVirtualMachine!(VmType.disassembler)(registers, memory, errorHandler,
		      codeBuffer[0..code-codeBuffer.ptr]);
+/    


/+
 TODO: need to figure out how to modify the instruction to accomodate
       storing integer types of different sizes
    writeln("[TEST] Copy(pword,word)");
    code = codeBuffer.ptr;
    code = emitCopy(code, pword(0), 0x49);
    code = emitHalt(code);
    code = emitCodePadding(code);
    registers.zero();
    runVirtualMachine(registers, memory, errorHandler,
		      codeBuffer[0..code-codeBuffer.ptr]);
    assert(cast memory.load!(0));
+/


    /+
     writeln("[TEST] Copy reg ^word");
     code = codeBuffer.ptr;
     code = emitCopy(code, reg.gp(0), 498392);
     code = emitCopy(code, reg.gp(2), reg.gp(0));
     code = emitHalt(code);
     code = emitCodePadding(code);
     registers.zero();
     runVirtualMachine(registers, memory, errorHandler,
     codeBuffer[0..code-codeBuffer.ptr]);
     assert(registers.registers[0] == 498392);
     assert(registers.registers[2] == 498392);
     +/
  }
}


} // end of vm template


unittest
{
  vm!1.basicRegressionTests();
  vm!2.basicRegressionTests();
  vm!4.basicRegressionTests();
  vm!8.basicRegressionTests();
  //vm!(size_t.sizeof).basicRegressionTests();
}


void main()
{
  // NOTE: the code buffer should be padded with some 0s.  This
  //       is so that the code that reads an instruction does not
  //       need to check the code size.  Instead, that code will assume
  //       that it has enough bytes for it's instruction, and overruns are
  //       are checked for between every instruction.  Because of this, it should
  //       be padded with enough 0s to handle the maximum instruction size.

/+
  ubyte[512] codeBuffer = void;
  
  auto code = codeBuffer.ptr;
  code = emitCopy(code, reg.gp(0), 3);
  //code = emitCopy(code, reg.gp(0), reg.gp(1));
  code = emitCopy(code, reg.gp(1), reg.gp(0));
  code = emitHalt(code);
  code = emitCodePadding(code);

  writefln("code %s", codeBuffer[0..code-codeBuffer.ptr]);

  runVirtualMachine(new FourRegisters(), new DefaultStack(new ubyte[512]), new DefaultErrorHandler(),
		    codeBuffer[0..code-codeBuffer.ptr]);
+/
}

