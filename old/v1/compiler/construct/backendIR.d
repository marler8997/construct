module construct.backendIR;

import std.array : Appender;
import std.conv  : to;
import construct.ir;


struct BackendConstructBuilder
{
  Appender!(BackendObject[]) args;
  BackendObject[const(char)[]] namedArgs;
}

alias BackendConstruct = ConstructImpl!(BackendObject);
alias BackendList = ConstructListImpl!BackendObject;


abstract class BackendType : BackendObject
{
  this(size_t lineNumber)
  {
    super(lineNumber);
  }
}
class BackendPrimitiveType : ConstructType {
  PrimitiveTypeEnum typeEnum;
  this(size_t lineNumber, PrimitiveTypeEnum typeEnum)
  {
    super(lineNumber);
    this.typeEnum = typeEnum;
  }
  override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink(to!string(typeEnum));
  }
  override bool equals(const(ConstructObject) otherObj, bool checkLineNumber = true) const
  {
    if(checkLineNumber && lineNumber != otherObj.lineNumber) {
      return false;
    }
    auto other = cast(PrimitiveType)otherObj;
    return other && this.typeEnum == other.typeEnum;
  }
}
class BackendConstructType : BackendType {
  const(char)[] constructName;
  this(size_t lineNumber, const(char)[] constructName) {
    super(lineNumber);
    this.constructName = constructName;
  }
  final override void toString(scope void delegate(const(char)[]) sink) const
  {
    sink("(construct ");
    sink(constructName);
    sink(")");
  }
  final override bool equals(const(ConstructObject) otherObj, bool checkLineNumber = true) const
  {
    auto other = cast(BackendConstructType)otherObj;
    return other && equals(other, checkLineNumber);
  }
  final bool equals(const(BackendConstructType) other, bool checkLineNumber = true) const
  {
    return this.constructName == other.constructName;
  }
}
