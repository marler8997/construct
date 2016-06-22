import construct.ir;
import std.array : Appender;

mixin IRLoaderEntryPoint;
void loadCode(Appender!(Construct[]) code)
{
  code.put(new Construct("import", [new StringLiteral("std.procedural")]));
  code.put(new Construct("function", [new StringLiteral("main"), StandardType.int_,
			 new Construct("import", [
						  new StringLiteral("std.io"),
						  ]),
			 new Construct("write", [
						 new StringLiteral("Hello, World!"),
						 ])]));
}
