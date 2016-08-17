import construct.ir;

/*
  Type (some examples):
    anything named? nullable? type symbol bool pointer
    number integer signed int byte unsigned uint ubyte
    uni construct block list array string ascii unicode
    utf8
  CountType:
    one (must appear once)
    optional (may or may not appear)
      prefix (presence may be dependent on a prefix keyword)
    list
      prefix (The list as a whole is present based on a keyword)
      itemPrefix (each item has the given prefix)
*/
enum CountType {
  one,
  optional,
  list,
}

struct PatternNode
{
  const(char)[] name;
  CountType countType;
}

struct PatternParameter
{
  const(char)[] name;
  ConstructType type;
  CountType countType;
  const(char)[] prefix;
  const(char)[] itemPrefix;
}
struct Pattern
{
  immutable(PatternParameter)[] params;
  /*
  this(PatternParameter[] params) immutable
  {
    this.params = params;
  }
  */
}

/*
class CatchBlockType
{
  //C
}
*/
/*
struct CatchInfo {
  errorSymbol symbol;
  errorType type;
  handler block;
}
*/




immutable tryPattern = Pattern
  ([immutable PatternParameter("tryBlock"    , new immutable PrimitiveType(0, PrimitiveTypeEnum.constructBlock)),
    immutable PatternParameter("catchBlocks" , new immutable PrimitiveType(0, PrimitiveTypeEnum.constructBlock), CountType.list, null, "catch"),
    immutable PatternParameter("finallyBlock", new immutable PrimitiveType(0, PrimitiveTypeEnum.constructBlock), CountType.optional, "finally"),
    ]);


unittest
{
  //defcon try (tryBlock block, catchBlocks CatchInfo list itemPrefix="catch", finallyBlock block prefix="finally")
  
}

