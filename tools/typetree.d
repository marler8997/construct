// NOTE: this tool should be a construct library so that you
//       could print the type tree in any construct context.
// A construct like: dumpTypeTree.

import std.stdio;
import std.array: Appender;
import std.conv : to;

import construct.backendCore;

immutable string spaces = "                                      ";

__gshared TreeNode root = new TreeNode(PrimitiveTypeEnum.anything);
class TreeNode
{
  PrimitiveTypeEnum primitiveType;
  Appender!(TreeNode[]) children;
  this(PrimitiveTypeEnum primitiveType)
  {
    this.primitiveType = primitiveType;
  }
  TreeNode getOrCreateChild(PrimitiveTypeEnum type)
  {
    foreach(existing; children.data) {
      if(existing.primitiveType == type) {
        return existing;
      }
    }
    TreeNode newNode = new TreeNode(type);
    children.put(newNode);
    return newNode;
  }
  void print(int level)
  {
    writefln("%s%s", spaces[0..level*2], primitiveType);
    level++;
    foreach(child; children.data) {
      child.print(level);
    }
  }
  void printBracket(int level)
  {
    if(children.data.length == 0) {
      writefln("%s%s", spaces[0..level*2], primitiveType);
    } else {
      writefln("%s[%s", spaces[0..level*2], primitiveType);
      level++;
      foreach(child; children.data) {
        child.printBracket(level);
      }
      level--;
      writefln("%s]", spaces[0..level*2]);
    }
  }
}

TreeNode getNode(PrimitiveTypeEnum type)
{
  if(type == PrimitiveTypeEnum.anything) {
    return root;
  }
  return getNode(type.definitionRef.parentTypeEnum).getOrCreateChild(type);
}

int main(string[] args)
{
  foreach(primitiveType; primitiveTypes[1..$]) {
    writefln("Adding primitive type: '%s'", primitiveType);
    getNode(primitiveType.parentTypeEnum).getOrCreateChild(primitiveType.typeEnum);
  }

  //root.print(0);
  root.printBracket(0);

  return 0;
}
