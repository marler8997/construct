import std.stdio;
import std.array  : Appender;
import std.string : format;

import construct.ir;
import construct.patterns;


IT round(IT,FT)(FT number) pure
{
  IT rounded = cast(IT)number;
  if(number >= 0) {
    if(number - rounded >= .5) {
      rounded++;
    }
  } else {
    throw new Exception("rounding negatives not implemented");
  }
  //writefln("Rounded %s to %s", number, rounded);
  return rounded;
}

//
// Fonts
//

struct ConsolasFont
{
  //                  charBoxWidth
  //                       |
  //               -  |----------| -
  //               |  |    /\    | |
  //               |  |   /  \   | |- charLineHeight
  // charBoxHeight |  |  /____\  | |
  //               |  | /      \ | |
  //               |  |/________\| - 
  //               |  |          |
  //               -  |----------|
  //
  //
  
  float size;
  float charBoxWidth;
  float charLineHeight;
  float charBoxHeight;

  size_t charBoxWidthRounded;
  size_t charBoxHeightRounded;
  this(float size, float charBoxWidth, float charLineHeight, float charBoxHeight) immutable
  {
    this.size = size;
    this.charBoxWidth = charBoxWidth;
    this.charLineHeight = charLineHeight;
    this.charBoxHeight = charBoxHeight;

    this.charBoxWidthRounded = charBoxWidth.round!size_t;
    this.charBoxHeightRounded = charBoxHeight.round!size_t;
  }
}
// Note: These font sizes are pretty good, but aren't perfect.
//       the height is not a big deal, should be good, but the width
//       will eventually be off, likely after a few hundred characters.
immutable font12 = immutable ConsolasFont(12, 6.5977, 9.6, 12);
immutable font20 = immutable ConsolasFont(20, 11, 16, 20);
immutable font40 = immutable ConsolasFont(40, 22, 32, 40);
//alias defaultFont = font20;
alias defaultFont = font20;

void fillHumanReadable(char[] buffer)
{
  char c = ' ';
  foreach(i; 0..buffer.length) {
    buffer[i] = c;
    if(c == 126) {
      c = ' ';
    } else {
      c++;
    }
  }
}
void renderFontSizeTest(File file, ConsolasFont font, bool renderAllRows = true)
{
  size_t originOffset = 40;
  char[127-32] testText;
  //char[2020] testText;
  char[testText.length] uniformRows;
  fillHumanReadable(testText);

  auto evenColumnColor = "#eee";
  auto underlineColor = "#ccc";

  // draw columns
  {
    float x = originOffset;
    float height;
    if(renderAllRows) {
      height = (testText.length + 1) * font.charBoxHeight;
    } else {
      height = font.charBoxHeight;
    }
    for(size_t i = 0; i < testText.length; i+=2) {
      file.writefln(`<rect x="%s" y="%s" width="%s" height="%s" style="fill:%s"/>`,
                    x, originOffset, font.charBoxWidth, height, evenColumnColor);
      x += font.charBoxWidth*2;
    }
  }
  
  size_t y = originOffset;
  file.writefln(`<rect x="%s" y="%s" width="%s" height="1" style="fill:%s"/>`,
                originOffset, y+font.charLineHeight, testText.length*font.charBoxWidth, underlineColor);
  file.writefln(`<text x="%s" y="%s">%s</text>`, originOffset, y + font.charLineHeight, SvgText(testText));

  if(renderAllRows) {
    foreach(i; 0..testText.length) {
      y+= font.charBoxHeight;
      uniformRows[] = testText[i];
      file.writefln(`<rect x="%s" y="%s" width="%s" height="1" style="fill:%s"/>`,
                    originOffset, y+font.charLineHeight, testText.length*font.charBoxWidth, underlineColor);
      file.writefln(`<text x="%s" y="%s">%s</text>`, originOffset, y + font.charLineHeight, SvgText(uniformRows));
    }
  }
}


void renderDebugHeight(File file, ConsolasFont font,
                       string label, string color, size_t x, size_t y, size_t height)
{
  file.writefln(`<rect x="%s" y="%s" width="5" height="%s" fill="%s"/>`, x, y, height, color);
  file.writefln(`<text x="%s" y="%s" style="font-size:%s">%s</text>`, x + 8, y+(height+font.charBoxHeight)/2, font.size, label);
}




void printGrid(File file, size_t x, size_t y, size_t width, size_t height, size_t size)
{
  for(auto row = y; row < y+height; row += size) {
    file.writefln(`<rect x="%s" y="%s" width="%s" height="1" fill="#dbdbdb"/>`, x, row, width);
  }
  for(auto col = x; col < x+width; col += size) {
    file.writefln(`<rect x="%s" y="%s" width="1" height="%s" fill="#dbdbdb"/>`, col, y, height);
  }
}


struct SvgText
{
  const(char)[] text;
  this(const(char)[] text)
  {
    this.text = text;
  }
  void toString(scope void delegate(const(char)[]) sink) const
  {
    foreach(c; text) {
      if(c == '<') {
        sink("&lt;");
      } else if(c == '>') {
        sink("&gt;");
      } else if(c == '&') {
        sink("&amp;");
      } else if(c == ' ') {
        sink("&#160;");
      } else {
        sink((&c)[0..1]);
      }
    }
  }
}

struct SvgPatternSettings
{
  ConsolasFont font;
  size_t graphicMarginY;
  size_t graphicMarginX;
  size_t nodePadding;
  size_t nodeMargin;
  size_t groupBoxMargin;
  size_t groupTitleAfterSpace;
  size_t nodeBorderWidth;
  size_t groupBorderWidth;
  size_t lineWidth;
  size_t railMarginY;
  size_t nodeCornerRadius;
  size_t terminalRadius;
  size_t curveRadius;
  
  size_t roadPadding;
  size_t roadMargin;

  size_t nodeOpacity;
  string nodeColor;
}

class SvgNode
{
  CountType countType;
  size_t leftWidth, rightWidth;
  size_t height;
  this(CountType countType, size_t leftWidth, size_t rightWidth, size_t height)
  {
    this.countType = countType;
    this.leftWidth = leftWidth;
    this.rightWidth = rightWidth;
    this.height = height;
  }
  abstract size_t margin(const(SvgPatternSettings)* settings) const;
  abstract inout(SvgSubPatternNode) tryAsSubPattern() inout;
  abstract void render(File file, const(SvgPatternSettings)* settings, size_t x, size_t y, const(SvgNode)[] rest) const;
}

class TerminalNode : SvgNode
{
  this(const(SvgPatternSettings)* settings)
  {
    super(CountType.one, settings.terminalRadius, settings.terminalRadius, settings.terminalRadius*2);
  }
  override size_t margin(const(SvgPatternSettings)* settings) const
  {
    return 0;
  }
  override inout(SvgSubPatternNode) tryAsSubPattern() inout { return null; }
  override void render(File file, const(SvgPatternSettings)* settings, size_t x, size_t y, const(SvgNode)[] rest) const
  {
    file.writefln(`  <circle cx="%s" cy="%s" r="%s" fill="black"/>`,
                  x+settings.terminalRadius, y+settings.terminalRadius, settings.terminalRadius);
  }
}
class SvgSingleNode : SvgNode
{
  const(char)[] text;
  this(CountType countType, const(char)[] text, const(SvgPatternSettings)* settings)
  {
    this.text = text;
    auto totalTextWidth = round!size_t(text.length*settings.font.charBoxWidth);
    auto leftTextWidth = totalTextWidth / 2;
    super(countType,
          settings.nodePadding + leftTextWidth,
          (totalTextWidth-leftTextWidth) + settings.nodePadding,
          settings.font.charBoxHeightRounded + 2*settings.nodePadding);
  }
  override size_t margin(const(SvgPatternSettings)* settings) const
  {
    return settings.nodeMargin;
  }
  override inout(SvgSubPatternNode) tryAsSubPattern() inout { return null; }
  override void render(File file, const(SvgPatternSettings)* settings, size_t x, size_t y, const(SvgNode)[] rest) const
  {
    file.writefln(`  <rect x="%s" y="%s" width="%s" height="%s" rx="%s" ry="%s" style="fill-opacity:%s;fill:%s;stroke-width:%s;stroke:black;" />`,
                  x, y, leftWidth+rightWidth, settings.font.charBoxHeight + 2*settings.nodePadding,
                  settings.nodeCornerRadius, settings.nodeCornerRadius,
		  settings.nodeOpacity, settings.nodeColor,
		  settings.nodeBorderWidth);
    assert(height == settings.font.charBoxHeight + 2*settings.nodePadding);
    file.writefln(`  <text x="%s" y="%s">%s</text>`,
                  x + settings.nodePadding,
                  y + settings.font.charLineHeight + settings.nodePadding, text);
  }
}
class SvgSubPatternNode : SvgNode
{
  const(char)[] title;
  SvgNode[] subNodes;
  bool hasOptional;
  //bool entirePatternOptional;

  override size_t margin(const(SvgPatternSettings)* settings) const
  {
    return settings.groupBoxMargin;
  }
  override inout(SvgSubPatternNode) tryAsSubPattern() inout { return this; }
  
  this(CountType countType, const(char)[] title, const(SvgPatternSettings)* settings, const(PatternNode)[] nodes)
  {
    assert(nodes.length > 0);
    
    this.title = title;
    
    subNodes = new SvgNode[nodes.length];
    scope creator = new SvgNodeCreator(subNodes, settings);
    foreach(nodeIndex, node; nodes) {
      creator.nextName = node.name;
      creator.nextCountType = node.countType;
      creator.nextIndex = nodeIndex;
      node.matcher.visit(creator);
    }

    auto bounds = measureBounds(settings, subNodes);

    size_t titleHeight = 0;
    size_t titleLeftWidth = 0;
    if(title.length > 0) {
      titleHeight = settings.font.charBoxHeightRounded + settings.groupTitleAfterSpace;
      titleLeftWidth = cast(size_t)(title.length * settings.font.charBoxWidth) + 2*settings.nodePadding;
    }
    size_t leftWidth = (titleLeftWidth > bounds.left) ? titleLeftWidth : bounds.left;
    super(countType, leftWidth, bounds.right, 2*settings.nodePadding+titleHeight+bounds.height);
  }
  override void render(File file, const(SvgPatternSettings)* settings, size_t x, size_t y, const(SvgNode)[] rest) const
  {
    size_t boxY = y;

    auto railStartY = y;
    y += settings.nodePadding;
    if(title.length > 0) {
      file.writefln(`  <text x="%s" y="%s">%s</text>`, x + settings.nodePadding, y + settings.font.charLineHeight, title);
      y += settings.font.charBoxHeightRounded;
      y += settings.groupTitleAfterSpace;
    }
    file.writefln(`  <line x1="%s" y1="%s" x2="%s" y2="%s" style="stroke:black;stroke-width:%s" />`,
		  x+leftWidth, railStartY, x+leftWidth, y, settings.lineWidth);

    y = renderNodes(file, settings, subNodes, x + leftWidth, y);
    
    file.writefln(`  <line x1="%s" y1="%s" x2="%s" y2="%s" style="stroke:black;stroke-width:%s" />`,
		  x+leftWidth, y, x+leftWidth, y + settings.nodePadding, settings.lineWidth);
    y += settings.nodePadding;
    file.writefln(`  <rect x="%s" y="%s" width="%s" height="%s" style="fill-opacity:0;stroke-width:%s;stroke:black" />`,
                  x, boxY, leftWidth+rightWidth, y - boxY, settings.groupBorderWidth);
    //assert(y-boxY==height, format("expected height %s, got %s", height, y-boxY));
  }
}

void renderRail(File file, const(SvgPatternSettings)* settings, ptrdiff_t drainX, ptrdiff_t railX, ptrdiff_t yTop, ptrdiff_t yBottom)
{
  ptrdiff_t xRadius;
  ptrdiff_t topYOffsetAtDrain;
  ptrdiff_t bottomYOffsetAtDrain;
  if(railX > drainX) {
    topYOffsetAtDrain    = settings.curveRadius;
    bottomYOffsetAtDrain = -settings.curveRadius;
    xRadius              = settings.curveRadius;
  } else {
    topYOffsetAtDrain    = -settings.curveRadius;
    bottomYOffsetAtDrain = settings.curveRadius;
    xRadius              = -settings.curveRadius;
  }
  file.writefln(`  <path d="M%s %s Q%s %s %s %sL%s %s Q%s %s %s %s L%s %s Q%s %s %s %s L%s %s Q%s %s %s %s"`~
                ` style="stroke:black;stroke-width:%s;fill-opacity:0" />`,
                drainX          , yTop + topYOffsetAtDrain,
                drainX          , yTop,
                drainX + xRadius, yTop,
                
                railX  - xRadius, yTop,
                railX           , yTop,
                railX           , yTop + settings.curveRadius,
                
                railX           , yBottom - settings.curveRadius,
                railX           , yBottom,
                railX - xRadius , yBottom,
                
                drainX + xRadius, yBottom,
                drainX          , yBottom,
                drainX          , yBottom + bottomYOffsetAtDrain,
                settings.lineWidth);
}


struct RailBounds
{
  size_t left,right,height;
}
RailBounds measureBounds(const(SvgPatternSettings)* settings, const(SvgNode)[] svgNodes)
{
  RailBounds bounds;
  
  foreach(nodeIndex, svgNode; svgNodes) {
    size_t svgNodeTotalLeftWidth  = svgNode.leftWidth + svgNode.margin(settings);
    size_t svgNodeTotalRightWidth = svgNode.rightWidth + svgNode.margin(settings);
    bounds.height += svgNode.height;

    // rail margins from the previous node
    if(nodeIndex > 0) {
      bounds.height += 2*settings.railMarginY;
    }
      
    if(nodeIndex > 0) {
      // right side: previous node's loop rail start
      if(svgNodes[nodeIndex-1].countType.isLoop) {
        bounds.height += settings.curveRadius*2;
      }
      // left side: option rail enters after skipping previous node
      if(svgNodes[nodeIndex-1].countType.isOptional) {
        bounds.height += settings.curveRadius*2;
      }
    }
    // left side: option rail starts
    if(svgNode.countType.isOptional) {
      bounds.height += settings.curveRadius*2;
      svgNodeTotalLeftWidth += settings.roadPadding + settings.roadMargin;
    }
    // right side: loop rail
    if(svgNode.countType.isLoop) {
      bounds.height += settings.curveRadius*2;
      svgNodeTotalRightWidth += settings.roadPadding + settings.roadMargin;
    }
    
    if(svgNodeTotalLeftWidth > bounds.left) {
      bounds.left = svgNodeTotalLeftWidth;
    }
    if(svgNodeTotalRightWidth > bounds.right) {
      bounds.right = svgNodeTotalRightWidth;
    }
  }
  // add spacing for bottom
  return bounds;
}

size_t renderNodes(File file, const(SvgPatternSettings)* settings, const(SvgNode)[] svgNodes, size_t center, size_t y)
                   
{
  if(svgNodes.length > 0) {
    SvgNode node = svgNodes[0].unconst;
    for(size_t i = 0;;) {
      node.render(file, settings, center - node.leftWidth, y, svgNodes[i+1..$]);
      y += node.height;
      i++;
      if(i >= svgNodes.length) {
        break;
      }
      auto previousNodeBottom = y;
      bool previousNodeIsLoop = node.countType.isLoop;
      bool previousNodeOptional = node.countType.isOptional;
      node = svgNodes[i].unconst;

      renderDebugHeight(file, font12, "top rail margin", "red", center, y, settings.railMarginY);
      y += settings.railMarginY;

      // right side: previous node's loop rail start
      if(previousNodeIsLoop) {
        y += settings.curveRadius*2;
      }
      // left side: option rail enters after skipping previous node
      if(previousNodeOptional) {
        y += settings.curveRadius*2; // make room for incoming rail
      }

      // left side: option rail starts
      size_t optionRailBranchY = 0;
      if(node.countType.isOptional) {
        optionRailBranchY = y + settings.curveRadius;
        y += settings.curveRadius*2; // make room for outgoing rail
      }

      // right side: loop rail
      size_t loopRailY = 0;
      if(node.countType.isLoop) {
        loopRailY = y + settings.curveRadius;
        y += settings.curveRadius*2; // make room for incoming loop rail
      }

      
      renderDebugHeight(file, font12, "bottom rail margin", "blue", center, y, settings.railMarginY);
      y += settings.railMarginY;
      // y should be located at the top of the node
      
      if(loopRailY) {
        renderRail(file, settings, center, center + node.rightWidth + node.margin(settings) + settings.roadPadding,
                   loopRailY, y + node.height + settings.railMarginY + settings.curveRadius);
      }
      if(optionRailBranchY) {
        size_t optionRailJoinY = y + node.height + settings.railMarginY + settings.curveRadius;
        if(loopRailY) {
          optionRailJoinY += 2*settings.curveRadius;
        }
        renderDebugHeight(file, font12, "option rail branch", "#00ff33", center, optionRailBranchY - settings.curveRadius, settings.curveRadius*2);
        renderDebugHeight(file, font12, "option rail join"  , "#00ff99", center, optionRailJoinY - settings.curveRadius, settings.curveRadius*2);
        renderRail(file, settings, center, center - node.leftWidth - node.margin(settings) - settings.roadPadding,
                   optionRailBranchY, optionRailJoinY);
      }
      
      file.writefln(`  <line x1="%s" y1="%s" x2="%s" y2="%s" style="stroke:black;stroke-width:%s" />`,
                    center, previousNodeBottom, center, y, settings.lineWidth);
    }

  }
  return y;
}


class SvgNodeCreator : IMatcherVisitHandler
{
  SvgNode[] svgNodes;
  const(SvgPatternSettings)* settings;
  
  const(char)[] nextName;
  CountType nextCountType;
  size_t nextIndex;

  this(SvgNode[] svgNodes, const(SvgPatternSettings)* settings)
  {
    this.svgNodes = svgNodes;
    this.settings = settings;
  }
  final void visit(const(AnyMatcher) matcher)
  {
    if(nextName) {
      svgNodes[nextIndex] = new SvgSingleNode(nextCountType, "anything (" ~ nextName ~ ")", settings);
    } else {
      svgNodes[nextIndex] = new SvgSingleNode(nextCountType, "anything", settings);
    }
  }
  final void visit(const(SymbolMatcher) matcher)
  {
    if(nextName) {
      svgNodes[nextIndex] = new SvgSingleNode(nextCountType, "symbol (" ~ nextName ~ ")", settings);
    } else {
      svgNodes[nextIndex] = new SvgSingleNode(nextCountType, "symbol", settings);
    }
  }
  final void visit(const(KeywordMatcher) matcher)
  {
    if(nextName) {
      svgNodes[nextIndex] = new SvgSingleNode(nextCountType, "'" ~ matcher.keyword ~ "' (" ~ nextName ~ ")", settings);
    } else {
      svgNodes[nextIndex] = new SvgSingleNode(nextCountType, "'"~matcher.keyword~"'", settings);
    }
  }
  final void visit(const(PrimitiveTypeMatcher!ConstructString) matcher)
  {
    if(nextName) {
      svgNodes[nextIndex] = new SvgSingleNode(nextCountType, "string (" ~ nextName ~ ")", settings);
    } else {
      svgNodes[nextIndex] = new SvgSingleNode(nextCountType, "string", settings);
    }
  }
  final void visit(const(PrimitiveTypeMatcher!ConstructBool) matcher)
  {
    if(nextName) {
      svgNodes[nextIndex] = new SvgSingleNode(nextCountType, "bool (" ~ nextName ~ ")", settings);
    } else {
      svgNodes[nextIndex] = new SvgSingleNode(nextCountType, "bool", settings);
    }
  }
  final void visit(const(PrimitiveTypeMatcher!ConstructList) matcher)
  {
    if(nextName) {
      svgNodes[nextIndex] = new SvgSingleNode(nextCountType, "list (" ~ nextName ~ ")", settings);
    } else {
      svgNodes[nextIndex] = new SvgSingleNode(nextCountType, "list", settings);
    }
  }
  final void visit(const(PrimitiveTypeMatcher!ObjectBreak) matcher)
  {
    if(nextName) {
      svgNodes[nextIndex] = new SvgSingleNode(nextCountType, "';' (" ~ nextName ~ ")", settings);
    } else {
      svgNodes[nextIndex] = new SvgSingleNode(nextCountType, "';'", settings);
    }
  }
  final void visit(const(PrimitiveTypeMatcher!ConstructBlock) matcher)
  {
    if(nextName) {
      svgNodes[nextIndex] = new SvgSingleNode(nextCountType, "block (" ~ nextName ~ ")", settings);
    } else {
      svgNodes[nextIndex] = new SvgSingleNode(nextCountType, "block", settings);
    }
  }
  final void visit(const(SubPatternMatcher) matcher)
  {
    if(nextName) {
      svgNodes[nextIndex] = new SvgSubPatternNode(nextCountType, "(" ~ nextName ~ ")", settings, matcher.nodes);
    } else {
      svgNodes[nextIndex] = new SvgSubPatternNode(nextCountType, "", settings, matcher.nodes);
    }
  }
}

void printSvg(Pattern pattern, File file, const(SvgPatternSettings)* settings)
{
  SvgNode[] svgNodes = new SvgNode[pattern.nodes.length+2];
  auto terminalNode = new TerminalNode(settings);
  svgNodes[0]   = terminalNode;
  svgNodes[$-1] = terminalNode;
  {
    scope creator = new SvgNodeCreator(svgNodes, settings);
    foreach(patternIndex, node; pattern.nodes) {
      creator.nextName = node.name;
      creator.nextCountType = node.countType;
      creator.nextIndex = patternIndex+1;
      node.matcher.visit(creator);
    }
  }

  auto bounds = measureBounds(settings, svgNodes);

  auto width  = 2*settings.graphicMarginX + bounds.left + bounds.right;
  auto height = 2*settings.graphicMarginY + bounds.height;
  
  file.writefln(`<svg xmlns="http://www.w3.org/2000/svg" version="1.1" font-family="Consolas" font-size="%s" width="%s" height="%s">`,
                settings.font.size, width, height);

  // background
  file.writefln(`  <rect x="0" y="0" width="%s" height="%s" style="fill:#f9f9f9" />`, width, height);
		
  //printGrid(file, 0, 0, 1000, 1000, 10);
  //printGrid(file, 0, 0, 1000, 1000, settings.font.charBoxWidth);

  //renderFontSizeTest(file, settings.font);

  size_t y = renderNodes(file, settings, svgNodes, settings.graphicMarginX + bounds.left, settings.graphicMarginY);
  file.writeln(`</svg>`);
}


void main()
{
  SvgPatternSettings settings;
  settings.font = defaultFont;
  settings.graphicMarginY = 40;
  settings.graphicMarginX = 40;
  settings.nodePadding = 14;
  settings.nodeMargin = 20;
  settings.groupBoxMargin = 30;
  settings.groupTitleAfterSpace = 20;
  settings.nodeBorderWidth = 3;
  settings.groupBorderWidth = 1;
  settings.lineWidth = 3;
  settings.railMarginY = 30;
  settings.nodeCornerRadius = 8;
  settings.terminalRadius = 16;
  settings.curveRadius = 40;
  settings.roadPadding = 30;
  settings.roadMargin = 15;

  settings.nodeOpacity = 1;
  settings.nodeColor = "#fff";

  immutable bigTestPattern = immutable Pattern
  ([immutable PatternNode("tryBlock", CountType.one, Matcher.block),
    immutable PatternNode("catchClauses", CountType.star, new immutable SubPatternMatcher
			  ([immutable PatternNode(null, CountType.one, new immutable KeywordMatcher("catch")),
			    immutable PatternNode("catchBlock", CountType.one, Matcher.block)])),

    immutable PatternNode("testClause", CountType.star, new immutable SubPatternMatcher
			  ([immutable PatternNode(null, CountType.one, new immutable KeywordMatcher("catch")),
			    immutable PatternNode("subSubPattern", CountType.star, new immutable SubPatternMatcher
                            ([immutable PatternNode("subSubClause", CountType.star, Matcher.string_),
                              immutable PatternNode("subSubBlock" , CountType.one , Matcher.block)]))])),
                                                    
    immutable PatternNode("finallyClause", CountType.optional, new immutable SubPatternMatcher
			  ([immutable PatternNode(null, CountType.one, new immutable KeywordMatcher("finally")),
                            immutable PatternNode("attr", CountType.star, Matcher.symbol),
			    immutable PatternNode("finallyBlock", CountType.one, Matcher.block)]))]);
  printSvg(bigTestPattern, File(`C:\temp\bigTestPattern.svg`, "w"), &settings);

  immutable testPattern = immutable Pattern
    ([immutable PatternNode("a really long name",CountType.one,Matcher.string_)]);
  printSvg(testPattern, File(`C:\temp\out.svg`, "w"), &settings);
  immutable testPattern2 = immutable Pattern
    ([immutable PatternNode("another long name",CountType.optional,Matcher.string_)]);
  printSvg(testPattern2, File(`C:\temp\out2.svg`, "w"), &settings);
  immutable testPattern3 = immutable Pattern
    ([immutable PatternNode(null,CountType.star,Matcher.string_)]);
  printSvg(testPattern3, File(`C:\temp\out3.svg`, "w"), &settings);
  printSvg(tryPattern, File(`C:\temp\tryPattern.svg`, "w"), &settings);

  immutable basicSubPattern = immutable Pattern
    ([immutable PatternNode(null,CountType.one,new immutable SubPatternMatcher
                            ([immutable PatternNode(null,CountType.one,Matcher.string_)])
                            )]);
  printSvg(basicSubPattern, File(`C:\temp\basicSubPattern.svg`, "w"), &settings);
}
