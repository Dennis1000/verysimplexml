{ VerySimpleXML v1.9 BETA - a lightweight, one-unit, cross-platform XML reader/writer
  for Delphi 2009-XE5 by Dennis Spreen
  http://blog.spreendigital.de/2011/11/10/verysimplexml-a-lightweight-delphi-xml-reader-and-writer/

  1.0 Initial release
  1.1 Removed "extended" quotation marks support, Renamed to XmlVerySimple
  1.2 Switched TStringList.Load to TStreamReader
  1.3 LoadFromFile/Stream now checks if header is UTF8
  1.4 Removed ' from node attributes
  1.5 Replaced string access with High/Low(string) for NextGen compiler compatibility
      Compact the XML now by using Options := [doCompact]
      XmlNode:
        Attribute value is now escaped/unescaped
        Attribute['tag'] is now Attributes['tag'] (TXmlDocument compatible), all attributes are found in AttributeList
        Added Name (same as NodeName)
        Moved 'find' procedures over to TXmlNodeList (added stubs accordingly)
      Added TXmlDocument compatible functions:
        NodeValue (same as Text), NodeName (same as Name), Encoding (same as Header.Attribute['encoding']),
        Version (same as Header.Attribute['version']), Options (the only compatible option is [doAutoIdent])
        Xml.AddChild (replaces root node name), Xml.DocumentElement (same as root), Node.FirstChild (same as
        Node.ChildNodes.First), Node.LastChild (same as Node.ChildNodes.Last), Node.NextSibling, Node.PreviousSibling
  1.6 Added comment nodes (see NodeType property)
  1.7 Switched TStringList.Save to TStreamWriter
      Renamed 'Indent' to 'NodeIndentStr' (TXmlDocument compatible)
      Added LineBreak (initalized to sLineBreak (=OS depended), e.g. set to #$0A if you want unix/osx/posix compatible linebreaks)
      Added TXmlDocument compatible functions:
        StandAlone (same as Header.Attributes['standalone'])
  1.8 Added basic doctype nodes (see NodeType property)
      Changed 'Root' to 'DocumentElement' (TXmlDocument compatible)
      Be sure to call Xml.AddChild('root') before adding nodes to Xml.DocumentElement!
      All nodes are now in Xml.ChildNodes (incl. comments and doctypes which may appear before the DocumentElement)
      Supports now loading of XML documents without xml header
      Added property Xml.Xml (same as Xml.Text, TXmlDocument compatible)
      Replaced String operations with character pointers (results in slightly faster loading)
  1.9 Added ntProcessingInstr nodes
      Added quote support for attributes to allow quotes inside attributes (e.g. attrib1='Franky "Gunshot" Gimley')
      Only '<' and '&' are now automatically escaped/unescaped inside node text content
      Added ntText nodes
      Parsing routines modularized
      Tries to detect BOM if no encoding is specified before loading
      Uses ANSI encoding if an encoding is set before loading but it's not 'utf-8'
      A text node is only created if the text consists of anything other than whitespaces/tab/returns
      Added High(String) function for D9-XE2
      Replaced XMLArray with TXmlNodeList

  (c) Copyrights 2011-2014 Dennis D. Spreen <dennis@spreendigital.de>
  This unit is free and can be used for any needs. The introduction of
  any changes and the use of those changed library is permitted without
  limitations. Only requirement:
  This text must be present without changes in all modifications of library.

  * The contents of this file are used with permission, subject to
  * the Mozilla Public License Version 1.1 (the "License"); you may   *
  * not use this file except in compliance with the License. You may  *
  * obtain a copy of the License at                                   *
  * http:  www.mozilla.org/MPL/MPL-1.1.html                           *
  *                                                                   *
  * Software distributed under the License is distributed on an       *
  * "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or    *
  * implied. See the License for the specific language governing      *
  * rights and limitations under the License.                         *
}
unit Xml.VerySimple;

interface

uses
  Classes, SysUtils, Generics.Defaults, Generics.Collections;

const
  TXmlSpaces = [#$20, #$0A, #$0D, #9];

type
  TXmlNode = class;
  TXmlNodeType = (ntElement, ntComment, ntDocType, ntDocument, ntXmlDecl, ntProcessingInstr, ntText);
  TXmlNodeList = class;
  TXmlOptions = set of (doNodeAutoIndent, doCompact);
  TExtractTextOptions = set of (etoDeleteStopChar, etoStopString);

  TXmlStreamReader = class(TStreamReader)
  public
    LineBreak: String;
    LinePos: Integer;
    LineLength: Integer;
    EndOfLine: Boolean;
    Line: String;
    function ExtractText(StopChars: String; Options: TExtractTextOptions): String;
    function ReadLine: String; override;
    function IsUppercaseText(Value: String): Boolean;
    procedure IncLinePos(Value: Integer = 1);
  end;

  TXmlAttribute = class(TObject)
  public
    Name: String; // Attribute name
    Value: String; // Attribute value (always as String)
    Quote: Char; // Quote used during reading, default "
    constructor Create; virtual;
  end;

  TXmlAttributeList = class(TObjectList<TXmlAttribute>)
  public
    function Find(const Name: String): TXmlAttribute; virtual;
    // Find an Attribute by Name (not case sensitive)
    procedure Delete(const Name: String); overload; virtual;
    function HasAttribute(const AttrName: String): Boolean; virtual;
  end;

  TXmlNode = class(TObject)
  private
  protected
    function GetAttr(const AttrName: String): String; virtual;
    procedure SetAttr(const AttrName: String; const AttrValue: String); virtual;
  public
    Parent: TXmlNode; // NIL only for Root-Node
    Name: String; // Node name
    ChildNodes: TXmlNodeList; // Child nodes, never NIL
    Text: String;
    AttributeList: TXmlAttributeList;
    NodeType: TXmlNodeType;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    // Find a childnode by its name
    function Find(const Name: String): TXmlNode; overload;
    // Find a childnode by Name/Attribute
    function Find(const Name, AttrName: String): TXmlNode; overload;
    // Find a childnode by Name/Attribute/Value
    function Find(const Name, AttrName, AttrValue: String): TXmlNode; overload;
    // Return a list of childodes with given Name
    function FindNodes(const Name: String): TXmlNodeList; virtual;

    // Return a list of childodes with given Name
    function GetNodes(const Name: String): TXmlNodeList; virtual;

    // Returns True if the Attribute exits
    function HasAttribute(const AttrName: String): Boolean; virtual;
    // Returns True if this child nodes exists
    function HasChild(const Name: String): Boolean; virtual;
    // Add a child node and return it
    function AddChild(const Name: String): TXmlNode; virtual;
    function InsertChild(const Name: String; Position: Integer): TXmlNode; virtual;
    function SetText(const Value: String): TXmlNode; virtual;
    function SetAttribute(const AttrName, AttrValue: String): TXmlNode; virtual;
    function FirstChild: TXmlNode;
    function LastChild: TXmlNode;
    function NextSibling: TXmlNode; overload;
    function PreviousSibling: TXmlNode; overload;
    function HasChildNodes: Boolean;
    function SetNodeType(Value: TXmlNodeType): TXmlNode;

    // Attributes of a Node, accessible by attribute name
    property Attributes[const AttrName: String]: String read GetAttr write SetAttr;

    property NodeName: String read Name write Name;
    property NodeValue: String read Text write Text;
  end;

  TXmlNodeList = class(TObjectList<TXmlNode>)
  public
    Node: TXmlNode;
    // Find a node by its name
    function Find(const Name: String): TXmlNode; overload;
    function FindNode(const Name: String): TXmlNode;
    // Find a node by Name/Attribute
    function Find(const Name, AttrName: String): TXmlNode; overload;
    // Find a node by Name/Attribute/Value
    function Find(const Name, AttrName, AttrValue: String): TXmlNode; overload;
    // Return a list of childodes with given Name
    function FindNodes(const Name: String): TXmlNodeList; virtual;

    // Return a list of childodes with given Name
    function GetNodes(const Name: String): TXmlNodeList; virtual;

    function HasNode(Name: String): Boolean; virtual;
    function Insert(const Name: String; Position: Integer): TXmlNode; overload;
    function FirstChild: TXmlNode;
    function NextSibling(Node: TXmlNode): TXmlNode;
    function PreviousSibling(Node: TXmlNode): TXmlNode;
    function Add(Value: TXmlNode): Integer; overload;
  end;

  TXmlVerySimple = class(TObject)
  private
    Root: TXmlNode;
    FHeader: TXmlNode;
    FDocumentElement: TXmlNode;
    procedure Parse(Reader: TXmlStreamReader);
    procedure ParseComment(Reader: TXmlStreamReader; var Parent: TXmlNode);
    procedure ParseDocType(Reader: TXmlStreamReader; var Parent: TXmlNode);
    procedure ParseProcessingInstr(Reader: TXmlStreamReader; var Parent: TXmlNode);
    procedure ParseText(const Line: String; Parent: TXmlNode);
    function ParseTag(Reader: TXmlStreamReader; ParseText: Boolean; var Parent: TXmlNode): TXmlNode;
    procedure Walk(Writer: TStreamWriter; const Prefix: String; Node: TXmlNode);
    procedure SetText(const Value: String);
    function GetText: String;
    procedure SetEncoding(const Value: String);
    function GetEncoding: String;
    procedure SetVersion(const Value: String);
    function GetVersion: String;
    procedure Compose(Writer: TStreamWriter);
    procedure SetStandAlone(const Value: String);
    function GetStandAlone: String;
    function GetChildNodes: TXmlNodeList;
    procedure CreateHeaderNode;
    function ExtractText(var Line: String; StopChars: String; Options: TExtractTextOptions): String;
  protected
  public
    NodeIndentStr: String;
    LineBreak: String;  // Default: sLineBreak (OS dependent)
    Options: TXmlOptions;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    function AddChild(const Name: String): TXmlNode; virtual;
    procedure LoadFromFile(const FileName: String); virtual;  // Load XML from a file
    procedure LoadFromStream(const Stream: TStream); virtual; // Load XML for a stream
    procedure SaveToStream(const Stream: TStream); virtual; // Encoding is specified in Header-Node
    procedure SaveToFile(const FileName: String); virtual;
    property Header: TXmlNode read FHeader; // XML declarations are stored in here as Attributes
    property DocumentElement: TXmlNode read FDocumentElement; // There is only one Root Node
    property ChildNodes: TXmlNodeList read GetChildNodes;
    property Encoding: String read GetEncoding write SetEncoding;
    property Version: String read GetVersion write SetVersion;
    property StandAlone: String read GetStandAlone write SetStandAlone;
    property Xml: String read GetText write SetText;
    property Text: String read GetText write SetText;
  end;

implementation

uses
  StrUtils;

const
{$IF CompilerVersion >= 24} // Delphi XE3 and up can use Low() and High()
  LowStr = Low(String); // Get string index base, may be 0 (NextGen compiler) or 1 (standard compiler)
{$ELSE} // For any previous Delphi version overwrite High() function and use 1 as string index base
  LowStr = 1;  // Use 1 as string index base

function High(const Value: String): Integer; inline;
begin
  Result := Length(Value);
end;
{$IFEND}

{$IF CompilerVersion < 22}  //Delphi XE added PosEx as an overload function)
function Pos(const SubStr, S: string; Offset: Integer): Integer; overload; Inline;
begin
  Result := PosEx(SubStr, S, Offset);
end;
{$IFEND}


{ TVerySimpleXml }

function TXmlVerySimple.AddChild(const Name: String): TXmlNode;
begin
  Result := TXmlNode.Create;
  if not assigned(FDocumentElement) then
    FDocumentElement := Result;

  Result.Name := Name;
  Root.ChildNodes.Add(Result);
end;

procedure TXmlVerySimple.Clear;
begin
  Root.Clear;
  FDocumentElement := NIL;
  FHeader := NIL;
end;


constructor TXmlVerySimple.Create;
begin
  inherited;
  Root := TXmlNode.Create;
  Root.Parent := Root;
  NodeIndentStr := '  ';
  Options := [doNodeAutoIndent];
  LineBreak := sLineBreak;
end;

procedure TXmlVerySimple.CreateHeaderNode;
begin
  if assigned(FHeader) then
    Exit;
  FHeader := TXmlNode.Create;
  FHeader.Name := '?xml';
  FHeader.Attributes['version'] := '1.0';  // Default XML version
  FHeader.Attributes['encoding'] := 'utf-8';
  FHeader.NodeType := ntXmlDecl;
  Root.ChildNodes.Insert(0, FHeader);
end;

destructor TXmlVerySimple.Destroy;
begin
  Root.Free;
  inherited;
end;

function TXmlVerySimple.GetChildNodes: TXmlNodeList;
begin
  Result := Root.ChildNodes;
end;

function TXmlVerySimple.GetEncoding: String;
begin
  if assigned(FHeader) then
    Result := FHeader.Attributes['encoding']
  else
    Result := '';
end;

function TXmlVerySimple.GetStandAlone: String;
begin
  if assigned(FHeader) then
    Result := FHeader.Attributes['standalone']
  else
    Result := '';
end;

function TXmlVerySimple.GetVersion: String;
begin
  if assigned(FHeader) then
    Result := FHeader.Attributes['version']
  else
    Result := '';
end;

function TXmlVerySimple.GetText: String;
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create;
  try
    SaveToStream(Stream);
    Result := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

procedure TXmlVerySimple.Compose(Writer: TStreamWriter);
var
  Child: TXmlNode;
begin
  if doCompact in Options then
    Writer.NewLine := ''
  else
    Writer.NewLine := LineBreak;

  for Child in Root.ChildNodes do
    Walk(Writer, '', Child);
end;

procedure TXmlVerySimple.LoadFromFile(const FileName: String);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead + fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TXmlVerySimple.LoadFromStream(const Stream: TStream);
var
  Reader: TXmlStreamReader;
begin
  if Encoding = '' then // none specified then use UTF8 with DetectBom
    Reader := TXmlStreamReader.Create(Stream, True)
  else
  if AnsiSameText(Encoding, 'utf-8') then
    Reader := TXmlStreamReader.Create(Stream, TEncoding.UTF8)
  else
    Reader := TXmlStreamReader.Create(Stream, TEncoding.ANSI);
  try
    Reader.LineBreak := LineBreak;
    Parse(Reader);
  finally
    Reader.Free;
  end;
end;

procedure TXmlVerySimple.Parse(Reader: TXmlStreamReader);
var
  Parent, Node: TXmlNode;
  FistChar: Char;
  ALine: String;
begin
  Clear;
  Parent := Root;

  while not Reader.EndOfStream do
  begin
    Reader.ReadLine;
    while not Reader.EndOfLine do
    begin
      ALine := Reader.ExtractText('<', [etoDeleteStopChar]);
      if ALine <> '' then  // Check for text nodes
        ParseText(Aline, Parent);

      FistChar := Reader.Line[Reader.LinePos];
      if FistChar = '!' then
        if Reader.IsUppercaseText('!--') then  // check for a comment node
          ParseComment(Reader, Parent)
        else
        if Reader.IsUppercaseText('!DOCTYPE') then // check for a doctype node
          ParseDocType(Reader, Parent)
        else
          ParseTag(Reader, False, Parent) // try to parse as tag
      else // Check for XML header / processing instructions
      if FistChar = '?' then // could be header or processing instruction
        if Reader.IsUppercaseText('?XML') then
        begin
          FHeader := ParseTag(Reader, False, Parent);
          Parent := FHeader.Parent;
          FHeader.NodeType := ntXmlDecl;
          FHeader.Name := '?xml';
          if (FHeader.AttributeList.Count > 0) and (FHeader.AttributeList.Last.Name='?') then
            FHeader.AttributeList.Delete(FHeader.AttributeList.Count - 1);
        end
        else
          ParseProcessingInstr(Reader, Parent)// it is a processing instruction
      else
      begin // Parse a tag, the first tag in a document is the DocumentElement
        Node := ParseTag(Reader, True, Parent);
        if (not assigned(FDocumentElement)) and (Parent = Root) then
          FDocumentElement := Node;
      end;
    end;
  end;
end;

procedure TXmlVerySimple.ParseText(const Line: String; Parent: TXmlNode);
var
  SingleChar: Char;
  Node: TXmlNode;
begin
  for SingleChar in Line do
    if not CharInSet(SingleChar, TXmlSpaces) then
    begin
      Node := TXmlNode.Create;
      Node.NodeType := ntText;
      Node.Text := Line;
      Parent.ChildNodes.Add(Node);
      Exit;
    end;
end;

procedure TXmlVerySimple.ParseComment(Reader: TXmlStreamReader; var Parent: TXmlNode);
var
  Node: TXmlNode;
begin
  Node := TXmlNode.Create;
  Node.NodeType := ntComment;
  Parent.ChildNodes.Add(Node);
  Node.Text := Reader.ExtractText('-->', [etoDeleteStopChar, etoStopString]);
end;

procedure TXmlVerySimple.ParseDocType(Reader: TXmlStreamReader; var Parent: TXmlNode);
var
  Node: TXmlNode;
  Quote: Char;
begin
  Node := TXmlNode.Create;
  Node.NodeType := ntDocType;
  Parent.ChildNodes.Add(Node);
  Node.Text := Reader.ExtractText('>[', []);
  if not Reader.EndOfLine then
  begin
    Quote := Reader.Line[Reader.LinePos];
    Reader.IncLinePos;
    if Quote = '[' then
      Node.Text := Node.Text + Quote + Reader.ExtractText(']',[etoDeleteStopChar]) + ']' +
        Reader.ExtractText('>', [etoDeleteStopChar]);
  end;
end;

procedure TXmlVerySimple.ParseProcessingInstr(Reader: TXmlStreamReader; var Parent: TXmlNode);
var
  Node: TXmlNode;
begin
  Node := TXmlNode.Create;
  Node.NodeType := ntProcessingInstr;
  Parent.ChildNodes.Add(Node);
  Reader.IncLinePos;
  Node.Text := Reader.ExtractText('?>', [etoDeleteStopChar, etoStopString]);
end;

function TXmlVerySimple.ParseTag(Reader: TXmlStreamReader; ParseText: Boolean; var Parent: TXmlNode): TXmlNode;
var
  Tag: String;
  Node: TXmlNode;
  Attribute: TXmlAttribute;
  ALine, Attr, AttrText: String;
  CharPos: Integer;
  Text: String;
  SingleChar: Char;
begin
  Tag := Reader.ExtractText('>', [etoDeleteStopChar]);

  // A closing tag does not have any attributes nor text
  if (Tag <> '') and (Tag[LowStr] = '/') then
  begin
    Result := Parent;
    Parent := Parent.Parent;
    Exit;
  end;

  // Creat a new new ntElement node
  Node := TXmlNode.Create;
  Parent.ChildNodes.Add(Node);
  Result := Node;

  // Check for a self-closing Tag (does not have any text)
  if (Tag <> '') and (Tag[High(Tag)] = '/') then
  begin
    ParseText := False;
    Delete(Tag, High(Tag), 1);
  end
  else
    Parent := Node;

  CharPos := Pos(' ', Tag);
  if CharPos >= LowStr then // Tag name has attributes
  begin
    ALine := Tag;
    Delete(Tag, CharPos, Length(Tag));
    Delete(ALine, LowStr, CharPos);

    while ALine <> '' do
    begin
      Attr := TrimLeft(ExtractText(ALine, '=', [etoDeleteStopChar])); // Get Attribute Name

      while (Attr <> '') and (Attr[High(Attr)]=' ') do  // if a space before equal sign then delete it
        Delete(Attr, High(Attr), 1);

      Attribute := TXmlAttribute.Create;
      Attribute.Name := Attr;
      ExtractText(ALine, '''' + '"', []);
      ALine := TrimLeft(ALine);
      if ALine <> '' then
      begin
        Attribute.Quote := ALine[LowStr];
        Delete(ALine, LowStr, 1);
        AttrText := ExtractText(ALine, Attribute.Quote, [etoDeleteStopChar]); // Get Attribute Value
        if Attribute.Quote = '"' then
          Attribute.Value := ReplaceStr(AttrText, '&quot;', '"')
        else
          Attribute.Value := ReplaceStr(AttrText, '&apos;', '''');
      end;
      Node.AttributeList.Add(Attribute);
    end;
  end;

  Node.Name := Tag;

  if ParseText then
  begin
    Text := Reader.ExtractText('<', []);
    Text := ReplaceStr(Text, '&amp;', '&');
    Text := ReplaceStr(Text, '&lt;', '<');
    for SingleChar in Text do
      if not CharInSet(SingleChar, TXmlSpaces) then
      begin
        Node.Text := Text;
        Break;
      end;
  end;
end;

procedure TXmlVerySimple.SaveToFile(const FileName: String);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TXmlVerySimple.SaveToStream(const Stream: TStream);
var
  Writer: TStreamWriter;
begin
  if AnsiSameText(Self.Encoding, 'utf-8') then
    Writer := TStreamWriter.Create(Stream, TEncoding.UTF8)
  else
    Writer := TStreamWriter.Create(Stream, TEncoding.ANSI);
  try
    Compose(Writer);
  finally
    Writer.Free;
  end;
end;

procedure TXmlVerySimple.SetEncoding(const Value: String);
begin
  CreateHeaderNode;
  FHeader.Attributes['encoding'] := Value;
end;

procedure TXmlVerySimple.SetStandAlone(const Value: String);
begin
  CreateHeaderNode;
  FHeader.Attributes['standalone'] := Value;
end;

procedure TXmlVerySimple.SetVersion(const Value: String);
begin
  CreateHeaderNode;
  FHeader.Attributes['version'] := Value;
end;

procedure TXmlVerySimple.SetText(const Value: String);
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create;
  try
    Stream.WriteString(Value);
    Stream.Position := 0;
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TXmlVerySimple.Walk(Writer: TStreamWriter; const Prefix: String; Node: TXmlNode);
var
  Child: TXmlNode;
  Attribute: TXmlAttribute;
  S, Text: String;
  TempIdent: String;
begin
  S := Prefix + '<';
  case Node.NodeType of
    ntComment:
      begin
        Writer.WriteLine(S + '!--' + Node.Text + '-->');
        Exit;
      end;
    ntDocType:
      begin
        Writer.WriteLine(S + '!DOCTYPE ' + Node.Text + '>');
        Exit;
      end;
    ntProcessingInstr:
      begin
        Writer.WriteLine(S + '?' + Node.Text + '?>');
        Exit;
      end;
    ntText:
      begin
        Writer.Write(Node.Text);
        Exit;
      end;
  end;

  S := S + Node.Name;
  for Attribute in Node.AttributeList do
  begin
    S := S + ' ' + Attribute.Name + '=' + Attribute.Quote;
    if Attribute.Quote = '"' then
      S := S + ReplaceStr(Attribute.Value, '"', '&quot;') + Attribute.Quote
    else
      S := S + ReplaceStr(Attribute.Value, '''', '&apos;') + Attribute.Quote;
  end;

  if Node = FHeader then // the Header node doesn't have any child nodes
  begin
    Writer.WriteLine(S + '?>');
    Exit;
  end;

  // Self closing tags
  if (Node.Text = '') and (not Node.HasChildNodes) then
   begin
    Writer.WriteLine(S + ' />');
    Exit;
  end;

  S := S + '>';
  if Node.Text <> '' then
  begin
    Text := ReplaceStr(Node.Text, '&', '&amp;');
    Text := ReplaceStr(Text, '<', '&lt;');
    S := S + Text;
  end;

  if (not Node.HasChildNodes) and (Node.Text <> '') and ((Node.Parent <> Root) or (Node = FDocumentElement)) then
  begin
    S := S + '</' + Node.Name + '>';
    Writer.WriteLine(S);
  end
  else
  begin
    Writer.WriteLine(S);
    if doCompact in Options then
      TempIdent := ''
    else
      TempIdent := NodeIndentStr;

    for Child in Node.ChildNodes do
      Walk(Writer, Prefix + TempIdent, Child);

    Writer.WriteLine(Prefix + '</' + Node.Name + '>');
  end;
end;

function TXmlVerySimple.ExtractText(var Line: String; StopChars: String;
  Options: TExtractTextOptions): String;
var
  CharPos, FoundPos: Integer;
  TestChar: Char;
begin
  FoundPos := -1;
  for TestChar in StopChars do
  begin
    CharPos := Pos(TestChar, Line);
    if (CharPos >= LowStr) and ((FoundPos = -1) or (CharPos < FoundPos)) then
      FoundPos := CharPos;
  end;

  if FoundPos <> -1 then
  begin
    Result := Copy(Line, LowStr, FoundPos - LowStr);
    if etoDeleteStopChar in Options then
      Inc(FoundPos);
    Delete(Line, LowStr, FoundPos - LowStr);
  end
  else
  begin
    Result := Line;
    Line := '';
  end;
end;

{ TXmlNode }

function TXmlNode.AddChild(const Name: String): TXmlNode;
begin
  Result := TXmlNode.Create;
  try
    Result.Name := Name;
    ChildNodes.Add(Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TXmlNode.Clear;
begin
  Text := '';
  AttributeList.Clear;
  ChildNodes.Clear;
end;

constructor TXmlNode.Create;
begin
  ChildNodes := TXmlNodeList.Create;
  ChildNodes.Node := Self;
  AttributeList := TXmlAttributeList.Create;
  NodeType := ntElement;
end;

destructor TXmlNode.Destroy;
begin
  if assigned(Parent) then
    Parent.ChildNodes.Remove(Self);
  AttributeList.Free;
  ChildNodes.Free;
  inherited;
end;

function TXmlNode.Find(const Name: String): TXmlNode;
begin
  Result := ChildNodes.Find(Name);
end;

function TXmlNode.Find(const Name, AttrName, AttrValue: String): TXmlNode;
begin
  Result := ChildNodes.Find(Name, AttrName, AttrValue);
end;

function TXmlNode.Find(const Name, AttrName: String): TXmlNode;
begin
  Result := ChildNodes.Find(Name, AttrName);
end;

function TXmlNode.FindNodes(const Name: String): TXmlNodeList;
begin
  Result := ChildNodes.FindNodes(Name);
end;

function TXmlNode.FirstChild: TXmlNode;
begin
  Result := ChildNodes.First;
end;

function TXmlNode.GetNodes(const Name: String): TXmlNodeList;
begin
  Result := ChildNodes.GetNodes(Name);
end;

function TXmlNode.GetAttr(const AttrName: String): String;
var
  Attribute: TXmlAttribute;
begin
  Attribute := AttributeList.Find(AttrName);
  if Assigned(Attribute) then
    Result := Attribute.Value
  else
    Result := '';
end;

function TXmlNode.HasAttribute(const AttrName: String): Boolean;
begin
  Result := AttributeList.HasAttribute(AttrName);
end;

function TXmlNode.HasChild(const Name: String): Boolean;
begin
  Result := ChildNodes.HasNode(Name);
end;

function TXmlNode.HasChildNodes: Boolean;
begin
  Result := (ChildNodes.Count > 0);
end;

function TXmlNode.InsertChild(const Name: String; Position: Integer): TXmlNode;
begin
  Result := ChildNodes.Insert(Name, Position);
  if assigned(Result) then
    Result.Parent := Self;
end;

function TXmlNode.LastChild: TXmlNode;
begin
  Result := ChildNodes.Last;
end;

function TXmlNode.NextSibling: TXmlNode;
begin
  if not assigned(Parent) then
    Result := NIL
  else
    Result := Parent.ChildNodes.NextSibling(Self);
end;

function TXmlNode.PreviousSibling: TXmlNode;
begin
  if not assigned(Parent) then
    Result := NIL
  else
    Result := Parent.ChildNodes.PreviousSibling(Self);
end;

procedure TXmlNode.SetAttr(const AttrName, AttrValue: String);
begin
  SetAttribute(AttrName, AttrValue);
end;

function TXmlNode.SetAttribute(const AttrName, AttrValue: String): TXmlNode;
var
  Attribute: TXmlAttribute;
begin
  Attribute := AttributeList.Find(AttrName); // Search for given name
  if not assigned(Attribute) then // If attribute is not found, create one
  begin
    Attribute := TXmlAttribute.Create;
    AttributeList.Add(Attribute);
  end;
  Attribute.Name := AttrName; // this allows rewriting of the attribute name
  Attribute.Value := AttrValue;
  Result := Self;
end;

function TXmlNode.SetNodeType(Value: TXmlNodeType): TXmlNode;
begin
  NodeType := Value;
  Result := Self;
end;

function TXmlNode.SetText(const Value: String): TXmlNode;
begin
  Text := Value;
  Result := Self;
end;

{ TXmlAttributeList }

procedure TXmlAttributeList.Delete(const Name: String);
var
  Attribute: TXmlAttribute;
begin
  Attribute := Find(Name);
  if Assigned(Attribute) then
    Remove(Attribute);
end;

function TXmlAttributeList.Find(const Name: String): TXmlAttribute;
var
  Attribute: TXmlAttribute;
begin
  Result := NIL;
  for Attribute in Self do
    if AnsiSameText(Attribute.Name, Name) then
    begin
      Result := Attribute;
      Break;
    end;
end;

function TXmlAttributeList.HasAttribute(const AttrName: String): Boolean;
begin
  Result := Assigned(Find(AttrName));
end;

{ TXmlNodeList }

function TXmlNodeList.Find(const Name: String): TXmlNode;
var
  Node: TXmlNode;
begin
  Result := NIL;
  for Node in Self do
    if AnsiSameText(Node.Name, Name) then
    begin
      Result := Node;
      Break;
    end;
end;

function TXmlNodeList.Add(Value: TXmlNode): Integer;
begin
  Result := inherited Add(Value);
  Value.Parent := Node;
end;

function TXmlNodeList.Find(const Name, AttrName, AttrValue: String): TXmlNode;
var
  Node: TXmlNode;
begin
  Result := NIL;
  for Node in Self do
    if (AnsiSameText(Node.Name, Name)) and (Node.HasAttribute(AttrName)) and
      (AnsiSameStr(Node.Attributes[AttrName], AttrValue)) then
    begin
      Result := Node;
      Break;
    end;
end;

function TXmlNodeList.Find(const Name, AttrName: String): TXmlNode;
var
  Node: TXmlNode;
begin
  Result := NIL;
  for Node in Self do
    if AnsiSameText(Node.Name, Name) and Node.HasAttribute(AttrName) then
    begin
      Result := Node;
      Break;
    end;
end;


function TXmlNodeList.FindNode(const Name: String): TXmlNode;
begin
  Result := Find(Name);
end;

function TXmlNodeList.FindNodes(const Name: String): TXmlNodeList;
var
  Node: TXmlNode;
begin
  Result := TXmlNodeList.Create(False);
  for Node in Self do
    if AnsiSameText(Node.Name, Name) then
      Result.Add(Node);
end;

function TXmlNodeList.FirstChild: TXmlNode;
begin
  Result := First;
end;

function TXmlNodeList.GetNodes(const Name: String): TXmlNodeList;
var
  Node: TXmlNode;
begin
  Result := TXmlNodeList.Create(False);
  try
  for Node in Self do
    if AnsiSameText(Node.Name, Name) then
      Result.Add(Node);
  except
    Result.Free;
    raise;
  end;
end;

function TXmlNodeList.HasNode(Name: String): Boolean;
begin
  Result := Assigned(Find(Name));
end;

function TXmlNodeList.Insert(const Name: String; Position: Integer): TXmlNode;
begin
  Result := TXmlNode.Create;
  try
    Result.Name := Name;
    Insert(Position, Result);
  except
    Result.Free;
    raise;
  end;
end;

function TXmlNodeList.NextSibling(Node: TXmlNode): TXmlNode;
var
  Index: Integer;
begin
  if (not assigned(Node)) and (Count > 0) then
    Result := First
  else
  begin
    Index := Self.IndexOf(Node);
    if (Index >= 0) and (Index + 1 < Count) then
      Result := Self[Index]
    else
      Result := NIL;
  end;
end;

function TXmlNodeList.PreviousSibling(Node: TXmlNode): TXmlNode;
var
  Index: Integer;
begin
  Index := Self.IndexOf(Node);
  if Index - 1 >= 0 then
    Result := Self[Index]
  else
    Result := NIL;
end;

{ TXmlStreamReader }

procedure TXmlStreamReader.IncLinePos(Value: Integer);
begin
  Inc(LinePos, Value);
  EndOfLine := (LinePos > LineLength);
end;

function TXmlStreamReader.IsUppercaseText(Value: String): Boolean;
begin
  Result := (Uppercase(copy(Line, LinePos, Length(Value))) = Value);
  if Result then
    IncLinePos(Length(Value));
end;


function TXmlStreamReader.ExtractText(StopChars: String;
  Options: TExtractTextOptions): String;
var
  CharPos, FoundPos: Integer;
  StopChar: Char;
  IncPos: Integer;
begin
  Result := '';
  repeat
    if not EndOfLine then
    begin
      FoundPos := LowStr - 1;
      if etoStopString in Options then
        FoundPos := Pos(StopChars, Line, LinePos)
      else
        for StopChar in StopChars do
        begin
          CharPos := Pos(StopChar, Line, LinePos);
          if (CharPos >= LowStr) and ((FoundPos = LowStr-1) or (CharPos < FoundPos)) then
            FoundPos := CharPos;
        end;

      if FoundPos <> LowStr-1 then
      begin
        IncPos := FoundPos-LinePos;
        Result := Result + Copy(Line, LinePos, IncPos);
        if etoDeleteStopChar in Options then
          if etoStopString in Options then
            Inc(IncPos, Length(StopChars))
          else
           Inc(IncPos);
        IncLinePos(IncPos);
        Exit;
      end;

      Result := Result + Copy(Line, LinePos);
    end;

    if EndOfStream then
      Exit;
    Line := ReadLine;
    Result := Result + LineBreak;
  until False;
end;

function TXmlStreamReader.ReadLine: String;
begin
  Result := inherited;
  Line := Result;
  LinePos := LowStr;
  LineLength := High(Line);
  IncLinePos(0);
end;

{ TXmlAttribute }

constructor TXmlAttribute.Create;
begin
  Quote := '"';  // Default attribute quotating mark
end;

end.
