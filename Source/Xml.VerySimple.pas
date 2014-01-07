{ VerySimpleXML v1.5 - a lightweight, one-unit, cross-platform XML reader/writer
  for Delphi 2009-XE5 by Dennis Spreen
  http://blog.spreendigital.de/2011/11/10/verysimplexml-a-lightweight-delphi-xml-reader-and-writer/

  1.0 Initial release
  1.1 Removed "extended" quotation marks support, Renamed to XmlVerySimple
  1.2 Code optimizations
  1.3 LoadFromFile/Stream now checks if header is UTF8
  1.4 Removed ' from node attributes
  1.5 Replaced string access with High/Low(string) for NextGen compiler compatibility

      XmlNode:
        Attribute value is now escaped/unescaped
        Added Name (same as NodeName)
        Moved 'find' procedures over to TXmlNodeList

      Compact the XML now by using Options := [doCompact]

      Added TXmlDocument compatible functions:
        NodeValue (same as Text), NodeName (same as Name), Encoding (same as Header.Attribute['encoding']),
        Version (same as Header.Attribute['version']), Options (the only compatible option is [doAutoIdent])
        Xml.AddChild (replaces root node name), Xml.DocumentElement (same as root), Node.FirstChild (same as
        Node.ChildNodes.First), Node.NextSibling, Node.PreviousSibling

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
  Classes, Generics.Defaults, Generics.Collections;

type
  TXmlNodeList = class;
  TXmlNode = class;
  TXmlNodeArray = TArray<TXmlNode>;

  TXmlOption = (doNodeAutoIndent, doCompact);
  TxmlOptions = set of TXmlOption;

  TXmlAttribute = class(TObject)
  public
    Name: String; // Attribute name
    Value: String; // Attribute value (always as String)
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
    Attributes: TXmlAttributeList;
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
    function FindNodes(const Name: String): TXmlNodeArray; virtual;

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
    function NextSibling: TXmlNode;
    function PreviousSibling: TXmlNode;

    // Attributes of a Node, accessible by attribute name
    property Attribute[const AttrName: String]: String read GetAttr write SetAttr;

    property NodeName: String read Name write Name;
    property NodeValue: String read Text write Text;
  end;

  TXmlNodeList = class(TObjectList<TXmlNode>)
  public
    // Find a node by its name
    function Find(const Name: String): TXmlNode; overload;
    function FindNode(const Name: String): TXmlNode;
    // Find a node by Name/Attribute
    function Find(const Name, AttrName: String): TXmlNode; overload;
    // Find a node by Name/Attribute/Value
    function Find(const Name, AttrName, AttrValue: String): TXmlNode; overload;
    // Return a list of childodes with given Name
    function FindNodes(const Name: String): TXmlNodeArray; virtual;

    // Return a list of childodes with given Name
    function GetNodes(const Name: String): TXmlNodeList; virtual;

    function HasNode(Name: String): Boolean; virtual;
    function Insert(const Name: String; Position: Integer): TXmlNode; overload;
    function FirstChild: TXmlNode;
    function NextSibling(Node: TXmlNode): TXmlNode;
    function PreviousSibling(Node: TXmlNode): TXmlNode;
  end;

  TXmlVerySimple = class(TObject)
  private
    procedure Parse(Lines: TStringList);
    procedure Walk(Lines: TStringList; const Prefix: String; Node: TXmlNode);
    procedure SetText(const Value: String);
    function GetText: String;
    procedure SetEncoding(const Value: String);
    function GetEncoding: String;
    procedure SetVersion(const Value: String);
    function GetVersion: String;
  protected
  public
    Root: TXmlNode; // There is only one Root Node
    Header: TXmlNode; // XML declarations are stored in here as Attributes
    Ident: String; // Set Ident:='' if you want a compact output
    Options: TXmlOptions;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    function AddChild(const Name: String): TXmlNode; virtual;
    procedure LoadFromFile(const FileName: String); virtual;  // Load XML from a file
    procedure LoadFromStream(const Stream: TStream); virtual; // Load XML for a stream
    procedure SaveToStream(const Stream: TStream); virtual; // Encoding is specified in Header-Node
    procedure SaveToFile(const FileName: String); virtual;
    class function Escape(const Value: String): String; virtual; // Convert special chars into escaped chars
    class function UnEscape(const Value: String): String; virtual;  // Convert escaped chars into special chars
    property Text: String read GetText write SetText;
    property Encoding: String read GetEncoding write SetEncoding;
    property Version: String read GetVersion write SetVersion;
    property DocumentElement: TXmlNode read Root;
  end;

implementation

uses
  SysUtils, StrUtils;

{ TVerySimpleXml }

function TXmlVerySimple.AddChild(const Name: String): TXmlNode;
begin
  Root.Name := Name;
  Result := Root;
end;

procedure TXmlVerySimple.Clear;
begin
  Root.Clear;
  Root.Name := '';
  Header.Clear;
end;

constructor TXmlVerySimple.Create;
begin
  inherited;
  Root := TXmlNode.Create;
  Header := TXmlNode.Create;
  Ident := '  ';
  Header.Name := '?xml'; // Default XML Header
  Version := '1.0'; // Default XML Version
  Options := [doNodeAutoIndent];
  Clear;
end;

destructor TXmlVerySimple.Destroy;
begin
  Root.Free;
  Header.Free;
  inherited;
end;

class function TXmlVerySimple.Escape(const Value: String): String;
begin
  Result := ReplaceStr(Value, '&', '&amp;');
  Result := ReplaceStr(Result, '<', '&lt;');
  Result := ReplaceStr(Result, '>', '&gt;');
  Result := ReplaceStr(Result, '''', '&apos;');
  Result := ReplaceStr(Result, '"', '&quot;');
end;

function TXmlVerySimple.GetEncoding: String;
begin
  Result := Header.Attribute['encoding'];
end;

function TXmlVerySimple.GetText: String;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    if doCompact in Options then
      Lines.LineBreak := '';

    // Create XML introduction
    Walk(Lines, '', Header);

    // Create nodes representation
    Walk(Lines, '', Root);
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;


function TXmlVerySimple.GetVersion: String;
begin
  Result := Header.Attribute['version'];
end;

procedure TXmlVerySimple.LoadFromFile(const FileName: String);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TXmlVerySimple.LoadFromStream(const Stream: TStream);
var
  Lines: TStringList;
  Encoding: TEncoding;
begin
  Lines := TStringList.Create;
  try
    if MatchText(Self.Encoding, ['utf-8', '']) then
      Encoding := TEncoding.UTF8
    else
      Encoding := TEncoding.Default;
    Clear;
    Lines.LoadFromStream(Stream, Encoding);
    Parse(Lines);
  finally
    Lines.Free;
  end;
end;

procedure TXmlVerySimple.Parse(Lines: TStringList);
var
  Line: String;
  IsTag, IsText: Boolean;
  Tag, Text: String;
  Parent, Node: TXmlNode;
  I: Integer;
  Attribute: TXmlAttribute;
  ALine, Attr, AttrText: String;
  P: Integer;
  IsSelfClosing: Boolean;

  // Return a text ended by StopChar, <ul>respect quotation marks</ul>
  function GetText(var Line: String; StartStr: String; StopChar: Char; DeleteStopChar: Boolean): String;
  begin
    while Length(Line) > 0 do
    begin
      if (Line[Low(String)] = StopChar) then
      begin
        if DeleteStopChar then
          Delete(Line, Low(String), 1);
        Break;
      end;

      StartStr := StartStr + Line[Low(String)];
      Delete(Line, Low(String), 1);
    end;
    Result := StartStr;
  end;

begin
  if assigned(Root) then // Release previous nodes (if set)
    Root.Free;

  IsTag := False;
  IsText := False;
  Node := NIL;

  for I := 0 to Lines.Count - 1 do
  begin
    Line := Lines[I];

    while (Length(Line) > 0) do
    begin
      if (not IsTag) and (not IsText) then
      begin
        while (Length(Line) > 0) and (Line[Low(String)] <> '<') do
          Delete(Line, Low(String), 1);

        if Length(Line) > 0 then
        begin
          IsTag := True;
          Delete(Line, Low(String), 1); // Delete openining tag
          Tag := '';
        end;
      end;

      if IsTag then
      begin
        Tag := GetText(Line, Tag, '>', False);

        if (Length(Line) > 0) and (Line[Low(String)] = '>') then
        begin
          Delete(Line, 1, 1);
          IsTag := False;

          if (Length(Tag) > 0) and (Tag[Low(String)] = '/') then
            Node := Node.Parent
          else
          begin
            Parent := Node;
            IsText := True;

            Node := TXmlNode.Create;
            if lowercase(copy(Tag, Low(String), 4)) = '?xml' then
            // check for xml header
            begin
              Header.Free;
              Header := Node;
            end;

            // Self-Closing Tag
            if (Length(Tag) > 0) and (Tag[High(Tag)] = '/') then // Length(Tag)
            begin
              IsSelfClosing := True;
              Delete(Tag, High(Tag), 1); // Length(Tag)
            end
            else
              IsSelfClosing := False;

            P := Pos(' ', Tag);
            if P <> 0 then // Tag name has attributes
            begin
              ALine := Tag;
              Delete(Tag, P, Length(Tag));
              Delete(ALine, Low(String), P);

              while Length(ALine) > 0 do
              begin
                Attr := GetText(ALine, '', '=', True); // Get Attribute Name
                AttrText := GetText(ALine, '', ' ', True);
                // Get Attribute Value

                if Length(AttrText) > 0 then
                begin
                  if (AttrText[Low(String)] = '"') or (AttrText[Low(String)] = '''') then
                  // Remove start/end quotation marks
                  begin
                    Delete(AttrText, Low(String), 1);
                    if (AttrText[High(AttrText)] = '"') or ((AttrText[High(AttrText)] = '''')) then
                      Delete(AttrText, High(AttrText), 1);
                  end;
                end;

                // Header node (Attr='?') does not support Attributes
                if not((Node = Header) and (Attr = '?')) then
                begin
                  Attribute := TXmlAttribute.Create;
                  Attribute.Name := Attr;
                  Attribute.Value := UnEscape(AttrText);
                  Node.Attributes.Add(Attribute);
                end;
              end;
            end;

            Node.Name := Tag;
            Node.Parent := Parent;
            if assigned(Parent) then
              Parent.ChildNodes.Add(Node)
            else if Node = Header then
            begin
              IsText := False;
              Node := NIL;
            end
            else
              Root := Node;

            Text := '';
            if IsSelfClosing then
              Node := Node.Parent;
          end;
        end;
      end;

      if IsText then
      begin
        Text := GetText(Line, Text, '<', False);
        if (Length(Line) > 0) and (Line[Low(String)] = '<') then
        begin
          IsText := False;
          while (Length(Text) > 0) and (Text[Low(String)] = ' ') do
            Delete(Text, Low(String), 1);
          Node.Text := UnEscape(Text);
        end;
      end;

    end;
  end;
end;

procedure TXmlVerySimple.SaveToFile(const FileName: String);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  SaveToStream(Stream);
  Stream.Free;
end;

procedure TXmlVerySimple.SaveToStream(const Stream: TStream);
var
  Encoding: TEncoding;
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := GetText;
    if MatchText(Self.Encoding, ['utf-8']) then
      Encoding := TEncoding.UTF8
    else
      Encoding := TEncoding.Default;
    Lines.SaveToStream(Stream, Encoding);
  finally
    Lines.Free;
  end;
end;

procedure TXmlVerySimple.SetEncoding(const Value: String);
begin
  Header.Attribute['encoding'] := Value;
end;

procedure TXmlVerySimple.SetText(const Value: String);
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := Value;
    Parse(Lines);
  finally
    Lines.Free;
  end;
end;

procedure TXmlVerySimple.SetVersion(const Value: String);
begin
  Header.Attribute['version'] := Value;
end;

class function TXmlVerySimple.UnEscape(const Value: String): String;
begin
  Result := ReplaceStr(Value, '&lt;', '<');
  Result := ReplaceStr(Result, '&gt;', '>');
  Result := ReplaceStr(Result, '&quot;', '"');
  Result := ReplaceStr(Result, '&apos;', '''');
  Result := ReplaceStr(Result, '&amp;', '&');
end;

procedure TXmlVerySimple.Walk(Lines: TStringList; const Prefix: String; Node: TXmlNode);
var
  Child: TXmlNode;
  Attribute: TXmlAttribute;
  S: String;
  TempIdent: String;
begin
  S := Prefix + '<' + Node.Name;
  for Attribute in Node.Attributes do
    S := S + ' ' + Attribute.Name + '="' + Escape(Attribute.Value) + '"';

  if Node = Header then
  begin
    Lines.Add(S + ' ?>');
    Exit;
  end;

  // Self closing tags
  if (Node.Text = '') and (Node.ChildNodes.Count = 0) then
   begin
    Lines.Add(S + ' />');
    Exit;
  end;

  S := S + '>';
  if Node.Text <> '' then
    S := S + Escape(Node.Text);

  if (Node.ChildNodes.Count = 0) and (Node.Text <> '') then
  begin
    S := S + '</' + Node.Name + '>';
    Lines.Add(S);
  end
  else
  begin
    Lines.Add(S);
    if (doNodeAutoIndent in Options) and (not (doCompact in Options)) then
      TempIdent := Ident
    else
      TempIdent := '';
    for Child in Node.ChildNodes do
      Walk(Lines, Prefix + TempIdent, Child);
    Lines.Add(Prefix + '</' + Node.Name + '>');
  end;
end;

{ TXmlNode }

function TXmlNode.AddChild(const Name: String): TXmlNode;
begin
  Result := TXmlNode.Create;
  try
    Result.Name := Name;
    Result.Parent := Self;
    ChildNodes.Add(Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TXmlNode.Clear;
begin
  Text := '';
  Attributes.Clear;
  ChildNodes.Clear;
end;

constructor TXmlNode.Create;
begin
  ChildNodes := TXmlNodeList.Create;
  Attributes := TXmlAttributeList.Create;
end;

destructor TXmlNode.Destroy;
begin
  if assigned(Parent) then
    Parent.ChildNodes.Remove(Self);
  Attributes.Free;
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

function TXmlNode.FindNodes(const Name: String): TXmlNodeArray;
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
  Attribute := Attributes.Find(AttrName);
  if Assigned(Attribute) then
    Result := Attribute.Value
  else
    Result := '';
end;

function TXmlNode.HasAttribute(const AttrName: String): Boolean;
begin
  Result := Attributes.HasAttribute(AttrName);
end;

function TXmlNode.HasChild(const Name: String): Boolean;
begin
  Result := ChildNodes.HasNode(Name);
end;

function TXmlNode.InsertChild(const Name: String; Position: Integer): TXmlNode;
begin
  Result := ChildNodes.Insert(Name, Position);
  if assigned(Result) then
    Result.Parent := Self;
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
  Attribute := Attributes.Find(AttrName); // Search for given name
  if not assigned(Attribute) then // If attribute is not found, create one
  begin
    Attribute := TXmlAttribute.Create;
    Attributes.Add(Attribute);
  end;
  Attribute.Name := AttrName; // this allows rewriting of the attribute name
  Attribute.Value := AttrValue;
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

function TXmlNodeList.Find(const Name, AttrName, AttrValue: String): TXmlNode;
var
  Node: TXmlNode;
begin
  Result := NIL;
  for Node in Self do
    if (AnsiSameText(Node.Name, Name)) and (Node.HasAttribute(AttrName)) and
      (AnsiSameStr(Node.Attribute[AttrName], AttrValue)) then
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

function TXmlNodeList.FindNodes(const Name: String): TXmlNodeArray;
var
  Node: TXmlNode;
begin
  SetLength(Result, 0);
  for Node in Self do
    if AnsiSameText(Node.Name, Name) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := Node;
    end;
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
  Index := Self.IndexOf(Node);
  if (Index >= 0) and (Index + 1 < Count) then
    Result := Self[Index]
  else
    Result := NIL;
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

end.
