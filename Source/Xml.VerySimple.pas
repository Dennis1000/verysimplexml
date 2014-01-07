{ VerySimpleXML v1.4 - a lightweight, one-unit XML reader/writer
  by Dennis Spreen
  http://blog.spreendigital.de/2011/11/10/verysimplexml-a-lightweight-delphi-xml-reader-and-writer/

  1.3 - LoadFromFile/Stream now checks if header is UTF8
  - Removed "extended" quotation marks support

  1.4 Remove ' from node attributes
  Renamed to XmlVerySimple

  1.5 High/Low for String Index

  (c) Copyrights 2011 Dennis D. Spreen <dennis@spreendigital.de>
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

  TXmlAttribute = class(TObject)
  public
    Name: String; // Attribute name
    Value: String; // Attribute value (always as String)
  end;

  TXmlAttributeList = class(TObjectList<TXmlAttribute>)
  public
    function Find(AttrName: String): TXmlAttribute;
    // Find an Attribute by Name (not case sensitive)
  end;

  TXmlNode = class(TObject)
  private
    FAttributes: TXmlAttributeList;
    function GetAttribute(const AttrName: String): String;
    procedure SetAttr(const AttrName: String; const Value: String);
  public
    Parent: TXmlNode; // NIL only for Root-Node
    NodeName: String; // Node name
    ChildNodes: TXmlNodeList; // Child nodes, never NIL
    Text: String;
    constructor Create; virtual;
    destructor Destroy; override;
    // Find a childnode by its name
    function Find(Name: String): TXmlNode; overload;
    // Find a childnode by Name/Attribute
    function Find(Name, Attribute: String): TXmlNode; overload;
    // Find a childnode by Name/Attribute/Value
    function Find(Name, Attribute, Value: String): TXmlNode; overload;
    // Return a list of childodes with given Name
    function FindNodes(Name: String): TXmlNodeList; virtual;
    // Returns True if the Attribute exits
    function HasAttribute(const Name: String): Boolean; virtual;
    // Returns True if this child nodes exists
    function HasChild(const Name: String): Boolean; virtual;
    // Add a child node and return it
    function AddChild(const Name: String): TXmlNode; virtual;
    function InsertChild(const Name: String; Pos: Integer): TXmlNode; virtual;
    function SetText(Value: String): TXmlNode; virtual;
    function SetAttribute(const AttrName: String; const Value: String)
      : TXmlNode; virtual;
    property Attribute[const AttrName: String]: String read GetAttribute
      write SetAttr; // Attributes of a Node, accessible by attribute name
  end;

  TXmlNodeList = class(TObjectList<TXmlNode>);

  TXmlVerySimple = class(TObject)
  private
    Lines: TStringList;
    procedure Parse;
    procedure Walk(Lines: TStringList; Prefix: String; Node: TXmlNode);
    procedure SetText(Value: String);
    function GetText: String;
  public
    Root: TXmlNode; // There is only one Root Node
    Header: TXmlNode; // XML declarations are stored in here as Attributes
    Ident: String; // Set Ident:='' if you want a compact output
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    // Load XML from a file
    procedure LoadFromFile(const FileName: String); virtual;
    // Load XML for a stream
    procedure LoadFromStream(const Stream: TStream); virtual;
    // Encoding is specified in Header-Node
    procedure SaveToStream(const Stream: TStream); virtual;
    procedure SaveToFile(const FileName: String); virtual;
    class function Escape(Value: String): String;
    class function UnEscape(Value: String): String;
    property Text: String read GetText write SetText;
  end;

implementation

uses
  SysUtils, StrUtils;

{ TVerySimpleXml }

procedure TXmlVerySimple.Clear;
begin
  Root.Free;
  Header.Free;
  Root := TXmlNode.Create;
  Header := TXmlNode.Create;
  Header.NodeName := '?xml'; // Default XML Header
  Header.Attribute['version'] := '1.0'; // Default XML Version
  Ident := '  '; // Set Ident:='' if you want a compact output
  Lines.Clear;
end;

constructor TXmlVerySimple.Create;
begin
  inherited;
  Lines := TStringList.Create;
  Clear;
end;

destructor TXmlVerySimple.Destroy;
begin
  Root.Free;
  Header.Free;
  Lines.Free;
  inherited;
end;

class function TXmlVerySimple.Escape(Value: String): String;
begin
  Result := ReplaceStr(Value, '&', '&amp;');
  Result := ReplaceStr(Result, '<', '&lt;');
  Result := ReplaceStr(Result, '>', '&gt;');
  Result := ReplaceStr(Result, chr(39), '&apos;');
  Result := ReplaceStr(Result, '"', '&quot;');
end;

function TXmlVerySimple.GetText: String;
begin
  Lines.Clear;
  if Ident = '' then
    Lines.LineBreak := '';

  // Create XML introduction
  Walk(Lines, '', Header);

  // Create nodes representation
  Walk(Lines, '', Root);
  Result := Lines.Text;
end;

procedure TXmlVerySimple.LoadFromFile(const FileName: String);
var
  Utf8: Boolean;
begin
  Utf8 := (lowercase(Header.Attribute['encoding']) = 'utf-8');
  Clear;
  if Utf8 then
    Lines.LoadFromFile(FileName, TEncoding.Utf8)
  else
    Lines.LoadFromFile(FileName);
  Parse;
  Lines.Clear;
end;

procedure TXmlVerySimple.LoadFromStream(const Stream: TStream);
var
  Utf8: Boolean;
begin
  Utf8 := (lowercase(Header.Attribute['encoding']) = 'utf-8');
  Clear;
  if Utf8 then
    Lines.LoadFromStream(Stream, TEncoding.Utf8)
  else
    Lines.LoadFromStream(Stream);
  Parse;
  Lines.Clear;
end;

procedure TXmlVerySimple.Parse;
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
  function GetText(var Line: String; StartStr: String; StopChar: Char;
    DeleteStopChar: Boolean): String;
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
                  if (AttrText[Low(String)] = '"') or
                    (AttrText[Low(String)] = '''') then
                  // Remove start/end quotation marks
                  begin
                    Delete(AttrText, Low(String), 1);
                    if (AttrText[High(AttrText)] = '"') or
                      ((AttrText[High(AttrText)] = '''')) then
                      Delete(AttrText, High(AttrText), 1);
                  end;
                end;

                // Header node (Attr='?') does not support Attributes
                if not((Node = Header) and (Attr = '?')) then
                begin
                  Attribute := TXmlAttribute.Create;
                  Attribute.Name := Attr;
                  Attribute.Value := UnEscape(AttrText);
                  Node.FAttributes.Add(Attribute);
                end;
              end;
            end;

            Node.NodeName := Tag;
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
begin
  GetText;
  Encoding := TEncoding.Default;
  if lowercase(Header.Attribute['encoding']) = 'utf-8' then
    Encoding := TEncoding.Utf8;
  Lines.SaveToStream(Stream, Encoding);
  Lines.Clear;
end;

procedure TXmlVerySimple.SetText(Value: String);
begin
  Clear;
  Lines.Text := Value;
  Parse;
  Lines.Clear;
end;

class function TXmlVerySimple.UnEscape(Value: String): String;
begin
  Result := ReplaceStr(Value, '&lt;', '<');
  Result := ReplaceStr(Result, '&gt;', '>');
  Result := ReplaceStr(Result, '&apos;', chr(39));
  Result := ReplaceStr(Result, '&quot;', '"');
  Result := ReplaceStr(Result, '&amp;', '&');
end;

procedure TXmlVerySimple.Walk(Lines: TStringList; Prefix: String;
  Node: TXmlNode);
var
  Child: TXmlNode;
  Attribute: TXmlAttribute;
  OriginalPrefix: String;
  S: String;
  IsSelfClosing: Boolean;
begin
  S := Prefix + '<' + Node.NodeName;
  for Attribute in Node.FAttributes do
    S := S + ' ' + Attribute.Name + '="' + Escape(Attribute.Value) + '"';

  if Node = Header then
    S := S + ' ?';

  IsSelfClosing := (Length(Node.Text) = 0) and (Node.ChildNodes.Count = 0) and
    (Node <> Header);
  if IsSelfClosing then
    S := S + ' /';

  S := S + '>';
  if Length(Node.Text) > 0 then
    S := S + Escape(Node.Text);

  if (Node.ChildNodes.Count = 0) and (Length(Node.Text) > 0) then
  begin
    S := S + '</' + Node.NodeName + '>';
    Lines.Add(S);
  end
  else
  begin
    Lines.Add(S);
    OriginalPrefix := Prefix;
    Prefix := Prefix + Ident;
    for Child in Node.ChildNodes do
      Walk(Lines, Prefix, Child);
    if (Node <> Header) and (not IsSelfClosing) then
      Lines.Add(OriginalPrefix + '</' + Node.NodeName + '>');
  end;
end;

{ TXmlNode }

function TXmlNode.AddChild(const Name: String): TXmlNode;
begin
  Result := TXmlNode.Create;
  Result.NodeName := Name;
  Result.Parent := Self;
  ChildNodes.Add(Result);
end;

constructor TXmlNode.Create;
begin
  ChildNodes := TXmlNodeList.Create;
  Parent := NIL;
  FAttributes := TXmlAttributeList.Create;
end;

destructor TXmlNode.Destroy;
begin
  FAttributes.Free;
  ChildNodes.Free;
  inherited;
end;

function TXmlNode.Find(Name: String): TXmlNode;
var
  Node: TXmlNode;
begin
  Result := NIL;
  Name := lowercase(Name);
  for Node in ChildNodes do
    if lowercase(Node.NodeName) = Name then
    begin
      Result := Node;
      Break;
    end;
end;

function TXmlNode.Find(Name, Attribute, Value: String): TXmlNode;
var
  Node: TXmlNode;
begin
  Result := NIL;
  Name := lowercase(Name);
  for Node in ChildNodes do
    if (lowercase(Node.NodeName) = Name) and (Node.HasAttribute(Attribute)) and
      (Node.Attribute[Attribute] = Value) then
    begin
      Result := Node;
      Break;
    end;
end;

function TXmlNode.Find(Name, Attribute: String): TXmlNode;
var
  Node: TXmlNode;
begin
  Result := NIL;
  Name := lowercase(Name);
  for Node in ChildNodes do
    if (lowercase(Node.NodeName) = Name) and (Node.HasAttribute(Attribute)) then
    begin
      Result := Node;
      Break;
    end;
end;

function TXmlNode.FindNodes(Name: String): TXmlNodeList;
var
  Node: TXmlNode;
begin
  Result := TXmlNodeList.Create(False);
  Name := lowercase(Name);
  for Node in ChildNodes do
    if (lowercase(Node.NodeName) = Name) then
      Result.Add(Node);
end;

function TXmlNode.GetAttribute(const AttrName: String): String;
var
  Attribute: TXmlAttribute;
begin
  Attribute := FAttributes.Find(AttrName);
  if assigned(Attribute) then
    Result := Attribute.Value
  else
    Result := '';
end;

function TXmlNode.HasAttribute(const Name: String): Boolean;
begin
  Result := assigned(FAttributes.Find(Name));
end;

function TXmlNode.HasChild(const Name: String): Boolean;
begin
  Result := assigned(Find(Name));
end;

function TXmlNode.InsertChild(const Name: String; Pos: Integer): TXmlNode;
begin
  Result := TXmlNode.Create;
  Result.NodeName := Name;
  Result.Parent := Self;
  ChildNodes.Insert(Pos, Result);
end;

procedure TXmlNode.SetAttr(const AttrName, Value: String);
begin
  SetAttribute(AttrName, Value);
end;

function TXmlNode.SetAttribute(const AttrName, Value: String): TXmlNode;
var
  Attribute: TXmlAttribute;
begin
  Attribute := FAttributes.Find(AttrName); // Search for given name
  if not assigned(Attribute) then // If attribute is not found, create one
  begin
    Attribute := TXmlAttribute.Create;
    FAttributes.Add(Attribute);
  end;
  Attribute.Name := AttrName; // this allows "name-style" rewriting
  Attribute.Value := TXmlVerySimple.Escape(Value);
  Result := Self;
end;

function TXmlNode.SetText(Value: String): TXmlNode;
begin
  Text := Value;
  Result := Self;
end;

{ TXmlAttributeList }

function TXmlAttributeList.Find(AttrName: String): TXmlAttribute;
var
  Attribute: TXmlAttribute;
begin
  Result := NIL;
  AttrName := lowercase(AttrName);
  for Attribute in Self do
    if lowercase(Attribute.Name) = AttrName then
    begin
      Result := Attribute;
      Break;
    end;
end;

end.
