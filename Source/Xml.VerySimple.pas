{ VerySimpleXML v1.1 - a lightweight, one-unit XML reader/writer
  by Dennis Spreen
  http://blog.spreendigital.de/2011/11/10/verysimplexml-a-lightweight-delphi-xml-reader-and-writer/

  (c) Copyrights 2012 Dennis D. Spreen <dennis@spreendigital.de>
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
    Text: String; // Node text content
    Obj: TObject; // attached object
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
    function SetAttribute(const AttrName: String;
      const Value: String): TXmlNode; virtual;
    property Attribute[const AttrName: String]: String read GetAttribute
      write SetAttr; // Attributes of a Node, accessible by attribute name
  end;

  TXmlNodeList = class(TObjectList<TXmlNode>);

  TXmlOnNodeSetText = procedure (Sender: TObject; Node: TXmlNode; Text: String) of
    object;

  TXmlVerySimple = class(TObject)
  private
    Lines: TStringList;
    FOnNodeSetText: TXmlOnNodeSetText;
    FOnNodeSetName: TXmlOnNodeSetText;
    procedure Parse(Reader: TStreamReader);
    procedure Walk(Lines: TStringList; Prefix: String; Node: TXmlNode);
    procedure SetText(Value: String);
    function GetText: String;
    function Escape(Value: String): String;
    function UnEscape(Value: String): String;
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
    procedure DefaultOnNodeSetText(Sender: TObject; Node: TXmlNode; Text: String); inline;
    procedure DefaultOnNodeSetName(Sender: TObject; Node: TXmlNode; Name: String); inline;
    property Text: String read GetText write SetText;
    property OnNodeSetText: TXmlOnNodeSetText read FOnNodeSetText write FOnNodeSetText;
    property OnNodeSetName: TXmlOnNodeSetText read FOnNodeSetName write FOnNodeSetName;
  end;

implementation

uses
  SysUtils, StrUtils;

{ TXmlVerySimple }

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
  FOnNodeSetText := DefaultOnNodeSetText;
  FOnNodeSetName := DefaultOnNodeSetName;
  Clear;
end;

procedure TXmlVerySimple.DefaultOnNodeSetName(Sender: TObject; Node: TXmlNode;
  Name: String);
begin
  Node.NodeName := Name;
end;

procedure TXmlVerySimple.DefaultOnNodeSetText(Sender: TObject; Node: TXmlNode;
  Text: String);
begin
  Node.Text := Text;
end;

destructor TXmlVerySimple.Destroy;
begin
  Root.Free;
  Header.Free;
  Lines.Free;
  inherited;
end;

function TXmlVerySimple.Escape(Value: String): String;
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
  FileStream: TFileStream;
begin
  FileStream :=  TFileStream.Create(FileName, fmOpenRead);
  LoadFromStream(FileStream);
  FileStream.Free;
end;

procedure TXmlVerySimple.LoadFromStream(const Stream: TStream);
var
  Reader: TStreamReader;
begin
  Clear;
  Reader := TStreamReader.Create(Stream);
  Parse(Reader);
  Reader.Free;
end;

procedure TXmlVerySimple.Parse(Reader: TStreamReader);
var
  Line: String;
  IsTag, IsText: Boolean;
  Tag, Text: String;
  Parent, Node: TXmlNode;
  Attribute: TXmlAttribute;
  ALine, Attr, AttrText: String;
  P: Integer;
  IsSelfClosing: Boolean;
  IsQuote: Boolean;

  // Return a text ended by StopChar, respect quotation marks
  function GetText(var Line: String; StartStr: String; StopChar: Char): String;
  var
    Chr: Char;
  begin
    while (Length(Line) > 0) and ((Line[1] <> StopChar) or (IsQuote)) do
    begin
      Chr := Line[1];
      if Chr = '"' then
        IsQuote := Not IsQuote;
      StartStr := StartStr + Chr;
      delete(Line, 1, 1);
    end;
    Result := StartStr;
  end;

begin
  if assigned(Root) then // Release previous nodes (if set)
    Root.Free;

  IsTag := False;
  IsText := False;
  IsQuote := False;
  Node := NIL;

  while not Reader.EndOfStream do
  begin
    Line := Reader.ReadLine;

    while (Length(Line) > 0) do
    begin
      if (not IsTag) and (not IsText) then
      begin
        while (Length(Line) > 0) and (Line[1] <> '<') do
          delete(Line, 1, 1);

        if Length(Line) > 0 then
        begin
          IsTag := True;
          delete(Line, 1, 1); // Delete openining tag
          Tag := '';
        end;
      end;

      if IsTag then
      begin
        Tag := GetText(Line, Tag, '>');

        if (Length(Line) > 0) and (Line[1] = '>') then
        begin
          delete(Line, 1, 1);
          IsTag := False;

          if (Length(Tag) > 0) and (Tag[1] = '/') then
            Node := Node.Parent
          else
          begin
            Parent := Node;
            IsText := True;
            IsQuote := False;

            Node := TXmlNode.Create;
            if lowercase(copy(Tag, 1, 4)) = '?xml' then // check for xml header
            begin
              Tag := TrimRight(Tag);
              if Tag[Length(Tag)]='?' then
                delete(Tag, Length(Tag), 1);
              Header.Free;
              Header := Node;
            end;

            // Self-Closing Tag
            if (Length(Tag) > 0) and (Tag[Length(Tag)] = '/') then
            begin
              IsSelfClosing := True;
              delete(Tag, Length(Tag), 1);
            end
            else
              IsSelfClosing := False;

            P := pos(' ', Tag);
            if P <> 0 then // Tag name has attributes
            begin
              ALine := Tag;
              delete(Tag, P, Length(Tag));
              delete(ALine, 1, P);

              while Length(ALine) > 0 do
              begin
                Attr := GetText(ALine, '', '='); // Get Attribute Name
                AttrText := GetText(ALine, '', ' '); // Get Attribute Value

                if Length(AttrText) > 0 then
                begin
                  delete(AttrText, 1, 1); // Remove blank

                  if AttrText[1] = '"' then // Remove start/end quotation marks
                  begin
                    delete(AttrText, 1, 1);
                    if AttrText[Length(AttrText)] = '"' then
                      delete(AttrText, Length(AttrText), 1);
                  end;
                end;

                if Length(ALine) > 0 then
                  delete(ALine, 1, 1);

                // Header node (Attr='?') does not support Attributes
                if not((Node = Header) and (Attr = '?')) then
                begin
                  Attribute := TXmlAttribute.Create;
                  Attribute.Name := Attr;
                  Attribute.Value := AttrText;
                  Node.FAttributes.Add(Attribute);
                end;
                IsQuote := False;
              end;
            end;

            FOnNodeSetName(Self, Node, Tag);
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
        Text := GetText(Line, Text, '<');
        if (Length(Line) > 0) and (Line[1] = '<') then
        begin
          IsText := False;
          while (Length(Text) > 0) and (Text[1] = ' ') do
            delete(Text, 1, 1);
          FOnNodeSetText(Self, Node, UnEscape(Text));
        end;
      end;

    end;
  end;
  Reader.Close;
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
    Encoding := TEncoding.UTF8;
  Lines.SaveToStream(Stream, Encoding);
  Lines.Clear;
end;

procedure TXmlVerySimple.SetText(Value: String);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  Lines.Text := Value;
  Lines.SaveToStream(Stream);
  Lines.Clear;
  Stream.Position := 0;
  LoadFromStream(Stream);
  Stream.Free;
end;

function TXmlVerySimple.UnEscape(Value: String): String;
begin
  Result := ReplaceStr(Value, '&lt;', '<' );
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
    S := S + ' ' + Attribute.Name + '="' + Attribute.Value + '"';

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
  Attribute.Value := Value;
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
