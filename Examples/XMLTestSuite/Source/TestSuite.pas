unit TestSuite;

interface

uses
  Classes, Xml.VerySimple;

type
  TOnMessage = procedure(Sender: TObject; Msg: String) of object;

  TTestSuite = class(TObject)
  private
    FOnMessage: TOnMessage;
    procedure Msg(Value: String);
    procedure Split(const Value: String; var Output1, Output2: String);
    function ProcessInstructions(Lines: TStringList; Xml: TXmlVerySimple): Boolean;
  protected
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property OnMessage: TOnMessage read FOnMessage write FOnMessage;
    procedure Run(Path: String);
  end;

implementation

{ TTestSuite }

uses
  SysUtils, IOUtils;

constructor TTestSuite.Create;
begin
  inherited;
end;

destructor TTestSuite.Destroy;
begin
  inherited;
end;

procedure TTestSuite.Msg(Value: String);
begin
  if assigned(OnMessage) then
    OnMessage(Self, Value);
end;

procedure TTestSuite.Run(Path: String);
var
  Xml: TXmlVerySimple;
  Dir: TSearchRec;
  Res: Integer;
  OutputStream, InputStream: TStringStream;
  Fail: Boolean;
  Files: TStringList;
  FileName: String;
  Name: String;
  Lines: TStringList;
  TempFileName: String;
begin
  Files := TStringList.Create;
  Files.Sorted := True;
  Res := FindFirst(Path + 'test*.xml', faAnyFile, Dir);
  while Res = 0 do
  begin
    Files.Add(Dir.Name);
    Res := FindNext(Dir);
  end;
  FindClose(Dir);

  if Files.Count = 0 then
  begin
    Msg('No tests found (' + Path + ')');
    Exit;
  end;

  Fail := False;
  for FileName in Files do
  begin
    Name := TPath.GetFileNameWithoutExtension(FileName);

    Msg('File:   ' + FileName);
    TempFileName := Path + Name + '.txt';
    if FileExists(TempFileName) then
    begin
      Lines := TStringList.Create;
      Lines.LoadFromFile(TempFileName);
      Msg('Desc:   ' + Trim(Lines.Text));
      Lines.Free;
    end;

    Xml := TXmlVerySimple.Create;
    TempFileName := Path + Name + '.pibl';
    if FileExists(TempFileName) then
    begin
      Lines := TStringList.Create;
      Lines.LoadFromFile(TempFileName);
      Msg('PIBL:   ' + Trim(Lines.Text));
      if not ProcessInstructions(Lines, Xml) then
        Fail := True;
      Lines.Free;
    end;

    if not Fail then
    begin
      Xml.LoadFromFile(Path + FileName);

      TempFileName := Path + Name + '.pial';
      if FileExists(TempFileName) then
      begin
        Lines := TStringList.Create;
        Lines.LoadFromFile(TempFileName);
        Msg('PIAL:   ' + Trim(Lines.Text));
        if not ProcessInstructions(Lines, Xml) then
          Fail := True;
        Lines.Free;
      end;

      OutputStream := TStringStream.Create;
      Xml.SaveToStream(OutputStream);
      Xml.SaveToFile(Name + '.out2');
      Xml.Free;

      InputStream := TStringStream.Create;
      InputStream.LoadFromFile(Path + Name +'.out');
      if InputStream.DataString <> OutputStream.DataString then
        Fail := True;

      InputStream.Free;
      OutputStream.Free;
    end;

    if not Fail then
    begin
       Msg('Result: Pass');
       Msg('');
    end
    else
    begin
      Msg('Result: Fail');
      Break;
    end;
  end;

  if not Fail then
    Msg('All tests passed.');

end;


function TTestSuite.ProcessInstructions(Lines: TStringList; Xml: TXmlVerySimple): Boolean;
var
  Command, Value: String;
  TrimmedLine, Line: String;
  Node: TXmlNode;
  Attribute: TXmlAttribute;
begin
  try
    Result := True;
    Node := Xml.ChildNodes.Parent;
    Attribute := NIL;
    for Line in Lines do
    begin
      TrimmedLine := TrimLeft(Line);
      if TrimmedLine = '' then
        Continue;

      Split(TrimmedLine, Command, Value);
      Command := Lowercase(Command);

      if (Command='compact') and (Value='1') then
        Xml.Options := Xml.Options + [doCompact]
      else
      if Command='indent' then
      begin
        Value := Copy(Value, 2, Length(Value) - 2);
        Xml.NodeIndentStr := Value;
      end
      else
      if Command='linebreak' then
      begin
        if Lowercase(Value)='unix' then
          Xml.LineBreak := #$0A
        else
          Xml.LineBreak := #$0D#$0A;
      end
      else
      if Command='findnode' then
      begin
        Node := Node.ChildNodes.Find(Value);
        if not assigned(Node) then
        begin
          Result := False;
          Exit;
        end;
      end
      else
      if Command='addnode' then
        Node := Node.AddChild(Value)
      else
      if Command='addattribute' then
        Attribute := Node.AttributeList.Add(Value)
      else
      if Command='setvalue' then
        Attribute.Value := Value
      else
      if Command='settext' then
        Node.SetText(Value)
      else
      if Command='parent' then
        Node := Node.Parent
      else
      if Command='istext' then
      begin
        if Node.Text <> Value then
        begin
          Result := False;
          Exit;
        end;
      end
      else
      begin
        Result := False;
        exit;
      end;
    end;
  except
    Result := False;
  end;
end;

procedure TTestSuite.Split(const Value: String; var Output1, Output2: String);
var
  ValuePos: Integer;
begin
  ValuePos := Pos('=', Value);
  if ValuePos = 0 then
  begin
    Output1 := Value;
    Output2 := '';
  end
  else
  begin
    Output1 := Copy(Value, 1, ValuePos - 1);
    Output2 := Copy(Value, ValuePos + 1);
  end;
end;

end.
