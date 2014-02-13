unit Xml.VerySimple.Extended;

interface

uses
  Xml.VerySimple;

type
  TXmlNodeHelper = class helper for TXmlNode
  private
    function GetAsInteger: Integer;
    procedure SetAsInteger(Value: Integer);
    function GetAsDateTime: TDateTime;
    procedure SetAsDateTime(Value: TDateTime);
    function GetAsDate: TDate;
    procedure SetAsDate(Value: TDate);
  public
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsDate: TDate read GetAsDate write SetAsDate;
  end;

implementation

uses
  SysUtils, DateUtils;

{ TXmlNodeHelper }

function TXmlNodeHelper.GetAsDate: TDate;
begin
  Result := DateOf(AsDateTime);
end;

function TXmlNodeHelper.GetAsDateTime: TDateTime;
begin
  Result := StrToDateTime(Text);
end;

function TXmlNodeHelper.GetAsInteger: Integer;
begin
  Result := StrToInt(Text);
end;

procedure TXmlNodeHelper.SetAsDate(Value: TDate);
begin
  Text := FormatDateTime('YYYYmmdd', Value);
end;

procedure TXmlNodeHelper.SetAsDateTime(Value: TDateTime);
begin
  Text := FormatDateTime('YYYYmmddhhnnsszzz', Value);
end;

procedure TXmlNodeHelper.SetAsInteger(Value: Integer);
begin
  Text := IntToStr(Value);
end;

end.
