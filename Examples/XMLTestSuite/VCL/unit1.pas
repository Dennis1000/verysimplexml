unit unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Xml.VerySimple, TestSuite;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure OnMessage(Sender: TObject; Msg: String);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  TestSuite: TTestSuite;
begin
  Memo1.Lines.Clear;
  TestSuite := TTestSuite.Create;
  TestSuite.OnMessage := OnMessage;
  TestSuite.Run('..\TestFiles\');
  TestSuite.Free;
end;

procedure TForm1.OnMessage(Sender: TObject; Msg: String);
begin
  Memo1.Lines.Add(Msg);
end;

end.
