unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.Memo, FMX.StdCtrls,
  Xml.VerySimple;

type
  TFrmMain = class(TForm)
    BtnGenerate: TButton;
    BtnModify: TButton;
    BtnCompact: TButton;
    BtnSave: TButton;
    BtnLoad: TButton;
    BtnGet: TButton;
    Memo1: TMemo;
    procedure BtnGenerateClick(Sender: TObject);
    procedure BtnModifyClick(Sender: TObject);
    procedure BtnCompactClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure BtnLoadClick(Sender: TObject);
    procedure BtnGetClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ShowXml(Xml: TXmlVerySimple);
    procedure MemoToXml(Xml: TXmlVerySimple);
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.fmx}

uses
  ExampleClass;

procedure TFrmMain.BtnCompactClick(Sender: TObject);
var
  Xml: TXmlVerySimple;
begin
  Xml := TXmlVerySimple.Create;
  // Load XML from Memo1
  MemoToXml(Xml);
  TExampleClass.Compact(Xml);
  ShowXml(Xml);
  Xml.Free;

  BtnCompact.Enabled := False;
  BtnSave.Enabled := True;
end;

procedure TFrmMain.BtnGenerateClick(Sender: TObject);
var
  Xml: TXmlVerySimple;
begin
  Xml := TXmlVerySimple.Create;
  TExampleClass.Generate(Xml);
  ShowXml(Xml);
  Xml.Free;

  BtnGenerate.Enabled := False;
  BtnModify.Enabled := True;
end;

procedure TFrmMain.BtnGetClick(Sender: TObject);
var
  Xml: TXmlVerySimple;
begin
  Xml := TXmlVerySimple.Create;
  // Load XML from Memo1
  MemoToXml(Xml);
  ShowMessage(TExampleClass.Get(Xml));
  Xml.Free;

  BtnGet.Enabled := False;
  BtnGenerate.Enabled := True;
end;

procedure TFrmMain.BtnLoadClick(Sender: TObject);
var
  Xml: TXmlVerySimple;
begin
  Xml := TXmlVerySimple.Create;
  Xml.LoadFromFile('example.xml');
  ShowXml(Xml);
  Xml.Free;

  BtnLoad.Enabled := False;
  BtnGet.Enabled := True;
end;

procedure TFrmMain.BtnModifyClick(Sender: TObject);
var
  Xml: TXmlVerySimple;
begin
  Xml := TXmlVerySimple.Create;
  // Add a new book
  Memo1.Lines.Insert(2, TExampleClass.InsertXML);
  // Load XML from Memo1
  MemoToXml(Xml);
  TExampleClass.Modify(Xml);
  ShowXml(Xml);
  Xml.Free;

  BtnModify.Enabled := False;
  BtnCompact.Enabled := True;
end;

procedure TFrmMain.BtnSaveClick(Sender: TObject);
var
  Xml: TXmlVerySimple;
begin
  Xml := TXmlVerySimple.Create;
  // Load XML from Memo1
  MemoToXml(Xml);
  Xml.SaveToFile('example.xml');
  Xml.Free;

  BtnSave.Enabled := False;
  BtnLoad.Enabled := True;
  Memo1.Text := 'example.xml saved.';
end;

procedure TFrmMain.MemoToXml(Xml: TXmlVerySimple);
begin
  Xml.Text := Memo1.Text;
end;

procedure TFrmMain.ShowXml(Xml: TXmlVerySimple);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  Xml.SaveToStream(Stream);
  Stream.Position := 0;
  Memo1.Lines.LoadFromStream(Stream);
  Stream.Free;
end;

end.
