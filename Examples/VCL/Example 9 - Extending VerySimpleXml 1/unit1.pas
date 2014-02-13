unit unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Xml.VerySimple, Xml.VerySimple.Extended;

procedure TForm1.FormShow(Sender: TObject);
var
  Xml: TXmlVerySimple;
  BookNodes: TXmlNodeList;
  BookNode: TXmlNode;
begin
  // Create a XML document first, and fill it
  Xml := TXmlVerySimple.Create;
  Xml.Text := '<?xml version="1.0" encoding="utf-8"?><books><book id="101"><author>Gambardella, Matthew</author>' +
    '<title>XML Developer''s Guide</title></book></books>';

  // Now add new entities to each book, and use the extended AsDate/AsDateTime function
  BookNodes := Xml.DocumentElement.FindNodes('book');
  for BookNode in BookNodes do
  begin
    BookNode.ChildNodes.Add('ReleaseDate').AsDate := EncodeDate(2000, 10, 1);
    BookNode.ChildNodes.Add('LastAccess').AsDateTime := Now;
  end;

  // Write to memo, the first 3 chars are the unicode BOM
  Memo1.Lines.Add(Xml.Text);
  Xml.SaveToFile('example9.xml');
  Xml.Free;
end;

end.
