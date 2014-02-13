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
  Xml.VerySimple;

procedure TForm1.FormShow(Sender: TObject);
var
  Xml: TXmlVerySimple;
  Node: TXmlNode;
begin
  // Create a XML document first, and fill it
  Xml := TXmlVerySimple.Create;
  Xml.Text := '<?xml version="1.0" encoding="utf-8"?><books><book id="bk101"><author>Gambardella, Matthew</author>' +
    '<title>XML Developer''s Guide</title></book></books>';

  // Create a processing instruction node
  Node := Xml.CreateNode('PI', ntProcessingInstr);

  // Add a name only attribute
  Node.AttributeList.Add('uppercase');

  // Add a valued attribute
  Node.Attributes['whitespace'] := 'ignore';

  // Add a valued attribute
  Node.Attributes['linebreaks'] := '0x0a';

  // Add the node below the XML header declaration
  Xml.ChildNodes.Insert(Xml.ChildNodes.IndexOf(Xml.Header) + 1, Node);

  // Write to memo, the first 3 chars are the unicode BOM
  Memo1.Lines.Add(Xml.Text);
  Xml.SaveToFile('example8.xml');
  Xml.Free;
end;

end.
