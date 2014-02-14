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
  Memo1.Lines.Add('Default: without parsing of the xml-stylesheet processing instructions (double spaces not removed, etc.):');
  Memo1.Lines.Add('');

  // Create a XML document first, and fill it
  Xml := TXmlVerySimple.Create;
  Xml.Text := '<?xml version="1.0" encoding="utf-8"?><?xml-stylesheet   type="text/css"   href="glossary.css" ?>'+
    '<books><book id="bk101"><author>Gambardella, Matthew</author><title>XML Developer''s Guide</title></book>'+
    '</books>';

  // Write to memo, the first 3 chars are the unicode BOM
  Memo1.Lines.Add(Xml.Text);
  Xml.SaveToFile('example7a.xml');
  Xml.Free;

  Memo1.Lines.Add('');
  Memo1.Lines.Add('With parsing of the processing instructions:');
  Memo1.Lines.Add('');

  // Create a XML document first, and fill it, but do now parse processing instructions
  Xml := TXmlVerySimple.Create;
  Xml.Options := Xml.Options + [doParseProcessingInstr];
  Xml.Text := '<?xml version="1.0" encoding="utf-8"?><?xml-stylesheet  type="text/css"  href="glossary.css" ?>'+
    '<books><book id="bk101"><author>Gambardella, Matthew</author><title>XML Developer''s Guide</title></book>'+
    '</books>';
  // Write to memo, the first 3 chars are the unicode BOM
  Memo1.Lines.Add(Xml.Text);
  Xml.SaveToFile('example7b.xml');

  Memo1.Lines.Add('');

  // Now find the xml-style sheet node
  Node := Xml.ChildNodes.Find('xml-stylesheet', [ntProcessingInstr]);
  if assigned(Node) then
  begin
    Memo1.Lines.Add('xml stylesheet href is "' + Node.Attributes['href'] + '"');
    // Now change the href value
    Node.Attributes['href'] := 'basic.css';
  end;

  Memo1.Lines.Add('');
  Memo1.Lines.Add('changed xml stylesheet href to "basic.css":');
  Memo1.Lines.Add('');

  Memo1.Lines.Add(Xml.Text);
  Xml.SaveToFile('example7c.xml');

  Xml.Free;
end;

end.
