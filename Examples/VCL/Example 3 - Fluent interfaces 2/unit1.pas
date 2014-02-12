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
  BookNode: TXmlNode;
begin
  // Create a new XML document
  Xml := TXmlVerySimple.Create;

  // Add a new child node, the first child node is the DocumentElement
  Xml.AddChild('books');

   // Create child nodes for the author and title
  Xml.DocumentElement.AddChild('book').SetAttribute('id', 'bk101').AddChild('author').
    SetText('Gambardella, Matthew').Parent.AddChild('title').Text := 'XML Developer''s Guide';

  // Write to memo, the first 3 chars are the unicode BOM
  Memo1.Lines.Text := Xml.Text;

  // Write to file
  Xml.SaveToFile('example3.xml');

  // And free resources
  Xml.Free;
end;

end.
