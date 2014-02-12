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
begin
  // Create a XML document first, and save it
  Xml := TXmlVerySimple.Create;
  Xml.AddChild('books');

  // Add a new comment node, the NodeName is left blank, because it is not used during output
  Xml.DocumentElement.AddChild('', ntComment).Text := ' this is the first book ';

  // Add a book
  Xml.DocumentElement.AddChild('book').SetAttribute('id', 'bk101').AddChild('author').
    SetText('Gambardella, Matthew').Parent.AddChild('title').Text := 'XML Developer''s Guide';

  // Add a new comment node, the NodeName is left blank, because it is not used during output
  Xml.DocumentElement.AddChild('', ntComment).Text := ' this is the second book ';

  // Add a book
  Xml.DocumentElement.AddChild('book').SetAttribute('id', 'bk103').AddChild('author').
    SetText('Corets, Eva').Parent.AddChild('title').Text := 'Maeve Ascendant';

  // Write to memo, the first 3 chars are the unicode BOM
  Memo1.Lines.Text := Xml.Text;

  // Write to file
  Xml.SaveToFile('example5.xml');

  // And free resources
  Xml.Free;
end;

end.
