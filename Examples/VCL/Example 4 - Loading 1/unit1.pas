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
  BookNode, EntityNode: TXmlNode;
  Books: TXmlNodeList;
begin
  // Create a XML document first, and save it
  Xml := TXmlVerySimple.Create;
  Xml.AddChild('books');
  Xml.DocumentElement.AddChild('book').SetAttribute('id', 'bk101').AddChild('author').
    SetText('Gambardella, Matthew').Parent.AddChild('title').Text := 'XML Developer''s Guide';

  Xml.DocumentElement.AddChild('book').SetAttribute('id', 'bk103').AddChild('author').
    SetText('Corets, Eva').Parent.AddChild('title').Text := 'Maeve Ascendant';
  Xml.SaveToFile('example4.xml');
  Xml.Free;

  // Now try to load it
  Xml := TXmlVerySimple.Create;
  Xml.LoadFromFile('example4.xml');

  // FindNodes returns a NodeList
  Books := Xml.DocumentElement.FindNodes('book');

  for BookNode in Books do
  begin
    Memo1.Lines.Add('Book: ' + BookNode.Attributes['id']);

    // Locate the 'title' node and write its content to the memo
    EntityNode := BookNode.Find('title');
    if assigned(EntityNode) then
      Memo1.Lines.Add('Title: ' + EntityNode.Text);

    // Locate the 'author' node and write its content to the memo
    EntityNode := BookNode.Find('author');
    if assigned(EntityNode) then
      Memo1.Lines.Add('Author: ' + EntityNode.Text);

    Memo1.Lines.Add('');
  end;
  Books.Free;

  // And free resources
  Xml.Free;
end;

end.
