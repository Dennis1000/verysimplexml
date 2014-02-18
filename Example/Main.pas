unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Xml.VerySimple;

type
  TFrmMain = class(TForm)
    BtnGenerate: TButton;
    Memo1: TMemo;
    BtnModify: TButton;
    BtnCompact: TButton;
    BtnSave: TButton;
    BtnLoad: TButton;
    BtnGet: TButton;
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

{$R *.dfm}

procedure TFrmMain.BtnCompactClick(Sender: TObject);
var
  Xml: TXmlVerySimple;
begin
  // Load XML from Memo1
  Xml := TXmlVerySimple.Create;
  MemoToXml(Xml);
  Xml.Ident := '';
  ShowXml(Xml);
  Xml.Free;

  BtnCompact.Enabled := False;
  BtnSave.Enabled := True;
end;

procedure TFrmMain.BtnGenerateClick(Sender: TObject);
var
  Xml: TXmlVerySimple;
  Node, Child, SubChild: TXmlNode;
begin
  Xml := TXmlVerySimple.Create;
  Xml.Header.Attribute['encoding'] := 'utf-8';

  Xml.Root.NodeName := 'catalog';
  Xml.Root.AddChild('book').SetAttribute('id', 'bk101').
    AddChild('author').SetText('Gambardella, Matthew').Parent.
    AddChild('title').SetText('XML Developer''s Guide').Parent.
    AddChild('description').SetText('An in-depth look at creating XML applications.');

  Xml.Root.AddChild('book').SetAttribute('id', 'bk102').SetAttribute('lang','en').
    AddChild('author').SetText('Ralls, Kim').Parent.
    AddChild('title').SetText('Midnight Rain').Parent.
    AddChild('description').SetText('A former architect battles corporate zombies, '+
      'an evil sorceress, and her own childhood to become queen of the world.');

  Node := Xml.Root.AddChild('book');
  Node.Attribute['id'] := 'bk103';

  Child := Node.AddChild('author');
  Child.Text := 'Corets, Eva';

  Child := Node.AddChild('title');
  Child.Text := 'Maeve Ascendant';

  Child := Node.AddChild('description');
  Child.Text := 'After the "collapse" of a <nanotechnology> society in England, ' +
  'the young survivors lay the foundation for a new society.';

  Child := Node.AddChild('keywords');
  SubChild := Child.AddChild('keyword');
  SubChild.Text := 'fantasy';
  SubChild := Child.AddChild('keyword');
  SubChild.Text := 'technology';
  SubChild := Child.AddChild('keyword');
  SubChild.Text := 'england';

  ShowXml(Xml);
  Xml.Free;

  BtnGenerate.Enabled := False;
  BtnModify.Enabled := True;
end;


procedure TFrmMain.BtnGetClick(Sender: TObject);
var
  Xml: TXmlVerySimple;
begin
  // Load XML from Memo1
  Xml := TXmlVerySimple.Create;
  MemoToXml(Xml);

  ShowMessage(Xml.Root.Find('book', 'id', 'bk103').Find('description').Text);

  Xml.Free;
  BtnGet.Enabled := False;
end;

procedure TFrmMain.BtnModifyClick(Sender: TObject);
var
  Xml: TXmlVerySimple;
  Node, BookNode: TXmlNode;
  Nodes, AllNodes: TXmlNodeList;
  Index: Integer;
begin
  // Load XML from Memo1
  Xml := TXmlVerySimple.Create;

  Index := Memo1.Lines.Count - 1;

  // Add a new book
  Memo1.Lines.Insert(Index, '<book id=bk104><author rate=">5">Corets, Eva</author><description/>' +
   '<keywords /></book>');

  MemoToXml(Xml);

  // Add a <keywords> section to every book if not exists
  Nodes := Xml.Root.FindNodes('book');
  for Node in Nodes do
    if not assigned(Node.Find('keywords')) then
      Node.AddChild('keywords');
  Nodes.Free;

  // Add a new keyword to a certain book (id=bk102)
  Xml.Root.Find('book', 'id', 'bk102').Find('keywords').
    AddChild('keyword').SetText('no-muerto').SetAttribute('lang', 'es').Parent.
    AddChild('keyword').SetText('zombies').Parent.AddChild('keywrd').SetText('flombies');


  // Add new keyword attribute (lang=en) to every book
  AllNodes := Xml.Root.FindNodes('book');
  for BookNode in AllNodes do
  begin
    Nodes := BookNode.Find('keywords').FindNodes('keyword');
    for Node in Nodes do
      if not Node.HasAttribute('lang') then
        Node.Attribute['lang'] := 'en';
    Nodes.Free;
  end;
  AllNodes.Free;

  // Delete a node
  Node := Xml.Root.Find('book', 'id', 'bk102').Find('author');

  // remove node from parent childnodes, the node itself will be freed then
  if assigned(Node) then
      Node.Parent.ChildNodes.Remove(Node);

  ShowXml(Xml);

  Xml.Free;
  BtnModify.Enabled := False;
  BtnCompact.Enabled := True;
end;


procedure TFrmMain.BtnSaveClick(Sender: TObject);
var
  Xml: TXmlVerySimple;
begin
  // Load XML from Memo1
  Xml := TXmlVerySimple.Create;
  MemoToXml(Xml);
  Xml.SaveToFile('example.xml');
  Xml.Free;

  BtnSave.Enabled := False;
  BtnLoad.Enabled := True;
  memo1.Text := 'example.xml saved.';
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


procedure TFrmMain.BtnLoadClick(Sender: TObject);
var
  Xml: TXmlVerySimple;
  Start: Cardinal;
begin
  // Load XML from Memo1
  Start := GetTickCount;
  Xml := TXmlVerySimple.Create;
  Xml.LoadFromFile('benchmark.xml');
  ShowMessage(IntToStr(GetTickCount-Start)+'ms');

  ShowXml(Xml);

  Xml.Free;
  BtnLoad.Enabled := False;
  BtnGet.Enabled := True;
end;

end.



