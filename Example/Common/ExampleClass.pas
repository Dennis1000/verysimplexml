unit ExampleClass;

interface

uses
  Xml.VerySimple;

type
  TExampleClass = class(TObject)
  private
  protected
  public
    class procedure Generate(Xml: TXmlVerySimple);
    class procedure Modify(Xml: TXmlVerySimple);
    class procedure Compact(Xml: TXmlVerySimple);
    class function Get(Xml: TXmlVerySimple): String;
    class function InsertXML: String;
  end;

implementation

{ TExampleClass }

class procedure TExampleClass.Compact;
begin
  Xml.Options := Xml.Options - [doNodeAutoIndent] + [doCompact];
end;

class procedure TExampleClass.Generate(Xml: TXmlVerySimple);
var
  Node, Child, SubChild: TXmlNode;
begin
  Xml.Encoding := 'utf-8';

  Xml.Root.NodeName := 'catalog';
  Xml.Root.AddChild('book').SetAttribute('id', 'bk101').AddChild('author').SetText('Gambardella, Matthew')
    .Parent.AddChild('title').SetText('XML Developer''s Guide').Parent.AddChild('description')
    .SetText('An in-depth look at creating XML applications.');

  Xml.Root.AddChild('book').SetAttribute('id', 'bk102').SetAttribute('lang', 'en').AddChild('author')
    .SetText('Ralls, Kim').Parent.AddChild('title').SetText('Midnight Rain').Parent.AddChild('description')
    .SetText('A former architect battles corporate zombies, ' +
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
end;

class function TExampleClass.Get(Xml: TXmlVerySimple): String;
begin
  Result := Xml.Root.Find('book', 'id', 'bk103').Find('description').Text;
end;

class function TExampleClass.InsertXML: String;
begin
  Result := '<book id=bk104><author rate="&gt;5">Corets, Eva</author><description/>' + '<keywords /></book>';
end;

class procedure TExampleClass.Modify(Xml: TXmlVerySimple);
var
  Node, BookNode: TXmlNode;
  Nodes, AllNodes: TXmlNodeList;
  NodesArray: TXmlNodeArray;
begin
  // Add a <keywords> section to every book if not exists
  // Use a NodeArray with FindNodes
  NodesArray := Xml.Root.FindNodes('book');
  for Node in NodesArray do
    if not assigned(Node.Find('keywords')) then
      Node.AddChild('keywords');

  // Add a new keyword to a certain book (id=bk102)
  Xml.Root.Find('book', 'id', 'bk102').Find('keywords').AddChild('keyword').SetText('no-muerto')
    .SetAttribute('lang', 'es').Parent.AddChild('keyword').SetText('zombies').Parent.AddChild('keywrd')
    .SetText('flombies');

  // Add new keyword attribute (lang=en) to every book
  AllNodes := Xml.Root.GetNodes('book');
  for BookNode in AllNodes do
  begin
    Nodes := BookNode.Find('keywords').GetNodes('keyword');
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
end;

end.
