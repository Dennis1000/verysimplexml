program Example1;

uses
  Forms,
  unit1 in 'unit1.pas' {Form1},
  Xml.VerySimple in '..\..\..\Source\Xml.VerySimple.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
