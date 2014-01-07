program example;

uses
  Vcl.Forms,
  Main in 'Main.pas' {FrmMain},
  Xml.VerySimple in '..\..\Source\Xml.VerySimple.pas',
  ExampleClass in '..\Common\ExampleClass.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
