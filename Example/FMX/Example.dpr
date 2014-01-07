program Example;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  Main in 'Main.pas' {FrmMain},
  ExampleClass in '..\Common\ExampleClass.pas',
  Xml.VerySimple in '..\..\Source\Xml.VerySimple.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
