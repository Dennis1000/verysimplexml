program example;

uses
  ExceptionLog,
  Vcl.Forms,
  Main in 'Main.pas' {FrmMain},
  Xml.VerySimple in '..\Source\Xml.VerySimple.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
