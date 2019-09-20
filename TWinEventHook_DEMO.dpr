program TWinEventHook_DEMO;

uses
  Vcl.Forms,
  MainU in 'MainU.pas' {Form1},
  WinEventHook in 'WinEventHook.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
