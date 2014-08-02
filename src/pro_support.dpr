program pro_support;

uses
  Forms,
  SitMain in 'SitMain.pas' {Form1},
  SitInfo in 'SitInfo.pas' {Form2},
  SitAPI in 'SitAPI.pas',
  SitUpdate in 'SitUpdate.pas' {Form3},
  SitUpdateThread in 'SitUpdateThread.pas',
  UpdateCheckThread in 'UpdateCheckThread.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'SIT';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
