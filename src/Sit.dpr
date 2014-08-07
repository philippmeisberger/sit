program Sit;

uses
  Forms,
  SitMain in 'SitMain.pas' {Form1},
  SitInfo in 'SitInfo.pas' {Form2},
  SitAPI in 'SitAPI.pas',
  SitUpdate in 'SitUpdate.pas' {Form3};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'SIT';
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
