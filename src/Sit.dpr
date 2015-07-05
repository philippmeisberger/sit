program Sit;

uses
  Vcl.Forms,
  PMCWMutex in 'PMCWMutex.pas',
  SitMain in 'SitMain.pas' {Main},
  SitInfo in 'SitInfo.pas' {Info},
  SitAPI in 'SitAPI.pas',
  AdminManifest in 'AdminManifest.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'SIT';
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
