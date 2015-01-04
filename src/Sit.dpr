program Sit;

uses
  Forms,
  SitMain in 'SitMain.pas' {Main},
  SitInfo in 'SitInfo.pas' {Info},
  SitAPI in 'SitAPI.pas',
  Mutex in 'Mutex.pas',
  AdminManifest in 'AdminManifest.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'SIT';
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
