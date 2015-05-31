program Sit;

uses
  Vcl.Forms,
  SitMain in 'SitMain.pas' {Main},
  SitInfo in 'SitInfo.pas' {Info},
  SitAPI in 'SitAPI.pas',
  PMCW.Mutex in 'PMCW.Mutex.pas',
  AdminManifest in 'AdminManifest.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'SIT';
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
