program Sit;

{$R 'description.res' 'description.rc'}
{$R 'changelog.res' 'changelog.rc'}

uses
  Vcl.Forms,
{$IFNDEF DEBUG}
  PMCW.AdminManifest,
{$ENDIF}
  PMCW.Application.Mutex,
  SitMain in 'SitMain.pas' {Main},
  SitAPI in 'SitAPI.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'SIT';
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
