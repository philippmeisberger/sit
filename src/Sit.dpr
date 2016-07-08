program Sit;

{$R 'description.res' 'description.rc'}
{$R 'changelog.res' 'changelog.rc'}

uses
  Vcl.Forms,
{$IFNDEF DEBUG}
  AdminManifest in 'AdminManifest.pas',
{$ENDIF}
  PMCWMutex in 'PMCWMutex.pas',
  SitMain in 'SitMain.pas' {Main},
  SitAPI in 'SitAPI.pas',
  PMCWAbout in 'PMCWAbout.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'SIT';
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
