program Sit;

uses
  Vcl.Forms,
  PMCWMutex in 'PMCWMutex.pas',
  SitMain in 'SitMain.pas' {Main},
  SitAPI in 'SitAPI.pas',
  AdminManifest in 'AdminManifest.pas',
  PMCWAbout in 'PMCWAbout.pas' {Info};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'SIT';
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
