{ *********************************************************************** }
{                                                                         }
{ SIT Update Unit v2.0                                                    }
{                                                                         }
{ Copyright (c) 2011-2014 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit SitUpdate;

interface

uses
  SysUtils, Classes, UpdateCheckThread, LanguageFile, Windows, FileCtrl, Forms,
  StdCtrls, ComCtrls, Controls, WinUtils, DownloadThread;

type
  { TUpdateCheck }
  TUpdateCheck = class(TObject)
  private
    FOwner: TComponent;
    FLang: TLanguageFile;
    FUserUpdate: Boolean;
    FNewBuild: Cardinal;
    { TUpdateCheckThread events }
    procedure OnCheckError(Sender: TThread);
    procedure OnNoUpdateAvailable(Sender: TThread);
    procedure OnUpdateAvailable(Sender: TThread; const ANewBuild: Cardinal);
  public
    constructor Create(AOwner: TComponent; ALang: TLanguageFile);
    procedure CheckForUpdate(AUserUpdate: Boolean);
  end;

  { User cancel download event }
  TOnUserCancelEvent = procedure(Sender: TObject) of object;

  { Download type }
  TDownloadType = (dtUpdate, dtCert);

  { TUpdate }
  TUpdate = class(TForm)
    pbProgress: TProgressBar;
    bFinished: TButton;
    lSize: TLabel;
    procedure bFinishedClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FOnUserCancel: TOnUserCancelEvent;
    FThreadRuns: Boolean;
    FFileName: string;
    FLang: TLanguageFile;
    FDownloadType: TDownloadType;
    procedure Reset();
    { TDownloadThread events }
    procedure OnDownloadCancel(Sender: TThread);
    procedure OnDownloadError(Sender: TThread; AResponseCode: Integer);
    procedure OnDownloadFinished(Sender: TThread);
    procedure OnDownloading(Sender: TThread; const ADownloadSize: Integer);
    procedure OnDownloadStart(Sender: TThread; const AFileSize: Integer);
  public
    constructor Create(AOwner: TComponent; ALangFile: TLanguageFile;
      AFormCaption: string = '');
    procedure Download(ADownloadType: TDownloadType);
  end;


implementation

uses SitMain;

{$R *.dfm}

{ TUpdateCheck }

{ public TUpdateCheck.Create

  Constructor for creating an TUpdateCheck instance. }

constructor TUpdateCheck.Create(AOwner: TComponent; ALang: TLanguageFile);
begin
  inherited Create;
  FLang := ALang;
  FOwner := AOwner;
end;

{ private TUpdateCheck.OnCheckError

  Event method that is called TUpdateCheckThread when error occurs while
  searching for update. }

procedure TUpdateCheck.OnCheckError(Sender: TThread);
begin
  if FUserUpdate then
     with FLang do
       MessageBox(GetString(68) +^J+ GetString(69), mtError, True);
end;

{ private TUpdateCheck.OnNoUpdateAvailable

  Event method that is called when TUpdateCheckThread search returns no update. }

procedure TUpdateCheck.OnNoUpdateAvailable(Sender: TThread);
begin
  if FUserUpdate then
     FLang.MessageBox(54, mtInfo, True);
end;

{ private TUpdateCheck.OnUpdateAvailable

  Event method that is called when TUpdateCheckThread search returns an update. }

procedure TUpdateCheck.OnUpdateAvailable(Sender: TThread; const ANewBuild: Cardinal);
begin
  // Update already available?
  if (FNewBuild = ANewBuild) then
     begin
     // Create and show TUpdate form directly
     with TUpdate.Create(FOwner, FLang, FLang.GetString(61)) do
       Download(dtUpdate);

     Abort;
     end;  //of begin

  // Store newest build
  FNewBuild := ANewBuild;

  // Show dialog: Ask for permitting download
  with FLang do
    if (MessageBox(Format(GetString(55) +^J+ GetString(56), [ANewBuild]),
       mtQuestion, True) = IDYES) then
       // Create and show TUpdate form
       with TUpdate.Create(FOwner, FLang, FLang.GetString(61)) do
         Download(dtUpdate)
    else
       (FOwner as TMain).mmUpdate.Caption := GetString(61);
end;

{ public TUpdateCheck.CheckForUpdate

  Searches for update on HTTP server. }

procedure TUpdateCheck.CheckForUpdate(AUserUpdate: Boolean);
begin
  FUserUpdate := AUserUpdate;

  // Update already available?
  if (FNewBuild > 0) then
     begin
     OnUpdateAvailable(nil, FNewBuild);
     Abort;
     end;  //of begin

  // Search for update
  with TUpdateCheckThread.Create(TWinUtils.GetBuildNumber(), 'Sit') do
  begin
    OnUpdate := OnUpdateAvailable;
    OnNoUpdate := OnNoUpdateAvailable;
    OnError := OnCheckError;
    Resume;
  end;  //of with
end;


{ TUpdate }

{ public TUpdate.Create

  Constructor for creating an TUpdate instance. }

constructor TUpdate.Create(AOwner: TComponent; ALangFile: TLanguageFile;
  AFormCaption: string = '');
begin
  inherited Create(AOwner);
  FLang := ALangFile;
  FThreadRuns := False;

  if (AFormCaption <> '') then
     Self.Caption := AFormCaption;
end;

{ private TUpdate.Reset

  Resets Update GUI. }

procedure TUpdate.Reset();
begin
  // Reset ProgressBar
  pbProgress.Position := 0;

  lSize.Caption := FLang.GetString(61);
  bFinished.Caption := FLang.GetString(62);
  FThreadRuns := False;
end;

{ private TUpdate.OnDownloadCancel

  Event method that is called by TDownloadThread when user canceled downlad. }
  
procedure TUpdate.OnDownloadCancel(Sender: TThread);
begin
  Reset;
  FLang.MessageBox(63, mtInfo);
end;

{ private TUpdate.OnDownloadError

  Event method that is called by TDownloadThread when an error occurs while
  downloading the update. }

procedure TUpdate.OnDownloadError(Sender: TThread; AResponseCode: Integer);
begin
  with FLang do
    MessageBox(Caption + GetString(48) +^J+^J+ GetString(49) + GetString(50), mtError, True);

  Reset;
end;

{ private TUpdate.OnDownloadFinished

  Event method that is called by TDownloadThread when download is finished. }

procedure TUpdate.OnDownloadFinished(Sender: TThread);
begin
  // Caption "finished"
  bFinished.Caption := FLang.GetString(62);
  bFinished.SetFocus;
  FThreadRuns := False;

  case FDownloadType of
    dtUpdate:
      begin
        // Caption "Search for update"
        (Owner as TMain).mmUpdate.Caption := FLang.GetString(11);
        (Owner as TMain).mmUpdate.Enabled := False;
      end;  //of begin

    dtCert:
      begin
      (Owner as TMain).mmDownloadCert.Enabled := False;
      TWinUtils.ShowAddRegistryDialog(FFileName);
      end;  //of begin
  end;  //of case
end;

{ private TUpdate.OnDownloading

  Event method that is called by TDownloadThread when download is in progress. }

procedure TUpdate.OnDownloading(Sender: TThread; const ADownloadSize: Integer);
begin
  pbProgress.Position := ADownloadSize;
  lSize.Caption := IntToStr(ADownloadSize) +'/'+ IntToStr(pbProgress.Max) +'KB';
end;

{ private TUpdate.OnDownloadStart

  Event method that is called by TDownloadThread when download starts. }

procedure TUpdate.OnDownloadStart(Sender: TThread; const AFileSize: Integer);
begin
  pbProgress.Max := AFileSize;
end;

{ public TUpdate.Download

  Starts downloading a file. }

procedure TUpdate.Download(ADownloadType: TDownloadType);
var
  Dir, Url: string;

begin
  FDownloadType := ADownloadType;

  // Show "Choose folder" dialog
  if SelectDirectory(FLang.GetString(59), '', Dir) then
     begin
     // Which download?
     case ADownloadType of
       dtUpdate:
         begin
         FFileName := Dir +'\SIT.exe';
         Url := URL_DIR +'downloader.php?file=sit.exe';
         end;  //of begin

       dtCert:
         begin
         FFileName := Dir + '\Install_PMCW_Cert.reg';
         Url := URL_DIR +'downloader.php?file=cert.reg';
         end;  //of begin
     end;  //of case

     // Try to init thread
     try
       with TDownloadThread.Create(Url, FFileName) do
       begin
         // Link download events
         FOnUserCancel := OnUserCancel;

         // Link TProgressBar events
         OnDownloading := Self.OnDownloading;
         OnCancel := OnDownloadCancel;
         OnStart := OnDownloadStart;
         OnFinish := OnDownloadFinished;
         OnError := OnDownloadError;

         // Start download thread
         Resume;
       end;  //of with

       // Caption "cancel"
       bFinished.Caption := FLang.GetString(61);
       FThreadRuns := True;

     except
       OnDownloadError(nil, 0);
     end  //of try
     end  //of begin
  else
     // Cancel clicked
     Reset;
end;

{ TUpdate.bFinishedClick

  Cancels download or closes update form when user clicks. }

procedure TUpdate.bFinishedClick(Sender: TObject);
begin
  Close;
end;

{ TUpdate.FormCloseQuery

  VCL event that is called before destructor is called. }

procedure TUpdate.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // Download still in progress?
  if FThreadRuns then
     begin
     // Cancel download
     FOnUserCancel(Self);
     CanClose := False;
     end  //of begin
  else
     // Close form
     CanClose := True;
end;

end.
