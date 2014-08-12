{ *********************************************************************** }
{                                                                         }
{ PM Code Works Cross Plattform Update VCL v2.1                           }
{                                                                         }
{ Copyright (c) 2011-2014 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit UpdateForm;

{$IFDEF LINUX} {$mode objfpc}{$H+} {$ENDIF}

interface

uses
  SysUtils, Classes, UpdateCheckThread, DownloadThread, LanguageFile, OSUtils,

{$IFDEF MSWINDOWS}
  Windows, FileCtrl, Forms, StdCtrls, ComCtrls, Controls, Graphics,
  ExtCtrls;
{$ELSE}
  LCLType;
{$ENDIF}

type
  { Events }
  TOnUpdateEvent = procedure(Sender: TObject; ANewBuild: Cardinal) of object;

  { TUpdateCheck }
  TUpdateCheck = class(TObject)
  private
    FLang: TLanguageFile;
    FUserUpdate: Boolean;
    FRemoteDirName: string;
    FNewBuild: Cardinal;
    FOnUpdate: TOnUpdateEvent;
    { TUpdateCheckThread events }
    procedure OnCheckError(Sender: TThread);
    procedure OnNoUpdateAvailable(Sender: TThread);
    procedure OnUpdateAvailable(Sender: TThread; const ANewBuild: Cardinal);
  public
    constructor Create(ARemoteDirName: string; ALang: TLanguageFile);
    procedure CheckForUpdate(AUserUpdate: Boolean);
    { external }
    property OnUpdate: TOnUpdateEvent read FOnUpdate write FOnUpdate;
  end;

  { Events }
  TOnUserCancelEvent = procedure(Sender: TObject) of object;
  TOnUpdateFinishEvent = procedure(Sender: TObject; AFileName: string) of object;

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
    FRemoteFileName, FLocalFileName, FFileName: string;
    FLang: TLanguageFile;
    FOnFinish: TOnUpdateFinishEvent;
    procedure Reset();
    { TDownloadThread events }
    procedure OnDownloadCancel(Sender: TThread);
    procedure OnDownloadError(Sender: TThread; AResponseCode: Integer);
    procedure OnDownloadFinished(Sender: TThread);
    procedure OnDownloading(Sender: TThread;
      const ADownloadSize: {$IFDEF MSWINDOWS}Integer{$ELSE}Int64{$ENDIF});
    procedure OnDownloadStart(Sender: TThread;
      const AFileSize: {$IFDEF MSWINDOWS}Integer{$ELSE}Int64{$ENDIF});
  public
    constructor Create(AOwner: TComponent; ALangFile: TLanguageFile;
      AFormCaption: string = ''); overload;
    constructor Create(AOwner: TComponent; ALangFile: TLanguageFile;
      ARemoteFileName, ALocalFileName: string; AFormCaption: string = ''); overload;
    procedure Download(); overload;
    procedure Download(ARemoteFileName, ALocalFileName: string); overload;
    procedure DownloadCertificate();
    { external }
    property OnUpdateFinish: TOnUpdateFinishEvent read FOnFinish write FOnFinish;
  end;

implementation

{$IFDEF MSWINDOWS}
{$R *.dfm}
{$ENDIF}

{ TUpdateCheck }

{ public TUpdateCheck.Create

  Constructor for creating an TUpdateCheck instance. }

constructor TUpdateCheck.Create(ARemoteDirName: string; ALang: TLanguageFile);
begin
  inherited Create;
  FLang := ALang;
  FRemoteDirName := ARemoteDirName;
end;

{ private TUpdateCheck.OnCheckError

  Event method that is called TUpdateCheckThread when error occurs while
  searching for update. }

procedure TUpdateCheck.OnCheckError(Sender: TThread);
begin
  if FUserUpdate then
    with FLang do
      MessageBox(GetString(12) +^J+ GetString(13), mtError, True);
end;

{ private TUpdateCheck.OnNoUpdateAvailable

  Event method that is called when TUpdateCheckThread search returns no update. }

procedure TUpdateCheck.OnNoUpdateAvailable(Sender: TThread);
begin
  if FUserUpdate then
    FLang.MessageBox(23, mtInfo, True);
end;

{ private TUpdateCheck.OnUpdateAvailable

  Event method that is called when TUpdateCheckThread search returns an update. }

procedure TUpdateCheck.OnUpdateAvailable(Sender: TThread; const ANewBuild: Cardinal);
begin
  if (FNewBuild <> ANewBuild) then
    // Store newest build
    FNewBuild := ANewBuild;

  // Notify user
  FOnUpdate(Self, FNewBuild);
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
  with TUpdateCheckThread.Create(TOSUtils.GetBuildNumber(), FRemoteDirName) do
  begin
  {$IFDEF MSWINDOWS}
    OnUpdate := OnUpdateAvailable;
    OnNoUpdate := OnNoUpdateAvailable;
    OnError := OnCheckError;
    Resume;
  {$ELSE}
    OnUpdate := @OnUpdateAvailable;
    OnNoUpdate := @OnNoUpdateAvailable;
    OnError := @OnCheckError;
    Start;
  {$ENDIF}
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

{ public TUpdate.Create

  Constructor for creating an TUpdate instance. }

constructor TUpdate.Create(AOwner: TComponent; ALangFile: TLanguageFile;
  ARemoteFileName, ALocalFileName: string; AFormCaption: string = '');
begin
  Create(AOwner, ALangFile, AFormCaption);
  FRemoteFileName := ARemoteFileName;
  FLocalFileName := ALocalFileName;
end;

{ private TUpdate.Reset

  Resets Update GUI. }

procedure TUpdate.Reset();
begin
  // Reset ProgressBar
  pbProgress.Position := 0;

  lSize.Caption := FLang.GetString(7);
  bFinished.Caption := FLang.GetString(8);
  FThreadRuns := False;
end;

{ private TUpdate.OnDownloadCancel

  Event method that is called by TDownloadThread when user canceled downlad. }
  
procedure TUpdate.OnDownloadCancel(Sender: TThread);
begin
  Reset();
  FLang.MessageBox(30, mtInfo);
end;

{ private TUpdate.OnDownloadError

  Event method that is called by TDownloadThread when an error occurs while
  downloading the update. }

procedure TUpdate.OnDownloadError(Sender: TThread; AResponseCode: Integer);
begin
  with FLang do
    MessageBox(Caption + GetString(18) +^J+^J+ GetString(19) + GetString(20), mtError, True);

  Reset();
end;

{ private TUpdate.OnDownloadFinished

  Event method that is called by TDownloadThread when download is finished. }

procedure TUpdate.OnDownloadFinished(Sender: TThread);
begin
  // Caption "finished"
  bFinished.Caption := FLang.GetString(8);
  bFinished.SetFocus;
  FThreadRuns := False;

{$IFDEF MSWINDOWS}
  // Show dialog to add certificate
  if (ExtractFileExt(FFileName) = '.reg') then
    if TOSUtils.ShowAddRegistryDialog(FFileName) then
      DeleteFile(PChar(FFileName));
{$ENDIF}

  // Notify main form
  FOnFinish(Self, FFileName);
end;

{ private TUpdate.OnDownloading

  Event method that is called by TDownloadThread when download is in progress. }

procedure TUpdate.OnDownloading(Sender: TThread;
  const ADownloadSize: {$IFDEF MSWINDOWS}Integer{$ELSE}Int64{$ENDIF});
begin
  pbProgress.Position := ADownloadSize;
  lSize.Caption := IntToStr(ADownloadSize) +'/'+ IntToStr(pbProgress.Max) +'KB';
end;

{ private TUpdate.OnDownloadStart

  Event method that is called by TDownloadThread when download starts. }

procedure TUpdate.OnDownloadStart(Sender: TThread;
  const AFileSize: {$IFDEF MSWINDOWS}Integer{$ELSE}Int64{$ENDIF});
begin
  pbProgress.Max := AFileSize;
end;

{ public TUpdate.Download

  Starts downloading a file. }

procedure TUpdate.Download();
begin
  if ((FRemoteFileName = '') or (FLocalFileName = '')) then
    raise Exception.Create('Missing argument: "RemoteFileName" or "LocalFileName"!');

  Download(FRemoteFileName, FLocalFileName);
end;

{ public TUpdate.Download

  Starts downloading a file. }

procedure TUpdate.Download(ARemoteFileName, ALocalFileName: string);
var
  Dir, Url: string;

begin
  FRemoteFileName := ARemoteFileName;
  FLocalFileName := ALocalFileName;

  // Show "Choose folder" dialog
  if SelectDirectory(FLang.GetString(9), '', Dir) then
  begin
    Url := URL_DIR +'downloader.php?file='+ FRemoteFileName;
    FFileName := IncludeTrailingPathDelimiter(Dir) + FLocalFileName;

    // Try to init thread
    try
      with TDownloadThread.Create(Url, FFileName) do
      begin
        // Link download events
        FOnUserCancel := OnUserCancel;

        // Link TProgressBar events and start download thread
      {$IFDEF MSWINDOWS}
        OnDownloading := Self.OnDownloading;
        OnCancel := OnDownloadCancel;
        OnStart := OnDownloadStart;
        OnFinish := OnDownloadFinished;
        OnError := OnDownloadError;
        Resume;
      {$ELSE}
        OnDownloading := @Self.OnDownloading;
        OnCancel := @OnDownloadCancel;
        OnStart := @OnDownloadStart;
        OnFinish := @OnDownloadFinished;
        OnError := @OnDownloadError;
        Start;
      {$ENDIF}        
      end;  //of with

      // Caption "cancel"
      bFinished.Caption := FLang.GetString(6);
      FThreadRuns := True;

    except
      OnDownloadError(nil, 0);
    end;  //of try
  end  //of begin
  else
    // Cancel clicked
    Reset;
end;

{ TUpdate.DownloadCertificate

  Starts downloading the PMCW certificate. }

procedure TUpdate.DownloadCertificate();
begin
  Download('cert.reg', 'Install_PMCW_Cert.reg');
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
