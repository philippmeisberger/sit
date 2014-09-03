{ *********************************************************************** }
{                                                                         }
{ PM Code Works Cross Plattform Update Check Thread v2.1                  }
{                                                                         }
{ Copyright (c) 2011-2014 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit UpdateCheckThread;

{$IFDEF LINUX} {$mode delphi}{$H+} {$ENDIF}

interface

uses
  Classes, SysUtils, IdHTTP;

const
  URL_DIR = 'http://www.pm-codeworks.de/media/';
  
type
  { Thread event }
  TOnUpdateAvailableEvent = procedure(Sender: TThread; const ANewBuild: Cardinal) of object;

  { TUpdateCheckThread }
  TUpdateCheckThread = class(TThread)
  private
    FHttp: TIdHTTP;
    FOnUpdate: TOnUpdateAvailableEvent;
    FOnNoUpdate, FOnError: TNotifyEvent;
    FCurBuild, FNewBuild: Cardinal;
    FRemoteDirName: string;
    { Synchronizable events }
    procedure DoNotifyOnError;
    procedure DoNotifyOnNoUpdate;
    procedure DoNotifyOnUpdate;
  protected
    procedure Execute; override;
  public
    constructor Create(ACurrentBuild: Cardinal; ARemoteDirName: string;
      ACreateSuspended: Boolean = True);
    destructor Destroy; override;
    { Externalized events }
    property OnError: TNotifyEvent read FOnError write FOnError;
    property OnNoUpdate: TNotifyEvent read FOnNoUpdate write FOnNoUpdate;
    property OnUpdate: TOnUpdateAvailableEvent read FOnUpdate write FOnUpdate;
  end;

implementation

{ TUpdateCheckThread }

{ public TUpdateCheckThread.Create

  Constructor for creating a TUpdateCheckThread instance. }

constructor TUpdateCheckThread.Create(ACurrentBuild: Cardinal;
  ARemoteDirName: string; ACreateSuspended: Boolean = True);
begin
  inherited Create(ACreateSuspended);
  
  // Thread deallocates his memory
  FreeOnTerminate := True;

  FCurBuild := ACurrentBuild;
  FRemoteDirName := ARemoteDirName;
  
  // Init IdHTTP component dynamically
  FHttp := TIdHTTP.Create(nil);
end;

{ public TUpdateCheckThread.Destroy

  Destructor for destroying a TUpdateCheckThread instance. }

destructor TUpdateCheckThread.Destroy;
begin
  FHttp.Free;
  inherited Destroy;
end;

{ protected TDownloadThread.Execute

  Thread main method that checks for update on an HTTP source. }
  
procedure TUpdateCheckThread.Execute;
var
  VersionUrl: string;
  
begin
  try
    // Download version file for application
    VersionUrl := URL_DIR + FRemoteDirName +'/version.txt';
    FNewBuild := StrToInt(FHttp.Get(VersionUrl));

    // Check if downloaded version is newer than current version
    if (FNewBuild > FCurBuild) then
      // Notify "update available"
      Synchronize(DoNotifyOnUpdate)
    else
      // Notify "no update available"
      Synchronize(DoNotifyOnNoUpdate);

  except
    Synchronize(DoNotifyOnError);
  end;  //of except
end;

{ private TDownloadThread.DoNotifyOnError

  Synchronizable event method that is called when error occurs while searching
  for update. }

procedure TUpdateCheckThread.DoNotifyOnError;
begin
  if Assigned(OnError) then
    OnError(Self);
end;

{ private TDownloadThread.DoNotifyOnNoUpdate

  Synchronizable event method that is called when search returns no update. }
  
procedure TUpdateCheckThread.DoNotifyOnNoUpdate;
begin
  if Assigned(OnNoUpdate) then
    OnNoUpdate(Self);
end;

{ private TDownloadThread.DoNotifyOnNoUpdate

  Synchronizable event method that is called when search search returns an
  update. }

procedure TUpdateCheckThread.DoNotifyOnUpdate;
begin
  if Assigned(OnUpdate) then
    OnUpdate(Self, FNewBuild);
end;

end.