{ *********************************************************************** }
{                                                                         }
{ SIT Download Unit                                                       }
{                                                                         }
{ Copyright (c) 2011-2012 P.Meisberger (PM Code Works)                    }
{                                                                         }
{ *********************************************************************** }

unit SitUpdate;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  ComCtrls, FileCtrl, SitAPI, SitUpdateThread, ShellApi, UpdateCheckThread,
  ExtCtrls;

type
  { Threadsteuerung Events }
  TOnCancelEvent = procedure(Sender: TObject) of object;
  TOnContinueEvent = procedure(Sender: TObject) of object;
  TOnExitEvent = procedure(Sender: TObject) of object;

  TForm3 = class(TForm)
    pbProgress: TProgressBar;
    bFinished: TButton;
    iBack: TImage;
    lSize: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bFinishedClick(Sender: TObject);
  private
    FOnCancel: TOnCancelEvent;
    FOnContinue: TOnContinueEvent;
    FOnExit: TOnExitEvent;
    FUpdate, FUpdateExists, FUserUpdate: Boolean;
    FFileName: string;
    sit: TSit;
    procedure Cancel;
    { Download-Thread Events }
    procedure OnThreadError(Sender: TThread);
    procedure OnThreadWork(Sender: TThread; const AWorkCount: Integer);
    procedure OnThreadWorkBegin(Sender: TThread; const AWorkCountMax: Integer);
    procedure OnThreadWorkEnd(Sender: TObject; AResponseCode: Integer);
    { Updatecheck-Thread Events }
    procedure OnCheckError(Sender: TThread);
    procedure OnNoUpdateAvailable(Sender: TThread);
    procedure OnUpdateAvailable(Sender: TThread; const ANewBuild: integer);
  public
    procedure CheckForUpdate;
    procedure Initialize(AFormCaption: string; AUpdate: Boolean);
    procedure DoUpdate;
    property OnCancel: TOnCancelEvent read FOnCancel write FOnCancel;
    property OnContinue: TOnContinueEvent read FOnContinue write FOnContinue;
    property OnExit: TOnExitEvent read FOnExit write FOnExit;
    property UpdateExists: Boolean read FUpdateExists;
    property UserUpdate: Boolean read FUserUpdate write FUserUpdate;
  end;

var
  Form3: TForm3;

implementation

uses SitMain;

{$R *.dfm}


procedure TForm3.Cancel;                                           //"Abbrechen"
begin
  DeleteFile(FFileName);                   //Datei löschen
  lSize.Caption := sit.GetString(61);
  bFinished.Caption := sit.GetString(62);
  bFinished.Enabled := true;

  if FUpdate then                          //falls Update-Fehler
     Form1.mmUpdate.Enabled := true
  else                                     //falls Zertifikat-Fehler
     Form1.mmDwnldCert.Enabled := true;
end;


procedure TForm3.FormActivate(Sender: TObject);                       //Download
var
  dir : string;

  function Rename(ADirectory, AName: string; AIndex: integer): string;   //Vermeidung von gleichen Dateinamen
  begin
    if FileExists(ADirectory + AName) then
       begin
       Inc(AIndex);
       AName := '\SIT('+IntToStr(AIndex)+').exe';       //umbenennen
       result := Rename(ADirectory, AName, AIndex);     //probieren
       end  //of begin
    else
       result := ADirectory + AName;                    //fertig
  end;  //of function

begin
  bFinished.Caption := sit.GetString(62);

  if SelectDirectory(sit.GetString(59), '', dir) then   //"Ordner wählen" - Dialog
     try
       with TDownloadThread.Create do             //init Thread (suspended!)
         begin
         OnWork := OnThreadWork;                  //Events verknüpfen
         OnWorkBegin := OnThreadWorkBegin;
         OnFinish := OnThreadWorkEnd;
         OnError := OnThreadError;

         if FUpdate then                          //Update oder Zertifikat
            begin
            FileName := Rename(dir, '\SIT.exe', 0);  //Dateinamen übergeben
            Url := URL_DIR +'downloader.php?file=sit.exe';
            end  //of begin
         else
            begin
            FileName := dir + '\Install_PMCW_Cert.reg';
            Url := URL_DIR +'downloader.php?file=cert.reg';
            end;  //of if

         FFileName := FileName;
         Resume;                                      //Thread starten
         end;  //of with

     except
       OnThreadError(Sender as TThread);              //im Fehlerfall
     end  //of except
  else                                                //"Abbrechen"-Klick
     Cancel;
end;


procedure TForm3.FormCreate(Sender: TObject);
begin
  inherited;
  FUpdateExists := false;
  FUserUpdate := false;
  CheckForUpdate;                                          //nach Update suchen
end;


procedure TForm3.bFinishedClick(Sender: TObject);                      //Fertig
begin
  if (bFinished.Caption = sit.GetString(62)) then  //falls Thread fertig...
     begin
     bFinished.Enabled := false;            //Button deaktivieren
     Hide;                                  //Download Form verstecken

     if (pbProgress.Position <> 0) then     //falls ProgressBar nicht auf 0...
        pbProgress.Position := 0;           //Reset ProgressBar

     lSize.Caption := '000/000KB';          //Reset Datenzählers
     Form1.Enabled := true;                 //Main Form aktivieren
     Form1.BringToFront;                    //Main Form zeigen
     end  //of begin
  else
     begin                                  //falls Thread nicht fertig...
     OnCancel(Sender);                      //Thread pausieren

     if (Form1.MessageBox(63, 41, MB_ICONQUESTION or MB_YESNO) = IDYes) then
        begin
        OnExit(Sender);                     //Thread beenden
        Cancel;
        end  //of begin
     else
        OnContinue(Sender);                 //Thread fortsetzen
     end;  //of if
end;

{ Download-Thread Events }
procedure TForm3.OnThreadError(Sender: TThread);                    //Fehlerfall
begin
  sit.CreateError(57, 50, 57);
  Cancel;
end;


procedure TForm3.OnThreadWork(Sender: TThread; const AWorkCount: Integer);
begin
  pbProgress.Position := AWorkCount;                      //Progress anzeigen
  lSize.Caption := IntToStr(Round(AWorkCount/1024)) +'/'+
                    FloatToStr(Round(pbProgress.Max/1024))+'KB';  //Dateigröße
end;


procedure TForm3.OnThreadWorkBegin(Sender: TThread; const AWorkCountMax: Integer);
begin
  pbProgress.Max := AWorkCountMax;
  bFinished.Caption := sit.GetString(60);
  bFinished.Enabled := true;
end;


procedure TForm3.OnThreadWorkEnd(Sender: TObject; AResponseCode: integer);
begin
  bFinished.Caption := sit.GetString(62);
  Form1.mmUpdate.Caption := sit.GetString(11);

  if not FUpdate and (lSize.Caption <> sit.GetString(61)) then         //falls Zertifikat heruntergeladen wurde...
     ShellExecute(0, nil, PChar('regedit'), PChar(ExtractFileName(FFileName)),
                  PChar(ExtractFilePath(FFileName)), SW_SHOWNORMAL);  //Dialog: Datei in REG einbinden?
end;

{ Updatecheck-Thread Events }
procedure TForm3.OnCheckError(Sender: TThread);
begin
  if FUserUpdate then
     Form1.MessageBox(sit.GetString(68) +^J+ sit.GetString(69), 57,
                      MB_ICONERROR);
end;


procedure TForm3.OnNoUpdateAvailable(Sender: TThread);
begin
  if FUserUpdate then
     Form1.MessageBox(54, 57, MB_ICONINFORMATION);
end;


procedure TForm3.OnUpdateAvailable(Sender: TThread; const ANewBuild: integer);
begin
  FUpdateExists := true;

  if (Form1.MessageBox(sit.GetString(55) +^J+ sit.GetString(56), 41,
     MB_ICONQUESTION or MB_YESNO) = IDYes) then
     DoUpdate
  else
     Form1.mmUpdate.Caption := sit.GetString(58);
end;
{ of Threads }

{ public }
procedure TForm3.Initialize(AFormCaption: string; AUpdate: Boolean);
begin
  Caption := AFormCaption;
  FUpdate := AUpdate;
  Show;
  BringToFront;
end;


procedure TForm3.CheckForUpdate;                            //nach Update suchen
begin
  sit := Form1.SitProp;                                     //init Instanz

  with TUpdateCheckThread.Create(sit.GetBuildNumber) do     //init Thread (suspended!)
    begin
    OnUpdate := OnUpdateAvailable;                          //Events verknüpfen
    OnNoUpdate := OnNoUpdateAvailable;
    OnError := OnCheckError;
    Resume;                                                 //Thread starten
    end;  //of with
end;


procedure TForm3.DoUpdate;                                      //Update starten
begin
  Form1.mmUpdate.Enabled := false;
  Form1.Enabled := false;
  Initialize(sit.GetString(57), true);
end;  //of begin

end.
