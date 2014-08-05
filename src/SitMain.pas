{ *********************************************************************** }
{                                                                         }
{ SIT Main Unit                                                           }
{                                                                         }
{ Copyright (c) 2011-2013 P.Meisberger (PM Code Works)                    }
{                                                                         }
{ *********************************************************************** }

unit SitMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Menus, SitAPI, ExtDlgs, ShellAPI, FileCtrl, LanguageFile;

type
  { TMain }
  TMain = class(TForm)
    iBack: TImage;
    lCopy: TLabel;
    lVersion: TLabel;
    Image: TImage;
    bAccept: TButton;
    bShowSupport: TButton;
    MainMenu: TMainMenu;
    mmFile: TMenuItem;
    mmImport: TMenuItem;
    mmExport: TMenuItem;
    mmEdit: TMenuItem;
    mmShowValues: TMenuItem;
    mmDelValues: TMenuItem;
    mmHelp: TMenuItem;
    mmInfo: TMenuItem;
    mmExportEdit: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    mmDelEdit: TMenuItem;
    mmDelLogo: TMenuItem;
    mmDwnldCert: TMenuItem;
    mmUpdate: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    mmView: TMenuItem;
    mmLang: TMenuItem;
    mmGer: TMenuItem;
    mmEng: TMenuItem;
    mmFra: TMenuItem;
    mmReport: TMenuItem;
    mmCopyIcon: TMenuItem;
    N5: TMenuItem;
    gbInfo: TGroupBox;
    eHours: TLabeledEdit;
    eModel: TLabeledEdit;
    ePhone: TLabeledEdit;
    eMan: TLabeledEdit;
    eUrl: TLabeledEdit;
    gbIcon: TGroupBox;
    bAdd: TButton;
    cbCopyIcon: TCheckBox;
    eLogo: TLabeledEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure bAcceptClick(Sender: TObject);
    procedure bAddClick(Sender: TObject);
    procedure mmImportClick(Sender: TObject);
    procedure mmCopyIconClick(Sender: TObject);
    procedure mmExportClick(Sender: TObject);
    procedure mmExportEditClick(Sender: TObject);
    procedure mmShowValuesClick(Sender: TObject);
    procedure mmDelValuesClick(Sender: TObject);
    procedure mmDelEditClick(Sender: TObject);
    procedure mmDelLogoClick(Sender: TObject);
    procedure mmGerClick(Sender: TObject);
    procedure mmEngClick(Sender: TObject);
    procedure mmFraClick(Sender: TObject);
    procedure mmUpdateClick(Sender: TObject);
    procedure mmDwnldCertClick(Sender: TObject);
    procedure mmReportClick(Sender: TObject);
    procedure mmInfoClick(Sender: TObject);
    procedure eHoursDblClick(Sender: TObject);
    procedure eLogoDblClick(Sender: TObject);
    procedure eManDblClick(Sender: TObject);
    procedure eModelDblClick(Sender: TObject);
    procedure ePhoneDblClick(Sender: TObject);
    procedure eUrlDblClick(Sender: TObject);
    procedure lCopyClick(Sender: TObject);
    procedure lCopyMouseEnter(Sender: TObject);
    procedure lCopyMouseLeave(Sender: TObject);
    procedure bShowSupportClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);   
  private
    FSupportInfo: TSupportInformationBase;
    FLang: TLanguageFile;
    FUpdateCheck: TUpdateCheck;
    procedure DoExport(ADirect: Boolean);
    function GetLangID: Word;
    function OpenLogo(): Boolean;
    procedure Refresh;
    procedure SetLanguage(ALangID: Word);
  public
    procedure ChangeLanguage(AMenuItem: TMenuItem; ALangID: Word);
  end;

var
  Form1: TMain;

implementation

uses SitInfo, SitUpdate;

{$R *.dfm}
{$R manifest.res}

{ private TMain.DoExport

  Allows users to export support information as *.reg or *.ini file. }
  
procedure TMain.DoExport(ADirect: Boolean);
var
  data: TSupportInformationBase;
  saveDialog : TSaveDialog;

begin
  saveDialog := TSaveDialog.Create(Self);

  with saveDialog do
  begin
    Title := FLang.GetString(23);
    FileName := FLang.GetString(25);
    Options := Options + [ofOverwritePrompt];
  end;  //of with

  if ADirect then
     data := FSupportInfo
  else
     data := TSupportInformationBase.Create(eHours.Text, eLogo.Text, eMan.Text,
             eModel.Text, ePhone.Text, eUrl.Text);
  try
    if TSupportInformationBase.CheckWindows() then
    begin
      saveDialog.Filter := FLang.GetString(26);
      saveDialog.FilterIndex := 2;
    end  //of begin
    else
      saveDialog.Filter := FLang.GetString(27);

    if saveDialog.Execute then
      case saveDialog.FilterIndex of
        1: FSupportInfo.SaveAsIni(data, saveDialog.FileName);
        2: FSupportInfo.SaveAsReg(data, saveDialog.FileName);
      end; //of case

  finally
    data := nil;
    saveDialog.Free;
  end;  //of finally
end;

{ private TMain.OpenLogo

  Allows users to search for a support information icon in *.bmp format. }

function TMain.OpenLogo(): Boolean;
var
  OpenLogoDialog : TOpenPictureDialog;

begin
  result := True;
  OpenLogoDialog := TOpenPictureDialog.Create(Self);

  with OpenLogoDialog do
  begin
    Options := Options + [ofFileMustExist];
    Filter := FLang.GetString(28);
    Title := FLang.GetString(24);

    if ((eLogo.Text <> '') and FileExists(eLogo.Text)) then
       begin
       InitialDir := ExtractFilePath(eLogo.Text);
       FileName := ExtractFileName(eLogo.Text);
       end  //of begin
    else
       InitialDir := GetCurrentDir;
  end;  //of with

  try
    if OpenLogoDialog.Execute then
    begin
      Image.Picture.LoadFromFile(OpenLogoDialog.FileName);

      // Check resolution > 400 x 500
      if ((Image.Height > 400) or (Image.Width > 500)) then
      begin
        MessageBox(FLang.GetString(31)+ IntToStr(Image.Height) +'x'+
                   IntToStr(Image.Width) +')!' +^J+ FLang.GetString(32), mtWarning);
        result := False;
      end  //of begin
      else
        eLogo.Text := OpenLogoDialog.FileName;
    end; //of if

  finally
    OpenLogoDialog.Free;
    eLogo.SetFocus;
  end;  //of finally
end;

{ private TMain.Refresh

  Refreshs all text fields. }

procedure TMain.Refresh();
begin
  with FSupportInfo do
  begin
    eLogo.Text := Icon;
    eMan.Text := Man;
    eModel.Text := Model;
    eUrl.Text := Url;
    ePhone.Text := Phone;
    eHours.Text := Hours;
  end;  //of with

  mmDelLogo.Enabled := FileExists(FSupportInfo.Icon);
  mmDelLogo.Visible := mmDelLogo.Enabled;
end;

{ private TMain.SetLanguage

  Updates all component captions with new language text. }

procedure TMain.SetLanguage(ALangID: Word);
begin
  FLang.LangID := ALangID;

  with FLang do
    begin
    // Set captions for TMenuItems
    mmFile.Caption := GetString(0);
    mmImport.Caption := GetString(1);
    mmExport.Caption := GetString(2);
    mmExportEdit.Caption := GetString(3);

    mmEdit.Caption := GetString(4);
    mmShowValues.Caption := GetString(5);
    mmDelValues.Caption := GetString(6);
    mmDelEdit.Caption := GetString(7);
    mmCopyIcon.Caption := GetString(75);
    mmDelLogo.Caption := GetString(8);

    mmView.Caption := GetString(9);
    mmLang.Caption := GetString(10);

    mmHelp.Caption := GetString(66);
    mmUpdate.Caption := GetString(11);
    mmDwnldCert.Caption := GetString(12);
    mmReport.Caption := GetString(70);
    mmInfo.Caption := GetString(13);

    // Set captions for labels
    gbIcon.Caption := GetString(76);
    eLogo.EditLabel.Caption := GetString(14);
    cbCopyIcon.Caption := mmCopyIcon.Caption;

    gbInfo.Caption := GetString(39);
    eMan.EditLabel.Caption := GetString(15);
    ePhone.EditLabel.Caption := GetString(16);
    eHours.EditLabel.Caption := GetString(17);
    eModel.EditLabel.Caption := GetString(18);
    eUrl.EditLabel.Caption := GetString(19);

    // Set captions for buttons
    bAccept.Caption := GetString(20);
    bShowSupport.Caption := GetString(21);

    Form3.bFinished.Caption := GetString(62);
    Form2.Caption := GetString(67);
    end;  //of with
end;

{ TMain.FormCreate }

procedure TMain.FormCreate(Sender: TObject);
begin
  // German language default
  FLang := TLanguageFile.Create(100, Application);

  // Init update notificator
  FUpdateCheck := TUpdateCheck.Create(Self, FLang);

  // Init support information instance
  if TSupportInformationBase.CheckWindows() then
    FSupportInfo := TSupportInformation.Create
  else
    FSupportInfo := TSupportInformationXP.Create;
end;

{ TMain.FormDestroy }

procedure TMain.FormDestroy(Sender: TObject);
begin
  FSupportInfo.Free;
  FUpdateCheck.Free;
  FLang.Free;
end;

{ TMain.FormShow }

procedure TMain.FormShow(Sender: TObject);
const
  BCM_FIRST = $1600;
  BCM_SETSHIELD = BCM_FIRST + $000C;

var
  windows: string;
  newWindows: Boolean;

begin
  windows := TSupportInformationBase.GetWinVersion();
  newWindows := TSupportInformationBase.CheckWindows();
  cbCopyIcon.Enabled := newWindows;
  
  // Check for incompatibility
  if not (newWindows or (windows[1] in ['X','2'])) then
     begin
     MessageBox(FLang.GetString(64) + windows + FLang.GetString(65), mtError);
     bAccept.Enabled := false;
     mmFile.Enabled := false;
     mmEdit.Enabled := false;
     eLogo.Enabled := false;
     eMan.Enabled := false;
     ePhone.Enabled := false;
     eHours.Enabled := false;
     eModel.Enabled := false;
     eUrl.Enabled := false;
     bShowSupport.Enabled := false;
     Exit();
     end;  //of begin

  // Show support information
  mmShowValues.Click;

  // Make UAC-Shield button
  SendMessage(bAccept.Handle, BCM_SETSHIELD, 0, integer(True)); 
end;

{ TMain.bAcceptClick

  Allows user to commit changes on support information. }

procedure TMain.bAcceptClick(Sender: TObject);
begin
  if (MessageBox(33, mtQuestion) = IDYES) then
  try
    if cbCopyIcon.Checked then
      mmCopyIcon.Click;

    with FSupportInfo do
    begin
      // Icon exists?
      if ((eLogo.Text = '') xor (FileExists(eLogo.Text))) then
        Icon := eLogo.Text
      else
      begin
        MessageBox(72, mtWarning);
        eLogo.SetFocus;
        Exit();
      end;  //of if

      if (eMan.Text <> '') then
      begin
        Phone := ePhone.Text;
        Hours := eHours.Text;
        Manufacturer := eMan.Text;
        Model := eModel.Text;
        Url := eUrl.Text;
        Save();
      end  //of begin
      else
      begin
        MessageBox(53, mtWarning);
        eMan.SetFocus;
        Exit;
      end;  //of if
    end;  //of with

    MessageBox(38);
    mmShowValues.Click;

  except
    FLang.MessageBox(71, mtError);
  end;  //of except
end;

{ TMain.bAddClick

  Allows users to add a support information icon. }

procedure TMain.bAddClick(Sender: TObject);
begin
  if not OpenLogo() then
    OpenLogo();
end;

{ TMain.mmImportClick

  Allows users to import support information as *.reg or *.ini file. }

procedure TMain.mmImportClick(Sender: TObject);
var
  openDialog : TOpenDialog;

begin
  openDialog := TOpenDialog.Create(Self);

  with openDialog do
  begin
    Title := FLang.GetString(22);
    Options := Options + [ofFileMustExist];

    if TSit.CheckWindows then
       begin
       Filter := FLang.GetString(26);
       FilterIndex := 2;
       end  //of begin
    else
       Filter := FLang.GetString(27);
  end;  //of with  

  try
    if openDialog.Execute then
       begin
       Caption := Application.Title + TSit.GetArchitecture +' - '+ ExtractFileName(openDialog.FileName);

       case openDialog.FilterIndex of
         1: FSupportInfo.LoadFromIni(openDialog.FileName);
         2: FSupportInfo.LoadFromReg(openDialog.FileName);
       end;  //of case

       Refresh();
       end;  //of begin

  finally
    openDialog.Free;
  end;  //of finally
end;

{ TMain.mmExportClick

  Allows users to export the stored support information. }

procedure TMain.mmExportClick(Sender: TObject);
begin
  DoExport(True);
end;

{ TMain.mmExportEditClick

  Allows users to export the entered content of the text fields. }

procedure TMain.mmExportEditClick(Sender: TObject);
begin
  if ((eLogo.Text = '') and (eMan.Text = '') and (eModel.Text = '') and
     (eUrl.Text = '') and (ePhone.Text = '') and (eHours.Text = '')) then
     FLang.MessageBox(53, mtWarning)
  else
     DoExport(False);
end;

{ TMain.mmShowValuesClick

  Allows users to load and show support information. }

procedure TMain.mmShowValuesClick(Sender: TObject);
begin
  mmDelEdit.Click;

  try
    FSupportInfo.Load();
    Refresh();
    mmDelValues.Enabled := FSupportInfo.DataExists();
    mmExport.Enabled := mmDelValues.Enabled;

  except
    FLang.MessageBox(46, mtError);
  end; //of except
end;

{ TMain.mmDelValuesClick

  Allows users to delete support information. }

procedure TMain.mmDelValuesClick(Sender: TObject);
begin
  if (FLang.MessageBox(34, mtQuestion) = IDYES) then
     begin
     mmDelLogo.Click;

     if (FLang.MessageBox(35, mtQuestion) = IDYES) then
        DoExport(true);

     if FSupportInfo.Delete then
        begin
        mmDelValues.Enabled := false;
        FLang.MessageBox(37);
        end  //of begin
     else
        FLang.MessageBox(44, mtError);
     end;  //of begin
end;

{ TMain.mmDelEditClick

  Allows users to clear all text fields. }

procedure TMain.mmDelEditClick(Sender: TObject);
begin
  Caption := Application.Title + TSit.GetArchitecture();
  mmDelLogo.Enabled := false;
  mmDelLogo.Visible := mmDelLogo.Enabled;
  //FSupportInfo.Clear;
  eLogo.Clear;
  eMan.Clear;
  eModel.Clear;
  ePhone.Clear;
  eHours.Clear;
  eUrl.Clear;
  eMan.SetFocus;
end;

{ TMain.mmCopyIconClick

  Allows users to copy a icon in *.bmp format. }

procedure TMain.mmCopyIconClick(Sender: TObject);               //Logo kopieren
var
  dir: string;

begin
  if FileExists(eLogo.Text) then                          //existiert Logo
     if (ExtractFileExt(eLogo.Text) <> '.bmp') then       //Endung ".bmp"?
        FLang.MessageBox(77, mtWarning)
     else
        begin
		    if SelectDirectory(FLang.GetString(59), '', dir) then  //"Ordner wählen"
           begin
           dir := dir +'\'+ ExtractFileName(eLogo.Text);

           if CopyFile(PChar(eLogo.Text), PChar(dir), true) then  //Logo kopieren
              begin
              FLang.MessageBox(FLang.GetString(73) + dir + FLang.GetString(74));
              eLogo.Text := dir;                          //neuer Logo-Pfad
              end  //of begin
           else
              FLang.MessageBox(71, mtError);
           end;  //of begin
		end  //of begin
  else
    FLang.MessageBox(72, mtWarning);
end;

{ TMain.mmDelLogoClick

  Allows users to delete the support information icon. }

procedure TMain.mmDelLogoClick(Sender: TObject);
begin
  if FileExists(FSupportInfo.GetOEMLogo()) then
    if (FLang.MessageBox(36, mtQuestion) = IDYES) then
      if FSupportInfo.DeleteIcon() then
      begin
        mmDelLogo.Visible := false;
        mmDelLogo.Enabled := false;
        eLogo.Clear;
      end  //of begin
      else
        FLang.MessageBox(45, mtError);
end;


procedure TMain.mmGerClick(Sender: TObject);
begin
  ChangeLanguage(mmGer, 100);
end;


procedure TMain.mmEngClick(Sender: TObject);
begin
  ChangeLanguage(mmEng, 200);
end;


procedure TMain.mmFraClick(Sender: TObject);
begin
  ChangeLanguage(mmFra, 300);
end;


{ TMain.mmLangClick

  MainMenu entry that allows to change the current language. }
{
procedure TMain.mmLangClick(Sender: TObject);
begin
  SetLanguage((Sender as TMenuItem).Caption);
end;}

{ TMain.mmDwnldCertClick

  MainMenu entry that allows to download the PM Code Works certificate. }

procedure TMain.mmDownloadCertClick(Sender: TObject);
begin
  // Certificate already installed?
  if (TGameWake.PMCertExists() and (FLang.MessageBox(FLang.GetString(67) +^J
     + FLang.GetString(68), mtQuestion) = IDYES)) then
     // Download certificate
     with TUpdate.Create(Self, FLang, FLang.GetString(8)) do
       Download(dtCert);
end;

{ TMain.mmUpdateClick

  MainMenu entry that allows users to manually search for updates. }

procedure TMain.mmUpdateClick(Sender: TObject);
begin
  FUpdateCheck.CheckForUpdate(True);
end;

{ TMain.mmReportClick

  MainMenu entry that allows users to easily report a bug by opening the web
  browser and using the "report bug" formular. }

procedure TMain.mmReportClick(Sender: TObject);
begin
  TSupportInformationBase.OpenUrl(URL_CONTACT);
end;

{ TMain.mmInfoClick

  MainMenu entry that shows a info page with build number and version history. }

procedure TMain.mmInfoClick(Sender: TObject);
var
  Info: TInfo;

begin
  Application.CreateForm(TInfo, Info);
  Info.ShowModal;
  Info.Free;
end;

procedure TMain.eLogoDblClick(Sender: TObject);
begin
  if (eLogo.Text = '') then
     OpenLogo
  else
     eLogo.SelectAll;
end;

procedure TMain.eManDblClick(Sender: TObject);
begin
  eMan.SelectAll;
end;

procedure TMain.eModelDblClick(Sender: TObject);
begin
  eModel.SelectAll;
end;

procedure TMain.ePhoneDblClick(Sender: TObject);
begin
  ePhone.SelectAll;
end;

procedure TMain.eHoursDblClick(Sender: TObject);
begin
  eHours.SelectAll;
end;

procedure TMain.eUrlDblClick(Sender: TObject);
begin
  eUrl.SelectAll;
end;

{ TMain.lCopyClick

  Opens the homepage of PM Code Works in a web browser. }

procedure TMain.lCopyClick(Sender: TObject);
begin
  TSupportInformationBase.OpenUrl(URL_BASE);
end;

{ TMain.lCopyMouseEnter

  Allows a label to have the look like a hyperlink.  }

procedure TMain.lCopyMouseEnter(Sender: TObject);
begin
  with (Sender as TLabel) do
    begin
    Font.Style := Font.Style + [fsUnderline];
    Font.Color := clBlue;
    Cursor := crHandPoint;
    end;  //of with
end;

{ TMain.lCopyMouseLeave

  Allows a label to have the look of a normal label again. }

procedure TMain.lCopyMouseLeave(Sender: TObject);
begin
  with (Sender as TLabel) do
    begin
    Font.Style := Font.Style - [fsUnderline];
    Font.Color := clBlack;
    Cursor := crDefault;
    end;  //of with
end;


procedure TMain.bCloseClick(Sender: TObject);
begin
  Close;
end;


procedure TMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Assigned(Form2) then
     Form2.Close;
  Form3.Close;
end;

end.
