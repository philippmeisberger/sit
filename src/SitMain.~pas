{ *********************************************************************** }
{                                                                         }
{ SIT Main Unit                                                           }
{                                                                         }
{ Copyright (c) 2011-2014 P.Meisberger (PM Code Works)                    }
{                                                                         }
{ *********************************************************************** }

unit SitMain;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls, Menus, ExtDlgs, FileCtrl, LanguageFile, OSUtils, Updater, SitAPI,
  SitInfo;

type
  { TMain }
  TMain = class(TForm, IChangeLanguageListener, IUpdateListener)
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
    mmDownloadCert: TMenuItem;
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
    procedure bShowSupportClick(Sender: TObject);
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
    procedure mmDownloadCertClick(Sender: TObject);
    procedure mmReportClick(Sender: TObject);
    procedure mmInfoClick(Sender: TObject);
    procedure eLogoDblClick(Sender: TObject);
    procedure lCopyClick(Sender: TObject);
    procedure lCopyMouseEnter(Sender: TObject);
    procedure lCopyMouseLeave(Sender: TObject);
    procedure eUrlDblClick(Sender: TObject);
  private
    FSupportInfo: TSupportInformationBase;
    FLang: TLanguageFile;
    FUpdateCheck: TUpdateCheck;
    procedure AfterUpdate(Sender: TObject; ADownloadedFileName: string);
    procedure BeforeUpdate(Sender: TObject; const ANewBuild: Cardinal);
    function CopyIcon(): Boolean;
    procedure DoExport(ADirect: Boolean);
    procedure Refresh();
    procedure SetLanguage(Sender: TObject);
  end;

var
  Main: TMain;

implementation

{$R *.dfm}
{$R manifest.res}

{ TMain.FormCreate

  VCL event that is called when form is being created. }

procedure TMain.FormCreate(Sender: TObject);
begin
  // German language default
  FLang := TLanguageFile.Create(100, Application);
  FLang.AddListener(Self);
  SetLanguage(Self);

  // Init update notificator
  FUpdateCheck := TUpdateCheck.Create(Self, 'SIT', FLang);

  // Check for update on startup
  FUpdateCheck.CheckForUpdate(False);

  // Init support information instance
  if TOSUtils.CheckWindows() then
    FSupportInfo := TSupportInformation.Create
  else
    FSupportInfo := TSupportInformationXP.Create;
end;

{ TMain.FormDestroy

  VCL event that is called when form is being destroyed. }

procedure TMain.FormDestroy(Sender: TObject);
begin
  FSupportInfo.Free;
  FUpdateCheck.Free;
  FLang.Free;
end;

{ TMain.FormShow

  VCL event that is called when form is shown. }

procedure TMain.FormShow(Sender: TObject);
const
  BCM_FIRST = $1600;
  BCM_SETSHIELD = BCM_FIRST + $000C;

var
  windows: string;
  newWindows: Boolean;

begin
  windows := TOSUtils.GetWinVersion();
  newWindows := TOSUtils.CheckWindows();
  cbCopyIcon.Enabled := newWindows;
  
  // Check for incompatibility
  if not (newWindows or (windows[1] in ['X','2'])) then
  begin
    Flang.MessageBox(FLang.GetString(74) + windows + FLang.GetString(75), mtError);
    bAccept.Enabled := False;
    mmFile.Enabled := False;
    mmEdit.Enabled := False;
    eLogo.Enabled := False;
    eMan.Enabled := False;
    ePhone.Enabled := False;
    eHours.Enabled := False;
    eModel.Enabled := False;
    eUrl.Enabled := False;
    bShowSupport.Enabled := False;
    Exit;
  end;  //of begin

  // Show support information
  mmShowValues.Click;

  // Make UAC-Shield button
  SendMessage(bAccept.Handle, BCM_SETSHIELD, 0, integer(True)); 
end;

{ private TMain.AfterUpdate

  Event method that is called by TUpdate when download is finished. }

procedure TMain.AfterUpdate(Sender: TObject; ADownloadedFileName: string);
begin
  if (ExtractFileExt(ADownloadedFileName) <> '.reg') then
  begin
    // Caption "Search for update"
    mmUpdate.Caption := FLang.GetString(15);
    mmUpdate.Enabled := False;
  end  //of begin
  else
    mmDownloadCert.Enabled := False;
end;

{ private TMain.BeforeUpdate

  Event that is called by TUpdateCheck when TUpdateCheckThread finds an update. }

procedure TMain.BeforeUpdate(Sender: TObject; const ANewBuild: Cardinal);
begin
  // Show dialog: Ask for permitting download
  with FLang do
    if (MessageBox(Format(GetString(21) +^J+ GetString(22), [ANewBuild]),
      mtQuestion, True) = IDYES) then
      with TUpdate.Create(Self, FLang, FLang.GetString(24)) do
        Download('sit.exe', 'SIT.exe')
    else
      mmUpdate.Caption := FLang.GetString(24);
end;

{ private TMain.CopyIcon

  Allows users to copy a icon in *.bmp format. }

function TMain.CopyIcon(): Boolean;
var
  dir: string;

begin
  result := False;

  // Icon exists and extension is .bmp
  if FileExists(eLogo.Text) then
    if (ExtractFileExt(eLogo.Text) = '.bmp') then
    begin
      // Show dialog
		  if SelectDirectory(FLang.GetString(9), '', dir) then
      begin
        dir := dir +'\'+ ExtractFileName(eLogo.Text);

        // Copy icon
        if CopyFile(PChar(eLogo.Text), PChar(dir), True) then
        begin
          FLang.MessageBox(Format(FLang.GetString(77), [dir]));
          eLogo.Text := dir;
          result := True;
        end  //of begin
        else
          FLang.MessageBox(70, mtError);
      end;  //of begin
		end  //of begin
    else
      FLang.MessageBox(78, mtWarning)
  else
    FLang.MessageBox(76, mtWarning);
end;

{ private TMain.DoExport

  Allows users to export support information as *.reg or *.ini file. }
  
procedure TMain.DoExport(ADirect: Boolean);
var
  SupportInfo: TSupportInformationBase;
  saveDialog : TSaveDialog;

begin
  try
    saveDialog := TSaveDialog.Create(Self);

    with saveDialog do
    begin
      Title := FLang.GetString(52);
      FileName := FLang.GetString(54);
      Options := Options + [ofOverwritePrompt];
    end;  //of with

    if ADirect then
      // Create deep copy of TSupportInformation object
      SupportInfo := TSupportInformation.Create(FSupportInfo)
    else
      // Create new TSupportInformation object with content of text fields
      SupportInfo := TSupportInformation.Create(eLogo.Text, eMan.Text,
        eModel.Text, eUrl.Text, ePhone.Text, eHours.Text);
    try
      if TOSUtils.CheckWindows() then
      begin
        // Windows >= Vista: Export as *.ini and *.reg
        saveDialog.Filter := FLang.GetString(55);
        saveDialog.FilterIndex := 2;
      end  //of begin
      else
        // Windows < Vista: Export only as *.ini
        saveDialog.Filter := FLang.GetString(56);

      // "Save" clicked
      if saveDialog.Execute then
        case saveDialog.FilterIndex of
          1: SupportInfo.SaveAsIni(saveDialog.FileName);
          2: (SupportInfo as TSupportInformation).SaveAsReg(saveDialog.FileName);
        end; //of case

    finally
      SupportInfo.Free;
      saveDialog.Free;
    end;  //of try

  except
    FLang.MessageBox(69, mtError);
  end;  //of try
end;

{ private TMain.Refresh

  Refreshs all text fields. }

procedure TMain.Refresh();
begin
  with FSupportInfo do
  begin
    eLogo.Text := Icon;
    eMan.Text := Manufacturer;
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

procedure TMain.SetLanguage(Sender: TObject);
begin
  with FLang do
  begin
    // Set captions for TMenuItems
    mmFile.Caption := GetString(31);
    mmImport.Caption := GetString(32);
    mmExport.Caption := GetString(33);
    mmExportEdit.Caption := GetString(34);

    mmEdit.Caption := GetString(35);
    mmShowValues.Caption := GetString(36);
    mmDelValues.Caption := GetString(37);
    mmDelEdit.Caption := GetString(38);
    mmCopyIcon.Caption := GetString(40);
    mmDelLogo.Caption := GetString(39);

    mmView.Caption := GetString(42);
    mmLang.Caption := GetString(25);

    mmHelp.Caption := GetString(14);
    mmUpdate.Caption := GetString(15);
    mmDownloadCert.Caption := GetString(16);
    mmReport.Caption := GetString(26);
    mmInfo.Caption := GetString(17);

    // Set captions for labels
    gbIcon.Caption := GetString(41);
    eLogo.EditLabel.Caption := GetString(43);
    cbCopyIcon.Caption := mmCopyIcon.Caption;

    gbInfo.Caption := GetString(0);
    eMan.EditLabel.Caption := GetString(44);
    ePhone.EditLabel.Caption := GetString(45);
    eHours.EditLabel.Caption := GetString(46);
    eModel.EditLabel.Caption := GetString(47);
    eUrl.EditLabel.Caption := GetString(48);

    // Set captions for buttons
    bAccept.Caption := GetString(49);
    bShowSupport.Caption := GetString(50);
    bAdd.Hint := GetString(53);
  end;  //of with
end;

{ TMain.bAcceptClick

  Allows user to commit changes on support information. }

procedure TMain.bAcceptClick(Sender: TObject);
begin
  if (Flang.MessageBox(60, mtQuestion) = IDYES) then
  try
    if cbCopyIcon.Checked then
      if not CopyIcon() then
        Exit;

    with FSupportInfo do
    begin
      // Icon exists?
      if ((not FileExists(eLogo.Text)) xor (eLogo.Text = '')) then
      begin
        Flang.MessageBox(77, mtWarning);
        eLogo.SetFocus;
        Exit;
      end  //of begin
      else
        Icon := eLogo.Text;

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
        FLang.MessageBox(73, mtWarning);
        eMan.SetFocus;
        Exit;
      end;  //of if
    end;  //of with

    Flang.MessageBox(65);
    mmShowValues.Click;

  except
    FLang.MessageBox(71, mtError);
  end;  //of except
end;

procedure TMain.bShowSupportClick(Sender: TObject);
begin
  FSupportInfo.Show(Application.Handle);
end;

{ TMain.bAddClick

  Allows users to search for a support information icon in *.bmp format. }

procedure TMain.bAddClick(Sender: TObject);
var
  OpenLogoDialog : TOpenPictureDialog;

begin
  OpenLogoDialog := TOpenPictureDialog.Create(Self);

  with OpenLogoDialog do
  begin
    Options := Options + [ofFileMustExist];
    Filter := FLang.GetString(57);
    Title := FLang.GetString(53);

    // Icon exists?
    if ((eLogo.Text <> '') and FileExists(eLogo.Text)) then
    begin
      // Open path of current icon
      InitialDir := ExtractFilePath(eLogo.Text);
      FileName := ExtractFileName(eLogo.Text);
    end  //of begin
    else
      // Open picture folder of current user
      InitialDir := '%USERPROFILE%\Pictures';
  end;  //of with

  try
    if OpenLogoDialog.Execute then
    begin
      Image.Picture.LoadFromFile(OpenLogoDialog.FileName);

      // Check square format of image and warn user
      if (Image.Height <> Image.Width) then
      begin
        FLang.MessageBox(Format(FLang.GetString(58), [Image.Height, Image.Width]) +
          FLang.GetString(59), mtWarning);
      end;  //of begin

      eLogo.Text := OpenLogoDialog.FileName;
    end; //of if

  finally
    OpenLogoDialog.Free;
    eLogo.SetFocus;
  end;  //of finally
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
    Title := FLang.GetString(51);
    Options := Options + [ofFileMustExist];

    // Windows >= Vista: Import *.ini and *.reg files
    if TOSUtils.CheckWindows() then
    begin
      Filter := FLang.GetString(55);
      FilterIndex := 2;
    end  //of begin
    else
      // Windows < Vista: Import only *.ini files
      Filter := FLang.GetString(56);
  end;  //of with  

  try
    if openDialog.Execute then
    begin
      Caption := Application.Title + TOSUtils.GetArchitecture() +' - '+
        ExtractFileName(openDialog.FileName);

      case openDialog.FilterIndex of
        1: FSupportInfo.LoadFromIni(openDialog.FileName);
        2: (FSupportInfo as TSupportInformation).LoadFromReg(openDialog.FileName);
      end;  //of case

      Refresh();
    end;  //of begin

  finally
    openDialog.Free;
  end;  //of try
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
    FLang.MessageBox(73, mtWarning)
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
    mmDelValues.Enabled := FSupportInfo.Exists();
    mmExport.Enabled := mmDelValues.Enabled;

  except
    FLang.MessageBox(68, mtError);
  end; //of except
end;

{ TMain.mmDelValuesClick

  Allows users to delete support information. }

procedure TMain.mmDelValuesClick(Sender: TObject);
begin
  // Show confirmation before deleting
  if (FLang.MessageBox(61, mtConfirm) = IDYES) then
  begin
    mmDelLogo.Click;

    if (FLang.MessageBox(62, mtQuestion) = IDYES) then
      DoExport(True);

    // Remove entries
    if FSupportInfo.Remove() then
    begin
      mmDelValues.Enabled := False;
      FLang.MessageBox(64);
    end  //of begin
    else
      FLang.MessageBox(FLang.GetString(66) + FLang.GetString(18), mtError);
  end;  //of begin
end;

{ TMain.mmDelEditClick

  Allows users to clear all text fields. }

procedure TMain.mmDelEditClick(Sender: TObject);
begin
  // Set title
  Caption := Application.Title + TOSUtils.GetArchitecture();

  // Update VCL
  mmDelLogo.Enabled := False;
  mmDelLogo.Visible := mmDelLogo.Enabled;

  // Clear text fields
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

procedure TMain.mmCopyIconClick(Sender: TObject);
begin
  CopyIcon();
end;

{ TMain.mmDelLogoClick

  Allows users to delete the support information icon. }

procedure TMain.mmDelLogoClick(Sender: TObject);
begin
  if FileExists(FSupportInfo.GetOEMLogo()) then
    // Show confirmation
    if (FLang.MessageBox(63, mtQuestion) = IDYES) then
      if FSupportInfo.DeleteIcon() then
      begin
        mmDelLogo.Visible := False;
        mmDelLogo.Enabled := False;
        eLogo.Clear;
      end  //of begin
      else
        FLang.MessageBox(FLang.GetString(67) + FLang.GetString(70), mtError);
end;

{ TMain.mmGerClick

  MainMenu entry that allows to change the current language to german. }

procedure TMain.mmGerClick(Sender: TObject);
begin
  FLang.ChangeLanguage(Sender, 100);
end;

{ TMain.mmEngClick

  MainMenu entry that allows to change the current language to english. }

procedure TMain.mmEngClick(Sender: TObject);
begin
  FLang.ChangeLanguage(Sender, 200);
end;

{ TMain.mmFraClick

  MainMenu entry that allows to change the current language to french. }

procedure TMain.mmFraClick(Sender: TObject);
begin
  FLang.ChangeLanguage(Sender, 300);
end;

{ TMain.mmDwnldCertClick

  MainMenu entry that allows to download the PM Code Works certificate. }

procedure TMain.mmDownloadCertClick(Sender: TObject);
begin
  // Certificate already installed?
  if (TOSUtils.PMCertExists() and (FLang.MessageBox(FLang.GetString(27) +^J
     + FLang.GetString(28), mtQuestion) = IDYES)) then
     // Download certificate
     with TUpdate.Create(Self, FLang, FLang.GetString(16)) do
       DownloadCertificate();
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
  TOSUtils.OpenUrl(URL_CONTACT);
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

{ TMain.eLogoDblClick

  Allows users to display open logo dialog when double click on text field. }

procedure TMain.eLogoDblClick(Sender: TObject);
begin
  if (eLogo.Text = '') then
    bAdd.Click
  else
    eLogo.SelectAll;
end;

{ TMain.eUrlDblClick

  Allows users to open selected URL in default webbrowser. }

procedure TMain.eUrlDblClick(Sender: TObject);
begin
  // Ask user to open URL
  if ((eUrl.Text <> '') and (FLang.MessageBox(79, mtQuestion) = ID_YES)) then
  begin
    // Try to open URL
    if not TOSUtils.OpenUrl(eUrl.Text) then
      FLang.MessageBox(80, mtError)
  end
  else
    eUrl.SelectAll;
end;

{ TMain.lCopyClick

  Opens the homepage of PM Code Works in a web browser. }

procedure TMain.lCopyClick(Sender: TObject);
begin
  TOSUtils.OpenUrl(URL_BASE);
end;

{ TMain.lCopyMouseEnter

  Allows a label to have the look like a hyperlink. }

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

end.
