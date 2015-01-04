{ *********************************************************************** }
{                                                                         }
{ Support Information Tool Main Unit                                      }
{                                                                         }
{ Copyright (c) 2011-2015 P.Meisberger (PM Code Works)                    }
{                                                                         }
{ *********************************************************************** }

unit SitMain;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls, Menus, ExtDlgs, LanguageFile, OSUtils, Updater, SitAPI, SitInfo;

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
    mmDeleteValues: TMenuItem;
    mmHelp: TMenuItem;
    mmInfo: TMenuItem;
    mmExportEdit: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    mmDeleteEdits: TMenuItem;
    mmDeleteIcon: TMenuItem;
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
    procedure mmDeleteValuesClick(Sender: TObject);
    procedure mmDeleteEditsClick(Sender: TObject);
    procedure mmDeleteIconClick(Sender: TObject);
    procedure mmGerClick(Sender: TObject);
    procedure mmEngClick(Sender: TObject);
    procedure mmFraClick(Sender: TObject);
    procedure mmUpdateClick(Sender: TObject);
    procedure mmDownloadCertClick(Sender: TObject);
    procedure mmReportClick(Sender: TObject);
    procedure mmInfoClick(Sender: TObject);
    procedure eLogoDblClick(Sender: TObject);
    procedure eUrlDblClick(Sender: TObject);
    procedure lCopyClick(Sender: TObject);
    procedure lCopyMouseEnter(Sender: TObject);
    procedure lCopyMouseLeave(Sender: TObject);
  private
    FSupportInfo: TSupportInformationBase;
    FLang: TLanguageFile;
    FUpdateCheck: TUpdateCheck;
    procedure AfterUpdate(Sender: TObject; ADownloadedFileName: string);
    procedure BeforeUpdate(Sender: TObject; const ANewBuild: Cardinal);
    procedure CheckIcon(AFile: string);
    procedure RefreshEdits(ASupportInfo: TSupportInformationBase);
    procedure SetLanguage(Sender: TObject);
    function ShowCopyIconDialog(AFile: string): string;
    procedure ShowExportDialog(AExportEdits: Boolean);
    procedure ShowValues(AReload: Boolean);
  end;

var
  Main: TMain;

implementation

{$R *.dfm}

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
  if TOSUtils.WindowsVistaOrLater() then
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
var
  windows: string;
  newWindows: Boolean;

begin
  windows := TOSUtils.GetWinVersion();
  newWindows := TOSUtils.WindowsVistaOrLater();
  cbCopyIcon.Enabled := newWindows;
  
  // Check for incompatibility
  if not (newWindows or (windows <> '')) then
  begin
    Flang.MessageBox(FLang.Format([74, 75], [windows]), mtError);
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
  ShowValues(True);

  // Make UAC-Shield button
  TOSUtils.MakeUACShieldButton(bAccept.Handle);
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
var
  Updater: TUpdate;

begin
  // Ask user to permit download
  if (FLang.MessageBox([21, NEW_LINE, 22], [ANewBuild], mtQuestion, True) = IDYES) then
  begin
    // init TUpdate instance
    Updater := TUpdate.Create(Self, FLang);

    try
      with Updater do
      begin
        Title := FLang.GetString(24);
        Download('sit.exe', 'SIT.exe');
      end;  //of begin

    finally
      Updater.Free;
    end;  //of try
  end  //of begin
  else
    mmUpdate.Caption := FLang.GetString(24);
end;

{ private TMain.CheckIcon

  Checks icon constraints. }

procedure TMain.CheckIcon(AFile: string);
begin
  // Icon exists?
  if not FileExists(AFile) then
  begin
    eLogo.SetFocus;
    raise EAbort.Create(FLang.GetString(76));
  end  //of begin
  else
    // Icon is a *.bmp file?
    if (ExtractFileExt(AFile) <> '.bmp') then
    begin
      eLogo.SetFocus;
      raise EAbort.Create(FLang.GetString(78));
    end;  //of if
end;

{ private TMain.RefreshEdits

  Refreshs all text fields. }

procedure TMain.RefreshEdits(ASupportInfo: TSupportInformationBase);
begin
  try
    with ASupportInfo do
    begin
      eLogo.Text := Icon;
      eMan.Text := Manufacturer;
      eModel.Text := Model;
      eUrl.Text := Url;
      ePhone.Text := Phone;
      eHours.Text := Hours;
    end;  //of with

  except
    FLang.MessageBox(68, mtError);
  end; //of try
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

    // "Edit" menu
    mmEdit.Caption := GetString(35);
    mmShowValues.Caption := GetString(36);
    mmDeleteValues.Caption := GetString(37);
    mmDeleteEdits.Caption := GetString(38);
    mmCopyIcon.Caption := GetString(40);
    mmDeleteIcon.Caption := GetString(39);

    // "View" menu
    mmView.Caption := GetString(20);
    mmLang.Caption := GetString(25);

    // "Help" menu
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

{ private TMain.ShowCopyIconDialog

  Allows users to copy a icon in *.bmp format. }

function TMain.ShowCopyIconDialog(AFile: string): string;
var
  SaveDialog : TSaveDialog;

begin
  result := '';

  // init dialog
  SaveDialog := TSaveDialog.Create(Self);

  try
    with SaveDialog do
    begin
      Title := FLang.GetString(52);
      FileName := ExtractFileName(AFile);
      Filter := FLang.GetString(57);
      DefaultExt := '.bmp';
      Options := Options + [ofOverwritePrompt];
    end;  //of with

    // Save clicked
    if SaveDialog.Execute then
    begin
      // Destination file is a *.bmp file?
      if (ExtractFileExt(SaveDialog.FileName) <> '.bmp') then
        raise EAbort.Create(FLang.GetString(78));

      // Copy valid icon
      if CopyFile(PChar(AFile), PChar(SaveDialog.FileName), False) then
      begin
        FLang.MessageBox(FLang.Format(77, [SaveDialog.FileName]));
        result := SaveDialog.FileName;
      end  //of begin
      else
        FLang.MessageBox(70, mtError);
    end;  //of begin

  finally
    SaveDialog.Free;
  end;  //of try
end;

{ private TMain.ShowExportDialog

  Allows users to export support information as *.reg or *.ini file. }
  
procedure TMain.ShowExportDialog(AExportEdits: Boolean);
var
  SaveDialog : TSaveDialog;
  SupportInfo: TSupportInformationBase;

begin
  // init dialog
  SaveDialog := TSaveDialog.Create(Self);

  try
    // Set dialog options
    with SaveDialog do
    begin
      Title := FLang.GetString(52);
      FileName := FLang.GetString(54);
      Options := Options + [ofOverwritePrompt];

      // Set OS dependend filter
      if TOSUtils.WindowsVistaOrLater() then
      begin
        // Windows >= Vista: Export as *.ini and *.reg
        Filter := FLang.GetString(55);
        FilterIndex := 2;
      end  //of begin
      else
      begin
        // Windows < Vista: Export only as *.ini
        Filter := FLang.GetString(56);
        DefaultExt := '.ini';
      end;  //of if
    end;  //of with

    if AExportEdits then
      // Create new TSupportInformation object with content of text fields
      SupportInfo := TSupportInformation.Create(eLogo.Text, eMan.Text,
        eModel.Text, eUrl.Text, ePhone.Text, eHours.Text)
    else
      // Create deep copy of TSupportInformation object
      SupportInfo := TSupportInformation.Create(FSupportInfo);

    try
      // "Save" clicked
      if SaveDialog.Execute then
        case saveDialog.FilterIndex of
          1: SupportInfo.SaveAsIni(saveDialog.FileName);
          2: (SupportInfo as TSupportInformation).SaveAsReg(saveDialog.FileName);
        end; //of case

    finally
      SupportInfo.Free;
      SaveDialog.Free;
    end;  //of try

  except
    FLang.MessageBox(69, mtError);
  end;  //of try
end;

{ private TMain.ShowValues

  Allows users to show support information. }

procedure TMain.ShowValues(AReload: Boolean);
begin
  // Set title
  Caption := Application.Title + TOSUtils.GetArchitecture();

  if AReload then
    // Reload support information
    FSupportInfo.Load();

  // Refresh VCL
  mmDeleteIcon.Enabled := FileExists(FSupportInfo.GetOEMIcon());
  mmCopyIcon.Enabled := mmDeleteIcon.Enabled;
  mmDeleteValues.Enabled := FSupportInfo.Exists();
  mmExport.Enabled := mmDeleteValues.Enabled;

  // Load content of loaded support information into text fields
  RefreshEdits(FSupportInfo);
end;

{ TMain.bAcceptClick

  Allows user to commit changes on support information. }

procedure TMain.bAcceptClick(Sender: TObject);
var
  IconPath: string;

begin
  // Confirm save progress
  if (FLang.MessageBox(60, mtQuestion) = IDYES) then
  try
    IconPath := eLogo.Text;

    // Icon exists?
    if (IconPath <> '') then
    begin
      // Check icon constraints
      CheckIcon(IconPath);

      // Make copy of selected icon?
      if cbCopyIcon.Checked then
      begin
        IconPath := ShowCopyIconDialog(eLogo.Text);

        // User clicked cancel?
        if (IconPath = '') then
          Exit;
      end;  //of if
    end;  //of if

    // Save support information
    // Note: Manufacturer is essential!
    if (eMan.Text <> '') then
      with FSupportInfo do
      begin
        Icon := IconPath;
        Phone := ePhone.Text;
        Hours := eHours.Text;
        Manufacturer := eMan.Text;
        Model := eModel.Text;
        Url := eUrl.Text;
        Save();
      end  //of with
    else
    begin
      eMan.SetFocus;
      raise EAbort.Create(FLang.GetString(73));
    end;  //of if

    // Refresh VCL
    ShowValues(False);

    // Show success message in best case
    Flang.MessageBox(65);

  except
    on E: EAbort do
      FLang.MessageBox(E.Message, mtWarning);

    on E: Exception do
      FLang.MessageBox(71, mtError);
  end;  //of try
end;

{ TMain.bShowSupportClick

  Allows users to show the saved support information in Windows. }

procedure TMain.bShowSupportClick(Sender: TObject);
begin
  FSupportInfo.Show();
end;

{ TMain.bAddClick

  Allows users to search for a support information icon in *.bmp format. }

procedure TMain.bAddClick(Sender: TObject);
var
  OpenLogoDialog : TOpenPictureDialog;

begin
  // init dialog
  OpenLogoDialog := TOpenPictureDialog.Create(Self);

  try
    // Set dialog options
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

    // "Open" clicked
    if OpenLogoDialog.Execute then
    begin
      Image.Picture.LoadFromFile(OpenLogoDialog.FileName);

      // Check square format of image and warn user
      if (Image.Height <> Image.Width) then
      begin
        FLang.MessageBox(FLang.Format([58, 59], [Image.Height, Image.Width]),
          mtWarning);
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
  OpenDialog : TOpenDialog;
  SupportInfo: TSupportInformationBase;

begin
  // init dialog
  OpenDialog := TOpenDialog.Create(Self);

  try
    // Set dialog options
    with OpenDialog do
    begin
      Title := FLang.GetString(51);
      Options := Options + [ofFileMustExist];

      // Windows >= Vista: Import *.ini and *.reg files
      if TOSUtils.WindowsVistaOrLater() then
      begin
        Filter := FLang.GetString(55);
        FilterIndex := 2;
      end  //of begin
      else
        // Windows < Vista: Import only *.ini files
        Filter := FLang.GetString(56);
    end;  //of with

    // Create deep copy of TSupportInformation object
    SupportInfo := TSupportInformation.Create(FSupportInfo);

    try
      // "Open" clicked
      if OpenDialog.Execute then
      begin
        Caption := Application.Title + TOSUtils.GetArchitecture() +' - '+
          ExtractFileName(OpenDialog.FileName);

        case OpenDialog.FilterIndex of
          1: SupportInfo.LoadFromIni(OpenDialog.FileName);
          2: (SupportInfo as TSupportInformation).LoadFromReg(OpenDialog.FileName);
        end;  //of case

        // Load content of loaded file into text fields
        RefreshEdits(SupportInfo);
      end;  //of begin

    finally
      SupportInfo.Free;
      OpenDialog.Free;
    end;  //of try

  except
    FLang.MessageBox(72, mtError);
  end;  //of try
end;

{ TMain.mmExportClick

  Allows users to export the stored support information. }

procedure TMain.mmExportClick(Sender: TObject);
begin
  ShowExportDialog(False);
end;

{ TMain.mmExportEditClick

  Allows users to export the entered content of the text fields. }

procedure TMain.mmExportEditClick(Sender: TObject);
begin
  if ((eLogo.Text = '') and (eMan.Text = '') and (eModel.Text = '') and
    (eUrl.Text = '') and (ePhone.Text = '') and (eHours.Text = '')) then
    FLang.MessageBox(73, mtWarning)
  else
    ShowExportDialog(True);
end;

{ TMain.mmShowValuesClick

  Allows users to show support information. }

procedure TMain.mmShowValuesClick(Sender: TObject);
begin
  ShowValues(True);
end;

{ TMain.mmDelValuesClick

  Allows users to delete support information. }

procedure TMain.mmDeleteValuesClick(Sender: TObject);
begin
  // Show confirmation before deleting
  if (FLang.MessageBox(61, mtConfirm) = IDYES) then
  begin
    if (FLang.MessageBox(62, mtQuestion) = IDYES) then
      ShowExportDialog(False);

    // Remove icon?
    mmDeleteIcon.Click;

    // Remove entries
    if FSupportInfo.Remove() then
    begin
      mmDeleteValues.Enabled := False;
      mmExport.Enabled := False;
      mmDeleteIcon.Enabled := FileExists(FSupportInfo.GetOEMIcon());
      mmCopyIcon.Enabled := mmDeleteIcon.Enabled;
      FSupportInfo.Clear;
      FLang.MessageBox(64);
    end  //of begin
    else
      FLang.MessageBox(66, mtError);
  end;  //of begin
end;

{ TMain.mmDelEditClick

  Allows users to clear all text fields. }

procedure TMain.mmDeleteEditsClick(Sender: TObject);
begin
  Caption := Application.Title + TOSUtils.GetArchitecture();
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
  try
    // Check icon constraints
    CheckIcon(FSupportInfo.Icon);

    // Start copy icon procedure
    ShowCopyIconDialog(FSupportInfo.Icon);

  except
    on E: EAbort do
      FLang.MessageBox(E.Message, mtWarning);

    on E: Exception do
      FLang.MessageBox(70, mtError);
  end;  //of try
end;

{ TMain.mmDelLogoClick

  Allows users to delete the support information icon. }

procedure TMain.mmDeleteIconClick(Sender: TObject);
begin
  // Show confirmation
  if (FLang.MessageBox(63, mtQuestion) = IDYES) then
    if FSupportInfo.DeleteOEMIcon() then
    begin
      mmDeleteIcon.Enabled := False;
      mmCopyIcon.Enabled := False;
      FSupportInfo.Icon := '';
    end  //of begin
    else
      FLang.MessageBox(67, mtError);
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

{ TMain.mmDownloadCertClick

  MainMenu entry that allows to download the PM Code Works certificate. }

procedure TMain.mmDownloadCertClick(Sender: TObject);
var
  Updater: TUpdate;

begin
  // Certificate already installed?
  if (TOSUtils.PMCertExists() and (FLang.MessageBox([27, NEW_LINE, 28],
    mtQuestion) = IDYES)) then
  begin
    // Init downloader
    Updater := TUpdate.Create(Self, FLang);

    // Download certificate
    try
      with Updater do
      begin
        Title := FLang.GetString(16);
        DownloadCertificate();
      end;  //of begin

    finally
      Updater.Free;
    end;  //of try
  end;  //of begin
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
  if ((eUrl.Text <> '') and (FLang.MessageBox(79, mtQuestion) = IDYES)) then
  begin
    // Try to open URL
    if not TOSUtils.OpenUrl(eUrl.Text) then
      FLang.MessageBox(80, mtError);
  end  //of begin
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
