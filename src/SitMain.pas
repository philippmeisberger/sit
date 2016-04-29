{ *********************************************************************** }
{                                                                         }
{ Support Information Tool Main Unit                                      }
{                                                                         }
{ Copyright (c) 2011-2016 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit SitMain;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Winapi.CommCtrl, Vcl.Menus, Vcl.Dialogs,
  Vcl.ExtDlgs, Vcl.Imaging.jpeg, System.UITypes, SitAPI, PMCWAbout, PMCWOSUtils,
  PMCWLanguageFile, PMCWUpdater, SHFolder;

type
  { TMain }
  TMain = class(TForm, IChangeLanguageListener, IUpdateListener)
    lCopy: TLabel;
    lVersion: TLabel;
    bAccept: TButton;
    bShowSupport: TButton;
    MainMenu: TMainMenu;
    mmFile: TMenuItem;
    mmImport: TMenuItem;
    mmExport: TMenuItem;
    mmEdit: TMenuItem;
    mmDeleteValues: TMenuItem;
    mmHelp: TMenuItem;
    mmAbout: TMenuItem;
    mmExportEdit: TMenuItem;
    N1: TMenuItem;
    mmDeleteEdits: TMenuItem;
    mmDeleteIcon: TMenuItem;
    mmInstallCertificate: TMenuItem;
    mmUpdate: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    mmView: TMenuItem;
    mmLang: TMenuItem;
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
    mmShow: TMenuItem;
    N6: TMenuItem;
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
    procedure mmShowClick(Sender: TObject);
    procedure mmDeleteValuesClick(Sender: TObject);
    procedure mmDeleteEditsClick(Sender: TObject);
    procedure mmDeleteIconClick(Sender: TObject);
    procedure mmUpdateClick(Sender: TObject);
    procedure mmInstallCertificateClick(Sender: TObject);
    procedure mmReportClick(Sender: TObject);
    procedure mmAboutClick(Sender: TObject);
    procedure eLogoDblClick(Sender: TObject);
    procedure eUrlDblClick(Sender: TObject);
    procedure lCopyClick(Sender: TObject);
    procedure lCopyMouseEnter(Sender: TObject);
    procedure lCopyMouseLeave(Sender: TObject);
  private
    FSupportInfo: TSupportInformationBase;
    FLang: TLanguageFile;
    FUpdateCheck: TUpdateCheck;
    procedure OnUpdate(Sender: TObject; const ANewBuild: Cardinal);
    procedure RefreshEdits(ASupportInfo: TSupportInformationBase);
    procedure SetLanguage(Sender: TObject);
    function ShowCopyIconDialog(const AFile: string): string;
    procedure ShowExportDialog(AExportEdits: Boolean);
    procedure ShowValues(AReload: Boolean = True);
  end;

var
  Main: TMain;

implementation

{$R *.dfm}
{$I LanguageIDs.inc}

{ TMain.FormCreate

  VCL event that is called when form is being created. }

procedure TMain.FormCreate(Sender: TObject);
var
  VersionInfo: TFileProductVersion;

begin
  // Setup language
  FLang := TLanguageFile.Create(Self);
  FLang.Interval := 100;
  FLang.BuildLanguageMenu(MainMenu, mmLang);

  // Init update notificator
  FUpdateCheck := TUpdateCheck.Create(Self, 'SIT', FLang);

  // Check for update on startup
  FUpdateCheck.CheckForUpdate(False);

  // Init support information instance
  if CheckWin32Version(6) then
    FSupportInfo := TSupportInformation.Create
  else
    FSupportInfo := TSupportInformationXP.Create;

  // Get version information
  if TUpdateCheck.GetFileVersion(Application.ExeName, VersionInfo) then
  begin
    lVersion.Caption := Format('v%d.%d', [VersionInfo[VERSION_MAJOR],
      VersionInfo[VERSION_MINOR]]);
  end;  //of begin
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
begin
  // At least Windows 2000!
  if not CheckWin32Version(5) then
  begin
    FLang.ShowMessage(FLang.Format([LID_ERROR_INCOMPATIBLE1, LID_ERROR_INCOMPATIBLE2],
      [TOSVersion.Name]), mtError);
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

  // Copy icon only available for Vista and later
  cbCopyIcon.Enabled := CheckWin32Version(6);

  // Show support information
  ShowValues();
end;

{ private TMain.OnUpdate

  Event that is called by TUpdateCheck when an update is available. }

procedure TMain.OnUpdate(Sender: TObject; const ANewBuild: Cardinal);
var
  Updater: TUpdate;

begin
  // Ask user to permit download
  if (FLang.ShowMessage(FLang.Format(LID_UPDATE_AVAILABLE, [ANewBuild]),
    FLang.GetString(LID_UPDATE_CONFIRM_DOWNLOAD), mtConfirmation) = IDYES) then
  begin
    // init TUpdate instance
    Updater := TUpdate.Create(Self, FLang);

    try
      // Set updater options
      with Updater do
      begin
        Title := FLang.GetString(LID_UPDATE_DOWNLOAD);
        FileNameLocal := 'SIT.exe';

      {$IFDEF WIN64}
        FileNameRemote := 'sit64.exe';
      {$ELSE}
        // Ask user to permit download of 64-Bit version
        if ((TOSVersion.Architecture = arIntelX64) and (FLang.ShowMessage(
          FLang.Format([LID_UPDATE_64BIT, LID_UPDATE_64BIT_CONFIRM], ['SIT']),
          mtConfirmation) = IDYES)) then
          FileNameRemote := 'sit64.exe'
        else
          FileNameRemote := 'sit.exe';
      {$ENDIF}
      end;  //of begin

      // Successfully downloaded update?
      if Updater.Execute() then
      begin
        // Caption "Search for update"
        mmUpdate.Caption := FLang.GetString(LID_UPDATE_SEARCH);
        mmUpdate.Enabled := False;
      end;  //of begin

    finally
      Updater.Free;
    end;  //of try
  end  //of begin
  else
    mmUpdate.Caption := FLang.GetString(LID_UPDATE_DOWNLOAD);
end;

{ private TMain.RefreshEdits

  Refreshs all text fields. }

procedure TMain.RefreshEdits(ASupportInfo: TSupportInformationBase);
begin
  with ASupportInfo do
  begin
    eLogo.Text := Icon;
    eMan.Text := Manufacturer;
    eModel.Text := Model;
    eUrl.Text := Url;
    ePhone.Text := Phone;
    eHours.Text := Hours;
  end;  //of with
end;

{ private TMain.SetLanguage

  Updates all component captions with new language text. }

procedure TMain.SetLanguage(Sender: TObject);
begin
  with FLang do
  begin
    // Set captions for TMenuItems
    mmFile.Caption := GetString(LID_FILE);
    mmImport.Caption := GetString(LID_IMPORT);
    mmExport.Caption := GetString(LID_EXPORT);
    mmExportEdit.Caption := GetString(LID_INPUT_EXPORT);

    // "Edit" menu
    mmEdit.Caption := GetString(LID_EDIT);
    mmShow.Caption := GetString(LID_ITEMS_SHOW);
    mmDeleteValues.Caption := GetString(LID_ITEMS_DELETE);
    mmDeleteEdits.Caption := GetString(LID_INPUT_DELETE);
    mmCopyIcon.Caption := GetString(LID_ICON_COPY);
    mmDeleteIcon.Caption := GetString(LID_ICON_DELETE);

    // "View" menu
    mmView.Caption := GetString(LID_VIEW);
    mmLang.Caption := GetString(LID_SELECT_LANGUAGE);

    // "Help" menu
    mmHelp.Caption := GetString(LID_HELP);
    mmUpdate.Caption := GetString(LID_UPDATE_SEARCH);
    mmInstallCertificate.Caption := GetString(LID_CERTIFICATE_INSTALL);
    mmReport.Caption := GetString(LID_REPORT_BUG);
    mmAbout.Caption := Format(LID_ABOUT, [Application.Title]);

    // Set captions for labels
    gbIcon.Caption := GetString(LID_ICON);
    eLogo.EditLabel.Caption := GetString(LID_ICON_FILE);
    cbCopyIcon.Caption := mmCopyIcon.Caption;

    gbInfo.Caption := GetString(52);
    eMan.EditLabel.Caption := GetString(LID_MANUFACTURER);
    ePhone.EditLabel.Caption := GetString(LID_PHONE);
    eHours.EditLabel.Caption := GetString(LID_HOURS);
    eModel.EditLabel.Caption := GetString(LID_MODEL);
    eUrl.EditLabel.Caption := GetString(LID_URL);

    // Set captions for buttons
    bAccept.Caption := GetString(LID_APPLY);
    bShowSupport.Caption := GetString(LID_SHOW);
    bAdd.Hint := GetString(LID_ICON_SELECT);
    lCopy.Hint := GetString(LID_TO_WEBSITE);
  end;  //of with
end;

{ private TMain.ShowCopyIconDialog

  Allows users to copy a icon in *.bmp format. }

function TMain.ShowCopyIconDialog(const AFile: string): string;
var
  Image: TPicture;
  Bitmap: TBitmap;
  FileName: string;

begin
  Result := '';

  // File exists?
  if not FileExists(AFile) then
    raise EAbort.Create(FLang.GetString(LID_ERROR_ICON_NOT_EXIST));

  FileName := ChangeFileExt(ExtractFileName(AFile), '.bmp');

  if not PromptForFileName(FileName, GraphicFilter(TBitmap), '.bmp',
    StripHotkey(mmCopyIcon.Caption), '', True) then
    Exit;

  // Destination file is a *.bmp file?
  if (ExtractFileExt(FileName) <> '.bmp') then
    raise EAbort.Create(FLang.GetString(LID_ERROR_INVALID_EXT));

  Image := TPicture.Create;
  Bitmap := TBitmap.Create;

  // Try to convert any picture to bitmap
  try
    Image.LoadFromFile(AFile);
    Bitmap.Width := Image.Width;
    Bitmap.Height := Image.Height;
    Bitmap.Canvas.Draw(0, 0, Image.Graphic);
    Bitmap.SaveToFile(FileName);

    FLang.ShowMessage(FLang.Format(LID_ICON_COPIED, [FileName]));
    Result := FileName;

  finally
    Bitmap.Free;
    Image.Free;
  end;  //of try
end;

{ private TMain.ShowExportDialog

  Allows users to export support information as *.reg or *.ini file. }
  
procedure TMain.ShowExportDialog(AExportEdits: Boolean);
var
  SaveDialog: TSaveDialog;
  SupportInfo: TSupportInformationBase;

begin
  // init dialog
  SaveDialog := TSaveDialog.Create(Self);

  try
    // Set dialog options
    with SaveDialog do
    begin
      if AExportEdits then
        Title := StripHotkey(mmExportEdit.Caption)
      else
        Title := StripHotkey(mmExport.Caption);

      Options := Options + [ofOverwritePrompt];
      FileName := FLang.GetString(LID_EXPORT_DEFAULT);

      // Set OS dependend filter
      if CheckWin32Version(6) then
      begin
        // Windows >= Vista: Export as *.ini and *.reg
        Filter := FLang.GetString(LID_FILTER_INI_FILE) +'|'+ FLang.GetString(LID_FILTER_REGISTRY_FILE);
        FilterIndex := 2;
      end  //of begin
      else
      begin
        // Windows < Vista: Export only as *.ini
        Filter := FLang.GetString(LID_FILTER_INI_FILE);
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
        case SaveDialog.FilterIndex of
          1: SupportInfo.SaveAsIni(SaveDialog.FileName);
          2: (SupportInfo as TSupportInformation).SaveAsReg(SaveDialog.FileName);
        end;  //of case

    finally
      SupportInfo.Free;
      SaveDialog.Free;
    end;  //of try

  except
    on E: Exception do
      FLang.ShowException(FLang.GetString(LID_ERROR_WRITING_FILE), E.Message);
  end;  //of try
end;

{ private TMain.ShowValues

  Allows users to show support information. }

procedure TMain.ShowValues(AReload: Boolean = True);
begin
  // Set title
  Caption := Application.Title + PLATFORM_ARCH;

  // Reload support information?
  if AReload then
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
  if (FLang.ShowMessage(FLang.GetString(LID_ITEMS_SAVE_CONFIRM), mtConfirmation) = IDYES) then
  try
    // Manufacturer is essential!
    if (Trim(eMan.Text) = '') then
    begin
      FLang.EditBalloonTip(eMan.Handle, LID_ERROR, LID_NOTHING_ENTERED, biError);
      Exit;
    end;  //of begin

    IconPath := eLogo.Text;

    // Icon seems to exist?
    if (IconPath <> '') then
    begin
      if not FileExists(IconPath) then
      begin
        FLang.EditBalloonTip(eLogo.Handle, LID_ERROR, LID_ERROR_ICON_NOT_EXIST, biError);
        Exit;
      end;  //of begin

      // Make copy of selected icon?
      if cbCopyIcon.Checked then
      begin
        IconPath := ShowCopyIconDialog(eLogo.Text);

        // User clicked cancel?
        if (IconPath = '') then
          Exit;
      end  //of if
      else
        // Icon must be a bitmap!
        if (ExtractFileExt(IconPath) <> '.bmp') then
        begin
          FLang.EditBalloonTip(eLogo.Handle, LID_ERROR, LID_ERROR_INVALID_EXT, biError);
          Exit;
        end;  //of begin
    end;  //of begin

    // Save support information
    with FSupportInfo do
    begin
      Icon := IconPath;
      Phone := ePhone.Text;
      Hours := eHours.Text;
      Manufacturer := eMan.Text;
      Model := eModel.Text;
      Url := eUrl.Text;
      Save();
    end;  //of with

    // Refresh VCL
    ShowValues(False);

    // Show success message in best case
    FLang.ShowMessage(FLang.GetString(LID_ITEMS_SAVED));

  except
    on E: EAbort do
      FLang.ShowMessage(E.Message, mtWarning);

    on E: Exception do
      FLang.ShowException(FLang.GetString(LID_ERROR_SAVING), E.Message);
  end;  //of try
end;

{ TMain.bShowSupportClick

  Allows users to show the saved support information in Windows. }

procedure TMain.bShowSupportClick(Sender: TObject);
begin
  FSupportInfo.Show();
end;

{ TMain.bAddClick

  Allows users to search for a support information image. }

procedure TMain.bAddClick(Sender: TObject);
var
  OpenDialog: TOpenPictureDialog;
  Image: TPicture;

begin
  // Init dialog
  OpenDialog := TOpenPictureDialog.Create(Self);

  try
    // Set dialog options
    with OpenDialog do
    begin
      Title := FLang.GetString(LID_ICON_SELECT);

      // Icon exists?
      if ((eLogo.Text <> '') and FileExists(eLogo.Text)) then
      begin
        // Open path of current icon
        InitialDir := ExtractFilePath(eLogo.Text);
        FileName := ExtractFileName(eLogo.Text);
      end  //of begin
      else
        // Open picture folder of current user
        InitialDir := GetFolderPath(CSIDL_MYPICTURES);
    end;  //of with

    // "Open" clicked
    if OpenDialog.Execute then
    begin
      Image := TPicture.Create;

      try
        Image.LoadFromFile(OpenDialog.FileName);

        // Check square format of image and warn user
        if (Image.Height <> Image.Width) then
        begin
          FLang.ShowMessage(FLang.Format([LID_ICON_NOT_SQUARE1, LID_ICON_NOT_SQUARE2],
            [Image.Width, Image.Height]), mtWarning);
        end;  //of begin

        cbCopyIcon.Checked := not (Image.Graphic is TBitmap);
        eLogo.Text := OpenDialog.FileName;

      finally
        Image.Free;
      end;  //of try
    end;  //of begin

  finally
    OpenDialog.Free;
    eLogo.SetFocus;
  end;  //of try
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
      Title := StripHotkey(mmImport.Caption);

      // Windows >= Vista: Import *.ini and *.reg files
      if CheckWin32Version(6) then
      begin
        Filter := FLang.GetString(LID_FILTER_INI_FILE) +'|'+ FLang.GetString(LID_FILTER_REGISTRY_FILE);
        FilterIndex := 2;
      end  //of begin
      else
        // Windows < Vista: Import only *.ini files
        Filter := FLang.GetString(LID_FILTER_INI_FILE);
    end;  //of with

    // Create deep copy of TSupportInformation object
    SupportInfo := TSupportInformation.Create(FSupportInfo);

    try
      // "Open" clicked
      if OpenDialog.Execute then
      begin
        Caption := Application.Title + PLATFORM_ARCH +' - '+
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
    on E: Exception do
      FLang.ShowException(FLang.GetString(LID_ERROR_IMPORTING), E.Message);
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
    FLang.ShowMessage(FLang.GetString(LID_NOTHING_ENTERED), mtWarning)
  else
    ShowExportDialog(True);
end;

{ TMain.mmShowClick

  Allows users to show support information. }

procedure TMain.mmShowClick(Sender: TObject);
begin
  ShowValues();
end;

{ TMain.mmDelValuesClick

  Allows users to delete support information. }

procedure TMain.mmDeleteValuesClick(Sender: TObject);
begin
  // Show confirmation before deleting
  if (FLang.ShowMessage(FLang.GetString(LID_ITEMS_DELETE_CONFIRM), mtCustom) = IDYES) then
  begin
    if (FLang.ShowMessage(FLang.GetString(LID_ITEMS_DELETE_STORE),
      mtConfirmation) = IDYES) then
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
      FLang.ShowMessage(FLang.GetString(LID_ITEMS_DELETED));
    end  //of begin
    else
      FLang.ShowMessage(FLang.GetString(LID_ERROR_DELETING), mtError);
  end;  //of begin
end;

{ TMain.mmDeleteEditsClick

  Allows users to clear all text fields. }

procedure TMain.mmDeleteEditsClick(Sender: TObject);
begin
  Caption := Application.Title + PLATFORM_ARCH;
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
    // Start copy icon procedure
    ShowCopyIconDialog(FSupportInfo.Icon);

  except
    on E: EAbort do
      FLang.ShowMessage(E.Message, mtWarning);

    on E: Exception do
      FLang.ShowException(FLang.GetString(LID_ERROR_COPYING), E.Message);
  end;  //of try
end;

{ TMain.mmDeleteIconClick

  Allows users to delete the support information icon. }

procedure TMain.mmDeleteIconClick(Sender: TObject);
begin
  // Show confirmation
  if (FLang.ShowMessage(FLang.Format(LID_ICON_DELETE_CONFIRM, [FSupportInfo.Icon]),
    mtCustom) = IDYES) then
    if FSupportInfo.DeleteOEMIcon() then
    begin
      mmDeleteIcon.Enabled := False;
      mmCopyIcon.Enabled := False;
      FSupportInfo.Icon := '';
      eLogo.Clear;
    end  //of begin
    else
      FLang.ShowMessage(FLang.GetString(LID_ERROR_DELETING_ICON), mtError);
end;

{ TMain.mmInstallCertificateClick

  MainMenu entry that allows to install the PM Code Works certificate. }

procedure TMain.mmInstallCertificateClick(Sender: TObject);
var
  Updater: TUpdate;

begin
  Updater := TUpdate.Create(Self, FLang);

  try
    // Certificate already installed?
    if not Updater.CertificateExists() then
      Updater.InstallCertificate()
    else
      FLang.ShowMessage(FLang.GetString(LID_CERTIFICATE_ALREADY_INSTALLED), mtInformation);

  finally
    Updater.Free;
  end;  //of try
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
  OpenUrl(URL_CONTACT);
end;

{ TMain.mmInfoClick

  MainMenu entry that shows a info page with build number and version history. }

procedure TMain.mmAboutClick(Sender: TObject);
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
  if ((eUrl.Text <> '') and (FLang.ShowMessage(FLang.GetString(LID_URL_OPEN),
    mtConfirmation) = IDYES)) then
  begin
    // Try to open URL
    if not OpenUrl(eUrl.Text) then
      FLang.EditBalloonTip(eUrl.Handle, LID_ERROR, LID_ERROR_INVALID_URL, biError);
  end  //of begin
  else
    eUrl.SelectAll;
end;

{ TMain.lCopyClick

  Opens the homepage of PM Code Works in a web browser. }

procedure TMain.lCopyClick(Sender: TObject);
begin
  OpenUrl(URL_BASE);
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
