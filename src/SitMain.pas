{ *********************************************************************** }
{                                                                         }
{ Support Information Tool Main Unit                                      }
{                                                                         }
{ Copyright (c) 2011-2015 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit SitMain;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Winapi.CommCtrl, Vcl.Menus, Vcl.Dialogs,
  Vcl.ExtDlgs, Vcl.Imaging.jpeg, SitAPI, PMCWAbout, PMCWLanguageFile, PMCWOSUtils,
  PMCWUpdater;

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
    mmInfo: TMenuItem;
    mmExportEdit: TMenuItem;
    N1: TMenuItem;
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
    mmFre: TMenuItem;
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
    procedure mmGerClick(Sender: TObject);
    procedure mmEngClick(Sender: TObject);
    procedure mmFreClick(Sender: TObject);
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

{ TMain.FormCreate

  VCL event that is called when form is being created. }

procedure TMain.FormCreate(Sender: TObject);
begin
  // Setup language
  FLang := TLanguageFile.Create(Self);
  FLang.AddLanguage(LANG_GERMAN, 100);
  FLang.AddLanguage(LANG_ENGLISH, 200);
  FLang.AddLanguage(LANG_FRENCH, 300);
  FLang.ChangeLanguage(LANG_USER);

  case FLang.Id of
    200: mmEng.Checked := True;
    300: mmFre.Checked := True;
    else
         mmGer.Checked := True;
  end;  //of case

  // Init update notificator
  FUpdateCheck := TUpdateCheck.Create(Self, 'SIT', FLang);

  // Check for update on startup
  FUpdateCheck.CheckForUpdate(False);

  // Init support information instance
  if (Win32MajorVersion >= 6) then
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
begin
  // At least Windows 2000!
  if not TOSVersion.Check(5) then
  begin
    FLang.ShowMessage(FLang.Format([84, 85], [TOSVersion.Name]), mtError);
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
  cbCopyIcon.Enabled := TOSVersion.Check(6);

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
  if (FLang.ShowMessage(FLang.Format(21, [ANewBuild]), FLang.GetString(22),
    mtConfirmation) = IDYES) then
  begin
    // init TUpdate instance
    Updater := TUpdate.Create(Self, FLang);

    try
      // Set updater options
      with Updater do
      begin
        Title := FLang.GetString(24);
        FileNameLocal := 'SIT.exe';

      {$IFDEF WIN64}
        FileNameRemote := 'sit64.exe';
      {$ELSE}
        // Ask user to permit download of 64-Bit version
        if ((TOSVersion.Architecture = arIntelX64) and (FLang.ShowMessage(
          FLang.Format([34, 35], ['SIT']), mtConfirmation) = IDYES)) then
          FileNameRemote := 'sit64.exe'
        else
          FileNameRemote := 'sit.exe';
      {$ENDIF}
      end;  //of begin

      // Successfully downloaded update?
      if Updater.Execute() then
      begin
        // Caption "Search for update"
        mmUpdate.Caption := FLang.GetString(15);
        mmUpdate.Enabled := False;
      end;  //of begin

    finally
      Updater.Free;
    end;  //of try
  end  //of begin
  else
    mmUpdate.Caption := FLang.GetString(24);
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
    mmFile.Caption := GetString(41);
    mmImport.Caption := GetString(42);
    mmExport.Caption := GetString(43);
    mmExportEdit.Caption := GetString(44);

    // "Edit" menu
    mmEdit.Caption := GetString(45);
    mmShow.Caption := GetString(46);
    mmDeleteValues.Caption := GetString(47);
    mmDeleteEdits.Caption := GetString(48);
    mmCopyIcon.Caption := GetString(50);
    mmDeleteIcon.Caption := GetString(49);

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
    gbIcon.Caption := GetString(51);
    eLogo.EditLabel.Caption := GetString(53);
    cbCopyIcon.Caption := mmCopyIcon.Caption;

    gbInfo.Caption := GetString(0);
    eMan.EditLabel.Caption := GetString(54);
    ePhone.EditLabel.Caption := GetString(55);
    eHours.EditLabel.Caption := GetString(56);
    eModel.EditLabel.Caption := GetString(57);
    eUrl.EditLabel.Caption := GetString(58);

    // Set captions for buttons
    bAccept.Caption := GetString(59);
    bShowSupport.Caption := GetString(60);
    bAdd.Hint := GetString(63);
    lCopy.Hint := GetString(29);
  end;  //of with
end;

{ private TMain.ShowCopyIconDialog

  Allows users to copy a icon in *.bmp format. }

function TMain.ShowCopyIconDialog(const AFile: string): string;
var
  SaveDialog : TSaveDialog;
  Image: TPicture;
  Bitmap: TBitmap;

begin
  Result := '';

  // File exists?
  if not FileExists(AFile) then
    raise EAbort.Create(FLang.GetString(86));

  // Init dialog
  SaveDialog := TSaveDialog.Create(Self);

  try
    with SaveDialog do
    begin
      Title := StripHotkey(mmCopyIcon.Caption);
      Filter := GraphicFilter(TBitmap);
      DefaultExt := '.bmp';
      Options := Options + [ofOverwritePrompt];
      FileName := ChangeFileExt(ExtractFileName(AFile), DefaultExt);
    end;  //of with

    // Save clicked
    if SaveDialog.Execute then
    begin
      // Destination file is a *.bmp file?
      if (ExtractFileExt(SaveDialog.FileName) <> SaveDialog.DefaultExt) then
        raise EAbort.Create(FLang.GetString(88));

      Image := TPicture.Create;
      Bitmap := TBitmap.Create;

      // Try to convert any picture to bitmap
      try
        Image.LoadFromFile(AFile);
        Bitmap.Width := Image.Width;
        Bitmap.Height := Image.Height;
        Bitmap.Canvas.Draw(0, 0, Image.Graphic);
        Bitmap.SaveToFile(SaveDialog.FileName);

        FLang.ShowMessage(FLang.Format(87, [SaveDialog.FileName]));
        Result := SaveDialog.FileName;

      finally
        Bitmap.Free;
        Image.Free;
      end;  //of try
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
      if AExportEdits then
        Title := StripHotkey(mmExportEdit.Caption)
      else
        Title := StripHotkey(mmExport.Caption);

      Options := Options + [ofOverwritePrompt];
      FileName := FLang.GetString(64);

      // Set OS dependend filter
      if (Win32MajorVersion >= 6) then
      begin
        // Windows >= Vista: Export as *.ini and *.reg
        Filter := FLang.GetString(65);
        FilterIndex := 2;
      end  //of begin
      else
      begin
        // Windows < Vista: Export only as *.ini
        Filter := FLang.GetString(66);
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
          2: (SupportInfo as TSupportInformation).SaveAsReg(SaveDialog.FileName);
        end;  //of case

    finally
      SupportInfo.Free;
      SaveDialog.Free;
    end;  //of try

  except
    on E: Exception do
      FLang.ShowException(FLang.GetString(79), E.Message);
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
  if (FLang.ShowMessage(FLang.GetString(70), mtConfirmation) = IDYES) then
  try
    // Manufacturer is essential!
    if (Trim(eMan.Text) = '') then
    begin
      FLang.EditBalloonTip(eMan.Handle, 2, 83, biError);
      Exit;
    end;  //of begin

    IconPath := eLogo.Text;

    // Icon seems to exist?
    if (IconPath <> '') then
    begin
      if not FileExists(IconPath) then
      begin
        FLang.EditBalloonTip(eLogo.Handle, 2, 86, biError);
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
          FLang.EditBalloonTip(eLogo.Handle, 2, 88, biError);
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
    FLang.ShowMessage(FLang.GetString(75));

  except
    on E: EAbort do
      FLang.ShowMessage(E.Message, mtWarning);

    on E: Exception do
      FLang.ShowException(FLang.GetString(81), E.Message);
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
      Title := FLang.GetString(63);

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
    if OpenDialog.Execute then
    begin
      Image := TPicture.Create;

      try
        Image.LoadFromFile(OpenDialog.FileName);

        // Check square format of image and warn user
        if (Image.Height <> Image.Width) then
        begin
          FLang.ShowMessage(FLang.Format([68, 69], [Image.Width, Image.Height]),
            mtWarning);
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
      if (Win32MajorVersion >= 6) then
      begin
        Filter := FLang.GetString(65);
        FilterIndex := 2;
      end  //of begin
      else
        // Windows < Vista: Import only *.ini files
        Filter := FLang.GetString(66);
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
      FLang.ShowException(FLang.GetString(82), E.Message);
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
    FLang.ShowMessage(FLang.GetString(83), mtWarning)
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
  if (FLang.ShowMessage(FLang.GetString(71), mtCustom) = IDYES) then
  begin
    if (FLang.ShowMessage(FLang.GetString(72), mtConfirmation) = IDYES) then
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
      FLang.ShowMessage(FLang.GetString(74));
    end  //of begin
    else
      FLang.ShowMessage(FLang.GetString(76), mtError);
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
      FLang.ShowException(FLang.GetString(80), E.Message);
  end;  //of try
end;

{ TMain.mmDeleteIconClick

  Allows users to delete the support information icon. }

procedure TMain.mmDeleteIconClick(Sender: TObject);
begin
  // Show confirmation
  if (FLang.ShowMessage(FLang.Format(73, [FSupportInfo.Icon]), mtCustom) = IDYES) then
    if FSupportInfo.DeleteOEMIcon() then
    begin
      mmDeleteIcon.Enabled := False;
      mmCopyIcon.Enabled := False;
      FSupportInfo.Icon := '';
      eLogo.Clear;
    end  //of begin
    else
      FLang.ShowMessage(FLang.GetString(77), mtError);
end;

{ TMain.mmGerClick

  MainMenu entry that allows to change the current language to german. }

procedure TMain.mmGerClick(Sender: TObject);
begin
  FLang.ChangeLanguage(LANG_GERMAN);
end;

{ TMain.mmEngClick

  MainMenu entry that allows to change the current language to english. }

procedure TMain.mmEngClick(Sender: TObject);
begin
  FLang.ChangeLanguage(LANG_ENGLISH);
end;

{ TMain.mmFraClick

  MainMenu entry that allows to change the current language to french. }

procedure TMain.mmFreClick(Sender: TObject);
begin
  FLang.ChangeLanguage(LANG_FRENCH);
end;

{ TMain.mmDownloadCertClick

  MainMenu entry that allows to download the PM Code Works certificate. }

procedure TMain.mmDownloadCertClick(Sender: TObject);
var
  Updater: TUpdate;

begin
  // Certificate already installed?
  if (TUpdate.PMCertificateExists() and (FLang.ShowMessage(27, 28,
    mtConfirmation) = IDNO)) then
    Exit;

  // Init downloader
  Updater := TUpdate.Create(Self, FLang);

  // Download certificate
  try
    with Updater do
    begin
      Title := FLang.GetString(16);
      FileNameRemote := 'cert.reg';
      FileNameLocal := 'PMCW-Certificate.reg';
      DownloadDirectory := GetTempDir();
    end;  //of begin

    // Successfully downloaded certificate?
    if Updater.Execute() then
      mmDownloadCert.Enabled := False;

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
  if ((eUrl.Text <> '') and (FLang.ShowMessage(FLang.GetString(89), mtConfirmation) = IDYES)) then
  begin
    // Try to open URL
    if not OpenUrl(eUrl.Text) then
      FLang.EditBalloonTip(eUrl.Handle, 2, 90, biError);
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
