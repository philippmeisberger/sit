{ *********************************************************************** }
{                                                                         }
{ Support Information Tool Main Unit                                      }
{                                                                         }
{ Copyright (c) 2011-2018 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit SitMain;

interface

uses
  Winapi.Windows, System.SysUtils, Vcl.Graphics, System.Classes, Vcl.Controls,
  Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Menus, Vcl.Dialogs, Vcl.ExtDlgs,
  Vcl.Imaging.jpeg, System.UITypes, Winapi.Knownfolders, SitAPI, PMCW.Dialogs,
  PMCW.LanguageFile, PMCW.SysUtils, PMCW.Controls, PMCW.Application;

type
  { TMain }
  TMain = class(TMainForm)
    bApply: TButton;
    bShowSupport: TButton;
    MainMenu: TMainMenu;
    mmFile: TMenuItem;
    mmImport: TMenuItem;
    mmExport: TMenuItem;
    mmEdit: TMenuItem;
    mmDeleteValues: TMenuItem;
    mmHelp: TMenuItem;
    mmExportEdit: TMenuItem;
    N1: TMenuItem;
    mmDeleteEdits: TMenuItem;
    mmDeleteIcon: TMenuItem;
    mmView: TMenuItem;
    mmLang: TMenuItem;
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
    mmRefresh: TMenuItem;
    N6: TMenuItem;
    lCopy: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bApplyClick(Sender: TObject);
    procedure bShowSupportClick(Sender: TObject);
    procedure bAddClick(Sender: TObject);
    procedure mmImportClick(Sender: TObject);
    procedure mmCopyIconClick(Sender: TObject);
    procedure mmExportClick(Sender: TObject);
    procedure mmExportEditClick(Sender: TObject);
    procedure mmRefreshClick(Sender: TObject);
    procedure mmDeleteValuesClick(Sender: TObject);
    procedure mmDeleteEditsClick(Sender: TObject);
    procedure mmDeleteIconClick(Sender: TObject);
    procedure eLogoDblClick(Sender: TObject);
    procedure eUrlDblClick(Sender: TObject);
    procedure lCopyClick(Sender: TObject);
    procedure lCopyMouseEnter(Sender: TObject);
    procedure lCopyMouseLeave(Sender: TObject);
  private
    FSupportInfo: TSupportInformationBase;
    procedure RefreshEdits(ASupportInfo: TSupportInformationBase);
    function ShowCopyIconDialog(const AFile: string): string;
    procedure ShowExportDialog(AExportEdits: Boolean);
    procedure Refresh(AReload: Boolean = True);
  protected
    procedure LanguageChanged(); override;
  end;

var
  Main: TMain;

implementation

{$R *.dfm}
{$I LanguageIDs.inc}

{ TMain.FormCreate

  VCL event that is called when form is being created. }

procedure TMain.FormCreate(Sender: TObject);
begin
  // Setup languages
  FLang := TLanguageFile.Create(100);
  FLang.AddListener(Self);

  // Build menus
  BuildLanguageMenu(mmLang);
  BuildHelpMenu(mmHelp);

  // Init update notificator
  CheckForUpdate('SIT', 'sit.exe', 'sit64.exe', 'SIT.exe');

  // Init support information instance
  if CheckWin32Version(6) then
    FSupportInfo := TSupportInformation.Create
  else
    FSupportInfo := TSupportInformationXP.Create;

  // Copy icon only available for Vista and later
  cbCopyIcon.Enabled := CheckWin32Version(6);

  // Show support information
  Refresh();
end;

{ TMain.FormDestroy

  VCL event that is called when form is being destroyed. }

procedure TMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSupportInfo);
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

{ private TMain.LanguageChanged

  Updates all component captions with new language text. }

procedure TMain.LanguageChanged();
begin
  inherited LanguageChanged();

  with FLang do
  begin
    // Set captions for TMenuItems
    mmFile.Caption := GetString(LID_FILE);
    mmImport.Caption := GetString(LID_IMPORT);
    mmExport.Caption := GetString(LID_EXPORT);
    mmExportEdit.Caption := GetString(LID_INPUT_EXPORT);

    // "Edit" menu
    mmEdit.Caption := GetString(LID_EDIT);
    mmRefresh.Caption := GetString(LID_ITEMS_SHOW);
    mmDeleteValues.Caption := GetString(LID_ITEMS_DELETE);
    mmDeleteEdits.Caption := GetString(LID_INPUT_DELETE);
    mmCopyIcon.Caption := GetString(LID_ICON_COPY);
    mmDeleteIcon.Caption := GetString(LID_ICON_DELETE);

    // "View" menu
    mmView.Caption := GetString(LID_VIEW);

    // Set captions for labels
    gbIcon.Caption := GetString(LID_ICON);
    eLogo.EditLabel.Caption := GetString(LID_ICON_FILE);
    cbCopyIcon.Caption := mmCopyIcon.Caption;

    gbInfo.Caption := GetString(LID_INFORMATION);
    eMan.EditLabel.Caption := GetString(LID_MANUFACTURER);
    ePhone.EditLabel.Caption := GetString(LID_PHONE);
    eHours.EditLabel.Caption := GetString(LID_HOURS);
    eModel.EditLabel.Caption := GetString(LID_MODEL);
    eUrl.EditLabel.Caption := GetString(LID_URL);

    // Set captions for buttons
    bApply.Caption := GetString(LID_APPLY);
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

    MessageDlg(FLang.Format(LID_ICON_COPIED, [FileName]), mtInformation, [mbOK], 0);
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
      ExceptionDlg(FLang, FLang.GetString(LID_ERROR_WRITING_FILE), E.Message);
  end;  //of try
end;

{ private TMain.Refresh

  Refreshs the support information. }

procedure TMain.Refresh(AReload: Boolean = True);
begin
  Caption := Application.Title;

  // Reload support information?
  if AReload then
    FSupportInfo.Load();

  mmDeleteIcon.Enabled := FileExists(FSupportInfo.GetOEMIcon());
  mmCopyIcon.Enabled := mmDeleteIcon.Enabled;
  mmDeleteValues.Enabled := FSupportInfo.Exists();
  mmExport.Enabled := mmDeleteValues.Enabled;

  // Load content of loaded support information into text fields
  RefreshEdits(FSupportInfo);
end;

{ TMain.bApplyClick

  Allows user to commit changes on support information. }

procedure TMain.bApplyClick(Sender: TObject);
var
  IconPath: string;

begin
  // Confirm save progress
  if (MessageDlg(FLang.GetString(LID_ITEMS_SAVE_CONFIRM), mtConfirmation,
    mbYesNo, 0) = idYes) then
  try
    // Manufacturer is essential!
    if (Trim(eMan.Text) = '') then
    begin
      eMan.ShowBalloonTip(FLang.GetString(LID_ERROR), FLang.GetString(LID_NOTHING_ENTERED), biError);
      Exit;
    end;  //of begin

    IconPath := eLogo.Text;

    // Icon seems to exist?
    if (IconPath <> '') then
    begin
      if not FileExists(IconPath) then
      begin
        eLogo.ShowBalloonTip(FLang.GetString(LID_ERROR), FLang.GetString(LID_ERROR_ICON_NOT_EXIST), biError);
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
          eLogo.ShowBalloonTip(FLang.GetString(LID_ERROR), FLang.GetString(LID_ERROR_INVALID_EXT), biError);
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

    Refresh(False);

    // Show success message in best case
    MessageDlg(FLang.GetString(LID_ITEMS_SAVED), mtInformation, [mbOK], 0);

  except
    on E: EAbort do
      MessageDlg(E.Message, mtWarning, [mbOK], 0);

    on E: Exception do
      ExceptionDlg(FLang, FLang.GetString(LID_ERROR_SAVING), E.Message);
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
      begin
        // Open picture folder of current user
        if CheckWin32Version(6) then
          InitialDir := GetKnownFolderPath(FOLDERID_Pictures);
      end;  //of if
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
          MessageDlg(FLang.Format([LID_ICON_NOT_SQUARE1, LID_ICON_NOT_SQUARE2],
            [Image.Width, Image.Height]), mtWarning, [mbOK], 0);
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
        Caption := Application.Title +' - '+ ExtractFileName(OpenDialog.FileName);

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
      ExceptionDlg(FLang, FLang.GetString(LID_ERROR_IMPORTING), E.Message);
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
    MessageDlg(FLang.GetString(LID_NOTHING_ENTERED), mtWarning, [mbOK], 0)
  else
    ShowExportDialog(True);
end;

{ TMain.mmShowClick

  Refreshs the support information. }

procedure TMain.mmRefreshClick(Sender: TObject);
begin
  Refresh();
end;

{ TMain.mmDeleteValuesClick

  Allows users to delete support information. }

procedure TMain.mmDeleteValuesClick(Sender: TObject);
begin
  // Show confirmation before deleting
  if (MessageDlg(FLang.GetString(LID_ITEMS_DELETE_CONFIRM), mtWarning,
    mbYesNo, 0, mbNo) = idYes) then
  begin
    // Ask user to export
    if (MessageDlg(FLang.GetString(LID_ITEMS_DELETE_STORE), mtConfirmation,
      mbYesNo, 0) = idYes) then
      ShowExportDialog(False);

    try
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
        MessageDlg(FLang.GetString(LID_ITEMS_DELETED), mtInformation, [mbOK], 0);
      end  //of begin
      else
        MessageDlg(FLang.GetString(LID_ERROR_DELETING), mtError, [mbOK], 0);

    except
      on E: Exception do
        ExceptionDlg(FLang, FLang.GetString(LID_ERROR_DELETING), E.Message);
    end;  //of try
  end;  //of begin
end;

{ TMain.mmDeleteEditsClick

  Allows users to clear all text fields. }

procedure TMain.mmDeleteEditsClick(Sender: TObject);
begin
  Caption := Application.Title;
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
      MessageDlg(E.Message, mtWarning, [mbOK], 0);

    on E: Exception do
      ExceptionDlg(FLang, FLang.GetString(LID_ERROR_COPYING), E.Message);
  end;  //of try
end;

{ TMain.mmDeleteIconClick

  Allows users to delete the support information icon. }

procedure TMain.mmDeleteIconClick(Sender: TObject);
begin
  // Show confirmation
  if (MessageDlg(FLang.Format(LID_ICON_DELETE_CONFIRM, [FSupportInfo.Icon]),
    mtWarning, mbYesNo, 0, mbNo) = idYes) then
  try
    if FSupportInfo.DeleteOEMIcon() then
    begin
      mmDeleteIcon.Enabled := False;
      mmCopyIcon.Enabled := False;
      FSupportInfo.Icon := '';
      eLogo.Clear;
    end  //of begin
    else
      MessageDlg(FLang.GetString(LID_ERROR_DELETING_ICON), mtError, [mbOK], 0);

  except
    on E: Exception do
      ExceptionDlg(FLang, FLang.GetString(LID_ERROR_DELETING_ICON), E.Message);
  end;  //of try
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
  if ((eUrl.Text <> '') and (MessageDlg(FLang.GetString(LID_URL_OPEN),
    mtConfirmation, mbYesNo, 0) = idYes)) then
  begin
    // Only open HTTP URLs
    if ((not string(eUrl.Text).StartsWith('http://') and
      not string(eUrl.Text).StartsWith('https://')) or not OpenUrl(eUrl.Text)) then
      eUrl.ShowBalloonTip(FLang.GetString(LID_ERROR), FLang.GetString(LID_ERROR_INVALID_URL), biError);
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
