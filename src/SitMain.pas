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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Menus, SitAPI, ExtDlgs, ShellAPI;

type
  TForm1 = class(TForm)
    iBack: TImage;
    lCopy: TLabel;
    lVersion: TLabel;
    Image: TImage;
    eLogo: TLabeledEdit;
    eMan: TLabeledEdit;
    eUrl: TLabeledEdit;
    eModel: TLabeledEdit;
    ePhone: TLabeledEdit;
    bAccept: TButton;
    bClose: TButton;
    MainMenu: TMainMenu;
    mmFile: TMenuItem;
    mmImport: TMenuItem;
    mmExport: TMenuItem;
    mmEdit: TMenuItem;
    mmShowValues: TMenuItem;
    mmDelValues: TMenuItem;
    mmHelp: TMenuItem;
    mmInfo: TMenuItem;
    SaveDialog: TSaveDialog;
    OpenDialog: TOpenDialog;
    OpenLogoDialog: TOpenPictureDialog;
    eHours: TLabeledEdit;
    bAdd: TButton;
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure bAcceptClick(Sender: TObject);
    procedure bAddClick(Sender: TObject);
    procedure mmImportClick(Sender: TObject);
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
    procedure bCloseClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FSit: TSupportInformationBase;
    procedure DoExport(ADirect: Boolean);
    procedure OpenLogo();
    procedure SetLanguage(ALangID: Word);
  public
    procedure ChangeLanguage(AMenuItem: TMenuItem; ALangID: integer);
    {function MessageBox(Text: string; CaptionID, Flags: Integer): Integer; overload;
    function MessageBox(Text: string; CaptionID: Integer = 39): Integer; overload;
    function MessageBox(TextID, CaptionID: Integer; Flags: Integer): Integer; overload;}
  end;

var
  Form1: TForm1;

implementation

uses SitInfo, SitUpdate;

{$R *.dfm}
{$R manifest.res}
{$R lang.res}

{ private }
procedure TForm1.DoExport(ADirect: Boolean);                       //Exportieren
var
  saveDialog : TSaveDialog;
  sitData: TSitData;
  ext: string;

begin
  saveDialog := TSaveDialog.Create(self);           //init saveDialog
  saveDialog.Title := sit.GetString(23);
  saveDialog.FileName := sit.GetString(25);
  sitData := TSitData.Create(eLogo.Text, eMan.Text, eModel.Text, eUrl.Text,
                              ePhone.Text, eHours.Text);
  try
    if sitData.CheckWindows then
       begin
       saveDialog.Filter := sit.GetString(26);
       openDialog.FilterIndex := 2;
       end  //of begin
    else
       saveDialog.Filter := sit.GetString(27);

    if saveDialog.Execute then                  //"speichern" click
       begin
       if FileExists(saveDialog.FileName) then  //Datei existiert
          begin
          if (MessageBox(saveDialog.FileName + sit.GetString(29) +^J+^J+
             sit.GetString(30), 40, MB_YESNO or MB_ICONWARNING) = IDNO) then  //"nicht ersetzen"
             begin
             saveDialog.Free;
             Exit;
             end;  //of begin
          end  //of begin
       else
         case saveDialog.FilterIndex of         //Dateiendung f�r neue Datei
           1: ext := '.ini';
           2: ext := '.reg';
         end; //of case

         try                                    //in Datei schreiben
           case saveDialog.FilterIndex of
             1: sitData.SaveAsIni(saveDialog.FileName + ext, ADirect);
             2: sitData.SaveAsReg(saveDialog.FileName + ext, ADirect);
           end; //of case

         finally                                //Dialog Objekt freigeben
           saveDialog.Free;
         end; //of finally
       end;  //of if

  finally                                       //Export Objekt freigeben
    sitData.Free;
  end;  //of finally
end;


procedure TForm1.OpenLogo;                                      //Logo einbinden
var
  OpenLogoDialog : TOpenPictureDialog;

begin
OpenLogoDialog := TOpenPictureDialog.Create(Self);     //init OpenLogoDialog

  try
    if (eLogo.Text <> '') then               //wenn logo existiert, dir. �ffnen
       begin
       OpenLogoDialog.InitialDir := eLogo.Text;
       OpenLogoDialog.FileName := ExtractFileName(eLogo.Text);
       end  //of begin
    else
       OpenLogoDialog.InitialDir := GetCurrentDir;

    OpenLogoDialog.Options := OpenLogoDialog.Options + [ofFileMustExist];
    OpenLogoDialog.Filter := sit.GetString(28);
    OpenLogoDialog.Title := sit.GetString(24);

    if OpenLogoDialog.Execute then
       begin                                                //"�ffnen" click
       Image.Picture.LoadFromFile(OpenLogoDialog.FileName);

       if ((Image.Height > 400) or (Image.Width > 500)) then  //pr�fen ob nicht > 400x500 Px
          begin    // > 400x500 Px
          MessageBox(sit.GetString(31)+ IntToStr(Image.Height) +'x'+
                     IntToStr(Image.Width) +')!' +^J+ sit.GetString(32), 40);
          OpenLogoDialog.Free;
          OpenLogo;              //neues pic ausw�hlen
          Exit;
          end  //of begin
       else
          begin    //< 400x500 Px
          eLogo.Text := OpenLogoDialog.FileName;
          OpenLogoDialog.Free;
          end; //of if
       end; //of if

  finally
    eLogo.SetFocus;
  end; //of finally
end;

{ private TMain.SetLanguage

  Updates all component captions with new language text. }

procedure TForm1.SetLanguage(ALangID: integer);
begin
  sit.LangID := ALangID;

  with sit do
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
    mmDelLogo.Caption := GetString(8);

    mmView.Caption := GetString(9);
    mmLang.Caption := GetString(10);

    mmHelp.Caption := GetString(66);
    mmUpdate.Caption := GetString(11);
    mmDwnldCert.Caption := GetString(12);
    mmReport.Caption := GetString(70);
    mmInfo.Caption := GetString(13);

    // Set captions for labels
    eLogo.EditLabel.Caption := GetString(14);
    eMan.EditLabel.Caption := GetString(15);
    ePhone.EditLabel.Caption := GetString(16);
    eHours.EditLabel.Caption := GetString(17);
    eModel.EditLabel.Caption := GetString(18);
    eUrl.EditLabel.Caption := GetString(19);

    // Set captions for buttons
    bAccept.Caption := GetString(20);
    bClose.Caption := GetString(21);

    Form3.bFinished.Caption := GetString(62);
    Form2.Caption := GetString(67);
    end;  //of with
end;
{ of private }

{ public }
procedure TForm1.ChangeLanguage(AMenuItem: TMenuItem; ALangID: integer);
begin
  AMenuItem.Checked := not AMenuItem.Checked;
  SetLanguage(ALangID);
end;

{
function TForm1.MessageBox(Text: string; CaptionID, Flags: Integer): Integer;
begin
  result := Application.MessageBox(PChar(Text), PChar(sit.GetString(CaptionID)), Flags);
end;


function TForm1.MessageBox(Text: string; CaptionID: Integer = 39): Integer;
var
  Flags: Integer;

begin
  case CaptionID of
    39: Flags := MB_ICONINFORMATION;
    40: Flags := MB_ICONWARNING;
    42: Flags := MB_ICONERROR;
  else
    Flags := 0;
  end;  //of case

  result := Application.MessageBox(PChar(Text), PChar(sit.GetString(CaptionID)), Flags);
end;


function TForm1.MessageBox(TextID, CaptionID: Integer; Flags: Integer): Integer;
begin
  result := Application.MessageBox(PChar(sit.GetString(TextID)),
              PChar(sit.GetString(CaptionID)), Flags);
end;}

{ TMain.FormCreate }

procedure TForm1.FormCreate(Sender: TObject);
begin
  if TWinUtils.CheckWindows() then
     FSit := TSupportInformation.Create
  else
     FSit := TSupportInformationXP.Create;
end;

{ TMain.FormDestroy }

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FSit.Free;
end;


procedure TForm1.FormShow(Sender: TObject);
const
  BCM_FIRST = $1600;
  BCM_SETSHIELD = BCM_FIRST + $000C;

begin
  // Check for incompatibility
  if (FSit.GetWinVersion()[1] = 'u')) then
     begin
     MessageBox(sit.GetString(64) + FSit.GetWinVersion() + sit.GetString(65), 42);
     bAccept.Enabled := false;
     mmFile.Enabled := false;
     mmEdit.Enabled := false;
     eLogo.Enabled := false;
     eMan.Enabled := false;
     ePhone.Enabled := false;
     eHours.Enabled := false;
     eModel.Enabled := false;
     eUrl.Enabled := false;
     bClose.Default := true;
     Exit;
     end; //of begin

  // Show support information
  mmShowValuesClick(Self);

  // Make UAC-Shield button
  SendMessage(bAccept.Handle, BCM_SETSHIELD, 0, integer(True));  
end;


procedure TForm1.bAcceptClick(Sender: TObject);
begin
  if (MessageBox(sit.GetString(33), 43, MB_ICONQUESTION or MB_YESNO) = IDYES) then
     begin
     FSit.Logo := eLogo.Text;
     FSit.Manufacturer = eMan.Text;
     FSit.Model := eModel.Text;
     FSit.Url := eUrl.Text;
     FSit.Phone := ePhone.Text;
     FSit.Hours := eHours.Text;
     
     try
       // Add support information      
       if FSit.Add() then
          MessageBox(sit.GetString(38));

     finally
       sitData.Free;
     end;  //of finally
     end;  //of begin

  mmShowValues.Click;
end;


procedure TForm1.bAddClick(Sender: TObject);
begin
  OpenLogo;
end;


procedure TForm1.mmImportClick(Sender: TObject);                  //Importieren
var
  openDialog : TOpenDialog;
  regFile: TStringList;
  logo: string;

begin
openDialog := TOpenDialog.Create(self);                  //init openDialog
openDialog.Title := sit.GetString(22);
openDialog.Options := openDialog.Options + [ofFileMustExist];

if sit.CheckWindows then
   begin
   openDialog.Filter := sit.GetString(26);
   openDialog.FilterIndex := 2;
   end  //of begin
else
   openDialog.Filter := sit.GetString(27);


if openDialog.Execute then                               //"�ffnen" click
   begin
   Caption := sit.SetCaption +' - '+ ExtractFileName(openDialog.FileName);

   case openDialog.FilterIndex of
     1: try                            //lesen aus *.ini Datei
          eLogo.Text := sit.GetIniValue(openDialog.FileName, 'Logo', 'Logo');
          eMan.Text := sit.GetIniValue(openDialog.FileName, 'General', 'Manufacturer');
          eModel.Text := sit.GetIniValue(openDialog.FileName,'General', 'Model');
          eUrl.Text := sit.GetIniValue(openDialog.FileName,'General', 'SupportURL');
          ePhone.Text := sit.GetIniValue(openDialog.FileName, SUPP_SEC, 'SupportPhone');
          eHours.Text := sit.GetIniValue(openDialog.FileName, SUPP_SEC, 'SupportHours');    

        finally
          openDialog.Free;
        end; //of finally

     2: begin
        regFile := TStringList.Create;                   //init regFile

          try                                            //lesen aus *.reg Datei
            regFile.LoadFromFile(openDialog.FileName);   //Dateiimport
            Caption := sit.SetCaption +' - '+ ExtractFileName(openDialog.FileName);

            logo := StringReplace(regFile.Values['"Logo"'], '\\', '\', [rfReplaceAll]);
            eLogo.Text := sit.DelGooseFoots(logo);

            eMan.Text := sit.DelGooseFoots(regFile.Values['"Manufacturer"']);
            eModel.Text := sit.DelGooseFoots(regFile.Values['"Model"']);
            ePhone.Text := sit.DelGooseFoots(regFile.Values['"SupportPhone"']);
            eHours.Text := sit.DelGooseFoots(regFile.Values['"SupportHours"']);
            eUrl.Text := sit.DelGooseFoots(regFile.Values['"SupportURL"']);

          finally
            regFile.Free;
            openDialog.Free;
          end; //of finally
        end; //of begin
     end; //of case
   end; //of if
end;


procedure TForm1.mmExportClick(Sender: TObject);
begin
  DoExport(true);
end;


procedure TForm1.mmExportEditClick(Sender: TObject);
begin
  if ((eLogo.Text = '') and (eMan.Text = '') and (eModel.Text = '') and
     (eUrl.Text = '') and (ePhone.Text = '') and (eHours.Text = '')) then
     MessageBox(sit.GetString(53), 40)
  else
     DoExport(false);
end;


procedure TForm1.mmShowValuesClick(Sender: TObject);        //Eintr�ge anzeigen
begin
mmDelEditClick(Sender);

  try
    if not sit.CheckWindows then          //auslesen der OEM Eintr�ge
       begin
       eMan.Text := sit.GetIniValue(sit.GetOemInfoDir, 'General', 'Manufacturer');
       eModel.Text := sit.GetIniValue(sit.GetOemInfoDir, 'General', 'Model');
       eUrl.Text := sit.DelGooseFoots(sit.GetIniValue(sit.GetOemInfoDir, SUPP_SEC, 'Line2'));
       ePhone.Text := sit.DelGooseFoots(sit.GetIniValue(sit.GetOemInfoDir, SUPP_SEC, 'Line3'));
       eHours.Text := sit.DelGooseFoots(sit.GetIniValue(sit.GetOemInfoDir, SUPP_SEC, 'Line4'));

       if FileExists(sit.GetOemLogoDir) then
          begin
          eLogo.Text := sit.GetOemLogoDir;
          mmDelLogo.Visible := true;
          end;  //of begin
       end  //of begin
    else
       begin                              //auslesen der reg-Eintr�ge
       eLogo.Text := sit.GetRegValue('Logo');
       eMan.Text := sit.GetRegValue('Manufacturer');
       eModel.Text := sit.GetRegValue('Model');
       ePhone.Text := sit.GetRegValue('SupportPhone');
       eHours.Text := sit.GetRegValue('SupportHours');
       eUrl.Text := sit.GetRegValue('SupportURL');
       end; //of begin

    mmDelValues.Enabled := sit.DataExists;
    mmexport.Enabled := mmDelValues.Enabled;

  except
    sit.CreateError(46, 52);
  end; //of except
end;

{ TMain.mmDelValuesClick

  MainMenu entry that allows users to delete support information. }

procedure TForm1.mmDelValuesClick(Sender: TObject);
begin
  if (MessageBox(sit.GetString(34), 43, MB_ICONQUESTION or MB_YESNO) = IDYES) then
    begin
    if (MessageBox(35, 41, MB_ICONQUESTION or MB_YESNO) = IDYES) then
       DoExport(True);                  //exportieren

    // Delete information
    if FSit.Remove() then
    begin
      MessageBox(sit.GetString(37));
      mmDelValues.Enabled := False;
    end  //of begin
    else
      MessageBox(44, 52);
    end  //of begin


    else
      begin
        if FileExists(sit.GetOemLogoDir) then
           mmDelLogoClick(Sender);          //Logo l�schen

        if DeleteFile(sit.GetOemInfoDir) then
           begin
           MessageBox(sit.GetString(37));
           mmDelValues.Enabled := false;
           end  //of begin
        else
           sit.CreateError(44, 52);
        end; //of if
     end;  //of begin
end;

{ TMain.mmDelEditClick

  MainMenu entry that allows users to clear text fields. }

procedure TForm1.mmDelEditClick(Sender: TObject);
begin
  Caption := sit.SetCaption;                //FormCaption setzen
  eLogo.Clear;                              //Editfelder leeren
  eMan.Clear;
  eModel.Clear;
  ePhone.Clear;
  eHours.Clear;
  eUrl.Clear;
  eMan.SetFocus;                            //Fokus auf Hersteller
end;

{ TMain.mmDelLogoClick

  MainMenu entry that allows users to delete the OEMLOGO.bmp }

procedure TForm1.mmDelLogoClick(Sender: TObject);
begin
  if (MessageBox(sit.GetString(36), 41, MB_ICONQUESTION or MB_YESNO) = IDYES) then
    if FSit.DeleteLogo() then
    begin
      mmDelLogo.Visible := False;
      eLogo.Clear;
    end  //of begin
    else
      sit.CreateError(45, 52);
end;


procedure TForm1.mmGerClick(Sender: TObject);
begin
  ChangeLanguage(mmGer, 100);
end;


procedure TForm1.mmEngClick(Sender: TObject);
begin
  ChangeLanguage(mmEng, 200);
end;


procedure TForm1.mmFraClick(Sender: TObject);
begin
  ChangeLanguage(mmFra, 300);
end;

{ TMain.mmUpdateClick

  MainMenu entry that allows users to manually search for updates. }

procedure TForm1.mmUpdateClick(Sender: TObject);
begin
  FUpdateCheck.CheckForUpdate(True);
end;

{ TMain.mmDwnldCertClick

  MainMenu entry that allows to download the PM Code Works certificate. }

procedure TForm1.mmDwnldCertClick(Sender: TObject);
begin
  // Certificate already installed?
  if (TGameWake.PMCertExists() and (FLang.MessageBox(FLang.GetString(67) +^J
     + FLang.GetString(68), mtQuestion) = IDYES)) then
     // Download certificate
     with TUpdate.Create(Self, FLang, FLang.GetString(8)) do
       Download(dtCert);
end;

{ TMain.mmReportClick

  MainMenu entry that allows users to easily report a bug by opening the web
  browser and using the "report bug" formular. }

procedure TForm1.mmReportClick(Sender: TObject);
begin
  FSit.OpenUrl(URL_CONTACT);
end;

{ TMain.mmInfoClick

  MainMenu entry that shows a info page with build number and version history. }

procedure TForm1.mmInfoClick(Sender: TObject);
var
  Info: TInfo;

begin
  Application.CreateForm(TInfo, Info);
  Info.ShowModal;
  Info.Free;
end;

{ Edit-Felder }
procedure TForm1.eLogoDblClick(Sender: TObject);
begin
  if (eLogo.Text = '') then
     OpenLogo
  else
     eLogo.SelectAll;
end;


procedure TForm1.eManDblClick(Sender: TObject);
begin
  eMan.SelectAll;
end;


procedure TForm1.eModelDblClick(Sender: TObject);
begin
  eModel.SelectAll;
end;


procedure TForm1.ePhoneDblClick(Sender: TObject);
begin
  ePhone.SelectAll;
end;


procedure TForm1.eHoursDblClick(Sender: TObject);
begin
  eHours.SelectAll;
end;


procedure TForm1.eUrlDblClick(Sender: TObject);
begin
  eUrl.SelectAll;
end;

{ TMain.lCopyClick

  Opens the homepage of PM Code Works in a web browser. }

procedure TForm1.lCopyClick(Sender: TObject);
begin
  FSit.OpenUrl(URL_BASE);
end;

{ TMain.lCopyMouseEnter

  Allows a label to have the look like a hyperlink.  }

procedure TForm1.lCopyMouseEnter(Sender: TObject);
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

procedure TForm1.lCopyMouseLeave(Sender: TObject);
begin
  with (Sender as TLabel) do
    begin
    Font.Style := Font.Style - [fsUnderline];
    Font.Color := clBlack;
    Cursor := crDefault;
    end;  //of with
end;


procedure TForm1.bCloseClick(Sender: TObject);
begin
  Close;
end;


procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Assigned(Form2) then
     Form2.Close;
  Form3.Close;
end;
{ of public }

end.
