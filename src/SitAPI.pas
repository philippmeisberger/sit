{ *********************************************************************** }
{                                                                         }
{ SIT API Interface Unit v3.0                                             }
{                                                                         }
{ Copyright (c) 2011-2014 P.Meisberger (PM Code Works)                    }
{                                                                         }
{ *********************************************************************** }

unit SitAPI;

interface

uses
  Windows, Classes, SysUtils, Registry, TLHelp32, IniFiles, WinUtils;

const
  OEMINFO_KEY = 'SOFTWARE\Microsoft\Windows\CurrentVersion\OEMInformation';
  LOGO_DIR = '\System32\OEMLOGO.bmp';
  INFO_DIR = '\System32\OEMINFO.ini';
  URL_BASE = 'http://www.pm-codeworks.de/';
  URL_CONTACT = URL_BASE +'kontakt.html';

  INFO_ICON = 'Logo';
  INFO_MAN = 'Manufacturer';
  INFO_MODEL = 'Model';
  INFO_PHONE = 'SupportPhone';
  INFO_HOURS = 'SupportHours';
  INFO_URL = 'SupportURL';
  INFO_GENERAL = 'General';
  
type
  { Support information base class }
  TSupportInformationBase = class(TWinUtils)
  private
    FIcon, FMan, FModel, FUrl, FPhone, FHours: string;
  public
    constructor Create(AIcon, AMan, AModel, AUrl, APhone, AHours: string); overload;
    procedure Clear();
    function DeleteIcon(): Boolean; override;
    function Exists(): Boolean; virtual; abstract;
    function GetOEMLogo(): string; virtual; abstract;
    procedure Load(); virtual; abstract;
    procedure LoadFromIni(const AFilename: string);
    function Remove(): Boolean; virtual; abstract;
    function Save(): Boolean; virtual; abstract;
    procedure SaveAsIni(const AFilename: string);
    { external }
    property Logo: string read FIcon write FIcon;
    property Manufacturer: string read FMan write FMan;
    property Model: string read FModel write FModel;
    property Url: string read FUrl write FUrl;
    property Phone: string read FPhone write FPhone;
    property Hours: string read FHours write FHours;
  end;

  { Support information class }
  TSupportInformation = class(TSupportInformationBase)
  public
    function DeleteIcon(): Boolean; override;
    function Exists(): Boolean; override;
    function GetOEMLogo(): string; override;
    procedure Load(); override;
    procedure LoadFromReg(const AFilename: string);
    function Remove(): Boolean; override;
    procedure SaveAsReg(const AFilename: string);
    function Save(): Boolean; override;
    procedure Show(); override;
  end;

  { Support information class until Windows XP }
  TSupportInformationXP = class(TSupportInformationBase)
  private
    function GetOEMInfo(): string;
  public
    function DeleteIcon(): Boolean; override;
    function Exists(): Boolean; override;
    function GetOEMLogo(): string; override;
    procedure Load(); override;
    function Remove(): Boolean; override;
    function Save(): Boolean; override;
    procedure Show(); override;
  end;


implementation

{ TSupportInformationBase }

{ public TSupportInformationBase.Create

  General constructor for creating a TSupportInformationBase instance. }
  
constructor TSupportInformationBase.Create(AIcon, AMan, AModel, AUrl, APhone,
  AHours: string);
begin
  inherited Create;
  FIcon := AIcon;
  FMan := AMan;
  FModel := AModel;
  FUrl := AUrl;
  FPhone := APhone;
  FHours := AHours;
end;

{ protected TSupportInformationBase.AccessData

  Commits changes on *.ini based support information. }

{function TSupportInformationBase.AccessData(AIniFile: TIniFile; const ASection,
  AValueName, AValue: string): Boolean;
var
  list: TStringList;
  
begin
  if (AValue <> '') then
  begin
    AIniFile.WriteString(ASection, AValueName, AValue);
    result := true;
  end  //of begin
  else
    begin
    list := TStringList.Create;
     
    try
      if (AIniFile.SectionExists(ASection) and AIniFile.ValueExists(ASection, AValueName)) then
      begin
        AIniFile.DeleteKey(ASection, AValueName);
        AIniFile.ReadSection(ASection, list);

        // Delete section if empty
        if (list.Count = 0) then
          AIniFile.EraseSection(ASection);
        end;  //of begin
   
     result := true;

    finally
      list.Free;
    end;  //of finally
  end;  //of begin
end;}

{ public TSupportInformationBase.Clear

  Clears the entered support information. }

procedure TSupportInformationBase.Clear;
begin
  FHours := '';
  FIcon := '';
  FMan := '';
  FModel := '';
  FPhone := '';
  FUrl := '';
end;

{ public TSupportInformationBase.LoadFromIni

  Loads a TSupportInformationBase object from an *.ini file. }

procedure TSupportInformationBase.LoadFromIni(const AFilename: string);
var
  ini: TIniFile;

begin
  ini := TIniFile.Create(AFileName);

  try
    FIcon := ini.ReadString(INFO_ICON, INFO_ICON, '');
    FMan := ini.ReadString(INFO_GENERAL, INFO_MAN, '');
    FModel := ini.ReadString(INFO_GENERAL, INFO_MODEL, '');
    FUrl := ini.ReadString(INFO_GENERAL, INFO_URL, '');
    FPhone := ini.ReadString(SUPPORT_INFO, INFO_PHONE, '');
    FHours := ini.ReadString(SUPPORT_INFO, INFO_HOURS, '');
    
  finally
    ini.Free;
  end;  //of finally
end;

{ public TSupportInformationBase.SaveAsIni

  Saves a TSupportInformationBase object as an *.ini file. }

procedure TSupportInformationBase.SaveAsIni(const AFilename: string);
var
  ext: string;
  ini: TIniFile;
  
begin
  if (ExtractFileExt(AFileName) = '') then
    ext := '.ini'
  else
    ext := '';
      
  ini := TIniFile.Create(AFileName + ext);

  try
    ini.WriteString(INFO_ICON, INFO_ICON, FIcon);
    ini.WriteString(INFO_GENERAL, INFO_MAN, FMan);
    ini.WriteString(INFO_GENERAL, INFO_MODEL, FModel);
    ini.WriteString(INFO_GENERAL, INFO_URL, FUrl);
    ini.WriteString(SUPPORT_INFO, INFO_PHONE, FPhone);
    ini.WriteString(SUPPORT_INFO, INFO_HOURS, FHours);

  finally
    ini.Free;
  end;  //of finally
end;


{ TSupportInformation }

{ private TSupportInformation.AccessData

  Commits changes on support information. }

function TSupportInformation.AccessData(AReg: TRegistry; const AValueName,
  AValue: string): Boolean;
begin
  if (AValue <> '') then
  begin
    AReg.WriteString(AValueName, AValue);
    result := True;
  end  //of begin
  else
    if AReg.ValueExists(AValueName) then
      result := AReg.DeleteValue(AValueName);
    else
      result := True;
end;

{ public TSupportInformation.Exists

  Checks if any support information exist. }
  
function TSupportInformation.Exists(): Boolean;
var
  reg: TRegistry;

begin
  reg := TRegistry.Create(SetKeyAccessMode());

  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    reg.OpenKey(OEMINFO_KEY, True);
    result := (reg.ValueExists('Logo') or reg.ValueExists('Manufacturer') or
              reg.ValueExists('Model') or reg.ValueExists('SupportPhone') or
              reg.ValueExists('SupportHours') or reg.ValueExists('SupportURL'));

  finally
    reg.CloseKey();
    reg.Free;
  end;  //of finally
end;

{ public TSupportInformation.GetOEMLogo

  Returns path to support information logo. }

function TSupportInformation.GetOEMLogo(): string;
var
  reg: TRegistry;

begin
  reg := TRegistry.Create(SetKeyAccessMode);

  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    reg.OpenKey(OEMINFO_KEY, false);
    result := reg.ReadString(INFO_ICON);

  finally
    reg.CloseKey();
    reg.Free;
  end;  //of finally
end;

{ public TSupportInformation.Load

  Loads fresh support information. }

procedure TSupportInformation.Load();
var
  reg: TRegistry;

  function ReadValue(AValueName: string): string;
  begin
    if reg.ValueExists(AValueName) then
      result := reg.ReadString(AValueName);
    else
      result := '';
  end;
  
begin
  reg := TRegistry.Create(SetKeyAccessMode());

  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    reg.OpenKey(OEMINFO_KEY, True);

    FIcon := ReadValue(INFO_ICON);
    FHours := ReadValue(INFO_HOURS);
    FMan := ReadValue(INFO_MAN);
    FModel := ReadValue(INFO_MODEL);
    FPhone := ReadValue(INFO_PHONE);
    FUrl := ReadValue(INFO_URL);   
    
  finally
    reg.CloseKey();
    reg.Free;
  end;  //of finally
end;

{ public TSupportInformation.LoadFromReg

  Loads a TSupportInformation object from a *.reg file. }

procedure TSupportInformation.LoadFromReg(const AFilename: string);
var
  regFile: TStringList;
  icon: string;

  function DelPathIndicator(AName: string): string;
  begin
    result := StringReplace(AName, '"', '', [rfReplaceAll]);
  end;

begin
  regFile := TStringList.Create;

  try
    regFile.LoadFromFile(AFilename);
    icon := StringReplace(regFile.Values['"'+INFO_ICON+'"'], '\\', '\', [rfReplaceAll]);
    FIcon := DelPathIndicator(icon);
    FMan := DelPathIndicator(regFile.Values['"'+INFO_MAN+'"']);
    FModel := DelPathIndicator(regFile.Values['"'+INFO_MODEL+'"']);
    FPhone := DelPathIndicator(regFile.Values['"'+INFO_PHONE+'"']);
    FHours := DelPathIndicator(regFile.Values['"'+INFO_HOURS+'"']);
    FUrl := DelPathIndicator(regFile.Values['"'+INFO_URL+'"']);

  finally
    regFile.Free;
  end;  //of finally
end;

{ public TSupportInformation.Remove

  Removes support information from the Windows Registry. }

function TSupportInformation.Remove(): Boolean;
var
  reg: TRegistry;

begin
  reg := TRegistry.Create(SetKeyAccessMode());

  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    reg.OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion', True);
    result := reg.DeleteKey('OEMInformation');

  finally
    reg.CloseKey();
    reg.Free;
  end;  //of finally
end;

{ public TSupportInformation.Save

  Commits changes on support information. }

procedure TSupportInformation.Save();
var
  reg: TRegistry;

  procedure WriteValue(AName, AValue: string);
  begin
    if (AValue <> '') then
       reg.WriteString(AName, AValue);
  end;

begin
  reg := TRegistry.Create(SetKeyAccessMode());

  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    reg.OpenKey(OEMINFO_KEY, True);

    WriteValue(INFO_HOURS, FHours);
    WriteValue(INFO_ICON, FIcon);
    WriteValue(INFO_MAN, FMan);
    WriteValue(INFO_MODEL, FModel);
    WriteValue(INFO_PHONE, FPhone);
    WriteValue(INFO_URL, FUrl);

  finally
    reg.CloseKey();
    reg.Free;
  end;  //of finally
end;

{ public TSupportInformation.Show

  Shows Windows system properties. }

procedure TSupportInformation.Show(AHandle: HWND);
begin
  ShellExecute(AHandle, nil, 'control', 'system', nil, SW_SHOWNORMAL);
end;

{ public TSupportInformation.SaveAsReg

  Saves a TSupportInformation object as a *.reg file. }  

procedure TSupportInformation.SaveAsReg(const AFilename: string);
var
  regFile: TStringList;
  path, ext: string;

  procedure AppendToFile(AName, AValue: string);
  begin
    if (AValue <> '') then
      regFile.Append('"'+ AName +'"="'+ AValue +'"');
  end;
  
begin
  ext := '';
  regFile := TStringList.Create;
  regFile.Append('Windows Registry Editor Version 5.00');
  regFile.Append('');
  regFile.Append('[HKEY_LOCAL_MACHINE\'+ OEMINFO_KEY +']');

  try
    path := StringReplace(FIcon, '\', '\\', [rfReplaceAll]);
    AppendToFile('Logo', path);
    AppendToFile('Manufacturer', FMan);
    AppendToFile('Model', FModel);
    AppendToFile('SupportPhone', FPhone);
    AppendToFile('SupportHours', FHours);
    AppendToFile('SupportURL', FUrl);

    if (ExtractFileExt(AFileName) = '') then
       ext := '.reg';
       
    regFile.SaveToFile(AFilename + ext);

  finally
    regFile.Free;
  end;  //of finally
end;


{ TSupportInformationXP }

{ private TSupportInformationXP.GetOEMInfo

  Returns path to OEMINFO.ini }

function TSupportInformationXP.GetOEMInfo(): string;
begin
  ChangeFSRedirection(True);                
  result := GetWinDir() + INFO_DIR;
  ChangeFSRedirection(False);
end;

{ public TSupportInformationXP.DeleteIcon

  Deletes the OEMLOGO.bmp if exists and changes OEMINFO.ini }

function TSupportInformationXP.DeleteIcon(): Boolean;
var
  ini: TIniFile;

begin
  if DeleteFile(GetOemLogo()) then
  begin
    // OEMINFO.ini exists?
    if not FileExists(GetOemInfo()) then
       result := True;
       Exit();
       end;  //of begin

    // Delete logo path from *.ini file
    ini := TIniFile.Create(GetOemInfo());

    try
      result := AccessData(ini, 'Logo', 'Logo', '');
      
    finally
      ini.Free;
    end;  //of finally    
  end;  //of begin  
end;

{ public TSupportInformationXP.Exists

  Checks if any support information exist. }
  
function TSupportInformationXP.Exists(): Boolean;
begin
  result := FileExists(GetOemInfo()) or FileExists(GetOemLogo());
end;

{ public TSupportInformationXP.GetOEMLogo

  Returns path to support information logo. }

function TSupportInformationXP.GetOEMLogo(): string;
begin
  ChangeFSRedirection(True);
  result := GetWinDir() + LOGO_DIR;
  ChangeFSRedirection(False);
end;

{ public TSupportInformationXP.Load

  Loads fresh support information. }

procedure TSupportInformationXP.Load();
begin
  LoadFromIni(GetOemInfoDir());
end;

{ public TSupportInformation.Remove

  Removes support information. }

function TSupportInformationXP.Remove(): Boolean;
begin
  result := DeleteFile(GetOemInfoDir());
end;

{ public TSupportInformationXP.Save

  Commits changes on support information. }

procedure TSupportInformationXP.Save();
begin
  SaveAsIni(GetOemInfoDir());

  // Copy logo if exists
  if FileExists(eLogo.Text) then
    CopyFile(PChar(FIcon), PChar(GetOemLogoDir()), False);
end;

{ public TSupportInformationXP.Show

  Shows Windows system properties. }

procedure TSupportInformationXP.Show();
begin
  ShellExecute(AHandle, nil, 'sysdm.cpl', nil, nil, SW_SHOWNORMAL);
end;

end.
