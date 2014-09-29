{ *********************************************************************** }
{                                                                         }
{ SIT API Interface Unit v3.1                                             }
{                                                                         }
{ Copyright (c) 2011-2014 P.Meisberger (PM Code Works)                    }
{                                                                         }
{ *********************************************************************** }

unit SitAPI;

interface

uses
  Windows, Classes, SysUtils, Registry, IniFiles, OSUtils, ShellAPI;

const
  { Ini-file section name constants }
  INI_GENERAL = 'General';
  INI_SUPPORT_INFO = 'Support Information';

  { OEM information location >= Windows 2000 }
  OEMINFO_LOGO = '\System32\OEMLOGO.bmp';
  OEMINFO_INFO = '\System32\OEMINFO.ini';

  { OEM information location >= Windows Vista }
  OEMINFO_KEY = 'SOFTWARE\Microsoft\Windows\CurrentVersion\OEMInformation';

  { Registry value name constants }
  INFO_ICON = 'Logo';
  INFO_MAN = 'Manufacturer';
  INFO_MODEL = 'Model';
  INFO_PHONE = 'SupportPhone';
  INFO_HOURS = 'SupportHours';
  INFO_URL = 'SupportURL';

type
  { Support information base class }
  TSupportInformationBase = class(TWinWOW64)
  private
    FIcon, FMan, FModel, FUrl, FPhone, FHours: string;
  public
    constructor Create(AIcon, AMan, AModel, AUrl, APhone, AHours: string); overload;
    constructor Create(ASupportInformationBase: TSupportInformationBase); overload;
    procedure Clear(); virtual;
    function DeleteOEMIcon(): Boolean; virtual; abstract;
    function Exists(): Boolean; virtual; abstract;
    function GetOEMIcon(): string; virtual; abstract;
    procedure Load(); virtual; abstract;
    procedure LoadFromIni(const AFilename: string);
    function Remove(): Boolean; virtual; abstract;
    procedure Save(); virtual; abstract;
    procedure SaveAsIni(const AFilename: string);
    procedure Show(AHandle: HWND); virtual; abstract;
    { external }
    property Hours: string read FHours write FHours;
    property Icon: string read FIcon write FIcon;
    property Manufacturer: string read FMan write FMan;
    property Model: string read FModel write FModel;
    property Url: string read FUrl write FUrl;
    property Phone: string read FPhone write FPhone;    
  end;

  { Support information class >= Windows Vista }
  TSupportInformation = class(TSupportInformationBase)
  public
    function DeleteOEMIcon(): Boolean; override;
    function Exists(): Boolean; override;
    function GetOEMIcon(): string; override;
    procedure Load(); override;
    procedure LoadFromReg(const AFilename: string);
    function Remove(): Boolean; override;
    procedure SaveAsReg(const AFilename: string);
    procedure Save(); override;
    procedure Show(AHandle: HWND); override;
  end;

  { Support information class >= Windows 2000 }
  TSupportInformationXP = class(TSupportInformationBase)
  private
    function GetOEMInfo(): string;
  public
    procedure Clear(); override;
    function DeleteOEMIcon(): Boolean; override;
    function Exists(): Boolean; override;
    function GetOEMIcon(): string; override;
    procedure Load(); override;
    function Remove(): Boolean; override;
    procedure Save(); override;
    procedure Show(AHandle: HWND); override;
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

{ public TSupportInformationBase.Create

  Copy constructor for creating a copy of a TSupportInformationBase instance. }

constructor TSupportInformationBase.Create(ASupportInformationBase: TSupportInformationBase);
begin
  inherited Create;
  FIcon := ASupportInformationBase.Icon;
  FMan := ASupportInformationBase.Manufacturer;
  FModel := ASupportInformationBase.Model;
  FHours := ASupportInformationBase.Hours;
  FPhone := ASupportInformationBase.Phone;
  FUrl := ASupportInformationBase.Url;
end;

{ public TSupportInformationBase.Clear

  Clears the entered support information. }

procedure TSupportInformationBase.Clear();
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
    with ini do
    begin
      FIcon := ReadString(INFO_ICON, INFO_ICON, '');
      FMan := ReadString(INI_GENERAL, INFO_MAN, '');
      FModel := ReadString(INI_GENERAL, INFO_MODEL, '');
      FUrl := ReadString(INI_GENERAL, INFO_URL, '');
      FPhone := ReadString(INI_SUPPORT_INFO, INFO_PHONE, '');
      FHours := ReadString(INI_SUPPORT_INFO, INFO_HOURS, '');
    end;  //of with

  finally
    ini.Free;
  end;  //of try
end;

{ public TSupportInformationBase.SaveAsIni

  Saves a TSupportInformationBase object as an *.ini file. }

procedure TSupportInformationBase.SaveAsIni(const AFilename: string);
var
  ext: string;
  ini: TIniFile;

  procedure AppendToFile(const ASection, AName, AValue: string);
  begin
    if (AValue <> '') then
      ini.WriteString(ASection, AName, AValue)
    else
      if ini.ValueExists(ASection, AName) then
        ini.DeleteKey(ASection, AName);
  end;

begin
  if (ExtractFileExt(AFileName) = '') then
    ext := '.ini'
  else
    ext := '';

  ini := TIniFile.Create(AFileName + ext);

  try
    AppendToFile(INFO_ICON, INFO_ICON, FIcon);
    AppendToFile(INI_GENERAL, INFO_MAN, FMan);
    AppendToFile(INI_GENERAL, INFO_MODEL, FModel);
    AppendToFile(INI_GENERAL, INFO_URL, FUrl);
    AppendToFile(INI_SUPPORT_INFO, INFO_PHONE, FPhone);
    AppendToFile(INI_SUPPORT_INFO, INFO_HOURS, FHours);

  finally
    ini.Free;
  end;  //of try
end;


{ TSupportInformation }

{ public TSupportInformation.DeleteIcon

  Deletes the support information icon. }

function TSupportInformation.DeleteOEMIcon(): Boolean;
var
  reg: TRegistry;
  Icon: string;

begin
  reg := TRegistry.Create(DenyWOW64Redirection(KEY_WRITE or KEY_READ));

  try
    reg.RootKey := HKEY_LOCAL_MACHINE;

    if reg.OpenKey(OEMINFO_KEY, False) then
    begin
      // Remove icon file and registry value
      Icon := reg.ReadString(INFO_ICON);
      result := DeleteFile(Icon) and reg.DeleteValue(INFO_ICON);
    end  //of begin
    else
      result := False;

  finally
    reg.CloseKey();
    reg.Free;
  end;  //of try
end;

{ public TSupportInformation.Exists

  Checks if any support information exist. }
  
function TSupportInformation.Exists(): Boolean;
var
  reg: TRegistry;

begin
  reg := TRegistry.Create(DenyWOW64Redirection(KEY_READ));

  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    reg.OpenKey(OEMINFO_KEY, True);
    result := (reg.ValueExists(INFO_ICON) or reg.ValueExists(INFO_MAN) or
               reg.ValueExists(INFO_MODEL) or reg.ValueExists(INFO_PHONE) or
               reg.ValueExists(INFO_HOURS) or reg.ValueExists(INFO_URL));

  finally
    reg.CloseKey();
    reg.Free;
  end;  //of try
end;

{ public TSupportInformation.GetOEMIcon

  Returns path to support information icon. }

function TSupportInformation.GetOEMIcon(): string;
var
  reg: TRegistry;

begin
  reg := TRegistry.Create(DenyWOW64Redirection(KEY_READ));

  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    reg.OpenKey(OEMINFO_KEY, False);

    if reg.ValueExists(INFO_ICON) then
      result := reg.ReadString(INFO_ICON)
    else
      result := '';

  finally
    reg.CloseKey();
    reg.Free;
  end;  //of try
end;

{ public TSupportInformation.Load

  Loads fresh support information. }

procedure TSupportInformation.Load();
var
  reg: TRegistry;

  function ReadValue(AValueName: string): string;
  begin
    if reg.ValueExists(AValueName) then
      result := reg.ReadString(AValueName)
    else
      result := '';
  end;
  
begin
  reg := TRegistry.Create(DenyWOW64Redirection(KEY_READ));

  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    reg.OpenKey(OEMINFO_KEY, True);

    // Read all OEM information
    FIcon := ReadValue(INFO_ICON);
    FHours := ReadValue(INFO_HOURS);
    FMan := ReadValue(INFO_MAN);
    FModel := ReadValue(INFO_MODEL);
    FPhone := ReadValue(INFO_PHONE);
    FUrl := ReadValue(INFO_URL);   
    
  finally
    reg.CloseKey();
    reg.Free;
  end;  //of try
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
  end;  //of try
end;

{ public TSupportInformation.Remove

  Removes support information from the Windows Registry. }

function TSupportInformation.Remove(): Boolean;
var
  reg: TRegistry;

begin
  reg := TRegistry.Create(DenyWOW64Redirection(KEY_WRITE));

  try
    reg.RootKey := HKEY_LOCAL_MACHINE;

    if reg.OpenKey(ExtractFileDir(OEMINFO_KEY), False) then
      result := reg.DeleteKey(ExtractFileName(OEMINFO_KEY))
    else
      result := False;

  finally
    reg.CloseKey();
    reg.Free;
  end;  //of try
end;

{ public TSupportInformation.Save

  Commits changes on support information. }

procedure TSupportInformation.Save();
var
  reg: TRegistry;

  procedure WriteValue(AValueName, AValue: string);
  begin
    if (AValue <> '') then
      reg.WriteString(AValueName, AValue)
    else
      if reg.ValueExists(AValueName) then
        reg.DeleteValue(AValueName);
  end;

begin
  reg := TRegistry.Create(DenyWOW64Redirection(KEY_ALL_ACCESS));

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
  end;  //of try
end;

{ public TSupportInformation.Show

  Shows Windows system properties. }

procedure TSupportInformation.Show(AHandle: HWND);
begin
  ShellExecute(AHandle, 'open', 'control', 'system', nil, SW_SHOWNORMAL);
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
  regFile := TStringList.Create;
  regFile.Append('Windows Registry Editor Version 5.00');
  regFile.Append('');
  regFile.Append('[HKEY_LOCAL_MACHINE\'+ OEMINFO_KEY +']');

  try
    path := StringReplace(FIcon, '\', '\\', [rfReplaceAll]);
    AppendToFile(INFO_ICON, path);
    AppendToFile(INFO_MAN, FMan);
    AppendToFile(INFO_MODEL, FModel);
    AppendToFile(INFO_PHONE, FPhone);
    AppendToFile(INFO_HOURS, FHours);
    AppendToFile(INFO_URL, FUrl);

    if (ExtractFileExt(AFileName) = '') then
      ext := '.reg'
    else
      ext := '';
       
    regFile.SaveToFile(AFilename + ext);

  finally
    regFile.Free;
  end;  //of try
end;


{ TSupportInformationXP }

{ private TSupportInformationXP.GetOEMInfo

  Returns path to OEMINFO.ini }

function TSupportInformationXP.GetOEMInfo(): string;
begin
  result := TOSUtils.GetWinDir() + OEMINFO_INFO;
end;

{ public TSupportInformationXP.Clear

  Clears the entered support information except the icon. }

procedure TSupportInformationXP.Clear();
begin
  FHours := '';
  FMan := '';
  FModel := '';
  FPhone := '';
  FUrl := '';
end;

{ public TSupportInformationXP.DeleteOEMIcon

  Deletes the OEMLOGO.bmp if exists. }

function TSupportInformationXP.DeleteOEMIcon(): Boolean;
begin
  result := DeleteFile(FIcon);
end;

{ public TSupportInformationXP.Exists

  Checks if any support information exist. }
  
function TSupportInformationXP.Exists(): Boolean;
begin
  result := FileExists(GetOemInfo());
end;

{ public TSupportInformationXP.GetOEMIcon

  Returns path to support information logo. }

function TSupportInformationXP.GetOEMIcon(): string;
begin
  result := TOSUtils.GetWinDir() + OEMINFO_LOGO;
end;

{ public TSupportInformationXP.Load

  Loads fresh support information. }

procedure TSupportInformationXP.Load();
var
  ini: TIniFile;

begin
  // Read OEMINFO.ini
  ini := TIniFile.Create(GetOEMInfo());

  try
    with ini do
    begin
      FMan := ReadString(INI_GENERAL, INFO_MAN, '');
      FModel := ReadString(INI_GENERAL, INFO_MODEL, '');
      FUrl := ReadString(INI_GENERAL, INFO_URL, '');
      FPhone := ReadString(INI_SUPPORT_INFO, 'Line3', '');
      FHours := ReadString(INI_SUPPORT_INFO, 'Line4', '');
    end;  //of with

  finally
    ini.Free;
  end;  //of try

  // OEMLOGO.bmp exists?
  if FileExists(GetOEMIcon()) then
    FIcon := GetOEMIcon()
  else
    FIcon := '';
end;

{ public TSupportInformationXP.Remove

  Removes support information. }

function TSupportInformationXP.Remove(): Boolean;
begin
  result := DeleteFile(GetOEMInfo());
end;

{ public TSupportInformationXP.Save

  Commits changes on support information. }

procedure TSupportInformationXP.Save();
var
  ini: TIniFile;
  OEMIcon: string;

begin
  ini := TIniFile.Create(GetOEMInfo());

  try
    with ini do
    begin
      WriteString(INI_GENERAL, INFO_MAN, FMan);
      WriteString(INI_GENERAL, INFO_MODEL, FModel);
      WriteString(INI_GENERAL, INFO_URL, FUrl);
      WriteString(INI_SUPPORT_INFO, 'Line1', FMan);
      WriteString(INI_SUPPORT_INFO, 'Line2', FModel);
      WriteString(INI_SUPPORT_INFO, 'Line3', FPhone);
      WriteString(INI_SUPPORT_INFO, 'Line4', FHours);
      WriteString(INI_SUPPORT_INFO, 'Line5', FUrl);
    end;  //of with

  finally
    ini.Free;
  end;  //of try

  OEMIcon := GetOEMIcon();

  // Copy logo if exists
  if (FileExists(FIcon) and (FIcon <> OEMIcon)) then
    if CopyFile(PChar(FIcon), PChar(OEMIcon), False) then
      FIcon := OEMIcon
    else
      raise Exception.Create('Error while copying icon!');
end;

{ public TSupportInformationXP.Show

  Shows Windows system properties. }

procedure TSupportInformationXP.Show(AHandle: HWND);
begin
  ShellExecute(AHandle, nil, 'sysdm.cpl', nil, nil, SW_SHOWNORMAL);
end;

end.
