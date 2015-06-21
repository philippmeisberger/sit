{ *********************************************************************** }
{                                                                         }
{ SIT API Interface Unit v3.2                                             }
{                                                                         }
{ Copyright (c) 2011-2015 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit SitAPI;

interface

uses
  Windows, Classes, SysUtils, Registry, ShellAPI, PMCWIniFileParser, PMCWOSUtils;

const
  { Ini-file section name constants }
  INI_GENERAL      = 'General';
  INI_SUPPORT_INFO = 'Support Information';

  { OEM information location >= Windows 2000 }
  OEMINFO_LOGO     = '\System32\OEMLOGO.bmp';
  OEMINFO_INFO     = '\System32\OEMINFO.ini';

  { OEM information location >= Windows Vista }
  OEMINFO_KEY      = 'SOFTWARE\Microsoft\Windows\CurrentVersion\OEMInformation';

  { Registry value name constants }
  INFO_ICON        = 'Logo';
  INFO_MAN         = 'Manufacturer';
  INFO_MODEL       = 'Model';
  INFO_PHONE       = 'SupportPhone';
  INFO_HOURS       = 'SupportHours';
  INFO_URL         = 'SupportURL';

type
  { Support information base class }
  TSupportInformationBase = class(TObject)
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
    procedure Show(AOwner: HWND = 0); virtual; abstract;
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
    procedure Save(); override;
    procedure SaveAsReg(const AFilename: string);
    procedure Show(AOwner: HWND = 0); override;
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
    procedure Show(AOwner: HWND = 0); override;
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
      FIcon := ReadString(INFO_ICON, INFO_ICON);
      FMan := ReadString(INI_GENERAL, INFO_MAN);
      FModel := ReadString(INI_GENERAL, INFO_MODEL);
      FUrl := ReadString(INI_GENERAL, INFO_URL);
      FPhone := ReadString(INI_SUPPORT_INFO, INFO_PHONE);
      FHours := ReadString(INI_SUPPORT_INFO, INFO_HOURS);
    end;  //of with

  finally
    ini.Free;
  end;  //of try
end;

{ public TSupportInformationBase.SaveAsIni

  Saves a TSupportInformationBase object as an *.ini file. }

procedure TSupportInformationBase.SaveAsIni(const AFilename: string);
var
  Ext: string;
  Ini: TIniFile;

begin
  if (ExtractFileExt(AFileName) = '') then
    Ext := '.ini'
  else
    Ext := '';

  Ini := TIniFile.Create(AFileName + Ext, True);

  try
    Ini.AddRemove(INFO_ICON, INFO_ICON, FIcon);
    Ini.AddRemove(INI_GENERAL, INFO_MAN, FMan);
    Ini.AddRemove(INI_GENERAL, INFO_MODEL, FModel);
    Ini.AddRemove(INI_GENERAL, INFO_URL, FUrl);
    Ini.AddRemove(INI_SUPPORT_INFO, INFO_PHONE, FPhone);
    Ini.AddRemove(INI_SUPPORT_INFO, INFO_HOURS, FHours);

    // Save file
    Ini.Save();

  finally
    Ini.Free;
  end;  //of try
end;


{ TSupportInformation }

{ public TSupportInformation.DeleteIcon

  Deletes the support information icon. }

function TSupportInformation.DeleteOEMIcon(): Boolean;
var
  Reg: TRegistry;
  Icon: string;

begin
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_WRITE or KEY_READ);

  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    if Reg.OpenKey(OEMINFO_KEY, False) then
    begin
      // Remove icon file and registry value
      Icon := Reg.ReadString(INFO_ICON);
      Result := DeleteFile(Icon) and Reg.DeleteValue(INFO_ICON);
    end  //of begin
    else
      Result := False;

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

{ public TSupportInformation.Exists

  Checks if any support information exist. }
  
function TSupportInformation.Exists(): Boolean;
var
  Reg: TRegistry;

begin
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ);

  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKey(OEMINFO_KEY, True);
    Result := (Reg.ValueExists(INFO_ICON) or Reg.ValueExists(INFO_MAN) or
               Reg.ValueExists(INFO_MODEL) or Reg.ValueExists(INFO_PHONE) or
               Reg.ValueExists(INFO_HOURS) or Reg.ValueExists(INFO_URL));

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

{ public TSupportInformation.GetOEMIcon

  Returns path to support information icon. }

function TSupportInformation.GetOEMIcon(): string;
var
  Reg: TRegistry;

begin
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ);

  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKey(OEMINFO_KEY, False);

    if Reg.ValueExists(INFO_ICON) then
      Result := Reg.ReadString(INFO_ICON)
    else
      Result := '';

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

{ public TSupportInformation.Load

  Loads fresh support information. }

procedure TSupportInformation.Load();
var
  Reg: TRegistry;

  function ReadValue(AValueName: string): string;
  begin
    if Reg.ValueExists(AValueName) then
      Result := Reg.ReadString(AValueName)
    else
      Result := '';
  end;
  
begin
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ);

  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKey(OEMINFO_KEY, True);

    // Read all OEM information
    FIcon := ReadValue(INFO_ICON);
    FHours := ReadValue(INFO_HOURS);
    FMan := ReadValue(INFO_MAN);
    FModel := ReadValue(INFO_MODEL);
    FPhone := ReadValue(INFO_PHONE);
    FUrl := ReadValue(INFO_URL);

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

{ public TSupportInformation.LoadFromReg

  Loads a TSupportInformation object from a *.reg file. }

procedure TSupportInformation.LoadFromReg(const AFilename: string);
var
  RegFile: TRegistryFile;
  RegSection: string;

begin
  RegFile := TRegistryFile.Create(AFilename);

  try
    RegSection := RegFile.GetSection(HKEY_LOCAL_MACHINE, OEMINFO_KEY);
    FIcon := RegFile.ReadString(RegSection, INFO_ICON);
    FMan := RegFile.ReadString(RegSection, INFO_MAN);
    FModel := RegFile.ReadString(RegSection, INFO_MODEL);
    FPhone := RegFile.ReadString(RegSection, INFO_PHONE);
    FHours := RegFile.ReadString(RegSection, INFO_HOURS);
    FUrl := RegFile.ReadString(RegSection, INFO_URL);

  finally
    RegFile.Free;
  end;  //of try
end;

{ public TSupportInformation.Remove

  Removes support information from the Windows Registry. }

function TSupportInformation.Remove(): Boolean;
var
  Reg: TRegistry;

begin
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_WRITE);

  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    if Reg.OpenKey(ExtractFileDir(OEMINFO_KEY), False) then
      Result := Reg.DeleteKey(ExtractFileName(OEMINFO_KEY))
    else
      Result := False;

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

{ public TSupportInformation.Save

  Commits changes on support information. }

procedure TSupportInformation.Save();
var
  Reg: TRegistry;

  procedure WriteValue(AValueName, AValue: string);
  begin
    if (AValue <> '') then
      Reg.WriteString(AValueName, AValue)
    else
      if Reg.ValueExists(AValueName) then
        Reg.DeleteValue(AValueName);
  end;

begin
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ or KEY_WRITE);

  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKey(OEMINFO_KEY, True);

    WriteValue(INFO_HOURS, FHours);
    WriteValue(INFO_ICON, FIcon);
    WriteValue(INFO_MAN, FMan);
    WriteValue(INFO_MODEL, FModel);
    WriteValue(INFO_PHONE, FPhone);
    WriteValue(INFO_URL, FUrl);

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

{ public TSupportInformation.SaveAsReg

  Saves a TSupportInformation object as a *.reg file. }

procedure TSupportInformation.SaveAsReg(const AFilename: string);
var
  RegFile: TRegistryFile;
  ext, Section: string;

begin
  if (ExtractFileExt(AFileName) = '') then
    ext := '.reg'
  else
    ext := '';

  // init *.reg file
  RegFile := TRegistryFile.Create(AFilename + ext);

  try
    RegFile.MakeHeadline();
    Section := RegFile.GetSection(HKEY_LOCAL_MACHINE, OEMINFO_KEY);
    RegFile.AddRemove(Section, INFO_ICON, FIcon);
    RegFile.AddRemove(Section, INFO_MAN, FMan);
    RegFile.AddRemove(Section, INFO_MODEL, FModel);
    RegFile.AddRemove(Section, INFO_PHONE, FPhone);
    RegFile.AddRemove(Section, INFO_HOURS, FHours);
    RegFile.AddRemove(Section, INFO_URL, FUrl);

    // Save file
    RegFile.Save();

  finally
    RegFile.Free;
  end;  //of try
end;

{ public TSupportInformation.Show

  Shows Windows system properties. }

procedure TSupportInformation.Show(AOwner: HWND = 0);
begin
  ShellExecute(AOwner, nil, 'control', 'system', nil, SW_SHOWNORMAL);
end;


{ TSupportInformationXP }

{ private TSupportInformationXP.GetOEMInfo

  Returns path to OEMINFO.ini }

function TSupportInformationXP.GetOEMInfo(): string;
begin
  Result := GetWinDir() + OEMINFO_INFO;
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
  Result := DeleteFile(FIcon);
end;

{ public TSupportInformationXP.Exists

  Checks if any support information exist. }
  
function TSupportInformationXP.Exists(): Boolean;
begin
  Result := FileExists(GetOemInfo());
end;

{ public TSupportInformationXP.GetOEMIcon

  Returns path to support information logo. }

function TSupportInformationXP.GetOEMIcon(): string;
begin
  Result := GetWinDir() + OEMINFO_LOGO;
end;

{ public TSupportInformationXP.Load

  Loads fresh support information. }

procedure TSupportInformationXP.Load();
var
  Ini: TIniFile;

begin
  // Read OEMINFO.ini
  Ini := TIniFile.Create(GetOEMInfo());

  try
    with Ini do
    begin
      FMan := ReadString(INI_GENERAL, INFO_MAN);
      FModel := ReadString(INI_GENERAL, INFO_MODEL);
      FUrl := ReadString(INI_GENERAL, INFO_URL);
      FPhone := ReadString(INI_SUPPORT_INFO, 'Line3');
      FHours := ReadString(INI_SUPPORT_INFO, 'Line4');
    end;  //of wit

  finally
    Ini.Free;
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
  Result := DeleteFile(GetOEMInfo());
end;

{ public TSupportInformationXP.Save

  Commits changes on support information. }

procedure TSupportInformationXP.Save();
var
  Ini: TIniFile;
  OEMIcon: string;

begin
  ini := TIniFile.Create(GetOEMInfo());

  try
    with Ini do
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
  
    // Save file
    Ini.Save();

  finally
    ini.Free;
  end;  //of try

  OEMIcon := GetOEMIcon();

  // Delete current icon when new icon is empty and old still exists
  if ((FIcon = '') and FileExists(OEMIcon)) then
  begin
    if DeleteFile(OEMIcon) then
      FIcon := ''
    else
      raise Exception.Create('Error while deleting icon!');
  end  //of begin
  else
    begin
      // Copy logo only if paths differ
      if ((FIcon <> '') and (FIcon <> OEMIcon)) then
        if CopyFile(PChar(FIcon), PChar(OEMIcon), False) then
          FIcon := OEMIcon
        else
          raise Exception.Create('Error while copying icon!');
    end;  //of if
end;

{ public TSupportInformationXP.Show

  Shows Windows system properties. }

procedure TSupportInformationXP.Show(AOwner: HWND = 0);
begin
  ShellExecute(AOwner, nil, 'sysdm.cpl', nil, nil, SW_SHOWNORMAL);
end;

end.
