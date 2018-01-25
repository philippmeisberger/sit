{ *********************************************************************** }
{                                                                         }
{ SIT API Interface Unit                                                  }
{                                                                         }
{ Copyright (c) 2011-2018 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit SitAPI;

interface

uses
  Winapi.Windows, System.SysUtils, Registry, Winapi.ShellAPI, Winapi.SHFolder,
  PMCW.IniFileParser, PMCW.SysUtils;

const
  { Ini-file section name constants }
  INI_GENERAL      = 'General';
  INI_SUPPORT_INFO = 'Support Information';

  { OEM information location >= Windows Vista }
  KEY_OEMINFO      = 'SOFTWARE\Microsoft\Windows\CurrentVersion\OEMInformation';

  { Registry value name constants }
  INFO_ICON        = 'Logo';
  INFO_MAN         = 'Manufacturer';
  INFO_MODEL       = 'Model';
  INFO_PHONE       = 'SupportPhone';
  INFO_HOURS       = 'SupportHours';
  INFO_URL         = 'SupportURL';

type
  /// <summary>
  ///   <c>TSupportInformationBase</c> is the abstract class for managing
  ///   support information.
  /// </summary>
  TSupportInformationBase = class abstract(TObject)
  private
    FIcon,
    FMan,
    FModel,
    FUrl,
    FPhone,
    FHours: string;
  public
    /// <summary>
    ///   Constructor for creating a <c>TSupportInformationBase</c> instance
    /// </summary>
    /// <param name="AIcon">
    ///   The icon to show.
    /// </param>
    /// <param name="AMan">
    ///   The manufacturer.
    /// </param>
    /// <param name="AModel">
    ///   The model.
    /// </param>
    /// <param name="AUrl">
    ///   The support URL.
    /// </param>
    /// <param name="APhone">
    ///   The support phone number.
    /// </param>
    /// <param name="AHours">
    ///   The telephone support working hours.
    /// </param>
    constructor Create(const AIcon, AMan, AModel, AUrl, APhone, AHours: string); overload;

    /// <summary>
    ///   Constructor for creating a <c>TSupportInformationBase</c> instance
    /// </summary>
    /// <param name="ASupportInformationBase">
    ///   The support information to copy.
    /// </param>
    constructor Create(ASupportInformationBase: TSupportInformationBase); overload;

    /// <summary>
    ///   Clears the internal cached support information.
    /// </summary>
    procedure Clear(); virtual;

    /// <summary>
    ///   Deletes the company icon.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if icon was successfully deleted or <c>False</c> otherwise.
    /// </returns>
    function DeleteOEMIcon(): Boolean; virtual; abstract;

    /// <summary>
    ///   Checks if any support information currently exists.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if any exists or <c>False</c> otherwise.
    /// </returns>
    function Exists(): Boolean; virtual; abstract;

    /// <summary>
    ///   Gets the path to the support information icon.
    /// </summary>
    /// <returns>
    ///   The path.
    /// </returns>
    function GetOEMIcon(): string; virtual; abstract;

    /// <summary>
    ///   Loads the support information of the system.
    /// </summary>
    procedure Load(); virtual; abstract;

    /// <summary>
    ///   Loads the support information from an external .ini file.
    /// </summary>
    /// <param name="AFilename">
    ///   Absolute filename to an .ini file.
    /// </param>
    procedure LoadFromIni(const AFilename: string);

    /// <summary>
    ///   Removes the support information.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if removing was successful or <c>False</c> otherwise.
    /// </returns>
    function Remove(): Boolean; virtual; abstract;

    /// <summary>
    ///   Saves the current support information.
    /// </summary>
    procedure Save(); virtual; abstract;

    /// <summary>
    ///   Saves the current support information to an external .ini file.
    /// </summary>
    /// <param name="AFilename">
    ///   Absolute filename to an .ini file.
    /// </param>
    procedure SaveAsIni(const AFilename: string);

    /// <summary>
    ///   Shows the current support information.
    /// </summary>
    procedure Show(); virtual; abstract;

    /// <summary>
    ///   Gets or sets the telephone support working hours.
    /// </summary>
    property Hours: string read FHours write FHours;

    /// <summary>
    ///   Gets or sets the icon.
    /// </summary>
    property Icon: string read FIcon write FIcon;

    /// <summary>
    ///   Gets or sets the manufacturer.
    /// </summary>
    property Manufacturer: string read FMan write FMan;

    /// <summary>
    ///   Gets or sets the model.
    /// </summary>
    property Model: string read FModel write FModel;

    /// <summary>
    ///   Gets or sets the support URL.
    /// </summary>
    property Url: string read FUrl write FUrl;

    /// <summary>
    ///   Gets or sets the support phone number.
    /// </summary>
    property Phone: string read FPhone write FPhone;
  end;

  /// <summary>
  ///   <c>TSupportInformation</c> manages support information on Windows Vista
  ///   and later.
  /// </summary>
  TSupportInformation = class(TSupportInformationBase)
  public
    /// <summary>
    ///   Deletes the company icon.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if icon was successfully deleted or <c>False</c> otherwise.
    /// </returns>
    function DeleteOEMIcon(): Boolean; override;

    /// <summary>
    ///   Checks if any support information currently exists.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if any exists or <c>False</c> otherwise.
    /// </returns>
    function Exists(): Boolean; override;

    /// <summary>
    ///   Gets the path to the support information icon.
    /// </summary>
    /// <returns>
    ///   The path.
    /// </returns>
    function GetOEMIcon(): string; override;

    /// <summary>
    ///   Loads the support information of the system.
    /// </summary>
    procedure Load(); override;

    /// <summary>
    ///   Loads the support information from an external .reg file.
    /// </summary>
    /// <param name="AFilename">
    ///   Absolute filename to an .reg file.
    /// </param>
    procedure LoadFromReg(const AFilename: string);

    /// <summary>
    ///   Removes the support information.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if removing was successful or <c>False</c> otherwise.
    /// </returns>
    function Remove(): Boolean; override;

    /// <summary>
    ///   Saves the current support information.
    /// </summary>
    /// <exception cref="ERegistryException">
    ///   if opening key failed.
    /// </exception>
    procedure Save(); override;

    /// <summary>
    ///   Saves the current support information to an external .reg file.
    /// </summary>
    /// <param name="AFilename">
    ///   Absolute filename to an .reg file.
    /// </param>
    procedure SaveAsReg(const AFilename: string);

    /// <summary>
    ///   Shows the current support information.
    /// </summary>
    procedure Show(); override;
  end;

  /// <summary>
  ///   <c>TSupportInformationXP</c> manages support information prior to
  ///   Windows XP.
  /// </summary>
  TSupportInformationXP = class(TSupportInformationBase)
  private
    function GetOEMInfo(): string;
  public
    /// <summary>
    ///   Clears the internal cached support information.
    /// </summary>
    procedure Clear(); override;

    /// <summary>
    ///   Deletes the company icon.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if icon was successfully deleted or <c>False</c> otherwise.
    /// </returns>
    function DeleteOEMIcon(): Boolean; override;

    /// <summary>
    ///   Checks if any support information currently exists.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if any exists or <c>False</c> otherwise.
    /// </returns>
    function Exists(): Boolean; override;

    /// <summary>
    ///   Gets the path to the support information icon.
    /// </summary>
    /// <returns>
    ///   The path.
    /// </returns>
    function GetOEMIcon(): string; override;

    /// <summary>
    ///   Loads the support information of the system.
    /// </summary>
    procedure Load(); override;

    /// <summary>
    ///   Removes the support information.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if removing was successful or <c>False</c> otherwise.
    /// </returns>
    function Remove(): Boolean; override;

    /// <summary>
    ///   Saves the current support information.
    /// </summary>
    procedure Save(); override;

    /// <summary>
    ///   Shows the current support information.
    /// </summary>
    procedure Show(); override;
  end;  //deprecated 'Since Windows Vista';

implementation

{ TSupportInformationBase }

constructor TSupportInformationBase.Create(const AIcon, AMan, AModel, AUrl,
  APhone, AHours: string);
begin
  inherited Create;
  FIcon := AIcon;
  FMan := AMan;
  FModel := AModel;
  FUrl := AUrl;
  FPhone := APhone;
  FHours := AHours;
end;

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

procedure TSupportInformationBase.Clear();
begin
  FHours := '';
  FIcon := '';
  FMan := '';
  FModel := '';
  FPhone := '';
  FUrl := '';
end;

procedure TSupportInformationBase.LoadFromIni(const AFilename: string);
var
  Ini: TIniFile;

begin
  Ini := TIniFile.Create(AFileName);

  try
    with Ini do
    begin
      FIcon := ReadString(INFO_ICON, INFO_ICON);
      FMan := ReadString(INI_GENERAL, INFO_MAN);
      FModel := ReadString(INI_GENERAL, INFO_MODEL);
      FUrl := ReadString(INI_GENERAL, INFO_URL);
      FPhone := ReadString(INI_SUPPORT_INFO, INFO_PHONE);
      FHours := ReadString(INI_SUPPORT_INFO, INFO_HOURS);
    end;  //of with

  finally
    Ini.Free;
  end;  //of try
end;

procedure TSupportInformationBase.SaveAsIni(const AFilename: string);
var
  Ini: TIniFile;

begin
  Ini := TIniFile.Create(ChangeFileExt(AFilename, '.ini'), True);

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

function TSupportInformation.DeleteOEMIcon(): Boolean;
var
  Reg: TRegistry;
  Icon: string;

begin
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_WRITE or KEY_READ);

  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    if Reg.OpenKey(KEY_OEMINFO, False) then
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

function TSupportInformation.Exists(): Boolean;
var
  Reg: TRegistry;

begin
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ);

  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKey(KEY_OEMINFO, False);
    Result := (Reg.ValueExists(INFO_ICON) or Reg.ValueExists(INFO_MAN) or
               Reg.ValueExists(INFO_MODEL) or Reg.ValueExists(INFO_PHONE) or
               Reg.ValueExists(INFO_HOURS) or Reg.ValueExists(INFO_URL));

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

function TSupportInformation.GetOEMIcon(): string;
var
  Reg: TRegistry;

begin
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_READ);

  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKey(KEY_OEMINFO, False);

    if Reg.ValueExists(INFO_ICON) then
      Result := Reg.ReadString(INFO_ICON)
    else
      Result := '';

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

procedure TSupportInformation.Load();
var
  Reg: TRegistry;

  function ReadValue(const AValueName: string): string;
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
    Reg.OpenKey(KEY_OEMINFO, False);

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

procedure TSupportInformation.LoadFromReg(const AFilename: string);
var
  RegFile: TRegistryFile;
  RegSection: string;

begin
  RegFile := TRegistryFile.Create(AFilename);

  try
    RegSection := RegFile.GetSection(HKEY_LOCAL_MACHINE, KEY_OEMINFO);
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

function TSupportInformation.Remove(): Boolean;
var
  Reg: TRegistry;

begin
  Reg := TRegistry.Create(KEY_WOW64_64KEY or KEY_WRITE);

  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Result := Reg.DeleteKey(KEY_OEMINFO);

  finally
    Reg.Free;
  end;  //of try
end;

procedure TSupportInformation.Save();
var
  Reg: TRegistry;

  procedure WriteValue(const AValueName, AValue: string);
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

    if not Reg.OpenKey(KEY_OEMINFO, True) then
      raise ERegistryException.Create(Reg.LastErrorMsg);

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

procedure TSupportInformation.SaveAsReg(const AFilename: string);
var
  RegFile: TRegistryFile;
  Section: string;

begin
  RegFile := TRegistryFile.Create(ChangeFileExt(AFilename, '.reg'));

  try
    RegFile.MakeHeadline();
    Section := RegFile.GetSection(HKEY_LOCAL_MACHINE, KEY_OEMINFO);
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

procedure TSupportInformation.Show();
begin
  ShellExecute(0, nil, 'control', 'system', nil, SW_SHOWNORMAL);
end;


{ TSupportInformationXP }

function TSupportInformationXP.GetOEMInfo(): string;
begin
{$WARN SYMBOL_DEPRECATED OFF}
  Result := GetFolderPath(CSIDL_SYSTEM) +'OEMINFO.ini';
{$WARN SYMBOL_DEPRECATED ON}
end;

procedure TSupportInformationXP.Clear();
begin
  FHours := '';
  FMan := '';
  FModel := '';
  FPhone := '';
  FUrl := '';
end;

function TSupportInformationXP.DeleteOEMIcon(): Boolean;
begin
  Result := DeleteFile(FIcon);
end;
  
function TSupportInformationXP.Exists(): Boolean;
begin
  Result := FileExists(GetOemInfo());
end;

function TSupportInformationXP.GetOEMIcon(): string;
begin
{$WARN SYMBOL_DEPRECATED OFF}
  Result := GetFolderPath(CSIDL_SYSTEM) +'OEMLOGO.bmp';
{$WARN SYMBOL_DEPRECATED ON}
end;

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
    end;  //of with

  finally
    Ini.Free;
  end;  //of try

  // OEMLOGO.bmp exists?
  if FileExists(GetOEMIcon()) then
    FIcon := GetOEMIcon()
  else
    FIcon := '';
end;

function TSupportInformationXP.Remove(): Boolean;
begin
  Result := DeleteFile(GetOEMInfo());
end;

procedure TSupportInformationXP.Save();
var
  Ini: TIniFile;
  OEMIcon: string;

begin
  Ini := TIniFile.Create(GetOEMInfo());

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
    Ini.Free;
  end;  //of try

  OEMIcon := GetOEMIcon();

  // Delete current icon when new icon is empty and old still exists
  if ((FIcon = '') and FileExists(OEMIcon)) then
  begin
    if not DeleteFile(OEMIcon) then
      raise Exception.Create(SysErrorMessage(GetLastError()));

    FIcon := '';
  end  //of begin
  else
  begin
    // Copy logo only if paths differ
    if ((FIcon <> '') and (FIcon <> OEMIcon)) then
    begin
      if not CopyFile(PChar(FIcon), PChar(OEMIcon), False) then
        raise Exception.Create(SysErrorMessage(GetLastError()));

      FIcon := OEMIcon;
    end;  //of begin
  end;  //of if
end;

procedure TSupportInformationXP.Show();
begin
  ShellExecute(0, nil, 'sysdm.cpl', nil, nil, SW_SHOWNORMAL);
end;

end.
