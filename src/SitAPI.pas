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
  System.IniFiles, PMCW.RegistryFile, PMCW.SysUtils;

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
  protected
    const
      /// <summary>
      ///   INI file general section.
      /// </summary>
      SectionGeneral            = 'General';

      /// <summary>
      ///   INI file support information section.
      /// </summary>
      SectionSupport            = 'Support Information';

      /// <summary>
      ///   Canonical name for manufacturer icon.
      /// </summary>
      IconCanonicalName         = 'Logo';

      /// <summary>
      ///   Canonical name for manufacturer name.
      /// </summary>
      ManufacturerCanonicalName = 'Manufacturer';

      /// <summary>
      ///   Canonical name for product model.
      /// </summary>
      ModelCanonicalName        = 'Model';

      /// <summary>
      ///   Canonical name for phone nummer.
      /// </summary>
      PhoneCanonicalName        = 'SupportPhone';

      /// <summary>
      ///   Canonical name for support phone hours.
      /// </summary>
      HoursCanonicalName        = 'SupportHours';

      /// <summary>
      ///   Canonical name for manufacturer website.
      /// </summary>
      UrlCanonicalName          = 'SupportURL';

    /// <summary>
    ///   Adds or removes a value from an INI file.
    /// </summary>
    /// <param name="AIniFile">
    ///   The INI file.
    /// </param>
    /// <param name="ASection">
    ///   The section.
    /// </param>
    /// <param name="AIdent">
    ///   The identifier.
    /// </param>
    /// <param name="AValue">
    ///   The value. If empty the identifier will be deleted.
    /// </param>
    procedure AddRemove(AIniFile: TCustomIniFile; const ASection, AIdent, AValue: string);
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
    const
      /// <summary>
      ///  OEM information Registry location since Windows Vista.
      /// </summary>
      OemInfoKey = 'SOFTWARE\Microsoft\Windows\CurrentVersion\OEMInformation';

      /// <summary>
      ///  Full OEM information Registry location since Windows Vista.
      /// </summary>
      SectionOem = 'HKEY_LOCAL_MACHINE\'+ OemInfoKey;

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

procedure TSupportInformationBase.AddRemove(AIniFile: TCustomIniFile;
  const ASection, AIdent, AValue: string);
begin
  if (AValue = '') then
    AIniFile.DeleteKey(ASection, AIdent)
  else
    AIniFile.WriteString(ASection, AIdent, AValue);
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
      FIcon := ReadString(IconCanonicalName, IconCanonicalName, '');
      FMan := ReadString(SectionGeneral, ManufacturerCanonicalName, '');
      FModel := ReadString(SectionGeneral, ModelCanonicalName, '');
      FUrl := ReadString(SectionGeneral, UrlCanonicalName, '');
      FPhone := ReadString(SectionSupport, PhoneCanonicalName, '');
      FHours := ReadString(SectionSupport, HoursCanonicalName, '');
    end;  //of with

  finally
    Ini.Free;
  end;  //of try
end;

procedure TSupportInformationBase.SaveAsIni(const AFilename: string);
var
  Ini: TIniFile;

begin
  Ini := TIniFile.Create(ChangeFileExt(AFilename, '.ini'));

  try
    AddRemove(Ini, IconCanonicalName, IconCanonicalName, FIcon);
    AddRemove(Ini, SectionGeneral, ManufacturerCanonicalName, FMan);
    AddRemove(Ini, SectionGeneral, ModelCanonicalName, FModel);
    AddRemove(Ini, SectionGeneral, UrlCanonicalName, FUrl);
    AddRemove(Ini, SectionSupport, PhoneCanonicalName, FPhone);
    AddRemove(Ini, SectionSupport, HoursCanonicalName, FHours);

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

    if Reg.OpenKey(OemInfoKey, False) then
    begin
      // Remove icon file and registry value
      Icon := Reg.ReadString(IconCanonicalName);
      Result := DeleteFile(Icon) and Reg.DeleteValue(IconCanonicalName);
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
    Reg.OpenKey(OemInfoKey, False);
    Result := (Reg.ValueExists(IconCanonicalName) or Reg.ValueExists(ManufacturerCanonicalName) or
               Reg.ValueExists(ModelCanonicalName) or Reg.ValueExists(PhoneCanonicalName) or
               Reg.ValueExists(HoursCanonicalName) or Reg.ValueExists(UrlCanonicalName));

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
    Reg.OpenKey(OemInfoKey, False);

    if Reg.ValueExists(IconCanonicalName) then
      Result := Reg.ReadString(IconCanonicalName)
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
    Reg.OpenKey(OemInfoKey, False);

    // Read all OEM information
    FIcon := ReadValue(IconCanonicalName);
    FHours := ReadValue(HoursCanonicalName);
    FMan := ReadValue(ManufacturerCanonicalName);
    FModel := ReadValue(ModelCanonicalName);
    FPhone := ReadValue(PhoneCanonicalName);
    FUrl := ReadValue(UrlCanonicalName);

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

procedure TSupportInformation.LoadFromReg(const AFilename: string);
var
  RegFile: TRegistryFile;

begin
  RegFile := TRegistryFile.Create(AFilename);

  try
    FIcon := RegFile.ReadString(SectionOem, IconCanonicalName, '');
    FMan := RegFile.ReadString(SectionOem, ManufacturerCanonicalName, '');
    FModel := RegFile.ReadString(SectionOem, ModelCanonicalName, '');
    FPhone := RegFile.ReadString(SectionOem, PhoneCanonicalName, '');
    FHours := RegFile.ReadString(SectionOem, HoursCanonicalName, '');
    FUrl := RegFile.ReadString(SectionOem, UrlCanonicalName, '');

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
    Result := Reg.DeleteKey(OemInfoKey);

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

    if not Reg.OpenKey(OemInfoKey, True) then
      raise ERegistryException.Create(Reg.LastErrorMsg);

    WriteValue(HoursCanonicalName, FHours);
    WriteValue(IconCanonicalName, FIcon);
    WriteValue(ManufacturerCanonicalName, FMan);
    WriteValue(ModelCanonicalName, FModel);
    WriteValue(PhoneCanonicalName, FPhone);
    WriteValue(UrlCanonicalName, FUrl);

  finally
    Reg.CloseKey();
    Reg.Free;
  end;  //of try
end;

procedure TSupportInformation.SaveAsReg(const AFilename: string);
var
  RegFile: TRegistryFile;

begin
  RegFile := TRegistryFile.Create(ChangeFileExt(AFilename, '.reg'));

  try
    AddRemove(RegFile, SectionOem, IconCanonicalName, FIcon);
    AddRemove(RegFile, SectionOem, ManufacturerCanonicalName, FMan);
    AddRemove(RegFile, SectionOem, ModelCanonicalName, FModel);
    AddRemove(RegFile, SectionOem, PhoneCanonicalName, FPhone);
    AddRemove(RegFile, SectionOem, HoursCanonicalName, FHours);
    AddRemove(RegFile, SectionOem, UrlCanonicalName, FUrl);

    // Save file
    RegFile.UpdateFile();

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
      FMan := ReadString(SectionGeneral, ManufacturerCanonicalName, '');
      FModel := ReadString(SectionGeneral, ModelCanonicalName, '');
      FUrl := ReadString(SectionGeneral, UrlCanonicalName, '');
      FPhone := ReadString(SectionSupport, 'Line3', '');
      FHours := ReadString(SectionSupport, 'Line4', '');
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
      WriteString(SectionGeneral, ManufacturerCanonicalName, FMan);
      WriteString(SectionGeneral, ModelCanonicalName, FModel);
      WriteString(SectionGeneral, UrlCanonicalName, FUrl);
      WriteString(SectionSupport, 'Line1', FMan);
      WriteString(SectionSupport, 'Line2', FModel);
      WriteString(SectionSupport, 'Line3', FPhone);
      WriteString(SectionSupport, 'Line4', FHours);
      WriteString(SectionSupport, 'Line5', FUrl);
    end;  //of with

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
