{ *********************************************************************** }
{                                                                         }
{ SIT API Interface Unit                                                  }
{                                                                         }
{ Copyright (c) 2011-2012 P.Meisberger (PM Code Works)                    }
{                                                                         }
{ *********************************************************************** }

unit SitAPI;

interface

uses
  Windows, Classes, SysUtils, Registry, TLHelp32, IniFiles;

const
  OEMINFO_KEY = 'SOFTWARE\Microsoft\Windows\CurrentVersion\OEMInformation';
  SUPP_SEC = 'Support Information';
  LOGO_DIR = '\System32\OEMLOGO.bmp';
  INFO_DIR = '\System32\OEMINFO.ini';
  URL_BASE = 'http://www.pm-codeworks.de/';
  URL_DIR = URL_BASE +'media/';
  URL_CONTACT = URL_BASE +'kontakt.html';

type
  { Exception class }
  ESitError = class(Exception);

  { Base class }
  TSit = class
  private
    FLangID: Integer;
    function ChangeFSRedirection(AAccess: Boolean): Boolean;
    function DelIniValue(AFileName, ASection, AValueName: string): Boolean;
    function DelRegValue(AValueName: string): Boolean;
    function ExtendHKey(AMainKey: string): HKEY;
    function GetKeyValue(AMainKey, AKeyPath, AValueName: string): string;
    function GetNativeSystemInfo(var ASystemInfo: TSystemInfo): Boolean; //not by me
    function GetWinDir: string;
    function IsWindows64: Boolean;  //not by me
    function SetIniValue(AFileName, ASection, AValueName, AValue: string): Boolean;
    function SetKeyAccessMode: Cardinal;
    function SetRegValue(AValueName, AValue: string): Boolean;
  public
    constructor Create(ALangID: integer = 100);
    function AccessIniData(AFileName, ASection, AValueName, AValue: string): Boolean;
    function AccessRegData(AValueName, AValue: string): Boolean;
    function CheckWindows: Boolean;
    function CreateError(AMsgTypeID, AContentID: Integer; ATitleID: Integer = 42): Integer; overload;
    function CreateError(AMsgType: string; AContentID: Integer; ATitleID: Integer = 42): Integer; overload;
    function DelGooseFoots(AName: string): string;
    function DataExists: Boolean;
    class function GetBuildNumber: integer;                     //not by me
    function GetIniValue(AFileName, ASection, AValueName: string): string;
    function GetOemInfoDir: string;
    function GetOemLogoDir: string;
    function GetRegValue(AValueName: string): string;
    function GetString(const AIndex: integer): string;
    function GetWinVersion: string;
    function RegDelete: Boolean;
    function SetCaption: string;

    property LangID: Integer read FLangID write FLangID;
  end;

  { Data Access class }
  TSitData = class(TSit)
  private
    FLogo, FMan, FModel, FUrl, FPhone, FHours: string;
  public
    constructor Create(ALogo, AMan, AModel, AUrl, APhone, AHours: string);
    procedure SaveAsIni(AFilename: string; ADirect: Bool);
    procedure SaveAsReg(AFilename: string; ADirect: Bool);
    procedure SetIniData;
    procedure SetRegData;
  end;


implementation

uses SITMain;

constructor TSit.Create(ALangID: integer = 100);
begin
  inherited Create;
  FLangID := ALangID;
end;

{ private }
function TSit.ChangeFSRedirection(AAccess: Boolean): Boolean;  //32Bit Zugriffe auf
type                                                           //64Bit Systemen emulieren
  TWow64DisableWow64FsRedirection = function(var Wow64FsEnableRedirection: LongBool): LongBool; stdCall;
  TWow64EnableWow64FsRedirection = function(var Wow64FsEnableRedirection: LongBool): LongBool; stdCall;

var
  hHandle: THandle;
  Wow64DisableWow64FsRedirection: TWow64DisableWow64FsRedirection;
  Wow64EnableWow64FsRedirection: TWow64EnableWow64FsRedirection;
  Wow64FsEnableRedirection: LongBool;

begin
  result := true;

  if not IsWindows64 then           //kein 64-Bit-Win?
     Exit;

    try
      hHandle := GetModuleHandle('kernel32.dll');
      @Wow64EnableWow64FsRedirection := GetProcAddress(hHandle, 'Wow64EnableWow64FsRedirection');
      @Wow64DisableWow64FsRedirection := GetProcAddress(hHandle, 'Wow64DisableWow64FsRedirection');

      if (AAccess = true) then
         begin
         if ((hHandle <> 0) and (@Wow64EnableWow64FsRedirection <> nil) and
            (@Wow64DisableWow64FsRedirection <> nil)) then
            Wow64DisableWow64FsRedirection(Wow64FsEnableRedirection);
         end
      else
        if ((hHandle <> 0) and (@Wow64EnableWow64FsRedirection <> nil) and
           (@Wow64DisableWow64FsRedirection <> nil)) then
           Wow64EnableWow64FsRedirection(Wow64FsEnableRedirection);

    except
      result := false;
    end;
end;


function TSit.DelIniValue(AFileName, ASection, AValueName: string): Boolean;
var
  ini: TIniFile;
  list: TStringList;

begin
list := TStringList.Create;                     //init temp Liste
ini := TIniFile.Create(AFileName);              //init Ini Datei

  try
    if (ini.SectionExists(ASection) and ini.ValueExists(ASection, AValueName)) then  //Existiert Sektion und Wert
       begin
       ini.DeleteKey(ASection, AValueName);     //Eintrag löschen
       ini.ReadSection(ASection, list);

       if (list.Count = 0) then
          ini.EraseSection(ASection);           //Sektion löschen, falls leer
       end;  //of begin
    
    result := true;

  except                                        //im Fehlerfall
    result := false;
  end;  //of except

list.Free;                                      //freigeben
ini.Free;
end;


function TSit.DelRegValue(AValueName: string): Boolean;
var
  reg: TRegistry;

begin
reg := TRegistry.Create(SetKeyAccessMode);        //init von reg mit setzen der Rechte

  try
    reg.RootKey := HKEY_LOCAL_MACHINE;            //HKEY öffnen
    reg.OpenKey(OEMINFO_KEY, false);              //SubKey öffnen
    result := reg.DeleteValue(AValueName);        //Wert löschen

  finally                                         //freigeben
    reg.Free;
  end; //of finally
end;


function TSit.ExtendHKey(AMainKey: string): HKEY;   //wandelt HKEY-Kurzschreibweise
begin                                               //in Langfassung vom Typ HKEY
if (AMainKey = 'HKCU') then
   result := HKEY_CURRENT_USER
else
   if ((AMainKey = 'HKLM') or (AMainKey = 'Startup User') or (AMainKey = 'Startup Common')
      or (AMainKey = 'Startup')) then
      result := HKEY_LOCAL_MACHINE
   else
      if (AMainKey = 'HKCR') then
         result := HKEY_CLASSES_ROOT
      else
         result := 0;      //Fehlererkennung
end;


function TSit.GetKeyValue(AMainKey, AKeyPath, AValueName: string): string;
var
  reg: TRegistry;

begin
reg := TRegistry.Create(SetKeyAccessMode);     //init von reg mit setzen der Rechte

  try
    reg.RootKey := ExtendHKey(AMainKey);       //HKEY öffnen
    reg.OpenKey(AKeyPath, true);               //SubKey öffnen

    if (reg.ValueExists(AValueName)) then      //existiert Wert?
       result := reg.ReadString(AValueName)    //...dann Wert auslesen
    else
       result := '';                           //sonst Fehler vermeiden

  finally                                      //freigeben
    reg.CloseKey;
    reg.Free;
  end; //of finally
end;


function TSit.GetNativeSystemInfo(var ASystemInfo: TSystemInfo): Boolean;  //not by me
type
  TGetNativeSystemInfo = procedure (var ASystemInfo: TSystemInfo) stdcall;
var
  LibraryHandle: HMODULE;
  GetNativeSystemInfo: TGetNativeSystemInfo;

begin
result := false;
LibraryHandle := GetModuleHandle(kernel32);

if (LibraryHandle <> 0) then
   begin
   GetNativeSystemInfo := GetProcAddress(LibraryHandle,'GetNativeSystemInfo');

   if Assigned(GetNativeSystemInfo) then
      begin
      GetNativeSystemInfo(ASystemInfo);
      result := true;
      end //of begin
   else
      GetSystemInfo(ASystemInfo);
   end //of begin
else
   GetSystemInfo(ASystemInfo);
end;


function TSit.GetWinDir: string;
begin
  result := SysUtils.GetEnvironmentVariable('windir');
end;


function TSit.IsWindows64: Boolean;                      //32 oder 64Bit Windows
var
  ASystemInfo: TSystemInfo;

const
  PROCESSOR_ARCHITECTURE_INTEL = 0;
  PROCESSOR_ARCHITECTURE_IA64 = 6;
  PROCESSOR_ARCHITECTURE_AMD64 = 9;

begin
  GetNativeSystemInfo(ASystemInfo);
  result := ASystemInfo.wProcessorArchitecture in [PROCESSOR_ARCHITECTURE_IA64,PROCESSOR_ARCHITECTURE_AMD64];
end;


function TSit.SetIniValue(AFileName, ASection, AValueName, AValue: string): Boolean;
var
  ini: TIniFile;

begin
  try
    ini := TIniFile.Create(AFileName);                 //init Ini Datei
    ini.WriteString(ASection, AValueName, AValue);     //Name + Wert übernehmen
    ini.Free;                                          //freigeben
    result := true;

  except
    result := false;                                   //im Fehlerfall
  end;  //of except
end;


function TSit.SetKeyAccessMode: Cardinal;        //Key-Access für Schreibzugriff
const                                            //auf Registry
  KEY_WOW64_64KEY = $0100;
  KEY_WOW64_32KEY = $0200;

begin
  if IsWindows64 then                              //32 oder 64bit Windows?
     result := KEY_ALL_ACCESS or KEY_WOW64_64KEY
  else
     result := KEY_ALL_ACCESS or KEY_WOW64_32KEY;
end;


function TSit.SetRegValue(AValueName, AValue: string): Boolean;
var
  reg: TRegistry;

begin
reg := TRegistry.Create(SetKeyAccessMode);      //init von reg mit setzen der Rechte

  try
    reg.RootKey := HKEY_LOCAL_MACHINE;          //HKEY öffnen
    reg.OpenKey(OEMINFO_KEY, true);             //SubKey öffnen
    reg.WriteString(AValueName, AValue);        //Name + Wert übernehmen
    reg.Free;                                   //freigeben
    result := true;

  except                                        
    result := false;                            //im Fehlerfall
  end;  //of except
end;
{ of private }

{ public }
function TSit.AccessIniData(AFileName, ASection, AValueName, AValue: string): Boolean;
begin
  if (AValue <> '') then
     result := SetIniValue(AFileName, ASection, AValueName, AValue)
  else
     result := DelIniValue(AFileName, ASection, AValueName);
end;


function TSit.AccessRegData(AValueName, AValue: string): Boolean;
begin
  if (AValue <> '') then
     result := SetRegValue(AValueName, AValue)
  else
     result := DelRegValue(AValueName);
end;


function TSit.CheckWindows: Boolean;           //Prüfen, ob Win Vista oder Win 7
begin
  if ((GetWinVersion[1] = '7') or (GetWinVersion[1] = 'V') or
     (GetWinVersion[1] = '8')) then
     result := true
  else
     result := false;
end;


function TSit.CreateError(AMsgTypeID, AContentID: Integer; ATitleID: Integer = 42): Integer;
begin
  result := Form1.MessageBox(GetString(AMsgTypeID) + GetString(48) +^J+ GetString(49)
                            + GetString(AContentID), ATitleID, MB_ICONERROR);
end;


function TSit.CreateError(AMsgType: string; AContentID: Integer; ATitleID: Integer = 42): Integer;
begin
  result := Form1.MessageBox(AMsgType + GetString(48) +^J+ GetString(49)
                            + GetString(AContentID), ATitleID, MB_ICONERROR);
end;


function TSit.DelGooseFoots(AName: string): string;
begin
  result := StringReplace(AName, '"', '', [rfReplaceAll]);
end;


class function TSit.GetBuildNumber: integer;
var
  VerInfoSize, VerValueSize, Dummy: DWord;
  VerInfo: Pointer;
  VerValue: PVSFixedFileInfo;

begin
  VerInfoSize := GetFileVersionInfoSize(PChar(ParamStr(0)), Dummy);

  if VerInfoSize <> 0 then
     begin
     GetMem(VerInfo, VerInfoSize);

       try
         GetFileVersionInfo(PChar(ParamStr(0)), 0, VerInfoSize, VerInfo);

         if VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize) then
            with VerValue^ do
              result := dwFileVersionLS and $FFFF
         else
            result := 0;

       finally
         FreeMem(VerInfo, VerInfoSize);
       end;   //of finally
     end  //of begin
  else
     result := 0;
end;


function TSit.DataExists: Boolean;               //REG Einträge oder OEMINFO.ini
var                                              //vorhanden
  reg: TRegistry;

begin

if not CheckWindows then
   result := FileExists(GetOemInfoDir) or FileExists(GetOemLogoDir)
else
   begin
   reg := TRegistry.Create(SetKeyAccessMode);      //init reg

     try
       reg.RootKey := HKEY_LOCAL_MACHINE;
       reg.OpenKey(OEMINFO_KEY, true);             //RegKey öffnen
       result := (reg.ValueExists('Logo') or reg.ValueExists('Manufacturer') or
                 reg.ValueExists('Model') or reg.ValueExists('SupportPhone') or
                 reg.ValueExists('SupportHours') or reg.ValueExists('SupportURL'));
       reg.CloseKey;

     finally
       reg.Free;
     end;  //of finally
   end;  //of if
end;


function TSit.GetIniValue(AFileName, ASection, AValueName: string): string;
var
  ini: TIniFile;

begin
ini := TIniFile.Create(AFileName);

  try
    result := ini.ReadString(ASection, AValueName, '');

  finally
    ini.Free;
  end;  //of finally
end;


function TSit.GetOemInfoDir: string;                          //OEMINFO.ini Pfad
begin
  ChangeFSRedirection(true);                 //32 auf 64Bit Zugriff emulieren
  result := GetWinDir + INFO_DIR;
  ChangeFSRedirection(false);                //32Bit Zugriff
end;


function TSit.GetOemLogoDir: string;                          //OEMLOGO.bmp Pfad
begin
  ChangeFSRedirection(true);                 //32 auf 64Bit Zugriff emulieren
  result := GetWinDir + LOGO_DIR;
  ChangeFSRedirection(false);                //32Bit Zugriff
end;


function TSit.GetRegValue(AValueName: string): string;
begin
  result := GetKeyValue('HKLM', OEMINFO_KEY, AValueName);
end;


function TSit.GetString(const AIndex: integer) : string;   //Zugriff auf Stringtable
var
  Buffer : array[0..85] of char;
  ls : integer;

begin
  result := '';
  ls := LoadString(hInstance, AIndex + FLangID, Buffer, SizeOf(Buffer));  //String laden

  if (ls <> 0) then
     result := Buffer;
end;


function TSit.GetWinVersion: string;                  //Windows-Version auslesen
begin                                                 //not by me
result := 'unbekannt';

case Win32Platform of                      // 9x-Reihe
  1: if (Win32MajorVersion = 4) then
        case Win32MinorVersion of
          0: result := '95';
         10: result := '98';
         90: result := 'Me';
        end; //of case

  2: case Win32MajorVersion of            // NT-Reihe
       3: if (Win32MinorVersion = 51) then
             result := 'NT 3.51';

       4: if (Win32MinorVersion = 0) then
             result := 'NT 4';

       5: case Win32MinorVersion of      //W2K Reihe
            0: result := '2000';
            1: result := 'XP';
            2: result := '.NET Server';
          end; //of case

       6: case Win32MinorVersion of
            0: result := 'Vista';
            1: result := '7';
            2: result := '8';
          end; //of case
     end; //of case
end; //of case
end;


function TSit.RegDelete: Boolean;                         //REG Einträge löschen
begin
  try
    DelRegValue('Logo');
    DelRegValue('Manufacturer');
    DelRegValue('Model');
    DelRegValue('SupportPhone');
    DelRegValue('SupportHours');
    DelRegValue('SupportURL');
    result := true;

  except
    result := false;
  end;  //of finally
end;


function TSit.SetCaption: string;                                  //FormCaption
begin
  if IsWindows64 then
     result := 'SIT [64bit]'      //Bits in Form Label schr.
  else
     result := 'SIT [32bit]';
end;
{ of TSit }

{##############################################################################}
{ TSitData }

constructor TSitData.Create(ALogo, AMan, AModel, AUrl, APhone, AHours: string);
begin
  FLogo := ALogo;
  FMan := AMan;
  FModel := AModel;
  FUrl := AUrl;
  FPhone := APhone;
  FHours := AHours;
  inherited Create;
end;


procedure TSitData.SaveAsIni(AFilename: string; ADirect: Bool);  //als INI-Datei speichern
begin
  try
    if ADirect then
       begin
       if CheckWindows then            //ab Windows Vista
          begin
          AccessIniData(AFilename, 'Logo', 'Logo', GetRegValue('Logo'));
          AccessIniData(AFilename, 'General', 'Manufacturer', GetRegValue('Manufacturer'));
          AccessIniData(AFilename, 'General', 'Model', GetRegValue('Model'));
          AccessIniData(AFilename, 'General', 'SupportURL', GetRegValue('SupportURL'));
          AccessIniData(AFilename, SUPP_SEC, 'SupportPhone', GetRegValue('SupportPhone'));
          AccessIniData(AFilename, SUPP_SEC, 'SupportHours', GetRegValue('SupportHours'));
          end  //of begin
       else
          if FileExists(GetOemInfoDir) then  //ab Win2000
             CopyFile(PChar(GetOemInfoDir), PChar(AFilename), false);  //ini kopieren
       end  //of begin
    else
       begin                           //aus Editfeldern
       if not CheckWindows then        //ab Win2000
          if FileExists(GetOemLogoDir) then
             AccessIniData(GetOemInfoDir, 'Logo', 'Logo', GetOemLogoDir);

       AccessIniData(AFilename, 'Logo', 'Logo', FLogo);
       AccessIniData(AFilename, 'General', 'Manufacturer', FMan);
       AccessIniData(AFilename, 'General', 'Model', FModel);
       AccessIniData(AFilename, 'General', 'SupportURL', FUrl);
       AccessIniData(AFilename, SUPP_SEC, 'SupportPhone', FPhone);
       AccessIniData(AFilename, SUPP_SEC, 'SupportHours', FHours);
       end;  //of if

  except
    CreateError(GetString(47) + ExtractFileName(AFilename), 51);
  end;  //of except
end;


procedure TSitData.SaveAsReg(AFilename: string; ADirect: Bool); //als REG Datei speichern
var
  regFile: TStringList;
  path: string;

begin
regFile := TStringList.Create;                     //init Liste
regFile.Append('Windows Registry Editor Version 5.00');
regFile.Append('');
regFile.Append('[HKEY_LOCAL_MACHINE\'+ OEMINFO_KEY +']');

  try
    if ADirect then
       begin                                       //init direkt aus Registry
       if CheckWindows then          //ab Windows Vista
          begin
          path := StringReplace(GetRegValue('Logo'), '\', '\\', [rfReplaceAll]);
          regFile.Append('"Logo"="'+ path +'"');
          regFile.Append('"Manufacturer"="'+ GetRegValue('Manufacturer') +'"');
          regFile.Append('"Model"="'+ GetRegValue('Model') +'"');
          regFile.Append('"SupportPhone"="'+ GetRegValue('SupportPhone') +'"');
          regFile.Append('"SupportHours"="'+ GetRegValue('SupportHours') +'"');
          regFile.Append('"SupportURL"="'+ GetRegValue('SupportURL') +'"');
          end  //of begin
       else
          begin                      //ab Windows 2000
          path := StringReplace(GetRegValue('Logo'), '\', '\\', [rfReplaceAll]);
          regFile.Append('"Logo"="'+ path +'"');
          regFile.Append('"Manufacturer"="'+ GetRegValue('Manufacturer') +'"');
          regFile.Append('"Model"="'+ GetRegValue('Model') +'"');
          regFile.Append('"SupportPhone"="'+ GetRegValue('SupportPhone') +'"');
          regFile.Append('"SupportHours"="'+ GetRegValue('SupportHours') +'"');
          regFile.Append('"SupportURL"="'+ GetRegValue('SupportURL') +'"');
          end;  //of begin
       end  //of begin
    else
       begin                         //aus Editfeldern
       path := StringReplace(FLogo, '\', '\\', [rfReplaceAll]);
       regFile.Append('"Logo"="'+ path +'"');
       regFile.Append('"Manufacturer"="'+ FMan +'"');
       regFile.Append('"Model"="'+ FModel +'"');
       regFile.Append('"SupportPhone"="'+ FPhone +'"');
       regFile.Append('"SupportHours"="'+ FHours +'"');
       regFile.Append('"SupportURL"="'+ FUrl +'"');
       end;  //of if

    regFile.SaveToFile(AFilename);                 //Datei speichern
                                                   //freigeben
  except
    CreateError(GetString(47) + ExtractFileName(AFilename), 51);
  end;  //of except

regFile.Free;
end;


procedure TSitData.SetIniData;
begin
  SaveAsIni(GetOemInfoDir, false);
end;


procedure TSitData.SetRegData;
begin
  AccessRegData('Logo', FLogo);
  AccessRegData('Manufacturer', FMan);
  AccessRegData('Model', FModel);
  AccessRegData('SupportPhone', FPhone);
  AccessRegData('SupportHours', FHours);
  AccessRegData('SupportURL', FUrl);
end;
{ of TSitData }

end.
