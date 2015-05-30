{ *********************************************************************** }
{                                                                         }
{ SIT Info Pages                                                          }
{                                                                         }
{ Copyright (c) 2011-2015 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit SitInfo;

interface

uses
  SysUtils, Graphics, Controls, Forms, StdCtrls, ExtCtrls, ComCtrls,
  PMCW.OSUtils;

type
  { TInfo }
  TInfo = class(TForm)
    PageControl: TPageControl;
    ts_infos: TTabSheet;
    ts_history: TTabSheet;
    mInfos: TMemo;
    mHistory: TMemo;
    bOk: TButton;
    lCopy2: TLabel;
    bOk2: TButton;
    iLogo: TImage;
    lVersion: TLabel;
    lCopy: TLabel;
    lBuild: TLabel;
    procedure FormCreate(Sender: TObject);
  end;

implementation

{$R *.dfm}

procedure TInfo.FormCreate(Sender: TObject);
begin
  lBuild.Caption := '(Build: '+ IntToStr(TOSUtils.GetBuildNumber()) +')';
end;

end.
