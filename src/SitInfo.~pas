{ *********************************************************************** }
{                                                                         }
{ SIT Info Pages                                                          }
{                                                                         }
{ Copyright (c) 2011-2014 P.Meisberger (PM Code Works)                    }
{                                                                         }
{ *********************************************************************** }

unit SitInfo;

interface

uses
  Graphics, Controls, Forms, StdCtrls, ExtCtrls, ComCtrls, OSUtils, SysUtils,
  Classes;

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
    procedure bOkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.dfm}

procedure TInfo.FormCreate(Sender: TObject);
begin
  lBuild.Caption := '(Build: '+ IntToStr(TOSUtils.GetBuildNumber) +')';
end;


procedure TInfo.bOkClick(Sender: TObject);
begin
  Close;
end;

end.
