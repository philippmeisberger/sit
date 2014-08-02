{ *********************************************************************** }
{                                                                         }
{ SIT Info Pages                                                          }
{                                                                         }
{ Copyright (c) 2011-2013 P.Meisberger (PM Code Works)                    }
{                                                                         }
{ *********************************************************************** }

unit SitInfo;

interface

uses
  Windows, Classes, Graphics, Controls, Forms, StdCtrls, ExtCtrls, SitAPI,
  ComCtrls, SysUtils;

type
  TForm2 = class(TForm)
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
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses SitMain;

{$R *.dfm}


procedure TForm2.FormCreate(Sender: TObject);
begin
  lBuild.Caption := '(Build: '+ IntToStr(TSit.GetBuildNumber) +')';
end;


procedure TForm2.bOkClick(Sender: TObject);
begin
  Close;
end;


procedure TForm2.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Hide;
  Form1.Enabled := true;
  Form1.BringToFront;
  CanClose := true;
end;

end.
