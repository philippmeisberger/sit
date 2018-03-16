object Main: TMain
  Left = 577
  Top = 243
  HorzScrollBar.Visible = False
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'SIT'
  ClientHeight = 278
  ClientWidth = 400
  Color = 1797885
  Constraints.MinHeight = 170
  Constraints.MinWidth = 362
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    400
    278)
  PixelsPerInch = 96
  TextHeight = 14
  object lCopy: TLabel
    Left = 156
    Top = 254
    Width = 88
    Height = 14
    Alignment = taCenter
    Caption = 'PM Code Works'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    Transparent = True
    OnClick = lCopyClick
    OnMouseEnter = lCopyMouseEnter
    OnMouseLeave = lCopyMouseLeave
  end
  object bApply: TButton
    Left = 8
    Top = 240
    Width = 105
    Height = 32
    Anchors = [akLeft, akBottom]
    Caption = 'Apply'
    Default = True
    ElevationRequired = True
    TabOrder = 0
    OnClick = bApplyClick
  end
  object bShowSupport: TButton
    Left = 288
    Top = 240
    Width = 105
    Height = 32
    Anchors = [akRight, akBottom]
    Caption = 'Show'
    TabOrder = 3
    OnClick = bShowSupportClick
  end
  object gbInfo: TGroupBox
    Left = 8
    Top = 110
    Width = 385
    Height = 121
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Information'
    TabOrder = 2
    object eHours: TLabeledEdit
      Left = 293
      Top = 35
      Width = 76
      Height = 22
      EditLabel.Width = 29
      EditLabel.Height = 14
      EditLabel.Caption = 'Hours'
      EditLabel.Transparent = True
      TabOrder = 3
    end
    object eModel: TLabeledEdit
      Left = 18
      Top = 83
      Width = 119
      Height = 22
      EditLabel.Width = 28
      EditLabel.Height = 14
      EditLabel.Caption = 'Model'
      EditLabel.Transparent = True
      TabOrder = 1
    end
    object ePhone: TLabeledEdit
      Left = 152
      Top = 35
      Width = 125
      Height = 22
      EditLabel.Width = 30
      EditLabel.Height = 14
      EditLabel.Caption = 'Phone'
      EditLabel.Transparent = True
      TabOrder = 2
    end
    object eMan: TLabeledEdit
      Left = 18
      Top = 35
      Width = 119
      Height = 22
      EditLabel.Width = 65
      EditLabel.Height = 14
      EditLabel.Caption = 'Manufacturer'
      EditLabel.Transparent = True
      TabOrder = 0
    end
    object eUrl: TLabeledEdit
      Left = 152
      Top = 83
      Width = 217
      Height = 22
      EditLabel.Width = 39
      EditLabel.Height = 14
      EditLabel.Caption = 'Website'
      EditLabel.Transparent = True
      TabOrder = 4
      OnDblClick = eUrlDblClick
    end
  end
  object gbIcon: TGroupBox
    Left = 8
    Top = 18
    Width = 385
    Height = 85
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Logo'
    TabOrder = 1
    DesignSize = (
      385
      85)
    object bAdd: TButton
      Left = 340
      Top = 36
      Width = 28
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '...'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = bAddClick
    end
    object cbCopyIcon: TCheckBox
      Left = 18
      Top = 60
      Width = 97
      Height = 17
      Caption = 'Copy logo'
      Enabled = False
      TabOrder = 2
    end
    object eLogo: TLabeledEdit
      Left = 18
      Top = 36
      Width = 316
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      EditLabel.Width = 48
      EditLabel.Height = 14
      EditLabel.Caption = 'Logo path'
      EditLabel.Transparent = True
      TabOrder = 0
      OnDblClick = eLogoDblClick
    end
  end
  object MainMenu: TMainMenu
    Left = 336
    object mmFile: TMenuItem
      Caption = 'File'
      object mmImport: TMenuItem
        Caption = 'Import'
        ShortCut = 112
        OnClick = mmImportClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mmExport: TMenuItem
        Caption = 'Export entries'
        ShortCut = 113
        OnClick = mmExportClick
      end
      object mmExportEdit: TMenuItem
        Caption = 'Export input'
        ShortCut = 114
        OnClick = mmExportEditClick
      end
    end
    object mmEdit: TMenuItem
      Caption = 'Edit'
      object mmDeleteValues: TMenuItem
        Caption = 'Delete entries'
        ShortCut = 117
        OnClick = mmDeleteValuesClick
      end
      object mmDeleteEdits: TMenuItem
        Caption = 'Delete input'
        ShortCut = 118
        OnClick = mmDeleteEditsClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object mmCopyIcon: TMenuItem
        Caption = 'Copy logo'
        ShortCut = 119
        OnClick = mmCopyIconClick
      end
      object mmDeleteIcon: TMenuItem
        Caption = 'Delete logo'
        ShortCut = 120
        OnClick = mmDeleteIconClick
      end
    end
    object mmView: TMenuItem
      Caption = 'View'
      object mmRefresh: TMenuItem
        Caption = 'Refresh'
        ShortCut = 116
        OnClick = mmRefreshClick
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object mmLang: TMenuItem
        Caption = 'Choose language'
      end
    end
    object mmHelp: TMenuItem
      Caption = 'Help'
      object mmUpdate: TMenuItem
        Caption = 'Search for Update'
        OnClick = mmUpdateClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object mmInstallCertificate: TMenuItem
        Caption = 'Install certificate'
        OnClick = mmInstallCertificateClick
      end
      object mmReport: TMenuItem
        Caption = 'Report a bug'
        OnClick = mmReportClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object mmAbout: TMenuItem
        Caption = 'About SIT'
        OnClick = mmAboutClick
      end
    end
  end
end
