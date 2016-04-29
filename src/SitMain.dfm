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
  Visible = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    400
    278)
  PixelsPerInch = 96
  TextHeight = 14
  object lCopy: TLabel
    Left = 144
    Top = 255
    Width = 112
    Height = 14
    Anchors = [akBottom]
    Caption = #169' P.Meisberger 2016'
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
  object lVersion: TLabel
    Left = 372
    Top = 6
    Width = 21
    Height = 14
    Anchors = [akTop, akRight]
    Caption = 'v3.2'
    Transparent = True
  end
  object bAccept: TButton
    Left = 8
    Top = 240
    Width = 105
    Height = 32
    Anchors = [akLeft, akBottom]
    Caption = #220'bernehmen'
    Default = True
    ElevationRequired = True
    TabOrder = 0
    OnClick = bAcceptClick
  end
  object bShowSupport: TButton
    Left = 288
    Top = 240
    Width = 105
    Height = 32
    Anchors = [akRight, akBottom]
    Caption = 'Anzeigen'
    TabOrder = 3
    OnClick = bShowSupportClick
  end
  object gbInfo: TGroupBox
    Left = 8
    Top = 110
    Width = 385
    Height = 121
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Informationen'
    TabOrder = 2
    object eHours: TLabeledEdit
      Left = 293
      Top = 35
      Width = 76
      Height = 22
      EditLabel.Width = 37
      EditLabel.Height = 14
      EditLabel.Caption = 'Uhrzeit:'
      EditLabel.Transparent = True
      TabOrder = 3
    end
    object eModel: TLabeledEdit
      Left = 18
      Top = 83
      Width = 119
      Height = 22
      EditLabel.Width = 33
      EditLabel.Height = 14
      EditLabel.Caption = 'Modell:'
      EditLabel.Transparent = True
      TabOrder = 1
    end
    object ePhone: TLabeledEdit
      Left = 152
      Top = 35
      Width = 125
      Height = 22
      EditLabel.Width = 76
      EditLabel.Height = 14
      EditLabel.Caption = 'Telefonnummer:'
      EditLabel.Transparent = True
      TabOrder = 2
    end
    object eMan: TLabeledEdit
      Left = 18
      Top = 35
      Width = 119
      Height = 22
      EditLabel.Width = 49
      EditLabel.Height = 14
      EditLabel.Caption = 'Hersteller:'
      EditLabel.Transparent = True
      TabOrder = 0
    end
    object eUrl: TLabeledEdit
      Left = 152
      Top = 83
      Width = 217
      Height = 22
      EditLabel.Width = 79
      EditLabel.Height = 14
      EditLabel.Caption = 'Internetadresse:'
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
      Caption = 'Logo kopieren'
      Enabled = False
      TabOrder = 2
    end
    object eLogo: TLabeledEdit
      Left = 18
      Top = 36
      Width = 316
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      EditLabel.Width = 52
      EditLabel.Height = 14
      EditLabel.Caption = 'Logo Pfad:'
      EditLabel.Transparent = True
      TabOrder = 0
      OnDblClick = eLogoDblClick
    end
  end
  object MainMenu: TMainMenu
    Left = 336
    object mmFile: TMenuItem
      Caption = 'Datei'
      object mmImport: TMenuItem
        Caption = 'Importieren'
        ShortCut = 112
        OnClick = mmImportClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mmExport: TMenuItem
        Caption = 'Eintr'#228'ge exportieren'
        ShortCut = 113
        OnClick = mmExportClick
      end
      object mmExportEdit: TMenuItem
        Caption = 'Eingaben exportieren'
        ShortCut = 114
        OnClick = mmExportEditClick
      end
    end
    object mmEdit: TMenuItem
      Caption = 'Bearbeiten'
      object mmDeleteValues: TMenuItem
        Caption = 'Eintr'#228'ge l'#246'schen'
        ShortCut = 117
        OnClick = mmDeleteValuesClick
      end
      object mmDeleteEdits: TMenuItem
        Caption = 'Eingaben l'#246'schen'
        ShortCut = 118
        OnClick = mmDeleteEditsClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object mmCopyIcon: TMenuItem
        Caption = 'Logo kopieren'
        ShortCut = 119
        OnClick = mmCopyIconClick
      end
      object mmDeleteIcon: TMenuItem
        Caption = 'Logo l'#246'schen'
        ShortCut = 120
        OnClick = mmDeleteIconClick
      end
    end
    object mmView: TMenuItem
      Caption = 'Ansicht'
      object mmShow: TMenuItem
        Caption = 'Eintr'#228'ge anzeigen'
        ShortCut = 116
        OnClick = mmShowClick
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object mmLang: TMenuItem
        Caption = 'Sprache w'#228'hlen'
      end
    end
    object mmHelp: TMenuItem
      Caption = 'Hilfe'
      object mmUpdate: TMenuItem
        Caption = 'Nach Update suchen'
        OnClick = mmUpdateClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object mmInstallCertificate: TMenuItem
        Caption = 'Zertifikat installieren'
        OnClick = mmInstallCertificateClick
      end
      object mmReport: TMenuItem
        Caption = 'Fehler melden'
        OnClick = mmReportClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object mmAbout: TMenuItem
        Caption = #220'ber SIT'
        OnClick = mmAboutClick
      end
    end
  end
end
