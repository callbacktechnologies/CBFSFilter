object FormAddHeader: TFormAddHeader
  Left = 259
  Top = 139
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Add Header'
  ClientHeight = 554
  ClientWidth = 777
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 9
    Width = 761
    Height = 33
    AutoSize = False
    Caption = 
      'The Add Header demo shows how additional application specific da' +
      'ta may be stored with file data on disk. Users accessing the fil' +
      'e will not see the additional data. The data is automatically ad' +
      'ded when written to disk and stripped from the data that the use' +
      'r sees. '
    WordWrap = True
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 40
    Width = 761
    Height = 57
    Caption = 'Driver Management'
    TabOrder = 0
    object lblDrvStatus: TLabel
      Left = 199
      Top = 29
      Width = 64
      Height = 13
      Caption = 'Driver Status:'
    end
    object btnInstall: TButton
      Left = 16
      Top = 24
      Width = 85
      Height = 25
      Caption = 'Install driver'
      TabOrder = 0
      OnClick = btnInstallClick
    end
    object btnUninstall: TButton
      Left = 108
      Top = 24
      Width = 85
      Height = 25
      Caption = 'Uninstall drvier'
      TabOrder = 1
      OnClick = btnUninstallClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 103
    Width = 761
    Height = 442
    Caption = 'Filter Settings'
    TabOrder = 1
    object lblPath: TLabel
      Left = 16
      Top = 24
      Width = 74
      Height = 13
      Caption = 'Monitoring Path'
    end
    object btnSetFilter: TButton
      Left = 16
      Top = 48
      Width = 85
      Height = 25
      Caption = 'Set Filter'
      TabOrder = 1
      OnClick = btnSetFilterClick
    end
    object memLog: TMemo
      Left = 16
      Top = 79
      Width = 729
      Height = 329
      ScrollBars = ssBoth
      TabOrder = 4
    end
    object btnDeleteFilter: TButton
      Left = 107
      Top = 48
      Width = 85
      Height = 25
      Caption = 'Delete Filter'
      TabOrder = 2
      OnClick = btnDeleteFilterClick
    end
    object edtPath: TEdit
      Left = 108
      Top = 21
      Width = 637
      Height = 21
      TabOrder = 0
      Text = 'C:\Crypt'
    end
    object ButtonClearLog: TButton
      Left = 660
      Top = 48
      Width = 85
      Height = 25
      Caption = 'Clear Log'
      TabOrder = 3
      OnClick = btnClearLog
    end
    object CheckBoxCached: TCheckBox
      Left = 119
      Top = 414
      Width = 176
      Height = 17
      Caption = 'Process Cached Requests'
      Checked = True
      State = cbChecked
      TabOrder = 6
      OnClick = CheckBoxCachedClick
    end
    object CheckBoxLogging: TCheckBox
      Left = 16
      Top = 414
      Width = 97
      Height = 17
      Caption = 'Log Enabled'
      Checked = True
      State = cbChecked
      TabOrder = 5
      OnClick = CheckBoxLoggingClick
    end
  end
  object dlgOpen: TOpenDialog
    FileName = 'cbfilter.cab'
    Filter = 'CBFS Filter driver package (cbfilter.cab)|cbfilter.cab'
    Title = 'Select CBFS Filter driver package'
    Left = 664
    Top = 24
  end
  object tmLog: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tmLogTimer
    Left = 696
    Top = 24
  end
end


