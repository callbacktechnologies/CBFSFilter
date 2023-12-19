object FormEncryptFiles: TFormEncryptFiles
  Left = 259
  Top = 139
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Encrypt Files'
  ClientHeight = 565
  ClientWidth = 483
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 465
    Height = 41
    AutoSize = False
    Caption = 
      'The Encrypt Files demo shows how to use CBFilter to intercept re' +
      'quests for files and encrypt or decrypt the content on the fly w' +
      'ith a simple XOR algorithm. When files in the specified path are' +
      ' read or written they are decrypted or encrypted respectively.'
    WordWrap = True
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 55
    Width = 465
    Height = 58
    Caption = 'Driver Management'
    TabOrder = 0
    object lblDrvStatus: TLabel
      Left = 193
      Top = 29
      Width = 64
      Height = 13
      Caption = 'Driver Status:'
    end
    object btnInstall: TButton
      Left = 11
      Top = 24
      Width = 85
      Height = 25
      Caption = 'Install driver'
      TabOrder = 0
      OnClick = btnInstallClick
    end
    object btnUninstall: TButton
      Left = 102
      Top = 24
      Width = 85
      Height = 25
      Caption = 'Uninstall drvier'
      Enabled = False
      TabOrder = 1
      OnClick = btnUninstallClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 119
    Width = 465
    Height = 434
    Caption = 'Filter Settings'
    TabOrder = 1
    object lblPath: TLabel
      Left = 11
      Top = 32
      Width = 74
      Height = 13
      Caption = 'Monitoring Path'
    end
    object btnSetFilter: TButton
      Left = 11
      Top = 56
      Width = 85
      Height = 25
      Caption = 'Set Filter'
      TabOrder = 0
      OnClick = btnSetFilterClick
    end
    object memLog: TMemo
      Left = 11
      Top = 87
      Width = 443
      Height = 336
      ScrollBars = ssBoth
      TabOrder = 1
    end
    object btnDeleteFilter: TButton
      Left = 102
      Top = 56
      Width = 85
      Height = 25
      Caption = 'Delete Filter'
      TabOrder = 2
      OnClick = btnDeleteFilterClick
    end
    object edtPath: TEdit
      Left = 91
      Top = 29
      Width = 358
      Height = 21
      TabOrder = 3
      Text = 'C:\Crypt'
    end
  end
  object dlgOpen: TOpenDialog
    FileName = 'cbfilter.cab'
    Filter = 'CBFS Filter driver package (cbfilter.cab)|cbfilter.cab'
    Title = 'Select CBFS Filter driver package'
    Left = 384
    Top = 40
  end
  object tmLog: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tmLogTimer
    Left = 448
    Top = 40
  end
end


