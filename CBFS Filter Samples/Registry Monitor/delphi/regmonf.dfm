object FormRegmon: TFormRegmon
  Left = 515
  Top = 383
  Caption = 'Registry Monitor'
  ClientHeight = 639
  ClientWidth = 816
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    816
    639)
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 804
    Height = 25
    AutoSize = False
    Caption = 
      'The Registry Monitor demo filters events made to the registry by' +
      ' the specified process. Select a process for which events will b' +
      'e fired, then begin filtering. As events occur entries in the Ev' +
      'ents window will be displayed.'
    WordWrap = True
  end
  object grpDriver: TGroupBox
    Left = 8
    Top = 39
    Width = 800
    Height = 62
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Driver Management'
    TabOrder = 0
    DesignSize = (
      800
      62)
    object lblStatus: TLabel
      Left = 240
      Top = 28
      Width = 552
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Driver Status'
      ShowAccelChar = False
      ExplicitWidth = 553
    end
    object btnInstall: TButton
      Left = 10
      Top = 22
      Width = 100
      Height = 25
      Caption = 'Install Driver'
      TabOrder = 0
      OnClick = btnInstallClick
    end
    object btnUninstall: TButton
      Left = 115
      Top = 22
      Width = 100
      Height = 25
      Caption = 'Uninstall Driver'
      TabOrder = 1
      OnClick = btnUninstallClick
    end
  end
  object grpProcess: TGroupBox
    Left = 8
    Top = 107
    Width = 800
    Height = 63
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Process '
    TabOrder = 1
    DesignSize = (
      800
      63)
    object lblFilename: TLabel
      Left = 18
      Top = 27
      Width = 46
      Height = 13
      Caption = 'Filename:'
    end
    object edtFilename: TEdit
      Left = 70
      Top = 24
      Width = 473
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'notepad.exe'
      ExplicitWidth = 474
    end
    object btnSelectFile: TButton
      Left = 549
      Top = 22
      Width = 28
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '...'
      TabOrder = 1
      OnClick = btnSelectFileClick
      ExplicitLeft = 550
    end
    object btnStart: TButton
      Left = 583
      Top = 22
      Width = 100
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Start Filtering'
      TabOrder = 2
      OnClick = btnStartClick
      ExplicitLeft = 584
    end
    object btnStop: TButton
      Left = 689
      Top = 22
      Width = 100
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Stop Filtering'
      Enabled = False
      TabOrder = 3
      OnClick = btnStopClick
      ExplicitLeft = 690
    end
  end
  object grpLog: TGroupBox
    Left = 8
    Top = 176
    Width = 800
    Height = 455
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = ' Events Log '
    TabOrder = 2
    DesignSize = (
      800
      455)
    object lvwLog: TListView
      Left = 12
      Top = 18
      Width = 777
      Height = 426
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          Caption = '#'
        end
        item
          Caption = 'Time (ms)'
          Width = 60
        end
        item
          Caption = 'Operation'
          Width = 100
        end
        item
          Caption = 'Key/Value Name'
          Width = 350
        end
        item
          Caption = 'Result'
          Width = 70
        end
        item
          AutoSize = True
          Caption = 'Details'
        end>
      ColumnClick = False
      FlatScrollBars = True
      GridLines = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      ExplicitWidth = 778
      ExplicitHeight = 300
    end
  end
  object LogTimer: TTimer
    Enabled = False
    OnTimer = LogTimerTimer
    Left = 400
    Top = 328
  end
end


