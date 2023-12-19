object FormFilemon: TFormFilemon
  Left = 192
  Top = 108
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'CBFS Filter File Monitor'
  ClientHeight = 643
  ClientWidth = 978
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 936
    Height = 13
    AutoSize = False
    Caption = 
      'The File Monitor shows how to monitor file activity using the CB' +
      'Monitor component. Specify a path and filemask and observe event' +
      's as they occur in the log below.'
    WordWrap = True
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 27
    Width = 961
    Height = 62
    Caption = 'Driver Management'
    TabOrder = 0
    object lblDrvStatus: TLabel
      Left = 198
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
      Left = 107
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
    Top = 95
    Width = 961
    Height = 538
    Caption = 'Monitor Settings'
    TabOrder = 1
    DesignSize = (
      961
      538)
    object lblPath: TLabel
      Left = 16
      Top = 28
      Width = 74
      Height = 13
      Caption = 'Monitoring Path'
    end
    object btnSetFilter: TButton
      Left = 16
      Top = 58
      Width = 85
      Height = 25
      Caption = 'Set Filter'
      TabOrder = 0
      OnClick = btnSetFilterClick
    end
    object btnDeleteFilter: TButton
      Left = 107
      Top = 58
      Width = 85
      Height = 25
      Caption = 'Delete Filter'
      TabOrder = 1
      OnClick = btnDeleteFilterClick
    end
    object edtPath: TEdit
      Left = 107
      Top = 25
      Width = 839
      Height = 21
      TabOrder = 2
      Text = 'c:\*.*'
    end
    object ButtonCls: TButton
      Left = 1205
      Top = 370
      Width = 75
      Height = 25
      Caption = 'Clear Screen'
      TabOrder = 3
      OnClick = ButtonClsClick
    end
    object memLog: TListView
      Left = 16
      Top = 89
      Width = 930
      Height = 432
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelEdges = [beLeft, beTop]
      Columns = <
        item
          Caption = 'Operation'
          Width = 150
        end
        item
          Caption = 'Path'
          Width = 250
        end
        item
          Caption = 'Originator Process'
          Width = 150
        end
        item
          Caption = 'Process ID'
          Width = 150
        end
        item
          Caption = 'User Name'
          Width = 150
        end
        item
          Caption = 'Result'
          Width = 150
        end>
      FullDrag = True
      GridLines = True
      TabOrder = 4
      ViewStyle = vsReport
      ExplicitHeight = 440
    end
  end
  object dlgOpen: TOpenDialog
    FileName = 'cbfilter.cab'
    Filter = 'CBFS Filter driver package (cbfilter.cab)|cbfilter.cab'
    Title = 'Select CBFS Filter driver package'
    Left = 824
    Top = 16
  end
  object tmLog: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tmLogTimer
    Left = 776
    Top = 16
  end
end


