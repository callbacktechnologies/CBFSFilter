object FormDirhide: TFormDirhide
  Left = 192
  Top = 108
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Directory Hider'
  ClientHeight = 440
  ClientWidth = 763
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    763
    440)
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 417
    Height = 13
    Caption = 
      'The Directory Hider demo shows how to hide a directory from acce' +
      'ss  by other programs.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clDefault
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object grpDirectory: TGroupBox
    Left = 8
    Top = 27
    Width = 434
    Height = 105
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Directory to Hide '
    TabOrder = 0
    ExplicitWidth = 430
    DesignSize = (
      434
      105)
    object Label2: TLabel
      Left = 11
      Top = 21
      Width = 185
      Height = 13
      Caption = 'Enter the full path of a directory to hide.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clDefault
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object edtPath: TEdit
      Left = 11
      Top = 40
      Width = 413
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      ExplicitWidth = 409
    end
    object btnHide: TButton
      Left = 11
      Top = 67
      Width = 100
      Height = 25
      Caption = 'Hide'
      TabOrder = 1
      OnClick = btnHideClick
    end
    object btnUnhide: TButton
      Left = 117
      Top = 67
      Width = 100
      Height = 25
      Caption = 'Unhide'
      TabOrder = 2
      OnClick = btnUnhideClick
    end
  end
  object grpDriver: TGroupBox
    Left = 448
    Top = 27
    Width = 307
    Height = 105
    Anchors = [akTop, akRight]
    Caption = ' Driver Management '
    TabOrder = 1
    ExplicitLeft = 444
    object Label3: TLabel
      Left = 12
      Top = 21
      Width = 283
      Height = 13
      Caption = 
        'Install or uninstall the driver. Administrator rights are requir' +
        'ed.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clDefault
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lblDriverStatus: TLabel
      Left = 9
      Top = 75
      Width = 62
      Height = 13
      Caption = 'Driver status:'
    end
    object btnInstall: TButton
      Left = 9
      Top = 40
      Width = 141
      Height = 25
      Caption = 'Install driver...'
      TabOrder = 0
      OnClick = btnInstallClick
    end
    object btnUninstall: TButton
      Left = 156
      Top = 40
      Width = 142
      Height = 25
      Caption = 'Uninstall driver...'
      Enabled = False
      TabOrder = 1
      OnClick = btnUninstallClick
    end
  end
  object grpLog: TGroupBox
    Left = 8
    Top = 138
    Width = 747
    Height = 294
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = ' Operations Log '
    TabOrder = 2
    ExplicitWidth = 743
    ExplicitHeight = 293
    DesignSize = (
      747
      294)
    object btnClear: TButton
      Left = 11
      Top = 260
      Width = 100
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Clear Log'
      TabOrder = 0
      OnClick = btnClearClick
      ExplicitTop = 259
    end
    object lvwLog: TListView
      Left = 11
      Top = 21
      Width = 724
      Height = 233
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelEdges = [beLeft, beTop]
      Columns = <
        item
          Caption = 'Operation'
          Width = 120
        end
        item
          Caption = 'Path'
          Width = 200
        end
        item
          Alignment = taRightJustify
          Caption = 'Process ID'
          Width = 70
        end
        item
          Caption = 'Process Name'
          Width = 200
        end
        item
          Caption = 'Result'
          Width = 110
        end>
      FullDrag = True
      GridLines = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 1
      ViewStyle = vsReport
      ExplicitWidth = 720
      ExplicitHeight = 232
    end
  end
  object dlgOpen: TOpenDialog
    FileName = 'cbfilter.cab'
    Filter = 'CBFS Filter driver package (cbfilter.cab)|cbfilter.cab'
    Title = 'Select CBFS Filter driver package'
    Left = 706
    Top = 89
  end
  object tmLog: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tmLogTimer
    Left = 344
    Top = 107
  end
end


