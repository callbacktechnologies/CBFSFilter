object FormFilter: TFormFilter
  Left = 578
  Top = 327
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Filter'
  ClientHeight = 432
  ClientWidth = 544
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    544
    432)
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 528
    Height = 26
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'The Filter demo provides no-op event handlers that you can add y' +
      'our own code to in order to create basic CBFilter-based applicat' +
      'ions. Once you'#39've added your code, run the demo and create the d' +
      'esired filter rules.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object grpDriver: TGroupBox
    Left = 8
    Top = 45
    Width = 528
    Height = 61
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Driver Management'
    TabOrder = 0
    ExplicitWidth = 524
    DesignSize = (
      528
      61)
    object lblDriverStatus: TLabel
      Left = 204
      Top = 21
      Width = 315
      Height = 28
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Driver status'
      Layout = tlCenter
      WordWrap = True
    end
    object btnInstall: TButton
      Left = 9
      Top = 23
      Width = 89
      Height = 25
      Caption = 'Install Driver...'
      TabOrder = 0
      OnClick = btnInstallClick
    end
    object btnUninstall: TButton
      Left = 104
      Top = 23
      Width = 89
      Height = 25
      Caption = 'Uninstall Driver'
      TabOrder = 1
      OnClick = btnUninstallClick
    end
  end
  object grpRules: TGroupBox
    Left = 8
    Top = 112
    Width = 528
    Height = 246
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Rules'
    TabOrder = 1
    ExplicitWidth = 524
    ExplicitHeight = 245
    DesignSize = (
      528
      246)
    object lbMask: TLabel
      Left = 9
      Top = 29
      Width = 28
      Height = 13
      Caption = 'Mask:'
    end
    object lbFilterRules: TLabel
      Left = 9
      Top = 67
      Width = 63
      Height = 13
      Caption = 'Active Rules:'
    end
    object edtMask: TEdit
      Left = 43
      Top = 26
      Width = 186
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      ExplicitWidth = 182
    end
    object cmbFlags: TComboBox
      Left = 235
      Top = 26
      Width = 200
      Height = 21
      Style = csDropDownList
      Anchors = [akTop, akRight]
      TabOrder = 1
      ExplicitLeft = 231
    end
    object btnAddRule: TButton
      Left = 441
      Top = 24
      Width = 78
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Add'
      TabOrder = 2
      OnClick = btnAddRuleClick
      ExplicitLeft = 437
    end
    object btnDeleteRule: TButton
      Left = 441
      Top = 55
      Width = 78
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Delete'
      TabOrder = 3
      OnClick = btnDeleteRuleClick
      ExplicitLeft = 437
    end
    object lstRules: TListView
      Left = 9
      Top = 86
      Width = 510
      Height = 146
      Anchors = [akLeft, akTop, akRight, akBottom]
      Checkboxes = True
      Columns = <
        item
          Caption = 'Filter Mask'
          Width = 200
        end
        item
          Caption = 'Access Flags'
          Width = 95
        end
        item
          Caption = 'Control Flags'
          Width = 95
        end
        item
          Caption = 'Notify Flags'
          Width = 95
        end>
      ColumnClick = False
      GridLines = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 4
      ViewStyle = vsReport
      ExplicitWidth = 506
      ExplicitHeight = 145
    end
  end
  object grpFilter: TGroupBox
    Left = 8
    Top = 364
    Width = 528
    Height = 60
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Filter'
    TabOrder = 2
    ExplicitTop = 363
    ExplicitWidth = 524
    object btnStartFilter: TButton
      Left = 9
      Top = 20
      Width = 89
      Height = 25
      Caption = 'Start Filter'
      TabOrder = 0
      OnClick = btnStartFilterClick
    end
    object btnStopFilter: TButton
      Left = 104
      Top = 20
      Width = 89
      Height = 25
      Caption = 'Stop Filter'
      Enabled = False
      TabOrder = 1
      OnClick = btnStopFilterClick
    end
  end
  object dlgOpenDrv: TOpenDialog
    FileName = 'cbfilter.cab'
    Filter = 'CBFS Filter driver package (cbfilter.cab)|cbfilter.cab'
    Title = 'Select CBFS Filter driver package'
    Left = 255
    Top = 71
  end
  object XPManifest: TXPManifest
    Left = 458
    Top = 382
  end
end


