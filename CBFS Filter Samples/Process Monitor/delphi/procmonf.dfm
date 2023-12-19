object FormProcmon: TFormProcmon
  Left = 316
  Top = 230
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Process Monitor'
  ClientHeight = 517
  ClientWidth = 485
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    485
    517)
  TextHeight = 13
  object lblIntro: TLabel
    Left = 8
    Top = 8
    Width = 465
    Height = 41
    AutoSize = False
    Caption = 
      'The Process Monitor demo shows how to monitor and restrict acces' +
      's to a process. Specify a process to launch, restrictions to be ' +
      'enforced, and click Execute to start the process. Use the button' +
      's to attempt an operation on the process, and view the results i' +
      'n the log output.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object grpDriver: TGroupBox
    Left = 7
    Top = 55
    Width = 471
    Height = 63
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Driver Management'
    TabOrder = 0
    ExplicitWidth = 467
    DesignSize = (
      471
      63)
    object lblStatus: TLabel
      Left = 174
      Top = 27
      Width = 289
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Driver Status'
      ShowAccelChar = False
    end
    object btnInstall: TButton
      Left = 11
      Top = 22
      Width = 75
      Height = 25
      Caption = 'Install...'
      TabOrder = 0
      OnClick = btnInstallClick
    end
    object btnUninstall: TButton
      Left = 90
      Top = 22
      Width = 75
      Height = 25
      Caption = 'Uninstall'
      TabOrder = 1
      OnClick = btnUninstallClick
    end
  end
  object grpManager: TGroupBox
    Left = 6
    Top = 124
    Width = 471
    Height = 158
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Process Info'
    TabOrder = 1
    ExplicitWidth = 467
    DesignSize = (
      471
      158)
    object lblProcessName: TLabel
      Left = 11
      Top = 22
      Width = 73
      Height = 13
      Caption = ' Process name:'
    end
    object edtProcessName: TEdit
      Left = 11
      Top = 39
      Width = 421
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'C:\Windows\notepad.exe'
      ExplicitWidth = 417
    end
    object cbxDenyExecution: TCheckBox
      Left = 11
      Top = 71
      Width = 213
      Height = 17
      Caption = 'Deny execution'
      TabOrder = 2
      OnClick = cbxDenyExecutionClick
    end
    object cbxDenyTermination: TCheckBox
      Left = 11
      Top = 94
      Width = 213
      Height = 17
      Caption = 'Deny termination'
      TabOrder = 3
      OnClick = cbxDenyTerminationClick
    end
    object cbxDenySuspension: TCheckBox
      Left = 230
      Top = 71
      Width = 230
      Height = 17
      Caption = 'Deny suspension/resuming'
      TabOrder = 4
      OnClick = cbxDenySuspensionClick
    end
    object btnExecute: TButton
      Left = 11
      Top = 120
      Width = 75
      Height = 25
      Caption = 'Execute'
      TabOrder = 6
      OnClick = btnExecuteClick
    end
    object btnTerminate: TButton
      Left = 92
      Top = 120
      Width = 75
      Height = 25
      Caption = 'Terminate'
      TabOrder = 7
      OnClick = btnTerminateClick
    end
    object btnSuspend: TButton
      Left = 173
      Top = 120
      Width = 75
      Height = 25
      Caption = 'Suspend'
      TabOrder = 8
      OnClick = btnSuspendClick
    end
    object btnResume: TButton
      Left = 254
      Top = 120
      Width = 75
      Height = 25
      Caption = 'Resume'
      TabOrder = 9
      OnClick = btnResumeClick
    end
    object btnBrowse: TButton
      Left = 437
      Top = 39
      Width = 26
      Height = 22
      Anchors = [akTop, akRight]
      Caption = '...'
      TabOrder = 1
      OnClick = btnBrowseClick
      ExplicitLeft = 433
    end
    object cbxDenyThreadsTermination: TCheckBox
      Left = 230
      Top = 94
      Width = 230
      Height = 17
      Caption = 'Deny threads termination'
      TabOrder = 5
      OnClick = cbxDenyThreadsTerminationClick
    end
    object btnTerminateThreads: TButton
      Left = 335
      Top = 120
      Width = 125
      Height = 25
      Caption = 'Terminate threads'
      TabOrder = 10
      OnClick = btnTerminateThreadsClick
    end
  end
  object grpMonitor: TGroupBox
    Left = 8
    Top = 288
    Width = 469
    Height = 221
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Log'
    TabOrder = 2
    ExplicitWidth = 465
    ExplicitHeight = 220
    DesignSize = (
      469
      221)
    object lvwEvents: TListView
      Left = 10
      Top = 21
      Width = 449
      Height = 189
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          Caption = 'Time'
          Width = 60
        end
        item
          Caption = 'Event'
          Width = 360
        end>
      ColumnClick = False
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      ExplicitWidth = 445
      ExplicitHeight = 188
    end
  end
end


