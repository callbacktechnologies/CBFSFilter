(*
 * CBFS Filter 2022 Delphi Edition - Sample Project
 *
 * This sample project demonstrates the usage of CBFS Filter in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.callback.com/cbfsfilter
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 *)
unit procmonf;

interface

uses
  Windows, WinSvc, TlHelp32, Messages, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ComCtrls, XPMan,
  cbfconstants, cbfcbprocess;

const
  WM_LOG_ENTRY = WM_USER + 1;
  WM_STOP_FILTER = WM_USER + 2;

type
  TFormProcmon = class(TForm)
    grpDriver: TGroupBox;
    btnInstall: TButton;
    btnUninstall: TButton;
    lblStatus: TLabel;
    grpManager: TGroupBox;
    edtProcessName: TEdit;
    lblProcessName: TLabel;
    cbxDenyExecution: TCheckBox;
    cbxDenyTermination: TCheckBox;
    cbxDenySuspension: TCheckBox;
    btnExecute: TButton;
    btnTerminate: TButton;
    btnSuspend: TButton;
    btnResume: TButton;
    btnBrowse: TButton;
    cbxDenyThreadsTermination: TCheckBox;
    btnTerminateThreads: TButton;
    grpMonitor: TGroupBox;
    lvwEvents: TListView;
    lblIntro: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnInstallClick(Sender: TObject);
    procedure btnUninstallClick(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
    procedure btnSuspendClick(Sender: TObject);
    procedure btnResumeClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure cbxDenyExecutionClick(Sender: TObject);
    procedure cbxDenyTerminationClick(Sender: TObject);
    procedure cbxDenySuspensionClick(Sender: TObject);
    procedure btnTerminateClick(Sender: TObject);
    procedure btnTerminateThreadsClick(Sender: TObject);
    procedure cbxDenyThreadsTerminationClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    // Process id for the process to be tested.
    FProcessId: Cardinal;

    // Some flags that specify the program state.
    FDriverRunning: Boolean;
    FProcessExecuted: Boolean;
    FProcessSuspended: Boolean;

    // Flags represent state of some windows controls.
    FProcessDenyExecute: Boolean;
    FProcessDenyTerminate: Boolean;
    FProcessDenySuspendResume: Boolean;
    FThreadDenyTerminate: Boolean;

    // CBProcess instance.
    FFilter: TcbfCBProcess;

    FStarted: Int64;
	FClosing: boolean;

    procedure Log(Text: string);
    procedure AddLogEntry(Time: Integer; const Text: string);

    function CreateFilter(): TcbfCBProcess;
    function CreateOpenCabFileDialog(): TOpenDialog;
    function CreateOpenExeFileDialog(): TOpenDialog;

    procedure AskForReboot(IsInstall : boolean);
    procedure UpdateDriverStatus(Filter: TcbfCBProcess);
    procedure UpdateProcessControls();

    procedure ProcessCreationEventHandler(Sender: TObject; ProcessId: Integer; ParentProcessId: Integer;
      CreatingProcessId: Integer; CreatingThreadId: Integer; const ProcessName: string; const ImageFileName: string;
      FileOpenNameAvailable: Boolean; const CommandLine: string; var ResultCode: Integer);

    procedure ProcessHandleOperationEventHandler(Sender: TObject; Duplication: Boolean; ProcessId: Integer;
      OriginatorProcessId: Integer; OriginatorThreadId: Integer;
      SourceProcessId: Integer; TargetProcessId: Integer;
      OriginalDesiredAccess: Integer; var DesiredAccess: Integer;
      var ResultCode: Integer);

    procedure ProcessTerminationEventHandler(Sender: TObject; ProcessId: Integer; const ProcessName: string;
      var ResultCode: Integer);

    procedure ThreadCreationEventHandler(Sender: TObject; ProcessId: Integer; ThreadId: Integer;
      CreatingProcessId: Integer; CreatingThreadId: Integer; var ResultCode: Integer);

    procedure ThreadHandleOperationEventHandler(Sender: TObject; Duplication: Boolean; ProcessId: Integer;
      ThreadId: Integer; OriginatorProcessId: Integer; OriginatorThreadId: Integer;
      SourceProcessId: Integer; TargetProcessId: Integer;
      OriginalDesiredAccess: Integer; var DesiredAccess: Integer;
      var ResultCode: Integer);

    procedure ThreadTerminationEventHandler(Sender: TObject; ProcessId: Integer; ThreadId: Integer;
      var ResultCode: Integer);

    procedure WMLogEntry(var Msg: TMessage); message WM_LOG_ENTRY;
    procedure WMStopFilter(var Msg: TMessage); message WM_STOP_FILTER;
  public
    { Public declarations }
  end;

var
  FormProcmon: TFormProcmon;

implementation

{$R *.dfm}

const
  ProductGuid = '{713CC6CE-B3E2-4fd9-838D-E28F558F6866}';
  PROCESS_SUSPEND_RESUME = $0800;
  THREAD_TERMINATE = $0001;
  THREAD_ALL_ACCESS = $000F0000 or $00100000 or $3FF;

function LsaNtStatusToWinError(Status: Cardinal): Cardinal; stdcall; external 'advapi32.dll';
function NtSuspendProcess(Handle: THandle): Cardinal; stdcall; external 'ntdll.dll';
function NtResumeProcess(Handle: THandle): Cardinal; stdcall; external 'ntdll.dll';
function OpenThread(DesiredAccess: DWORD; InheritHandle: LongBool; ThreadId: DWORD): THandle; stdcall; external 'kernel32.dll';
function GetTickCount64(): Int64; stdcall; external 'kernel32.dll';

function IsRightAllowed(Access, Right: Integer): Boolean;
begin
  Result := (Access and Right) <> 0;
end;

procedure TFormProcmon.FormCreate(Sender: TObject);
var
  Filter: TcbfCBProcess;
begin
  FDriverRunning := false;
  FProcessExecuted := false;
  FProcessSuspended := false;
  FStarted := 0;

  Filter := CreateFilter();
  try
    UpdateDriverStatus(Filter);
  finally
    FreeAndNil(Filter);
  end;

  UpdateProcessControls;
end;

procedure TFormProcmon.FormDestroy(Sender: TObject);
begin
  FClosing := true;
  if FFilter <> nil then
  begin
    FreeAndNil(FFilter);
  end;
end;

procedure TFormProcmon.Log(Text: string);
var
  T, Sz: Integer;
  P: PChar;
begin
  if FClosing then Exit;
  T := Integer(GetTickCount64() - FStarted);
  Sz := (Length(Text) + 1) * SizeOf(Char);
  GetMem(P, Sz);
  Move(PChar(Text)^, P^, Sz);
  PostMessage(Handle, WM_LOG_ENTRY, T, LParam(P));
end;

function TFormProcmon.CreateFilter(): TcbfCBProcess;
begin
  Result := TcbfCBProcess.Create(nil);
  Result.OnProcessCreation := ProcessCreationEventHandler;
  Result.OnProcessHandleOperation := ProcessHandleOperationEventHandler;
  Result.OnProcessTermination := ProcessTerminationEventHandler;
  Result.OnThreadCreation := ThreadCreationEventHandler;
  Result.OnThreadHandleOperation := ThreadHandleOperationEventHandler;
  Result.OnThreadTermination := ThreadTerminationEventHandler;
end;

function TFormProcmon.CreateOpenCabFileDialog(): TOpenDialog;
begin
  Result := TOpenDialog.Create(nil);
  Result.Title := 'Select CBProcess Driver Package';
  Result.DefaultExt := 'cab';
  Result.FileName := 'cbprocess.cab';
  Result.Filter := 'CBProcess driver package|cbprocess.cab';
  Result.Options := [ofFileMustExist,ofNoNetworkButton,ofEnableSizing,ofDontAddToRecent];
end;

function TFormProcmon.CreateOpenExeFileDialog(): TOpenDialog;
begin
  Result := TOpenDialog.Create(nil);
  Result.Title := 'Select an Executable File';
  Result.DefaultExt := 'exe';
  Result.FileName := '*.exe';
  Result.Filter := 'Executable Files (*.exe)|*.exe|All Files (*.*)|*.*';
  Result.Options := [ofFileMustExist,ofNoNetworkButton,ofEnableSizing,ofDontAddToRecent];
end;

procedure TFormProcmon.UpdateDriverStatus(Filter: TcbfCBProcess);
var
  Status: Integer;
  State: string;
  Version: Int64;
begin
  Assert(Filter <> nil);
  FDriverRunning := false;

  try
    Status := Filter.GetDriverStatus(ProductGuid);

    if Status = 0 then
    begin
      lblStatus.Caption := 'Driver is not installed';
      btnUninstall.Enabled := false;
    end
    else
    begin
      case Status of
        SERVICE_STOPPED:
          State := 'stopped';
        SERVICE_START_PENDING:
          State := 'start pending';
        SERVICE_STOP_PENDING:
          State := 'stop pending';
        SERVICE_RUNNING:
          begin
           State := 'running';
           FDriverRunning := true;
          end;
        SERVICE_CONTINUE_PENDING:
          State := 'continue pending';
        SERVICE_PAUSE_PENDING:
          State := 'pause pending';
        SERVICE_PAUSED:
          State := 'paused';
      else
        State := Format('unknown state %d', [Status]);
      end;
      btnUninstall.Enabled := true;

      Version := Filter.GetDriverVersion(ProductGuid);
      lblStatus.Caption := Format('Driver (ver %d.%d.%d.%d) installed, service %s',
        [Version shr 48, Version shr 32 and $FFFF, Version shr 16 and $FFFF, Version and $FFFF, State]);
    end;
  except
    on E: Exception do
      lblStatus.Caption := 'Error: ' + E.Message;
  end;
end;

procedure TFormProcmon.UpdateProcessControls();
begin
  btnInstall.Enabled := not FDriverRunning;
  btnUninstall.Enabled := FDriverRunning and not FProcessExecuted;

  edtProcessName.Enabled := FDriverRunning and not FProcessExecuted;
  btnBrowse.Enabled := FDriverRunning and not FProcessExecuted;

  cbxDenyExecution.Enabled := FDriverRunning;
  cbxDenyTermination.Enabled := FDriverRunning;
  cbxDenySuspension.Enabled := FDriverRunning;
  cbxDenyThreadsTermination.Enabled := FDriverRunning;

  btnExecute.Enabled := FDriverRunning and not FProcessExecuted;
  btnTerminate.Enabled := FDriverRunning and FProcessExecuted;
  btnSuspend.Enabled := FDriverRunning and FProcessExecuted and not FProcessSuspended;
  btnResume.Enabled := FDriverRunning and FProcessExecuted and FProcessSuspended;
  btnTerminateThreads.Enabled := FDriverRunning and FProcessExecuted;
end;

procedure TFormProcmon.AddLogEntry(Time: Integer; const Text: string);
var
  Item: TListItem;
begin
  if FClosing then Exit;
  lvwEvents.Items.BeginUpdate();
  Item := lvwEvents.Items.Add();
  Item.Caption := IntToStr(Time);
  Item.SubItems.Add(Text);
  lvwEvents.Items.EndUpdate();
  Item.MakeVisible(False);
end;

procedure TFormProcmon.AskForReboot(isInstall: Boolean);
var
  t: String;
begin
  if isInstall then 
    t := 'install'
  else 
    t := 'uninstall';
  MessageDlg('System restart is needed in order to ' + t + ' the drivers.' +
    #13#10 + 'Please, reboot your computer now.', mtInformation, [mbOK], 0);
end;

procedure TFormProcmon.btnInstallClick(Sender: TObject);
var
  Dialog: TOpenDialog;
  CabFile: string;
  Filter: TcbfCBProcess;
  RebootNeeded: Boolean;
begin
  Dialog := CreateOpenCabFileDialog();
  try
    if not Dialog.Execute() then
      Exit;

    CabFile := Dialog.FileName;
  finally
    FreeAndNil(Dialog);
  end;

  Filter := CreateFilter();
  try
    try
      RebootNeeded := Filter.Install(CabFile, ProductGuid, '', 0);
      UpdateDriverStatus(Filter);
      UpdateProcessControls();
    except
      on E: EcbfCBProcess do
      begin
        if (E.Code = ERROR_ACCESS_DENIED) or (E.Code = ERROR_PRIVILEGE_NOT_HELD) then
          MessageDlg('Installation requires administrator rights. Please run the app as Administrator', mtWarning, [mbOk], 0)
        else
          MessageDlg('Installation failed.'#13#10 + E.Message, mtError, [mbOk], 0);
        Exit;
      end;
    end;
  finally
    FreeAndNil(Filter);
  end;

  if RebootNeeded then
    AskForReboot(true)
  else
    MessageDlg('Driver installed successfully', mtInformation, [mbOk], 0);
end;

procedure TFormProcmon.btnUninstallClick(Sender: TObject);
var
  Dialog: TOpenDialog;
  CabFile: string;
  Filter: TcbfCBProcess;
  RebootNeeded: Boolean;
begin
  Dialog := CreateOpenCabFileDialog();
  try
    if not Dialog.Execute() then
      Exit;

    CabFile := Dialog.FileName;
  finally
    FreeAndNil(Dialog);
  end;

  Filter := CreateFilter();
  try
    try
      RebootNeeded := Filter.Uninstall(CabFile, ProductGuid, '', UNINSTALL_VERSION_CURRENT);
      UpdateDriverStatus(Filter);
      UpdateProcessControls();
    except
      on E: EcbfCBProcess do
      begin
        if (E.Code = ERROR_ACCESS_DENIED) or (E.Code = ERROR_PRIVILEGE_NOT_HELD) then
          MessageDlg('Uninstallation requires administrator rights. Please run the app as Administrator', mtWarning, [mbOk], 0)
        else
          MessageDlg('Uninstallation failed.'#13#10 + E.Message, mtError, [mbOk], 0);
        Exit;
      end;
    end;
  finally
    FreeAndNil(Filter);
  end;

  if RebootNeeded then
    AskForReboot(false)
  else
    MessageDlg('Driver uninstalled successfully', mtInformation, [mbOk], 0);
end;

procedure TFormProcmon.cbxDenyExecutionClick(Sender: TObject);
begin
  FProcessDenyExecute := cbxDenyExecution.Checked;
end;

procedure TFormProcmon.cbxDenySuspensionClick(Sender: TObject);
begin
  FProcessDenySuspendResume := cbxDenySuspension.Checked;
end;

procedure TFormProcmon.cbxDenyTerminationClick(Sender: TObject);
begin
  FProcessDenyTerminate := cbxDenyTermination.Checked;
end;

procedure TFormProcmon.cbxDenyThreadsTerminationClick(Sender: TObject);
begin
  FThreadDenyTerminate := cbxDenyThreadsTermination.Checked;
end;

procedure TFormProcmon.btnBrowseClick(Sender: TObject);
var
  Dialog: TOpenDialog;
begin
  Dialog := CreateOpenExeFileDialog();
  try
    if Dialog.Execute() then
      edtProcessName.Text := Dialog.FileName;
  finally
    FreeAndNil(Dialog);
  end;
end;

procedure TFormProcmon.btnExecuteClick(Sender: TObject);
var
  Filename: string;
  StartInfo: TStartUpInfo;
  ProcInfo: TProcessInformation;
  Started: Boolean;
begin
  lvwEvents.Clear();

 if FFilter = nil then
  FFilter := CreateFilter();
  try
    FFilter.Initialize(ProductGuid);

    // Make the demo can receive notifications triggered by itself.
    FFilter.Config('FilterOwnRequests=true');
    FFilter.Config('EventsToFire=-1');
    FStarted := GetTickCount64();
    FFilter.StartFilter(30000);
    Log('Filter started');
  except
    on E: EcbfCBProcess do
    begin
      FreeAndNil(FFilter);
      MessageDlg('Filter not started.'#13#10 + E.Message, mtError, [mbOk],0);
      Exit;
    end;
  end;

  Filename := edtProcessName.Text;

  // Ask the filter to call the events for the process.
  FFilter.AddFilteredProcessByName(ExtractFileName(Filename), false);

  ZeroMemory(@StartInfo, SizeOf(TStartupInfo));
  StartInfo.cb := SizeOf(TStartupInfo);

  Started := CreateProcess(nil, PChar(Filename), nil, nil, false, NORMAL_PRIORITY_CLASS, nil, nil, StartInfo, ProcInfo);

  if not Started then
  begin
    FFilter.StopFilter();
    FreeAndNil(FFilter);
    Log('Filter stopped');
    MessageDlg(SysErrorMessage(GetLastError), mtError, [mbOk], 0);
    Exit;
  end;

  CloseHandle(ProcInfo.hThread);
  CloseHandle(ProcInfo.hProcess);

  FProcessId := ProcInfo.dwProcessId;
  FProcessExecuted := true;
  FProcessSuspended := false;
  UpdateProcessControls();
end;

procedure TFormProcmon.btnTerminateClick(Sender: TObject);
var
  ProcessHandle: THandle;
begin
  ProcessHandle := OpenProcess(PROCESS_ALL_ACCESS{PROCESS_TERMINATE}, false, FProcessId);
  if ProcessHandle = 0 then
  begin
    MessageDlg(SysErrorMessage(GetLastError()), mtError, [mbOk], 0);
    Exit;
  end;

  if not TerminateProcess(ProcessHandle, 0) then
    MessageDlg(SysErrorMessage(GetLastError()), mtError, [mbOk], 0);

  CloseHandle(ProcessHandle);
end;

procedure TFormProcmon.btnTerminateThreadsClick(Sender: TObject);
var
  ThreadSnap: THandle;
  ThreadHandle: THandle;
  ThreadEntry: TThreadEntry32;
begin
  ThreadSnap := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
  if ThreadSnap = INVALID_HANDLE_VALUE then
  begin
    MessageDlg(SysErrorMessage(GetLastError()), mtError, [mbOk], 0);
    Exit;
  end;

  ThreadEntry.dwSize := SizeOf(ThreadEntry);
  if not Thread32First(ThreadSnap, ThreadEntry) then
  begin
    MessageDlg(SysErrorMessage(GetLastError()), mtError, [mbOk], 0);
    CloseHandle(ThreadSnap);
    Exit;
  end;

  repeat
    if ThreadEntry.th32OwnerProcessID = FProcessId then
    begin
      ThreadHandle := OpenThread(THREAD_ALL_ACCESS{THREAD_TERMINATE}, false, ThreadEntry.th32ThreadID);
      if ThreadHandle = 0 then
      begin
        MessageDlg(SysErrorMessage(GetLastError()), mtError, [mbOk], 0);
        Continue;
      end;

      if not TerminateThread(ThreadHandle, 0) then
        MessageDlg(SysErrorMessage(GetLastError()), mtError, [mbOk], 0);

      CloseHandle(ThreadHandle);
    end;
  until not Thread32Next(ThreadSnap, ThreadEntry);

  CloseHandle(ThreadSnap);
end;

procedure TFormProcmon.btnSuspendClick(Sender: TObject);
var
  ProcessHandle: THandle;
  Status: Cardinal;
  Error: Cardinal;
begin
  ProcessHandle := OpenProcess(PROCESS_ALL_ACCESS{PROCESS_SUSPEND_RESUME}, false, FProcessId);
  if ProcessHandle = 0 then
  begin
    MessageDlg(SysErrorMessage(GetLastError()), mtError, [mbOk], 0);
    Exit;
  end;

  Status := NtSuspendProcess(ProcessHandle);
  CloseHandle(ProcessHandle);

  if Status = 0{STATUS_SUCCESS} then
    FProcessSuspended := true
  else
  begin
    Error := LsaNtStatusToWinError(Status);
    MessageDlg(SysErrorMessage(Error), mtError, [mbOk], 0);
  end;

  UpdateProcessControls();
end;

procedure TFormProcmon.btnResumeClick(Sender: TObject);
var
  ProcessHandle: THandle;
  Status: Cardinal;
  Error: Cardinal;
begin
  ProcessHandle := OpenProcess(PROCESS_ALL_ACCESS{PROCESS_SUSPEND_RESUME}, false, FProcessId);
  if ProcessHandle = 0 then
  begin
    MessageDlg(SysErrorMessage(GetLastError), mtError, [mbOk], 0);
    Exit;
  end;

  Status := NtResumeProcess(ProcessHandle);
  CloseHandle(ProcessHandle);

  if Status = 0{STATUS_SUCCESS} then
    FProcessSuspended := false
  else
  begin
    Error := LsaNtStatusToWinError(Status);
    MessageDlg(SysErrorMessage(Error), mtError, [mbOk], 0);
  end;

  UpdateProcessControls();
end;

procedure TFormProcmon.ProcessCreationEventHandler(Sender: TObject; ProcessId, ParentProcessId, CreatingProcessId,
  CreatingThreadId: Integer; const ProcessName, ImageFileName: string; FileOpenNameAvailable: Boolean;
  const CommandLine: string; var ResultCode: Integer);
begin
  if Cardinal(CreatingProcessId) <> GetCurrentProcessId() then
  begin
    Log(Format('Process %d created by another process -> no action required', [ProcessId]));
    Exit;
  end;

  Log(Format('Process %d created from %s', [ProcessId, ImageFileName]));

  // In order to avoid possible deadlock we don't ask directly
  // the "Deny to execute" checkbox about its state.
  if not FProcessDenyExecute then
    Log('Process creation allowed')
  else
  begin
    ResultCode := ERROR_ACCESS_DENIED;
    Log('PROCESS CREATION BLOCKED');
  end;
end;

procedure TFormProcmon.ProcessHandleOperationEventHandler(Sender: TObject; Duplication: Boolean; ProcessId: Integer;
      OriginatorProcessId: Integer; OriginatorThreadId: Integer;
      SourceProcessId: Integer; TargetProcessId: Integer;
      OriginalDesiredAccess: Integer; var DesiredAccess: Integer;
      var ResultCode: Integer);
begin
  // check the operation originator process to prevent system delays
  if Cardinal(OriginatorProcessId) <> GetCurrentProcessId() then
    Exit;

  // check if the operation is performed on the managed process
  if Cardinal(ProcessId) <> FProcessId then
    Exit;

  Log(Format('Process %d is being opened with rights $%X', [ProcessId, DesiredAccess]));

  if IsRightAllowed(DesiredAccess, PROCESS_TERMINATE) then
  begin
    if not FProcessDenyTerminate then
      Log('Process termination right allowed')
    else
    begin
      DesiredAccess := DesiredAccess and not PROCESS_TERMINATE;
      Log(Format('PROCESS TERMINATION RIGHT REMOVED: $%X', [DesiredAccess]));
    end;
  end;

  if IsRightAllowed(DesiredAccess, PROCESS_SUSPEND_RESUME) then
  begin
    if not FProcessDenySuspendResume then
      Log('Process suspension/resuming right allowed')
    else
    begin
      DesiredAccess := DesiredAccess and not PROCESS_SUSPEND_RESUME;
      Log(Format('PROCESS SUSPENSION/RESUMING RIGHT REMOVED: $%X', [DesiredAccess]));
    end;
  end;
end;

procedure TFormProcmon.ProcessTerminationEventHandler(Sender: TObject; ProcessId: Integer; const ProcessName: string;
  var ResultCode: Integer);
begin
  if Cardinal(ProcessId) <> FProcessId then
    Exit;

  Log(Format('Process %d terminated', [ProcessId]));

  // The managed process has been terminated. So we can stop the filter but
  // in order to avoid possible deadlock do it asynchronously.
  PostMessage(Self.Handle, WM_STOP_FILTER, 0, 0);
end;

procedure TFormProcmon.ThreadCreationEventHandler(Sender: TObject; ProcessId, ThreadId, CreatingProcessId,
  CreatingThreadId: Integer; var ResultCode: Integer);
begin
  Log(Format('Thread %d created', [ThreadId]));
end;

procedure TFormProcmon.ThreadHandleOperationEventHandler(Sender: TObject; Duplication: Boolean; ProcessId: Integer;
  ThreadId: Integer; OriginatorProcessId: Integer; OriginatorThreadId: Integer;
  SourceProcessId: Integer; TargetProcessId: Integer;
  OriginalDesiredAccess: Integer; var DesiredAccess: Integer;
  var ResultCode: Integer);
begin
  // check the operation originator process to prevent system delays
  if Cardinal(OriginatorProcessId) <> GetCurrentProcessId() then
    Exit;

  Log(Format('Thread %d is being opened with rights $%X', [ThreadId, DesiredAccess]));

  if IsRightAllowed(DesiredAccess, THREAD_TERMINATE) then
  begin
    if not FThreadDenyTerminate then
      Log('Thread termination right allowed')
    else
    begin
      DesiredAccess := DesiredAccess and not THREAD_TERMINATE;
      Log(Format('THREAD TERMINATION RIGHT REMOVED: $%X', [DesiredAccess]));
    end;
  end;
end;

procedure TFormProcmon.ThreadTerminationEventHandler(Sender: TObject; ProcessId, ThreadId: Integer;
  var ResultCode: Integer);
begin
  Log(Format('Thread %d terminated', [ThreadId]));
end;

procedure TFormProcmon.WMLogEntry(var Msg: TMessage);
var
  S: string;
  P: PChar;
begin
  P := PChar(Msg.LParam);
  Msg.LParam := 0;

  S := StrPas(P);
  UniqueString(S);
  FreeMem(P);

  AddLogEntry(Msg.WParam, S);
end;

procedure TFormProcmon.WMStopFilter(var Msg: TMessage);
begin
  FFilter.StopFilter();
  FreeAndNil(FFilter);
  AddLogEntry(Integer(GetTickCount64() - FStarted), 'Filter stopped');

  FProcessId := 0;
  FProcessExecuted := false;
  UpdateProcessControls();
end;

end.


