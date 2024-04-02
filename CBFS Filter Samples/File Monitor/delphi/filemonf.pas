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
unit filemonf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  WinSvc, IOUtils, RegularExpressions,
  StdCtrls,
  ComCtrls,
  SyncObjs,
  ExtCtrls,
  cbfconstants,
  cbfcbmonitor;

const
  MAX_QUEUE_LEN = 1000;
  MAX_OP_LEN = 50;
  
type
  TLogMessage = packed record
    Operation: array[0..MAX_OP_LEN] of Char;
    Path: array[0..MAX_PATH] of Char;
    ProcessName: array[0..MAX_PATH] of Char;
    ProcessID: Cardinal;
    Status: LongWord;
  end;

  TFormFilemon = class(TForm)
    dlgOpen: TOpenDialog;
    tmLog: TTimer;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    lblDrvStatus: TLabel;
    btnInstall: TButton;
    btnUninstall: TButton;
    GroupBox2: TGroupBox;
    lblPath: TLabel;
    btnSetFilter: TButton;
    btnDeleteFilter: TButton;
    edtPath: TEdit;
    ButtonCls: TButton;
    memLog: TListView;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnInstallClick(Sender: TObject);
    procedure btnUninstallClick(Sender: TObject);
    procedure btnSetFilterClick(Sender: TObject);
    procedure btnDeleteFilterClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonClsClick(Sender: TObject);
    procedure tmLogTimer(Sender: TObject);
  private
    cbfFilter: TcbfCBMonitor;
    Closing: boolean;

    msgList: array[0..MAX_QUEUE_LEN - 1] of TLogMessage;
    msgListSize: integer;
    lock: TCriticalSection;

    procedure cbfFilterNotifyCreateFile(Sender: TObject;
      const FileName: String;
      ExistingAttributes: Integer;
      DesiredAccess: Integer;
      Attributes: Integer;
      ShareMode: Integer;
      Options: Integer;
      CreateDisposition: Integer;
      SecurityDescriptor: Pointer;
      Length: Integer;
      Status: Integer;
      var ResultCode: Integer);
    procedure cbfFilterNotifyOpenFile(Sender: TObject;
      const FileName: String;
      ExistingAttributes: Integer;
      DesiredAccess: Integer;
      Attributes: Integer;
      ShareMode: Integer;
      Options: Integer;
      CreateDisposition: Integer;
      SecurityDescriptor: Pointer;
      Length: Integer;
      Status: Integer;
      var ResultCode: Integer);
    procedure cbfFilterNotifySetAllocationSize(Sender: TObject;
      const FileName: String; AllocationSize: Int64; Status: Integer; var ResultCode: Integer);
    procedure cbfFilterNotifySetFileSize(Sender: TObject;
      const FileName: String; Size: Int64; Status: Integer; var ResultCode: Integer);
    procedure cbfFilterNotifySetFileAttributes(Sender: TObject;
      const FileName: String; CreationTime: TDateTime; LastAccessTime: TDateTime;
      LastWriteTime: TDateTime; ChangeTime: TDateTime; FileAttributes: Integer; Status: Integer; var ResultCode: Integer);
    procedure cbfFilterNotifyRenameOrMoveFile(Sender: TObject;
      const FileName: String; const NewFileName: String; Status: Integer; var ResultCode: Integer);
    procedure cbfFilterNotifyCreateHardLink(Sender: TObject;
      const FileName: String; const LinkName: String; Status: Integer; var ResultCode: Integer);
    procedure cbfFilterNotifyReadFile(Sender: TObject;
      const FileName: String; Position: Int64; BytesToRead: Integer; Direction: Integer; BytesRead: Integer; Status: Integer; var ResultCode: Integer);
    procedure cbfFilterNotifyWriteFile(Sender: TObject;
      const FileName: String; Position: Int64; BytesToWrite: Integer; Direction: Integer; BytesWritten: Integer; Status: Integer; var ResultCode: Integer);
    procedure cbfFilterNotifyEnumerateDirectory(Sender: TObject;
      const DirectoryName: String;
      Flags: Integer;
      Index: Integer;
      const FileName: String;
      CreationTime: TDateTime;
      LastAccessTime: TDateTime;
      LastWriteTime: TDateTime;
      ChangeTime: TDateTime;
      Size: Int64;
      AllocationSize: Int64;
      FileId: Int64;
      Attributes: Integer;
      Status: Integer;
      var ResultCode: Integer);
    procedure cbfFilterNotifyCloseFile(Sender: TObject; const FileName: String; var ResultCode: Integer);
    procedure cbfFilterNotifySetFileSecurity(Sender: TObject;
      const FileName: String; SecurityInformation: Integer; SecurityDescriptor: Pointer;
      Length: Integer; Status: Integer; var ResultCode: Integer);
    procedure cbfFilterStop(Sender: TObject; var ResultCode: Integer);

    procedure AskForReboot(isInstall: Boolean);
    procedure UpdateAllButtons;
    procedure AddToLog(Flt: TObject;
      Operation: String; Path: String; Status: LongWord);
    procedure ViewMessages;
  public
    { Public declarations }
    function ConvertRelativePathToAbsolute(const path: string): string;
    function IsDriveLetter(const path: string): Boolean;
  end;

const
   FGuid = '{713CC6CE-B3E2-4fd9-838D-E28F558F6866}';
   ALTITUDE_FAKE_VALUE_FOR_DEBUG = '360000';

var
  FormFilemon: TFormFilemon;

implementation

{$R *.DFM}

function ArrayToString(const Arr: array of Char): string;
var
  Len: integer;
begin
  Len := StrLen(Arr);

  if Len > 0 then
  begin
    SetLength(Result, Len);
    Move(Arr[0], Result[1], Len * sizeof(Char));
  end
  else
    Result := '';
end;

procedure TFormFilemon.AskForReboot(isInstall: Boolean);
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

procedure TFormFilemon.tmLogTimer(Sender: TObject);
begin
  if cbfFilter <> nil then
    ViewMessages;
end;

function TFormFilemon.ConvertRelativePathToAbsolute(const path: string): string;
var
  res: string;
  homeDir: string;
begin
  res := path;
  if not path.IsEmpty then
  begin
    if (path = '~') or StartsText('~/', path) then
    begin
      homeDir := GetEnvironmentVariable('HOME');
      if path = '~' then
        Exit(homeDir)
      else
        Exit(homeDir + Copy(path, 2, MaxInt));
    end
    else if not IsDriveLetter(path) then
    begin
      try
        res := TPath.GetFullPath(path);

        if StartsText('\\', res) and not DirectoryExists(res) then
        begin
          MessageDlg('The network folder "' + res + '" does not exist.', mtError, [mbOk], 0);
        end
        else if not FileExists(res) and not DirectoryExists(res) then
        begin
          MessageDlg('The path "' + res + '" does not exist.', mtError, [mbOk], 0);
        end;
      except
        on E: Exception do
          MessageDlg('Error while converting to absolute path: ' + E.Message, mtError, [mbOk], 0);
      end;
    end;
  end;
  Result := res;
end;

function TFormFilemon.IsDriveLetter(const path: string): Boolean;
begin
  Result := False;

  if (path <> '') and (not path.IsEmpty) then
  begin
    if (path[1] in ['A'..'Z', 'a'..'z']) and (Length(path) = 2) and (path[2] = ':') then
      Result := True;
  end;
end;

procedure TFormFilemon.AddToLog(Flt: TObject;
  Operation: String; Path: String; Status: LongWord);
var
  ProcessName: string;
  Msg: ^TLogMessage;
begin
  if Closing then
    Exit;

  lock.Enter;
  try
    if msgListSize + 1 >= Length(msgList) then
      raise Exception.Create('Messages queue overflow!');

    Msg := @msgList[msgListSize];

    FillChar(Msg^, sizeof(TLogMessage), 0);

    Msg^.ProcessID := TcbfCBMonitor(Flt).GetOriginatorProcessId;
    Msg^.Status := Status;

    ProcessName := TcbfCBMonitor(Flt).GetOriginatorProcessName;

    StrPLCopy(@Msg^.Operation[0], Operation, Length(Msg^.Operation));
    StrPLCopy(@Msg^.Path[0], Path, Length(Msg^.Path));
    StrPLCopy(@Msg^.ProcessName[0], ProcessName, Length(Msg^.ProcessName));

    Inc(msgListSize);
  finally
    lock.Leave;
  end;
end;

procedure TFormFilemon.ViewMessages;
var
  Buf: TByteArray;
  Msg: ^TLogMessage;
  Item: TListItem;
  ProcessName, Error: string;
  i: integer;
begin
  if Closing then
    Exit;

  lock.Enter;
  memLog.Items.BeginUpdate;
  try
    for i := 0 to msgListSize - 1 do
    begin
      if memLog.Items.Count > 500 then
        memLog.Items.Clear;

      Msg := @msgList[i];

      Item := memLog.Items.Add;
      Item.Caption := ArrayToString(Msg^.Operation);
      Item.SubItems.Add(ArrayToString(Msg^.Path));

      ProcessName := ArrayToString(Msg^.ProcessName);

      if Length(ProcessName) = 0 then
        ProcessName := 'System';

      item.SubItems.Add(ProcessName);
      item.SubItems.Add(Format('%d', [Msg^.ProcessID]));

      if msgList[i].Status = ERROR_SUCCESS then
        Error := 'SUCCESS'
      else
        Error := SysErrorMessage(Msg^.Status);

      item.SubItems.Add('');
      item.SubItems.Add(Error);

      memLog.Scroll(0, 8);
    end;

    msgListSize := 0;
  finally
    memLog.Items.EndUpdate;
    lock.Leave;
  end;
end;

procedure TFormFilemon.UpdateAllButtons;
var
  Status: Integer;
  State: string;
  Version: Int64;
begin
  btnSetFilter.Enabled := not cbfFilter.Active;
  btnDeleteFilter.Enabled := cbfFilter.Active;
  tmLog.Enabled := cbfFilter.Active;
  try
    Status := cbfFilter.GetDriverStatus(FGuid);
    if Status = 0 then
    begin
      lblDrvStatus.Caption := 'Driver is not installed';
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
          State := 'running';
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
      Version := cbfFilter.GetDriverVersion(FGuid);
      lblDrvStatus.Caption := Format('Driver (ver %d.%d.%d.%d) installed, service %s',
        [Version shr 48, Version shr 32 and $FFFF, Version shr 16 and $FFFF, Version and $FFFF, State]);
    end;

  except
    on E: Exception do
      lblDrvStatus.Caption := 'Error: ' + E.Message;
  end;

end;

procedure TFormFilemon.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Closing := true;
end;

procedure TFormFilemon.FormCreate(Sender: TObject);
begin
  lock := TCriticalSection.Create;

  cbfFilter := TcbfCBMonitor.Create(Self);

  cbfFilter.OnNotifyCreateFile := cbfFilterNotifyCreateFile;
  cbfFilter.OnNotifyOpenFile := cbfFilterNotifyOpenFile;
  cbfFilter.OnNotifySetAllocationSize := cbfFilterNotifySetAllocationSize;
  cbfFilter.OnNotifySetFileSize := cbfFilterNotifySetFileSize;
  cbfFilter.OnNotifySetFileAttributes := cbfFilterNotifySetFileAttributes;
  cbfFilter.OnNotifyRenameOrMoveFile := cbfFilterNotifyRenameOrMoveFile;
  cbfFilter.OnNotifyCreateHardLink := cbfFilterNotifyCreateHardLink;
  cbfFilter.OnNotifyReadFile := cbfFilterNotifyReadFile;
  cbfFilter.OnNotifyWriteFile := cbfFilterNotifyWriteFile;
  cbfFilter.OnNotifyEnumerateDirectory := cbfFilterNotifyEnumerateDirectory;
  cbfFilter.OnNotifyCloseFile := cbfFilterNotifyCloseFile;
  cbfFilter.OnNotifySetFileSecurity := cbfFilterNotifySetFileSecurity;
  cbfFilter.OnFilterStop := cbfFilterStop;

  UpdateAllButtons;

  msgListSize := 0;
end;


procedure TFormFilemon.FormDestroy(Sender: TObject);
begin
  FreeAndNil(cbfFilter);
  FreeAndNil(lock);
end;


procedure TFormFilemon.btnInstallClick(Sender: TObject);
var
  RebootNeeded: Boolean;
begin
  RebootNeeded := false;
  if dlgOpen.Execute then
  begin
    try
      RebootNeeded := cbfFilter.Install(dlgOpen.FileName, FGuid, '', ALTITUDE_FAKE_VALUE_FOR_DEBUG, 0);
    except on E: EcbfCBMonitor do
      begin
        if (E.Code = ERROR_ACCESS_DENIED) or (E.Code = ERROR_PRIVILEGE_NOT_HELD) then
          MessageDlg('Installation requires administrator rights. Run the app as administrator', mtError, [mbOk], 0)
        else
          MessageDlg('Installation failed.'#13#10 + E.Message, mtError, [mbOk], 0);
      end;
    end;

    UpdateAllButtons;

    if RebootNeeded then
      AskForReboot(true);
  end;
end;


procedure TFormFilemon.btnUninstallClick(Sender: TObject);
var
  RebootNeeded: Boolean;
begin
  RebootNeeded := false;
  if dlgOpen.Execute then
  begin
    try
      RebootNeeded := cbfFilter.Uninstall(dlgOpen.FileName, FGuid, '', UNINSTALL_VERSION_CURRENT);
    except on E: EcbfCBMonitor do
      begin
        if (E.Code = ERROR_ACCESS_DENIED) or (E.Code = ERROR_PRIVILEGE_NOT_HELD) then
          MessageDlg('Uninstallation requires administrator rights. Run the app as administrator', mtError, [mbOk], 0)
        else
          MessageDlg('Installation failed.'#13#10 + E.Message, mtError, [mbOk], 0);
        end;
      end;
    UpdateAllButtons;
    if RebootNeeded then
      AskForReboot(false);
  end;
end;


procedure TFormFilemon.ButtonClsClick(Sender: TObject);
begin
  memLog.Clear;
end;


procedure TFormFilemon.btnSetFilterClick(Sender: TObject);
var
  Path: string;

begin
  Path := ConvertRelativePathToAbsolute(edtPath.Text);
  if Path = '' then 
  begin
    Dialogs.MessageDlg('Error: Invalid path', mtError, [mbOk], 0);
    Exit;
  end;
  cbfFilter.AddFilterRule(Path,
    cbfConstants.FS_NE_READ or
    cbfConstants.FS_NE_WRITE or
    cbfConstants.FS_NE_CREATE or
    cbfConstants.FS_NE_RENAME or
    cbfConstants.FS_NE_CREATE_HARD_LINK or
    cbfConstants.FS_NE_SET_SIZES or
    cbfConstants.FS_NE_DELETE or
    cbfConstants.FS_NE_SET_ATTRIBUTES or
    cbfConstants.FS_NE_ENUMERATE_DIRECTORY or
    cbfConstants.FS_NE_OPEN or
    cbfConstants.FS_NE_CLOSE or
    cbfConstants.FS_NE_SET_SECURITY
  );
  cbfFilter.ProcessFailedRequests := true;
  cbfFilter.Initialize(FGuid);
  cbfFilter.StartFilter();
  UpdateAllButtons;
end;


procedure TFormFilemon.btnDeleteFilterClick(Sender: TObject);
begin
  cbfFilter.DeleteAllFilterRules;
  cbfFilter.StopFilter(false);

  lock.Enter;
  try
    msgListSize := 0;
  finally
    lock.Leave;
  end;
end;


//
// Events
//
procedure TFormFilemon.cbfFilterNotifyCloseFile(Sender: TObject; const FileName: String; var ResultCode: Integer);
begin
  AddToLog(Sender, 'NotifyCloseFile', FileName, 0);
end;

procedure TFormFilemon.cbfFilterNotifyCreateFile(Sender: TObject;
  const FileName: String;
  ExistingAttributes: Integer;
  DesiredAccess: Integer;
  Attributes: Integer;
  ShareMode: Integer;
  Options: Integer;
  CreateDisposition: Integer;
  SecurityDescriptor: Pointer;
  Length: Integer;
  Status: Integer;
  var ResultCode: Integer);
begin
  AddToLog(Sender, 'NotifyCreateFile', FileName, cbfFilter.NtStatusToWin32Error(Status));
end;

procedure TFormFilemon.cbfFilterNotifyEnumerateDirectory(Sender: TObject;
  const DirectoryName: String;
  Flags: Integer;
  Index: Integer;
  const FileName: String;
  CreationTime: TDateTime;
  LastAccessTime: TDateTime;
  LastWriteTime: TDateTime;
  ChangeTime: TDateTime;
  Size: Int64;
  AllocationSize: Int64;
  FileId: Int64;
  Attributes: Integer;
  Status: Integer;
  var ResultCode: Integer);
begin
  AddToLog(Sender, 'NotifyEnumerateDirectory', DirectoryName+ '\' + FileName, cbfFilter.NtStatusToWin32Error(Status));
end;

procedure TFormFilemon.cbfFilterNotifyOpenFile(Sender: TObject;
  const FileName: String;
  ExistingAttributes: Integer;
  DesiredAccess: Integer;
  Attributes: Integer;
  ShareMode: Integer;
  Options: Integer;
  CreateDisposition: Integer;
  SecurityDescriptor: Pointer;
  Length: Integer;
  Status: Integer;
  var ResultCode: Integer);
begin
  AddToLog(Sender, 'NotifyOpenFile', FileName, cbfFilter.NtStatusToWin32Error(Status));
end;

procedure TFormFilemon.cbfFilterNotifyReadFile(Sender: TObject;
  const FileName: String; Position: Int64; BytesToRead: Integer; Direction: Integer; BytesRead: Integer; Status: Integer; var ResultCode: Integer);
begin
  AddToLog(Sender, 'NotifyReadFile', FileName, cbfFilter.NtStatusToWin32Error(Status));
end;

procedure TFormFilemon.cbfFilterNotifyRenameOrMoveFile(Sender: TObject;
  const FileName: String; const NewFileName: String; Status: Integer; var ResultCode: Integer);
begin
  AddToLog(Sender, 'CBFSFilterNotifyRenameOrMoveFile', FileName, cbfFilter.NtStatusToWin32Error(Status));
end;

procedure TFormFilemon.cbfFilterNotifyCreateHardLink(Sender: TObject;
  const FileName: String; const LinkName: String; Status: Integer; var ResultCode: Integer);
begin
  AddToLog(Sender, 'CBFSFilterNotifyCreateHardLink', LinkName, cbfFilter.NtStatusToWin32Error(Status));
end;

procedure TFormFilemon.cbfFilterNotifySetAllocationSize(Sender: TObject;
  const FileName: String; AllocationSize: Int64; Status: Integer; var ResultCode: Integer);
begin
  AddToLog(Sender, 'NotifySetAllocationSize', FileName, cbfFilter.NtStatusToWin32Error(Status));
end;

procedure TFormFilemon.cbfFilterNotifySetFileSize(Sender: TObject;
  const FileName: String; Size: Int64; Status: Integer; var ResultCode: Integer);
begin
  AddToLog(Sender, 'NotifySetFileSize', FileName, cbfFilter.NtStatusToWin32Error(Status));
end;

procedure TFormFilemon.cbfFilterNotifySetFileAttributes(Sender: TObject;
  const FileName: String; CreationTime: TDateTime; LastAccessTime: TDateTime;
  LastWriteTime: TDateTime; ChangeTime: TDateTime; FileAttributes: Integer; Status: Integer; var ResultCode: Integer);
begin
  AddToLog(Sender, 'NotifySetFileAttributes', FileName, cbfFilter.NtStatusToWin32Error(Status));
end;

procedure TFormFilemon.cbfFilterNotifyWriteFile(Sender: TObject;
  const FileName: String; Position: Int64; BytesToWrite: Integer; Direction: Integer; BytesWritten: Integer; Status: Integer; var ResultCode: Integer);
begin
  AddToLog(Sender, 'NotifyWriteFile', FileName, cbfFilter.NtStatusToWin32Error(Status));
end;

procedure TFormFilemon.cbfFilterNotifySetFileSecurity(Sender: TObject;
  const FileName: String; SecurityInformation: Integer; SecurityDescriptor: Pointer;
  Length: Integer; Status: Integer; var ResultCode: Integer);
begin
  AddToLog(Sender, 'NotifySetFileSecurity', FileName, cbfFilter.NtStatusToWin32Error(Status));
end;

procedure TFormFilemon.cbfFilterStop(Sender: TObject; var ResultCode: Integer);
begin
  if not Closing then
    UpdateAllButtons;
end;

end.




