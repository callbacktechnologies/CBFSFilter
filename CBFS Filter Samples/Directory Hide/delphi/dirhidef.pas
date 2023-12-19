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
unit dirhidef;

interface

uses
  Windows, WinSvc, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, SyncObjs, IOUtils, RegularExpressions,
  cbfconstants, cbfcore, cbfcbfilter, ExtCtrls;

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

type
  TFormDirhide = class(TForm)
    dlgOpen: TOpenDialog;
    grpDirectory: TGroupBox;
    Label1: TLabel;
    grpDriver: TGroupBox;
    Label2: TLabel;
    edtPath: TEdit;
    btnHide: TButton;
    btnUnhide: TButton;
    Label3: TLabel;
    btnInstall: TButton;
    btnUninstall: TButton;
    lblDriverStatus: TLabel;
    grpLog: TGroupBox;
    btnClear: TButton;
    lvwLog: TListView;
    tmLog: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnInstallClick(Sender: TObject);
    procedure btnUninstallClick(Sender: TObject);
    procedure btnHideClick(Sender: TObject);
    procedure btnUnhideClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure tmLogTimer(Sender: TObject);
  private
    FFilter: TcbfCBFilter;
    FClosing: boolean;
    FOwnNameToHide: string;
    FPathToHide: string;

    msgList: array[0..MAX_QUEUE_LEN - 1] of TLogMessage;
    msgListSize: integer;
    lock: TCriticalSection;

    procedure cbfFilterAfterEnumerateDirectory(Sender: TObject;
      const DirectoryName: String;
      Flags: Integer;
      Index: Integer;
      var FileName: String;
      var CreationTime: TDateTime;
      var LastAccessTime: TDateTime;
      var LastWriteTime: TDateTime;
      var ChangeTime: TDateTime;
      var Size: Int64;
      var AllocationSize: Int64;
      var FileId: Int64;
      var Attributes: Integer;
      var Status: Integer;
      var DirectoryContext: Pointer;
      var HandleContext: Pointer;
      var EnumerationContext: Pointer;
      var ProcessRequest: Boolean;
      var ResultCode: Integer);

    procedure cbfFilterBeforeCreateFile(Sender: TObject;
      const FileName: String;
      ExistingAttributes: Integer;
      var Isolate: Boolean;
      var BackendFileName: String;
      var DesiredAccess: Integer;
      var Attributes: Integer;
      var ShareMode: Integer;
      var Options: Integer;
      var CreateDisposition: Integer;
      SecurityDescriptor: Pointer;
      Length: Integer;
      var FileContext: Pointer;
      var HandleContext: Pointer;
      var ProcessRequest: Boolean;
      var ResultCode: Integer);

    procedure cbfFilterBeforeOpenFile(Sender: TObject;
      const FileName: String;
      ExistingAttributes: Integer;
      var Isolate: Boolean;
      var BackendFileName: String;
      var DesiredAccess: Integer;
      var Attributes: Integer;
      var ShareMode: Integer;
      var Options: Integer;
      var CreateDisposition: Integer;
      SecurityDescriptor: Pointer;
      Length: Integer;
      var FileContext: Pointer;
      var HandleContext: Pointer;
      var ProcessRequest: Boolean;
      var ResultCode: Integer);

    procedure cbfFilterStop(Sender: TObject; var ResultCode: Integer);

    procedure AskForReboot(IsInstall: Boolean);
    procedure UpdateAllButtons;
    procedure AddToLog(Flt: TObject; Operation: String; Path: String; Status: LongWord);
    procedure ViewMessages;
  public
    { Public declarations }
    function ConvertRelativePathToAbsolute(const path: string): string;
    function IsDriveLetter(const path: string): Boolean;
  end;

var
  FormDirhide: TFormDirhide;

implementation

{$R *.DFM}

const
   ProgramName = '{713CC6CE-B3E2-4fd9-838D-E28F558F6866}';
   FakeAltitudeForDebug = '360000';

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

procedure TFormDirhide.AddToLog(Flt: TObject;
  Operation: String; Path: String; Status: LongWord);
var
  ProcessName: string;
  Msg: ^TLogMessage;
begin
  if FClosing then
    Exit;

  lock.Enter;
  try
    if msgListSize + 1 >= Length(msgList) then
      raise Exception.Create('Messages queue overflow!');

    Msg := @msgList[msgListSize];

    FillChar(Msg^, sizeof(TLogMessage), 0);

    Msg^.ProcessID := TcbfCBFilter(Flt).GetOriginatorProcessId;
    Msg^.Status := Status;

    ProcessName := TcbfCBFilter(Flt).GetOriginatorProcessName;

    StrPLCopy(@Msg^.Operation[0], Operation, Length(Msg^.Operation));
    StrPLCopy(@Msg^.Path[0], Path, Length(Msg^.Path));
    StrPLCopy(@Msg^.ProcessName[0], ProcessName, Length(Msg^.ProcessName));

    Inc(msgListSize);
  finally
    lock.Leave;
  end;
end;

procedure TFormDirhide.ViewMessages;
var
  Buf: TByteArray;
  Msg: ^TLogMessage;
  Item: TListItem;
  ProcessName, Error: string;
  i: integer;
begin
  if FClosing then exit;
  
  lock.Enter;
  lvwLog.Items.BeginUpdate;
  try
    for i := 0 to msgListSize - 1 do
    begin
      if lvwLog.Items.Count > 500 then
        lvwLog.Items.Clear;

      Msg := @msgList[i];

      Item := lvwLog.Items.Add;
      Item.Caption := ArrayToString(Msg^.Operation);
      Item.SubItems.Add(ArrayToString(Msg^.Path));
      Item.SubItems.Add(IntToStr(Msg^.ProcessID));

      ProcessName := ArrayToString(Msg^.ProcessName);

      if Length(ProcessName) = 0 then
        ProcessName := 'System';
      Item.SubItems.Add(ProcessName);

      case Msg^.Status of
        ERROR_SUCCESS:
          Error := 'Success';
        ERROR_FILE_NOT_FOUND:
          Error := 'File not found';
        ERROR_PATH_NOT_FOUND:
          Error := 'Path not found';
        ERROR_INVALID_NAME:
          Error := 'Invalid name';
      else
        Error := Format('%d = %s', [Msg^.Status, SysErrorMessage(Msg^.Status)]);
      end;
      Item.SubItems.Add(Error);

      Item.MakeVisible(False);
    end;

    msgListSize := 0;
  finally
    lvwLog.Items.EndUpdate;
    lock.Leave;
  end;
end;

function TFormDirhide.ConvertRelativePathToAbsolute(const path: string): string;
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

function TFormDirhide.IsDriveLetter(const path: string): Boolean;
begin
  Result := False;

  if (path <> '') and (not path.IsEmpty) then
  begin
    if (path[1] in ['A'..'Z', 'a'..'z']) and (Length(path) = 2) and (path[2] = ':') then
      Result := True;
  end;
end;

procedure TFormDirhide.AskForReboot(IsInstall: Boolean);
var
  Op: string;
begin
  if IsInstall then
    Op := 'install'
  else 
    Op := 'uninstall';

  MessageDlg('System restart is needed in order to ' + Op + ' the drivers.' +
    #13#10 + 'Please, reboot your computer now.', mtInformation, [mbOK], 0);
end;

procedure TFormDirhide.UpdateAllButtons();
var
  Status: Integer;
  State: string;
  Version: Int64;
begin
  btnHide.Enabled := not FFilter.Active;
  btnUnhide.Enabled := FFilter.Active;
  tmLog.Enabled := FFilter.Active;
  try
    Status := FFilter.GetDriverStatus(ProgramName);
    if Status = 0 then
    begin
      lblDrvStatus.Caption := 'Driver is not installed';
      btnUninstall.Enabled := false;
    end
    else
    begin
      case Status of
        SERVICE_STOPPED:
          State := 'STOPPED';
        SERVICE_START_PENDING:
          State := 'STARTING';
        SERVICE_STOP_PENDING:
          State := 'STOPPING';
        SERVICE_RUNNING:
          State := 'RUNNING';
        SERVICE_CONTINUE_PENDING:
          State := 'CONTINUING';
        SERVICE_PAUSE_PENDING:
          State := 'PAUSING';
        SERVICE_PAUSED:
          State := 'PAUSED';
      else
        State := Format('UNKNOWN - %d', [Status]);
      end;
      btnUninstall.Enabled := true;

      Version := FFilter.GetDriverVersion(ProgramName);
      lblDriverStatus.Caption := Format('Driver status: %s (version %d.%d.%d.%d)',
        [State, Version shr 48, Version shr 32 and $FFFF, Version shr 16 and $FFFF, Version and $FFFF]);
    end;
  except
    on E: Exception do
      lblDriverStatus.Caption := 'Error: ' + E.Message;
  end;
end;

procedure TFormDirhide.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FClosing := true;
end;

procedure TFormDirhide.FormCreate(Sender: TObject);
begin
  lock := TCriticalSection.Create;

  FFilter := TcbfCBFilter.Create(nil);
  FFilter.ProcessFailedRequests := False;

  FFilter.OnBeforeCreateFile := cbfFilterBeforeCreateFile;
  FFilter.OnBeforeOpenFile := cbfFilterBeforeOpenFile;
  FFilter.OnAfterEnumerateDirectory := cbfFilterAfterEnumerateDirectory;
  FFilter.OnFilterStop := cbfFilterStop;

  UpdateAllButtons();
  msgListSize := 0;
end;

procedure TFormDirhide.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FFilter);
  FreeAndNil(lock);
end;

procedure TFormDirhide.tmLogTimer(Sender: TObject);
begin
  if FFilter <> nil then
    ViewMessages;
end;

procedure TFormDirhide.btnInstallClick(Sender: TObject);
var
  RebootNeeded: Boolean;
begin
  RebootNeeded := False;
  if not dlgOpen.Execute() then
    Exit;

  try
    RebootNeeded := FFilter.Install(dlgOpen.FileName, ProgramName, '', FakeAltitudeForDebug, 0);
  except
    on E: ECBFSFilter do
    begin
      if (E.Code = ERROR_ACCESS_DENIED) or (E.Code = ERROR_PRIVILEGE_NOT_HELD) then
        MessageDlg('Installation requires administrator rights. Run the app as administrator', mtError, [mbOk], 0)
      else
        MessageDlg('Installation failed.'#13#10 + E.Message, mtError, [mbOk], 0);
    end;
  end;

  UpdateAllButtons();

  if RebootNeeded then
    AskForReboot(True);
end;


procedure TFormDirhide.btnUninstallClick(Sender: TObject);
var
  RebootNeeded: Boolean;
begin
  RebootNeeded := False;
  if not dlgOpen.Execute() then
    Exit;

  try
    RebootNeeded := FFilter.Uninstall(dlgOpen.FileName, ProgramName, '', UNINSTALL_VERSION_CURRENT);
  except
    on E: ECBFSFilter do
    begin
      if (E.Code = ERROR_ACCESS_DENIED) or (E.Code = ERROR_PRIVILEGE_NOT_HELD) then
        MessageDlg('Uninstallation requires administrator rights. Run the app as administrator', mtError, [mbOk], 0)
      else
        MessageDlg('Installation failed.'#13#10 + E.Message, mtError, [mbOk], 0);
      end;
    end;

  UpdateAllButtons();

  if RebootNeeded then
    AskForReboot(False);
end;

procedure TFormDirhide.btnClearClick(Sender: TObject);
begin
  lvwLog.Clear();
end;

procedure TFormDirhide.btnHideClick(Sender: TObject);
var
  Parent: string;
begin
  if FFilter.Active then
  begin
    MessageDlg('Some directory is already hidden. Unhide the previous directory first.', mtError, [mbOk], 0);
    Exit;
  end;

  FFilter.Initialize(ProgramName);

  FPathToHide := ConvertRelativePathToAbsolute(edtPath.Text);

  if FPathToHide = '' then
  begin
    MessageDlg('No directory to hide specified', mtError, [mbOk], 0);
    Exit;
  end;

  if not DirectoryExists(FPathToHide) then
  begin
    MessageDlg('The specified directory does not exist, nothing to hide.', mtError, [mbOk], 0);
    Exit;
  end;

  // strip ending slashes and backslashes
  while (Length(FPathToHide) <> 0) and (FPathToHide[Length(FPathToHide)] in ['/', '\'])  do
    Delete(FPathToHide, Length(FPathToHide), 1);

  Parent := ExtractFileDir(FPathToHide);
  // check it's not the root of a drive
  if Parent = ExtractFileDrive(FPathToHide) then
  begin
    MessageDlg('Cannot hide the root of a drive.', mtError, [mbOk], 0);
    Exit;
  end;

  FOwnNameToHide := ExtractFileName(FPathToHide);

  // Hide directory from the parent's enumerations
  FFilter.AddFilterRule(Parent,
    cbfConstants.ACCESS_NONE,
    cbfConstants.FS_CE_AFTER_ENUMERATE_DIRECTORY,
    cbfConstants.FS_NE_NONE
  );

  // Prevent direct opening of the directory
  FFilter.AddFilterRule(FPathToHide,
    cbfConstants.ACCESS_NONE,
    cbfConstants.FS_CE_BEFORE_OPEN or
    cbfConstants.FS_CE_BEFORE_CREATE,
    cbfConstants.FS_NE_NONE
  );

  // Prevent direct operations on files and subdirectories in the directory
  FFilter.AddFilterRule(FPathToHide + SysUtils.PathDelim + '*.*',
    cbfConstants.ACCESS_NONE,
    cbfConstants.FS_CE_BEFORE_OPEN or
    cbfConstants.FS_CE_BEFORE_CREATE,
    cbfConstants.FS_NE_NONE
  );

  FFilter.StartFilter(30000);
  UpdateAllButtons();
end;


procedure TFormDirhide.btnUnhideClick(Sender: TObject);
begin
  FFilter.DeleteAllFilterRules;
  FFilter.StopFilter(False);

  lock.Enter;
  try
    msgListSize := 0;
  finally
    lock.Leave;
  end;
end;

//
// Event handlers
//

procedure TFormDirhide.cbfFilterStop(Sender: TObject; var ResultCode: Integer);
begin
  if not FClosing then
    UpdateAllButtons;
end;

procedure TFormDirhide.cbfFilterAfterEnumerateDirectory(Sender: TObject;
  const DirectoryName: String;
  Flags: Integer;
  Index: Integer;
  var FileName: String;
  var CreationTime: TDateTime;
  var LastAccessTime: TDateTime;
  var LastWriteTime: TDateTime;
  var ChangeTime: TDateTime;
  var Size: Int64;
  var AllocationSize: Int64;
  var FileId: Int64;
  var Attributes: Integer;
  var Status: Integer;
  var DirectoryContext: Pointer;
  var HandleContext: Pointer;
  var EnumerationContext: Pointer;
  var ProcessRequest: Boolean;
  var ResultCode: Integer);
begin
  // if the filesystem returned the name of the "hidden" directory, then block it
  if CompareText(FileName, FOwnNameToHide) = 0 then
  begin
    ProcessRequest := False;
    AddToLog(Sender, 'AfterEnumerateDirectory', FileName, ERROR_FILE_NOT_FOUND);
  end;
end;

function StringStartsWith(const S, SubS: string): Boolean;
begin
  if (S = '') or (SubS = '') or (Length(S) < Length(SubS)) then
    Result := False
  else
    Result := (AnsiStrLIComp(PChar(S), PChar(SubS), Length(SubS)) = 0)
end;

procedure TFormDirhide.cbfFilterBeforeCreateFile(Sender: TObject;
  const FileName: String;
  ExistingAttributes: Integer;
  var Isolate: Boolean;
  var BackendFileName: String;
  var DesiredAccess: Integer;
  var Attributes: Integer;
  var ShareMode: Integer;
  var Options: Integer;
  var CreateDisposition: Integer;
  SecurityDescriptor: Pointer;
  Length: Integer;
  var FileContext: Pointer;
  var HandleContext: Pointer;
  var ProcessRequest: Boolean;
  var ResultCode: Integer);
var
  Dir: string;
begin
  // block attempts to create a new directory with the same name as the "hidden" one
  if CompareText(FileName, FPathToHide) = 0 then
  begin
    ResultCode := ERROR_INVALID_NAME;
    AddToLog(Sender, 'BeforeCreateFile', FileName, ResultCode);
    Exit;
  end;

  // block attempts to create a file or a subdirectory in the "hidden" directory
  Dir := FPathToHide + SysUtils.PathDelim;
  if StringStartsWith(Dir, FPathToHide) then
  begin
    ResultCode := ERROR_PATH_NOT_FOUND;
    AddToLog(Sender, 'BeforeOpenFile', FileName, ResultCode);
  end;
end;

procedure TFormDirhide.cbfFilterBeforeOpenFile(Sender: TObject;
  const FileName: String;
  ExistingAttributes: Integer;
  var Isolate: Boolean;
  var BackendFileName: String;
  var DesiredAccess: Integer;
  var Attributes: Integer;
  var ShareMode: Integer;
  var Options: Integer;
  var CreateDisposition: Integer;
  SecurityDescriptor: Pointer;
  Length: Integer;
  var FileContext: Pointer;
  var HandleContext: Pointer;
  var ProcessRequest: Boolean;
  var ResultCode: Integer);
var
  Dir: string;
begin
   // block attempts to open the "hidden" directory itself
  if AnsiSameText(FileName, FPathToHide) then
  begin
    ResultCode := ERROR_FILE_NOT_FOUND;
    AddToLog(Sender, 'BeforeOpenFile', FileName, ResultCode);
    Exit;
  end;

  // block attempts to open files and subdirectories in the "hidden" directory
  Dir := FPathToHide + SysUtils.PathDelim;
  if StringStartsWith(Dir, FPathToHide) then
  begin
    ResultCode := ERROR_PATH_NOT_FOUND;
    AddToLog(Sender, 'BeforeOpenFile', FileName, ResultCode);
  end;
end;

end.




