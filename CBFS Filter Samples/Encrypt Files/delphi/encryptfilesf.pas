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
unit encryptfilesf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, IOUtils, RegularExpressions,
  WinSvc, StdCtrls, SyncObjs, cbfconstants, cbfcore, cbfcbfilter, ExtCtrls;

type
  TFormEncryptFiles = class(TForm)
    dlgOpen: TOpenDialog;
    tmLog: TTimer;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    lblDrvStatus: TLabel;
    btnInstall: TButton;
    btnUninstall: TButton;
    GroupBox2: TGroupBox;
    lblPath: TLabel;
    btnSetFilter: TButton;
    memLog: TMemo;
    btnDeleteFilter: TButton;
    edtPath: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure btnInstallClick(Sender: TObject);
    procedure btnUninstallClick(Sender: TObject);
    procedure btnSetFilterClick(Sender: TObject);
    procedure btnDeleteFilterClick(Sender: TObject);
    procedure tmLogTimer(Sender: TObject);

  private
    procedure AskForReboot(isInstall: Boolean);
    procedure UpdateAllButtons;

    procedure AddToLog(Value: String);
    procedure ViewMessages;
  private
    cbfFilter: TcbfCBFilter;
    Closing: boolean;

    msgList: TStringList;
    lock: TCriticalSection;

    procedure CBFSFilterBeforeCanFileBeDeleted(Sender: TObject;
      const FileName: String;
      RequestType: Integer;
      var CanDelete: Boolean;
      var FileContext: Pointer;
      var HandleContext: Pointer;
      var ProcessRequest: Boolean;
      var ResultCode: Integer);
    procedure CBFSFilterCloseEnumerationC(Sender: TObject;
      const DirectoryName: String;
      DirectoryContext: Pointer;
      HandleContext: Pointer;
      EnumerationContext: Pointer;
      var ResultCode: Integer);
    procedure CBFSFilterBeforeCloseFile(Sender: TObject;
      const FileName: String; var FileContext: Pointer; var HandleContext: Pointer; var ResultCode: Integer);
    procedure CBFSFilterBeforeCreateFile(Sender: TObject;
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
    procedure CBFSFilterAfterEnumerateDirectory(Sender: TObject;
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
    procedure CBFSFilterBeforeOpenFile(Sender: TObject;
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
    procedure CBFSFilterAfterCreateFile(Sender: TObject;
      const FileName: String;
      ExistingAttributes: Integer;
      Isolate: Boolean;
      const BackendFileName: String;
      DesiredAccess: Integer;
      Attributes: Integer;
      ShareMode: Integer;
      Options: Integer;
      CreateDisposition: Integer;
      SecurityDescriptor: Pointer;
      Length: Integer;
      var Status: Integer;
      var FileContext: Pointer;
      var HandleContext: Pointer;
      var ResultCode: Integer);
    procedure CBFSFilterAfterOpenFile(Sender: TObject;
      const FileName: String;
      ExistingAttributes: Integer;
      Isolate: Boolean;
      const BackendFileName: String;
      DesiredAccess: Integer;
      Attributes: Integer;
      ShareMode: Integer;
      Options: Integer;
      CreateDisposition: Integer;
      SecurityDescriptor: Pointer;
      Length: Integer;
      var Status: Integer;
      var FileContext: Pointer;
      var HandleContext: Pointer;
      var ResultCode: Integer);
    procedure CBFSFilterBeforeRenameOrMoveFile(Sender: TObject;
      const FileName: String; const NewFileName: String; var ReplaceIfExists: Boolean;
      var FileContext: Pointer; var HandleContext: Pointer; var ProcessRequest: Boolean;
      var ResultCode: Integer);
    procedure CBFSFilterBeforeSetAllocationSize(Sender: TObject;
      const FileName: String; var AllocationSize: Int64; var FileContext: Pointer;
      var HandleContext: Pointer; var ProcessRequest: Boolean; var ResultCode: Integer);
    procedure CBFSFilterBeforeSetFileSize(Sender: TObject;
      const FileName: String; var Size: Int64; var FileContext: Pointer;
      var HandleContext: Pointer; var ProcessRequest: Boolean; var ResultCode: Integer);
    procedure CBFSFilterBeforeSetFileAttributes(Sender: TObject;
      const FileName: String; var CreationTime: TDateTime; var LastAccessTime: TDateTime;
      var LastWriteTime: TDateTime; var ChangeTime: TDateTime; var FileAttributes: Integer; var FileContext: Pointer;
      var HandleContext: Pointer; var ProcessRequest: Boolean; var ResultCode: Integer);
    procedure CBFSFilterAfterReadFile(Sender: TObject;
      const FileName: String;
      Position: Int64;
      Buffer: Pointer;
      BufferLength: Integer;
      BytesToRead: Integer;
      Reserved: Integer;
      Direction: Integer;
      var BytesRead: Integer;
      var Status: Integer;
      var FileContext: Pointer;
      var HandleContext: Pointer;
      var ResultCode: Integer);
    procedure CBFSFilterBeforeWriteFile(Sender: TObject;
      const FileName: String;
      var Position: Int64;
      Buffer: Pointer;
      BufferLength: Integer;
      var BytesToWrite: Integer;
      var Reserved: Integer;
      Direction: Integer;
      var FileContext: Pointer;
      var HandleContext: Pointer;
      var ProcessRequest: Boolean;
      var ResultCode: Integer);
    procedure CBFSFilterStop(Sender: TObject; var ResultCode: Integer);

    { Private declarations }
  public
    { Public declarations }
    function ConvertRelativePathToAbsolute(const path: string): string;
    function IsDriveLetter(const path: string): Boolean;
  end;

var
  FormEncryptFiles: TFormEncryptFiles;
const
  FGuid = '{713CC6CE-B3E2-4fd9-838D-E28F558F6866}';
   ALTITUDE_FAKE_VALUE_FOR_DEBUG = '149995';

implementation

{$R *.DFM}

uses
  Math;

type
  TEncryptFilesContext = class
  private
    FFilter: TcbfCBFilter;
    FHandle: THandle;
    FBufferSize: Cardinal;
    FBuffer: PAnsiChar;
    FCurrentSize: Int64;
    FRefCnt: Integer;
    FSectorSize: Word;
    FInitialized: Boolean;
  public
    constructor Create(Filter: TcbfCBFilter; FileName: string;
      NonFiltered: Boolean);
    destructor Destroy; override;

    procedure MoveFile(FileName, NewFileName: string; Decrypt, Encrypt: Boolean);
    procedure EncryptDecryptBuffer(Buffer: PAnsiChar; BufferSize: Cardinal);
    function IncrementRef: Integer;
    function DecrementRef: Integer;
    procedure CloseFile;
    function OpenFile(FileName: string): Boolean;

    property CurrentSize: Int64 read FCurrentSize write FCurrentSize;
    property ReferenceCounter: Integer read FRefCnt write FRefCnt;
    property Initialized: Boolean read FInitialized;
  end;

procedure TFormEncryptFiles.AskForReboot(isInstall: Boolean);
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

procedure TFormEncryptFiles.AddToLog(Value: String);
begin
  if Closing then
    Exit;

  lock.Acquire;
  try
    msgList.Add(Value);
  finally
    lock.Release;
  end;
end;

procedure TFormEncryptFiles.ViewMessages;
begin
  if Closing then
    exit;

  lock.Acquire;
  try
    if memLog.Lines.Count > 1000 then
      memLog.Clear;

    memLog.Lines.AddStrings(msgList);
    msgList.Clear;
  finally
    lock.Release;
  end;
end;

function TFormEncryptFiles.ConvertRelativePathToAbsolute(const path: string): string;
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

function TFormEncryptFiles.IsDriveLetter(const path: string): Boolean;
begin
  Result := False;

  if (path <> '') and (not path.IsEmpty) then
  begin
    if (path[1] in ['A'..'Z', 'a'..'z']) and (Length(path) = 2) and (path[2] = ':') then
      Result := True;
  end;
end;

procedure TFormEncryptFiles.UpdateAllButtons;
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

  btnUninstall.Enabled := true;
end;

procedure TFormEncryptFiles.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Closing := true;
end;

procedure TFormEncryptFiles.FormCreate(Sender: TObject);
begin
  lock := TCriticalSection.Create;
  //{
  cbfFilter := TcbfCBFilter.Create(Self);

  cbfFilter.OnBeforeCreateFile := CBFSFilterBeforeCreateFile;
  cbfFilter.OnAfterCreateFile := CBFSFilterAfterCreateFile;
  cbfFilter.OnBeforeOpenFile := CBFSFilterBeforeOpenFile;
  cbfFilter.OnAfterOpenFile := CBFSFilterAfterOpenFile;
  cbfFilter.OnBeforeSetAllocationSize := CBFSFilterBeforeSetAllocationSize;
  cbfFilter.OnBeforeSetFileSize := CBFSFilterBeforeSetFileSize;
  cbfFilter.OnBeforeSetFileAttributes := CBFSFilterBeforeSetFileAttributes;
  cbfFilter.OnBeforeCanFileBeDeleted := CBFSFilterBeforeCanFileBeDeleted;
  cbfFilter.OnBeforeRenameOrMoveFile := CBFSFilterBeforeRenameOrMoveFile;
  cbfFilter.OnAfterReadFile := CBFSFilterAfterReadFile;
  cbfFilter.OnBeforeWriteFile := CBFSFilterBeforeWriteFile;
  cbfFilter.OnAfterEnumerateDirectory := CBFSFilterAfterEnumerateDirectory;
  cbfFilter.OnAfterCloseEnumeration := CBFSFilterCloseEnumerationC;
  cbfFilter.OnBeforeCloseFile := CBFSFilterBeforeCloseFile;
  cbfFilter.OnFilterStop := CBFSFilterStop;
  //}
  UpdateAllButtons;

  msgList := TStringList.Create;
end;

procedure TFormEncryptFiles.btnInstallClick(Sender: TObject);
var
  RebootNeeded: Boolean;
begin
  RebootNeeded := false;
  if dlgOpen.Execute then
  begin
    try
      RebootNeeded := cbfFilter.Install(dlgOpen.FileName, FGuid, '', ALTITUDE_FAKE_VALUE_FOR_DEBUG, 0);
    except on E: ECBFSFilter do
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

procedure TFormEncryptFiles.btnUninstallClick(Sender: TObject);
var
  RebootNeeded: Boolean;
begin
  RebootNeeded := false;
  if dlgOpen.Execute then
  begin
    try
      RebootNeeded := cbfFilter.Uninstall(dlgOpen.FileName, FGuid, '', UNINSTALL_VERSION_CURRENT);
    except on E: ECBFSFilter do
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

procedure TFormEncryptFiles.btnSetFilterClick(Sender: TObject);
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
    cbfConstants.ACCESS_NONE,
    cbfConstants.FS_CE_AFTER_READ or
    cbfConstants.FS_CE_BEFORE_WRITE or
    cbfConstants.FS_CE_BEFORE_CREATE or
    cbfConstants.FS_CE_AFTER_CREATE or
    cbfConstants.FS_CE_BEFORE_RENAME or
    cbfConstants.FS_CE_BEFORE_SET_SIZES or
    cbfConstants.FS_CE_BEFORE_DELETE or
    cbfConstants.FS_CE_BEFORE_SET_ATTRIBUTES or
    cbfConstants.FS_CE_BEFORE_OPEN or
    cbfConstants.FS_CE_AFTER_OPEN or
    cbfConstants.FS_CE_BEFORE_CLOSE,
    cbfConstants.FS_NE_NONE
  );
  cbfFilter.AddFilterRule(Path + '\*.*',
    cbfConstants.ACCESS_NONE,
    cbfConstants.FS_CE_AFTER_READ or
    cbfConstants.FS_CE_BEFORE_WRITE or
    cbfConstants.FS_CE_BEFORE_CREATE or
    cbfConstants.FS_CE_AFTER_CREATE or
    cbfConstants.FS_CE_BEFORE_RENAME or
    cbfConstants.FS_CE_BEFORE_SET_SIZES or
    cbfConstants.FS_CE_BEFORE_DELETE or
    cbfConstants.FS_CE_BEFORE_SET_ATTRIBUTES or
    cbfConstants.FS_CE_BEFORE_OPEN or
    cbfConstants.FS_CE_AFTER_OPEN or
    cbfConstants.FS_CE_BEFORE_CLOSE,
    cbfConstants.FS_NE_NONE
  );
  try
    cbfFilter.Initialize(FGuid);
    cbfFilter.Config('AllowFileAccessInBeforeOpen=false');
    cbfFilter.Config('ModifiableReadWriteBuffers=true');
    cbfFilter.StartFilter(30000);
    cbfFilter.FileFlushingBehavior := cbfConstants.FS_SUPPORT_FILE_ENCRYPTION;
  except
    on E: ECBFSFilter do
    Dialogs.MessageDlg(E.Message, mtError, [mbOk], 0);
  end;

  UpdateAllButtons;
end;

procedure TFormEncryptFiles.btnDeleteFilterClick(Sender: TObject);
begin
  cbfFilter.DeleteAllFilterRules;
  cbfFilter.StopFilter(false);
end;

procedure TFormEncryptFiles.FormDestroy(Sender: TObject);
begin
  Closing := true;
  FreeAndNil(cbfFilter);
  FreeAndNil(msgList);
  FreeAndNil(lock);
end;

procedure TFormEncryptFiles.tmLogTimer(Sender: TObject);
begin
  ViewMessages;
end;

// Events

procedure TFormEncryptFiles.CBFSFilterBeforeCreateFile(Sender: TObject;
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
begin
  if (Attributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    AddToLog(Format('BeforeCreateFile %s', [FileName]));
  ProcessRequest := true;
end;

procedure TFormEncryptFiles.CBFSFilterAfterCreateFile(Sender: TObject;
  const FileName: String;
  ExistingAttributes: Integer;
  Isolate: Boolean;
  const BackendFileName: String;
  DesiredAccess: Integer;
  Attributes: Integer;
  ShareMode: Integer;
  Options: Integer;
  CreateDisposition: Integer;
  SecurityDescriptor: Pointer;
  Length: Integer;
  var Status: Integer;
  var FileContext: Pointer;
  var HandleContext: Pointer;
  var ResultCode: Integer);
var
  Context: TEncryptFilesContext;
begin
  Context := nil;
  if FileContext = nil then
  begin
    if (Attributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
      begin
        Context := TEncryptFilesContext.Create(TcbfCBFilter(Sender), FileName, false);
        if Context.Initialized then
          FileContext := Pointer(Context)
        else
          begin
            FreeAndNil(Context);
          end;
      end;
  end
  else
  begin
    Context := TEncryptFilesContext(FileContext);
    //enable this code if you have closed file during rename callback
    Context.OpenFile(FileName);

    Context.IncrementRef;
  end;
  if (Context <> nil) and ((Attributes and FILE_ATTRIBUTE_DIRECTORY) = 0) then
    AddToLog(Format('AfterCreateFile(%d) %s',[Context.FRefCnt, FileName]));
end;

procedure TFormEncryptFiles.CBFSFilterBeforeOpenFile(Sender: TObject;
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
begin
  if (Attributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    AddToLog(Format('BeforeOpenFile %s', [FileName]));
  ProcessRequest := true;
end;

procedure TFormEncryptFiles.CBFSFilterAfterOpenFile(Sender: TObject;
  const FileName: String;
  ExistingAttributes: Integer;
  Isolate: Boolean;
  const BackendFileName: String;
  DesiredAccess: Integer;
  Attributes: Integer;
  ShareMode: Integer;
  Options: Integer;
  CreateDisposition: Integer;
  SecurityDescriptor: Pointer;
  Length: Integer;
  var Status: Integer;
  var FileContext: Pointer;
  var HandleContext: Pointer;
  var ResultCode: Integer);
var
  Context: TEncryptFilesContext;
begin
  Context := nil;
  if FileContext = nil then
  begin
    if (Attributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
      begin
        Context := TEncryptFilesContext.Create(TcbfCBFilter(Sender), FileName, false);
        if Context.Initialized then
          FileContext := Pointer(Context)
        else
          begin
            FreeAndNil(Context);
          end;
      end;
  end
  else
  begin
    Context := TEncryptFilesContext(FileContext);
    //enable this code if you have closed file during rename callback
    Context.OpenFile(FileName);

    Context.IncrementRef;
  end;
  if (Context <> nil) and ((Attributes and FILE_ATTRIBUTE_DIRECTORY) = 0) then
    AddToLog(Format('AfterOpenFile(%d) %s', [Context.FRefCnt,FileName]));
end;

procedure TFormEncryptFiles.CBFSFilterBeforeCloseFile(Sender: TObject;
  const FileName: String; var FileContext: Pointer; var HandleContext: Pointer; var ResultCode: Integer);
var
  Context: TEncryptFilesContext;
  RefCnt: Integer;
begin
  Context := TEncryptFilesContext(FileContext);
  RefCnt := 0;
  if Context <> nil then
    RefCnt := Context.FRefCnt;
  AddToLog(Format('BeforeCloseFile(%d) %s', [RefCnt,FileName]));
  if (Context <> nil) and (Context.DecrementRef = 0) then
  begin
    Context.Free;
    FileContext := nil;
  end;
end;

procedure TFormEncryptFiles.CBFSFilterAfterReadFile(Sender: TObject;
  const FileName: String;
  Position: Int64;
  Buffer: Pointer;
  BufferLength: Integer;
  BytesToRead: Integer;
  Reserved: Integer;
  Direction: Integer;
  var BytesRead: Integer;
  var Status: Integer;
  var FileContext: Pointer;
  var HandleContext: Pointer;
  var ResultCode: Integer);
var
  Context: TEncryptFilesContext;
begin
  Context := TEncryptFilesContext(FileContext);
  if (Context <> nil) and (Direction <> cbfConstants.FS_REQUEST_DIR_USER_CACHED) and (Direction <> cbfConstants.FS_REQUEST_DIR_SYSTEM_CACHED) then
  begin
    AddToLog(Format('AfterReadFile %s, FileHandle = %x', [FileName, Context.FHandle]));
    Context.EncryptDecryptBuffer(Buffer, BytesRead);
  end;
end;

procedure TFormEncryptFiles.CBFSFilterStop(Sender: TObject; var ResultCode: Integer);
begin
  if not Closing then
    UpdateAllButtons;
end;

procedure TFormEncryptFiles.CBFSFilterBeforeWriteFile(Sender: TObject;
  const FileName: String;
  var Position: Int64;
  Buffer: Pointer;
  BufferLength: Integer;
  var BytesToWrite: Integer;
  var Reserved: Integer;
  Direction: Integer;
  var FileContext: Pointer;
  var HandleContext: Pointer;
  var ProcessRequest: Boolean;
  var ResultCode: Integer);
var
  Context: TEncryptFilesContext;
begin
  if (Direction = cbfConstants.FS_REQUEST_DIR_USER_CACHED) or (Direction = cbfConstants.FS_REQUEST_DIR_SYSTEM_CACHED) then
	  Exit;
  AddToLog(Format('BeforeWriteFile %s', [FileName]));
  Context := TEncryptFilesContext(FileContext);
  if Context <> nil then
    Context.EncryptDecryptBuffer(Buffer, BytesToWrite);
end;

procedure TFormEncryptFiles.CBFSFilterBeforeCanFileBeDeleted(Sender: TObject;
  const FileName: String;
  RequestType: Integer;
  var CanDelete: Boolean;
  var FileContext: Pointer;
  var HandleContext: Pointer;
  var ProcessRequest: Boolean;
  var ResultCode: Integer);
begin
  AddToLog(Format('BeforeCanFileBeDeleted %s', [FileName]));
  ProcessRequest := true;
end;

procedure TFormEncryptFiles.CBFSFilterCloseEnumerationC(Sender: TObject;
  const DirectoryName: String;
  DirectoryContext: Pointer;
  HandleContext: Pointer;
  EnumerationContext: Pointer;
  var ResultCode: Integer);
begin
  AddToLog(Format('CloseEnumeration %s', [DirectoryName]));
end;

procedure TFormEncryptFiles.CBFSFilterAfterEnumerateDirectory(Sender: TObject;
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
  //AddToLog(Format('AfterEnumerateDirectory %s %s', [DirectoryName, FileName]));
  ProcessRequest := true;
end;

procedure TFormEncryptFiles.CBFSFilterBeforeRenameOrMoveFile(Sender: TObject;
  const FileName: String; const NewFileName: String; var ReplaceIfExists: Boolean;
  var FileContext: Pointer; var HandleContext: Pointer; var ProcessRequest: Boolean;
  var ResultCode: Integer);
var
  Context: TEncryptFilesContext;
  SrcFiltered, DstFiltered: Boolean;
begin
  ProcessRequest := false;
  SrcFiltered := cbfFilter.IsFileFiltered(FileName);
  DstFiltered := cbfFilter.IsFileFiltered(NewFileName);
  if SrcFiltered xor DstFiltered then
    begin
      if FileContext <> nil then
        Context := TEncryptFilesContext(FileContext)
      else
        begin
          Context := TEncryptFilesContext.Create(TcbfCBFilter(Sender), FileName,
            not SrcFiltered);
          if not Context.Initialized then
            begin
              Context.Free;
              Exit;
            end;
        end;
      try
        Context.MoveFile(FileName, NewFileName, SrcFiltered, DstFiltered);
      finally
        if FileContext = nil then
          Context.Free;
        if not DstFiltered then
          begin
            TEncryptFilesContext(FileContext).Free;
            FileContext := nil;
          end;
      end;
    end;
    AddToLog(Format('BeforeRenameOrMoveFile %s %s', [FileName, NewFileName]));
    {
    optional - enable this code for network share filtering,
    this would avoid the rename problem
    else if (Context := TEncryptFilesContext(FileContext)) <> nil then
        Context.CloseFile;
    }
  ProcessRequest := true;
end;

procedure TFormEncryptFiles.CBFSFilterBeforeSetAllocationSize(Sender: TObject;
  const FileName: String; var AllocationSize: Int64; var FileContext: Pointer;
  var HandleContext: Pointer; var ProcessRequest: Boolean; var ResultCode: Integer);
begin
  AddToLog(Format('BeforeSetAllocationSize %s %d', [FileName, AllocationSize]));
  ProcessRequest := true;
end;

procedure TFormEncryptFiles.CBFSFilterBeforeSetFileSize(Sender: TObject;
  const FileName: String; var Size: Int64; var FileContext: Pointer;
  var HandleContext: Pointer; var ProcessRequest: Boolean; var ResultCode: Integer);
var
  Context: TEncryptFilesContext;
begin
  AddToLog(Format('BeforeSetFileSize %s %d', [FileName, Size]));
  Context := TEncryptFilesContext(FileContext);
  if Context <> nil then
    Context.CurrentSize := Size;
  ProcessRequest := true;
end;

procedure TFormEncryptFiles.CBFSFilterBeforeSetFileAttributes(Sender: TObject;
  const FileName: String; var CreationTime: TDateTime; var LastAccessTime: TDateTime;
  var LastWriteTime: TDateTime; var ChangeTime: TDateTime; var FileAttributes: Integer; 
  var FileContext: Pointer; var HandleContext: Pointer; 
  var ProcessRequest: Boolean; var ResultCode: Integer);
begin
  AddToLog(Format('BeforeSetFileAttributes %s', [FileName]));
  ProcessRequest := true;
end;

{ TEncryptFilesContext }

constructor TEncryptFilesContext.Create(Filter: TcbfCBFilter; FileName: string;
  NonFiltered: Boolean);
var
  I: Integer;
  RootPath: string;
  SystemInfo: TSystemInfo;
  SectorsPerCluster, BytesPerSector,
  NumberOfFreeClusters, TotalNumberOfClusters: Cardinal;
  size_high: LongWord;
begin
  FRefCnt := 0;
  FBuffer := nil;
  FBufferSize := 0;
  FCurrentSize := 0;
  FInitialized := false;
  FFilter := Filter;
  if NonFiltered then
    FHandle := FFilter.CreateFileDirect(FileName, false, Integer(GENERIC_READ) or GENERIC_WRITE,
      OPEN_EXISTING, FILE_FLAG_NO_BUFFERING, 0, false)
  else
    FHandle := FFilter.CreateFileDirect(FileName, true, 0, 0, 0, 0, false);
  if FHandle = INVALID_HANDLE_VALUE then
    Exit;
  IncrementRef;
  // determaining disk cluster size and memory page size
  RootPath := FileName;
  BytesPerSector := $200;
  SectorsPerCluster := 0;
  NumberOfFreeClusters := 0;
  TotalNumberOfClusters := 0;
  while Length(RootPath) > 0 do
    begin
      if GetDiskFreeSpaceW(PWideChar(RootPath), SectorsPerCluster, BytesPerSector,
        NumberOfFreeClusters, TotalNumberOfClusters) then
        Break;
      I := Length(RootPath);
      if RootPath[I] = '\' then
        Dec(I);
      while (I > 0) and (RootPath[I] <> '\') do
        Dec(I);
      SetLength(RootPath, I);
    end;
  GetSystemInfo(SystemInfo);
  FBufferSize := Max(SystemInfo.dwPageSize,
    SectorsPerCluster * BytesPerSector);
  FSectorSize := BytesPerSector;
  //optional - the more buffer size, the faster read/write callback processing
  FBufferSize := FBufferSize shl 5;

  // allocating buffer for read/write operations
  FBuffer := VirtualAlloc(nil, FBufferSize, MEM_COMMIT, PAGE_READWRITE);
  CurrentSize := GetFileSize(FHandle, @size_high);
  if CurrentSize = INVALID_FILE_SIZE then
    Exit;
  FInitialized := true;
end;

destructor TEncryptFilesContext.Destroy;
begin
  if FBuffer <> nil then
    VirtualFree(FBuffer, FBufferSize, MEM_DECOMMIT);
  if FHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(FHandle);
  FHandle := INVALID_HANDLE_VALUE;
  FRefCnt := 0;
  inherited;
end;

type
  IO_STATUS_BLOCK = record
    {
    case Integer of
    0: (Status: DWORD);
    1: (APointer: Pointer);
    end;
    }
    Status: DWORD;
    Information: PLongWord;
  end;
  PIO_STATUS_BLOCK = ^IO_STATUS_BLOCK;

function NtSetInformationFile(FileHandle: THandle;
  IoStatusBlock: PIO_STATUS_BLOCK;
  FileInformation: Pointer;
  Length: LongWord;
  FileInformationClass: DWORD): DWORD; stdcall; external 'ntdll.dll';

procedure TEncryptFilesContext.MoveFile(FileName, NewFileName: string; Decrypt,
  Encrypt: Boolean);
var
  CurrPos: Int64;
  Completed: Cardinal;
  Overlapped: TOverlapped;
begin
  Completed := 0;
  if FHandle <> INVALID_HANDLE_VALUE then
    begin
      CurrPos := 0;
      while CurrPos < CurrentSize do
        begin
          // reading internal buffer
          FillChar(Overlapped, SizeOf(Overlapped), 0);
          Overlapped.Offset := CurrPos and $FFFFFFFF;
          Overlapped.OffsetHigh := (CurrPos shr 32) and $FFFFFFFF;
          if not ReadFile(FHandle, FBuffer^, FBufferSize, Completed,
            @Overlapped) or (Completed = 0) then
              Break;
            EncryptDecryptBuffer(FBuffer, Completed);
          // writing internal buffer
          FillChar(Overlapped, SizeOf(Overlapped), 0);
          Overlapped.Offset := CurrPos and $FFFFFFFF;
          Overlapped.OffsetHigh := (CurrPos shr 32) and $FFFFFFFF;
          if not WriteFile(FHandle, FBuffer^, Completed, Completed,
            @Overlapped) or (Completed < FBufferSize) then
              Break;
          // preparing to next part of data
          Inc(CurrPos, FBufferSize);
        end;
    end;
end;

procedure TEncryptFilesContext.EncryptDecryptBuffer(Buffer: PAnsiChar; BufferSize: Cardinal);
var
  I: Cardinal;
begin
  for I := 0 to BufferSize - 1 do
    Buffer[I] := AnsiChar(Byte(Buffer[I]) xor $FF);
end;

function TEncryptFilesContext.IncrementRef: Integer;
begin
  Inc(FRefCnt);
  Result := FRefCnt;
end;

function TEncryptFilesContext.DecrementRef: Integer;
begin
  if FRefCnt > 0 then
    Dec(FRefCnt);
  Result := FRefCnt;
end;

procedure TEncryptFilesContext.CloseFile;
begin
  if FHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(FHandle);
  FHandle := INVALID_HANDLE_VALUE;
end;

function TEncryptFilesContext.OpenFile(FileName: string): Boolean;
begin
if FHandle = INVALID_HANDLE_VALUE then
  FHandle := FFilter.CreateFileDirect(FileName, true, 0, 0, 0, 0, false);
  Result := (FHandle <> INVALID_HANDLE_VALUE);
end;

end.




