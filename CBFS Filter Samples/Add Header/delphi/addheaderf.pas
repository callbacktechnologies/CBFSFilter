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
unit addheaderf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  WinSvc, StdCtrls, SyncObjs, IOUtils, RegularExpressions,
  cbfconstants, cbfcore, cbfcbfilter, ExtCtrls;

const
  DEFAULT_HEADER_SIZE = $1000;

var
  BYTES_PER_SECTOR: LongWord  = $200;
  DEFAULT_CLUSTER_SIZE: LongWord = 4096; // this value is specific to the system configuration and must be obtained on initialization

type
  TFormAddHeader = class(TForm)
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
    ButtonClearLog: TButton;
    CheckBoxCached: TCheckBox;
    CheckBoxLogging: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure btnInstallClick(Sender: TObject);
    procedure btnUninstallClick(Sender: TObject);
    procedure btnSetFilterClick(Sender: TObject);
    procedure btnDeleteFilterClick(Sender: TObject);
    procedure btnClearLog(Sender: TObject);

    procedure CheckBoxCachedClick(Sender: TObject);
    procedure CheckBoxLoggingClick(Sender: TObject);
    procedure tmLogTimer(Sender: TObject);

  private
    cbfFilter: TcbfCBFilter;
    Closing: boolean;

    msgList: TStringList;
    lock: TCriticalSection;

    procedure AskForReboot(isInstall: Boolean);
    procedure UpdateAllButtons;

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
    procedure CBFSFilterAfterRenameOrMoveFile(Sender: TObject;
      const FileName: String; const NewFileName: String; var Status: Integer; var FileContext: Pointer;
      var HandleContext: Pointer; var ResultCode: Integer);
    procedure CBFSFilterBeforeSetAllocationSize(Sender: TObject;
      const FileName: String; var AllocationSize: Int64; var FileContext: Pointer;
      var HandleContext: Pointer; var ProcessRequest: Boolean; var ResultCode: Integer);
    procedure CBFSFilterBeforeSetFileSize(Sender: TObject;
      const FileName: String; var Size: Int64; var FileContext: Pointer;
      var HandleContext: Pointer; var ProcessRequest: Boolean; var ResultCode: Integer);
    procedure CBFSFilterAfterGetFileSizes(Sender: TObject;
      const FileName: String; var Size: Int64; var AllocationSize: Int64;
      var Status: Integer; var FileContext: Pointer; var HandleContext: Pointer;
      var ResultCode: Integer);
    procedure CBFSFilterBeforeReadFile(Sender: TObject;
      const FileName: String;
      var Position: Int64;
      Buffer: Pointer;
      BufferLength: Integer;
      var BytesToRead: Integer;
      var Reserved: Integer;
      Direction: Integer;
      var FileContext: Pointer;
      var HandleContext: Pointer;
      var ProcessRequest: Boolean;
      var ResultCode: Integer);
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
  public
    { Public declarations }
    procedure AddToLog(Value: String);
    procedure ViewMessages;
    function ConvertRelativePathToAbsolute(const path: string): string;
    function IsDriveLetter(const path: string): Boolean;
  end;

var
  FormAddHeader: TFormAddHeader;
  g_CacheStateOn: Boolean;
  g_LogEnabled: Boolean;
  g_osvi: OSVERSIONINFO;
const
  FGuid = '{713CC6CE-B3E2-4fd9-838D-E28F558F6866}';
  ALTITUDE_FAKE_VALUE_FOR_DEBUG = '149995';

implementation

{$R *.DFM}

uses
  Math;

const
  MagicString: PAnsiChar = '!!!!!@@@@@@@@EncryptHeader@@@@@@@@@!!!!!';

var
  DefaultHeaderSize: Integer;


type
  TFileHeader = class
  private
    FInitialized: Boolean;
    FHeader: Pointer;
    FHeaderSize: Integer;
  private
    function GetHeaderSize: Integer;
    procedure SetHeaderSize(Value: Integer);
  public
    constructor Create(HeaderSize: Integer);
    destructor Destroy; override;

    procedure Init(Value: Integer); overload;
    procedure Init(FileHandle: THandle); overload;
    function Write(FileHandle: THandle): LongWord;
    function Initialized: Boolean;
    property HeaderSize: Integer read GetHeaderSize write SetHeaderSize;


  end;

type
  TEncryptContext = class
  private
    FFilter: TcbfCBFilter;
    FHandle: THandle;
    FBufferSize: Cardinal;
    FBuffer: PAnsiChar;
    FCurrentSize: Int64;
    FRefCnt: Integer;
    FInitialized: Boolean;
    FOpenedForModification: Boolean;
    FHeader: TFileHeader;
  private
    procedure SetEof(Size: Int64);
    procedure SetEofSync(Size: Int64);
    procedure SetAllocation(Size: Int64);
    function GetHeaderSize: Integer;
    function WriteHeader: Integer;
    function IsHeaderPresent: Boolean;

  public
    constructor Create(Filter: TcbfCBFilter; FileName: string;
      DesiredAccess: Integer; HeaderSize: Integer; NonFiltered: Boolean);
    destructor Destroy; override;

    function EncryptFile: Integer;
    function DecryptFile: Integer;
    procedure EncryptBuffer(Buffer: Pointer; BufferSize: Integer);
    procedure DecryptBuffer(Buffer: Pointer; BufferSize: Integer);
    function IncrementRef: Integer;
    function DecrementRef: Integer;
    procedure CloseFile;
    function OpenFile(FileName: string): Boolean;

    property CurrentSize: Int64 read FCurrentSize write FCurrentSize;
    property HeaderSize: Integer read GetHeaderSize;
    property ReferenceCounter: Integer read FRefCnt write FRefCnt;
    property Initialized: Boolean read FInitialized;
    property HeaderPresent: Boolean read IsHeaderPresent;
    property OpenedForWrite: Boolean read FOpenedForModification;
    property FileHandle: THandle read FHandle;
    property Eof: Int64 write SetEofSync;
    property Allocation: Int64 write SetAllocation;
  end;

  LONG_PTR = NativeInt;
  ULONG_PTR = NativeUInt;

type
  FILE_INFO_BY_HANDLE_CLASS = (
    FileBasicInfo                   = 0,
    FileStandardInfo                = 1,
    FileNameInfo                    = 2,
    FileRenameInfo                  = 3,
    FileDispositionInfo             = 4,
    FileAllocationInfo              = 5,
    FileEndOfFileInfo               = 6,
    FileStreamInfo                  = 7,
    FileCompressionInfo             = 8,
    FileAttributeTagInfo            = 9,
    FileIdBothDirectoryInfo         = 10, // 0xA
    FileIdBothDirectoryRestartInfo  = 11, // 0xB
    FileIoPriorityHintInfo          = 12, // 0xC
    FileRemoteProtocolInfo          = 13, // 0xD
    FileFullDirectoryInfo           = 14, // 0xE
    FileFullDirectoryRestartInfo    = 15, // 0xF
    FileStorageInfo                 = 16, // 0x10
    FileAlignmentInfo               = 17, // 0x11
    FileIdInfo                      = 18, // 0x12
    FileIdExtdDirectoryInfo         = 19, // 0x13
    FileIdExtdDirectoryRestartInfo  = 20, // 0x14
    MaximumFileInfoByHandlesClass);

  FILE_ALLOCATION_INFO = record
    Allocation: LARGE_INTEGER;
  end;

function SetFileInformationByHandle(hFile: THandle;
  FileInformationClass: FILE_INFO_BY_HANDLE_CLASS;
  lpFileInformation: Pointer;
  dwBufferSize: DWord): BOOL; stdcall; external 'kernel32.dll';

function ROUND_UP_PTR(Ptr: Cardinal; Pow2: Cardinal): Cardinal;
begin
  Result := (ULONG_PTR(Ptr) + Pow2 - 1) and (not(LONG_PTR(Pow2) - 1));
end;

function ROUND_UP_SIZE(Value: Cardinal; Pow2: Cardinal): Cardinal;
begin
  Result := (Value + Pow2 - 1) and not(Pow2 - 1);
end;

function ROUND_TO_CLUSTER(Size: Int64): Int64;
begin
  Result := (Size + DEFAULT_CLUSTER_SIZE - 1) and  (not(DEFAULT_CLUSTER_SIZE - 1));
end;

procedure TFormAddHeader.AskForReboot(isInstall: Boolean);
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

procedure TFormAddHeader.AddToLog(Value: String);
begin
  if not g_LogEnabled then exit;
  if Closing then exit;

  lock.Acquire;
  try
    msgList.Add(Value);
  finally
    lock.Release;
  end;
end;

procedure TFormAddHeader.ViewMessages;
begin
  if Closing then
    exit;

  lock.Acquire;
  try
    if memLog.Lines.Count > 5000 then
      memLog.Clear;

    memLog.Lines.AddStrings(msgList);
    msgList.Clear;
  finally
    lock.Release;
  end;
end;

function TFormAddHeader.ConvertRelativePathToAbsolute(const path: string): string;
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
          MessageDlg('Error while converting the path ' + path + ' to absolute path: ' + E.Message, mtError, [mbOk], 0);
          Exit('');
      end;
    end;
  end;
  Result := res;
end;

function TFormAddHeader.IsDriveLetter(const path: string): Boolean;
begin
  Result := False;

  if (path <> '') and (not path.IsEmpty) then
  begin
    if (path[1] in ['A'..'Z', 'a'..'z']) and (Length(path) = 2) and (path[2] = ':') then
      Result := True;
  end;
end;

procedure TFormAddHeader.UpdateAllButtons;
var
  Status: Integer;
  State: string;
  Version: Int64;

begin
  g_CacheStateOn := CheckBoxCached.Checked;
  g_LogEnabled := CheckBoxLogging.Checked;
  btnSetFilter.Enabled := not cbfFilter.Active;
  btnDeleteFilter.Enabled := cbfFilter.Active;
  checkBoxCached.Enabled := not cbfFilter.Active;

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

procedure TFormAddHeader.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Closing := true;
end;

procedure TFormAddHeader.FormCreate(Sender: TObject);
begin
  lock := TCriticalSection.Create;
  //{
  cbfFilter := TcbfCBFilter.Create(Self);

  cbfFilter.Initialize(FGuid);

  cbfFilter.OnBeforeCreateFile := CBFSFilterBeforeCreateFile;
  cbfFilter.OnAfterCreateFile := CBFSFilterAfterCreateFile;
  cbfFilter.OnBeforeOpenFile := CBFSFilterBeforeOpenFile;
  cbfFilter.OnAfterOpenFile := CBFSFilterAfterOpenFile;
  cbfFilter.OnBeforeSetAllocationSize := CBFSFilterBeforeSetAllocationSize;
  cbfFilter.OnBeforeSetFileSize := CBFSFilterBeforeSetFileSize;
  cbfFilter.OnAfterRenameOrMoveFile := CBFSFilterAfterRenameOrMoveFile;
  cbfFilter.OnBeforeReadFile := CBFSFilterBeforeReadFile;
  cbfFilter.OnAfterReadFile := CBFSFilterAfterReadFile;
  cbfFilter.OnBeforeWriteFile := CBFSFilterBeforeWriteFile;
  cbfFilter.OnAfterEnumerateDirectory := CBFSFilterAfterEnumerateDirectory;
  cbfFilter.OnBeforeCloseFile := CBFSFilterBeforeCloseFile;
  cbfFilter.OnAfterGetFileSizes := CBFSFilterAfterGetFileSizes;
  cbfFilter.OnFilterStop := CBFSFilterStop;
  //}
  UpdateAllButtons;

  msgList := TStringList.Create;
end;

procedure TFormAddHeader.btnInstallClick(Sender: TObject);
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

procedure TFormAddHeader.btnUninstallClick(Sender: TObject);
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

procedure TFormAddHeader.btnSetFilterClick(Sender: TObject);
var
  DiskName: string;
  Position: integer;
  SectorsPerCluster, NumberOfFreeClusters, TotalNumberOfClusters: Cardinal;
  FilterPath: string;

begin
  FilterPath := ConvertRelativePathToAbsolute(edtPath.Text);
  if FilterPath = '' then 
  begin
    Dialogs.MessageDlg('Error: Invalid path', mtError, [mbOk], 0);
    Exit;
  end;
  cbfFilter.AddFilterRuleEx(FilterPath,
    '',
    cbfConstants.ACCESS_NONE,
    cbfConstants.FS_CE_AFTER_ENUMERATE_DIRECTORY,
    cbfConstants.FS_NE_NONE,
    -1, -1, FILE_ATTRIBUTE_DIRECTORY, 0
  );

  cbfFilter.AddFilterRuleEx(FilterPath + '\*.*',
    '',
    cbfConstants.ACCESS_NONE,
    cbfConstants.FS_CE_BEFORE_READ or
    cbfConstants.FS_CE_AFTER_READ or
    cbfConstants.FS_CE_BEFORE_WRITE or
    cbfConstants.FS_CE_BEFORE_CREATE or
    cbfConstants.FS_CE_AFTER_CREATE or
    cbfConstants.FS_CE_AFTER_RENAME or
    cbfConstants.FS_CE_BEFORE_SET_SIZES or
    cbfConstants.FS_CE_AFTER_ENUMERATE_DIRECTORY or
    cbfConstants.FS_CE_BEFORE_OPEN or
    cbfConstants.FS_CE_AFTER_OPEN or
    cbfConstants.FS_CE_BEFORE_CLOSE or
    cbfConstants.FS_CE_AFTER_GET_SIZES,
    cbfConstants.FS_NE_NONE,
    -1, -1, 0, FILE_ATTRIBUTE_DIRECTORY
  );
  try
    // determine on disk sector size, in order to use
    // data and buffers properly aligned data size an
    // buffer addresses in ReadFile/WriteFile request,
    // using the handle, obtained with CBFSFilter.CreateFileDirect()
    Position := Pos(FilterPath, '|');
    if Position > 0 then
      DiskName := FilterPath[Position+1] + FilterPath[Position+2] + FilterPath[Position+3]
    else
      DiskName := FilterPath[1] + FilterPath[2] + FilterPath[3];

    if not GetDiskFreeSpace(PChar(DiskName), SectorsPerCluster, BYTES_PER_SECTOR,
        NumberOfFreeClusters, TotalNumberOfClusters) then
      begin
        Dialogs.MessageDlg('Please specify filtered path related to local disk name', mtError, [mbOk], 0);
        Exit;
      end;

	DEFAULT_CLUSTER_SIZE := BYTES_PER_SECTOR * SectorsPerCluster;

    // When encryption is done without a hidden header,
    // you don't need to handle cached read/write operations.
    // Cached operations support should be enabled to support 
    // an additional hidden header in files in the filtered directory
    if g_CacheStateOn = true then
      cbfFilter.ProcessCachedIORequests := true
    else
      cbfFilter.ProcessCachedIORequests := false;

    //
    // Add additional bytes to the write buffer in the corresponding events.
    // They are needed for the hidden file header implementation.
    // This value must be a multiple of the disk sector size
    //
    cbfFilter.AddBytesToWriteBuffer(DEFAULT_HEADER_SIZE);

    cbfFilter.Config('AllowFileAccessInBeforeOpen=false');
    cbfFilter.Config('ModifiableReadWriteBuffers=true');
    cbfFilter.Config('CacheRemoteFilesLocally=true');
    cbfFilter.StartFilter(0);
    cbfFilter.FileFlushingBehavior := cbfConstants.FS_SUPPORT_FILE_ENCRYPTION;

  except
    on E: ECBFSFilter do
    Dialogs.MessageDlg(E.Message, mtError, [mbOk], 0);
  end;

  UpdateAllButtons;
end;

procedure TFormAddHeader.btnDeleteFilterClick(Sender: TObject);
begin
  cbfFilter.DeleteAllFilterRules;
  cbfFilter.StopFilter(false);
end;

procedure TFormAddHeader.btnClearLog(Sender: TObject);
begin
  memLog.Clear;
end;

procedure TFormAddHeader.CheckBoxCachedClick(Sender: TObject);
begin
  g_CacheStateOn := CheckBoxCached.Checked;
end;

procedure TFormAddHeader.CheckBoxLoggingClick(Sender: TObject);
begin
  g_LogEnabled := CheckBoxLogging.Checked;
end;

procedure TFormAddHeader.FormDestroy(Sender: TObject);
begin
  Closing := true;
  FreeAndNil(cbfFilter);
  FreeAndNil(msgList);
  FreeAndNil(lock);
end;

procedure TFormAddHeader.tmLogTimer(Sender: TObject);
begin
  ViewMessages;
end;

// Events

procedure TFormAddHeader.CBFSFilterBeforeCreateFile(Sender: TObject;
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
  //if (FileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
  //  AddToLog(Format('BeforeCreateFile %s', [FileName]));
  ProcessRequest := true;
end;

procedure TFormAddHeader.CBFSFilterAfterCreateFile(Sender: TObject;
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
  Context: TEncryptContext;
  RefCnt: Integer;
begin
  RefCnt := 0;
  if FileContext = nil then
  begin
    if (Attributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin
      Context := TEncryptContext.Create(TcbfCBFilter(Sender),
        FileName, DesiredAccess, DEFAULT_HEADER_SIZE, false);
      RefCnt := Context.ReferenceCounter;
      if Context.Initialized then
      begin
        FileContext := Pointer(Context);
        AddToLog(Format('ContextAlloc(%p)', [Pointer(Context)]));
      end
      else
        Context.Free;
    end;
  end
  else
  begin
    Context := TEncryptContext(FileContext);
    Context.OpenFile(FileName);
    Context.IncrementRef;
  end;
  if (CreateDisposition = CREATE_ALWAYS) or
     (CreateDisposition = TRUNCATE_EXISTING) then
  begin
    Context.CurrentSize :=0;
    Context.WriteHeader;
  end;
  RefCnt := Context.ReferenceCounter;
  //if (FileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
  AddToLog(Format('AfterCreateFile (%d) %s',[RefCnt, FileName]));
end;

procedure TFormAddHeader.CBFSFilterBeforeOpenFile(Sender: TObject;
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
  //if (FileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
  //  AddToLog(Format('BeforeOpenFile %s', [FileName]));
  ProcessRequest := true;
end;

procedure TFormAddHeader.CBFSFilterAfterOpenFile(Sender: TObject;
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
  Context: TEncryptContext;
  RefCnt: Integer;
begin
  RefCnt := 0;
  if FileContext = nil then
  begin
    if (Attributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin
      Context := TEncryptContext.Create(TcbfCBFilter(Sender),
        FileName, DesiredAccess, DEFAULT_HEADER_SIZE, false);
      RefCnt := Context.ReferenceCounter;
      if Context.Initialized then
      begin
        FileContext := Pointer(Context);
        AddToLog(Format('ContextAlloc(%p)', [Pointer(Context)]));
      end
      else
      begin
        Context.Free;
        Exit;
      end;
    end;
  end
  else
  begin
    Context := TEncryptContext(FileContext);
    Context.OpenFile(FileName);
    Context.IncrementRef;
  end;
  if (CreateDisposition = CREATE_ALWAYS) or
     (CreateDisposition = TRUNCATE_EXISTING) then
  begin
    Context.CurrentSize :=0;
    Context.WriteHeader;
  end;
  RefCnt := Context.ReferenceCounter;
  //if (FileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
  AddToLog(Format('AfterOpenFile (%d) %s', [RefCnt, FileName]));
end;

procedure TFormAddHeader.CBFSFilterBeforeCloseFile(Sender: TObject;
  const FileName: String; var FileContext: Pointer; var HandleContext: Pointer; var ResultCode: Integer);
var
  Context: TEncryptContext;
begin
  Context := TEncryptContext(FileContext);
  if Context <> nil then
  begin
    AddToLog(Format('BeforeCloseFile (%d) %s', [Context.FRefCnt,FileName]));
  end;
  if (Context <> nil) and (Context.DecrementRef = 0) then
  begin
    AddToLog(Format('ContextFree(%p)', [Pointer(Context)]));
    Context.Free;
    FileContext := nil;
  end;
end;

procedure TFormAddHeader.CBFSFilterBeforeReadFile(Sender: TObject;
  const FileName: String;
  var Position: Int64;
  Buffer: Pointer;
  BufferLength: Integer;
  var BytesToRead: Integer;
  var Reserved: Integer;
  Direction: Integer;
  var FileContext: Pointer;
  var HandleContext: Pointer;
  var ProcessRequest: Boolean;
  var ResultCode: Integer);
var
  Context: TEncryptContext;
  hsize: LongWord;
  cursize: Int64;
begin
  Context := TEncryptContext(FileContext);
  // this is a case when some other kernel component like an antivirus monitor tries
  // to read file within create/open file request. To prevent encrypted data
  // staying in the cache, we deny such request
  // The thrown error code may vary, you may change it for the tests
	if Context = nil then
  begin
    ResultCode := ERROR_SHARING_VIOLATION;
    Exit;
  end;

  if  Context <> nil then
  begin
    if not Context.HeaderPresent and Context.OpenedForWrite then
      Context.EncryptFile;

    AddToLog(Format('BeforeReadFile %s, FileHandle = %x', [FileName, Context.FHandle]));
    hsize := Context.HeaderSize;
		cursize := Context.CurrentSize;
    if  ((Direction = cbfConstants.FS_REQUEST_DIR_USER_CACHED) or (Direction = cbfConstants.FS_REQUEST_DIR_SYSTEM_CACHED)) and (Position >= (cursize - hsize)) then
    begin
      AddToLog('ECBFSFltError(EOF) !!!!!');
      BytesToRead := 0;
      ResultCode := ERROR_HANDLE_EOF;
      Exit;
    end;
    if (Direction <> cbfConstants.FS_REQUEST_DIR_USER_CACHED) and (Direction <> cbfConstants.FS_REQUEST_DIR_SYSTEM_CACHED) then
    begin
      Inc(Position, hsize);
      AddToLog(Format('POS(%0X) READ(%0X)', [Position, BytesToRead]));
    end
    else//Cached read
    begin
      if (Position + BytesToRead ) > (cursize - hsize) then
        BytesToRead := cursize - Position - hsize;
    end;
  end;
end;

procedure TFormAddHeader.CBFSFilterAfterReadFile(Sender: TObject;
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
  Context: TEncryptContext;
begin
  if (FileContext = nil) then Exit;

  Context := TEncryptContext(FileContext);

  if (Direction <> cbfConstants.FS_REQUEST_DIR_USER_CACHED) and
     (Direction <> cbfConstants.FS_REQUEST_DIR_SYSTEM_CACHED) and
     (Context.HeaderPresent) then
  begin
    Context.DecryptBuffer(Buffer, BytesToRead);
  end;
end;

procedure TFormAddHeader.CBFSFilterBeforeWriteFile(Sender: TObject;
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
  Context: TEncryptContext;
  NewEof: Int64;
begin
  if FileContext = nil then	Exit;
  AddToLog(Format('BeforeWriteFile %s', [FileName]));
  Context := TEncryptContext(FileContext);
  ProcessRequest := true;

  if (Direction = cbfConstants.FS_REQUEST_DIR_USER_CACHED) or (Direction = cbfConstants.FS_REQUEST_DIR_SYSTEM_CACHED) then
  begin
    if not Context.IsHeaderPresent then
      Context.EncryptFile;
    NewEof := Position + BytesToWrite + Context.HeaderSize;
    if (Position + BytesToWrite) >= (Context.CurrentSize - Context.HeaderSize) then
    begin
      Inc(BytesToWrite, Context.HeaderSize);// this step requires a prior call to g_CBFSFlt.AddClustersToWriteBuffer(1)
      Context.CurrentSize := NewEof;
    end;
    Exit;
  end
  else
  begin
    Context.EncryptBuffer(Buffer, BytesToWrite);
    Inc(Position, Context.HeaderSize);
  end;
end;

procedure TFormAddHeader.CBFSFilterAfterEnumerateDirectory(Sender: TObject;
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
  AddToLog(Format('AfterEnumerateDirectory %s %s', [DirectoryName, FileName]));
  // assumed that all files in directory are filtered
  if Size >= DefaultHeaderSize then
    Dec(Size, DefaultHeaderSize);
  ProcessRequest := true;
end;

procedure TFormAddHeader.CBFSFilterAfterRenameOrMoveFile(Sender: TObject;
  const FileName: String; const NewFileName: String; var Status: Integer; var FileContext: Pointer;
  var HandleContext: Pointer; var ResultCode: Integer);
var
  Context: TEncryptContext;
  SrcFiltered, DstFiltered: Boolean;
begin
  AddToLog(Format('AfterRenameOrMoveFile %s %s', [FileName, NewFileName]));
  SrcFiltered := cbfFilter.IsFileFiltered(FileName);
  DstFiltered := cbfFilter.IsFileFiltered(NewFileName);

  if SrcFiltered xor DstFiltered then
  begin
    if FileContext <> nil then
    begin
      Context := TEncryptContext(FileContext);

      if (SrcFiltered And Context.HeaderPresent) Then
        Context.DecryptFile;
      if DstFiltered Then
        Context.EncryptFile;

      if (not DstFiltered And Context.HeaderPresent) then
      begin
        Context.Eof := Context.CurrentSize - DEFAULT_HEADER_SIZE;
        TEncryptContext(FileContext).Free;
        FileContext := nil;
      end;
    end
    else
    begin
      //
      // this is case when file is not in the filtered path and context was already freed.
      // Desired access we specified (FILE_WRITE_DATA) force the filtered file be encrypted in the place.
      //
      Context := TEncryptContext.Create(TcbfCBFilter(Sender), NewFileName,
        FILE_WRITE_DATA, DEFAULT_HEADER_SIZE, false);
      if Context.Initialized then
      begin
        FileContext := Pointer(Context);
        Context.EncryptFile;
      end
      else
        Context.Free;
    end;
  end;
end;

procedure TFormAddHeader.CBFSFilterBeforeSetAllocationSize(Sender: TObject;
  const FileName: String; var AllocationSize: Int64; var FileContext: Pointer;
  var HandleContext: Pointer; var ProcessRequest: Boolean; var ResultCode: Integer);
var
Context: TEncryptContext;
begin
  AddToLog(Format('BeforeSetAllocationSize %s %d', [FileName, AllocationSize]));
  if FileContext <> nil then
  begin
    Context := TEncryptContext(FileContext);
    Inc(AllocationSize, Context.HeaderSize);
    AllocationSize := ROUND_TO_CLUSTER(AllocationSize);
    if Context.CurrentSize > AllocationSize then
      Context.CurrentSize := AllocationSize;
  end;
end;

procedure TFormAddHeader.CBFSFilterBeforeSetFileSize(Sender: TObject;
  const FileName: String; var Size: Int64; var FileContext: Pointer;
  var HandleContext: Pointer; var ProcessRequest: Boolean; var ResultCode: Integer);
var
  Context: TEncryptContext;
begin
  AddToLog(Format('BeforeSetFileSize %s %d', [FileName, Size]));
  Context := TEncryptContext(FileContext);
  if Context <> nil then
  begin
    Context.CurrentSize := Size + Context.HeaderSize;
    Inc(Size, Context.HeaderSize);
  end;
end;

procedure TFormAddHeader.CBFSFilterAfterGetFileSizes(Sender: TObject;
  const FileName: String; var Size: Int64; var AllocationSize: Int64;
  var Status: Integer; var FileContext: Pointer; var HandleContext: Pointer;
  var ResultCode: Integer);
var
  Context: TEncryptContext;
begin
  Context := TEncryptContext(FileContext);
  AddToLog(Format('CBFSFltAfterGetFileSizes %s Size(%d) AllocationSize(%d)',
    [FileName, Size, AllocationSize]));
  if ((Context <> nil) and Context.HeaderPresent ) then
  begin
    Dec(Size, Context.HeaderSize);
    Dec(AllocationSize, Context.HeaderSize);
  end;
end;

procedure TFormAddHeader.CBFSFilterStop(Sender: TObject; var ResultCode: Integer);
begin
  if not Closing then
    UpdateAllButtons;
end;

{ TFileHeader }
constructor TFileHeader.Create(HeaderSize: Integer);
begin
  FHeader := nil;
  FHeaderSize := HeaderSize;
  FInitialized := false;
  if HeaderSize <> 0 then
    FHeader := VirtualAlloc(nil, HeaderSize, MEM_COMMIT, PAGE_READWRITE);
  if FHeaderSize > Length(MagicString) then
    begin
      FillMemory(FHeader, FHeaderSize, 0);
      CopyMemory(FHeader, MagicString, Length(MagicString));
    end;
end;

destructor TFileHeader.Destroy;
begin
  if FHeader <> nil then
    VirtualFree(FHeader, 0, MEM_RELEASE);
  FHeader := nil;
  inherited;
end;

function TFileHeader.GetHeaderSize: Integer;
begin
  if FInitialized = true then
    Result := FHeaderSize
  else
    Result := 0;
end;

procedure TFileHeader.SetHeaderSize(Value: Integer);
begin
  FHeaderSize := Value;
end;

procedure TFileHeader.Init(Value: Integer);
begin
  if FHeaderSize = 0 then
  begin
    FHeader := VirtualAlloc(nil, Value, MEM_COMMIT, PAGE_READWRITE);
    FHeaderSize := Value;
  end;
  if FHeaderSize > Length(MagicString) then
  begin
    ZeroMemory(FHeader, FHeaderSize);
    CopyMemory(FHeader, MagicString, Length(MagicString));
  end;
end;

procedure TFileHeader.Init(FileHandle: THandle);
var
  Overlapped: TOverlapped;
  Completed: LongWord;
begin
  FInitialized := false;
  FillChar(Overlapped, SizeOf(Overlapped), 0);
  if FileHandle = INVALID_HANDLE_VALUE then
    Exit;
  if ReadFile(FileHandle, FHeader^, FHeaderSize, Completed, @Overlapped) and
     (Completed = Cardinal(FHeaderSize)) then
  begin
    if (0 = lstrcmpA(PAnsiChar(FHeader), MagicString)) then
    FInitialized := true;
  end;
  {restore initial header}
  ZeroMemory(FHeader, FHeaderSize);
  CopyMemory(FHeader, MagicString, Length(MagicString));
end;

function TFileHeader.Write(FileHandle: THandle): LongWord;
var
  Overlapped: TOverlapped;
  Completed: LongWord;
begin
  FillChar(Overlapped, SizeOf(Overlapped), 0);
  Completed := 0;
  Result := 0;
  if FileHandle <> INVALID_HANDLE_VALUE then
  begin
    FInitialized := false;
    if WriteFile(FileHandle, FHeader^, FHeaderSize, Completed, @Overlapped) and
      (Completed = Cardinal(FHeaderSize)) then
    begin
      FInitialized := true;
      Result := Completed;
    end;
  end;
end;

function TFileHeader.Initialized: Boolean;
begin
  Result := FInitialized;
end;

{ TEncryptContext }

constructor TEncryptContext.Create(Filter: TcbfCBFilter;
  FileName: string; DesiredAccess: Integer; HeaderSize: Integer; NonFiltered: Boolean);
var
  SystemInfo: TSystemInfo;
  size_high, size_low: LongWord;
  FileSize: Int64;
  Error: LongWord;

begin
  FHeader := TFileHeader.Create(HeaderSize);
  FRefCnt := 1;
  FBuffer := nil;
  FBufferSize := 0;
  FCurrentSize := 0;
  FInitialized := false;
  FOpenedForModification := false;
  FFilter := Filter;
  FHandle := INVALID_HANDLE_VALUE;
  Error := NO_ERROR;

	//
	// uncomment this line of code if your intention to encrypt
	// unencrypted files when they first time opened/created
	//
	// FOpenedForModification := true;

	if ((DesiredAccess and (_DELETE or FILE_WRITE_DATA or FILE_APPEND_DATA or WRITE_DAC or WRITE_OWNER or FILE_WRITE_EA or FILE_WRITE_ATTRIBUTES)) <> 0) then
		FOpenedForModification := true;

  (*TFileHeader.*)DefaultHeaderSize := HeaderSize;
  if NonFiltered then
    FHandle := FFilter.CreateFileDirect(FileName, false,
      Integer(GENERIC_READ) or GENERIC_WRITE, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0, false)
  else
    FHandle := FFilter.CreateFileDirect(FileName, true, 0, 0, 0, 0, false);
  if FHandle = INVALID_HANDLE_VALUE then
  begin
    Error := GetLastError;
    OutputDebugString(PChar(Format('!!!!TEncryptContext.Create Error open file (%d)',[Error])));
    Exit;
  end;
  if not NonFiltered then FHeader.Init(FHandle);

  GetSystemInfo(SystemInfo);
  FBufferSize := SystemInfo.dwPageSize;
  //optional - the more buffer size, the faster read/write callback processing
  FBufferSize := FBufferSize shl 4;

  // allocating buffer for read/write operations
  FBuffer := VirtualAlloc(nil, FBufferSize, MEM_RESERVE, PAGE_READWRITE);
  size_low := GetFileSize(FHandle, @size_high);
  if size_low = INVALID_FILE_SIZE then
  begin
    Error := GetLastError;
    CloseHandle(FHandle);
    FHandle := INVALID_HANDLE_VALUE;
    OutputDebugString(PChar(Format('!!!!!TEncryptContext.Create Error get file size (%d)',[Error])));
    Exit;
  end;

  FileSize := (Int64(size_high) shl 32) or size_low;
  CurrentSize := FileSize;

  if not FHeader.Initialized and (FOpenedForModification = true) then
  begin
    if (FileSize = 0) then
    begin
      Allocation := DEFAULT_CLUSTER_SIZE;
      Eof := HeaderSize;
      if (FHeader.Write(FHandle) <> 0) then
        FileSize := HeaderSize;
    end
    else
    begin
    {
    //
    // uncomment this code if your intention to encrypt
    // unencrypted files when they first time opened,
    // otherwise,
    // it will be encrypted conditionally in
    // read/write callbacks:
    //
    // a) file was created/opened for write access
    // b) reead/write callback event was triggered
    //
      Error := EncryptFile;
    }
    end;
  end;
  if Error = NO_ERROR then
    FInitialized := true;
end;

destructor TEncryptContext.Destroy;
begin
  FHeader.Free;
  if FBuffer <> nil then
    VirtualFree(FBuffer, FBufferSize, MEM_RELEASE);
  CloseHandle(FHandle);
  FHandle := INVALID_HANDLE_VALUE;
  FRefCnt := 0;
  inherited;
end;

function TEncryptContext.EncryptFile: Integer;
var
  Error: LongWord;
  Overlapped: TOverlapped;
  buf1, buf2, rbuf, wbuf: PAnsiChar;
  curPos: Int64;
	buflen1, buflen2: LongWord;
	len1, len2 : PLongWord;
  arrbuf: array[0..1] of PAnsiChar;
  arrsize: array[0..1] of PLongWord;
  i: Integer;
begin
  // we got here non-encrypted file
  // try add header and encrypt it

  // the code below assumes that our intermediate
  // buffer size is greater or equal of additional header size
  //
  Assert(FBufferSize >= DEFAULT_HEADER_SIZE);
  Error := NO_ERROR;

  buf1 := PAnsiChar(VirtualAlloc(nil, FBufferSize, MEM_COMMIT, PAGE_READWRITE));
  buf2 := PAnsiChar(VirtualAlloc(nil, FBufferSize, MEM_COMMIT, PAGE_READWRITE));

  if ((buf2 = nil)  or (buf1 = nil)) then
  begin
    if (buf1 = nil) then
      VirtualFree(buf1, 0, MEM_RELEASE);
    if (buf2 = nil) then
      VirtualFree(buf2, 0, MEM_RELEASE);
    Result := ERROR_OUTOFMEMORY;
    Exit;
  end;

  CurPos := 0; buflen1 := 0; buflen2 := 0;

  FillChar(Overlapped, SizeOf(Overlapped), 0);
  Overlapped.Offset := curPos and $FFFFFFFF;
  Overlapped.OffsetHigh := (curPos shr 32) and $FFFFFFFF;

  // read the first portion of the file
  //
  if not ReadFile(FHandle, buf1^, FBufferSize, &buflen1,
    @Overlapped) or (buflen1 = 0) then
  begin
    Error := GetLastError;
  end;

  arrbuf[0] := buf1;
  arrbuf[1] := buf2;
  arrsize[0] := @buflen1;
  arrsize[1] := @buflen2;

  i := 0;

  while (Error = NO_ERROR) do
  begin
    // hold the data in two buffers switching them.
    // One buffer keeps the data in shifted write position.
    // Another buffer keeps the next data.
    //
    rbuf := arrbuf[(i + 1) and 1];
    wbuf := arrbuf[i and 1];

    len1 := arrsize[(i + 1) and 1];
    len2 := arrsize[i and 1];

    len1^ := 0;

    FillChar(Overlapped, SizeOf(Overlapped), 0);
    Overlapped.Offset := (curPos + FBufferSize) and $FFFFFFFF;
    Overlapped.OffsetHigh := ((curPos + FBufferSize) shr 32) and $FFFFFFFF;

    if not ReadFile(FHandle, rbuf^, FBufferSize, len1^, @Overlapped) or (len1^ = 0) then
    begin
      Error := GetLastError();
      if (Error = ERROR_HANDLE_EOF) then
      begin
        Error := NO_ERROR;
      end;
    end;
    EncryptBuffer(wbuf, len2^);

    if not FHeader.Initialized then
    begin
      // we must extend EOF as a first step,
      // so that write will not failed
      //
      SetAllocation(ROUND_TO_CLUSTER(DEFAULT_HEADER_SIZE + FCurrentSize));
      SetEof(DEFAULT_HEADER_SIZE + FCurrentSize);
      FHeader.Write(FHandle);
    end;

    FillChar(Overlapped, SizeOf(Overlapped), 0);
    Overlapped.Offset := (curPos + DEFAULT_HEADER_SIZE) and $FFFFFFFF;
    Overlapped.OffsetHigh := ((curPos + DEFAULT_HEADER_SIZE) shr 32) and $FFFFFFFF;

    // write must succeed, as we move EOF to the new value
    //
    if not WriteFile(FHandle, wbuf^, FBufferSize, len2^, @Overlapped) then
    begin
      Error := GetLastError;
      break;
    end;

    Inc(CurPos, FBufferSize);
    Inc(i);

    if (len2^ < FBufferSize) then
      break;
    if (len1^ = 0) then
      break;
    if (len1^ < FBufferSize) then
    begin
      //
      // write the last portion of file
      //
      EncryptBuffer(rbuf, len1^);

      FillChar(Overlapped, SizeOf(Overlapped), 0);
      Overlapped.Offset := (curPos + DEFAULT_HEADER_SIZE) and $FFFFFFFF;
      Overlapped.OffsetHigh := ((curPos + DEFAULT_HEADER_SIZE) shr 32) and $FFFFFFFF;

      if not WriteFile(FHandle, rbuf^, FBufferSize, len1^, @Overlapped) then
      begin
        Error := GetLastError;
      end;
      break;
    end
  end;
  VirtualFree(buf1, 0, MEM_RELEASE);
  VirtualFree(buf2, 0, MEM_RELEASE);
  Result := Error;
end;

function TEncryptContext.DecryptFile: Integer;
var
  CurrPos, CurrSize: Int64;
  Completed, LastError: Cardinal;
  Overlapped: TOverlapped;
  BytesToRead: Integer;
begin
  Completed := 0;
  if FHandle <> INVALID_HANDLE_VALUE then
  begin
    CurrPos := 0;
    CurrSize := CurrentSize;
    VirtualAlloc(FBuffer, FBufferSize, MEM_COMMIT, PAGE_READWRITE);
    BytesToRead := FBufferSize;
    while CurrPos < (CurrSize - HeaderSize) do
    begin
      // reading internal buffer
      FillChar(Overlapped, SizeOf(Overlapped), 0);
      Overlapped.Offset := CurrPos and $FFFFFFFF;
      Overlapped.OffsetHigh := (CurrPos shr 32) and $FFFFFFFF;
      Inc(Overlapped.Offset, HeaderSize);
      if not ReadFile(FHandle, FBuffer^, BytesToRead, Completed,
        @Overlapped) or (Completed = 0) then
      begin
        LastError := GetLastError;
        OutputDebugString(PChar(Format('!!!!!TEncryptContext.DecryptFile ReadFile Error(%d)',[LastError])));
        Break;
      end;
      DecryptBuffer(FBuffer, Completed);
      // writing internal buffer
      FillChar(Overlapped, SizeOf(Overlapped), 0);
      Overlapped.Offset := CurrPos and $FFFFFFFF;
      Overlapped.OffsetHigh := (CurrPos shr 32) and $FFFFFFFF;
      if not WriteFile(FHandle,
        FBuffer^, FBufferSize, Completed, @Overlapped) then
      begin
        LastError := GetLastError;
        OutputDebugString(PChar(Format('!!!!!TEncryptContext.DecryptFile WriteFile Error(%d)',[LastError])));
        Break;
      end;
      // preparing to next part of data
      Inc(CurrPos, Min(Completed, FBufferSize));
    end;
    VirtualFree(FBuffer, 0, MEM_RELEASE);
  end;
  Result := 0;
end;

procedure TEncryptContext.DecryptBuffer(Buffer: Pointer; BufferSize: Integer);
var
  I: Cardinal;
  buf: PByte;
begin
  buf := PByte(Buffer);
  for I := 0 to BufferSize - 1 do
    PByteArray(buf)[I] := Byte(PByteArray(buf)[I]) xor $FF;
end;

procedure TEncryptContext.EncryptBuffer(Buffer: Pointer; BufferSize: Integer);
var
  I: Cardinal;
  buf: PByte;
begin
  buf := PByte(Buffer);
  for I := 0 to BufferSize - 1 do
    PByteArray(buf)[I] := Byte(PByteArray(buf)[I]) xor $FF;
end;

function TEncryptContext.IncrementRef: Integer;
begin
  InterlockedIncrement(FRefCnt);
  Result := FRefCnt;
end;

function TEncryptContext.DecrementRef: Integer;
begin
  InterlockedDecrement(FRefCnt);
  Result := FRefCnt;
end;

procedure TEncryptContext.CloseFile;
begin
  CloseHandle(FHandle);
  FHandle := INVALID_HANDLE_VALUE;
end;

function TEncryptContext.OpenFile(FileName: string): Boolean;
begin
  if FHandle = INVALID_HANDLE_VALUE then
    FHandle := FFilter.CreateFileDirect(FileName, true, 0, 0, 0, 0, false);
  Result := FHandle <> INVALID_HANDLE_VALUE;
end;

function TEncryptContext.WriteHeader: Integer;
begin
  if FCurrentSize < DEFAULT_HEADER_SIZE then
    Eof := FCurrentSize + DEFAULT_HEADER_SIZE;
  Result := FHeader.Write(FHandle);
end;

procedure TEncryptContext.SetEof(Size: Int64);
var
  Handle: THandle;
begin
  if FFilter.SetFileSizeDirect(FHandle, Size) then
  CurrentSize := Size;
end;

procedure TEncryptContext.SetEofSync(Size: Int64);
begin
  SetEof(Size);
end;

procedure TEncryptContext.SetAllocation(Size: Int64);
var
  Info: FILE_ALLOCATION_INFO;
begin
	Info.Allocation.QuadPart := Size;
  SetFileInformationByHandle(FHandle, FileAllocationInfo, @Info, sizeof(FILE_ALLOCATION_INFO));
end;

function TEncryptContext.GetHeaderSize: Integer;
begin
  Result := FHeader.HeaderSize;
end;

function TEncryptContext.IsHeaderPresent: Boolean;
begin
  Result := FHeader.Initialized;
end;

end.


