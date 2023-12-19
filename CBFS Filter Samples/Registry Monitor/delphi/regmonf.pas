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
unit regmonf;

interface

uses
  Windows, WinSvc, Messages, Math, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls,
  cbfconstants, cbfcbregistry;

const
  WM_LOG = WM_USER + 1;

type
  TFormRegmon = class(TForm)
    grpDriver: TGroupBox;
    grpProcess: TGroupBox;
    btnInstall: TButton;
    lblStatus: TLabel;
    btnUninstall: TButton;
    grpLog: TGroupBox;
    edtFilename: TEdit;
    btnSelectFile: TButton;
    lvwLog: TListView;
    btnStart: TButton;
    btnStop: TButton;
    lblFilename: TLabel;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnInstallClick(Sender: TObject);
    procedure btnUninstallClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnSelectFileClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FFilter: TcbfCBRegistry;
    FDriverRunning: Boolean;
    FFilterWorking: Boolean;
    FItemsCount: Integer;
    FTicksStarted: Int64;
	FClosing: boolean;
    NoLog : boolean;
    procedure AskForReboot(IsInstall : boolean);
    function CreateFilter(): TcbfCBRegistry;
    function CreateOpenCabFileDialog(): TOpenDialog;
    function CreateOpenExeFileDialog(): TOpenDialog;
    procedure Log(const Operation, Name: string; Status: Integer; const Details: string);
    procedure UpdateControls();
    procedure UpdateDriverStatus(Filter: TcbfCBRegistry);
    procedure WMLog(var Msg: TMessage); message WM_LOG;

    procedure CleanupKeyContextHandler(Sender: TObject; KeyContext: Pointer; var ResultCode: Integer);

    procedure BeforeCreateKeyHandler(Sender: TObject; const FullName: String; DesiredAccess: Integer;
      var KeyHandle: Int64; var KeyHandleContext: Pointer; var GrantedAccess: Int64; var KeyContext: Pointer;
      var StopFiltering: Boolean; var ResultCode: Integer);
    procedure BeforeOpenKeyHandler(Sender: TObject; const FullName: string; DesiredAccess: Integer;
      var KeyHandle: Int64; var KeyHandleContext: Pointer; var GrantedAccess: Integer; var KeyContext: Pointer;
      var StopFiltering: Boolean; var ResultCode: Integer);
    procedure BeforeRenameKeyHandler(Sender: TObject; KeyContext: Pointer; const NewName: string;
      var Processed: Boolean; var FireAfterEvent: Boolean; var StopFiltering: Boolean; var ResultCode: Integer);
    procedure BeforeSetKeyHandler(Sender: TObject; KeyContext: Pointer; LastWriteTime: TDateTime;
      var Processed: Boolean; var FireAfterEvent: Boolean; var StopFiltering: Boolean; var ResultCode: Integer);

    procedure AfterCloseKeyHandler(Sender: TObject; KeyContext: Pointer; var ResultCode: Integer);
    procedure AfterCreateKeyHandler(Sender: TObject; KeyContext: Pointer; var Status: Integer; var KeyHandle: Int64;
      var KeyHandleContext: Pointer; var GrantedAccess: Integer; var StopFiltering: Boolean; var ResultCode: Integer);
    procedure AfterDeleteKeyHandler(Sender: TObject; KeyContext: Pointer; var Status: Integer;
      var Processed: Boolean; var StopFiltering: Boolean; var ResultCode: Integer);
    procedure AfterEnumerateKeyHandler(Sender: TObject; KeyContext: Pointer; var Status: Integer; Index: Integer;
      ValidFields: Integer; var LastWriteTime: TDateTime; var Name: string; var ClassName: string;
      var SubKeys: Integer; var MaxNameLength: Integer; var MaxClassNameLength: Integer; var Values: Integer;
      var MaxValueNameLength: Integer; var MaxValueDataSize: Integer; var VirtualizationCandidate: Boolean;
      var VirtualizationEnabled: Boolean; var VirtualTarget: Boolean; var VirtualStore: Boolean;
      var VirtualSource: Boolean; var Processed: Boolean; var StopFiltering: Boolean; var ResultCode: Integer);
    procedure AfterOpenKeyHandler(Sender: TObject; KeyContext: Pointer; var Status: Integer; var KeyHandle: Int64;
      var KeyHandleContext: Pointer; var GrantedAccess: Integer; var StopFiltering: Boolean; var ResultCode: Integer);
    procedure AfterQueryKeyHandler(Sender: TObject; KeyContext: Pointer; var Status: Integer; ValidFields: Integer;
      var LastWriteTime: TDateTime; var Name: string; var ClassName: string; var SubKeys: Integer;
      var MaxNameLength: Integer; var MaxClassNameLength: Integer; var Values: Integer;
      var MaxValueNameLength: Integer; var MaxValueDataSize: Integer; var VirtualizationCandidate: Boolean;
      var VirtualizationEnabled: Boolean; var VirtualTarget: Boolean; var VirtualStore: Boolean;
      var VirtualSource: Boolean; var Processed: Boolean; var StopFiltering: Boolean; var ResultCode: Integer);
    procedure AfterRenameKeyHandler(Sender: TObject; KeyContext: Pointer; var Status: Integer;
      var Processed: Boolean; var StopFiltering: Boolean; var ResultCode: Integer);
    procedure AfterSetKeyHandler(Sender: TObject; KeyContext: Pointer; var Status: Integer; var Processed: Boolean;
      var StopFiltering: Boolean; var ResultCode: Integer);

    procedure BeforeDeleteValueHandler(Sender: TObject; KeyContext: Pointer; const ValueName: string;
      var Processed: Boolean; var CallPostEvent: Boolean; var StopFiltering: Boolean; var ResultCode: Integer);
    procedure BeforeSetValueHandler(Sender: TObject; KeyContext: Pointer; const ValueName: string; ValueType: Integer;
      IntegerValue: Int64; const StringValue: string; BinaryValue: Pointer; BinaryValueSize: Integer;
      var Processed: Boolean; var CallPostEvent: Boolean; var StopFiltering: Boolean; var ResultCode: Integer);

    procedure AfterDeleteValueHandler(Sender: TObject; KeyContext: Pointer; var Status: Integer;
      var Processed: Boolean; var StopFiltering: Boolean; var ResultCode: Integer);
    procedure AfterEnumerateValueHandler(Sender: TObject; KeyContext: Pointer; var Status: Integer;
      Index: Integer; ValidFields: Integer; var ValueName: string; var ValueType: Integer; var IntegerValue: Int64;
      var StringValue: string; BinaryValue: Pointer; MaxBinaryValueSize: Integer; var BinaryValueSize: Integer;
      var Processed: Boolean; var StopFiltering: Boolean; var ResultCode: Integer);
    procedure AfterQueryValueHandler(Sender: TObject; KeyContext: Pointer; var Status: Integer;
      const ValueName: string; ValidFields: Integer; var ValueType: Integer; var IntegerValue: Int64;
      var StringValue: string; BinaryValue: Pointer; MaxBinaryValueSize: Integer; var BinaryValueSize: Integer;
      var Processed: Boolean; var StopFiltering: Boolean; var ResultCode: Integer);
    procedure AfterSetValueHandler(Sender: TObject; KeyContext: Pointer; var Status: Integer;
      var Processed: Boolean; var StopFiltering: Boolean; var ResultCode: Integer);
  public
    { Public declarations }
  end;

var
  FormRegmon: TFormRegmon;

implementation

{$R *.dfm}

const
  ProductGuid = '{713CC6CE-B3E2-4fd9-838D-E28F558F6866}';
  HKLM = '\Registry\Machine\';
  HKU = '\Registry\User\';
  HKCUTemplate = '\Registry\User\%s\';
  HKCRTemplate = '\Registry\User\%s_Classes\';

type
  ByteArray = array of Byte;

  TKeyContext = class
  private
    FAccess: Integer;
    FKeyName: string;
    FNewName: string;
    FLastWriteTime: TDateTime;
    FValueName: string;
    FValueType: Integer;
    FIntegerValue: Int64;
    FStringValue: string;
    FBinaryValue: ByteArray;
    procedure SetNewName(const Value: string);
    procedure SetStringValue(const Value: string);
    procedure SetValueName(const Value: string);
  public
    constructor Create(AKeyName: string; AAccess: Integer);
    procedure ClearValue();
    procedure SetBinaryValue(Data: Pointer; Size: Integer);
    property Access: Integer read FAccess;
    property KeyName: string read FKeyName;
    property NewName: string read FNewName write SetNewName;
    property LastWriteTime: TDateTime read FLastWriteTime write FLastWriteTime;
    property ValueName: string read FValueName write SetValueName;
    property ValueType: Integer read FValueType write FValueType;
    property IntegerValue: Int64 read FIntegerValue write FIntegerValue;
    property StringValue: string read FStringValue write SetStringValue;
    property BinaryValue: ByteArray read FBinaryValue;
  end;

  TLogEntry = record
    Number: Integer;
    Ticks: Int64;
    Operation: PChar;
    Name: PChar;
    Status: Integer;
    Details: PChar;
  end;
  PLogEntry = ^TLogEntry;

  PTokenUser = ^TTokenUser;
  TTokenUser = record
    User: TSIDAndAttributes;
  end;

var
  HKCU: string;
  HKCR: string;

function GetTickCount64(): Int64; stdcall; external 'kernel32.dll';
function ConvertSidToStringSid(Sid: PSID; var StringSid: LPWSTR): BOOL; stdcall; external advapi32 name 'ConvertSidToStringSidW';

function GetCurrentUserSid(): string;
var
  AccessToken: THandle;
  UserToken: PTokenUser;
  UserTokenSize: DWORD;
  P: PWideChar;
begin
  Result := '';

  if not OpenThreadToken(GetCurrentThread(), TOKEN_QUERY, true, AccessToken) then
  begin
    if GetLastError() <> ERROR_NO_TOKEN then
      Exit;

    if not OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, AccessToken) then
      Exit;
  end;

  try
    GetMem(UserToken, 1024);
    try
      if not GetTokenInformation(AccessToken, TokenUser, UserToken, 1024, UserTokenSize) then
        Exit;

      P := nil;
      if not ConvertSidToStringSid(UserToken.User.Sid, P) then
        Exit;
      Result := String(WideString(P));
      LocalFree(Integer(P));
    finally
      FreeMem(UserToken);
    end;
  finally
    CloseHandle(AccessToken);
  end;
end;

procedure InitHK();
var
  Sid: string;
begin
  Sid := GetCurrentUserSid();
  HKCU := Format(HKCUTemplate, [Sid]);
  HKCR := Format(HKCRTemplate, [Sid]);
end;

function IsBitSet(const Value, Bit: Integer): Boolean;
begin
  Result := (Value and Bit) <> 0;
end;

function StringStartsWith(const S, SubS: string): Boolean;
begin
  if (S = '') or (SubS = '') then
    Result := false
  else
  if Length(S) < Length(SubS) then
    Result := false
  else
    Result := (AnsiStrLIComp(PChar(S), PChar(SubS), Length(SubS)) = 0);
end;

function NtKeyNameToWinApiKeyName(const KeyName: string): string;
begin
  if KeyName = '' then
    Result := ''
  else
  if StringStartsWith(HKLM, KeyName) then
    Result := 'HKLM'
  else
  if StringStartsWith(HKCU, KeyName) then
    Result := 'HKCU'
  else
  if StringStartsWith(HKCR, KeyName) then
    Result := 'HKCR'
  else
  if StringStartsWith(HKU, KeyName) then
    Result := 'HKU'
  else
  if StringStartsWith(KeyName, HKLM) then
    Result := 'HKLM' + Copy(KeyName, Length(HKLM), MaxInt)
  else
  if StringStartsWith(KeyName, HKCU) then
    Result := 'HKCU' + Copy(KeyName, Length(HKCU), MaxInt)
  else
  if StringStartsWith(KeyName, HKCR) then
    Result := 'HKCR' + Copy(KeyName, Length(HKCR), MaxInt)
  else
  if StringStartsWith(KeyName, HKU) then
    Result := 'HKU' + Copy(KeyName, Length(HKU), MaxInt)
  else
  begin
    Result := KeyName;
    UniqueString(Result);
  end;
end;

function StatusToStr(Status: Integer): string;
begin
  case Status of
    ERROR_SUCCESS:
      Result := 'SUCCESS';

    ERROR_FILE_NOT_FOUND:
      Result := 'NOT FOUND';

    ERROR_INSUFFICIENT_BUFFER:
      Result := 'SMALL BUFFER';

    ERROR_MORE_DATA:
      Result := 'MORE DATA';

    741:  // ERROR_REPARSE
      Result := 'REPARSE';

  else
    Result := IntToStr(Status);
  end;
end;

function ValueTypeToStr(ValueType: Integer): string;
begin
  case ValueType of
    REG_VALUETYPE_SZ:
      Result := 'SZ';

    REG_VALUETYPE_EXPAND_SZ:
      Result := 'EXPAND_SZ';

    REG_VALUETYPE_BINARY:
      Result := 'BINARY';

    REG_VALUETYPE_DWORD:
      Result := 'DWORD';

    REG_VALUETYPE_MULTI_SZ:
      Result := 'MULTI_SZ';

    REG_VALUETYPE_QWORD:
      Result := 'QWORD';

  else
    Result := 'unknown (' + IntToStr(ValueType) + ')';
  end;
end;

function DumpBinaryValue(Data: Pointer; Size: Integer; MaxSize: Integer): string; overload;
const
  Hex: array [0..15] of Char = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
var
  I, J: Integer;
  P: PByte;
  Count: Integer;
begin
  if (Data = nil) or (Size = 0) or (MaxSize = 0) then
    Result := ''
  else
  begin
    Count := Min(Size, MaxSize);
    Result := StringOfChar(' ', Count * 3 - 1);
    if Count < Size then
      Result := Result + '...';

    P := Data;
    J := 1;
    for I := 0 to Count - 1 do
    begin
      Result[J] := Hex[P^ shr 4];
      Inc(J);
      Result[J] := Hex[P^ and $F];
      Inc(J, 2);
      Inc(P);
    end;
  end;
end;

function DumpBinaryValue(Data: ByteArray; MaxSize: Integer): string; overload;
begin
  if (Length(Data) = 0) or (MaxSize = 0) then
    Result := ''
  else
    Result := DumpBinaryValue(@Data[0], Length(Data), MaxSize);
end;

function StringReplace(const S: string; Original, ReplaceWith: Char): string;
var
  I, L: Integer;
  C: Char;
begin
  L := Length(S);
  if L = 0 then
    Result := ''
  else
  begin
    SetLength(Result, L);
    for I := 1 to L do
    begin
      C := S[I];
      if C = Original then
        Result[I] := ReplaceWith
      else
        Result[I] := C;
    end;
  end;
end;

{ TKeyContext }

constructor TKeyContext.Create(AKeyName: string; AAccess: Integer);
begin
  inherited Create();
  FKeyName := NtKeyNameToWinApiKeyName(AKeyName);
  FAccess := AAccess;
  FNewName := '';
  FLastWriteTime := 0;
  FValueName := '';
  FValueType := 0;
  FIntegerValue := 0;
  FStringValue := '';
  SetLength(FBinaryValue, 0);
end;

procedure TKeyContext.ClearValue();
begin
  FValueName := '';
  FValueType := 0;
  FIntegerValue := 0;
  FStringValue := '';
  SetLength(FBinaryValue, 0);
end;

procedure TKeyContext.SetBinaryValue(Data: Pointer; Size: Integer);
begin
  if (Data = nil) or (Size = 0) then
    SetLength(FBinaryValue, 0)
  else
  begin
    SetLength(FBinaryValue, Size);
    Move(Data^, FBinaryValue[0], Size);
  end;
end;

procedure TKeyContext.SetNewName(const Value: string);
begin
  if Value = '' then
    FNewName := ''
  else
  begin
    FNewName := Value;
    UniqueString(FNewName);
  end;
end;

procedure TKeyContext.SetStringValue(const Value: string);
begin
  if Value = '' then
    FStringValue := ''
  else
  begin
    FStringValue := Value;
    UniqueString(FStringValue);
  end;
end;

procedure TKeyContext.SetValueName(const Value: string);
begin
  if Value = '' then
    FValueName := ''
  else
  begin
    FValueName := Value;
    UniqueString(FValueName);
  end;
end;

{ TLogEntry }

function NewEntry(ANumber: Integer; ATicks: Int64; const AOperation, AName: string; AStatus: Integer): PLogEntry;
begin
  New(Result);
  Result.Number := ANumber;
  Result.Ticks := ATicks;
  Result.Operation := StrNew(PChar(AOperation));
  Result.Name := StrNew(PChar(AName));
  Result.Status := AStatus;
  Result.Details := nil;
end;

procedure SetEntryDetails(Entry: PLogEntry; const Value: string);
begin
  if Entry.Details <> nil then
    StrDispose(Entry.Details);
  Entry.Details := StrNew(PChar(Value));
end;

procedure DisposeEntry(var Entry: PLogEntry);
begin
  if Entry = nil then
    Exit;
  StrDispose(Entry.Operation);
  Entry.Operation := nil;
  StrDispose(Entry.Name);
  Entry.Name := nil;
  if Entry.Details <> nil then
    StrDispose(Entry.Details);
  Entry.Details := nil;
  Dispose(Entry);
  Entry := nil;
end;

{ TFormRegmon }

procedure TFormRegmon.AskForReboot(isInstall: Boolean);
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

procedure TFormRegmon.btnInstallClick(Sender: TObject);
var
  Dialog: TOpenDialog;
  CabFile: string;
  Filter: TcbfCBRegistry;
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
      RebootNeeded := Filter.Install(CabFile, ProductGuid, '', INSTALL_REMOVE_OLD_VERSIONS);
      UpdateDriverStatus(Filter);
      UpdateControls();
    except
      on E: EcbfCBRegistry do
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

procedure TFormRegmon.btnSelectFileClick(Sender: TObject);
var
  Dialog: TOpenDialog;
begin
  Dialog := CreateOpenExeFileDialog();
  try
    if Dialog.Execute() then
      edtFilename.Text := Dialog.FileName;
  finally
    FreeAndNil(Dialog);
  end;
end;

procedure TFormRegmon.btnStartClick(Sender: TObject);
begin
  Assert(not FFilterWorking);

  lvwLog.Items.Clear();
  FItemsCount := 0;
  FTicksStarted := GetTickCount64();

  FFilter := CreateFilter();
  try
    FFilter.Initialize(ProductGuid);
    FFilter.SerializeEvents := TcbfcbregistrySerializeEvents.seOnOneWorkerThread;
    FFilter.StartFilter(30000);
    FFilter.AddFilteredProcessByName(edtFilename.Text, false);
  except
    on E: EcbfCBRegistry do
    begin
      FreeAndNil(FFilter);
      MessageDlg('Filter not started.'#13#10 + E.Message, mtError, [mbOk],0);
      Exit;
    end;
  end;

  FFilterWorking := true;
  UpdateControls();
end;

procedure TFormRegmon.btnStopClick(Sender: TObject);
begin
  Assert(FFilterWorking);
  
  if FFilter <> nil then
	FreeAndNil(FFilter);
  
  FFilterWorking := false;
  UpdateControls();
end;

procedure TFormRegmon.btnUninstallClick(Sender: TObject);
var
  Dialog: TOpenDialog;
  CabFile: string;
  Filter: TcbfCBRegistry;
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
      RebootNeeded := Filter.Uninstall(CabFile, ProductGuid, '', UNINSTALL_VERSION_ALL);
      UpdateDriverStatus(Filter);
      UpdateControls();
    except
      on E: EcbfCBRegistry do
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

function TFormRegmon.CreateFilter(): TcbfCBRegistry;
begin
  Result := TcbfCBRegistry.Create(nil);
  Result.OnCleanupKeyContext := CleanupKeyContextHandler;

  Result.OnBeforeCreateKey := BeforeCreateKeyHandler;
  Result.OnBeforeOpenKey := BeforeOpenKeyHandler;
  Result.OnBeforeRenameKey := BeforeRenameKeyHandler;
  Result.OnBeforeSetKey := BeforeSetKeyHandler;

  Result.OnAfterCloseKey := AfterCloseKeyHandler;
  Result.OnAfterCreateKey := AfterCreateKeyHandler;
  Result.OnAfterDeleteKey := AfterDeleteKeyHandler;
  Result.OnAfterEnumerateKey := AfterEnumerateKeyHandler;
  Result.OnAfterOpenKey := AfterOpenKeyHandler;
  Result.OnAfterQueryKey := AfterQueryKeyHandler;
  Result.OnAfterRenameKey := AfterRenameKeyHandler;
  Result.OnAfterSetKey := AfterSetKeyHandler;

  Result.OnBeforeDeleteValue := BeforeDeleteValueHandler;
  Result.OnBeforeSetValue := BeforeSetValueHandler;

  Result.OnAfterDeleteValue := AfterDeleteValueHandler;
  Result.OnAfterEnumerateValue := AfterEnumerateValueHandler;
  Result.OnAfterQueryValue := AfterQueryValueHandler;
  Result.OnAfterSetValue := AfterSetValueHandler;
end;

function TFormRegmon.CreateOpenCabFileDialog(): TOpenDialog;
begin
  Result := TOpenDialog.Create(nil);
  Result.Title := 'Select CBRegistry Driver Package';
  Result.DefaultExt := 'cab';
  Result.FileName := 'cbregistry.cab';
  Result.Filter := 'CBRegistry driver package|cbregistry.cab';
  Result.Options := [ofFileMustExist,ofNoNetworkButton,ofEnableSizing,ofDontAddToRecent];
end;

function TFormRegmon.CreateOpenExeFileDialog: TOpenDialog;
begin
  Result := TOpenDialog.Create(nil);
  Result.Title := 'Select an Executable File';
  Result.DefaultExt := 'exe';
  Result.FileName := '*.exe';
  Result.Filter := 'Executable Files (*.exe)|*.exe|All Files (*.*)|*.*';
  Result.Options := [ofFileMustExist,ofNoNetworkButton,ofEnableSizing,ofDontAddToRecent];
end;

procedure TFormRegmon.FormCreate(Sender: TObject);
var
  Filter: TcbfCBRegistry;
begin
  FDriverRunning := false;
  FFilterWorking := false;
  NoLog := false;

  Filter := CreateFilter();
  try
    UpdateDriverStatus(Filter);
  finally
    FreeAndNil(Filter);
  end;

  UpdateControls();
end;

procedure TFormRegmon.FormDestroy(Sender: TObject);
begin
  FClosing := true;
  FreeAndNil(FFilter);
end;

procedure TFormRegmon.CleanupKeyContextHandler(Sender: TObject; KeyContext: Pointer; var ResultCode: Integer);
begin
  FreeAndNil(TKeyContext(KeyContext));
end;

procedure TFormRegmon.AfterCloseKeyHandler(Sender: TObject; KeyContext: Pointer; var ResultCode: Integer);
var
  Context: TKeyContext;
begin
  if KeyContext = nil then
    Exit;
  Context := KeyContext;
  Log('CloseKey', Context.KeyName, ERROR_SUCCESS, '');
end;

procedure TFormRegmon.AfterCreateKeyHandler(Sender: TObject; KeyContext: Pointer; var Status: Integer;
  var KeyHandle: Int64; var KeyHandleContext: Pointer; var GrantedAccess: Integer; var StopFiltering: Boolean;
  var ResultCode: Integer);
var
  Context: TKeyContext;
  Access: integer;
  Details: string;
begin
  if KeyContext = nil then
    Exit;
  Context := KeyContext;
  if GrantedAccess = 0 then
    Access := Context.Access
  else
    Access := GrantedAccess;
  Details := Format('Access: $%X', [Access]);
  Log('CreateKey', Context.KeyName, Status, Details);
end;

procedure TFormRegmon.AfterDeleteKeyHandler(Sender: TObject; KeyContext: Pointer; var Status: Integer;
  var Processed, StopFiltering: Boolean; var ResultCode: Integer);
var
  Context: TKeyContext;
begin
  if KeyContext = nil then
    Exit;
  Context := KeyContext;
  Log('DeleteKey', Context.KeyName, Status, '');
end;

procedure TFormRegmon.AfterDeleteValueHandler(Sender: TObject; KeyContext: Pointer; var Status: Integer;
  var Processed, StopFiltering: Boolean; var ResultCode: Integer);
var
  Context: TKeyContext;
begin
  if KeyContext = nil then
    Exit;
  Context := KeyContext;
  Log('DeleteKey', Context.KeyName + '\' + Context.ValueName, Status, '');
  Context.ValueName := '';
end;

procedure TFormRegmon.AfterEnumerateKeyHandler(Sender: TObject; KeyContext: Pointer; var Status: Integer;
  Index: Integer; ValidFields: Integer; var LastWriteTime: TDateTime; var Name: string; var ClassName: string;
  var SubKeys: Integer; var MaxNameLength: Integer; var MaxClassNameLength: Integer; var Values: Integer;
  var MaxValueNameLength: Integer; var MaxValueDataSize: Integer; var VirtualizationCandidate: Boolean;
  var VirtualizationEnabled: Boolean; var VirtualTarget: Boolean; var VirtualStore: Boolean;
  var VirtualSource: Boolean; var Processed: Boolean; var StopFiltering: Boolean; var ResultCode: Integer);
var
  Context: TKeyContext;
  Details: string;
begin
  if (KeyContext = nil) or (Status = ERROR_NO_MORE_ITEMS) then
    Exit;
  Context := KeyContext;
  Details := '';
  if Status = ERROR_SUCCESS then
  begin
    Details := IntToStr(Index) + ': ';
    if IsBitSet(ValidFields, REG_KEYFIELD_NAME) then
      Details := Details + Name;
    if IsBitSet(ValidFields, REG_KEYFIELD_SUBKEYS) then
      Details := Details + '; Subkeys: ' + IntToStr(SubKeys);
    if IsBitSet(ValidFields, REG_KEYFIELD_VALUES) then
      Details := Details + '; Values: ' + IntToStr(Values);
  end;
  Log('EnumKey', Context.KeyName, Status, Details);
end;

procedure TFormRegmon.AfterEnumerateValueHandler(Sender: TObject; KeyContext: Pointer; var Status: Integer; Index,
  ValidFields: Integer; var ValueName: string; var ValueType: Integer; var IntegerValue: Int64; var StringValue: string;
  BinaryValue: Pointer; MaxBinaryValueSize: Integer; var BinaryValueSize: Integer; var Processed,
  StopFiltering: Boolean; var ResultCode: Integer);
var
  Context: TKeyContext;
  Details: string;
begin
  if (KeyContext = nil) or (Status = ERROR_NO_MORE_ITEMS) then
    Exit;
  Context := KeyContext;
  Details := IntToStr(Index) + ': ';
  if IsBitSet(ValidFields, REG_VALUEFIELD_NAME) then
  begin
    if ValueName = '' then
      Details := Details + '(Default)'
    else
      Details := Details + ValueName;
  end;
  if IsBitSet(ValidFields, REG_VALUEFIELD_TYPE) then
  begin
    Details := Details + '; Type: ' + ValueTypeToStr(ValueType);

    if IsBitSet(ValidFields, REG_VALUEFIELD_DATA) then
    begin
      Details := Details + '; Data: ';
      case ValueType of
        REG_VALUETYPE_SZ, REG_VALUETYPE_EXPAND_SZ:
          Details := Details + StringValue;

        REG_VALUETYPE_BINARY:
          Details := Details + Format('[%d] %s', [BinaryValueSize, DumpBinaryValue(BinaryValue, BinaryValueSize, 20)]);

        REG_VALUETYPE_DWORD, REG_VALUETYPE_QWORD:
          Details := Details + IntToStr(IntegerValue);

        REG_VALUETYPE_MULTI_SZ:
          StringReplace(StringValue, #$17, ' ');
      end;
    end;
  end;
  Log('EnumValue', Context.KeyName, Status, Details);
end;

procedure TFormRegmon.AfterOpenKeyHandler(Sender: TObject; KeyContext: Pointer; var Status: Integer;
  var KeyHandle: Int64; var KeyHandleContext: Pointer; var GrantedAccess: Integer; var StopFiltering: Boolean;
  var ResultCode: Integer);
var
  Context: TKeyContext;
  Access: integer;
  Details: string;
begin
  if KeyContext = nil then
    Exit;
  Context := KeyContext;
  if GrantedAccess = 0 then
    Access := Context.Access
  else
    Access := GrantedAccess;
  Details := Format('Access: $%X', [Access]);
  Log('OpenKey', Context.KeyName, Status, Details);
end;

procedure TFormRegmon.AfterQueryKeyHandler(Sender: TObject; KeyContext: Pointer; var Status: Integer;
  ValidFields: Integer; var LastWriteTime: TDateTime; var Name, ClassName: string; var SubKeys, MaxNameLength,
  MaxClassNameLength, Values, MaxValueNameLength, MaxValueDataSize: Integer; var VirtualizationCandidate,
  VirtualizationEnabled, VirtualTarget, VirtualStore, VirtualSource, Processed, StopFiltering: Boolean;
  var ResultCode: Integer);
var
  Context: TKeyContext;
  Details: string;
begin
  if KeyContext = nil then
    Exit;
  Context := KeyContext;
  Details := '';
  if Status = ERROR_SUCCESS then
  begin
    if IsBitSet(ValidFields, REG_KEYFIELD_SUBKEYS) then
      Details := Details + 'Subkeys: ' + IntToStr(SubKeys);
    if IsBitSet(ValidFields, REG_KEYFIELD_VALUES) then
    begin
      if Details <> '' then
        Details := Details + '; ';
      Details := Details + 'Values: ' + IntToStr(Values);
    end;
  end;
  Log('QueryKey', Context.KeyName, Status, Details);
end;

procedure TFormRegmon.AfterQueryValueHandler(Sender: TObject; KeyContext: Pointer; var Status: Integer;
  const ValueName: string; ValidFields: Integer; var ValueType: Integer; var IntegerValue: Int64;
  var StringValue: string; BinaryValue: Pointer; MaxBinaryValueSize: Integer; var BinaryValueSize: Integer;
  var Processed, StopFiltering: Boolean; var ResultCode: Integer);
var
  Context: TKeyContext;
  Name, Details: string;
begin
  if KeyContext = nil then
    Exit;
  Context := KeyContext;
  Details := '';

  if ValueName = '' then
    Name := '(Default)'
  else
    Name := ValueName;

  if IsBitSet(ValidFields, REG_VALUEFIELD_TYPE) then
  begin
    Details := 'Type: ' + ValueTypeToStr(ValueType);

    if IsBitSet(ValidFields, REG_VALUEFIELD_DATA) then
    begin
      Details := Details + '; Data: ';
      case ValueType of
        REG_VALUETYPE_SZ, REG_VALUETYPE_EXPAND_SZ:
          Details := Details + StringValue;

        REG_VALUETYPE_BINARY:
          Details := Details + Format('[%d] %s', [BinaryValueSize, DumpBinaryValue(BinaryValue, BinaryValueSize, 20)]);

        REG_VALUETYPE_DWORD, REG_VALUETYPE_QWORD:
          Details := Details + IntToStr(IntegerValue);

        REG_VALUETYPE_MULTI_SZ:
          StringReplace(StringValue, #$17, ' ');
      end;
    end;
  end;

  Log('QueryValue', Context.KeyName + '\' + Name, Status, Details);
end;

procedure TFormRegmon.AfterRenameKeyHandler(Sender: TObject; KeyContext: Pointer; var Status: Integer; var Processed,
  StopFiltering: Boolean; var ResultCode: Integer);
var
  Context: TKeyContext;
  Details: string;
begin
  if KeyContext = nil then
    Exit;
  Context := KeyContext;
  Details := 'New name: ' + Context.NewName;
  Context.NewName := '';
  Log('RenameKey', Context.KeyName, Status, Details);
end;

procedure TFormRegmon.AfterSetKeyHandler(Sender: TObject; KeyContext: Pointer; var Status: Integer; var Processed,
  StopFiltering: Boolean; var ResultCode: Integer);
var
  Context: TKeyContext;
  Details: string;
begin
  if KeyContext = nil then
    Exit;
  Context := KeyContext;
  Details := '';
  if Context.LastWriteTime <> 0 then
    Details := 'Last write time: ' + DateTimeToStr(Context.LastWriteTime);
  Context.LastWriteTime := 0;
  Log('SetKey', Context.KeyName, Status, Details);
end;

procedure TFormRegmon.AfterSetValueHandler(Sender: TObject; KeyContext: Pointer; var Status: Integer;
  var Processed, StopFiltering: Boolean; var ResultCode: Integer);
var
  Context: TKeyContext;
  Name, Details: string;
begin
  if KeyContext = nil then
    Exit;
  Context := KeyContext;
  Details := '';

  if Context.ValueName = '' then
    Name := '(Default)'
  else
    Name := Context.ValueName;

  if Context.ValueType <> 0 then
  begin
    Details := 'Type: ' + ValueTypeToStr(Context.ValueType) + '; Data: ';

    case Context.ValueType of
      REG_VALUETYPE_SZ, REG_VALUETYPE_EXPAND_SZ:
        Details := Details + Context.StringValue;

      REG_VALUETYPE_BINARY:
        Details := Details + Format('[%d] %s', [Length(Context.BinaryValue), DumpBinaryValue(Context.BinaryValue, 20)]);

      REG_VALUETYPE_DWORD, REG_VALUETYPE_QWORD:
        Details := Details + IntToStr(Context.IntegerValue);

      REG_VALUETYPE_MULTI_SZ:
        StringReplace(Context.StringValue, #$17, ' ');
    end;
  end;
  Context.ClearValue();

  Log('SetValue', Context.KeyName + '\' + Name, Status, Details);
end;

procedure TFormRegmon.BeforeCreateKeyHandler(Sender: TObject; const FullName: String; DesiredAccess: Integer;
  var KeyHandle: Int64; var KeyHandleContext: Pointer; var GrantedAccess: Int64; var KeyContext: Pointer;
  var StopFiltering: Boolean; var ResultCode: Integer);
begin
  KeyContext := TKeyContext.Create(FullName, DesiredAccess);
end;

procedure TFormRegmon.BeforeDeleteValueHandler(Sender: TObject; KeyContext: Pointer; const ValueName: string;
  var Processed, CallPostEvent, StopFiltering: Boolean; var ResultCode: Integer);
begin
  if KeyContext <> nil then
    TKeyContext(KeyContext).ValueName := ValueName;
end;

procedure TFormRegmon.BeforeOpenKeyHandler(Sender: TObject; const FullName: string; DesiredAccess: Integer;
  var KeyHandle: Int64; var KeyHandleContext: Pointer; var GrantedAccess: Integer; var KeyContext: Pointer;
  var StopFiltering: Boolean; var ResultCode: Integer);
begin
  KeyContext := TKeyContext.Create(FullName, DesiredAccess);
end;

procedure TFormRegmon.BeforeRenameKeyHandler(Sender: TObject; KeyContext: Pointer; const NewName: string; var Processed,
  FireAfterEvent, StopFiltering: Boolean; var ResultCode: Integer);
begin
  if KeyContext <> nil then
    TKeyContext(KeyContext).NewName := NewName;
end;

procedure TFormRegmon.BeforeSetKeyHandler(Sender: TObject; KeyContext: Pointer; LastWriteTime: TDateTime; var Processed,
  FireAfterEvent, StopFiltering: Boolean; var ResultCode: Integer);
begin
  if KeyContext <> nil then
    TKeyContext(KeyContext).LastWriteTime := LastWriteTime;
end;

procedure TFormRegmon.BeforeSetValueHandler(Sender: TObject; KeyContext: Pointer; const ValueName: string;
  ValueType: Integer; IntegerValue: Int64; const StringValue: string; BinaryValue: Pointer; BinaryValueSize: Integer;
  var Processed, CallPostEvent, StopFiltering: Boolean; var ResultCode: Integer);
var
  Context: TKeyContext;
begin
  if KeyContext = nil then
    Exit;
  Context := KeyContext;
  Context.ValueName := ValueName;
  Context.ValueType := ValueType;
  Context.IntegerValue := IntegerValue;
  Context.StringValue := StringValue;
  Context.SetBinaryValue(BinaryValue, BinaryValueSize);
end;

procedure TFormRegmon.Log(const Operation, Name: string; Status: Integer; const Details: string);
var
  Entry: PLogEntry;
  Number: Integer;
  Ticks: Int64;
begin
  if FClosing then Exit;
  Number := InterlockedIncrement(FItemsCount);
  Ticks := GetTickCount64() - FTicksStarted;
  Entry := NewEntry(Number, Ticks, Operation, Name, Status);
  SetEntryDetails(Entry, Details);
  PostMessage(Self.Handle, WM_LOG, 0, LParam(Entry));
end;

procedure TFormRegmon.UpdateControls();
begin
  btnUninstall.Enabled := FDriverRunning and not FFilterWorking;

  edtFilename.Enabled := FDriverRunning and not FFilterWorking;
  btnSelectFile.Enabled := FDriverRunning and not FFilterWorking;

  btnStart.Enabled := FDriverRunning and not FFilterWorking;
  btnStop.Enabled := FFilterWorking;
end;

procedure TFormRegmon.UpdateDriverStatus(Filter: TcbfCBRegistry);
var
  Status, Version: Int64;
  StatusStr: string;
begin
  Assert(Filter <> nil);
  FDriverRunning := false;

  try
    Status := Filter.GetDriverStatus(ProductGuid);
    if Status = 0 then
      lblStatus.Caption := 'Driver is not installed'
    else
    begin
      case Status of
        SERVICE_STOPPED:
          StatusStr := 'stopped';
        SERVICE_START_PENDING:
          StatusStr := 'start pending';
        SERVICE_STOP_PENDING:
          StatusStr := 'stop pending';
        SERVICE_RUNNING:
          begin
           StatusStr := 'running';
           FDriverRunning := true;
          end;
        SERVICE_CONTINUE_PENDING:
          StatusStr := 'continue pending';
        SERVICE_PAUSE_PENDING:
          StatusStr := 'pause pending';
        SERVICE_PAUSED:
          StatusStr := 'paused';
      else
        StatusStr := Format('unknown %d ($%0:X)', [Status]);
      end;

      Version := Filter.GetDriverVersion(ProductGuid);
      lblStatus.Caption := Format('Driver is installed (version %d.%d.%d.%d), service is %s',
        [Version shr 48, Version shr 32 and $FFFF, Version shr 16 and $FFFF, Version and $FFFF, StatusStr]);
    end;
  except
    on E: Exception do
      lblStatus.Caption := 'Error: ' + E.Message;
  end;
end;

procedure TFormRegmon.WMLog(var Msg: TMessage);
var
  Item: TListItem;
  Entry: PLogEntry;
begin
  if FClosing then 
    exit;
  if NoLog then
    exit;
  if Msg.LParam = 0 then
    Exit;
  Entry := PLogEntry(Msg.LParam);
  Msg.LParam := 0;
  Msg.Result := 1;
  lvwLog.Items.BeginUpdate();
  try
    Item := lvwLog.Items.Add();
    Item.Caption := IntToStr(Entry.Number);
    Item.SubItems.Add(IntToStr(Entry.Ticks));
    Item.SubItems.Add(Entry.Operation);
    Item.SubItems.Add(Entry.Name);
    Item.SubItems.Add(StatusToStr(Entry.Status));
    Item.SubItems.Add(Entry.Details);
  finally
    lvwLog.Items.EndUpdate();
    DisposeEntry(Entry);
  end;
end;

procedure TFormRegmon.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  NoLog := true;
end;

initialization
  InitHK();

end.



