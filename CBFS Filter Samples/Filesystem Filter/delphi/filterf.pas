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
unit filterf;

interface

uses
  Windows, WinSvc, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, cbfconstants, cbfcore, cbfcbfilter, XPMan;

type
  TFormFilter = class(TForm)
    dlgOpenDrv: TOpenDialog;
    Label1: TLabel;
    grpDriver: TGroupBox;
    btnInstall: TButton;
    btnUninstall: TButton;
    lblDriverStatus: TLabel;
    grpRules: TGroupBox;
    lbMask: TLabel;
    edtMask: TEdit;
    cmbFlags: TComboBox;
    btnAddRule: TButton;
    btnDeleteRule: TButton;
    lbFilterRules: TLabel;
    lstRules: TListView;
    grpFilter: TGroupBox;
    btnStartFilter: TButton;
    btnStopFilter: TButton;
    XPManifest: TXPManifest;
    procedure btnDeleteRuleClick(Sender: TObject);
    procedure btnAddRuleClick(Sender: TObject);
    procedure btnStopFilterClick(Sender: TObject);
    procedure btnStartFilterClick(Sender: TObject);
    procedure btnUninstallClick(Sender: TObject);
    procedure btnInstallClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure FilterCleanupContext(Sender: TObject; FileContext: Pointer;
      HandleContext: Pointer; var ResultCode: Integer);

    { Control events }

    procedure FilterAfterCanFileBeDeleted(Sender: TObject;
      const FileName: String;
      RequestType: Integer;
      CanDelete: Boolean;
      var Status: Integer;
      var FileContext: Pointer;
      var HandleContext: Pointer;
      var ResultCode: Integer);
    procedure FilterAfterCleanupFile(Sender: TObject; const FileName: string; var FileContext: Pointer;
      var HandleContext: Pointer; var ResultCode: Integer);
    procedure FilterAfterCloseEnumeration(Sender: TObject;
      const DirectoryName: String;
      DirectoryContext: Pointer;
      HandleContext: Pointer;
      EnumerationContext: Pointer;
      var ResultCode: Integer);
    procedure FilterAfterCloseFile(Sender: TObject; const FileName: string; var FileContext: Pointer;
      var HandleContext: Pointer; var ResultCode: Integer);
    procedure FilterAfterCreateFile(Sender: TObject;
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
    procedure FilterAfterCreateHardLink(Sender: TObject; const FileName: string; const LinkName: string;
      var Status: Integer; var FileContext: Pointer; var HandleContext: Pointer; var ResultCode: Integer);
    procedure FilterAfterDeleteFile(Sender: TObject;
      const FileName: String;
      RequestType: Integer;
      var FileContext: Pointer;
      var HandleContext: Pointer;
      var ResultCode: Integer);
    procedure FilterAfterEnumerateDirectory(Sender: TObject;
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
    procedure FilterAfterFsctl(Sender: TObject;
      const FileName: String;
      FsControlCode: Integer;
      InBuffer: Pointer;
      InBufferLength: Integer;
      InBufferValidBytes: Integer;
      OutBuffer: Pointer;
      OutBufferLength: Integer;
      var OutBufferValidBytes: Integer;
      var Status: Integer;
      var FileContext: Pointer;
      var HandleContext: Pointer;
      var ResultCode: Integer);
    procedure FilterAfterGetFileSecurity(Sender: TObject; const FileName: string; SecurityInformation: Integer;
      SecurityDescriptor: Pointer; Length: Integer; var LengthNeeded: Integer; var Status: Integer;
      var FileContext: Pointer; var HandleContext: Pointer; var ResultCode: Integer);
    procedure FilterAfterGetFileSizes(Sender: TObject; const FileName: string; var Size: Int64;
      var AllocationSize: Int64; var Status: Integer; var FileContext: Pointer; var HandleContext: Pointer;
      var ResultCode: Integer);
    procedure FilterAfterIoctl(Sender: TObject;
      const FileName: String;
      IoControlCode: Integer;
      InBuffer: Pointer;
      InBufferLength: Integer;
      InBufferValidBytes: Integer;
      OutBuffer: Pointer;
      OutBufferLength: Integer;
      var OutBufferValidBytes: Integer;
      var Status: Integer;
      var FileContext: Pointer;
      var HandleContext: Pointer;
      var ResultCode: Integer);
    procedure FilterAfterLock(Sender: TObject; const FileName: string; Offset: Int64; Length: Int64;
      Key: Int64; FailImmediately: Boolean; ExclusiveLock: Boolean; var Status: Integer; 
      var FileContext: Pointer; var HandleContext: Pointer; var ResultCode: Integer);
    procedure FilterAfterOpenFile(Sender: TObject;
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
    procedure FilterAfterQueryFileInfo(Sender: TObject; const FileName: string; FileInformationClass: Integer;
      Buffer: Pointer; BufferLength: Integer; var ValidBytes: Integer; var Status: Integer; 
      var FileContext: Pointer; var HandleContext: Pointer; var ResultCode: Integer);
    procedure FilterAfterReadFile(Sender: TObject;
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
    procedure FilterAfterRenameOrMoveFile(Sender: TObject; const FileName: string; const NewFileName: string;
      var Status: Integer; var FileContext: Pointer; var HandleContext: Pointer; var ResultCode: Integer);
    procedure FilterAfterSetAllocationSize(Sender: TObject; const FileName: string; AllocationSize: Int64;
      var Status: Integer; var FileContext: Pointer; var HandleContext: Pointer; var ResultCode: Integer);
    procedure FilterAfterSetFileAttributes(Sender: TObject; const FileName: string; CreationTime: TDateTime;
      LastAccessTime: TDateTime; LastWriteTime: TDateTime; ChangeTime: TDateTime; FileAttributes: Integer; var Status: Integer;
      var FileContext: Pointer; var HandleContext: Pointer; var ResultCode: Integer);
    procedure FilterAfterSetFileSecurity(Sender: TObject; const FileName: string; SecurityInformation: Integer;
      SecurityDescriptor: Pointer; Length: Integer; var Status: Integer; var FileContext: Pointer;
      var HandleContext: Pointer; var ResultCode: Integer);
    procedure FilterAfterSetFileSize(Sender: TObject; const FileName: string; Size: Int64; var Status: Integer;
      var FileContext: Pointer; var HandleContext: Pointer; var ResultCode: Integer);
    procedure FilterAfterUnlockAll(Sender: TObject; const FileName: string; var Status: Integer;
      var FileContext: Pointer; var HandleContext: Pointer; var ResultCode: Integer);
    procedure FilterAfterUnlockAllByKey(Sender: TObject; const FileName: string; Key: Int64; var Status: Integer;
      var FileContext: Pointer; var HandleContext: Pointer; var ResultCode: Integer);
    procedure FilterAfterUnlockSingle(Sender: TObject; const FileName: string; Offset: Int64; 
      Length: Int64; Key: Int64; var Status: Integer; var FileContext: Pointer; var HandleContext: Pointer; 
      var ResultCode: Integer);
    procedure FilterAfterWriteFile(Sender: TObject; const FileName: string; Position: Int64; Buffer: Pointer; 
      BufferLength : Integer; BytesToWrite: Integer; Direction: Integer; var BytesWritten: Integer; 
      var Status: Integer; var FileContext: Pointer; var HandleContext: Pointer; var ResultCode: Integer);

    procedure FilterBeforeCanFileBeDeleted(Sender: TObject;
      const FileName: String;
      RequestType: Integer;
      var CanDelete: Boolean;
      var FileContext: Pointer;
      var HandleContext: Pointer;
      var ProcessRequest: Boolean;
      var ResultCode: Integer);
    procedure FilterBeforeCleanupFile(Sender: TObject; const FileName: string; var FileContext: Pointer;
      var HandleContext: Pointer; var ResultCode: Integer);
    procedure FilterBeforeCloseFile(Sender: TObject; const FileName: string; var FileContext: Pointer; 
      var HandleContext: Pointer; var ResultCode: Integer);
    procedure FilterBeforeCreateFile(Sender: TObject;
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
    procedure FilterBeforeCreateHardLink(Sender: TObject; const FileName: string; const LinkName: String; 
      var ReplaceIfExists: Boolean; var FileContext: Pointer; var HandleContext: Pointer; 
      var ProcessRequest: Boolean; var ResultCode: Integer);
    procedure FilterBeforeDeleteFile(Sender: TObject;
      const FileName: String;
      RequestType: Integer;
      var FileContext: Pointer;
      var HandleContext: Pointer;
      var ResultCode: Integer);
    procedure FilterBeforeFsctl(Sender: TObject;
      const FileName: String;
      FsControlCode: Integer;
      InBuffer: Pointer;
      InBufferLength: Integer;
      var InBufferValidBytes: Integer;
      OutBuffer: Pointer;
      OutBufferLength: Integer;
      var OutBufferValidBytes: Integer;
      var FileContext: Pointer;
      var HandleContext: Pointer;
      var ProcessRequest: Boolean;
      var ResultCode: Integer);
    procedure FilterBeforeGetFileSecurity(Sender: TObject; const FileName: string; SecurityInformation: Integer;
      SecurityDescriptor: Pointer; Length: Integer; var LengthNeeded: Integer; var FileContext: Pointer;
      var HandleContext: Pointer; var ProcessRequest: Boolean; var ResultCode: Integer);
    procedure FilterBeforeIoctl(Sender: TObject;
      const FileName: String;
      IoControlCode: Integer;
      InBuffer: Pointer;
      InBufferLength: Integer;
      var InBufferValidBytes: Integer;
      OutBuffer: Pointer;
      OutBufferLength: Integer;
      var OutBufferValidBytes: Integer;
      var FileContext: Pointer;
      var HandleContext: Pointer;
      var ProcessRequest: Boolean;
      var ResultCode: Integer);
    procedure FilterBeforeLock(Sender: TObject; const FileName: string; var Offset: Int64; var Length: Int64;
      Key: Int64; var FailImmediately: Boolean; var ExclusiveLock: Boolean; var FileContext: Pointer;
      var HandleContext: Pointer; var ProcessRequest: Boolean; var ResultCode: Integer);
    procedure FilterBeforeOpenFile(Sender: TObject;
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
    procedure FilterBeforeQueryFileInfo(Sender: TObject; const FileName: string; FileInformationClass: Integer;
      Buffer: Pointer; BufferLength: Integer; var ValidBytes: Integer; var FileContext: Pointer;
      var HandleContext: Pointer; var ProcessRequest: Boolean; var ResultCode: Integer);
    procedure FilterBeforeReadFile(Sender: TObject;
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
    procedure FilterBeforeRenameOrMoveFile(Sender: TObject; const FileName: string; const NewFileName: string;
      var ReplaceIfExists: Boolean; var FileContext: Pointer; var HandleContext: Pointer; 
      var ProcessRequest: Boolean; var ResultCode: Integer);
    procedure FilterBeforeSetAllocationSize(Sender: TObject; const FileName: string; var AllocationSize: Int64; 
      var FileContext: Pointer; var HandleContext: Pointer; var ProcessRequest: Boolean; var ResultCode: Integer);
    procedure FilterBeforeSetFileAttributes(Sender: TObject; const FileName: string; var CreationTime: TDateTime; 
      var LastAccessTime: TDateTime; var LastWriteTime: TDateTime; var ChangeTime: TDateTime; var FileAttributes: Integer; 
      var FileContext: Pointer; var HandleContext: Pointer; var ProcessRequest: Boolean; var ResultCode: Integer);
    procedure FilterBeforeSetFileSecurity(Sender: TObject; const FileName: String; SecurityInformation: Integer;
      SecurityDescriptor: Pointer; Length: Integer; var FileContext: Pointer; var HandleContext: Pointer;
      var ProcessRequest: Boolean; var ResultCode: Integer);
    procedure FilterBeforeSetFileSize(Sender: TObject; const FileName: string; var Size: Int64; 
      var FileContext: Pointer; var HandleContext: Pointer; var ProcessRequest: Boolean; var ResultCode: Integer);
    procedure FilterBeforeUnlockAll(Sender: TObject; const FileName: string; var FileContext: Pointer;
      var HandleContext: Pointer; var ProcessRequest: Boolean; var ResultCode: Integer);
    procedure FilterBeforeUnlockAllByKey(Sender: TObject; const FileName: string; Key: Int64;
      var FileContext: Pointer; var HandleContext: Pointer; var ProcessRequest: Boolean; var ResultCode: Integer);
    procedure FilterBeforeUnlockSingle(Sender: TObject; const FileName: string; var Offset: Int64;
      var Length: Int64; Key: Int64; var FileContext: Pointer; var HandleContext: Pointer;
      var ProcessRequest: Boolean; var ResultCode: Integer);
    procedure FilterBeforeWriteFile(Sender: TObject;
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

    procedure FilterReparseFileName(Sender: TObject; const FileName: string; DesiredAccess: Integer;
      const ReparsedFileName: string; var NewFileName: string; var ResultCode: Integer);
    procedure FilterReparseWithTag(Sender: TObject;
      const FileName: String;
      const NewFileName: String;
      ReparseTag: Integer;
      ReparseBuffer: Pointer;
      ReparseBufferLength: Integer;
      var ReissueIO: Boolean;
      var ResultCode: Integer);

    { Notification events }

    procedure FilterNotifyCanFileBeDeleted(Sender: TObject;
      const FileName: String;
      RequestType: Integer;
      CanDelete: Boolean;
      Status: Integer;
      var ResultCode: Integer);
    procedure FilterNotifyCleanupFile(Sender: TObject; const FileName: string; var ResultCode: Integer);
    procedure FilterNotifyCloseFile(Sender: TObject; const FileName: string; var ResultCode: Integer);
    procedure FilterNotifyCreateFile(Sender: TObject;
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
    procedure FilterNotifyCreateHardLink(Sender: TObject; const FileName: string; const LinkName: string;
      Status: Integer; var ResultCode: Integer);
    procedure FilterNotifyDeleteFile(Sender: TObject; const FileName: String;
      RequestType: Integer; var ResultCode: Integer);
    procedure FilterNotifyEnumerateDirectory(Sender: TObject;
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
    procedure FilterNotifyFsctl(Sender: TObject; const FileName: string; FsControlCode: Integer;
      InBuffer: Pointer; InBufferLength: Integer; InBufferValidBytes: Integer; OutBuffer: Pointer;
      OutBufferLength: Integer; OutBufferValidBytes: Integer; Status: Integer; var ResultCode: Integer);
    procedure FilterNotifyGetFileSecurity(Sender: TObject; const FileName: string; SecurityInformation: Integer;
      SecurityDescriptor: Pointer; Length: Integer; Status: Integer; var ResultCode: Integer);
    procedure FilterNotifyGetFileSizes(Sender: TObject; const FileName: string; Size: Int64;
      AllocationSize: Int64; Status: Integer; var ResultCode: Integer);
    procedure FilterNotifyIoctl(Sender: TObject; const FileName: string; IoControlCode: Integer;
      InBuffer: Pointer; InBufferLength: Integer; InBufferValidBytes: Integer; OutBuffer: Pointer;
      OutBufferLength: Integer; OutBufferValidBytes: Integer; Status: Integer; var ResultCode: Integer);
    procedure FilterNotifyLock(Sender: TObject; const FileName: string; Offset: Int64; Length: Int64;
      Key: Int64; FailImmediately: Boolean; ExclusiveLock: Boolean; Status: Integer; var ResultCode: Integer);
    procedure FilterNotifyOpenFile(Sender: TObject;
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
    procedure FilterNotifyQueryFileInfo(Sender: TObject; const FileName: string; FileInformationClass: Integer;
      Buffer: Pointer; BufferLength: Integer; ValidBytes: Integer; Status: Integer; var ResultCode: Integer);
    procedure FilterNotifyReadFile(Sender: TObject; const FileName: string; Position: Int64; BytesToRead: Integer;
      Direction: Integer; BytesRead : Integer; Status: Integer; var ResultCode: Integer);
    procedure FilterNotifyRenameOrMoveFile(Sender: TObject; const FileName: string; const NewFileName: string;
      Status: Integer; var ResultCode: Integer);
    procedure FilterNotifySetAllocationSize(Sender: TObject; const FileName: string; AllocationSize: Int64;
      Status: Integer; var ResultCode: Integer);
    procedure FilterNotifySetFileAttributes(Sender: TObject; const FileName: string;
      CreationTime: TDateTime; LastAccessTime: TDateTime; LastWriteTime: TDateTime; ChangeTime: TDateTime; FileAttributes: Integer;
      Status: Integer; var ResultCode: Integer);
    procedure FilterNotifySetFileSecurity(Sender: TObject; const FileName: string; SecurityInformation: Integer;
      SecurityDescriptor: Pointer; Length: Integer; Status: Integer; var ResultCode: Integer);
    procedure FilterNotifySetFileSize(Sender: TObject; const FileName: string; Size: Int64;
      Status: Integer; var ResultCode: Integer);
    procedure FilterNotifyUnlockAll(Sender: TObject; const FileName: string; Status: Integer;
      var ResultCode: Integer);
    procedure FilterNotifyUnlockAllByKey(Sender: TObject; const FileName: string; Key: Int64;
      Status: Integer; var ResultCode: Integer);
    procedure FilterNotifyUnlockSingle(Sender: TObject; const FileName: string; Offset: Int64; Length: Int64;
      Key: Int64; Status: Integer; var ResultCode: Integer);
    procedure FilterNotifyWriteFile(Sender: TObject; const FileName: string; Position: Int64;
      BytesToWrite: Integer; Direction: Integer; BytesWritten : integer; Status: Integer;
      var ResultCode: Integer);
    procedure FilterStop(Sender: TObject; var ResultCode: Integer);
  private
    FFilter: TcbfCBFilter;
    FClosing: boolean;

    FNotifyFlagsFirst: Integer;
    FControlFlagsFirst: Integer;

    procedure UpdateDriverStatus;
    procedure UpdateFilterlist;
    procedure AskForReboot(isInstall: Boolean);
    function StrToFlag(const S: string): Integer;
    function StrToFlag64(const S: string): Int64;
  public
    { Public declarations }
  end;

var
  FormFilter: TFormFilter;

const
   FGuid = '{713CC6CE-B3E2-4fd9-838D-E28F558F6866}';
   ALTITUDE_FAKE_VALUE_FOR_DEBUG = '360000';

implementation

{$R *.dfm}

type
  TRuleFlag = record
    Caption: string;
    Value: Int64;
  end;

const
  AccessFlags: array [0..3] of TRuleFlag = (
      (Caption: 'Access: DeleteProtect';  Value: cbfconstants.ACCESS_DELETE_PROTECT),
      (Caption: 'Access: DenyAll';        Value: cbfconstants.ACCESS_DENY_ALL),
      (Caption: 'Access: ReadOnly';       Value: cbfConstants.ACCESS_READ_ONLY),
      (Caption: 'Access: WriteOnly';      Value: cbfconstants.ACCESS_WRITE_ONLY)
    );

  ControlFlags: array [0..40] of TRuleFlag = (
      (Caption: 'Control: All'; Value: cbfconstants.FS_CE_ALL),
      (Caption: 'Control: BeforeCanDelete';         Value: cbfconstants.FS_CE_BEFORE_CAN_DELETE),
      (Caption: 'Control: BeforeCleanup';           Value: cbfconstants.FS_CE_BEFORE_CLEANUP),
      (Caption: 'Control: BeforeClose';             Value: cbfconstants.FS_CE_BEFORE_CLOSE),
      (Caption: 'Control: BeforeCreate';            Value: cbfconstants.FS_CE_BEFORE_CREATE),
      (Caption: 'Control: BeforeCreateHardLink';    Value: cbfconstants.FS_CE_BEFORE_CREATE_HARD_LINK),
      (Caption: 'Control: BeforeDelete';            Value: cbfconstants.FS_CE_BEFORE_DELETE),
      (Caption: 'Control: BeforeFsctl';             Value: cbfconstants.FS_CE_BEFORE_FSCTL),
      (Caption: 'Control: BeforeGetSecurity';       Value: cbfconstants.FS_CE_BEFORE_GET_SECURITY),
      (Caption: 'Control: BeforeIoctl';             Value: cbfconstants.FS_CE_BEFORE_IOCTL),
      (Caption: 'Control: BeforeLockControl';       Value: cbfconstants.FS_CE_BEFORE_LOCK_CONTROL),
      (Caption: 'Control: BeforeOpen';              Value: cbfconstants.FS_CE_BEFORE_OPEN),
      (Caption: 'Control: BeforeQueryFileInfo';     Value: cbfconstants.FS_CE_BEFORE_QUERY_FILE_INFO),
      (Caption: 'Control: BeforeRead';              Value: cbfconstants.FS_CE_BEFORE_READ),
      (Caption: 'Control: BeforeRename';            Value: cbfconstants.FS_CE_BEFORE_RENAME),
      (Caption: 'Control: BeforeSetAttributes';     Value: cbfconstants.FS_CE_BEFORE_SET_ATTRIBUTES),
      (Caption: 'Control: BeforeSetSecurity';       Value: cbfconstants.FS_CE_BEFORE_SET_SECURITY),
      (Caption: 'Control: BeforeSetSizes';          Value: cbfconstants.FS_CE_BEFORE_SET_SIZES),
      (Caption: 'Control: BeforeWrite';             Value: cbfconstants.FS_CE_BEFORE_WRITE),
      (Caption: 'Control: AfterCanDelete';          Value: cbfconstants.FS_CE_AFTER_CAN_DELETE),
      (Caption: 'Control: AfterCleanup';            Value: cbfconstants.FS_CE_AFTER_CLEANUP),
      (Caption: 'Control: AfterClose';              Value: cbfconstants.FS_CE_AFTER_CLOSE),
      (Caption: 'Control: AfterCreate';             Value: cbfconstants.FS_CE_AFTER_CREATE),
      (Caption: 'Control: AfterCreateHardLink';     Value: cbfconstants.FS_CE_AFTER_CREATE_HARD_LINK),
      (Caption: 'Control: AfterDelete';             Value: cbfconstants.FS_CE_AFTER_DELETE),
      (Caption: 'Control: AfterEnumerateDirectory'; Value: cbfconstants.FS_CE_AFTER_ENUMERATE_DIRECTORY),
      (Caption: 'Control: AfterFsctl';              Value: cbfconstants.FS_CE_BEFORE_FSCTL),
      (Caption: 'Control: AfterGetSecurity';        Value: cbfconstants.FS_CE_AFTER_GET_SECURITY),
      (Caption: 'Control: AfterGetSizes';           Value: cbfconstants.FS_CE_AFTER_GET_SIZES),
      (Caption: 'Control: AfterIoctl';              Value: cbfconstants.FS_CE_AFTER_IOCTL),
      (Caption: 'Control: AfterLockControl';        Value: cbfconstants.FS_CE_AFTER_LOCK_CONTROL),
      (Caption: 'Control: AfterOpen';               Value: cbfconstants.FS_CE_AFTER_OPEN),
      (Caption: 'Control: AfterQueryFileInfo';      Value: cbfconstants.FS_CE_AFTER_QUERY_FILE_INFO),
      (Caption: 'Control: AfterRead';               Value: cbfconstants.FS_CE_AFTER_READ),
      (Caption: 'Control: AfterRename';             Value: cbfconstants.FS_CE_AFTER_RENAME),
      (Caption: 'Control: AfterSetAttributes';      Value: cbfconstants.FS_CE_AFTER_SET_ATTRIBUTES),
      (Caption: 'Control: AfterSetSecurity';        Value: cbfconstants.FS_CE_AFTER_SET_SECURITY),
      (Caption: 'Control: AfterSetSizes';           Value: cbfconstants.FS_CE_AFTER_SET_SIZES),
      (Caption: 'Control: AfterWrite';              Value: cbfconstants.FS_CE_AFTER_WRITE),
      (Caption: 'Control: ReparseFilename';         Value: cbfconstants.FS_CE_REPARSE_FILENAME),
      (Caption: 'Control: ReparseTag';              Value: cbfconstants.FS_CE_REPARSE_TAG)
    );

  NotifyFlags: array [0..20] of TRuleFlag = (
      (Caption: 'Notify: All';                Value: cbfconstants.FS_NE_ALL),
      (Caption: 'Notify: CanDelete';          Value: cbfconstants.FS_NE_CAN_DELETE),
      (Caption: 'Notify: Cleanup';            Value: cbfconstants.FS_NE_CLEANUP),
      (Caption: 'Notify: Close';              Value: cbfconstants.FS_NE_CLOSE),
      (Caption: 'Notify: Create';             Value: cbfconstants.FS_NE_CREATE),
      (Caption: 'Notify: CreateHardLink';     Value: cbfconstants.FS_NE_CREATE_HARD_LINK),
      (Caption: 'Notify: Delete';             Value: cbfconstants.FS_NE_DELETE),
      (Caption: 'Notify: EnumerateDirectory'; Value: cbfconstants.FS_NE_ENUMERATE_DIRECTORY),
      (Caption: 'Notify: Fsctl';              Value: cbfconstants.FS_NE_FSCTL),
      (Caption: 'Notify: GetSecurity';        Value: cbfconstants.FS_NE_GET_SECURITY),
      (Caption: 'Notify: GetSizes';           Value: cbfconstants.FS_NE_GET_SIZES),
      (Caption: 'Notify: Ioctl';              Value: cbfconstants.FS_NE_IOCTL),
      (Caption: 'Notify: LockControl';        Value: cbfconstants.FS_NE_LOCK_CONTROL),
      (Caption: 'Notify: Open';               Value: cbfconstants.FS_NE_OPEN),
      (Caption: 'Notify: QueryFileInfo';      Value: cbfconstants.FS_NE_QUERY_FILE_INFO),
      (Caption: 'Notify: Read';               Value: cbfconstants.FS_NE_READ),
      (Caption: 'Notify: Rename';             Value: cbfconstants.FS_NE_RENAME),
      (Caption: 'Notify: SetAttributes';      Value: cbfconstants.FS_NE_SET_ATTRIBUTES),
      (Caption: 'Notify: SetSecurity';        Value: cbfconstants.FS_NE_SET_SECURITY),
      (Caption: 'Notify: SetSizes';           Value: cbfconstants.FS_NE_SET_SIZES),
      (Caption: 'Notify: Write';              Value: cbfconstants.FS_NE_WRITE)
    );

procedure TFormFilter.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  FFilter                      := TcbfCBFilter.Create(Self);

  FFilter.OnCleanupContext := FilterCleanupContext;

  FFilter.OnAfterCanFileBeDeleted   := FilterAfterCanFileBeDeleted;
  FFilter.OnAfterCleanupFile        := FilterAfterCleanupFile;
  FFilter.OnAfterCloseEnumeration   := FilterAfterCloseEnumeration;
  FFilter.OnAfterCloseFile          := FilterAfterCloseFile;
  FFilter.OnAfterCreateFile         := FilterAfterCreateFile;
  FFilter.OnAfterCreateHardLink     := FilterAfterCreateHardLink;
  FFilter.OnAfterDeleteFile         := FilterAfterDeleteFile;
  FFilter.OnAfterEnumerateDirectory := FilterAfterEnumerateDirectory;
  FFilter.OnAfterFsctl              := FilterAfterFsctl;
  FFilter.OnAfterGetFileSecurity    := FilterAfterGetFileSecurity;
  FFilter.OnAfterGetFileSizes       := FilterAfterGetFileSizes;
  FFilter.OnAfterIoctl              := FilterAfterIoctl;
  FFilter.OnAfterLock               := FilterAfterLock;
  FFilter.OnAfterOpenFile           := FilterAfterOpenFile;
  FFilter.OnAfterQueryFileInfo      := FilterAfterQueryFileInfo;  
  FFilter.OnAfterReadFile           := FilterAfterReadFile;
  FFilter.OnAfterRenameOrMoveFile   := FilterAfterRenameOrMoveFile;
  FFilter.OnAfterSetAllocationSize  := FilterAfterSetAllocationSize;
  FFilter.OnAfterSetFileAttributes  := FilterAfterSetFileAttributes;
  FFilter.OnAfterSetFileSecurity    := FilterAfterSetFileSecurity;
  FFilter.OnAfterSetFileSize        := FilterAfterSetFileSize;
  FFilter.OnAfterUnlockAll          := FilterAfterUnlockAll;
  FFilter.OnAfterUnlockAllByKey     := FilterAfterUnlockAllByKey;
  FFilter.OnAfterUnlockSingle       := FilterAfterUnlockSingle;
  FFilter.OnAfterWriteFile          := FilterAfterWriteFile;

  FFilter.OnBeforeCanFileBeDeleted  := FilterBeforeCanFileBeDeleted;
  FFilter.OnBeforeCleanupFile       := FilterBeforeCleanupFile;
  FFilter.OnBeforeCloseFile         := FilterBeforeCloseFile;
  FFilter.OnBeforeCreateFile        := FilterBeforeCreateFile;
  FFilter.OnBeforeCreateHardLink    := FilterBeforeCreateHardLink;
  FFilter.OnBeforeDeleteFile        := FilterBeforeDeleteFile;
  FFilter.OnBeforeFsctl             := FilterBeforeFsctl;
  FFilter.OnBeforeGetFileSecurity   := FilterBeforeGetFileSecurity;
  FFilter.OnBeforeIoctl             := FilterBeforeIoctl;
  FFilter.OnBeforeLock              := FilterBeforeLock;
  FFilter.OnBeforeOpenFile          := FilterBeforeOpenFile;
  FFilter.OnBeforeQueryFileInfo     := FilterBeforeQueryFileInfo;
  FFilter.OnBeforeReadFile          := FilterBeforeReadFile;
  FFilter.OnBeforeRenameOrMoveFile  := FilterBeforeRenameOrMoveFile;
  FFilter.OnBeforeSetAllocationSize := FilterBeforeSetAllocationSize;
  FFilter.OnBeforeSetFileAttributes := FilterBeforeSetFileAttributes;
  FFilter.OnBeforeSetFileSecurity   := FilterBeforeSetFileSecurity;
  FFilter.OnBeforeSetFileSize       := FilterBeforeSetFileSize;
  FFilter.OnBeforeUnlockAll         := FilterBeforeUnlockAll;
  FFilter.OnBeforeUnlockAllByKey    := FilterBeforeUnlockAllByKey;
  FFilter.OnBeforeUnlockSingle      := FilterBeforeUnlockSingle;
  FFilter.OnBeforeWriteFile         := FilterBeforeWriteFile;

  FFilter.OnReparseFileName         := FilterReparseFileName;
  FFilter.OnReparseWithTag          := FilterReparseWithTag;

  FFilter.OnNotifyCanFileBeDeleted   := FilterNotifyCanFileBeDeleted;
  FFilter.OnNotifyCleanupFile        := FilterNotifyCleanupFile;
  FFilter.OnNotifyCloseFile          := FilterNotifyCloseFile;
  FFilter.OnNotifyCreateFile         := FilterNotifyCreateFile;
  FFilter.OnNotifyCreateHardLink     := FilterNotifyCreateHardLink;
  FFilter.OnNotifyDeleteFile         := FilterNotifyDeleteFile;
  FFilter.OnNotifyEnumerateDirectory := FilterNotifyEnumerateDirectory;
  FFilter.OnNotifyFsctl              := FilterNotifyFsctl;
  FFilter.OnNotifyGetFileSecurity    := FilterNotifyGetFileSecurity;
  FFilter.OnNotifyGetFileSizes       := FilterNotifyGetFileSizes;
  FFilter.OnNotifyIoctl              := FilterNotifyIoctl;
  FFilter.OnNotifyLock               := FilterNotifyLock;
  FFilter.OnNotifyOpenFile           := FilterNotifyOpenFile;
  FFilter.OnNotifyQueryFileInfo      := FilterNotifyQueryFileInfo;
  FFilter.OnNotifyReadFile           := FilterNotifyReadFile;
  FFilter.OnNotifyRenameOrMoveFile   := FilterNotifyRenameOrMoveFile;
  FFilter.OnNotifySetAllocationSize  := FilterNotifySetAllocationSize;
  FFilter.OnNotifySetFileAttributes  := FilterNotifySetFileAttributes;
  FFilter.OnNotifySetFileSecurity    := FilterNotifySetFileSecurity;
  FFilter.OnNotifySetFileSize        := FilterNotifySetFileSize;
  FFilter.OnNotifyUnlockAll          := FilterNotifyUnlockAll;
  FFilter.OnNotifyUnlockAllByKey     := FilterNotifyUnlockAllByKey;
  FFilter.OnNotifyUnlockSingle       := FilterNotifyUnlockSingle;
  FFilter.OnNotifyWriteFile          := FilterNotifyWriteFile;
  FFilter.OnFilterStop               := FilterStop;

  for I := Low(AccessFlags) to High(AccessFlags) do
    cmbFlags.Items.Add(AccessFlags[I].Caption);

  FNotifyFlagsFirst := cmbFlags.Items.Count;
  for I := Low(NotifyFlags) to High(NotifyFlags) do
    cmbFlags.Items.Add(NotifyFlags[I].Caption);

  FControlFlagsFirst := cmbFlags.Items.Count;
  for I := Low(ControlFlags) to High(ControlFlags) do
    cmbFlags.Items.Add(ControlFlags[I].Caption);

  cmbFlags.ItemIndex := 0;

  UpdateDriverStatus;
  UpdateFilterList;
end;

procedure TFormFilter.FormDestroy(Sender: TObject);
begin
  FClosing := true;
  FFilter.Free;
end;

function TFormFilter.StrToFlag(const S: string): Integer;
begin
  Result := StrToIntDef('$' + S, 0);
end;

function TFormFilter.StrToFlag64(const S: string): Int64;
begin
  Result := StrToInt64Def('$' + S, 0);
end;

procedure TFormFilter.UpdateDriverStatus;
var
  Status: Integer;
  State: string;
  Version: Int64;
begin

  btnStartFilter.Enabled := not FFilter.Active;
  btnStopFilter.Enabled := FFilter.Active;

  try
    Status := FFilter.GetDriverStatus(FGuid);
    if Status = 0 then
    begin
      lblDriverStatus.Caption := 'Driver is not installed'; 
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

      Version := FFilter.GetDriverVersion(FGuid);
      lblDriverStatus.Caption := Format('Driver (ver %d.%d.%d.%d) installed, service %s',
        [Version shr 48, Version shr 32 and $FFFF, Version shr 16 and $FFFF, Version and $FFFF, State]);
    end;
  except
    on E: Exception do
      lblDriverStatus.Caption := 'Error: ' + E.Message;
  end;
end;

procedure TFormFilter.UpdateFilterList;
var
  Index: Integer;
  Item: TListItem;
begin
  lstRules.Items.Clear;
  for Index := 0 to  FFilter.FilterRuleCount - 1 do
  begin
    Item := lstRules.Items.Add;
    Item.Caption := FFilter.FilterRuleMask[Index];
    lstRules.Items[Index].SubItems.Add(Format('%.2X', [FFilter.FilterRuleAccessFlags[Index]]));
    lstRules.Items[Index].SubItems.Add(Format('%.12X', [FFilter.FilterRuleControlFlags[Index]]));
    lstRules.Items[Index].SubItems.Add(Format('%.6X', [FFilter.FilterRuleNotifyFlags[Index]]));
  end;
end;

procedure TFormFilter.AskForReboot(isInstall: Boolean);
var
  Action: string;
begin
  if isInstall then 
    Action := 'install'
  else 
    Action := 'uninstall';

  MessageDlg('System restart is needed in order to ' + Action + ' the drivers.' +
    #13#10 + 'Please, reboot your computer now.', mtWarning, [mbOK], 0);
end;

procedure TFormFilter.btnInstallClick(Sender: TObject);
var
  RebootNeeded: Boolean;
begin
  RebootNeeded := false;
  if dlgOpenDrv.Execute then
  begin
    try
      RebootNeeded := FFilter.Install(dlgOpenDrv.FileName, FGuid, '', ALTITUDE_FAKE_VALUE_FOR_DEBUG, 0);
    except on E: ECBFSFilter do
      begin
        if (E.Code = ERROR_ACCESS_DENIED) or (E.Code = ERROR_PRIVILEGE_NOT_HELD) then
          MessageDlg('Installation requires administrator rights. Run the app as administrator', mtError, [mbOk], 0)
        else
          MessageDlg('Installation failed.'#13#10 + E.Message, mtError, [mbOk], 0);
      end;
    end;

    UpdateDriverStatus;

    if RebootNeeded then
      AskForReboot(true);
  end;
end;

procedure TFormFilter.btnUninstallClick(Sender: TObject);
var
  RebootNeeded: Boolean;
begin
  RebootNeeded := false;
  if dlgOpenDrv.Execute then
  begin
    try
      RebootNeeded := FFilter.Uninstall(dlgOpenDrv.FileName, FGuid, '', UNINSTALL_VERSION_CURRENT);
    except on E: ECBFSFilter do
      begin
        if (E.Code = ERROR_ACCESS_DENIED) or (E.Code = ERROR_PRIVILEGE_NOT_HELD) then
          MessageDlg('Uninstallation requires administrator rights. Run the app as administrator', mtError, [mbOk], 0)
        else
          MessageDlg('Installation failed.'#13#10 + E.Message, mtError, [mbOk], 0);
        end;
      end;
    UpdateDriverStatus;
    if RebootNeeded then
      AskForReboot(false);
  end;
end;

procedure TFormFilter.btnStartFilterClick(Sender: TObject);
begin
  try
    FFilter.Initialize(FGuid);
    FFilter.StartFilter(30000);
    UpdateDriverStatus;
    UpdateFilterList;
  except
    On E: Exception do
    MessageDlg(E.Message, mtError, [mbOk], 0);
  end;
end;

procedure TFormFilter.btnStopFilterClick(Sender: TObject);
begin
  try
    FFilter.StopFilter(false);
  except
    On E: Exception do
    MessageDlg(E.Message, mtError, [mbOk], 0);
  end;
end;


procedure TFormFilter.btnAddRuleClick(Sender: TObject);
var
  I: Integer;
begin
  try
    I := cmbFlags.ItemIndex;

    if I < FNotifyFlagsFirst then
      FFilter.AddFilterRule(edtMask.Text, AccessFlags[I].Value, 0, 0)
    else
    if I < FControlFlagsFirst then
      FFilter.AddFilterRule(edtMask.Text, 0, cbfConstants.FS_CE_NONE, NotifyFlags[I - FNotifyFlagsFirst].Value)
    else
      FFilter.AddFilterRule(edtMask.Text, 0, ControlFlags[I - FControlFlagsFirst].Value, cbfConstants.FS_NE_NONE);

    UpdateFilterList();
  except
    on E: Exception do
      MessageDlg(E.Message, mtError, [mbOk], 0);
  end;
end;

procedure TFormFilter.btnDeleteRuleClick(Sender: TObject);
var
  I: Integer;
  Mask: string;
begin
  try
    for I := 0 to lstRules.Items.Count - 1 do
    begin
      if not lstRules.Items[I].Checked then
        Continue;

      Mask := lstRules.Items[I].Caption;
      FFilter.DeleteFilterRule(Mask, ACCESS_ALL_FLAGS, FS_CE_ALL, FS_NE_ALL);
    end;
    UpdateFilterList();
  except
    On E: Exception do
    MessageDlg(E.Message, mtError, [mbOk], 0);
  end;
end;

{ General filter events }

procedure TFormFilter.FilterStop(Sender: TObject; var ResultCode: Integer);
begin
  if not FClosing then
    UpdateDriverStatus;
end;

procedure TFormFilter.FilterCleanupContext(Sender: TObject;
  FileContext: Pointer; HandleContext: Pointer; var ResultCode: Integer);
begin
  { insert your code here }
end;

{ Notificaton events }

procedure TFormFilter.FilterNotifyCanFileBeDeleted(Sender: TObject;
  const FileName: String;
  RequestType: Integer;
  CanDelete: Boolean;
  Status: Integer;
  var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterNotifyCleanupFile(Sender: TObject; const FileName: string; var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterNotifyCloseFile(Sender: TObject; const FileName: string; var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterNotifyCreateFile(Sender: TObject;
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
  { insert your code here }
end;

procedure TFormFilter.FilterNotifyCreateHardLink(Sender: TObject; const FileName: string; const LinkName: string;
  Status: Integer; var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterNotifyDeleteFile(Sender: TObject;
  const FileName: String; RequestType: Integer; var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterNotifyEnumerateDirectory(Sender: TObject;
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
  { insert your code here }
end;

procedure TFormFilter.FilterNotifyFsctl(Sender: TObject; const FileName: string; FsControlCode: Integer;
  InBuffer: Pointer; InBufferLength: Integer; InBufferValidBytes: Integer; OutBuffer: Pointer;
  OutBufferLength: Integer; OutBufferValidBytes: Integer; Status: Integer; var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterNotifyGetFileSecurity(Sender: TObject; const FileName: string;
  SecurityInformation: Integer; SecurityDescriptor: Pointer; Length: Integer; Status: Integer;
  var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterNotifyGetFileSizes(Sender: TObject; const FileName: string; Size: Int64;
  AllocationSize: Int64; Status: Integer; var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterNotifyIoctl(Sender: TObject; const FileName: string; IoControlCode: Integer;
  InBuffer: Pointer; InBufferLength: Integer; InBufferValidBytes: Integer; OutBuffer: Pointer;
  OutBufferLength: Integer; OutBufferValidBytes: Integer; Status: Integer; var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterNotifyLock(Sender: TObject; const FileName: string; Offset: Int64; Length: Int64;
  Key: Int64; FailImmediately: Boolean; ExclusiveLock: Boolean; Status: Integer; var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterNotifyOpenFile(Sender: TObject;
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
  { insert your code here }
end;

procedure TFormFilter.FilterNotifyQueryFileInfo(Sender: TObject; const FileName: string;
  FileInformationClass: Integer; Buffer: Pointer; BufferLength: Integer; ValidBytes: Integer;
  Status: Integer; var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterNotifyReadFile(Sender: TObject; const FileName: string; Position: Int64;
  BytesToRead: Integer; Direction: Integer; BytesRead : Integer; Status: Integer; var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterNotifyRenameOrMoveFile(Sender: TObject; const FileName: string;
  const NewFileName: string; Status: Integer; var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterNotifySetAllocationSize(Sender: TObject; const FileName: string;
  AllocationSize: Int64; Status: Integer; var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterNotifySetFileAttributes(Sender: TObject; const FileName: string;
  CreationTime: TDateTime; LastAccessTime: TDateTime; LastWriteTime: TDateTime; ChangeTime: TDateTime; FileAttributes: Integer;
  Status: Integer; var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterNotifySetFileSecurity(Sender: TObject; const FileName: string;
  SecurityInformation: Integer; SecurityDescriptor: Pointer; Length: Integer; Status: Integer;
  var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterNotifySetFileSize(Sender: TObject; const FileName: string; Size: Int64;
  Status: Integer; var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterNotifyUnlockAll(Sender: TObject; const FileName: string; Status: Integer;
  var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterNotifyUnlockAllByKey(Sender: TObject; const FileName: string; Key: Int64;
  Status: Integer; var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterNotifyUnlockSingle(Sender: TObject; const FileName: string; Offset: Int64;
  Length: Int64; Key: Int64; Status: Integer; var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterNotifyWriteFile(Sender: TObject; const FileName: string; Position: Int64;
  BytesToWrite: Integer; Direction: Integer; BytesWritten : integer; Status: Integer; var ResultCode: Integer);
begin
  { insert your code here }
end;

{ Control events }

procedure TFormFilter.FilterAfterCanFileBeDeleted(Sender: TObject;
  const FileName: String;
  RequestType: Integer;
  CanDelete: Boolean;
  var Status: Integer;
  var FileContext: Pointer;
  var HandleContext: Pointer;
  var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterAfterCleanupFile(Sender: TObject; const FileName: string; 
  var FileContext: Pointer; var HandleContext: Pointer; var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterAfterCloseEnumeration(Sender: TObject;
  const DirectoryName: String;
  DirectoryContext: Pointer;
  HandleContext: Pointer;
  EnumerationContext: Pointer;
  var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterAfterCloseFile(Sender: TObject; const FileName: string; 
  var FileContext: Pointer; var HandleContext: Pointer; var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterAfterCreateFile(Sender: TObject;
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
begin
  { insert your code here }
end;

procedure TFormFilter.FilterAfterCreateHardLink(Sender: TObject; const FileName: string; const LinkName: string;
  var Status: Integer; var FileContext: Pointer; var HandleContext: Pointer; var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterAfterDeleteFile(Sender: TObject;
  const FileName: String;
  RequestType: Integer;
  var FileContext: Pointer;
  var HandleContext: Pointer;
  var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterAfterEnumerateDirectory(Sender: TObject;
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
  { insert your code here }
  ProcessRequest := True;
end;

procedure TFormFilter.FilterAfterFsctl(Sender: TObject;
  const FileName: String;
  FsControlCode: Integer;
  InBuffer: Pointer;
  InBufferLength: Integer;
  InBufferValidBytes: Integer;
  OutBuffer: Pointer;
  OutBufferLength: Integer;
  var OutBufferValidBytes: Integer;
  var Status: Integer;
  var FileContext: Pointer;
  var HandleContext: Pointer;
  var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterAfterGetFileSecurity(Sender: TObject; const FileName: string; 
  SecurityInformation: Integer; SecurityDescriptor: Pointer; Length: Integer; var LengthNeeded: Integer;
  var Status: Integer; var FileContext: Pointer; var HandleContext: Pointer; var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterAfterGetFileSizes(Sender: TObject; const FileName: string; var Size: Int64;
  var AllocationSize: Int64; var Status: Integer; var FileContext: Pointer; var HandleContext: Pointer;
  var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterAfterIoctl(Sender: TObject;
  const FileName: String;
  IoControlCode: Integer;
  InBuffer: Pointer;
  InBufferLength: Integer;
  InBufferValidBytes: Integer;
  OutBuffer: Pointer;
  OutBufferLength: Integer;
  var OutBufferValidBytes: Integer;
  var Status: Integer;
  var FileContext: Pointer;
  var HandleContext: Pointer;
  var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterAfterLock(Sender: TObject; const FileName: string; Offset: Int64; 
  Length: Int64; Key: Int64; FailImmediately: Boolean; ExclusiveLock: Boolean; var Status: Integer; 
  var FileContext: Pointer; var HandleContext: Pointer; var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterAfterOpenFile(Sender: TObject;
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
begin
  { insert your code here }
end;

procedure TFormFilter.FilterAfterQueryFileInfo(Sender: TObject; const FileName: string; 
  FileInformationClass: Integer; Buffer: Pointer; BufferLength: Integer; var ValidBytes: Integer; 
  var Status: Integer; var FileContext: Pointer; var HandleContext: Pointer; var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterAfterReadFile(Sender: TObject;
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
begin
  { insert your code here }
end;

procedure TFormFilter.FilterAfterRenameOrMoveFile(Sender: TObject; const FileName: string; 
  const NewFileName: string; var Status: Integer; var FileContext: Pointer; var HandleContext: Pointer; 
  var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterAfterSetAllocationSize(Sender: TObject; const FileName: string; 
  AllocationSize: Int64; var Status: Integer; var FileContext: Pointer; var HandleContext: Pointer; 
  var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterAfterSetFileAttributes(Sender: TObject; const FileName: string; 
  CreationTime: TDateTime; LastAccessTime: TDateTime; LastWriteTime: TDateTime; ChangeTime: TDateTime; FileAttributes: Integer; 
  var Status: Integer; var FileContext: Pointer; var HandleContext: Pointer; var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterAfterSetFileSecurity(Sender: TObject; const FileName: string; 
  SecurityInformation: Integer; SecurityDescriptor: Pointer; Length: Integer; var Status: Integer; 
  var FileContext: Pointer; var HandleContext: Pointer; var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterAfterSetFileSize(Sender: TObject; const FileName: string; Size: Int64; 
  var Status: Integer; var FileContext: Pointer; var HandleContext: Pointer; var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterAfterUnlockAll(Sender: TObject; const FileName: string; var Status: Integer;
  var FileContext: Pointer; var HandleContext: Pointer; var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterAfterUnlockAllByKey(Sender: TObject; const FileName: string; Key: Int64; 
  var Status: Integer; var FileContext: Pointer; var HandleContext: Pointer; var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterAfterUnlockSingle(Sender: TObject; const FileName: string; Offset: Int64; 
  Length: Int64; Key: Int64; var Status: Integer; var FileContext: Pointer; var HandleContext: Pointer; 
  var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterAfterWriteFile(Sender: TObject; const FileName: string; Position: Int64; 
  Buffer: Pointer; BufferLength : Integer; BytesToWrite: Integer; Direction: Integer; var BytesWritten: Integer; 
  var Status: Integer; var FileContext: Pointer; var HandleContext: Pointer; var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterBeforeCanFileBeDeleted(Sender: TObject;
  const FileName: String;
  RequestType: Integer;
  var CanDelete: Boolean;
  var FileContext: Pointer;
  var HandleContext: Pointer;
  var ProcessRequest: Boolean;
  var ResultCode: Integer);
begin
  { insert your code here }
  ProcessRequest := True;
end;

procedure TFormFilter.FilterBeforeCleanupFile(Sender: TObject; const FileName: string; var FileContext: Pointer;
  var HandleContext: Pointer; var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterBeforeCloseFile(Sender: TObject; const FileName: string; var FileContext: Pointer; 
  var HandleContext: Pointer; var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterBeforeCreateFile(Sender: TObject;
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
  { insert your code here }
  ProcessRequest := True;
end;

procedure TFormFilter.FilterBeforeCreateHardLink(Sender: TObject; const FileName: string; const LinkName: String; 
  var ReplaceIfExists: Boolean; var FileContext: Pointer; var HandleContext: Pointer; 
  var ProcessRequest: Boolean; var ResultCode: Integer);
begin
  { insert your code here }
  ProcessRequest := True;
end;

procedure TFormFilter.FilterBeforeDeleteFile(
  Sender: TObject;
  const FileName: String;
  RequestType: Integer;
  var FileContext: Pointer;
  var HandleContext: Pointer;
  var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterBeforeFsctl(Sender: TObject;
  const FileName: String;
  FsControlCode: Integer;
  InBuffer: Pointer;
  InBufferLength: Integer;
  var InBufferValidBytes: Integer;
  OutBuffer: Pointer;
  OutBufferLength: Integer;
  var OutBufferValidBytes: Integer;
  var FileContext: Pointer;
  var HandleContext: Pointer;
  var ProcessRequest: Boolean;
  var ResultCode: Integer);
begin
  { insert your code here }
  ProcessRequest := True;
end;

procedure TFormFilter.FilterBeforeGetFileSecurity(Sender: TObject; const FileName: string; 
  SecurityInformation: Integer; SecurityDescriptor: Pointer; Length: Integer; var LengthNeeded: Integer;
  var FileContext: Pointer; var HandleContext: Pointer; var ProcessRequest: Boolean; var ResultCode: Integer);
begin
  { insert your code here }
  ProcessRequest := True;
end;

procedure TFormFilter.FilterBeforeIoctl(Sender: TObject;
  const FileName: String;
  IoControlCode: Integer;
  InBuffer: Pointer;
  InBufferLength: Integer;
  var InBufferValidBytes: Integer;
  OutBuffer: Pointer;
  OutBufferLength: Integer;
  var OutBufferValidBytes: Integer;
  var FileContext: Pointer;
  var HandleContext: Pointer;
  var ProcessRequest: Boolean;
  var ResultCode: Integer);
begin
  { insert your code here }
  ProcessRequest := True;
end;

procedure TFormFilter.FilterBeforeLock(Sender: TObject; const FileName: string; var Offset: Int64; 
  var Length: Int64; Key: Int64; var FailImmediately: Boolean; var ExclusiveLock: Boolean; 
  var FileContext: Pointer; var HandleContext: Pointer; var ProcessRequest: Boolean; var ResultCode: Integer);
begin
  { insert your code here }
  ProcessRequest := True;
end;

procedure TFormFilter.FilterBeforeOpenFile(Sender: TObject;
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
  { insert your code here }
  ProcessRequest := True;
end;

procedure TFormFilter.FilterBeforeQueryFileInfo(Sender: TObject; const FileName: string; 
  FileInformationClass: Integer; Buffer: Pointer; BufferLength: Integer; var ValidBytes: Integer; 
  var FileContext: Pointer; var HandleContext: Pointer; var ProcessRequest: Boolean; var ResultCode: Integer);
begin
  { insert your code here }
  ProcessRequest := True;
end;

procedure TFormFilter.FilterBeforeReadFile(Sender: TObject;
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
begin
  { insert your code here }

  // if you wish to stop the request with desired error status,
  // return FALSE in "ProcessRequest" parameter and set ResultCode 
  // to the desired Win32 error code

  // ResultCode := ERROR_ACCESS_DENIED;
  // ProcessRequest := false;

  // if you wish to complete request with status = success
  // without any actions performed by FSD handler, just set ProcessRequest to false:
  // ProcessRequest := false;

  // otherwise the request will be processed as usual

  ProcessRequest := true;
end;

procedure TFormFilter.FilterBeforeRenameOrMoveFile(Sender: TObject; const FileName: string; 
  const NewFileName: string; var ReplaceIfExists: Boolean; var FileContext: Pointer; 
  var HandleContext: Pointer; var ProcessRequest: Boolean; var ResultCode: Integer);
begin
  { insert your code here }
  ProcessRequest := True;
end;

procedure TFormFilter.FilterBeforeSetAllocationSize(Sender: TObject; const FileName: string; 
  var AllocationSize: Int64; var FileContext: Pointer; var HandleContext: Pointer; 
  var ProcessRequest: Boolean; var ResultCode: Integer);
begin
  { insert your code here }
  ProcessRequest := True;
end;

procedure TFormFilter.FilterBeforeSetFileAttributes(Sender: TObject; const FileName: string; 
  var CreationTime: TDateTime; var LastAccessTime: TDateTime; var LastWriteTime: TDateTime; var ChangeTime: TDateTime; 
  var FileAttributes: Integer; var FileContext: Pointer; var HandleContext: Pointer; 
  var ProcessRequest: Boolean; var ResultCode: Integer);
begin
  { insert your code here }
  ProcessRequest := True;
end;

procedure TFormFilter.FilterBeforeSetFileSecurity(Sender: TObject; const FileName: String; 
  SecurityInformation: Integer; SecurityDescriptor: Pointer; Length: Integer; var FileContext: Pointer; 
  var HandleContext: Pointer; var ProcessRequest: Boolean; var ResultCode: Integer);
begin
  { insert your code here }
  ProcessRequest := True;
end;

procedure TFormFilter.FilterBeforeSetFileSize(Sender: TObject; const FileName: string; var Size: Int64; 
  var FileContext: Pointer; var HandleContext: Pointer; var ProcessRequest: Boolean; var ResultCode: Integer);
begin
  { insert your code here }
  ProcessRequest := True;
end;

procedure TFormFilter.FilterBeforeUnlockAll(Sender: TObject; const FileName: string; var FileContext: Pointer;
  var HandleContext: Pointer; var ProcessRequest: Boolean; var ResultCode: Integer);
begin
  { insert your code here }
  ProcessRequest := True;
end;

procedure TFormFilter.FilterBeforeUnlockAllByKey(Sender: TObject; const FileName: string; Key: Int64;
  var FileContext: Pointer; var HandleContext: Pointer; var ProcessRequest: Boolean; var ResultCode: Integer);
begin
  { insert your code here }
  ProcessRequest := True;
end;

procedure TFormFilter.FilterBeforeUnlockSingle(Sender: TObject; const FileName: string; var Offset: Int64;
  var Length: Int64; Key: Int64; var FileContext: Pointer; var HandleContext: Pointer;
  var ProcessRequest: Boolean; var ResultCode: Integer);
begin
  { insert your code here }
  ProcessRequest := True;
end;

procedure TFormFilter.FilterBeforeWriteFile(Sender: TObject;
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
begin
  { insert your code here }

  // if you wish to stop the request with desired error status,
  // return FALSE in "ProcessRequest" parameter and set ResultCode 
  // to the desired Win32 error code

  // ResultCode := ERROR_ACCESS_DENIED;
  // ProcessRequest := false;

  // if you wish to complete request with status = success
  // without any actions performed by FSD handler, just set ProcessRequest to false:
  // ProcessRequest := false;

  // otherwise the request will be processed as usual

  ProcessRequest := true;
end;

procedure TFormFilter.FilterReparseFileName(Sender: TObject; const FileName: string; DesiredAccess: Integer;
  const ReparsedFileName: string; var NewFileName: string; var ResultCode: Integer);
begin
  { insert your code here }
end;

procedure TFormFilter.FilterReparseWithTag(Sender: TObject;
  const FileName: String;
  const NewFileName: String;
  ReparseTag: Integer;
  ReparseBuffer: Pointer;
  ReparseBufferLength: Integer;
  var ReissueIO: Boolean;
  var ResultCode: Integer);
begin
  { insert your code here }
end;

end.




