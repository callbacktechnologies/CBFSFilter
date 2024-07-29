# 
# CBFS Filter 2024 Python Edition - Sample Project
# 
# This sample project demonstrates the usage of CBFS Filter in a 
# simple, straightforward way. It is not intended to be a complete 
# application. Error handling and other checks are simplified for clarity.
# 
# www.callback.com/cbfsfilter
# 
# This code is subject to the terms and conditions specified in the 
# corresponding product license agreement which outlines the authorized 
# usage and restrictions.
# 

import sys
import string
from cbfsfilter import *

input = sys.hexversion<0x03000000 and raw_input or input

def ensureArg(args, prompt, index):
  if len(args) <= index:
    while len(args) <= index:
      args.append(None)
    args[index] = input(prompt)
  elif args[index] is None:
    args[index] = input(prompt)


from threading import Lock
import ctypes

from cbfsfilter import *

guid = "{713CC6CE-B3E2-4fd9-838D-E28F558F6866}"
ALTITUDE_FAKE_VALUE_FOR_DEBUG = "360000.24"
DEFAULT_CLUSTER_SIZE:int = 4096 # this value is specific to the system configuration and must be obtained on initialization
Password = "isolatedencryptionDemo"

ERROR_ACCESS_DENIED = 5
ERROR_NOT_SUPPORTED = 50
ERROR_PRIVILEGE_NOT_HELD = 1314

context_holder = {}
holder_lock = Lock()
holder_key_counter = 0

def allocContext(context):
    global context_holder, holder_lock, holder_key_counter
    with holder_lock:
        holder_key_counter += 1
        context_holder[holder_key_counter] = context
        return holder_key_counter
def getContext(id:int):
    global context_holder, holder_lock, holder_key_counter
    with holder_lock:
        if id in context_holder:
            return context_holder[id]
        else:
            return None
def freeContext(id:int):
    global context_holder, holder_lock, holder_key_counter
    with holder_lock:
        context_holder.pop(id)

class FileHeader:
    HEADER_SIZE:int = 512

    def __init__(self, masterKey:bytes) -> None:
        self.headerTag = "@@@!!!cbfilter isolatedencryption!!!@@@".encode("utf-8")

        self.present = False

        self.masterXoxKey = c_ubyte(0)
        for keyData in masterKey:
            self.masterXoxKey.value += c_ubyte(keyData).value

        self.sessionKey = c_ubyte(0)
        self.sessionKey.value = int(datetime.now().timestamp())

        self.authTag = c_ubyte(ord("x"))

        self.headerData = bytearray(bytes(FileHeader.HEADER_SIZE))

    def getPresent(self) : return self.present

    def parseHeader(self, fileHandle:CBFSFilterStream):
        position = 0
        bytesToRead = FileHeader.HEADER_SIZE

        fileHandle.seek(position)

        try:
            data = fileHandle.read(bytesToRead)
        except CBFSFilterError as e:
            return e.code

        for i in range(0, len(data)):
            self.headerData[i] = data[i]

        for i in range(0, len(self.headerTag)):
            # It is not an encrypted file.
            if self.headerTag[i] != self.headerData[i]:
                return 0

        if c_ubyte(self.headerData[len(self.headerTag)]).value ^ self.masterXoxKey.value != self.authTag.value:
            return 86 #ERROR_INVALID_PASSWORD

        self.sessionKey.value = c_ubyte(self.headerData[len(self.headerTag) + 1]).value ^ self.masterXoxKey.value

        self.present = True

        return 0

    def setupHeader(self):
        self.headerData = bytearray(bytes(FileHeader.HEADER_SIZE))

        for i in range(0, len(self.headerTag)):
            self.headerData[i] = self.headerTag[i]

        value = c_ubyte(0)
        value.value = self.authTag.value ^ self.masterXoxKey.value
        self.headerData[len(self.headerTag)] = value.value
        value.value = self.sessionKey.value ^ self.masterXoxKey.value
        self.headerData[len(self.headerTag) + 1] = value.value

        self.present = False

        return 0
    
    def writeHeader(self, fileHandle:CBFSFilterStream):
        position = 0

        try:
            fileHandle.seek(position)
            fileHandle.write(bytes(self.headerData))
        except CBFSFilterError as e:
            return e.code

        self.present = True

        return 0

    def encryptBuffer(self, buffer:c_void_p, bytesToWrite:int, bufferLength:int):
        assert(bufferLength % 512 == 0)

        byte_array = (c_ubyte * bufferLength).from_address(buffer.value)

        for i in range(0, bytesToWrite):
            byte_array[i] = byte_array[i] ^ self.sessionKey.value

    def decryptBuffer(self, buffer:c_void_p, bytesRead:int, bufferLength:int):
        assert(bufferLength % 512 == 0)

        byte_array = (c_ubyte * bufferLength).from_address(buffer.value)

        for i in range(0, bytesRead):
            byte_array[i] = byte_array[i] ^ self.sessionKey.value


def ROUND_TO_CLUSTER(value:int)->int:
    return ((value + DEFAULT_CLUSTER_SIZE - 1) & ~(DEFAULT_CLUSTER_SIZE - 1))

class FileContext:
    MASTER_KEY_SIZE = 16

    def __init__(self, filter:CBFilter, fileName:str, password:str) -> None:
        self.filter = filter
        self.fileName = fileName
        self.fileHandle:CBFSFilterStream = None
        self.fileHandleRaw:int = -1
        self.fileSize = 0

        keyBytes = (password + "abcdefghijklmnopqrst").encode("utf-8")

        self.masterKey = keyBytes[:FileContext.MASTER_KEY_SIZE]

        self.fileHeader = FileHeader(self.masterKey)

    def getFileName(self) : return self.fileName

    def getHeaderPresent(self) : return self.fileHeader.getPresent()

    def getActualFileSize(self) : return self.fileSize

    def getCalcedFileSize(self) : return self.getActualFileSize() - FileHeader.HEADER_SIZE if self.getHeaderPresent() else self.getActualFileSize()

    def getCalcedHeaderSize(self) : return FileHeader.HEADER_SIZE if self.getHeaderPresent() else 0

    def initialize(self, desiredAccess:int, shareMode:int, creationDisposition:int, flagsAndAttributes:int)->int :
        errorCode = 0
        auxiHandle:CBFSFilterStream = None
        operHandle:CBFSFilterStream = None
        operHandleRaw = -1

        while True:

            handleHolder = [0]

            try:
                self.fileHandle = self.filter.create_file_direct_as_stream(self.fileName, False,
                                                                           desiredAccess, shareMode, creationDisposition,
                                                                           flagsAndAttributes, False, handleHolder)
                self.fileHandleRaw = handleHolder[0]
            except CBFSFilterError as e:
                errorCode = e.code
                break
        
            operHandle = self.fileHandle
            operHandleRaw = self.fileHandleRaw

            if desiredAccess & cbfsfilter.DESIRED_ACCESS_FILE_READ_ATTRIBUTES == 0:
                try:
                    auxiHandle = self.filter.create_file_direct_as_stream(self.fileName, False,
                                                                          cbfsfilter.DESIRED_ACCESS_FILE_GENERIC_READ,
                                                                          cbfsfilter.FILESYS_SHARE_READ | cbfsfilter.FILESYS_SHARE_WRITE | cbfsfilter.FILESYS_SHARE_DELETE |
                                                                          cbfsfilter.CBFILTER_IGNORE_SHARE_ACCESS_CHECK,
                                                                          cbfsfilter.FILE_DISPOSITION_OPEN_EXISTING,
                                                                          0x02000000,#FILE_FLAG_BACKUP_SEMANTICS
                                                                          False, handleHolder)
                except CBFSFilterError as e:
                    errorCode = e.code
                    break

                operHandle = auxiHandle
                operHandleRaw = handleHolder[0]
            '''
            typedef struct _FILE_BASIC_INFORMATION {
                LARGE_INTEGER CreationTime;
                LARGE_INTEGER LastAccessTime;
                LARGE_INTEGER LastWriteTime;
                LARGE_INTEGER ChangeTime;
                ULONG FileAttributes;
            } FILE_BASIC_INFORMATION, *PFILE_BASIC_INFORMATION;
            '''

            basicInfoBuff = bytearray(5 * 8)
            try:
                bytesWritten = [0]
                self.filter.query_file_information_direct(operHandleRaw, basicInfoBuff, 4, #FileBasicInformation
                                                          bytesWritten)
            except CBFSFilterError as e:
                errorCode = e.code
                break

            intBasicBuffer = (c_int32 * 10).from_buffer(basicInfoBuff)
            if intBasicBuffer[8] & cbfsfilter.FILE_SYS_ATTR_DIRECTORY == cbfsfilter.FILE_SYS_ATTR_DIRECTORY:
                errorCode = ERROR_NOT_SUPPORTED
                break

            self.fileSize = operHandle.length

            if self.fileSize >= FileHeader.HEADER_SIZE:
                if auxiHandle is None and desiredAccess & cbfsfilter.DESIRED_ACCESS_FILE_READ_DATA == 0:
                    try:
                        auxiHandle = self.filter.create_file_direct_as_stream(self.fileName, False,
                                                                              cbfsfilter.DESIRED_ACCESS_FILE_GENERIC_READ,
                                                                              cbfsfilter.FILESYS_SHARE_READ | cbfsfilter.FILESYS_SHARE_WRITE | cbfsfilter.FILESYS_SHARE_DELETE |
                                                                              cbfsfilter.CBFILTER_IGNORE_SHARE_ACCESS_CHECK,
                                                                              cbfsfilter.FILE_DISPOSITION_OPEN_EXISTING,
                                                                              0x02000000,#FILE_FLAG_BACKUP_SEMANTICS
                                                                              False, handleHolder)
                    except CBFSFilterError as e:
                        errorCode = e.code
                        break
                    
                    operHandle = auxiHandle
                    operHandleRaw = handleHolder[0]

                errorCode = self.fileHeader.parseHeader(operHandle)
                if errorCode != 0:
                    break
            
            if not self.getHeaderPresent():
                errorCode = self.fileHeader.setupHeader()
                if errorCode != 0:
                    break

            break

        if auxiHandle is not None:
            auxiHandle.close()

        if errorCode != 0:
            self.close()

        return errorCode

    def onQueryFileInfo(self, buffer:c_void_p, length:int, fileInfoClass:int, bytesWritten:list[int])->int:
        errorCode = 0

        infoBuffer = (c_ubyte * length).from_address(buffer.value)

        try:
            self.filter.query_file_information_direct(self.fileHandleRaw, infoBuffer, fileInfoClass, bytesWritten)
        except CBFSFilterError as e:
            errorCode = e.code
        
        if errorCode == 0 or errorCode == 234: #ERROR_MORE_DATA
            '''
            typedef struct _FILE_ALL_INFORMATION {
               FILE_BASIC_INFORMATION BasicInformation;
               FILE_STANDARD_INFORMATION StandardInformation;
                ...
            } FILE_ALL_INFORMATION, *PFILE_ALL_INFORMATION
            typedef struct _FILE_BASIC_INFORMATION {
                LARGE_INTEGER CreationTime;
                LARGE_INTEGER LastAccessTime;
                LARGE_INTEGER LastWriteTime;
                LARGE_INTEGER ChangeTime;
                ULONG FileAttributes;
            } FILE_BASIC_INFORMATION, *PFILE_BASIC_INFORMATION
            typedef struct _FILE_STANDARD_INFORMATION {
                LARGE_INTEGER AllocationSize;
                LARGE_INTEGER EndOfFile;
                ULONG NumberOfLinks;
                BOOLEAN DeletePending;
                BOOLEAN Directory;
            } FILE_STANDARD_INFORMATION, *PFILE_STANDARD_INFORMATION;
            '''
            if fileInfoClass == 18: #FileAllInformation
                int64_array = (c_int64 * 6).from_address(buffer.value)
                int64_array[5] = ROUND_TO_CLUSTER(self.getCalcedFileSize()) #AllocationSize
                int64_array[6] = self.getCalcedFileSize() #EndOfFile
            elif fileInfoClass == 5: #FileStandardInformation
                int64_array = (c_int64 * 2).from_address(buffer.value)
                int64_array[0] = ROUND_TO_CLUSTER(self.getCalcedFileSize()) #AllocationSize
                int64_array[1] = self.getCalcedFileSize() #EndOfFile

        return errorCode

    def onSetFileInfo(self, buffer:c_void_p, length:int, fileInfoClass:int)->int:
        newActualSize = -1

        if fileInfoClass == 19: #FileAllocationInformation
            '''
            typedef struct _FILE_ALLOCATION_INFORMATION {
                LARGE_INTEGER AllocationSize;
            } FILE_ALLOCATION_INFORMATION, *PFILE_ALLOCATION_INFORMATION;
            '''
            if length >= 8:
                int64_array = (c_int64 * 1).from_address(buffer.value)
                if int64_array[0] < self.getCalcedFileSize():
                    int64_array[0] = int64_array[0] + self.getCalcedHeaderSize()
                    newActualSize = int64_array[0]

                    # We directly replace it with FileEndOfFileInformation.
                    fileInfoClass = 20 #FileEndOfFileInformation
                else:
                    int64_array[0] = int64_array[0] + self.getCalcedHeaderSize()
                    int64_array[0] = ROUND_TO_CLUSTER(int64_array[0])
            else:
                return 87 #ERROR_INVALID_PARAMETER
        elif fileInfoClass == 20: #FileEndOfFileInformation
            '''
            typedef struct _FILE_END_OF_FILE_INFORMATION {
                LARGE_INTEGER EndOfFile;
            } FILE_END_OF_FILE_INFORMATION, *PFILE_END_OF_FILE_INFORMATION;
            '''
            if length >= 8:
                int64_array = (c_int64 * 1).from_address(buffer.value)
                int64_array[0] = int64_array[0] + self.getCalcedHeaderSize()
                newActualSize = int64_array[0]
            else:
                return 87 #ERROR_INVALID_PARAMETER
        
        errorCode = 0
        try:
            self.filter.set_file_information_direct(self.fileHandleRaw, fileInfoClass, (c_ubyte * length).from_address(buffer.value))
        except CBFSFilterError as e:
            errorCode = e.code

        if errorCode == 0 and newActualSize != -1:
            self.fileSize = newActualSize

        return errorCode

    def onQuerySecurity(self, securityInfo:int, securityDescriptor:c_void_p, length:int, lenNeeded:list[int])->int:
        errorCode = 0

        secuBuffer = (c_ubyte * length).from_address(securityDescriptor.value)
        try:
            self.filter.query_file_security_direct(self.fileHandleRaw, securityInfo, secuBuffer, lenNeeded)
        except CBFSFilterError as e:
            errorCode = e.code

        return errorCode

    def onSetSecurity(self, securityInfo:int, securityDescriptor:c_void_p, length:int)->int:
        errorCode = 0

        buffer = (ctypes.c_ubyte * length).from_address(securityDescriptor.value)

        try:
            self.filter.set_file_security_direct(self.fileHandleRaw, securityInfo, buffer)
        except CBFSFilterError as e:
            errorCode = e.code

        return errorCode

    def onCachedWriteExtendFileSize(self, size:int)->int:
        newActualSize = size + self.getCalcedHeaderSize()

        try:
            self.filter.set_file_size_direct(self.fileHandleRaw, newActualSize)
        except CBFSFilterError as e:
            return e.code

        self.fileSize = newActualSize

        return 0

    def onNonCachedRead(self, buffer:c_void_p, length:int, position:int, bytesToRead:int, bytesRead:list[int])->int:
        bufferLength = length

        assert(bufferLength % 512 == 0)

        if position > self.getCalcedFileSize():
            return 38 #ERROR_HANDLE_EOF
        
        if position + bytesToRead >= self.getCalcedFileSize():
            bytesToRead = self.getCalcedFileSize() - position

        position += self.getCalcedHeaderSize()

        try:
            self.fileHandle.seek(position)
            data = self.fileHandle.read(bytesToRead)
            bytesRead[0] = len(data)
            byte_array = (c_ubyte * bufferLength).from_address(buffer.value)
            for i in range(0, len(data)):
                byte_array[i] = data[i]
        except CBFSFilterError as e:
            return e.code

        if self.getHeaderPresent():
            self.fileHeader.decryptBuffer(buffer, bytesRead[0], bufferLength)

        return 0

    def onNonCachedWrite(self, buffer:c_void_p, length:int, position:int, bytesToWrite:int, bytesWritten:list[int])->int:
        bufferLength = length

        assert(bufferLength % 512 == 0)

        if position > self.getCalcedFileSize():
           return 0
        
        if position + bytesToWrite >= self.getCalcedFileSize():
           bytesToWrite = self.getCalcedFileSize() - position

        self.fileHeader.encryptBuffer(buffer, bytesToWrite, bufferLength)

        position += self.getCalcedHeaderSize()

        try:
            self.fileHandle.seek(position)
            bytesWritten[0] = self.fileHandle.write(bytes((ctypes.c_ubyte * bytesToWrite).from_address(buffer.value)))
        except CBFSFilterError as e:
            return e.code
        
        return 0

    def encryptFile(self)->int:
        assert(not self.getHeaderPresent())

        dataSize = self.fileSize

        try:
            self.filter.set_file_size_direct(self.fileHandleRaw, dataSize + FileHeader.HEADER_SIZE)
        except CBFSFilterError as e:
            return e.code
        
        buffer = bytearray(FileHeader.HEADER_SIZE)

        position = dataSize // len(buffer) * len(buffer)
        validDataLen = dataSize % len(buffer)

        while True:
            if validDataLen != 0:
                try:
                    self.fileHandle.seek(position)
                    data = self.fileHandle.read(validDataLen)
                    byteBuff = (c_ubyte * len(buffer)).from_buffer(data)
                    self.fileHeader.encryptBuffer(cast(byteBuff, c_void_p), len(data), len(byteBuff))
                    writePos = position + FileHeader.HEADER_SIZE
                    self.fileHandle.seek(writePos)
                    self.fileHandle.write(bytes(byteBuff[:len(data)]))
                except CBFSFilterError as e:
                    return e.code

            position -= FileHeader.HEADER_SIZE
            validDataLen = FileHeader.HEADER_SIZE

            if position < 0:
                break

        result = self.fileHeader.writeHeader(self.fileHandle)

        if result == 0:
            self.fileSize += FileHeader.HEADER_SIZE

        return result

    def onRenameOrMoveFile(self, newFileName:str, replaceIfExists:bool)->int:
        '''
        typedef struct _FILE_RENAME_INFORMATION {
            BOOLEAN ReplaceIfExists;
            HANDLE RootDirectory;
            ULONG FileNameLength;
            WCHAR FileName[1];
        } FILE_RENAME_INFORMATION, *PFILE_RENAME_INFORMATION;
        '''
        try:
            buffer = bytearray(0x20 + len(newFileName) * 2)
            byteBuff = (c_ubyte * 8).from_buffer(buffer)
            intBuff = (c_int32 * 6).from_buffer(buffer)
            charBuff = (c_wchar * 0x10 + len(newFileName))

            byteBuff[0] = 1 if replaceIfExists else 0
            intBuff[4] = len(newFileName) * 2

            for i in range(0, len(newFileName)):
                charBuff[10 + i] = ord(newFileName[i])

            self.filter.set_file_information_direct(self.fileHandleRaw, 10, #FileRenameInformation
                                                    buffer)
        except CBFSFilterError as e:
            return e.code
        
        return 0

    def close(self) :
        if self.fileHandle is not None:
            self.fileHandle.close()
            self.fileHandle = None
            self.fileHandleRaw = -1

class isolatedencryption:
    def __init__(self) -> None:
        self.filter = CBFilter()

        self.logging = False
        self.whitelistProcs = []

    def start(self, pathToIsolate:str, whitelistProcs:str, logging: bool):
        self.logging = logging
        self.whitelistProcs = whitelistProcs.split(sep=",")

        self.filter.on_before_create_file = self.beforeCreateFile
        self.filter.on_before_open_file = self.beforeOpenFile
        self.filter.on_before_query_file_info = self.beforeQueryFileInfo
        self.filter.on_before_set_file_info = self.beforeSetFileInfo
        self.filter.on_before_get_file_security = self.beforeGetFileSecurity
        self.filter.on_before_set_file_security = self.beforeSetFileSecurity
        self.filter.on_before_read_file = self.beforeReadFile
        self.filter.on_before_write_file = self.beforeWriteFile
        self.filter.on_before_rename_or_move_file = self.beforeRenameOrMoveFile
        self.filter.on_after_enumerate_directory = self.afterEnumerateDirectory
        self.filter.on_cleanup_context = self.cleanupContext

        self.filter.on_error = self.error

        try:
            self.filter.initialize(guid)

            self.filter.add_filter_rule_ex(pathToIsolate, 
                                           "",
                                           cbfsfilter.ACCESS_NONE,
                                           cbfsfilter.FS_CE_AFTER_ENUMERATE_DIRECTORY,
                                           cbfsfilter.FS_NE_NONE,
                                           -1, -1,
                                           cbfsfilter.FILE_SYS_ATTR_DIRECTORY,
                                           0)

            self.filter.add_filter_rule_ex(pathToIsolate + "\\*.*",
                                           "",
                                           cbfsfilter.ACCESS_NONE,
                                           cbfsfilter.FS_CE_BEFORE_CREATE |
                                           cbfsfilter.FS_CE_BEFORE_OPEN |
                                           cbfsfilter.FS_CE_BEFORE_QUERY_FILE_INFO |
                                           cbfsfilter.FS_CE_BEFORE_SET_FILE_INFO |
                                           cbfsfilter.FS_CE_BEFORE_GET_SECURITY |
                                           cbfsfilter.FS_CE_BEFORE_SET_SECURITY |
                                           cbfsfilter.FS_CE_BEFORE_READ |
                                           cbfsfilter.FS_CE_BEFORE_WRITE |
                                           cbfsfilter.FS_CE_BEFORE_RENAME |
                                           cbfsfilter.FS_CE_AFTER_ENUMERATE_DIRECTORY,
                                           cbfsfilter.FS_NE_NONE,
                                           -1, -1,
                                           0,
                                           cbfsfilter.FILE_SYS_ATTR_DIRECTORY
                                           )

            self.filter.set_process_cached_io_requests(True)

            self.filter.initialize(guid)
            self.filter.config("AllowFileAccessInBeforeOpen=false")
            self.filter.config("ModifiableReadWriteBuffers=true")

            self.filter.start_filter(0)
        except CBFSFilterError as e:
            print("Error in setFilter: " + e.message)

    def stop(self):
        self.filter.delete_all_filter_rules()
        if self.filter.active:
            self.filter.stop_filter()

    def AddToLog(self, s:str):
        if (self.logging == False):
            return
        print(s)

    def beforeCreateFile(self, e:CBFilterBeforeCreateFileEventParams):
        fileContext:FileContext=None

        procName:str=self.filter.get_originator_process_name()

        fileContextHolder:list[FileContext]=[None]

        e.result_code = self.onCreateOrOpenFile(e.file_name, e.desired_access, e.share_mode, e.create_disposition, e.options, e.attributes, procName, fileContextHolder)

        fileContext = fileContextHolder[0]

        if fileContext is not None:
            assert(e.result_code == 0)

            e.isolate = True
            e.backend_file_name = ""

            e.file_context = allocContext(fileContext)

            e.process_request = True

    def beforeOpenFile(self, e:CBFilterBeforeOpenFileEventParams):
        fileContext:FileContext=None

        procName:str=self.filter.get_originator_process_name()

        fileContextHolder:list[FileContext]=[None]

        e.result_code = self.onCreateOrOpenFile(e.file_name, e.desired_access, e.share_mode, e.create_disposition, e.options, e.attributes, procName, fileContextHolder)

        fileContext = fileContextHolder[0]

        if fileContext is not None:
            assert(e.result_code == 0)

            e.isolate = True
            e.backend_file_name = ""

            e.file_context = allocContext(fileContext)

            e.process_request = True

    def onCreateOrOpenFile(self, fileName:str, desiredAccess:int, shareMode:int, disposition:int, options:int, attributes:int, procName:str, fileContextHolder:list[FileContext])->int:
        result:int = 0
        isolated:bool = False

        if (attributes & cbfsfilter.FILE_SYS_ATTR_DIRECTORY) != 0:
            return result
    
        procName = procName.lower()
        for whitelistProc in self.whitelistProcs:
            if procName.endswith("\\" + whitelistProc.lower()):
                isolated = True
                break

        if not isolated:
            return result

        self.AddToLog(f"beforeCreate/OpenFile {procName} {fileName}")\
        
        context = FileContext(self.filter, fileName, Password)

        intResult = context.initialize(desiredAccess, shareMode, disposition, options | attributes)

        if (intResult == ERROR_NOT_SUPPORTED):
            return result

        result = intResult

        if intResult == 0:
            fileContextHolder[0] = context

        return result

    def beforeQueryFileInfo(self, e:CBFilterBeforeQueryFileInfoEventParams):
        fileContext:FileContext = None

        if (e.file_context == 0):
            return
        
        self.AddToLog(f"beforeQueryFileInfo {e.file_name}")

        fileContext = getContext(e.file_context)

        bytesWrittenHolder:list[int] = [0]

        e.result_code = fileContext.onQueryFileInfo(e.buffer, e.buffer_length, e.file_information_class, bytesWrittenHolder)

        e.valid_bytes = bytesWrittenHolder[0]
        e.process_request = False

    def beforeSetFileInfo(self, e:CBFilterBeforeSetFileInfoEventParams):
        fileContext:FileContext = None

        # Rename operations will be processed in CBFSFltBeforeRenameOrMoveFile

        # FileRenameInformation or FileRenameInformationEx
        if e.file_information_class == 10 or e.file_information_class == 65:
            return
        
        if (e.file_context == 0):
            return
        
        self.AddToLog(f"beforeSetFileInfo {e.file_name}")

        fileContext = getContext(e.file_context)

        e.result_code = fileContext.onSetFileInfo(e.buffer, e.buffer_length, e.file_information_class)
        e.process_request = False

    def beforeGetFileSecurity(self, e:CBFilterBeforeGetFileSecurityEventParams):
        fileContext:FileContext = None

        if (e.file_context == 0):
            return
        
        self.AddToLog(f"beforeGetFileSecurity {e.file_name}")

        fileContext = getContext(e.file_context)

        lengthNeededHolder:list[int] = [0]

        e.result_code = fileContext.onQuerySecurity(e.security_information, e.security_descriptor, e.length, lengthNeededHolder)

        e.length_needed = lengthNeededHolder[0]
        e.process_request = False

    def beforeSetFileSecurity(self, e:CBFilterBeforeSetFileSecurityEventParams):
        fileContext:FileContext = None

        if (e.file_context == 0):
            return

        self.AddToLog(f"beforeSetFileSecurity {e.file_name}")

        fileContext = getContext(e.file_context)

        e.result_code = fileContext.onSetSecurity(e.security_information, e.security_descriptor, e.length)
        e.process_request = False

    def beforeReadFile(self, e:CBFilterBeforeReadFileEventParams):
        fileContext:FileContext = None

        if (e.file_context == 0):
            return

        self.AddToLog(f"beforeReadFile {e.file_name}")

        fileContext = getContext(e.file_context)

        if e.direction == cbfsfilter.FS_REQUEST_DIR_USER_CACHED or e.direction == cbfsfilter.FS_REQUEST_DIR_SYSTEM_CACHED:
            pass
        else:
            bytesReadHolder:list[int] = [0]

            e.result_code = fileContext.onNonCachedRead(e.buffer, e.buffer_length, e.position, e.bytes_to_read, bytesReadHolder)

            e.bytes_to_read   = bytesReadHolder[0]
            e.process_request = False

    def beforeWriteFile(self, e:CBFilterBeforeWriteFileEventParams):
        fileContext:FileContext = None

        if (e.file_context == 0):
            return

        self.AddToLog(f"beforeWriteFile {e.file_name}")

        fileContext = getContext(e.file_context)

        if not fileContext.getHeaderPresent():
            e.result_code = fileContext.encryptFile()
            if e.result_code != 0:
                return

        if e.direction == cbfsfilter.FS_REQUEST_DIR_USER_CACHED or e.direction == cbfsfilter.FS_REQUEST_DIR_SYSTEM_CACHED:
            if e.position + e.bytes_to_write > fileContext.getCalcedFileSize():
                e.result_code = fileContext.onCachedWriteExtendFileSize(e.position + e.bytes_to_write)
        else:
            bytesWrittenHolder:list[int] = [0]

            e.result_code = fileContext.onNonCachedWrite(e.buffer, e.buffer_length, e.position, e.bytes_to_write, bytesWrittenHolder)

            e.bytes_to_write  = bytesWrittenHolder[0]
            e.process_request = False

    def beforeRenameOrMoveFile(self, e:CBFilterBeforeRenameOrMoveFileEventParams):
        fileContext:FileContext = None

        fielName = e.file_name
        newFileName = e.new_file_name

        self.AddToLog(f"beforeRenameOrMoveFile {fielName} => {newFileName}")

        if e.file_context != 0:
            if not self.filter.is_file_filtered(newFileName):
                e.result_code = 17 #ERROR_NOT_SAME_DEVICE
                return
            
            fileContext = getContext(e.file_context)
            
            e.result_code = fileContext.onRenameOrMoveFile(newFileName, e.replace_if_exists)
            e.process_request = False
        else:
            procName = self.filter.get_originator_process_name()

            procName = procName.lower()

            isWhiteProc = False
            for whitelistProc in self.whitelistProcs:
                if procName.endswith("\\" + whitelistProc.lower()):
                    isWhiteProc = True
                    break

            if isWhiteProc:
                e.result_code = 17 #ERROR_NOT_SAME_DEVICE
                return

            # The event is in filter mode at this point and we let the request go forward.
            e.process_request = True

    def afterEnumerateDirectory(self, e:CBFilterAfterEnumerateDirectoryEventParams):
        procName = self.filter.get_handle_creator_process_name()

        procName = procName.lower()

        '''
        we presume that all files in directory are filtered and encrypted,
        but we provide the real size of decrypted data only to those processes that will receive this decrypted data
        '''
        for whitelistProc in self.whitelistProcs:
            if procName.endswith("\\" + whitelistProc.lower()):
                if e.size >= FileHeader.HEADER_SIZE:
                    e.size -= FileHeader.HEADER_SIZE
                break

    def cleanupContext(self, e:CBFilterCleanupContextEventParams):
        fileContext:FileContext = None

        if e.file_context != 0:
            fileContext = getContext(e.file_context)

            self.AddToLog(f"cleanupContext {fileContext.getFileName()}")

            fileContext.close()

            freeContext(e.file_context)

    def error(self, e:CBFilterErrorEventParams):
        self.AddToLog(f"{e.description} {e.error_code}")

def banner():
    print("CBFS Filter isolatedencryption Demo Copyright (c) 2017-2024, Callback Technologies, Inc.\n\n")

def usage():
    print("Usage: isolatedencryption [-<switch 1> ... -<switch N>] [<path to isolate>] [<whitelist processes, comma separated>]\n")
    print("<Switches>")
    print("  -drv <path to cbfilter.cab> - Install the driver from the specified CAB file")
    print("  -- Stop switches scanning\n")

def is_drive_letter(path_str):
    if not path_str:
        return False
    c = path_str[0]
    if c.isalpha() and len(path_str) == 2 and path_str[1] == ':':
        return True
    else:
        return False

def convert_relative_path_to_absolute(path_str):
    res = None
    if path_str and path_str.strip():
        res = path_str
        # Linux-specific case of using a home directory
        if path_str == "~" or path_str.startswith("~/"):
            home_dir = os.path.expanduser("~")
            if path_str == "~":
                return home_dir
            else:
                return os.path.join(home_dir, path_str[1:])
        elif not is_drive_letter(path_str):
            try:
                res = os.path.abspath(path_str)
                if res.startswith("\\\\") and not os.path.exists(res):
                    print(f"The network folder '{res}' does not exist.")
                elif not os.path.exists(res):
                    print(f"The path '{res}' does not exist.")
            except Exception as e:
                print(f"Error while converting to absolute path: {e}")
                res = None
    return res

def check_driver(write_info=True):
    try:
        filter = CBFilter()
        module_status = filter.get_driver_status(guid)
        module_version = filter.get_driver_version(guid)
        version_high = (module_version >> 32) & 0xFFFFFFFF
        version_low = module_version & 0xFFFFFFFF
        if module_status != 0:
            driver_info = "Driver version: {}.{}.{}.{}".format(
                version_high >> 16, version_high & 0xFFFF, version_low >> 16, version_low & 0xFFFF)
        else:
            driver_info = "Driver: not installed"
        if write_info:
            print(driver_info)
        return module_status != 0
    except Exception as ex:
        print("Error in check_driver:", ex)
        return False

def install(file_name):
    if not os.path.exists(file_name):
        return
    try:
        print("Installing the driver from", file_name)
        filter = CBFilter()
        reboot = filter.install(file_name, guid, "", ALTITUDE_FAKE_VALUE_FOR_DEBUG, 0)
        check_driver()
        if reboot:
            print("Please, reboot the system for the changes to take effect")
        else:
            print("Driver installed successfully")
    except CBFSFilterError as e:
        if e.code == ERROR_PRIVILEGE_NOT_HELD or e.code == ERROR_ACCESS_DENIED:
            print("Installation requires administrator rights. Run the app as administrator")
        else:
            print(e.message)

def uninstall(file_name):
    if not os.path.exists(file_name):
        return
    try:
        print("Uninstalling the driver from " + file_name)
        # Assuming mWatcher is an instance variable
        filter = CBFilter()
        reboot = filter.uninstall(file_name, guid, "", 0)
        check_driver()
        if reboot:
            print("Please, reboot the system for the changes to take effect")
        else:
            print("Driver uninstalled successfully")
    except CBFSFilterError as e:
        if e.code() == ERROR_PRIVILEGE_NOT_HELD or e.code() == ERROR_ACCESS_DENIED:
            print("Uninstallation requires administrator rights. Run the app as administrator")
        else:
            print(e.message)

def main(args:list[str]):
    pathToIsolate:str = None
    whitelistProcs:str = None
    logging = False
    stopOpt = False

    banner()
    if (len(args) < 2):
        usage()
        return

    argi = 0
    while argi < len(args):
        arg = args[argi]
        argLen = len(arg)
        if argLen > 0:
            if arg[0] == '-' and not stopOpt:
                if arg.lower() == "-drv":
                    arg = convert_relative_path_to_absolute(args[argi + 1])
                    if arg is None:
                        print("Error: Invalid Driver Path.")
                        return
                    argi += 1
                    if argi < len(args):
                        install(arg)
                elif arg.lower() == "-log":
                    logging = True
                elif arg == "--":
                    stopOpt = True
                else:
                    print("Error: Invalid option " + arg)
            else:
                if pathToIsolate is None:
                    pathToIsolate = convert_relative_path_to_absolute(arg)
                    if pathToIsolate is None:
                        print("Error: Invalid Path To Monitor.")
                        return
                    if not os.path.exists(pathToIsolate):
                        print(
                            "ERROR: the specified path '" + pathToIsolate + "' does not point to an existing "
                                                                            "directory")
                        return
                elif whitelistProcs is None:
                    whitelistProcs = arg
                    break # no more parameters expected
        argi += 1

    if not check_driver():
        return

    if pathToIsolate is None or whitelistProcs is None:
        return

    print("Press any key to start isolating, then press any key to stop and quit.")
    sys.stdin.read(1)

    try:
        instance = isolatedencryption()
        instance.start(pathToIsolate=pathToIsolate, whitelistProcs=whitelistProcs, logging=logging)

        print("Press any key to stop isolating and quit.")
        sys.stdin.read(1)

        instance.stop()
    except Exception as ex:
        print("Error: " + repr(ex))

if __name__ == "__main__":
    main(sys.argv[1:])

