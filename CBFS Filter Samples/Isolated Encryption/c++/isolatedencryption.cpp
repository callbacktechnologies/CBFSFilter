/*
 * CBFS Filter 2024 C++ Edition - Sample Project
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
 */

#define WIN32_LEAN_AND_MEAN        // Exclude rarely-used stuff from Windows headers

#define _CRT_SECURE_NO_DEPRECATE
#define _CRT_NON_CONFORMING_SWPRINTFS

#include <stdlib.h>
#include <stdio.h>
#include <conio.h>
#include <windows.h>
#include <winternl.h>
#include <assert.h>
#include <tchar.h>
#include <iostream>
#include <Shlwapi.h>
#include <filesystem>
#include <string>
#include <vector>

#define ASSERT assert

#ifdef _UNICODE
#include "../../include/unicode/cbfsfilter.h"
#else
#include "../../include/cbfsfilter.h"
#endif

#ifdef UNIX
#include <unistd.h>
#endif

using namespace std;

#ifdef UNICODE
#define sout wcout
#define scin wcin
typedef std::wstring cbt_string;
#else
#define sout cout
#define scin cin
typedef std::string cbt_string;
#define _T(q) q
#endif

DWORD BYTES_PER_SECTOR = 0x200;

class FileHeader;
class FileContext;

SYSTEM_INFO gSystemInfo;
INT g_DEFAULT_CLUSTER_SIZE = 4096; // this value is specific to the system configuration and must be obtained on initialization

#define ROUND_TO_CLUSTER(Size)  (((ULONG_PTR)(Size) + g_DEFAULT_CLUSTER_SIZE - 1) & ~(g_DEFAULT_CLUSTER_SIZE - 1))

class CBFSFilter : public CBFilter
{
public:
    CBFSFilter() : CBFilter()
    {
    }
    virtual INT FireBeforeCreateFile(CBFilterBeforeCreateFileEventParams* e) override;
    virtual INT FireBeforeOpenFile(CBFilterBeforeOpenFileEventParams* e) override;
    virtual INT FireBeforeQueryFileInfo(CBFilterBeforeQueryFileInfoEventParams *e) override;
    virtual INT FireBeforeSetFileInfo(CBFilterBeforeSetFileInfoEventParams *e) override;
    virtual INT FireBeforeGetFileSecurity(CBFilterBeforeGetFileSecurityEventParams *e) override;
    virtual INT FireBeforeSetFileSecurity(CBFilterBeforeSetFileSecurityEventParams *e) override;
    virtual INT FireBeforeReadFile(CBFilterBeforeReadFileEventParams* e) override;
    virtual INT FireBeforeWriteFile(CBFilterBeforeWriteFileEventParams* e) override;
    virtual INT FireBeforeRenameOrMoveFile(CBFilterBeforeRenameOrMoveFileEventParams *e) override;
    virtual INT FireAfterEnumerateDirectory(CBFilterAfterEnumerateDirectoryEventParams* e) override;
    virtual INT FireCleanupContext(CBFilterCleanupContextEventParams *e) override;
    virtual int FireFilterStop(CBFilterFilterStopEventParams* e) override;

protected:
    INT OnCreateOrOpenFile(LPCTSTR fileName, INT desiredAccess, INT shareMode, INT disposition, INT options, INT attributes, LPCTSTR procName, FileContext** fileContext);
};


const TCHAR g_ProductId[] = TEXT("{713CC6CE-B3E2-4fd9-838D-E28F558F6866}");
const CHAR g_Password[] = "isolatedencryptionDemo";

CBFSFilter g_CBFSFlt;
std::vector<cbt_string> g_WhitelistProcs;

const TCHAR ALTITUDE_FAKE_VALUE_FOR_DEBUG[] = TEXT("149995.24");

BOOL g_LogginOn = FALSE;

void AddToLog(LPCTSTR Value);
void SetFilter(LPCTSTR PathToMonitor);
void DeleteFilter(void);


/****************************************************************************************************
*
* support routines
*
*****************************************************************************************************/

void AddToLog(LPCTSTR Value)
{
    if (!g_LogginOn) return;

    _tprintf(_T("%s\n"), Value);
}


/****************************************************************************************************
*
* FileHeader class
*
*****************************************************************************************************/

class FileHeader
{
public:
    static const DWORD HEADER_SIZE = 512;
    static const CHAR HeaderTag[];
    static const CHAR AuthTag = 'x';
public:
    FileHeader(LPCSTR masterKey)
    {
        mMasterXoxKey = 0;
        while (*masterKey != 0)
        {
            mMasterXoxKey += *masterKey;

            ++masterKey;
        }

        mSessionKey = (CHAR)GetTickCount64();

        ZeroMemory(mHeaderData, sizeof(mHeaderData));
    }
    ~FileHeader()
    {
    }

public:
    BOOL GetPresent();

public:
    INT ParseHeader(HANDLE fileHandle);
    INT SetupHeader();
    INT WriteHeader(HANDLE fileHandle);
    VOID EncryptBuffer(PVOID buffer, INT bytesToWrite, INT bufferLength);
    VOID DecryptBuffer(PVOID buffer, INT bytesRead, INT bufferLength);

private:
    CHAR mMasterXoxKey;
    CHAR mSessionKey;
    CHAR mHeaderData[HEADER_SIZE];
    BOOL mPresent = FALSE;
};

const CHAR FileHeader::HeaderTag[] = "@@@!!!cbfilter isolatedencryption!!!@@@";

BOOL FileHeader::GetPresent()
{
    return mPresent;
}

INT FileHeader::ParseHeader(HANDLE fileHandle)
{
    LONG64 position = 0;
    INT    bytesToRead = HEADER_SIZE;
    DWORD  bytesRead = 0;

    OVERLAPPED overlapped = {0};

    overlapped.hEvent     = NULL;
    overlapped.Offset     = (int)position;
    overlapped.OffsetHigh = (int)(position >> 32);

    if (ReadFile(fileHandle, mHeaderData, bytesToRead, &bytesRead, &overlapped) == false)
    {
        return GetLastError();
    }

    for (INT i = 0; i < (sizeof(HeaderTag) - 1); ++i)
    {
        // It is not an encrypted file.
        if (HeaderTag[i] != mHeaderData[i])
            return NO_ERROR;
    }

    if ((mHeaderData[sizeof(HeaderTag) - 1] ^ mMasterXoxKey) != AuthTag)
    {
        return ERROR_INVALID_PASSWORD;
    }

    mSessionKey = (mHeaderData[sizeof(HeaderTag) - 1 + 1] ^ mMasterXoxKey);

    mPresent = TRUE;

    return NO_ERROR;
}

INT FileHeader::SetupHeader()
{
    ZeroMemory(mHeaderData, sizeof(mHeaderData));

    memcpy(mHeaderData, HeaderTag, sizeof(HeaderTag) - 1);

    mHeaderData[sizeof(HeaderTag) - 1] = AuthTag ^ mMasterXoxKey;
    mHeaderData[sizeof(HeaderTag) - 1 + 1] = mSessionKey ^ mMasterXoxKey;

    mPresent = FALSE;

    return NO_ERROR;
}

INT FileHeader::WriteHeader(HANDLE fileHandle)
{
    LONG64 position = 0;
    INT bytesToWrite = HEADER_SIZE;
    DWORD bytesWritten = 0;
    OVERLAPPED overlapped = { 0 };

    overlapped.hEvent     = NULL;
    overlapped.Offset     = (int)position;
    overlapped.OffsetHigh = (int)(position >> 32);

    if (WriteFile(fileHandle, mHeaderData, bytesToWrite, &bytesWritten, &overlapped) == false)
    {
        return GetLastError();
    }

    mPresent = TRUE;

    return NO_ERROR;
}

VOID FileHeader::EncryptBuffer(PVOID buffer, INT bytesToWrite, INT bufferLength)
{
    ASSERT(bufferLength % 512 == 0);

    for (INT i = 0; i < bytesToWrite; ++i)
    {
        ((LPSTR)buffer)[i] = ((LPCSTR)buffer)[i] ^ mSessionKey;
    }
}

VOID FileHeader::DecryptBuffer(PVOID buffer, INT bytesRead, INT bufferLength)
{
    ASSERT(bufferLength % 512 == 0);

    for (INT i = 0; i < bytesRead; ++i)
    {
        ((LPSTR)buffer)[i] = ((LPCSTR)buffer)[i] ^ mSessionKey;
    }
}

/****************************************************************************************************
*
* EncryptContext class
*
*****************************************************************************************************/

class FileContext {
public:
    static const int MASTER_KEY_SIZE = 16;
public:
    FileContext(CBFSFilter* filter, LPCTSTR fileName, LPCSTR password);
    ~FileContext();

    INT Initialize(INT desiredAccess, INT shareMode, INT creationDisposition, INT flagsAndAttributes);
    VOID Close();
public:
    LPCTSTR GetFileName();
    BOOL GetHeaderPresent();
    LONG64 GetActualFileSize();
    LONG64 GetCalcedFileSize();
    INT GetCalcedHeaderSize();
public:
    INT OnNtQueryFileInfo(PVOID buffer, INT length, INT fileInfoClass, PINT bytesWritten);
    INT OnNtSetFileInfo(PVOID buffer, INT length, INT fileInfoClass);
    INT OnNtQuerySecurity(INT securityInfo, PVOID securityDescriptor, INT length, PINT lenNeeded);
    INT OnNtSetSecurity(INT securityInfo, PVOID securityDescriptor);
    INT OnCachedWriteExtendFileSize(LONG64 size);
    INT OnNonCachedRead(PVOID buffer, LONG64 position, INT bytesToRead, PINT bytesRead);
    INT OnNonCachedWrite(PVOID buffer, LONG64 position, INT bytesToWrite, PINT bytesWritten);
    INT OnRenameOrMoveFile(LPCTSTR newFileName, BOOL replaceIfExists);
public:
    INT EncryptFile();
private:
    CBFSFilter* mFilter;
    FileHeader* mFileHeader;
    LPCTSTR mFileName;
    HANDLE mFileHandle = INVALID_HANDLE_VALUE;
    LONG64 mFileSize = 0;
};

/****************************************************************************************************
*
* EncryptContext class implementation
*
*****************************************************************************************************/

FileContext::FileContext(CBFSFilter* filter, LPCTSTR fileName, LPCSTR password)
    : mFilter(filter)
{
    mFileName = _tcsdup(fileName);

    std::string masterKeyData = password;
    masterKeyData += "abcdefghijklmnopqrst";

    masterKeyData.resize(MASTER_KEY_SIZE);

    mFileHeader = new FileHeader(masterKeyData.c_str());
}

FileContext::~FileContext()
{
    Close();

    free(mFileHeader);
    free((void*)mFileName);
}

INT FileContext::Initialize(INT desiredAccess, INT shareMode, INT creationDisposition, INT flagsAndAttributes)
{
    INT errorCode = NO_ERROR;
    HANDLE auxiHandle =INVALID_HANDLE_VALUE;
    HANDLE operHandle;

    // Use original input parameters to perform share access check.
    LONG64 handleVaule = mFilter->CreateFileDirect(mFileName, FALSE,
        desiredAccess, shareMode, creationDisposition, flagsAndAttributes,
        FALSE, FALSE);

    mFileHandle = (HANDLE)handleVaule;

    if (mFileHandle == INVALID_HANDLE_VALUE)
    {
        errorCode = mFilter->GetLastErrorCode();
        goto end;
    }

    operHandle = mFileHandle;

    if ((desiredAccess & FILE_READ_ATTRIBUTES) == 0)
    {
        LONG64 auxiHandleValue = mFilter->CreateFileDirect(mFileName, FALSE,
            GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE |
            cbfConstants::CBFILTER_IGNORE_SHARE_ACCESS_CHECK
            ,
            OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS,
            FALSE, FALSE);

        auxiHandle = (HANDLE)auxiHandleValue;

        if (auxiHandle == INVALID_HANDLE_VALUE)
        {
            errorCode = mFilter->GetLastErrorCode();
            goto end;
        }

        operHandle = auxiHandle;
    }

    BY_HANDLE_FILE_INFORMATION fileInfo;
    if (!GetFileInformationByHandle(operHandle, &fileInfo))
    {
        errorCode = GetLastError();
        goto end;
    }

    if ((fileInfo.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) == FILE_ATTRIBUTE_DIRECTORY)
    {
        errorCode = ERROR_NOT_SUPPORTED;
        goto end;
    }

    mFileSize = (((LONG64)fileInfo.nFileSizeHigh) << 32) | ((LONG64)fileInfo.nFileSizeLow);

    if (mFileSize >= FileHeader::HEADER_SIZE)
    {
        if (auxiHandle == INVALID_HANDLE_VALUE && (desiredAccess & FILE_READ_DATA) == 0)
        {
            LONG64 auxiHandleValue = mFilter->CreateFileDirect(mFileName, FALSE,
                GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE |
                cbfConstants::CBFILTER_IGNORE_SHARE_ACCESS_CHECK
                ,
                OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS,
                FALSE, FALSE);

            auxiHandle = (HANDLE)auxiHandleValue;

            if (auxiHandle == INVALID_HANDLE_VALUE)
            {
                errorCode = mFilter->GetLastErrorCode();
                goto end;
            }

            operHandle = auxiHandle;
        }

        errorCode = mFileHeader->ParseHeader(operHandle);
        if (errorCode != NO_ERROR)
            goto end;
    }

    if (GetHeaderPresent() == FALSE)
    {
        errorCode = mFileHeader->SetupHeader();
        if (errorCode != NO_ERROR)
            goto end;
    }

end:

    if (auxiHandle != INVALID_HANDLE_VALUE)
        CloseHandle(auxiHandle);

    if (errorCode != NO_ERROR)
        Close();

    return errorCode;
}

VOID FileContext::Close()
{
    if (mFileHandle != INVALID_HANDLE_VALUE)
    {
        CloseHandle(mFileHandle);
        mFileHandle = INVALID_HANDLE_VALUE;
    }
}

LPCTSTR FileContext::GetFileName()
{
    return mFileName;
}

BOOL FileContext::GetHeaderPresent()
{
    return mFileHeader->GetPresent();
}

LONG64 FileContext::GetActualFileSize()
{
    return mFileSize;
}

LONG64 FileContext::GetCalcedFileSize()
{
    return GetHeaderPresent() ? GetActualFileSize() - FileHeader::HEADER_SIZE : GetActualFileSize();
}

INT FileContext::GetCalcedHeaderSize()
{
    return GetHeaderPresent() ? FileHeader::HEADER_SIZE : 0;
}

#pragma comment(lib, "ntdll")

#define STATUS_BUFFER_OVERFLOW           ((NTSTATUS)0x80000005L)
//#define STATUS_INVALID_PARAMETER         ((NTSTATUS)0xC000000DL)

typedef struct _FILE_BASIC_INFORMATION {
    LARGE_INTEGER CreationTime;
    LARGE_INTEGER LastAccessTime;
    LARGE_INTEGER LastWriteTime;
    LARGE_INTEGER ChangeTime;
    ULONG         FileAttributes;
} FILE_BASIC_INFORMATION, *PFILE_BASIC_INFORMATION;

typedef struct _FILE_STANDARD_INFORMATION {
    LARGE_INTEGER AllocationSize;
    LARGE_INTEGER EndOfFile;
    ULONG         NumberOfLinks;
    BOOLEAN       DeletePending;
    BOOLEAN       Directory;
} FILE_STANDARD_INFORMATION, *PFILE_STANDARD_INFORMATION;

typedef struct _FILE_ALLOCATION_INFORMATION {
    LARGE_INTEGER AllocationSize;
} FILE_ALLOCATION_INFORMATION, *PFILE_ALLOCATION_INFORMATION;

typedef struct _FILE_END_OF_FILE_INFORMATION {
    LARGE_INTEGER EndOfFile;
} FILE_END_OF_FILE_INFORMATION, *PFILE_END_OF_FILE_INFORMATION;

typedef struct _FILE_RENAME_INFORMATION {
    BOOLEAN ReplaceIfExists;
    HANDLE RootDirectory;
    ULONG FileNameLength;
    WCHAR FileName[1];
} FILE_RENAME_INFORMATION, *PFILE_RENAME_INFORMATION;

EXTERN_C NTSTATUS NTSYSCALLAPI NtQueryInformationFile(HANDLE FileHandle, PIO_STATUS_BLOCK IoStatusBlock, PVOID FileInformation, ULONG Length, FILE_INFORMATION_CLASS FileInformationClass);
EXTERN_C NTSTATUS NTSYSCALLAPI NtSetInformationFile(HANDLE FileHandle, PIO_STATUS_BLOCK IoStatusBlock, PVOID FileInformation, ULONG Length, FILE_INFORMATION_CLASS FileInformationClass);
EXTERN_C NTSTATUS NTSYSCALLAPI NtQuerySecurityObject(HANDLE Handle, SECURITY_INFORMATION SecurityInformation, PSECURITY_DESCRIPTOR SecurityDescriptor, ULONG Length, PULONG LengthNeeded);
EXTERN_C NTSTATUS NTSYSCALLAPI NtSetSecurityObject(HANDLE Handle, SECURITY_INFORMATION SecurityInformation, PSECURITY_DESCRIPTOR SecurityDescriptor);

INT FileContext::OnNtQueryFileInfo(PVOID buffer, INT length, INT fileInfoClass, PINT bytesWritten)
{
    IO_STATUS_BLOCK ioStatus;

    auto status = NtQueryInformationFile(mFileHandle, &ioStatus, buffer, length, (FILE_INFORMATION_CLASS)fileInfoClass);

    *bytesWritten = (INT)ioStatus.Information;

    if (status == 0 || status == STATUS_BUFFER_OVERFLOW)
    {
        if (fileInfoClass == 18/*FileAllInformation*/)
        {
            /*
             typedef struct _FILE_ALL_INFORMATION {
                FILE_BASIC_INFORMATION BasicInformation;
                FILE_STANDARD_INFORMATION StandardInformation;
                ...
             } FILE_ALL_INFORMATION, *PFILE_ALL_INFORMATION;
             */
            if ((int)ioStatus.Information >= sizeof(FILE_BASIC_INFORMATION) + sizeof(FILE_STANDARD_INFORMATION))
            {
                auto standInfo = (PFILE_STANDARD_INFORMATION)((PUCHAR)buffer + sizeof(FILE_BASIC_INFORMATION));

                standInfo->EndOfFile.QuadPart = GetCalcedFileSize();
                standInfo->AllocationSize.QuadPart = ROUND_TO_CLUSTER(standInfo->EndOfFile.QuadPart);
            }
        }
        else
        if (fileInfoClass == 5/*FileStandardInformation*/)
        {
            auto standInfo = (PFILE_STANDARD_INFORMATION)(buffer);

            standInfo->EndOfFile.QuadPart = GetCalcedFileSize();
            standInfo->AllocationSize.QuadPart = ROUND_TO_CLUSTER(standInfo->EndOfFile.QuadPart);
        }
    }

    return status;
}

INT FileContext::OnNtSetFileInfo(PVOID buffer, INT length, INT fileInfoClass)
{
    LONG64 newActualSize = -1;

    if (fileInfoClass == 19/*FileAllocationInformation*/)
    {
        if (length >= sizeof(FILE_ALLOCATION_INFORMATION))
        {
            auto allocInfo = (PFILE_ALLOCATION_INFORMATION)buffer;
            if (allocInfo->AllocationSize.QuadPart < GetCalcedFileSize())
            {
                allocInfo->AllocationSize.QuadPart += GetCalcedHeaderSize();
                newActualSize = allocInfo->AllocationSize.QuadPart;

                ASSERT(sizeof(FILE_ALLOCATION_INFORMATION) == sizeof(FILE_END_OF_FILE_INFORMATION));

                //
                // We directly replace it with FileEndOfFileInformation.
                //

                fileInfoClass = 20/*FileEndOfFileInformation*/;
            }
            else
            {
                allocInfo->AllocationSize.QuadPart += GetCalcedHeaderSize();
                allocInfo->AllocationSize.QuadPart = ROUND_TO_CLUSTER(allocInfo->AllocationSize.QuadPart);
            }
        }
        else
        {
            return STATUS_INVALID_PARAMETER;
        }
    }
    else
    if (fileInfoClass == 20/*FileEndOfFileInformation*/)
    {
        if (length >= sizeof(FILE_END_OF_FILE_INFORMATION))
        {
            auto eofInfo = (PFILE_END_OF_FILE_INFORMATION)buffer;

            eofInfo->EndOfFile.QuadPart += GetCalcedHeaderSize();

            newActualSize = eofInfo->EndOfFile.QuadPart;
        }
        else
        {
            return STATUS_INVALID_PARAMETER;
        }
    }

    IO_STATUS_BLOCK ioStatus;

    auto status = NtSetInformationFile(mFileHandle, &ioStatus, buffer, length, (FILE_INFORMATION_CLASS)fileInfoClass);
    if (status == 0 && newActualSize != -1)
    {
        mFileSize = newActualSize;
    }

    return status;
}

INT FileContext::OnNtQuerySecurity(INT securityInfo, PVOID securityDescriptor, INT length, PINT lenNeeded)
{
    auto status = NtQuerySecurityObject(mFileHandle, securityInfo, securityDescriptor, length, (PULONG)lenNeeded);

    return status;
}

INT FileContext::OnNtSetSecurity(INT securityInfo, PVOID securityDescriptor)
{
    auto status = NtSetSecurityObject(mFileHandle, securityInfo, securityDescriptor);

    return status;
}

INT FileContext::OnCachedWriteExtendFileSize(LONG64 size)
{
    FILE_END_OF_FILE_INFO fileInfo = { 0 };

    auto newActualSize = size + GetCalcedHeaderSize();

    fileInfo.EndOfFile.QuadPart = newActualSize;

    if (SetFileInformationByHandle(mFileHandle, FileEndOfFileInfo, &fileInfo, sizeof(FILE_END_OF_FILE_INFO)) == FALSE)
    {
        return GetLastError();
    }

    mFileSize = newActualSize;

    return NO_ERROR;
}

INT FileContext::OnNonCachedRead(PVOID buffer, LONG64 position, INT bytesToRead, PINT bytesRead)
{
    INT bufferLength = bytesToRead;

    ASSERT(bufferLength % 512 == 0);

    *bytesRead = 0;

    if (position > GetCalcedFileSize())
        return ERROR_HANDLE_EOF;

    if (position + bytesToRead >= GetCalcedFileSize())
    {
        bytesToRead = (INT)(GetCalcedFileSize() - position);
    }

    OVERLAPPED overlapped = { 0 };

    position += GetCalcedHeaderSize();

    overlapped.hEvent     = NULL;
    overlapped.Offset     = (INT)position;
    overlapped.OffsetHigh = (INT)(position >> 32);

    if (ReadFile(mFileHandle, buffer, bytesToRead, (LPDWORD)bytesRead, &overlapped) == FALSE)
    {
        return GetLastError();
    }

    if (GetHeaderPresent())
    {
        mFileHeader->DecryptBuffer(buffer, *bytesRead, bufferLength);
    }

    return NO_ERROR;
}

INT FileContext::OnNonCachedWrite(PVOID buffer, LONG64 position, INT bytesToWrite, PINT bytesWritten)
{
    INT bufferLength = bytesToWrite;

    ASSERT(bufferLength % 512 == 0);

    *bytesWritten = 0;

    ASSERT(GetHeaderPresent());

    if (position > GetCalcedFileSize())
        return NO_ERROR;

    if (position + bytesToWrite >= GetCalcedFileSize())
    {
        bytesToWrite = (INT)(GetCalcedFileSize() - position);
    }

    mFileHeader->EncryptBuffer(buffer, bytesToWrite, bufferLength);

    OVERLAPPED overlapped = { 0 };

    position += GetCalcedHeaderSize();

    overlapped.hEvent     = NULL;
    overlapped.Offset     = (INT)position;
    overlapped.OffsetHigh = (INT)(position >> 32);

    if (WriteFile(mFileHandle, buffer, bytesToWrite, (LPDWORD)bytesWritten, &overlapped) == FALSE)
    {
        return GetLastError();
    }

    return NO_ERROR;
}

INT FileContext::OnRenameOrMoveFile(LPCTSTR newFileName, BOOL replaceIfExists)
{
    PFILE_RENAME_INFORMATION renameInfo = NULL;
    INT renameNameInfoLen = (INT)(sizeof(FILE_RENAME_INFORMATION) + _tcslen(newFileName) * sizeof(WCHAR));

    renameInfo = (PFILE_RENAME_INFORMATION)malloc(renameNameInfoLen);

    ZeroMemory(renameInfo, renameNameInfoLen);

    renameInfo->ReplaceIfExists = replaceIfExists;
    _tcscpy(renameInfo->FileName, newFileName);
    renameInfo->FileNameLength = (INT)(_tcslen(newFileName) * sizeof(WCHAR));

    //
    // When renaming file with direct handle, we need to use SetFileInformationDirect method.
    //

    auto result = mFilter->SetFileInformationDirect((LONG64)mFileHandle, 10/*FileRenameInformation*/, (LPCSTR)renameInfo, renameNameInfoLen);

    free(renameInfo);

    if (result != NO_ERROR)
        return result;

    free((LPVOID)mFileName);
    mFileName = _tcsdup(newFileName);

    return 0;
}

INT FileContext::EncryptFile()
{
    ASSERT(GetHeaderPresent() == FALSE);

    auto dataSize = mFileSize;

    FILE_END_OF_FILE_INFO fileInfo = { 0 };

    fileInfo.EndOfFile.QuadPart = dataSize + FileHeader::HEADER_SIZE;
    if (SetFileInformationByHandle(mFileHandle, FileEndOfFileInfo, &fileInfo, sizeof(fileInfo)) == FALSE)
    {
        return GetLastError();
    }

    CHAR buffer[FileHeader::HEADER_SIZE];
    auto bufferLength = FileHeader::HEADER_SIZE;

    auto position = dataSize / bufferLength * bufferLength;
    auto validDataLen = dataSize % bufferLength;

    OVERLAPPED overlapped = { 0 };

    while (true)
    {
        if (validDataLen != 0)
        {
            DWORD bytesRead;
            DWORD bytesWritten;

            overlapped.hEvent     = NULL;
            overlapped.Offset     = (INT)position;
            overlapped.OffsetHigh = (INT)(position >> 32);

            if (ReadFile(mFileHandle, buffer, (INT)validDataLen, &bytesRead, &overlapped) == FALSE)
            {
                return GetLastError();
            }

            mFileHeader->EncryptBuffer(buffer, (INT)validDataLen, bufferLength);

            auto writePos = position + FileHeader::HEADER_SIZE;

            overlapped.hEvent     = NULL;
            overlapped.Offset     = (INT)writePos;
            overlapped.OffsetHigh = (INT)(writePos >> 32);

            if (WriteFile(mFileHandle, buffer, (INT)validDataLen, &bytesWritten, &overlapped) == FALSE)
            {
                return GetLastError();
            }
        }

        position    -= FileHeader::HEADER_SIZE;
        validDataLen = FileHeader::HEADER_SIZE;

        if (position < 0)
            break;
    }

    auto result = mFileHeader->WriteHeader(mFileHandle);

    if (result == NO_ERROR)
        mFileSize += FileHeader::HEADER_SIZE;

    return result;
}


/*****************************************************************************************************/

void Banner(void)
{
    printf("CBFS Filter isolatedencryption Demo Copyright (c) 2017-2024, Callback Technologies, Inc.\n\n");
}

void Usage(void)
{
    printf("Usage: isolatedencryption [-<switch 1> ... -<switch N>] [<path to isolate>] [<whitelist processes, comma separated>]\n\n");
    printf("<Switches>\n");
    printf("  -drv <path to cbfilter.cab> - Install the driver from the specified CAB file\n");
    printf("  -log - Enable logging\n");
    printf("  -- Stop switches scanning\n\n");
}

int CheckResult(int result)
{
    if (result != 0)
    {
        fprintf(stderr, "Error %d (%s)\n", g_CBFSFlt.GetLastErrorCode(), g_CBFSFlt.GetLastError());
    }
    return result;
}

void CheckDriver()
{
    int state, number;

    state = g_CBFSFlt.GetDriverStatus(g_ProductId);
    if (CheckResult(g_CBFSFlt.GetLastErrorCode()))
        exit(0);
    if (state == cbfConstants::MODULE_STATUS_RUNNING)
    {
        int64 version;
        version = g_CBFSFlt.GetDriverVersion(g_ProductId);
        if (CheckResult(g_CBFSFlt.GetLastErrorCode()))
            exit(0);
        printf("The driver is installed, version: %d.%d",
            (int)((version & 0x7FFF000000000000) >> 48),
            (int)((version & 0xFFFF00000000) >> 32));
        number = (int)((version & 0xFFFF0000) >> 16);
        if (number != 0)
            printf(".%d", number);
        number = (int)(version & 0xFFFF);
        if (number != 0)
            printf(".%d", number);
        printf("\n");
    }
    else
    {
        printf("Error: the driver is not installed\n");
        exit(0);
    }
}

int optcmp(LPTSTR arg, LPCTSTR opt)
{
    int i = 0;
    while (1)
    {
        if (arg[i] >= 'A' && arg[i] <= 'Z')
            arg[i] = arg[i] - 'A' + 'a';
        if (arg[i] != opt[i])
            return 0;
        if (arg[i] == 0)
            return 1;
        i++;
    }
}

bool IsDriveLetter(const cbt_string& path) {
    if (path.empty())
        return false;

    wchar_t c = path[0];
    if (((c >= L'A' && c <= L'Z') || (c >= L'a' && c <= L'z')) && path.size() == 2 && path[1] == L':')
        return true;
    else
        return false;
}

bool IsAbsolutePath(const cbt_string& path) {
    if (path.empty()) {
        return false;
    }

#ifdef _WIN32
    // On Windows, check if the path starts with a drive letter followed by a colon and a separator
    if ((path.size() >= 3) && iswalpha(path[0]) && (path[1] == L':') && (path[2] == L'\\' || path[2] == L'/')) {
        return true;
    }

    // Check for UNC paths (e.g., \\server\share)
    if (path.size() >= 2 && (path[0] == L'\\') && (path[1] == L'\\')) {
        return true;
    }
#else
    // On Linux and Unix, check if the path starts with a '/'
    if (path[0] == _T('/')) {
        return true;
    }
#endif

    return false;
}

cbt_string ConvertRelativePathToAbsolute(const cbt_string& path, bool acceptDriveLetter = false) {
    cbt_string res;

    if (!path.empty()) {
        res = path;

#ifndef _WIN32
        // Linux/Unix-specific case of using a home directory
        if (path == "~" || path.find("~/") == 0) {
            const char* homeDir = getenv("HOME");

            if (path == "~") {
                return homeDir ? homeDir : "";
            }
            else {
                return homeDir ? cbt_string(homeDir) + path.substr(1) : "";
            }
        }
#endif
        if (!IsAbsolutePath(path)) {
#ifdef _WIN32
            if (IsDriveLetter(path)) {
                if (!acceptDriveLetter) {
                    sout << L"The path '" << path << L"' cannot be equal to the drive letter" << std::endl;
                    return _T("");
                }
                return res;
            }
            wchar_t currentDir[_MAX_PATH];
            const char pathSeparator = '\\';
            if (_wgetcwd(currentDir, _MAX_PATH) == nullptr) {
                sout << "Error getting current directory." << std::endl;
                return _T("");
            }
#else
            char currentDir[PATH_MAX];
            const char pathSeparator = '/';
            if (getcwd(currentDir, sizeof(currentDir)) == nullptr) {
                sout << "Error getting current directory." << std::endl;
                return return _T("");
            }
#endif
            cbt_string currentDirStr(currentDir);

            // Ensure that the current directory has a trailing backslash
            if (currentDirStr.back() != pathSeparator) {
                currentDirStr += pathSeparator;
            }

            return currentDirStr + path;
        }
    }
    else {
        sout << L"Error: The input path is empty." << std::endl;
        return _T("");
    }
    return path;
}

int wmain(int ArgC, wchar_t* ArgV[])
{
    LPTSTR PathToIsolate = NULL;
    LPTSTR WhitelistProcs = NULL;
    int ArgI = 0, ArgLen = 0, StopOpt = 0;

    Banner();

    // if no parameters specified, show usage and quit
    if (ArgC < 2)
    {
        Usage();
        CheckDriver();
        return 0;
    }

    for (ArgI = 1; ArgI < ArgC; ArgI++)
    {
        LPTSTR arg = ArgV[ArgI];
        ArgLen = (int)_tcslen(arg);
        if (ArgLen > 0)
        {
            if ((arg[0] == '-') && !StopOpt)
            {
                if (optcmp(arg, _T("-drv")))
                {
                    cbt_string arg_wstr = ConvertRelativePathToAbsolute(ArgV[++ArgI]);
                    if (arg_wstr.empty()) {
                        printf("Error: Invalid Driver Path\n");
                        exit(1);
                    }
                    arg = _wcsdup(arg_wstr.c_str());
                    if (ArgI < ArgC)
                    {
                        INT DrvReboot = 0;
                        _tprintf(_T("Installing the driver from '%s'\n"), arg);
                        DrvReboot = g_CBFSFlt.Install(arg, g_ProductId, NULL,
                            ALTITUDE_FAKE_VALUE_FOR_DEBUG, 0, NULL);// cbfConstants::INSTALL_REMOVE_OLD_VERSIONS);

                        int errcode = g_CBFSFlt.GetLastErrorCode();
                        if (errcode != 0)
                        {
                            if (errcode == ERROR_PRIVILEGE_NOT_HELD)
                                printf("The driver could not be installed, please try to install with administrator rights\n");
                            else
                                printf("The driver could not be installed, the error is %d (%s)\n", errcode, g_CBFSFlt.GetLastError());

                            exit(errcode);
                        }
                        printf("Drivers installed successfully\n");
                        if (DrvReboot != 0)
                        {
                            printf(", reboot is required\n");
                            exit(1);
                        }
                    }
                }
                else if (optcmp(arg, _T("-log")))
                {
                    g_LogginOn = TRUE;
                }
                else if (optcmp(arg, _T("--")))
                {
                    StopOpt = 1;
                }
                else
                {
                    fwprintf(stderr, _T("Invalid option \"%s\"\n"), arg);
                    exit(ERROR_INVALID_PARAMETER);
                }
            }
            else
            {
                // if we have not set the path yet, do this now. Otherwise, set the mask
                if (PathToIsolate == NULL)
                {
                    cbt_string path_wstr = ConvertRelativePathToAbsolute(_tcsdup(arg));
                    if (path_wstr.empty()) {
                        printf("Error: Invalid Path To Monitor\n");
                        exit(1);
                    }
                    PathToIsolate = _tcsdup(path_wstr.c_str());
                    DWORD attr = GetFileAttributes(PathToIsolate);
                    if (attr == INVALID_FILE_ATTRIBUTES || (attr & FILE_ATTRIBUTE_DIRECTORY) == 0)
                    {
                        fwprintf(stderr, L"ERROR: the specified path '%s' does not point to an existing directory\n", PathToIsolate);
                        exit(ERROR_INVALID_PARAMETER);
                    }
                }
                else
                if (WhitelistProcs == NULL)
                {
                    WhitelistProcs = arg;

                    int index = 0, startIndex = 0;
                    for (;;)
                    {
                        if (WhitelistProcs[index] == _T(',') || WhitelistProcs[index] == 0)
                        {
                            if (index - startIndex > 0)
                            {
                                g_WhitelistProcs.push_back(cbt_string(WhitelistProcs + startIndex, index - startIndex));
                            }

                            if (WhitelistProcs[index] == 0)
                                break;

                            startIndex = index + 1;
                        }

                        ++index;
                    }
                }
                else
                {
                    break; // no more parameters expected
                }
            }
        }
    }

    CheckDriver();

    // Probably, we have just installed the driver, so we can quit without monitoring anything
    if (PathToIsolate == NULL || g_WhitelistProcs.size() == 0)
        exit(0);

    _tprintf(_T("Press any key to start monitoring, then press any key to stop and quit.\n"));

    _getch();

    GetSystemInfo(&gSystemInfo); // we interested in system PageSize value
    SetFilter(PathToIsolate);

    _getch();

    DeleteFilter();

    return 0;
}

void SetFilter(LPCTSTR PathToMonitor)
{
    TCHAR text[MAX_PATH];
    TCHAR Disk[4];
    DWORD SectorsPerCluster, BYTES_PER_SECTOR, NumberOfFreeClusters, TotalNumberOfClusters;

    // 
    // parent directory without trailing back slash must be specified
    //

    g_CBFSFlt.AddFilterRuleEx(PathToMonitor,
        NULL,
        cbfConstants::ACCESS_NONE,
        cbfConstants::FS_CE_AFTER_ENUMERATE_DIRECTORY,
        cbfConstants::FS_NE_NONE,
        -1, -1, FILE_ATTRIBUTE_DIRECTORY, 0);

    ZeroMemory(text, sizeof(text));
    _tcscat(text, PathToMonitor);
    _tcscat(text, _T("\\*.*"));

    g_CBFSFlt.AddFilterRuleEx(text,
        NULL,
        cbfConstants::ACCESS_NONE,
        cbfConstants::FS_CE_BEFORE_CREATE |
        cbfConstants::FS_CE_BEFORE_OPEN |
        cbfConstants::FS_CE_BEFORE_QUERY_FILE_INFO |
        cbfConstants::FS_CE_BEFORE_SET_FILE_INFO |
        cbfConstants::FS_CE_BEFORE_GET_SECURITY |
        cbfConstants::FS_CE_BEFORE_SET_SECURITY |
        cbfConstants::FS_CE_BEFORE_READ |
        cbfConstants::FS_CE_BEFORE_WRITE |
        cbfConstants::FS_CE_BEFORE_RENAME |
        cbfConstants::FS_CE_AFTER_ENUMERATE_DIRECTORY,
        cbfConstants::FS_NE_NONE,
        -1, -1, 0, FILE_ATTRIBUTE_DIRECTORY);

    ZeroMemory(Disk, sizeof(Disk));
    _tcsncpy(Disk, PathToMonitor, 3);

    // determine on disk sector size, in order to use 
    // data and buffers properly aligned data size an 
    // buffer addresses in ReadFile/WriteFile request,
    // using the handle, obtained with CBFSFilter.CreateFileDirect()

    if (!GetDiskFreeSpace(Disk, &SectorsPerCluster, &BYTES_PER_SECTOR,
        &NumberOfFreeClusters, &TotalNumberOfClusters))
    {
        fwprintf(stderr, L"Please specify filtered path related to local disk name");
        exit(3);
    }

    g_DEFAULT_CLUSTER_SIZE = BYTES_PER_SECTOR * SectorsPerCluster;

    int retVal = 0;
    if (0 == retVal)
        retVal = g_CBFSFlt.Initialize(g_ProductId);

    g_CBFSFlt.SetProcessCachedIORequests(TRUE);

    g_CBFSFlt.Config(_T("AllowFileAccessInBeforeOpen=false"));
    g_CBFSFlt.Config(_T("ModifiableReadWriteBuffers=true"));

    if (0 == retVal)
        retVal = g_CBFSFlt.StartFilter(0);

    if (0 != retVal)
    {
        fprintf(stderr, "Cannot set filter: '%s'", g_CBFSFlt.GetLastError());
        exit(3);
    }
}

void DeleteFilter()
{
    g_CBFSFlt.DeleteAllFilterRules();
    g_CBFSFlt.StopFilter();
}


/***********************************************************************************************
* CALLBACK EVENT HANDLERS
************************************************************************************************/

INT CBFSFilter::FireBeforeCreateFile(CBFilterBeforeCreateFileEventParams* e)
{
    FileContext* fileContext = NULL;

    LPCTSTR procName = this->GetOriginatorProcessName();

    e->ResultCode = OnCreateOrOpenFile(e->FileName, e->DesiredAccess, e->ShareMode, e->CreateDisposition, e->Options, e->Attributes, procName, &fileContext);

    if (fileContext != NULL)
    {
        ASSERT(e->ResultCode == NO_ERROR);

        e->Isolate = TRUE;
        e->BackendFileName = _T("");
        e->lenBackendFileName = 0;

        e->FileContext = fileContext;

        e->ProcessRequest = TRUE;
    }

    return 0;
}

INT CBFSFilter::FireBeforeOpenFile(CBFilterBeforeOpenFileEventParams* e)
{
    FileContext* fileContext = NULL;

    LPCTSTR procName = this->GetOriginatorProcessName();

    e->ResultCode = OnCreateOrOpenFile(e->FileName, e->DesiredAccess, e->ShareMode, e->CreateDisposition, e->Options, e->Attributes, procName, &fileContext);

    if (fileContext != NULL)
    {
        ASSERT(e->ResultCode == NO_ERROR);

        e->Isolate = TRUE;
        e->BackendFileName = _T("");
        e->lenBackendFileName = 0;

        e->FileContext = fileContext;

        e->ProcessRequest = TRUE;
    }

    return 0;
}

INT CBFSFilter::OnCreateOrOpenFile(LPCTSTR fileName, INT desiredAccess, INT shareMode, INT disposition, INT options, INT attributes, LPCTSTR procName, FileContext** fileContext)
{
    INT result = NO_ERROR;
    BOOL isolated = FALSE;

    TCHAR text[MAX_PATH * 2];

    *fileContext = NULL;

    if ((attributes & FILE_ATTRIBUTE_DIRECTORY) != 0)
        return result;

    INT procNameLen = (INT)_tcslen(procName);
    for each (const cbt_string& whitelistProc in g_WhitelistProcs)
    {
        if (procNameLen < whitelistProc.length() + 1)
            continue;

        if (procName[procNameLen - (whitelistProc.length() + 1)] != _T('\\'))
            continue;

        if (_tcsicmp(procName + (procNameLen - whitelistProc.length()), whitelistProc.c_str()) == 0)
        {
            isolated = TRUE;
            break;
        }
    }

    if (isolated == FALSE)
        return result;

    _stprintf(text, _T("BeforeCreate/OpenFile %s %s"), procName, fileName);
    AddToLog(text);

    auto context = new FileContext(&g_CBFSFlt, fileName, g_Password);

    auto intResult = context->Initialize(desiredAccess, shareMode, disposition, options | attributes);

    if (intResult == ERROR_NOT_SUPPORTED)
        return result;

    result = intResult;

    if (result == NO_ERROR)
        *fileContext = context;
    else
        delete context;

    return result;

}

INT CBFSFilter::FireBeforeQueryFileInfo(CBFilterBeforeQueryFileInfoEventParams *e)
{
    FileContext* fileContext;

    if (e->FileContext == NULL)
        return 0;

    TCHAR text[MAX_PATH * 2];
    _stprintf(text, _T("BeforeQueryFileInfo %s"), e->FileName);
    AddToLog(text);

    fileContext = (FileContext*)e->FileContext;

    int bytesWritten = 0;

    e->Status = fileContext->OnNtQueryFileInfo(e->Buffer, e->BufferLength, e->FileInformationClass, &bytesWritten);

    e->ValidBytes = bytesWritten;
    e->ProcessRequest = FALSE;

    return 0;
}

INT CBFSFilter::FireBeforeSetFileInfo(CBFilterBeforeSetFileInfoEventParams *e)
{
    FileContext* fileContext;

    //
    // Rename operations will be processed in CBFSFltBeforeRenameOrMoveFile
    //

    if (e->FileInformationClass == 10/*FileRenameInformation*/ ||
        e->FileInformationClass == 65/*FileRenameInformationEx*/)
        return 0;

    if (e->FileContext == NULL)
        return 0;

    TCHAR text[MAX_PATH * 2];
    _stprintf(text, _T("BeforeSetFileInfo %s"), e->FileName);
    AddToLog(text);

    fileContext = (FileContext*)e->FileContext;

    e->Status = fileContext->OnNtSetFileInfo(e->Buffer, e->BufferLength, e->FileInformationClass);
    e->ProcessRequest = FALSE;

    return 0;
}

INT CBFSFilter::FireBeforeGetFileSecurity(CBFilterBeforeGetFileSecurityEventParams *e)
{
    FileContext* fileContext;

    if (e->FileContext == NULL)
        return 0;

    TCHAR text[MAX_PATH * 2];
    _stprintf(text, _T("BeforeGetFileSecurity %s"), e->FileName);
    AddToLog(text);

    fileContext = (FileContext*)e->FileContext;

    int lenNeeded = 0;

    e->Status = fileContext->OnNtQuerySecurity(e->SecurityInformation, e->SecurityDescriptor, e->Length, &lenNeeded);

    e->LengthNeeded = lenNeeded;
    e->ProcessRequest = FALSE;

    return 0;
}

INT CBFSFilter::FireBeforeSetFileSecurity(CBFilterBeforeSetFileSecurityEventParams *e)
{
    FileContext* fileContext;

    if (e->FileContext == NULL)
        return 0;

    TCHAR text[MAX_PATH * 2];
    _stprintf(text, _T("BeforeSetFileSecurity %s"), e->FileName);
    AddToLog(text);

    fileContext = (FileContext*)e->FileContext;

    e->Status = fileContext->OnNtSetSecurity(e->SecurityInformation, e->SecurityDescriptor);
    e->ProcessRequest = FALSE;

    return 0;
}

INT CBFSFilter::FireBeforeReadFile(CBFilterBeforeReadFileEventParams* e)
{
    FileContext* fileContext;

    if (e->FileContext == NULL)
        return 0;

    TCHAR text[MAX_PATH * 2];
    _stprintf(text, _T("BeforeReadFile %s %s POS(%I64X) READ(%0X)"), e->FileName, (e->Direction == cbfConstants::FS_REQUEST_DIR_USER_CACHED) || (e->Direction == cbfConstants::FS_REQUEST_DIR_SYSTEM_CACHED) ? _T("Cached") : _T("NonCached"),*e->pPosition, e->BytesToRead);
    AddToLog(text);

    fileContext = (FileContext*)e->FileContext;

    if (e->Direction == cbfConstants::FS_REQUEST_DIR_USER_CACHED ||
        e->Direction == cbfConstants::FS_REQUEST_DIR_SYSTEM_CACHED)
    {

    }
    else
    {
        int bytesRead = 0;

        e->ResultCode = fileContext->OnNonCachedRead(e->Buffer, *(e->pPosition), e->BytesToRead, &bytesRead);

        e->BytesToRead = bytesRead;
        e->ProcessRequest = false;
    }

    return 0;
}

INT CBFSFilter::FireBeforeWriteFile(CBFilterBeforeWriteFileEventParams* e)
{
    FileContext* fileContext;

    if (e->FileContext == NULL)
        return 0;

    TCHAR text[MAX_PATH * 2];
    _stprintf(text, _T("BeforeWriteFile %s %s, OFF(%I64X), Write(%d))"), e->FileName, 
        (e->Direction == cbfConstants::FS_REQUEST_DIR_USER_CACHED) || (e->Direction == cbfConstants::FS_REQUEST_DIR_SYSTEM_CACHED) ? _T("Cached") : _T("NonCached"),*e->pPosition, e->BytesToWrite);
    AddToLog(text);

    fileContext = (FileContext*)e->FileContext;

    if (fileContext->GetHeaderPresent() == false)
    {
        e->ResultCode = fileContext->EncryptFile();
        if (e->ResultCode != NO_ERROR)
            return 0;
    }

    if (e->Direction == cbfConstants::FS_REQUEST_DIR_USER_CACHED ||
        e->Direction == cbfConstants::FS_REQUEST_DIR_SYSTEM_CACHED)
    {
        if (*(e->pPosition) + e->BytesToWrite > fileContext->GetCalcedFileSize())
        {
            e->ResultCode = fileContext->OnCachedWriteExtendFileSize(*(e->pPosition) + e->BytesToWrite);
        }
    }
    else
    {
        int bytesWritten = 0;

        e->ResultCode = fileContext->OnNonCachedWrite(e->Buffer, *(e->pPosition), e->BytesToWrite, &bytesWritten);

        e->BytesToWrite = bytesWritten;
        e->ProcessRequest = false;
    }

    return 0;
}

INT CBFSFilter::FireBeforeRenameOrMoveFile(CBFilterBeforeRenameOrMoveFileEventParams* e)
{
    FileContext* fileContext;

    LPCTSTR fileName = e->FileName;
    LPCTSTR newFileName = e->NewFileName;

    TCHAR text[MAX_PATH * 2];
    _stprintf(text, _T("BeforeRenameOrMoveFile %s => %s"), fileName, newFileName);
    AddToLog(text);

    if (e->FileContext != NULL)
    {
        if (this->IsFileFiltered(newFileName) == FALSE)
        {
            e->ResultCode = ERROR_NOT_SAME_DEVICE;
            return 0;
        }

        fileContext = (FileContext*)e->FileContext;

        e->ResultCode = fileContext->OnRenameOrMoveFile(newFileName, e->ReplaceIfExists);

        e->ProcessRequest = FALSE;
    }
    else
    {
        LPCTSTR procName = this->GetOriginatorProcessName();

        bool isWhiteProc = false;
        INT procNameLen = (INT)_tcslen(procName);
        for each (const cbt_string& whitelistProc in g_WhitelistProcs)
        {
            if (procNameLen < whitelistProc.length() + 1)
                continue;

            if (procName[procNameLen - (whitelistProc.length() + 1)] != _T('\\'))
                continue;

            if (_tcsicmp(procName + (procNameLen - whitelistProc.length()), whitelistProc.c_str()) == 0)
            {
                isWhiteProc = TRUE;
                break;
            }
        }

        if (isWhiteProc == TRUE)
        {
            e->ResultCode = ERROR_NOT_SAME_DEVICE;
            return 0;
        }

        //
        // The event is in filter mode at this point and we let the request go forward.
        //

        e->ProcessRequest = TRUE;
    }

    return 0;
}

INT CBFSFilter::FireAfterEnumerateDirectory(CBFilterAfterEnumerateDirectoryEventParams* e)
{
    ASSERT(e->DirectoryContext == NULL);

    LPCTSTR procName = this->GetHandleCreatorProcessName();

    // we presume that all files in directory are filtered and encrypted,
    // but we provide the real size of decrypted data only to those processes that will receive this decrypted data
    INT procNameLen = (INT)_tcslen(procName);
    for each (const cbt_string& whitelistProc in g_WhitelistProcs)
    {
        if (procNameLen < whitelistProc.length() + 1)
            continue;

        if (procName[procNameLen - (whitelistProc.length() + 1)] != _T('\\'))
            continue;

        if (_tcsicmp(procName + (procNameLen - whitelistProc.length()), whitelistProc.c_str()) == 0)
        {
            if (*(e->pSize) >= FileHeader::HEADER_SIZE)
                *(e->pSize) -= FileHeader::HEADER_SIZE;

            break;
        }
    }

    return 0;
}

INT CBFSFilter::FireCleanupContext(CBFilterCleanupContextEventParams *e)
{
    FileContext* fileContext;

    if (e->FileContext != NULL)
    {
        fileContext = (FileContext*)e->FileContext;

        TCHAR text[MAX_PATH * 2];
        _stprintf(text, _T("CleanupContext %s"), fileContext->GetFileName());
        AddToLog(text);

        fileContext->Close();

        delete fileContext;
    }

    return 0;
}

INT CBFSFilter::FireFilterStop(CBFilterFilterStopEventParams* e)
{
    return 0;
}




