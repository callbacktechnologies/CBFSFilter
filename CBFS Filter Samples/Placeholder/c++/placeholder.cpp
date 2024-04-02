/*
 * CBFS Filter 2022 C++ Edition - Sample Project
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

﻿/*
 * CBFS Filter 2022 C++ Edition - Demo Application
 *
 * Copyright (c) 2023 Callback Technologies, Inc. - All rights reserved. - www.callback.com
 *
 */
#define WIN32_LEAN_AND_MEAN		// Exclude rarely-used stuff from Windows headers

#define _CRT_SECURE_NO_DEPRECATE
#define _CRT_NON_CONFORMING_SWPRINTFS

 // Windows Header Files:
#ifdef _WIN32
#include <windows.h>
#include <windowsx.h>
#endif

// C RunTime Header Files

#include <string>
#include <stdlib.h>
#include <stdio.h>
#include <conio.h>
#include <string.h>
#include <limits.h>
#include <malloc.h>
#include <memory.h>
#include <tchar.h>
#include <commctrl.h>
#include <assert.h>
#include <winioctl.h>
#include <iostream>
#include <Shlwapi.h>
#include <filesystem>

#include "../../include/unicode/cbfsfilter.h"

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

const wchar_t* g_lpGuid = _T("{713CC6CE-B3E2-4fd9-838D-E28F558F6866}");
const wchar_t* ALTITUDE_FAKE_VALUE_FOR_DEBUG(TEXT("360000"));
const unsigned int REPARSE_TAG_FOR_TEST = 0x55;
// {446AA145-BD68-4D6C-AC24-599921595B83}
static const GUID REPARSE_GUID_FOR_TEST =
{ 0x446aa145, 0xbd68, 0x4d6c, { 0xac, 0x24, 0x59, 0x99, 0x21, 0x59, 0x5b, 0x83 } };

CRITICAL_SECTION    g_LogLock = { 0, };

void AddToLog(CBFilter* Watcher, LPCTSTR Operation, LPCTSTR FileName, DWORD Status);
void SetReparsePoint(const wchar_t* pPlaceholderPath, const wchar_t* pSourcePath);
void SetFilter(const wchar_t* pPath);
void DeleteFilter();

class PlaceholderFilter : public CBFilter
{
    wchar_t* m_lpSourcePath = NULL;

public:
    PlaceholderFilter() : CBFilter() {}
    ~PlaceholderFilter() {
        if (m_lpSourcePath != NULL)
            free(m_lpSourcePath);
    }

    const wchar_t* get_SourcePath() const {
        return m_lpSourcePath;
    }
    void set_SourcePath(const wchar_t* pPath) {
        m_lpSourcePath = _tcsdup(pPath);
    }

    INT FireFilterStart(CBFilterFilterStartEventParams* e) {
        AddToLog(this, _T("FilterStart"), NULL, NtStatusToWin32Error(e->ResultCode));
        return 0;
    }

    INT FireFilterStop(CBFilterFilterStopEventParams* e) {
        AddToLog(this, _T("FilterStop"), NULL, NtStatusToWin32Error(e->ResultCode));
        return 0;
    }

    INT FireBeforeCreateFile(CBFilterBeforeCreateFileEventParams* e) {
        AddToLog(this, _T("BeforeCreateFile"), e->FileName, NtStatusToWin32Error(e->ResultCode));
        return 0;
    }

    INT FireAfterCreateFile(CBFilterAfterCreateFileEventParams* e) {
        AddToLog(this, _T("AfterCreateFile"), e->FileName, NtStatusToWin32Error(e->ResultCode));
        return 0;
    }

    INT FireBeforeOpenFile(CBFilterBeforeOpenFileEventParams* e) {
        AddToLog(this, _T("BeforeOpenFile"), e->FileName, NtStatusToWin32Error(e->ResultCode));
        return 0;
    }

    INT FireAfterOpenFile(CBFilterAfterOpenFileEventParams* e) {
        AddToLog(this, _T("AfterOpenFile"), e->FileName, NtStatusToWin32Error(e->ResultCode));
        return 0;
    }

    INT FireBeforeCloseFile(CBFilterBeforeCloseFileEventParams* e) {
        AddToLog(this, _T("BeforeCloseFile"), e->FileName, NtStatusToWin32Error(e->ResultCode));
        return 0;
    }

    INT FireReparseFileName(CBFilterReparseFileNameEventParams* e) {
        AddToLog(this, _T("ReparseFileName"), e->FileName, NtStatusToWin32Error(e->ResultCode));
        return 0;
    }

    INT FireReparseWithTag(CBFilterReparseWithTagEventParams* e) {
        AddToLog(this, _T("ReparseWithTag"), e->FileName, NtStatusToWin32Error(e->ResultCode));

        if (e->ReparseTag != REPARSE_TAG_FOR_TEST) return 0;

        PREPARSE_GUID_DATA_BUFFER pBuffer = (PREPARSE_GUID_DATA_BUFFER)e->ReparseBuffer;
        if (memcmp(&pBuffer->ReparseGuid, &REPARSE_GUID_FOR_TEST, sizeof(GUID))) return 0;

        const wchar_t* sourcePathNameBuffer = _wcsdup((const wchar_t*)pBuffer->GenericReparseBuffer.DataBuffer);

        HANDLE hFile = CreateFile(e->FileName,
            GENERIC_READ | FILE_WRITE_ATTRIBUTES,
            FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
            NULL,
            OPEN_ALWAYS,
            FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT,
            NULL
        );

        if (INVALID_HANDLE_VALUE == hFile)
        {
            e->ResultCode = ::GetLastError();
            free((void*)sourcePathNameBuffer);
            return 0;
        }

        PREPARSE_GUID_DATA_BUFFER pNewBuffer = (PREPARSE_GUID_DATA_BUFFER)malloc(REPARSE_GUID_DATA_BUFFER_HEADER_SIZE);
        ZeroMemory(pNewBuffer, REPARSE_GUID_DATA_BUFFER_HEADER_SIZE);
        pNewBuffer->ReparseTag = pBuffer->ReparseTag;
        memcpy(&pNewBuffer->ReparseGuid, &pBuffer->ReparseGuid, sizeof(GUID));

        DWORD dwBytes;
        if (!DeviceIoControl(hFile, // handle to file or directory
            FSCTL_DELETE_REPARSE_POINT, // dwIoControlCode
            pNewBuffer,                     // input buffer 
            REPARSE_GUID_DATA_BUFFER_HEADER_SIZE,         // size of input buffer
            NULL,                // lpOutBuffer
            0,                          // nOutBufferSize
            &dwBytes,          // lpBytesReturned
            NULL))            // OVERLAPPED structure
        {
            e->ResultCode = ::GetLastError();
        }

        CloseHandle(hFile);

        if (CopyFile(sourcePathNameBuffer, e->FileName, FALSE))
            e->ReissueIO = true;
        free((void*)sourcePathNameBuffer);
        free(pNewBuffer);

        return 0;
    }
};
PlaceholderFilter g_Filter;

void Banner(void)
{
    printf("CBFS Filter Placeholder Demo Copyright (c) 2017-2023, Callback Technologies, Inc.\n\n");
}

void Usage(void)
{
    printf("Usage: placeholder [-<switch 1> ... -<switch N>] [<placeholder path>] [<source path>]\n\n");
    printf("<Switches>\n");
    printf("  -drv <path to cbfilter.cab> - Install the driver from the specified CAB file\n");
    printf("  -- Stop switches scanning\n");
}

int CheckResult(int result)
{
    if (result != 0)
    {
        fprintf(stderr, "Error %d (%s)\n", g_Filter.GetLastErrorCode(), g_Filter.GetLastError());
    }
    return result;
}

void CheckDriver()
{
    int state, number;

    state = g_Filter.GetDriverStatus(g_lpGuid);
    if (CheckResult(g_Filter.GetLastErrorCode()))
        exit(0);
    if (state == cbfConstants::MODULE_STATUS_RUNNING)
    {
        INT64 version;
        version = g_Filter.GetDriverVersion(g_lpGuid);
        if (CheckResult(g_Filter.GetLastErrorCode()))
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

int optcmp(wchar_t* arg, const wchar_t* opt)
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
                }
                return res;
            }
            wchar_t currentDir[_MAX_PATH];
            const char pathSeparator = '\\';
            if (_wgetcwd(currentDir, _MAX_PATH) == nullptr) {
                sout << "Error getting current directory." << std::endl;
                return L"";
            }
#else
            char currentDir[PATH_MAX];
            const char pathSeparator = '/';
            if (getcwd(currentDir, sizeof(currentDir)) == nullptr) {
                sout << "Error getting current directory." << std::endl;
                return "";
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
    }
    return res;
}

int wmain(int argc, wchar_t* argv[])
{
    Banner();

    wchar_t* pPlaceholderPath = NULL;
    wchar_t* pSourcePath = NULL;

    // if no parameters specified, show usage and quit
    if (argc < 3)
    {
        Usage();
        return 0;
    }

    bool stop_opt = false;
    for (int argi = 1; argi < argc; argi++)
    {
        wchar_t* arg = argv[argi];
        size_t arg_len = wcslen(arg);
        if (arg_len > 0)
        {
            if ((arg[0] == '-') && !stop_opt)
            {
                if (optcmp(arg, _T("-drv")))
                {
                    cbt_string arg_wstr = ConvertRelativePathToAbsolute(argv[++argi]);
                    arg = const_cast<wchar_t*>(arg_wstr.c_str());
                    if (argi < argc)
                    {
                        _tprintf(_T("Installing the driver from '%s'\n"), arg);
                        int drv_reboot = g_Filter.Install(arg, g_lpGuid, NULL,
                            ALTITUDE_FAKE_VALUE_FOR_DEBUG, 0);

                        int errcode = g_Filter.GetLastErrorCode();
                        if (errcode != 0)
                        {
                            if (errcode == ERROR_PRIVILEGE_NOT_HELD)
                                printf("The driver could not be installed, please try to install with administrator rights\n");
                            else
                                printf("The driver could not be installed, the error is %d (%s)\n", errcode, g_Filter.GetLastError());

                            exit(errcode);
                        }
                        printf("Drivers installed successfully\n");
                        if (drv_reboot != 0)
                        {
                            printf(", reboot is required\n");
                            exit(1);
                        }
                    }
                }
                else
                    fwprintf(stderr, _T("Invalid option : %s"), arg);
            }
            else
            {
                // if we have not set the placeholder path yet, do this now. Otherwise, set the source path
                if (pPlaceholderPath == NULL) {
                    cbt_string placeholder_path_wstr = ConvertRelativePathToAbsolute(_tcsdup(arg));
                    pPlaceholderPath = _tcsdup(placeholder_path_wstr.c_str());
                }
                else {
                    cbt_string source_path_wstr = ConvertRelativePathToAbsolute(_tcsdup(arg));
                    pSourcePath = _tcsdup(source_path_wstr.c_str());
                    break; // no more parameters expected
                }
            }
        }
    }

    CheckDriver();

    // Probably, we have just installed the driver, so we can quit
    if (pPlaceholderPath == NULL)
        exit(0);

    if (pSourcePath == NULL) {
        Usage();
        exit(0);
    }

    _tprintf(_T("Press any key to stop and quit.\n"));
    InitializeCriticalSection(&g_LogLock);
    try {
        SetReparsePoint(pPlaceholderPath, pSourcePath);
        g_Filter.set_SourcePath(pSourcePath);
        SetFilter(pPlaceholderPath);
        _getch();
        DeleteFilter();
    }
    catch (int e) {
    }

    if (pPlaceholderPath)
        free(pPlaceholderPath);
    if (pSourcePath)
        free(pSourcePath);

    return 0;
}

void AddToLog(CBFilter* Watcher, LPCTSTR Operation, LPCTSTR FileName, DWORD Status)
{
    EnterCriticalSection(&g_LogLock);
    _tprintf(_T("%s - %s (operation status = %d)\n"), Operation, FileName, Status);
    LeaveCriticalSection(&g_LogLock);
}

void SetReparsePoint(const wchar_t* pPlaceholderPath, const wchar_t* pSourcePath)
//
// pPlaceholderPath - path to the placeholder file to be created and
//                    set as a custom reparse point
// pSourcePath - source file path that we put into the custom reparse guid buffer,
//               it will be used in CBFSFltReparseWithTag event handler as a source
//               of the data copied into placeholder file
{
    HANDLE hFile = CreateFile(pPlaceholderPath,
        GENERIC_READ | FILE_WRITE_ATTRIBUTES,
        FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
        NULL,
        CREATE_ALWAYS,
        FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT,
        NULL
    );

    if (INVALID_HANDLE_VALUE == hFile)
    {
        _tprintf(_T("Error %d while creating placeholder file\n"), GetLastError());
        exit(-1);
    }

    size_t pathLen = wcslen(pSourcePath);
    PREPARSE_GUID_DATA_BUFFER pReparseData =
        (PREPARSE_GUID_DATA_BUFFER)malloc(REPARSE_GUID_DATA_BUFFER_HEADER_SIZE + (pathLen + 1) * sizeof(wchar_t));
    ZeroMemory(pReparseData, REPARSE_GUID_DATA_BUFFER_HEADER_SIZE + +(pathLen + 1) * sizeof(wchar_t));

    pReparseData->ReparseTag = REPARSE_TAG_FOR_TEST;
    pReparseData->ReparseDataLength = (pathLen + 1) * sizeof(wchar_t);
    memcpy(pReparseData->GenericReparseBuffer.DataBuffer, pSourcePath, pathLen * sizeof(wchar_t));
    memcpy(&pReparseData->ReparseGuid, &REPARSE_GUID_FOR_TEST, sizeof(GUID));

    DWORD bytesReturned;
    if (!DeviceIoControl(hFile,                             // handle to file or directory
        FSCTL_SET_REPARSE_POINT,                            // dwIoControlCode
        pReparseData,                                       // input buffer 
        REPARSE_GUID_DATA_BUFFER_HEADER_SIZE + (pathLen + 1) * sizeof(wchar_t), // size of input buffer
        NULL,                                               // lpOutBuffer
        0,                                                  // nOutBufferSize
        &bytesReturned,                                     // lpBytesReturned
        NULL))                                    // OVERLAPPED structure
    {
        _tprintf(_T("Error %d while creating reparse point\n"), GetLastError());
        exit(-1);
    }

    free(pReparseData);
    CloseHandle(hFile);
}

void SetFilter(const wchar_t* pPath)
{
    // Add a filtering rule
    g_Filter.AddFilterRule(pPath,
        cbfConstants::ACCESS_NONE,
        cbfConstants::FS_CE_REPARSE_TAG |
        cbfConstants::FS_CE_BEFORE_CREATE |
        cbfConstants::FS_CE_AFTER_CREATE |
        cbfConstants::FS_CE_BEFORE_OPEN |
        cbfConstants::FS_CE_AFTER_OPEN |
        cbfConstants::FS_CE_BEFORE_CLOSE,
        cbfConstants::FS_NE_NONE);

    // Initialize and start the filter
    int retVal = g_Filter.Initialize(g_lpGuid);
    if (0 == retVal)
        retVal = g_Filter.StartFilter(30000);

    if (retVal != 0)
    {
        _tprintf(_T("Error %d while trying to start filtering\n"), retVal);
        exit(retVal);
    }
}

void DeleteFilter()
{
    g_Filter.StopFilter(TRUE);
}

 
 

