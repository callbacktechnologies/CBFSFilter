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
#include <iostream>
#include <Shlwapi.h>
#include <filesystem>

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

LPCTSTR g_Guid = _T("{713CC6CE-B3E2-4fd9-838D-E28F558F6866}");
BOOL g_Closing = FALSE;

void AddToLog(CBMonitor* Watcher, LPCTSTR Operation, LPCTSTR FileName, DWORD Status);

class FileWatcher : public CBMonitor
{
public:
    FileWatcher() : CBMonitor()
    {
    }

    INT FireNotifyCreateFile(CBMonitorNotifyCreateFileEventParams* e)
    {
        AddToLog(this, _T("NotifyCreateFile"), e->FileName, NtStatusToWin32Error(e->Status));
        return 0;
    }

    INT FireNotifyOpenFile(CBMonitorNotifyOpenFileEventParams* e)
    {
        AddToLog(this, _T("NotifyOpenFile"), e->FileName, NtStatusToWin32Error(e->Status));
        return 0;
    }

    INT FireNotifySetAllocationSize(CBMonitorNotifySetAllocationSizeEventParams* e)
    {
        AddToLog(this, _T("NotifySetAllocationSize"), e->FileName, NtStatusToWin32Error(e->Status));
        return 0;
    }

    INT FireNotifySetFileSize(CBMonitorNotifySetFileSizeEventParams* e)
    {
        AddToLog(this, _T("NotifySetFileSize"), e->FileName, NtStatusToWin32Error(e->Status));
        return 0;
    }

    INT FireNotifySetFileAttributes(CBMonitorNotifySetFileAttributesEventParams* e)
    {
        AddToLog(this, _T("NotifySetFileAttributes"), e->FileName, NtStatusToWin32Error(e->Status));
        return 0;
    }

    INT FireNotifyRenameOrMoveFile(CBMonitorNotifyRenameOrMoveFileEventParams* e)
    {
        AddToLog(this, _T("NotifyRenameOrMoveFile"), e->FileName, NtStatusToWin32Error(e->Status));
        return 0;
    }

    INT FireNotifyCreateHardLink(CBMonitorNotifyCreateHardLinkEventParams* e)
    {
        AddToLog(this, _T("NotifyCreateHardLink"), e->LinkName, NtStatusToWin32Error(e->Status));
        return 0;
    }

    INT FireNotifyReadFile(CBMonitorNotifyReadFileEventParams* e)
    {
        AddToLog(this, _T("NotifyReadFile"), e->FileName, NtStatusToWin32Error(e->Status));
        return 0;
    }

    INT FireNotifyWriteFile(CBMonitorNotifyWriteFileEventParams* e)
    {
        AddToLog(this, _T("NotifyWriteFile"), e->FileName, NtStatusToWin32Error(e->Status));
        return 0;
    }

    INT FireNotifyDeleteFile(CBMonitorNotifyDeleteFileEventParams* e)
    {
        AddToLog(this, _T("NotifyDeleteFile"), e->FileName, 0);
        return 0;
    }

    INT FireNotifyEnumerateDirectory(CBMonitorNotifyEnumerateDirectoryEventParams* e)
    {
        size_t len = _tcslen(e->DirectoryName) + _tcslen(e->FileName) + 2;
        LPTSTR buf = (LPTSTR)malloc(len * sizeof(TCHAR));

        if (buf != 0)
        {
            _tcscpy_s(buf, len, e->DirectoryName);
            _tcscat_s(buf, len, _T("\\"));
            _tcscat_s(buf, len, e->FileName);
            AddToLog(this, _T("NotifyEnumerateDirectory"), buf, NtStatusToWin32Error(e->Status));

            free(buf);
        }
        return 0;
    }

    INT FireNotifyCloseFile(CBMonitorNotifyCloseFileEventParams* e)
    {
        AddToLog(this, _T("NotifyCloseFile"), e->FileName, 0);
        return 0;
    }

    INT FireNotifyCleanupFile(CBMonitorNotifyCleanupFileEventParams* e)
    {
        AddToLog(this, _T("NotifyCleanupFile"), e->FileName, 0);
        return 0;
    }

    INT FireNotifySetFileSecurity(CBMonitorNotifySetFileSecurityEventParams* e)
    {
        AddToLog(this, _T("NotifySetFileSecurity"), e->FileName, NtStatusToWin32Error(e->Status));
        return 0;
    }

    INT FireFilterStop(CBFilterFilterStopEventParams* e)
    {
        return 0;
    }
};


FileWatcher g_CBMonitor;

#define CBFSFILTER_LOG_OPERATION             0
#define CBFSFILTER_LOG_PATH                  1
#define CBFSFILTER_LOG_ORIGINATOR_PROCESS    2
#define CBFSFILTER_LOG_PROCESS_ID            3
#define CBFSFILTER_LOG_USER_NAME             4
#define CBFSFILTER_LOG_RESULT                5

CRITICAL_SECTION    g_LogListLock = { 0, };

typedef __int64 int64;

const cbt_string ALTITUDE_FAKE_VALUE_FOR_DEBUG(TEXT("360000.24"));

const cbt_string product_id(TEXT("{713CC6CE-B3E2-4fd9-838D-E28F558F6866}"));
const cbt_string strInvalidOption(TEXT("Invalid option \"%s\"\n"));

const std::string strMonitorError("Error %d (%s)\n");

void SetFilter(LPCTSTR PathToMonitor, LPCTSTR FilenameMask);
void DeleteFilter();

using namespace std;

void Banner(void)
{
    printf("CBFS Filter FileMon Demo Copyright (c) 2017-2023, Callback Technologies, Inc.\n\n");
}

void Usage(void)
{
    printf("Usage: filemon [-<switch 1> ... -<switch N>] [<path to monitor> [<filename mask to monitor>]]\n\n");
    printf("<Switches>\n");
    printf("  -drv <path to cbfilter.cab> - Install the driver from the specified CAB file\n");
    printf("  -- Stop switches scanning\n");
}

// ----------------------------------------------------------------------------------

int CheckResult(int result)
{
    if (result != 0)
    {
        fprintf(stderr, strMonitorError.c_str(), g_CBMonitor.GetLastErrorCode(), g_CBMonitor.GetLastError());
    }
    return result;
}


void CheckDriver()
{
    int state, number;

    state = g_CBMonitor.GetDriverStatus(product_id.c_str());
    if (CheckResult(g_CBMonitor.GetLastErrorCode()))
        exit(0);
    if (state == cbfConstants::MODULE_STATUS_RUNNING)
    {
        int64 version;
        version = g_CBMonitor.GetDriverVersion(product_id.c_str());
        if (CheckResult(g_CBMonitor.GetLastErrorCode()))
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

int optcmp(cbt_string arg, string opt)
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
                return _T("");
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

int wmain(int argc, wchar_t* argv[])
{
    INT drv_reboot = 0;
    INT opt_terminate = 0;

    LPTSTR PathToMonitor = NULL;
    LPTSTR FilenameMask = NULL;

    int res_code = 0, argi = 0, arg_len = 0, stop_opt = 0;

    Banner();

    // if no parameters specified, show usage and quit
    if (argc < 2)
    {
        Usage();
        CheckDriver();
        return 0;
    }

    for (argi = 1; argi < argc; argi++)
    {
        cbt_string arg = argv[argi];
        arg_len = arg.length();
        if (arg_len > 0)
        {
            if ((arg[0] == '-') && !stop_opt)
            {
                if (optcmp(arg, "-drv"))
                {
                    arg = argv[++argi];
                    if (argi < argc)
                    {
                        _tprintf(_T("Installing the driver from '%s'\n"), arg.c_str());
                        cbt_string drv_path_wstr = ConvertRelativePathToAbsolute(arg.c_str());
                        if (drv_path_wstr.empty()) {
                            printf("Error: Invalid Driver Path\n");
                            exit(1); 
                        }
                        drv_reboot = g_CBMonitor.Install(drv_path_wstr.c_str(), product_id.c_str(), NULL,
                            ALTITUDE_FAKE_VALUE_FOR_DEBUG.c_str(), 0, NULL);// cbfConstants::INSTALL_REMOVE_OLD_VERSIONS);

                        int errcode = g_CBMonitor.GetLastErrorCode();
                        if (errcode != 0)
                        {
                            if (errcode == ERROR_PRIVILEGE_NOT_HELD)
                                printf("The driver could not be installed, please try to install with administrator rights\n");
                            else
                                printf("The driver could not be installed, the error is %d (%s)\n", errcode, g_CBMonitor.GetLastError());

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
                    fwprintf(stderr, strInvalidOption.c_str(), arg.c_str());
            }
            else
            {
                // if we have not set the path yet, do this now. Otherwise, set the mask
                if (PathToMonitor == NULL)
                {
                    cbt_string path_to_monitor_wstr = ConvertRelativePathToAbsolute(_tcsdup(arg.c_str()));
                    if(path_to_monitor_wstr.empty()) {
                        printf("Error: Invalid Path To Monitor\n");
                        exit(1); 
                    }
                    PathToMonitor = _tcsdup(path_to_monitor_wstr.c_str());
                    DWORD attr = GetFileAttributes(PathToMonitor);
                    if (attr == INVALID_FILE_ATTRIBUTES || (attr & FILE_ATTRIBUTE_DIRECTORY) == 0)
                    {
                        _tprintf(_T("ERROR: the specified path '%s' does not point to an existing directory\n"), PathToMonitor);
                        exit(3);
                    }
                }
                else
                {
                    FilenameMask = _tcsdup(arg.c_str());
                    break; // no more parameters expected
                }
            }
        }
    }

    CheckDriver();

    // Probably, we have just installed the driver, so we can quit without monitoring anything
    if (PathToMonitor == NULL)
        exit(0);

    _tprintf(_T("Press any key to start monitoring, then press any key to stop and quit.\n"));

    _getch();

    InitializeCriticalSection(&g_LogListLock);
    try
    {
        SetFilter(PathToMonitor, FilenameMask);
        _getch();
        DeleteFilter();
    }
    catch (int e)
    {
    }

    if (PathToMonitor)
        free(PathToMonitor);
    if (FilenameMask)
        free(FilenameMask);

    return 0;
}

void AddToLog(CBMonitor* Watcher, LPCTSTR Operation, LPCTSTR FileName, DWORD Status)
{
    EnterCriticalSection(&g_LogListLock);
    _tprintf(_T("%s - %s (operation status = %d)\n"), Operation, FileName, Status);
    LeaveCriticalSection(&g_LogListLock);
}

void SetFilter(LPCTSTR PathToMonitor, LPCTSTR FilenameMask)
{
    // Compose the filtering rule by concatenating the path and the mask
    int l = _tcslen(PathToMonitor);
    int path_len = l;

    if (l == 0)
    {
        printf("Nothing to monitor, quitting");
        exit(3);
    }

    if (FilenameMask != NULL)
        l += _tcslen(FilenameMask);
    else
        l += 1; // for '*'
    l += 2; // for the separator and the trailing \0

    LPTSTR mask_buf = (LPTSTR)malloc(l * sizeof(TCHAR));
    _tcscpy(mask_buf, PathToMonitor);
    if (PathToMonitor[path_len - 1] != '\\')
        _tcscat(mask_buf, _T("\\"));
    if (FilenameMask != NULL)
        _tcscat(mask_buf, FilenameMask);
    else
        _tcscat(mask_buf, _T("*"));

    // Add a filtering rule
    g_CBMonitor.AddFilterRule(mask_buf,
        cbfConstants::FS_NE_READ |
        cbfConstants::FS_NE_WRITE |
        cbfConstants::FS_NE_DELETE |
        cbfConstants::FS_NE_CREATE |
        cbfConstants::FS_NE_RENAME |
        cbfConstants::FS_NE_CREATE_HARD_LINK |
        cbfConstants::FS_NE_SET_SIZES |
        cbfConstants::FS_NE_DELETE |
        cbfConstants::FS_NE_ENUMERATE_DIRECTORY |
        cbfConstants::FS_NE_OPEN |
        cbfConstants::FS_NE_CLOSE |
        cbfConstants::FS_NE_CLEANUP |
        cbfConstants::FS_NE_SET_SECURITY);

    // Initialize and start the filter
    int retVal = g_CBMonitor.SetProcessFailedRequests(TRUE);
    if (0 == retVal)
        retVal = g_CBMonitor.Initialize(g_Guid);
    if (0 == retVal)
        retVal = g_CBMonitor.StartFilter();

    if (retVal != 0)
    {
        _tprintf(_T("Error %d while trying to start filtering\n"), retVal);
        exit(retVal);

    }
    _tprintf(_T("Started the monitor with the rule mask '%s'\n"), mask_buf);
}

void DeleteFilter()
{
    g_CBMonitor.StopFilter();
}

