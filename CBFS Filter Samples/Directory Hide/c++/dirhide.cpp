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

const TCHAR DirectorySeparatorChar =
#ifdef _WIN32
_T('\\');
#else
_T('/');
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

const TCHAR DirectorySeparator[] = { DirectorySeparatorChar, _T('\0') };

LPCTSTR g_Guid = _T("{713CC6CE-B3E2-4fd9-838D-E28F558F6866}");
BOOL g_Closing = FALSE;

void AddToLog(CBFilter* Filter, LPCTSTR Operation, LPCTSTR FileName, DWORD Status);

class CBFSFilter : public CBFilter
{
public:
    static TCHAR mPathToHide[MAX_PATH];
    static PTCHAR mOwnNameToHide;

    CBFSFilter() : CBFilter()
    {
    }

    INT FireFilterStop(CBFilterFilterStopEventParams* e)
    {
        return 0;
    }

    INT FireNotifyRenameOrMoveFile(CBFilterNotifyRenameOrMoveFileEventParams* e)
    {
        AddToLog(this, _T("NotifyRenameOrMoveFile"), e->FileName, NtStatusToWin32Error(e->Status));
        return 0;
    }

    INT FireBeforeCreateFile(CBFilterBeforeCreateFileEventParams* e)
    {
        if (!_tcsicmp(e->FileName, mPathToHide))
        {
            AddToLog(this, _T("BeforeCreateFile"), e->FileName, ERROR_FILE_NOT_FOUND);
            e->ResultCode = ERROR_FILE_NOT_FOUND;
            return 0;
        }

        const TCHAR* Separator = _tcsrchr(e->FileName, DirectorySeparatorChar);
        if (Separator && !_tcsnicmp(e->FileName, mPathToHide, Separator - e->FileName))
        {
            e->ResultCode = ERROR_PATH_NOT_FOUND;
        }
        /*AddToLog(this, _T("BeforeCreateFile"), e->FileName, NtStatusToWin32Error(e->Status));*/
        return 0;
    }

    INT FireBeforeOpenFile(CBFilterBeforeOpenFileEventParams* e)
    {
        if (!_tcsicmp(e->FileName, mPathToHide))
        {
            AddToLog(this, _T("BeforeOpenFile"), e->FileName, ERROR_FILE_NOT_FOUND);
            e->ResultCode = ERROR_FILE_NOT_FOUND;
            return 0;
        }

        const TCHAR* Separator = _tcsrchr(e->FileName, DirectorySeparatorChar);
        if (Separator && !_tcsnicmp(e->FileName, mPathToHide, Separator - e->FileName))
        {
            e->ResultCode = ERROR_PATH_NOT_FOUND;
        }
        //AddToLog(this, _T("NotifyOpenFile"), e->FileName, NtStatusToWin32Error(e->Status));
        return 0;
    }

    INT FireAfterEnumerateDirectory(CBFilterAfterEnumerateDirectoryEventParams* e)
    {
        // if the filesystem returned the name of the directory that we aim to hide, then hide it
        if (!_tcsicmp(e->FileName, mOwnNameToHide))
        {
            AddToLog(this, _T("AfterEnumerateDirectory"), e->FileName, ERROR_FILE_NOT_FOUND);
            e->ProcessRequest = FALSE;
            return 0;
        }

        //AddToLog(this, _T("NotifyEnumerateDirectory"), buf, NtStatusToWin32Error(e->Status));
        return 0;
    }
};

TCHAR CBFSFilter::mPathToHide[MAX_PATH] = { 0 };
PTCHAR CBFSFilter::mOwnNameToHide = NULL;

CBFSFilter g_CBFSFlt;

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

const std::string strFilterError("Error %d (%s)\n");

void HideDir(LPTSTR PathToHide);
void UnhideDir();

void SetFilter(LPCTSTR PathToMonitor, LPCTSTR FilenameMask);
void DeleteFilter();

using namespace std;

void Banner(void)
{
    printf("CBFS Filter DirHide Demo Copyright (c) 2017-2023, Callback Technologies, Inc.\n\n");
}

void Usage(void)
{
    printf("Usage: dirhide [-<switch 1> ... -<switch N>] <path to hide>\n\n");
    printf("<Switches>\n");
    printf("  -drv <path to cbfilter.cab> - Install the driver from the specified CAB file\n");
    printf("  -- Stop switches scanning\n");
}

// ----------------------------------------------------------------------------------

int CheckResult(int result)
{
    if (result != 0)
    {
        fprintf(stderr, strFilterError.c_str(), g_CBFSFlt.GetLastErrorCode(), g_CBFSFlt.GetLastError());
    }
    return result;
}

void CheckDriver()
{
    int state, number;

    state = g_CBFSFlt.GetDriverStatus(product_id.c_str());
    if (CheckResult(g_CBFSFlt.GetLastErrorCode()))
        exit(0);
    if (state == cbfConstants::MODULE_STATUS_RUNNING)
    {
        int64 version;
        version = g_CBFSFlt.GetDriverVersion(product_id.c_str());
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

int wmain(int argc, wchar_t* argv[]) {
    INT drv_reboot = 0;
    LPTSTR PathToHide = NULL;
    int res_code = 0, argi = 0, arg_len = 0, stop_opt = 0;

    Banner();

    // if no parameters specified, show usage and quit
    if (argc < 2) {
        Usage();
        CheckDriver();
        return 0;
    }

    for (argi = 1; argi < argc; argi++) {
        cbt_string arg = argv[argi];
        arg_len = (int)arg.length();
        if (arg_len > 0) {
            if ((arg[0] == '-') && !stop_opt) {
                if (optcmp(arg, "-drv")) {
                    arg = argv[++argi];
                    if (argi < argc) {
                        _tprintf(_T("Installing the driver from '%s'\n"), arg.c_str());
                        cbt_string drv_path_wstr = ConvertRelativePathToAbsolute(arg);
                        if (drv_path_wstr.empty()) {
                            printf("Error: Invalid Driver Path\n");
                            exit(1); 
                        }
                        drv_reboot = g_CBFSFlt.Install(const_cast<LPTSTR>(drv_path_wstr.c_str()), product_id.c_str(), NULL,
                            ALTITUDE_FAKE_VALUE_FOR_DEBUG.c_str(), 0, NULL);// cbfConstants::INSTALL_REMOVE_OLD_VERSIONS);

                        int errcode = g_CBFSFlt.GetLastErrorCode();
                        if (errcode != 0) {
                            if (errcode == ERROR_PRIVILEGE_NOT_HELD)
                                printf("The driver could not be installed, please try to install with administrator rights\n");
                            else
                                printf("The driver could not be installed, the error is %d (%s)\n", errcode, g_CBFSFlt.GetLastError());

                            exit(errcode);
                        }
                        printf("Drivers installed successfully\n");
                        if (drv_reboot != 0) {
                            printf(", reboot is required\n");
                            exit(1);
                        }
                    }
                }
                else
                    fwprintf(stderr, strInvalidOption.c_str(), arg.c_str());
            }
            else {
                cbt_string path_to_hide_wstr = ConvertRelativePathToAbsolute(_tcsdup(arg.c_str()));
                if (path_to_hide_wstr.empty()) {
                    printf("Error: Invalid Path To Hide\n");
                    exit(1); 
                }
                PathToHide = _tcsdup(path_to_hide_wstr.c_str());
                DWORD attr = GetFileAttributes(PathToHide);
                if (attr == INVALID_FILE_ATTRIBUTES || (attr & FILE_ATTRIBUTE_DIRECTORY) == 0) {
                    _tprintf(_T("ERROR: the specified path '%s' does not point to an existing directory\n"), PathToHide);
                    exit(3);
                }
            }
        }
    }

    CheckDriver();

    // Probably, we have just installed the driver, so we can quit without monitoring anything
    if (PathToHide == NULL)
        exit(0);

    _tprintf(_T("Press any key to start hiding, then press any key to stop and quit.\n"));

    _getch();

    InitializeCriticalSection(&g_LogListLock);
    try {
        HideDir(PathToHide);
        //SetFilter(PathToMonitor, FilenameMask);
        _getch();
        UnhideDir();
        //DeleteFilter();
    }
    catch (...) {
    }

    if (PathToHide)
        free(PathToHide);

    return 0;
}

void AddToLog(CBFilter* Filter, LPCTSTR Operation, LPCTSTR FileName, DWORD Status)
{
    EnterCriticalSection(&g_LogListLock);
    _tprintf(_T("%s - %s (operation status = %d)\n"), Operation, FileName, Status);
    LeaveCriticalSection(&g_LogListLock);
}

void HideDir(LPTSTR PathToHide)
{
    TCHAR Path[MAX_PATH], Parent[MAX_PATH];

    _tcscpy(CBFSFilter::mPathToHide, PathToHide);

    // strip ending backslashes
    while (CBFSFilter::mPathToHide[_tcslen(CBFSFilter::mPathToHide) - 1] == DirectorySeparatorChar)
    {
        CBFSFilter::mPathToHide[_tcslen(CBFSFilter::mPathToHide) - 1] = _T('\0');
    }

    CBFSFilter::mOwnNameToHide = _tcsrchr(CBFSFilter::mPathToHide, DirectorySeparatorChar);
    // Check that it's not a root to be hidden
    if (!CBFSFilter::mOwnNameToHide)
    {
        _tprintf(_T("ERROR: The specified directory is root, and roots cannot be hidden."));
        return;
    }

    _tcsncpy(Parent, CBFSFilter::mPathToHide, CBFSFilter::mOwnNameToHide - CBFSFilter::mPathToHide + 1);
    Parent[CBFSFilter::mOwnNameToHide - CBFSFilter::mPathToHide + 1] = 0;//_tcsncpy: No null wide character is implicitly appended at the end of destination if source is longer than num (thus, in this case, destination may not be a null terminated C wide string).

    CBFSFilter::mOwnNameToHide++;

    // Hide directory from the parent's enumerations
    g_CBFSFlt.AddFilterRule(Parent,
        cbfConstants::ACCESS_NONE,
        cbfConstants::FS_CE_AFTER_ENUMERATE_DIRECTORY,
        cbfConstants::FS_NE_NONE);

    // Prevent direct opening of the directory
    g_CBFSFlt.AddFilterRule(CBFSFilter::mPathToHide,
        cbfConstants::ACCESS_NONE,
        cbfConstants::FS_CE_BEFORE_OPEN |
        cbfConstants::FS_CE_BEFORE_CREATE,
        cbfConstants::FS_NE_NONE);

    // Prevent direct operations on files in the directory
    _tcscpy(Path, CBFSFilter::mPathToHide);
    _tcscat(Path, DirectorySeparator);
    _tcscat(Path, _T("*.*"));
    g_CBFSFlt.AddFilterRule(Path,
        cbfConstants::ACCESS_NONE,
        cbfConstants::FS_CE_BEFORE_OPEN |
        cbfConstants::FS_CE_BEFORE_CREATE,
        cbfConstants::FS_NE_NONE);

    int retVal = g_CBFSFlt.SetProcessFailedRequests(TRUE);
    if (0 == retVal)
        retVal = g_CBFSFlt.Initialize(g_Guid);
    if (0 == retVal)
        retVal = g_CBFSFlt.StartFilter(30000);

    if (0 != retVal)
        CheckResult(retVal);
}

void UnhideDir()
{
    g_CBFSFlt.DeleteAllFilterRules();
    g_CBFSFlt.StopFilter();
}


