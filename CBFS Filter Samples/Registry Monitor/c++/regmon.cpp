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
#include <filesystem>

#ifdef _UNICODE
#include "../../include/unicode/cbfsfilter.h"
#else
#include "../../include/cbfsfilter.h"
#endif

#define ASSERT assert

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

void AddToLog(CBRegistry* Filter, LPCTSTR Operation, LPTSTR KeyName, PVOID UserContext, DWORD Result = 0, LPTSTR Details = NULL);

class RegFilter : public CBRegistry
{
    // user defined context structure

    typedef struct _CBREG_USER_CONTEXT {

        DWORD DesiredAccess;
        HANDLE KeyHandle;
        TCHAR FullPath[1];

    } CBREG_USER_CONTEXT, * PCBREG_USER_CONTEXT;

public:
    RegFilter() : CBRegistry()
    {

    }

    int FireAfterCloseKey(CBRegistryAfterCloseKeyEventParams* e)
    {
        PCBREG_USER_CONTEXT ctx = (PCBREG_USER_CONTEXT)e->KeyContext;
        ASSERT(ctx);
        AddToLog(this, _T("RegAfterCloseKey"), ctx->FullPath, ctx);
        free(ctx);
        return 0;
    }

    int FireAfterCreateKey(CBRegistryAfterCreateKeyEventParams* e)
    {
        PCBREG_USER_CONTEXT ctx = (PCBREG_USER_CONTEXT)e->KeyContext;
        ASSERT(ctx);
        AddToLog(this, _T("RegAfterCreateKey"), ctx->FullPath, ctx);
        if (e->Status != ERROR_SUCCESS || e->StopFiltering)
            free(e->KeyContext);
        return 0;
    }

    int FireAfterDeleteKey(CBRegistryAfterDeleteKeyEventParams* e)
    {
        PCBREG_USER_CONTEXT ctx = (PCBREG_USER_CONTEXT)e->KeyContext;
        ASSERT(ctx);
        AddToLog(this, _T("RegAfterDeleteKey"), ctx->FullPath, ctx, e->Status);
        return 0;
    }

    int FireAfterDeleteValue(CBRegistryAfterDeleteValueEventParams* e)
    {
        PCBREG_USER_CONTEXT ctx = (PCBREG_USER_CONTEXT)e->KeyContext;
        ASSERT(ctx);
        AddToLog(this, _T("RegAfterDeleteValue"), ctx->FullPath, ctx, e->Status);
        return 0;
    }
    int FireAfterEnumerateKey(CBRegistryAfterEnumerateKeyEventParams* e)
    {
        PCBREG_USER_CONTEXT ctx = (PCBREG_USER_CONTEXT)e->KeyContext;
        ASSERT(ctx);
        AddToLog(this, _T("RegAfterEnumerateKey"), ctx->FullPath, ctx, e->Status);
        return 0;
    }

    int FireAfterEnumerateValue(CBRegistryAfterEnumerateValueEventParams* e)
    {
        PCBREG_USER_CONTEXT ctx = (PCBREG_USER_CONTEXT)e->KeyContext;
        ASSERT(ctx);
        AddToLog(this, _T("RegAfterEnumerateValue"), ctx->FullPath, ctx, e->Status);
        return 0;
    }

    int FireAfterOpenKey(CBRegistryAfterOpenKeyEventParams* e)
    {
        PCBREG_USER_CONTEXT ctx = (PCBREG_USER_CONTEXT)e->KeyContext;
        ASSERT(ctx);
        AddToLog(this, _T("RegAfterOpenKey"), ctx->FullPath, ctx, e->Status);
        if (e->Status != ERROR_SUCCESS || e->StopFiltering)
            free(e->KeyContext);
        return 0;
    }

    int FireAfterQueryKey(CBRegistryAfterQueryKeyEventParams* e)
    {
        PCBREG_USER_CONTEXT ctx = (PCBREG_USER_CONTEXT)e->KeyContext;
        ASSERT(ctx);
        AddToLog(this, _T("RegAfterQueryKey"), ctx->FullPath, ctx, e->Status);
        return 0;
    }

    int FireAfterQueryValue(CBRegistryAfterQueryValueEventParams* e)
    {
        PCBREG_USER_CONTEXT ctx = (PCBREG_USER_CONTEXT)e->KeyContext;
        ASSERT(ctx);
        AddToLog(this, _T("RegAfterQueryValue"), ctx->FullPath, ctx, e->Status);
        return 0;
    }

    int FireAfterRenameKey(CBRegistryAfterRenameKeyEventParams* e)
    {
        PCBREG_USER_CONTEXT ctx = (PCBREG_USER_CONTEXT)e->KeyContext;
        ASSERT(ctx);
        AddToLog(this, _T("RegAfterRename"), ctx->FullPath, ctx, e->Status);
        return 0;
    }

    int FireAfterSetKey(CBRegistryAfterSetKeyEventParams* e)
    {
        PCBREG_USER_CONTEXT ctx = (PCBREG_USER_CONTEXT)e->KeyContext;
        ASSERT(ctx);
        AddToLog(this, _T("RegAfterSetKey"), ctx->FullPath, ctx, e->Status);
        return 0;
    }

    int FireAfterSetValue(CBRegistryAfterSetValueEventParams* e)
    {
        PCBREG_USER_CONTEXT ctx = (PCBREG_USER_CONTEXT)e->KeyContext;
        ASSERT(ctx);
        AddToLog(this, _T("RegAfterSetValue"), ctx->FullPath, ctx, e->Status);
        return 0;
    }

    int FireBeforeCloseKey(CBRegistryBeforeCloseKeyEventParams* e)
    {
        PCBREG_USER_CONTEXT ctx = (PCBREG_USER_CONTEXT)e->KeyContext;
        ASSERT(ctx);
        return 0;
    }

    int FireBeforeCreateKey(CBRegistryBeforeCreateKeyEventParams* e)
    {
        size_t len = (_tcslen(e->FullName) + 1) * sizeof(TCHAR) + sizeof(CBREG_USER_CONTEXT);
        PCBREG_USER_CONTEXT ctx = (PCBREG_USER_CONTEXT)malloc(len);
        ZeroMemory(ctx, len);
        _tcscpy(&ctx->FullPath[0], e->FullName);
        ctx->DesiredAccess = e->DesiredAccess;
        e->KeyContext = ctx;
        return 0;
    }

    int FireBeforeDeleteKey(CBRegistryBeforeDeleteKeyEventParams* e)
    {
        PCBREG_USER_CONTEXT ctx = (PCBREG_USER_CONTEXT)e->KeyContext;
        ASSERT(ctx);
        return 0;
    }

    int FireBeforeDeleteValue(CBRegistryBeforeDeleteValueEventParams* e)
    {
        PCBREG_USER_CONTEXT ctx = (PCBREG_USER_CONTEXT)e->KeyContext;
        ASSERT(ctx);
        return 0;
    }

    int FireBeforeEnumerateKey(CBRegistryBeforeEnumerateKeyEventParams* e)
    {
        PCBREG_USER_CONTEXT ctx = (PCBREG_USER_CONTEXT)e->KeyContext;
        ASSERT(ctx);
        return 0;
    }

    int FireBeforeEnumerateValue(CBRegistryBeforeEnumerateValueEventParams* e)
    {
        PCBREG_USER_CONTEXT ctx = (PCBREG_USER_CONTEXT)e->KeyContext;
        ASSERT(ctx);
        AddToLog(this, _T("RegPreEnumerateValue"), ctx->FullPath, ctx);
        return 0;
    }

    int FireBeforeOpenKey(CBRegistryBeforeOpenKeyEventParams* e)
    {
        size_t len = (_tcslen(e->FullName) + 1) * sizeof(TCHAR) + sizeof(CBREG_USER_CONTEXT);
        PCBREG_USER_CONTEXT ctx = (PCBREG_USER_CONTEXT)malloc(len);
        ZeroMemory(ctx, len);
        _tcscpy(&ctx->FullPath[0], e->FullName);
        ctx->DesiredAccess = e->DesiredAccess;
        e->KeyContext = ctx;
        return 0;
    }

    int FireBeforeQueryKey(CBRegistryBeforeQueryKeyEventParams* e)
    {
        PCBREG_USER_CONTEXT ctx = (PCBREG_USER_CONTEXT)e->KeyContext;
        ASSERT(ctx);
        return 0;
    }

    int FireBeforeQueryValue(CBRegistryBeforeQueryValueEventParams* e)
    {
        PCBREG_USER_CONTEXT ctx = (PCBREG_USER_CONTEXT)e->KeyContext;
        ASSERT(ctx);
        return 0;
    }

    int FireBeforeRenameKey(CBRegistryBeforeRenameKeyEventParams* e)
    {
        PCBREG_USER_CONTEXT ctx = (PCBREG_USER_CONTEXT)e->KeyContext;
        ASSERT(ctx);
        AddToLog(this, _T("RegPreRenameKey"), ctx->FullPath, ctx);
        return 0;
    }

    int FireBeforeSetKey(CBRegistryBeforeSetKeyEventParams* e)
    {
        PCBREG_USER_CONTEXT ctx = (PCBREG_USER_CONTEXT)e->KeyContext;
        ASSERT(ctx);
        AddToLog(this, _T("RegPreSetKey"), ctx->FullPath, ctx);
        return 0;
    }

    int FireBeforeSetValue(CBRegistryBeforeSetValueEventParams* e)
    {
        PCBREG_USER_CONTEXT ctx = (PCBREG_USER_CONTEXT)e->KeyContext;
        ASSERT(ctx);
        AddToLog(this, _T("RegPreSetValue"), ctx->FullPath, ctx);
        return 0;
    }

};

RegFilter g_CBReg;

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

void SetFilter(LPCTSTR KeyToMonitor);
void DeleteFilter();

using namespace std;

void Banner(void)
{
    printf("CBFS Filter Registry Monitor Demo Copyright (c) 2017-2023, Callback Technologies, Inc.\n\n");
}

void Usage(void)
{
    printf("Usage: regmon [-<switch 1> ... -<switch N>] [<registry key to monitor>]\n\n");
    printf("<Switches>\n");
    printf("  -drv <path to cbregistry.cab> - Install the driver from the specified CAB file\n");
    printf("\n");
    printf("Example: regmon \"HKEY_CURRENT_USER\\SOFTWARE\\Callback Technologies\"\n");
}

// ----------------------------------------------------------------------------------

int CheckResult(int result)
{
    if (result != 0)
    {
        fprintf(stderr, strMonitorError.c_str(), g_CBReg.GetLastErrorCode(), g_CBReg.GetLastError());
    }
    return result;
}


void CheckDriver()
{
    int state, number;

    state = g_CBReg.GetDriverStatus(product_id.c_str());
    if (CheckResult(g_CBReg.GetLastErrorCode()))
        exit(0);
    if (state == cbfConstants::MODULE_STATUS_RUNNING)
    {
        int64 version;
        version = g_CBReg.GetDriverVersion(product_id.c_str());
        if (CheckResult(g_CBReg.GetLastErrorCode()))
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
    return res;
}

int wmain(int argc, wchar_t* argv[])
{
    INT drv_reboot = 0;
    INT opt_terminate = 0;

    LPTSTR KeyToMonitor = NULL;

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
                    arg = ConvertRelativePathToAbsolute(argv[++argi]);
                    if(arg.empty()) {
                        printf("Error: Invalid Driver Path\n");
                        exit(1); 
                    } 
                    if (argi < argc)
                    {
                        _tprintf(_T("Installing the driver from '%s'\n"), arg.c_str());
                        drv_reboot = g_CBReg.Install(arg.c_str(), product_id.c_str(), NULL, 0, NULL);// cbfConstants::INSTALL_REMOVE_OLD_VERSIONS);

                        int errcode = g_CBReg.GetLastErrorCode();
                        if (errcode != 0)
                        {
                            if (errcode == ERROR_PRIVILEGE_NOT_HELD)
                                printf("The driver could not be installed, please try to install with administrator rights\n");
                            else
                                printf("The driver could not be installed, the error is %d (%s)\n", errcode, g_CBReg.GetLastError());

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
                KeyToMonitor = _tcsdup(arg.c_str());
                break;
            }
        }
    }

    CheckDriver();

    // Probably, we have just installed the driver, so we can quit without monitoring anything
    if (KeyToMonitor == NULL)
        exit(0);

    _tprintf(_T("Press any key to start monitoring, then press any key to stop and quit.\n"));

    _getch();

    InitializeCriticalSection(&g_LogListLock);
    try {

        SetFilter(KeyToMonitor);

        _getch();
        DeleteFilter();
    }
    catch (int e)
    {
    }

    DeleteCriticalSection(&g_LogListLock);
    if (KeyToMonitor)
        free(KeyToMonitor);

    return 0;
}

void AddToLog(CBRegistry* Filter, LPCTSTR Operation, LPTSTR KeyName, PVOID UserContext, DWORD Result, LPTSTR Details)
{
    EnterCriticalSection(&g_LogListLock);
    _tprintf(_T("%s - %s (operation status = %d)\n"), Operation, KeyName, Result);
    LeaveCriticalSection(&g_LogListLock);
}

void SetFilter(LPCTSTR KeyToMonitor)
{
    // Compose the filtering rule by concatenating the path and the mask
    int l = _tcslen(KeyToMonitor);
    int path_len = l;

    if (l == 0)
    {
        printf("Nothing to monitor, quitting");
        exit(3);
    }

    l += 3; // for '*', the separator and the trailing \0

    LPTSTR mask_buf = (LPTSTR)malloc(l * sizeof(TCHAR));
    _tcscpy(mask_buf, KeyToMonitor);
    if (KeyToMonitor[path_len - 1] != '\\')
        _tcscat(mask_buf, _T("\\"));
    _tcscat(mask_buf, _T("*"));

    // Add a filtering rule
    g_CBReg.AddFilterRule(mask_buf,
        cbfConstants::ACCESS_NONE,
        cbfConstants::REG_CE_BEFORE_CREATE_KEY | // BeforeCreate and BeforeOpen are mandatory here because 
        cbfConstants::REG_CE_BEFORE_OPEN_KEY |   // they allocate the context, which is used in other handlers    
        cbfConstants::REG_CE_AFTER_CLOSE_KEY |
        cbfConstants::REG_CE_AFTER_CREATE_KEY |
        cbfConstants::REG_CE_AFTER_DELETE_KEY |
        cbfConstants::REG_CE_AFTER_DELETE_VALUE |
        cbfConstants::REG_CE_AFTER_ENUM_KEY |
        cbfConstants::REG_CE_AFTER_ENUM_VALUE |
        cbfConstants::REG_CE_AFTER_OPEN_KEY |
        cbfConstants::REG_CE_AFTER_QUERY_KEY |
        cbfConstants::REG_CE_AFTER_QUERY_VALUE |
        cbfConstants::REG_CE_AFTER_RENAME_KEY |
        cbfConstants::REG_CE_AFTER_SET_KEY |
        cbfConstants::REG_CE_AFTER_SET_VALUE);

    // Initialize and start the filter
    int retVal = g_CBReg.Initialize(g_Guid);
    if (0 == retVal)
        retVal = g_CBReg.StartFilter(30000);

    if (retVal != 0)
    {
        _tprintf(_T("Error %d while trying to start filtering\n"), retVal);
        exit(retVal);

    }
    _tprintf(_T("Started the monitor with the rule mask '%s'\n"), mask_buf);
}

void DeleteFilter()
{
    g_CBReg.StopFilter();
}

