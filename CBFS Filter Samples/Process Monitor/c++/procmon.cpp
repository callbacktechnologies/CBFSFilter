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

BOOL g_DenyExecute = FALSE;
BOOL g_DenyTerminate = FALSE;
TCHAR g_NoExecOpName[MAX_PATH + 1] = { 0, };
TCHAR g_NoTermOpName[MAX_PATH + 1] = { 0, };
INT g_NoTermOpPID = 0;
BOOL gNameConainsPath = FALSE;

//
//  These macros are used to test, set and clear flags respectively
//

#ifndef FlagOn
#define FlagOn(_F,_SF)        ((_F) & (_SF))
#endif

#ifndef BooleanFlagOn
#define BooleanFlagOn(F,SF)   ((BOOL)(((F) & (SF)) != 0))
#endif

#ifndef SetFlag
#define SetFlag(_F,_SF)       ((_F) |= (_SF))
#endif

#ifndef ClearFlag
#define ClearFlag(_F,_SF)     ((_F) &= ~(_SF))
#endif

void AddToLog(LPCTSTR Operation, LPCTSTR ProcessName, DWORD ProcessID);

class ProcFilter : public CBProcess
{
public:
    ProcFilter() : CBProcess()
    {
    }

    BOOL NameMatches(LPCTSTR ProcessName, LPTSTR RuleName, BOOL CompareJustNames)
    {
        TCHAR* NameStart;

        if (gNameConainsPath)
        {
            // if the full path was specified in command-line parameters, we simply compare paths
            if (!CompareJustNames)
                return _tcsicmp(ProcessName, RuleName) == 0;

            // Compare only names, so we need to find the beginning of the name in the condition    
            NameStart = (TCHAR*)_tcsrchr(ProcessName, '\\');

            // If the separator was not found, take the whole name, 
            // otherwise increment the pointer to point at the actual name
            if ((NameStart == NULL))
                NameStart = (TCHAR*)ProcessName;
            else
            {
                NameStart++;
                if (*NameStart == 0)
                    return FALSE;
            }

            TCHAR* NameStart2 = _tcsrchr(RuleName, '\\');
            if ((NameStart2 == NULL))
                NameStart2 = RuleName;
            else
            {
                NameStart2++;
                if (*NameStart2 == 0)
                    return FALSE;
            }
            return _tcsicmp(NameStart, NameStart2) == 0;
        }

        // find the beginning of the name in the actual process name
        NameStart = (TCHAR*)_tcsrchr(ProcessName, '\\');
        if ((NameStart == NULL))
            NameStart = (TCHAR*)ProcessName;
        else
        {
            NameStart++;
            if (*NameStart == 0)
                return FALSE;
        }
        return _tcsicmp(NameStart, RuleName) == 0;
    }

    BOOL PreventTerminate(DWORD PID)
    {
        if (g_DenyTerminate)
        {
            if ((g_NoTermOpPID != 0) && (PID == g_NoTermOpPID))
                return TRUE;
            LPTSTR ProcName = GetProcessName(PID);
            if (ProcName == NULL || ProcName[0] == 0)
                return FALSE;
            return NameMatches(ProcName, g_NoTermOpName, FALSE);
        }
        else
            return FALSE;
    }

    int FireProcessCreation(CBProcessProcessCreationEventParams* e)
    {
        // Possibly, prevent creation
        if (g_DenyExecute &&
            (g_NoExecOpName[0] != 0) &&
            NameMatches(e->ProcessName, g_NoExecOpName, FALSE /*e->FileOpenNameAvailable*/))
            e->ResultCode = ERROR_ACCESS_DENIED;

        AddToLog((e->ResultCode == ERROR_ACCESS_DENIED) ? _T("process creation denied") : _T("process created"),
            (LPTSTR)e->ProcessName,
            e->ProcessId);

        return 0;
    }

    int FireProcessHandleOperation(CBProcessProcessHandleOperationEventParams* e)
    {
        if (PreventTerminate(e->ProcessId))
        {
            ClearFlag(e->DesiredAccess, PROCESS_TERMINATE);

            AddToLog(_T("process handle operation; removed PROCESS_TERMINATED flag."),
                GetProcessName(e->ProcessId),
                e->ProcessId);
        }
        return 0;
    }

    // MdProcessTerminationEvent
    int FireProcessTermination(CBProcessProcessTerminationEventParams* e)
    {
        // We cannot prevent termination here, just log the event
        AddToLog(_T("process termination."),
            e->ProcessName,
            e->ProcessId);

        return 0;
    }

    // MbThreadHandleOperationEvent
    int FireThreadHandleOperation(CBProcessThreadHandleOperationEventParams* e)
    {
        if (PreventTerminate(e->ProcessId))
        {
            AddToLog(_T("thread handle operation; removed PROCESS_TERMINATED flag."),
                GetProcessName(e->ProcessId),
                e->ProcessId);
            ClearFlag(e->DesiredAccess, THREAD_TERMINATE);
        }
        return 0;
    }

};

//
// Filter instance.
//
ProcFilter g_CBProcess;

CRITICAL_SECTION    g_LogListLock = { 0, };

typedef __int64 int64;

const cbt_string product_id(TEXT("{713CC6CE-B3E2-4fd9-838D-E28F558F6866}"));
const cbt_string strInvalidOption(TEXT("Invalid option \"%s\"\n"));

const std::string strMonitorError("Error %d (%s)\n");

void SetFilter();
void DeleteFilter();

using namespace std;

void Banner(void)
{
    printf("CBFS Filter Process Monitor Demo Copyright (c) 2017-2023, Callback Technologies, Inc.\n\n");
}

void Usage(void)
{
    printf("Usage: procmon [-<switch 1> ... -<switch N>]\n\n");
    printf("<Switches>\n");
    printf("  -mon - monitor all processes in the system\n");
    printf("  -drv <path to cbprocess.cab> - Install the driver from the specified CAB file\n");
    printf("  -noexec <EXE name> - Prevent execution of the process from the given EXE name (may contain a path)\n");
    printf("  -noterm {<EXE name>|<PID>} - Prevent termination of the process with the given EXE name (may contain a path) or process ID\n");
    printf("IMPORTANT: termination protection doesn't prevent closing of process windows and graceful exit. It just removes the permission from the handle\n");
    printf("\n");
}

// ----------------------------------------------------------------------------------

int CheckResult(int result)
{
    if (result != 0)
    {
        fprintf(stderr, strMonitorError.c_str(), g_CBProcess.GetLastErrorCode(), g_CBProcess.GetLastError());
    }
    return result;
}


void CheckDriver()
{
    int state, number;

    state = g_CBProcess.GetDriverStatus(product_id.c_str());
    if (CheckResult(g_CBProcess.GetLastErrorCode()))
        exit(0);
    if (state == cbfConstants::MODULE_STATUS_RUNNING)
    {
        int64 version;
        version = g_CBProcess.GetDriverVersion(product_id.c_str());
        if (CheckResult(g_CBProcess.GetLastErrorCode()))
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
                    if (argi < argc)
                    {
                        arg = ConvertRelativePathToAbsolute(argv[++argi]);
                        if (arg.empty()){
                            printf("Error: Invalid Driver Path\n");
                            exit(1);
                        }
                        _tprintf(_T("Installing the driver from '%s'\n"), arg.c_str());
                        drv_reboot = g_CBProcess.Install(arg.c_str(), product_id.c_str(), NULL, 0, NULL);// cbfConstants::INSTALL_REMOVE_OLD_VERSIONS);

                        int errcode = g_CBProcess.GetLastErrorCode();
                        if (errcode != 0)
                        {
                            if (errcode == ERROR_PRIVILEGE_NOT_HELD)
                                printf("The driver could not be installed, please try to install with administrator rights\n");
                            else
                                printf("The driver could not be installed, the error is %d (%s)\n", errcode, g_CBProcess.GetLastError());

                            exit(errcode);
                        }
                        printf("Drivers installed successfully\n");
                        if (drv_reboot != 0)
                        {
                            printf(", reboot is required\n");
                            exit(1);
                        }
                        else
                            exit(0);
                    }
                }
                else
                    if (optcmp(arg, "-noexec"))
                    {
                        if (argi < argc)
                        {
                            arg = argv[++argi];
                            _tcscpy(g_NoExecOpName, arg.c_str());
                            g_DenyExecute = TRUE;
                        }
                        else
                        {
                            fprintf(stderr, "-noexec parameter requires a file name as the next argument");
                            exit(1);
                        }
                    }
                    else
                        if (optcmp(arg, "-noterm"))
                        {
                            if (argi < argc)
                            {
                                arg = argv[++argi];
                                _tcscpy(g_NoTermOpName, arg.c_str());
                                g_NoTermOpPID = _ttoi(g_NoTermOpName);
                                g_DenyTerminate = TRUE;
                            }
                            else
                            {
                                fprintf(stderr, "-noterm parameter requires a file name or a process ID as the next argument");
                                exit(1);
                            }
                        }
                        else
                            if (optcmp(arg, "-mon"))
                            { // nothing here, execution will pass through
                            }
                            else
                                fwprintf(stderr, strInvalidOption.c_str(), arg.c_str());
            }
            else
            {
                // if we have not set the path yet, do this now. Otherwise, set the mask
                KeyToMonitor = _tcsdup(arg.c_str());
                break;
            }
        }
    }

    CheckDriver();

    _tprintf(_T("Press any key to start monitoring, then press any key to stop and quit.\n"));

    _getch();

    InitializeCriticalSection(&g_LogListLock);
    try {

        SetFilter();

        _getch();
        DeleteFilter();
    }
    catch (int e)
    {
    }

    DeleteCriticalSection(&g_LogListLock);

    return 0;
}

void AddToLog(LPCTSTR Operation, LPCTSTR ProcessName, DWORD ProcessID)
{
    EnterCriticalSection(&g_LogListLock);
    _tprintf(_T("Process #%d (%s): %s\n"), ProcessID, ProcessName, Operation);
    LeaveCriticalSection(&g_LogListLock);
}

void SetFilter()
{
    // Initialize and start the filter
    int retVal = g_CBProcess.Initialize(g_Guid);
    if (0 == retVal)
    {
        g_CBProcess.Config(_T("EventsToFire=-1"));
        // Start monitoring   
        retVal = g_CBProcess.StartFilter(30000);
    }

    if (retVal != 0)
    {
        _tprintf(_T("Error %d while trying to start monitoring\n"), retVal);
        exit(retVal);
    }

    if (0 == retVal)
    {
        // Monitor all processes
        // Filtration rules MUST be added only after a successful call to StartFilter
        retVal = g_CBProcess.AddFilteredProcessById(-1, TRUE);
    }

    if (retVal != 0)
    {
        _tprintf(_T("Error %d while trying to add the filtering rule\n"), retVal);
        exit(retVal);
    }


    _tprintf(_T("Started the monitor\n"));
}

void DeleteFilter()
{
    g_CBProcess.StopFilter();
}

