/*
 * CBFS Filter 2024 .NET Edition - Sample Project
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
 * 
 */

﻿using System;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Runtime.InteropServices;
using System.Text.RegularExpressions;
using callback.CBFSFilter;

namespace callback.Demos
{
    public enum DriverStatus
    {
        NotInstalled,
        Stopped,
        StartPending,
        StopPending,
        Running,
        ContinuePending,
        PausePending,
        Paused
    };

    public enum AccessMask : int
    {
        StandardRightsRequired = 0x000f0000,
        Synchronize = 0x00100000,

        ProcessTerminate = 0x0001,
        ProcessCreateThread = 0x0002,
        ProcessSetSessionId = 0x0004,
        ProcessVmOperation = 0x0008,
        ProcessVmRead = 0x0010,
        ProcessVmWrite = 0x0020,
        ProcessDupHandle = 0x0040,
        ProcessCreateProcess = 0x0080,
        ProcessSetQuota = 0x0100,
        ProcessSetInformation = 0x0200,
        ProcessQueryInformation = 0x0400,
        ProcessSuspendResume = 0x0800,
        ProcessQueryLimitedInformation = 0x1000,

        ProcessAllAccess = StandardRightsRequired | Synchronize | 0xffff,

        ThreadTerminate = 0x0001,
        ThreadSuspendResume = 0x0002,
        ThreadAlert = 0x0004,
        ThreadGetContext = 0x0008,
        ThreadSetContext = 0x0010,
        ThreadSetInformation = 0x0020,
        ThreadSetLimitedInformation = 0x0400,
        ThreadQueryLimitedInformation = 0x0800,

        ThreadAllAccess = StandardRightsRequired | Synchronize | 0xffff
    };

    class procmonDemo : CBMonitor
    {
        [DllImport("advapi32.dll", EntryPoint = "InitiateSystemShutdown")]
        private static extern int InitiateSystemShutdown(string lpMachineName, string lpMessage, int dwTimeout, int bForceAppsClosed, int bRebootAfterShutdown);

        [DllImport("kernel32.dll", SetLastError = true)]
        static extern bool CloseHandle(IntPtr hHandle);

        private const string mProductGuid = "{713CC6CE-B3E2-4fd9-838D-E28F558F6866}";

        private const int ERROR_ACCESS_DENIED = 5;
        private const int ERROR_PRIVILEGE_NOT_HELD = 1314;

        private string mCabFileLocation = null;

        private string noExecOpName = null;
        private bool denyExecute = false;

        private string noTermOpName = null;
        private int noTermOpPID = 0;
        private bool denyTerminate = false;

        // CBProcess filter instance.
        private static CBProcess mFilter = null;

        // Process id for the process to be tested.
        private static UInt32 mProcessId;
        private static ulong mStartTime;

        public procmonDemo()
        {
            mFilter = CreateFilter();
            CheckDriver(mFilter);
            DisposeFilter(ref mFilter);
        }

        private static void AskForReboot()
        {
            Console.WriteLine("System restart is needed in order to install the drivers. Reboot now? (Input 'Yes' to reboot or anything else to decline)");
            string inputValue = Console.ReadLine();
            if (!String.IsNullOrEmpty(inputValue) && inputValue.Equals("Yes"))
                InitiateSystemShutdown("", "", 10000, 0, 1);
        }

        private void Install(string fileName)
        {
            bool rebootNeeded;

            using (var filter = CreateFilter())
            {
                try
                {
                    Console.WriteLine($"Installing the driver from '{fileName}'");
                    rebootNeeded = filter.Install(fileName, mProductGuid, null, 0, null);
                    CheckDriver(filter);
                    Console.WriteLine("Drivers installed successfully");
                    if (rebootNeeded)
                    {
                        Console.Write(", reboot is required\n");
                        AskForReboot();
                        return;
                    }
                }
                catch (CBFSFilterException err)
                {
                    string message;
                    Console.WriteLine("The driver could not be installed");
                    if (err.Code == ERROR_ACCESS_DENIED || err.Code == ERROR_PRIVILEGE_NOT_HELD)
                        message = "Installation requires administrator rights. Run the app as administrator";
                    else
                        message = err.Message;
                    Console.WriteLine("Error: " + message);
                    return;
                }
            }
        }

        private void Uninstall(string fileName)
        {
            bool rebootNeeded;

            using (var filter = CreateFilter())
            {
                try
                {
                    rebootNeeded = filter.Uninstall(fileName, mProductGuid, null, Constants.UNINSTALL_VERSION_CURRENT);
                    CheckDriver(filter);
                    Console.WriteLine("Drivers uninstalled successfully");
                    if (rebootNeeded)
                    {
                        Console.Write(", reboot is required\n");
                        AskForReboot();
                        return;
                    }
                }
                catch (CBFSFilterException err)
                {
                    string message;
                    Console.WriteLine("The driver could not be uninstalled");
                    if (err.Code == ERROR_ACCESS_DENIED || err.Code == ERROR_PRIVILEGE_NOT_HELD)
                        message = "Uninstallation requires administrator rights. Run the app as administrator";
                    else
                        message = err.Message;
                    Console.WriteLine("Error: " + message);
                    return;
                }
            }
        }

        private CBProcess CreateFilter()
        {
            var filter = new CBProcess();
            filter.OnProcessCreation += ProcessCreationEventHandler;
            filter.OnProcessHandleOperation += ProcessHandleOperationEventHandler;
            filter.OnProcessTermination += ProcessTerminationEventHandler;
            filter.OnThreadCreation += ThreadCreationEventHandler;
            filter.OnThreadHandleOperation += ThreadHandleOperationEventHandler;
            filter.OnThreadTermination += ThreadTerminationEventHandler;
            return filter;
        }

        private void CheckDriver(CBProcess filter = null, bool writeInfo = true)
        {
            string message;
            try
            {
                if (filter == null)
                {
                    filter = CreateFilter();
                }
                var status = (DriverStatus)filter.GetDriverStatus(mProductGuid);

                if (status == DriverStatus.NotInstalled)
                    message = status.ToString();
                else
                {
                    var version = filter.GetDriverVersion(mProductGuid);
                    var major = version >> 48;
                    var minor = (version >> 32) & 0xffff;
                    var release = (version >> 16) & 0xffff;
                    var build = version & 0xffff;
                    message = string.Format("Driver (ver {0}.{1}.{2}.{3}) installed, service {4}",
                                  major, minor, release, build, status);
                }
                if (writeInfo)
                {
                    Console.WriteLine(message);
                }
            }
            catch (CBFSFilterException err)
            {
                message = "Error - " + err.Message;
                Console.WriteLine(message);
            }
        }

        private static ulong GetTickCount()
        {
            return (ulong)(DateTime.Now.Ticks / TimeSpan.TicksPerMillisecond);
        }

        private void SetFilter()
        {
            mFilter = CreateFilter();
            try
            {
                mFilter.Initialize(mProductGuid);

                // Make the demo can receive notifications triggered by itself.
                mFilter.Config("FilterOwnRequests=true");
                mFilter.Config("EventsToFire=-1");
                mStartTime = GetTickCount();
                mFilter.StartFilter(30000);
                Log("Filter started");
            }
            catch (CBFSFilterException err)
            {
                Console.WriteLine("Error: " + err.Message);
                return;
            }

            try
            {
                mFilter.AddFilteredProcessById(-1, true);
            }
            catch (CBFSFilterException err)
            {
                Console.WriteLine("Error while trying to add the filtering rule: " + err.Message);
                return;
            }
        }

        private void StopFilter()
        {
            DisposeFilter(ref mFilter);
            Log("Filter stopped");
        }
        private static void DisposeFilter(ref CBProcess filter)
        {
            if (filter == null)
                return;

            filter.StopFilter();
            filter.Dispose();
            filter = null;
        }


        static void Banner()
        {
            Console.WriteLine("CBFS Filter Process Monitor Demo Copyright (c) 2017-2023, Callback Technologies, Inc.\n");
        }

        static void Usage()
        {
            Console.WriteLine("Usage: procmon [-<switch 1> ... -<switch N>]\n");
            Console.WriteLine("<Switches>");
            Console.WriteLine("  -mon - monitor all processes in the system");
            Console.WriteLine("  -drv <path to cbprocess.cab> - Install the driver from the specified CAB file");
            Console.WriteLine("  -noexec <EXE name> - Prevent execution of the process from the given EXE name (may contain a path)");
            Console.WriteLine("  -noterm {<EXE name>|<PID>} - Prevent termination of the process with the given EXE name (may contain a path) or process ID");
            Console.WriteLine("IMPORTANT: termination protection doesn't prevent closing of process windows and graceful exit. It just removes the permission from the handle\n");
        }

        private bool PreventTerminate(int PID)
        {
            if (denyTerminate)
            {
                if ((noTermOpPID != 0) && (PID == noTermOpPID))
                    return true;
                string ProcName = Process.GetProcessById(PID).ProcessName;
                if (ProcName == null || ProcName[0] == 0)
                    return false;
                return String.Equals(ProcName, noTermOpName, StringComparison.Ordinal);
            }
            else
                return false;
        }

        private void ProcessCreationEventHandler(object sender, CBProcessProcessCreationEventArgs e)
        {
            if (denyExecute &&
                (noExecOpName[0] != 0) &&
                String.Equals(e.ProcessName, noExecOpName, StringComparison.Ordinal))
                e.ResultCode = ERROR_ACCESS_DENIED;

            Log(((e.ResultCode == ERROR_ACCESS_DENIED) ? $"Process creation denied." : "Process created.") + $" Id: {e.ProcessId}, Name: {e.ProcessName}");
            return;
        }

        private void ProcessHandleOperationEventHandler(object sender, CBProcessProcessHandleOperationEventArgs e)
        {
            if (PreventTerminate(e.ProcessId))
            {
                e.DesiredAccess &= ~(int)AccessMask.ProcessTerminate;

                Log("Process handle operation; removed PROCESS_TERMINATED flag." + $" Id: {e.ProcessId}, Name: {Process.GetProcessById(e.ProcessId).ProcessName}");
            }
            return;
        }

        private void ProcessTerminationEventHandler(object sender, CBProcessProcessTerminationEventArgs e)
        {
            Log(String.Format("Process {0} terminated", e.ProcessId));

            // The managed process has been terminated. So we can stop the filter 
            // but in order to avoid possible deadlock do it asynchronously.
            StopFilter();
        }

        private void ThreadCreationEventHandler(object sender, CBProcessThreadCreationEventArgs e)
        {
            Log(String.Format("Thread {0} created", e.ThreadId));
        }

        private void ThreadHandleOperationEventHandler(object sender, CBProcessThreadHandleOperationEventArgs e)
        {
            Log(String.Format("Thread {0} is being opened with rights 0x{1:X}", e.ThreadId, e.DesiredAccess));

            if (PreventTerminate(e.ProcessId))
            {
                Log("Thread handle operation; removed PROCESS_TERMINATED flag." + $" Id: {e.ProcessId}, Name: {Process.GetProcessById(e.ProcessId).ProcessName}");
                e.DesiredAccess &= ~(int)AccessMask.ThreadTerminate;
            }
            return;
        }

        private void ThreadTerminationEventHandler(object sender, CBProcessThreadTerminationEventArgs e)
        {
            Log(String.Format("Thread {0} terminated", e.ThreadId));
        }

        private static void Log(string message)
        {
            ulong time = GetTickCount() - mStartTime;

            Console.WriteLine($"<LogInfo> Message: {message}. Time: {time}.");
        }

        private static string ConvertRelativePathToAbsolute(string path)
        {
            string res = null;
            if (!string.IsNullOrEmpty(path))
            {
                res = path;

                // Linux-specific case of using a home directory
                if (path.Equals("~") || path.StartsWith("~/"))
                {
                    string homeDir = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile);
                    if (path.Equals("~") || path.Equals("~/"))
                        return homeDir;
                    else
                        return Path.Combine(homeDir, path.Substring(2));
                }
                else if (!IsDriveLetter(path))
                {
                    try {
                        res = Path.GetFullPath(path);

                        if (res.StartsWith("\\\\") && !Directory.Exists(res))
                        {
                            Console.WriteLine("The network folder '{0}' does not exist.", res);
                        }
                        else if (!File.Exists(res) && !Directory.Exists(res))
                        {
                            Console.WriteLine("The path '{0}' does not exist.", res);
                        }
                    } catch (Exception ex) {
                        Console.WriteLine($"ConvertRelativePathToAbsolute error: {ex.Message}");
                        return null;
                    }
                }
            }
            return res;
        }

        private static bool IsDriveLetter(string path)
        {
            if (string.IsNullOrEmpty(path))
                return false;

            char c = path[0];
            if ((char.IsLetter(c) && path.Length == 2 && path[1] == ':'))
                return true;
            else
                return false;
        }

        static void Main(string[] args)
        {
            bool drvReboot = false;
            bool optTerminate = false;

            string KeyToMonitor = null;

            int resCode = 0, argi = 0, argLen = 0;
            bool stop_opt = false;

            Banner();

            try
            {
                procmonDemo procMonitor = new procmonDemo();
                // if no parameters specified, show usage and quit
                if (args.Length < 1)
                {
                    Usage();
                    return;
                }

                for (argi = 0; argi < args.Length; argi++)
                {
                    string arg = args[argi];
                    argLen = arg.Length;
                    if (argLen > 0)
                    {
                        if ((arg[0] == '-') && !stop_opt)
                        {
                            if (String.Equals(arg, "-drv", StringComparison.OrdinalIgnoreCase))
                            {
                                if (argi < args.Length)
                                {
                                    arg = ConvertRelativePathToAbsolute(args[++argi]);
                                    if (String.IsNullOrEmpty(arg)){
                                        Console.WriteLine("Invalid Driver Path");
                                        return;
                                    }
                                    procMonitor.Install(arg);
                                }
                            }
                            else
                            if (String.Equals(arg, "-noexec", StringComparison.OrdinalIgnoreCase))
                            {
                                if (argi < args.Length)
                                {
                                    arg = args[++argi];
                                    procMonitor.noExecOpName = arg.ToString();
                                    procMonitor.denyExecute = true;
                                }
                                else
                                {
                                    Console.WriteLine("Error: -noexec parameter requires a file name as the next argument");
                                    return;
                                }
                            }
                            else
                            if (String.Equals(arg, "-noterm", StringComparison.OrdinalIgnoreCase))
                            {
                                if (argi < args.Length)
                                {
                                    arg = args[++argi];
                                    procMonitor.noTermOpName = arg.ToString();
                                    if (int.TryParse(procMonitor.noTermOpName, out int value))
                                        procMonitor.noTermOpPID = value;
                                    procMonitor.denyTerminate = true;
                                }
                                else
                                {
                                    Console.WriteLine("Error: -noterm parameter requires a file name or a process ID as the next argument");
                                    return;
                                }
                            }
                            else
                            if (String.Equals(arg, "-mon", StringComparison.OrdinalIgnoreCase))
                            { // nothing here, execution will pass through
                            }
                            else
                                Console.WriteLine("Error: Invalid option " + arg);
                        }
                        else
                        {
                            // if we have not set the path yet, do this now. Otherwise, set the mask
                            KeyToMonitor = arg.ToString();
                            break;
                        }
                    }
                }

                Console.WriteLine("Press any key to start monitoring, then press any key to stop and quit.");

                Console.ReadKey();

                try
                {

                    procMonitor.SetFilter();
                    Console.WriteLine("Press any key to stop monitoring and quit.");
                    Console.ReadKey();
                    procMonitor.StopFilter();
                }
                catch (Exception ex)
                {
                    Console.WriteLine("Error: " + ex.Message);
                }

                return;
            }
            catch (Exception ex)
            {
                Console.WriteLine("Error in Main: " + ex.Message);
            }
        }
    }
}





class ConsoleDemo
{
  /// <summary>
  /// Takes a list of switch arguments or name-value arguments and turns it into a dictionary.
  /// </summary>
  public static System.Collections.Generic.Dictionary<string, string> ParseArgs(string[] args)
  {
    System.Collections.Generic.Dictionary<string, string> dict = new System.Collections.Generic.Dictionary<string, string>();

    for (int i = 0; i < args.Length; i++)
    {
      // Add a key to the dictionary for each argument.
      if (args[i].StartsWith("/"))
      {
        // If the next argument does NOT start with a "/", then it is a value.
        if (i + 1 < args.Length && !args[i + 1].StartsWith("/"))
        {
          // Save the value and skip the next entry in the list of arguments.
          dict.Add(args[i].ToLower().TrimStart('/'), args[i + 1]);
          i++;
        }
        else
        {
          // If the next argument starts with a "/", then we assume the current one is a switch.
          dict.Add(args[i].ToLower().TrimStart('/'), "");
        }
      }
      else
      {
        // If the argument does not start with a "/", store the argument based on the index.
        dict.Add(i.ToString(), args[i].ToLower());
      }
    }
    return dict;
  }
  /// <summary>
  /// Asks for user input interactively and returns the string response.
  /// </summary>
  public static string Prompt(string prompt, string defaultVal)
  {
    Console.Write(prompt + (defaultVal.Length > 0 ? " [" + defaultVal + "]": "") + ": ");
    string val = Console.ReadLine();
    if (val.Length == 0) val = defaultVal;
    return val;
  }
}