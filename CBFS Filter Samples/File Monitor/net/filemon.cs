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
using System.IO;
using System.Runtime.InteropServices;
using System.Text.RegularExpressions;
using callback.CBFSFilter;

namespace callback.Demos
{
    class filemonDemo
    {
        [DllImport("advapi32.dll", EntryPoint = "InitiateSystemShutdown")]
        private static extern int InitiateSystemShutdown(string lpMachineName, string lpMessage, int dwTimeout, int bForceAppsClosed, int bRebootAfterShutdown);

        [DllImport("kernel32.dll", SetLastError = true)]
        static extern bool CloseHandle(IntPtr hHandle);

        private static CBMonitor mWatcher = new CBMonitor();
        private const string mGuid = "{713CC6CE-B3E2-4fd9-838D-E28F558F6866}";
        private const uint ERROR_ACCESS_DENIED = 5;
        private const uint ERROR_PRIVILEGE_NOT_HELD = 1314;
        private const string ALTITUDE_FAKE_VALUE_FOR_DEBUG = "360000.24";

        static void Banner()
        {
            Console.WriteLine("CBFS Filter FileMon Demo Copyright (c) 2017-2023, Callback Technologies, Inc.\n\n");
        }

        static void Usage()
        {
            Console.WriteLine("Usage: filemon [-<switch 1> ... -<switch N>] [<path to monitor> [<filename mask to monitor>]]\n");
            Console.WriteLine("<Switches>");
            Console.WriteLine("  -drv <path to cbfilter.cab> - Install the driver from the specified CAB file");
            Console.WriteLine("  -- Stop switches scanning\n");
        }

        private static void Install(string fileName)
        {
            bool Reboot = false;
            try
            {
                Console.WriteLine($"Installing the driver from '{fileName}'");
                Reboot = mWatcher.Install(fileName, mGuid, null, ALTITUDE_FAKE_VALUE_FOR_DEBUG, 0, null);//INSTALL_REMOVE_OLD_VERSIONS);

                Console.WriteLine("Drivers installed successfully");
                if (Reboot)
                {
                    Console.Write(", reboot is required\n");
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
            }
        }

        private static void Uninstall(string fileName)
        {
            bool Reboot = false;
            try
            {
                Reboot = mWatcher.Uninstall(fileName, mGuid, null, 0);

                CheckDriver();

                if (Reboot)
                {
                    Console.WriteLine("Please, reboot the system for the changes to take effect");
                }
                else
                {
                    Console.WriteLine("Driver uninstalled successfully.");
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
            }
        }

        private static void SetFilter(string mask)
        {
            mWatcher.Initialize(mGuid);

            mWatcher.AddFilterRule(mask,
                Constants.FS_NE_READ |
                Constants.FS_NE_WRITE |
                Constants.FS_NE_CREATE |
                Constants.FS_NE_RENAME |
                Constants.FS_NE_CREATE_HARD_LINK |
                Constants.FS_NE_SET_SIZES |
                Constants.FS_NE_DELETE |
                Constants.FS_NE_SET_ATTRIBUTES |
                Constants.FS_NE_ENUMERATE_DIRECTORY |
                Constants.FS_NE_OPEN |
                Constants.FS_NE_CLOSE |
                Constants.FS_NE_SET_SECURITY
            );
            mWatcher.ProcessFailedRequests = true;

            mWatcher.StartFilter();
        }

        private static void DeleteFilter()
        {
            mWatcher.DeleteAllFilterRules();
            if (mWatcher.Active)
                mWatcher.StopFilter();
        }

        private static bool CheckDriver(bool writeInfo = true)
        {
            uint versionHigh, versionLow;
            int moduleStatus;
            ulong moduleVersion;

            moduleStatus = mWatcher.GetDriverStatus(mGuid);

            moduleVersion = (ulong)mWatcher.GetDriverVersion(mGuid);

            versionHigh = (uint)(moduleVersion >> 32);
            versionLow = (uint)(moduleVersion & 0xFFFFFFFF);

            string driverInfo;
            bool res = moduleStatus != 0;

            if (moduleStatus != 0)
            {
                driverInfo = string.Format("Driver version: {0}.{1}.{2}.{3}",
                  versionHigh >> 16, versionHigh & 0xFFFF, versionLow >> 16, versionLow & 0xFFFF);
            }
            else
            {
                driverInfo = "Driver: not installed";
            }

            if (writeInfo)
            {
                Console.WriteLine(driverInfo);
            }
            return res;
        }

        private void AddToLog(CBMonitor Watcher, string Operation, string FileName, Int32 Status)
        {
            string ProcessName;
            UInt32 ProcessID;

            try
            {
                ProcessName = Watcher.GetOriginatorProcessName();
                if (ProcessName.Length == 0) ProcessName = "System";
            }
            catch (Exception)
            {
                ProcessName = "< ERROR >";
            }

            try
            {
                ProcessID = (UInt32)Watcher.GetOriginatorProcessId();
            }
            catch (Exception)
            {
                ProcessID = 0;
            }

            string Result;
            if (Status == 0)
                Result = "SUCCESS";
            else
            {
                Result = string.Format("Error {0}", Status);
            }
            String message = $"<LogInfo> Operation: {Operation}, FilePath: {FileName}, ProcessName: {ProcessName}, ProcessID: {ProcessID}, Status: {Result}";

            Console.WriteLine(message);
        }

        private static void AddEventListners(CBMonitor watcher)
        {
            filemonDemo fMonDemo = new filemonDemo();
            watcher.OnNotifyCreateFile += fMonDemo.CBFSFltNotifyCreateFile;
            watcher.OnNotifyOpenFile += fMonDemo.CBFSFltNotifyOpenFile;
            watcher.OnNotifySetAllocationSize += fMonDemo.CBFSFltNotifySetAllocationSize;
            watcher.OnNotifySetFileSize += fMonDemo.CBFSFltNotifySetFileSize;
            watcher.OnNotifySetFileAttributes += fMonDemo.CBFSFltNotifySetFileAttributes;
            watcher.OnNotifyDeleteFile += fMonDemo.CBFSFltNotifyDeleteFile;
            watcher.OnNotifyRenameOrMoveFile += fMonDemo.CBFSFltNotifyRenameOrMoveFile;
            watcher.OnNotifyCreateHardLink += fMonDemo.CBFSFltNotifyCreateHardLink;
            watcher.OnNotifyReadFile += fMonDemo.CBFSFltNotifyReadFile;
            watcher.OnNotifyWriteFile += fMonDemo.CBFSFltNotifyWriteFile;
            watcher.OnNotifyEnumerateDirectory += fMonDemo.CBFSFltNotifyEnumerateDirectory;
            watcher.OnNotifyCloseFile += fMonDemo.CBFSFltNotifyCloseFile;
            watcher.OnNotifySetFileSecurity += fMonDemo.CBFSFltNotifySetFileSecurity;
            watcher.OnFilterStop += fMonDemo.CBFSFltFilterStop;
            watcher.OnAfterFilterAttachToVolume += fMonDemo.CBFSFltAfterFilterAttachToVolume;
            watcher.OnAfterFilterDetachFromVolume += fMonDemo.CBFSFltAfterFilterDetachFromVolume;
            watcher.OnNotifyFilterAttachToVolume += fMonDemo.CBFSFltNotifyFilterAttachToVolume;
            watcher.FireVolumeEvents = Constants.FS_MOUNT_BOTH;
        }

        private static void AskForReboot()
        {
            Console.WriteLine("System restart is needed in order to install the drivers. Reboot now? (Input 'Yes' to reboot or anything else to decline)");
            string inputValue = Console.ReadLine();
            if (!String.IsNullOrEmpty(inputValue) && inputValue.Equals("Yes"))
                InitiateSystemShutdown("", "", 10000, 0, 1);
        }

        void CBFSFltNotifyCreateFile(object Sender, CBMonitorNotifyCreateFileEventArgs args)
        {
            AddToLog((CBMonitor)Sender, "NotifyCreateFile", args.FileName, mWatcher.NtStatusToWin32Error(args.Status));
        }

        void CBFSFltNotifySetAllocationSize(object Sender, CBMonitorNotifySetAllocationSizeEventArgs args)
        {
            AddToLog((CBMonitor)Sender, "NotifySetAllocationSize", args.FileName, mWatcher.NtStatusToWin32Error(args.Status));
        }

        void CBFSFltNotifySetFileSize(object Sender, CBMonitorNotifySetFileSizeEventArgs args)
        {
            AddToLog((CBMonitor)Sender, "NotifySetFileSize", args.FileName, mWatcher.NtStatusToWin32Error(args.Status));
        }

        void CBFSFltNotifySetFileAttributes(object Sender, CBMonitorNotifySetFileAttributesEventArgs args)
        {
            AddToLog((CBMonitor)Sender, "NotifySetFileAttributes", args.FileName, mWatcher.NtStatusToWin32Error(args.Status));
        }

        void CBFSFltNotifyDeleteFile(object Sender, CBMonitorNotifyDeleteFileEventArgs args)
        {
            AddToLog((CBMonitor)Sender, "NotifyDelete", args.FileName, 0);
        }

        void CBFSFltNotifyRenameOrMoveFile(object Sender, CBMonitorNotifyRenameOrMoveFileEventArgs args)
        {
            AddToLog((CBMonitor)Sender, "NotifyRenameOrMoveFile", args.FileName, mWatcher.NtStatusToWin32Error(args.Status));
        }

        void CBFSFltNotifyCreateHardLink(object Sender, CBMonitorNotifyCreateHardLinkEventArgs args)
        {
            AddToLog((CBMonitor)Sender, "NotifyCreateHardLink", args.LinkName, mWatcher.NtStatusToWin32Error(args.Status));
        }

        void CBFSFltNotifyReadFile(object Sender, CBMonitorNotifyReadFileEventArgs args)
        {
            AddToLog((CBMonitor)Sender, "NotifyReadFile", args.FileName, mWatcher.NtStatusToWin32Error(args.Status));
        }

        void CBFSFltNotifyWriteFile(object Sender, CBMonitorNotifyWriteFileEventArgs args)
        {
            AddToLog((CBMonitor)Sender, "NotifyWriteFile", args.FileName, mWatcher.NtStatusToWin32Error(args.Status));
        }

        void CBFSFltNotifyEnumerateDirectory(object Sender, CBMonitorNotifyEnumerateDirectoryEventArgs args)
        {
            AddToLog((CBMonitor)Sender, "NotifyEnumerateDirectory", args.DirectoryName + Path.DirectorySeparatorChar + args.FileName, mWatcher.NtStatusToWin32Error(args.Status));
        }

        void CBFSFltNotifyOpenFile(object Sender, CBMonitorNotifyOpenFileEventArgs args)
        {
            AddToLog((CBMonitor)Sender, "NotifyOpenFile", args.FileName, mWatcher.NtStatusToWin32Error(args.Status));
        }

        void CBFSFltNotifyCloseFile(object Sender, CBMonitorNotifyCloseFileEventArgs args)
        {
            AddToLog((CBMonitor)Sender, "NotifyCloseFile", args.FileName, 0);
        }

        void CBFSFltNotifySetFileSecurity(object Sender, CBMonitorNotifySetFileSecurityEventArgs args)
        {
            AddToLog((CBMonitor)Sender, "NotifySetFileSecurity", args.FileName, mWatcher.NtStatusToWin32Error(args.Status));
        }

        private void CBFSFltNotifyFilterAttachToVolume(object sender, CBMonitorNotifyFilterAttachToVolumeEventArgs e)
        {
            AddToLog((CBMonitor)sender, "NotifyFilterAttach", e.VolumeName, 0);
        }

        private void CBFSFltAfterFilterDetachFromVolume(object sender, CBMonitorAfterFilterDetachFromVolumeEventArgs e)
        {
            AddToLog((CBMonitor)sender, "AfterFilterDetach", string.Format("{0} ({1})", e.VolumeName, mWatcher.GetVolumeGUID()), 0);
        }

        private void CBFSFltAfterFilterAttachToVolume(object sender, CBMonitorAfterFilterAttachToVolumeEventArgs e)
        {
            AddToLog((CBMonitor)sender, "AfterFilterAttach", string.Format("{0} ({1})", e.VolumeName, mWatcher.GetVolumeGUID()), 0);
        }

        private void CBFSFltFilterStop(object sender, CBMonitorFilterStopEventArgs e)
        {
            CheckDriver();
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
                    res = Path.GetFullPath(path);

                    if (res.StartsWith("\\\\") && !Directory.Exists(res))
                    {
                        Console.WriteLine("The network folder '{0}' does not exist.", res);
                    }
                    else if (!File.Exists(res) && !Directory.Exists(res))
                    {
                        Console.WriteLine("The path '{0}' does not exist.", res);
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
            string PathToMonitor = null;
            string FilenameMask = null;

            int argi = 0, argLen = 0;
            bool stopOpt = false;

            Banner();

            // if no parameters specified, show usage and quit
            if (args.Length < 2)
            {
                Usage();
                CheckDriver();
                return;
            }

            for (argi = 0; argi < args.Length; argi++)
            {
                string arg = args[argi];
                argLen = arg.Length;
                if (argLen > 0)
                {
                    if ((arg[0] == '-') && !stopOpt)
                    {
                        if (String.Equals(arg, "-drv", StringComparison.OrdinalIgnoreCase))
                        {
                            arg = ConvertRelativePathToAbsolute(args[++argi]);
                            if (String.IsNullOrEmpty(arg)){
                                Console.WriteLine("Invalid Driver Path");
                                return;
                            }
                            if (argi < args.Length)
                            {
                                Install(arg);
                            }
                        }
                        else
                            Console.WriteLine("Error: Invalid option " + arg);
                    }
                    else
                    {
                        // if we have not set the path yet, do this now. Otherwise, set the mask
                        if (String.IsNullOrEmpty(PathToMonitor))
                        {
                            PathToMonitor = ConvertRelativePathToAbsolute(arg.ToString());
                            if (String.IsNullOrEmpty(PathToMonitor)){
                                Console.WriteLine("Invalid Path To Monitor");
                                return;
                            }
                            if (!Directory.Exists(PathToMonitor))
                            {
                                Console.WriteLine($"ERROR: the specified path '{PathToMonitor}' does not point to an existing directory");
                                return;
                            }
                        }
                        else
                        {
                            FilenameMask = arg.ToString();
                            break; // no more parameters expected
                        }
                    }
                }
            }

            if (!CheckDriver()) //driver not installed, so we must quit
            {
                return;
            }

            // Probably, we have just installed the driver, so we can quit without monitoring anything
            if (PathToMonitor == null)
                return;

            Console.WriteLine("Press any key to start monitoring, then press any key to stop and quit.");

            Console.ReadKey();

            AddEventListners(mWatcher);

            try
            {
                string mask = $"{PathToMonitor}\\{FilenameMask}";
                SetFilter(mask);
                Console.WriteLine("Press any key to stop monitoring and quit.");
                Console.ReadKey();
                DeleteFilter();
            }
            catch (Exception ex)
            {
                Console.WriteLine("Error: " + ex.Message);
            }

            return;
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