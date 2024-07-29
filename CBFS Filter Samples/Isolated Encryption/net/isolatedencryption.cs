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
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Runtime.InteropServices;
using System.Diagnostics;
using Microsoft.Win32.SafeHandles;

using callback.CBFSFilter;

using static callback.Demos.Win32Import;

namespace callback.Demos
{
    internal class isolatedencryptionDemo
    {
        private const string mGuid = "{713CC6CE-B3E2-4fd9-838D-E28F558F6866}";
        private const string ALTITUDE_FAKE_VALUE_FOR_DEBUG = "149995.24";

        private const string Password = "isolatedencryptionDemo";

        public static uint DEFAULT_HEADER_SIZE = 512;
        public static uint BYTES_PER_SECTOR = 512;
        public static uint DEFAULT_CLUSTER_SIZE = 4096; // this value is specific to the system configuration and must be obtained on initialization

        private CBFilter mFilter = new CBFilter();
        private string[] whitelistProcs = new string[0];
        bool Logging = false;

        public isolatedencryptionDemo()
        {
            mFilter.OnBeforeCreateFile        += CBFSFltBeforeCreateFile;
            mFilter.OnBeforeOpenFile          += CBFSFltBeforeOpenFile;
            mFilter.OnBeforeQueryFileInfo     += CBFSFltBeforeQueryFileInfo;
            mFilter.OnBeforeSetFileInfo       += CBFSFltBeforeSetFileInfo;
            mFilter.OnBeforeGetFileSecurity   += CBFSFltBeforeGetFileSecurity;
            mFilter.OnBeforeSetFileSecurity   += CBFSFltBeforeSetFileSecurity;
            mFilter.OnBeforeReadFile          += CBFSFltBeforeReadFile;
            mFilter.OnBeforeWriteFile         += CBFSFltBeforeWriteFile;
            mFilter.OnBeforeRenameOrMoveFile  += CBFSFltBeforeRenameOrMoveFile;
            mFilter.OnAfterEnumerateDirectory += CBFSFltAfterEnumerateDirectory;
            mFilter.OnCleanupContext          += CBFSFltCleanupContext;
        }

        public static long ROUND_TO_CLUSTER(long Value)
        {
            return ((Value + DEFAULT_CLUSTER_SIZE - 1) & ~(DEFAULT_CLUSTER_SIZE - 1));
        }

        public void Start(String pathToIsolate, String whitelistProcs, bool logging)
        {
            Logging = logging;

            this.whitelistProcs = whitelistProcs.Split(",");

            try
            {
                mFilter.AddFilterRuleEx(pathToIsolate,
                    string.Empty,
                    Constants.ACCESS_NONE,
                    Constants.FS_CE_AFTER_ENUMERATE_DIRECTORY,
                    Constants.FS_NE_NONE,
                    -1, -1, FILE_ATTRIBUTE_DIRECTORY, 0
                    );

                mFilter.AddFilterRuleEx(pathToIsolate + "\\*.*",
                    string.Empty,
                    Constants.ACCESS_NONE,
                    Constants.FS_CE_BEFORE_CREATE |
                    Constants.FS_CE_BEFORE_OPEN |
                    Constants.FS_CE_BEFORE_QUERY_FILE_INFO |
                    Constants.FS_CE_BEFORE_SET_FILE_INFO |
                    Constants.FS_CE_BEFORE_GET_SECURITY |
                    Constants.FS_CE_BEFORE_SET_SECURITY |
                    Constants.FS_CE_BEFORE_READ |
                    Constants.FS_CE_BEFORE_WRITE |
                    Constants.FS_CE_BEFORE_RENAME |
                    Constants.FS_CE_AFTER_ENUMERATE_DIRECTORY,
                    Constants.FS_NE_NONE,
                    -1, -1, 0, FILE_ATTRIBUTE_DIRECTORY
                    );

                mFilter.ProcessCachedIORequests = true;

                mFilter.Initialize(mGuid);
                mFilter.Config("AllowFileAccessInBeforeOpen=false");
                mFilter.Config("ModifiableReadWriteBuffers=true");
                mFilter.StartFilter(0);
            }
            catch (CBFSFilterException err)
            {
                AddToLog("Error in start: " + err.Message);
            }
        }

        public void Stop()
        {
            mFilter.DeleteAllFilterRules();
            if (mFilter.Active)
                mFilter.StopFilter();
        }

        private void AddToLog(string Value)
        {
            if (!Logging) return;

            Console.WriteLine(Value);
        }

        private string CacheTypeToString(int type)
        {
            switch (type)
            {
                case Constants.FS_REQUEST_DIR_USER_CACHED:
                    return "c";
                case Constants.FS_REQUEST_DIR_SYSTEM_CACHED:
                    return "c";

                case Constants.FS_REQUEST_DIR_USER_NONCACHED:
                    return "n";

                case Constants.FS_REQUEST_DIR_SYSTEM_NONCACHED:
                    return "n";
                default:
                    throw new ArgumentException();
            }
        }

        #region CBFilter event handlers

        void CBFSFltBeforeCreateFile(object sender, CBFilterBeforeCreateFileEventArgs e)
        {
            FileContext fileContext;

            string procName = mFilter.GetOriginatorProcessName();

            e.ResultCode = OnCreateOrOpenFile(e.FileName, e.DesiredAccess, e.ShareMode, e.CreateDisposition, e.Options, e.Attributes, procName, out fileContext);

            if (fileContext != null)
            {
                Debug.Assert(e.ResultCode == NO_ERROR);

                e.Isolate = true;
                e.BackendFileName = "";

                e.FileContext = (IntPtr)GCHandle.Alloc(fileContext);

                e.ProcessRequest = true;
            }
        }

        void CBFSFltBeforeOpenFile(object sender, CBFilterBeforeOpenFileEventArgs e)
        {
            FileContext fileContext;

            string procName = mFilter.GetOriginatorProcessName();

            e.ResultCode = OnCreateOrOpenFile(e.FileName, e.DesiredAccess, e.ShareMode, e.CreateDisposition, e.Options, e.Attributes, procName, out fileContext);

            if (fileContext != null)
            {
                Debug.Assert(e.ResultCode == NO_ERROR);

                e.Isolate = true;
                e.BackendFileName = "";

                e.FileContext = (IntPtr)GCHandle.Alloc(fileContext);

                e.ProcessRequest = true;
            }
        }

        int OnCreateOrOpenFile(string fileName, int desiredAccess, int shareMode, int disposition, int options, int attributes, string procName, out FileContext fileContext)
        {
            int result = NO_ERROR;
            var isolated = false;

            fileContext = null;

            if ((attributes & FILE_ATTRIBUTE_DIRECTORY) != 0)
                return result;

            foreach (var whitelistProc in whitelistProcs)
            {
                if (procName.EndsWith("\\" + whitelistProc, StringComparison.OrdinalIgnoreCase))
                {
                    isolated = true;
                    break;
                }
            }

            if (isolated == false)
                return result;

            AddToLog(string.Format("BeforeCreate/OpenFile {0} {1}", procName, fileName));

            var context = new FileContext(mFilter, fileName, Password);

            var intResult = context.Initialize(desiredAccess, shareMode, disposition, options | attributes);

            if (intResult == ERROR_NOT_SUPPORTED)
                return result;

            result = intResult;

            if (result == NO_ERROR)
                fileContext = context;

            return result;
        }

        void CBFSFltBeforeQueryFileInfo(object sender, CBFilterBeforeQueryFileInfoEventArgs e)
        {
            FileContext fileContext;

            if (e.FileContext == IntPtr.Zero)
                return;

            AddToLog(string.Format("BeforeQueryFileInfo {0}", e.FileName));

            fileContext = (FileContext)GCHandle.FromIntPtr(e.FileContext).Target;

            int bytesWritten = 0;

            e.Status = fileContext.OnNtQueryFileInfo(e.Buffer, e.BufferLength, (FILE_INFORMATION_CLASS)e.FileInformationClass, out bytesWritten);

            e.ValidBytes     = bytesWritten;
            e.ProcessRequest = false;
        }

        void CBFSFltBeforeSetFileInfo(object sender, CBFilterBeforeSetFileInfoEventArgs e)
        {
            FileContext fileContext;

            //
            // Rename operations will be processed in CBFSFltBeforeRenameOrMoveFile
            //

            if (e.FileInformationClass == 10/*FileRenameInformation*/ || 
                e.FileInformationClass == 65/*FileRenameInformationEx*/)
                return;

            if (e.FileContext == IntPtr.Zero)
                return;

            AddToLog(string.Format("BeforeSetFileInfo {0}", e.FileName));

            fileContext = (FileContext)GCHandle.FromIntPtr(e.FileContext).Target;

            e.Status = fileContext.OnNtSetFileInfo(e.Buffer, e.BufferLength, (FILE_INFORMATION_CLASS)e.FileInformationClass);
            e.ProcessRequest = false;
        }

        void CBFSFltBeforeGetFileSecurity(object sender, CBFilterBeforeGetFileSecurityEventArgs e)
        {
            FileContext fileContext;

            if (e.FileContext == IntPtr.Zero)
                return;

            AddToLog(string.Format("BeforeGetFileSecurity {0}", e.FileName));

            fileContext = (FileContext)GCHandle.FromIntPtr(e.FileContext).Target;

            int lenNeeded = 0;

            e.Status = fileContext.OnNtQuerySecurity(e.SecurityInformation, e.SecurityDescriptor, e.Length, out lenNeeded);

            e.LengthNeeded = lenNeeded;
            e.ProcessRequest = false;
        }

        void CBFSFltBeforeSetFileSecurity(object sender, CBFilterBeforeSetFileSecurityEventArgs e)
        {
            FileContext fileContext;

            if (e.FileContext == IntPtr.Zero)
                return;

            AddToLog(string.Format("BeforeSetFileSecurity {0}", e.FileName));

            fileContext = (FileContext)GCHandle.FromIntPtr(e.FileContext).Target;

            e.Status = fileContext.OnNtSetSecurity(e.SecurityInformation, e.SecurityDescriptor);
            e.ProcessRequest = false;
        }

        void CBFSFltBeforeReadFile(object sender, CBFilterBeforeReadFileEventArgs e)
        {
            try
            {
                FileContext fileContext;

                if (IntPtr.Zero == e.FileContext)
                    return;

                AddToLog(string.Format("BeforeReadFile [{0}] {1} pos({2}) read({3})", CacheTypeToString(e.Direction), e.FileName, e.Position, e.BytesToRead));

                fileContext = (FileContext)GCHandle.FromIntPtr(e.FileContext).Target;

                if (e.Direction == Constants.FS_REQUEST_DIR_USER_CACHED ||
                    e.Direction == Constants.FS_REQUEST_DIR_SYSTEM_CACHED)
                {
                    AddToLog(string.Format("BeforeReadFile [c] POS({0}) ToRead({1})", e.Position, e.BytesToRead));
                }
                else
                {
                    AddToLog(string.Format("BeforeReadFile [n] POS({0}) ToRead({1})", e.Position, e.BytesToRead));

                    int bytesRead = 0;

                    e.ResultCode = fileContext.OnNonCachedRead(e.Buffer, e.Position, e.BytesToRead, out bytesRead);

                    e.BytesToRead    = bytesRead;
                    e.ProcessRequest = false;
                }
            }
            catch (Exception exc)
            {
                AddToLog(string.Format("BeforeReadFile Exception: {0}", exc.Message));
            }
        }

        void CBFSFltBeforeWriteFile(object sender, CBFilterBeforeWriteFileEventArgs e)
        {
            try
            {
                FileContext fileContext;

                if (e.FileContext == IntPtr.Zero)
                    return;

                AddToLog(string.Format("BeforeWriteFile [{0}] {0} Offset({1}) Size({2})", CacheTypeToString(e.Direction), e.FileName, e.Position, e.BytesToWrite));

                fileContext = (FileContext)GCHandle.FromIntPtr(e.FileContext).Target;

                if (fileContext.HeaderPresent == false)
                {
                    e.ResultCode = fileContext.EncryptFile();
                    if (e.ResultCode != NO_ERROR)
                        return;
                }

                if (e.Direction == Constants.FS_REQUEST_DIR_USER_CACHED ||
                    e.Direction == Constants.FS_REQUEST_DIR_SYSTEM_CACHED)
                {
                    if (e.Position + e.BytesToWrite > fileContext.CalcedFileSize)
                    {
                        e.ResultCode = fileContext.OnCachedWriteExtendFileSize(e.Position + e.BytesToWrite);
                    }
                }
                else
                {
                    int bytesWritten = 0;

                    e.ResultCode = fileContext.OnNonCachedWrite(e.Buffer, e.Position, e.BytesToWrite, out bytesWritten);

                    e.BytesToWrite   = bytesWritten;
                    e.ProcessRequest = false;
                }
            }
            catch (Exception exc)
            {
                AddToLog(string.Format("BeforeWriteFile Exception: {0}", exc.Message));
            }
        }

        void CBFSFltBeforeRenameOrMoveFile(object sender, CBFilterBeforeRenameOrMoveFileEventArgs e)
        {
            FileContext fileContext;

            var fileName = e.FileName;
            var newFileName = e.NewFileName;

            AddToLog(string.Format("BeforeRenameOrMoveFile {0} => {1}", fileName, newFileName));

            if (e.FileContext != IntPtr.Zero)
            {
                if (mFilter.IsFileFiltered(newFileName) == false)
                {
                    e.ResultCode = ERROR_NOT_SAME_DEVICE;
                    return;
                }

                fileContext = (FileContext)GCHandle.FromIntPtr(e.FileContext).Target;

                e.ResultCode = fileContext.OnRenameOrMoveFile(newFileName, e.ReplaceIfExists);

                e.ProcessRequest = false;
            }
            else
            {
                string procName = mFilter.GetOriginatorProcessName();

                bool isWhiteProc = false;
                foreach (var whitelistProc in whitelistProcs)
                {
                    if (procName.EndsWith("\\" + whitelistProc, StringComparison.OrdinalIgnoreCase))
                    {
                        isWhiteProc = true;
                        break;
                    }
                }

                if (isWhiteProc == true)
                {
                    e.ResultCode = ERROR_NOT_SAME_DEVICE;
                    return;
                }

                //
                // The event is in filter mode at this point and we let the request go forward.
                //

                e.ProcessRequest = true;
            }
        }

        void CBFSFltAfterEnumerateDirectory(object sender, CBFilterAfterEnumerateDirectoryEventArgs e)
        {
            //AddToLog(string.Format("AfterEnumerateDirectory {0}", e.FileName));

            Debug.Assert(e.DirectoryContext == IntPtr.Zero);

            string procName = mFilter.GetHandleCreatorProcessName();

            // we presume that all files in directory are filtered and encrypted,
            // but we provide the real size of decrypted data only to those processes that will receive this decrypted data
            foreach (var whitelistProc in whitelistProcs)
            {
                if (string.Compare(procName, whitelistProc, true) == 0)
                {
                    if (e.Size >= FileHeader.HEADER_SIZE)
                        e.Size -= FileHeader.HEADER_SIZE;

                    break;
                }
            }

            //e.FileFound = true;
        }

        void CBFSFltCleanupContext(object sender, CBFilterCleanupContextEventArgs e)
        {
            FileContext fileContext;

            if (e.FileContext != IntPtr.Zero)
            {
                fileContext = (FileContext)GCHandle.FromIntPtr(e.FileContext).Target;

                AddToLog(string.Format("CleanupContext {0}", fileContext.FileName));

                fileContext.Close();

                GCHandle.FromIntPtr(e.FileContext).Free();
            }
        }

        #endregion


        // ------------------------------------------------------------------------

        static void Main(string[] args)
        {
            String pathToIsolate = null;
            String whitelistProcs = null;
            bool logging = false;

            int argi, argLen;
            bool stopOpt = false;

            Banner();

            try
            {
                if (args.Length < 2)
                {
                    Usage();
                    return;
                }

                for (argi = 0; argi < args.Length; argi++)
                {
                    String arg = args[argi];
                    argLen = arg.Length;
                    if (argLen > 0)
                    {
                        if ((arg[0] == '-') && !stopOpt)
                        {
                            if (arg.ToLower() == "-drv")
                            {
                                arg = ConvertRelativePathToAbsolute(args[++argi]);
                                if (string.IsNullOrEmpty(arg))
                                {
                                    Console.WriteLine("Invalid Driver Path");
                                    return;
                                }
                                if (argi < args.Length)
                                {
                                    Install(arg);
                                }
                            }
                            else if (arg.ToLower() == "-log")
                            {
                                logging = true;
                            }
                            else if (arg == "--")
                            {
                                stopOpt = true;
                            }
                            else
                            {
                                Console.WriteLine("Error: Invalid option " + arg);
                            }
                        }
                        else
                        {
                            // if we have not set the path yet, do this now. Otherwise, set the mask
                            if (pathToIsolate == null)
                            {
                                pathToIsolate = ConvertRelativePathToAbsolute(arg);
                                if (string.IsNullOrEmpty(pathToIsolate))
                                {
                                    Console.WriteLine("Invalid Filter Path");
                                    return;
                                }
                                if (!Directory.Exists(pathToIsolate))
                                {
                                    Console.WriteLine("ERROR: the specified path '" + pathToIsolate
                                            + "' does not point to an existing directory");
                                    return;
                                }
                            }
                            else if (whitelistProcs == null)
                            {

                                whitelistProcs = arg;
                                break; // no more parameters expected
                            }
                        }
                    }
                }

                if (!CheckDriver())
                { // driver not installed, so we must quit
                    return;
                }

                // Probably, we have just installed the driver, so we can quit without monitoring anything
                if (pathToIsolate == null || whitelistProcs == null)
                    return;

                Console.WriteLine("Press 'Enter' to start monitoring, then press 'Enter' and quit.");
                Console.ReadKey();

                try
                {
                    isolatedencryptionDemo instance = new isolatedencryptionDemo();
                    instance.Start(pathToIsolate, whitelistProcs, logging);

                    Console.WriteLine("Press 'Enter' to stop monitoring and quit.");
                    Console.ReadKey();

                    instance.Stop();
                }
                catch (Exception ex)
                {
                    Console.WriteLine("Error: " + ex.Message);
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("Error in Main: " + e.Message);
            }
        }

        static void Banner()
        {
            Console.WriteLine("CBFS Filter isolatedencryption Demo Copyright (c) 2017-2024, Callback Technologies, Inc.\n\n");
        }

        static void Usage()
        {
            Console.WriteLine("Usage: isolatedencryption [-<switch 1> ... -<switch N>] [<path to isolate>] [<whitelist processes, comma separated>]\n");
            Console.WriteLine("<Switches>");
            Console.WriteLine("  -drv <path to cbfilter.cab> - Install the driver from the specified CAB file");
            Console.WriteLine("  -- Stop switches scanning\n");
        }

        static bool IsDriveLetter(string path)
        {
            if (string.IsNullOrEmpty(path))
                return false;

            char c = path[0];
            if ((char.IsLetter(c) && path.Length >= 2 && path[1] == ':'))
                return true;
            else
                return false;
        }

        static string ConvertRelativePathToAbsolute(string path)
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
                else
                if (!IsDriveLetter(path))
                {
                    try
                    {
                        res = Path.GetFullPath(path);

                        if (res.StartsWith("\\\\") && !Directory.Exists(res))
                        {
                            Console.WriteLine("The network folder '" + res + "' does not exist.");
                        }
                        else
                        if (!File.Exists(res) && !Directory.Exists(res))
                        {
                            Console.WriteLine("The path '" + res + "' does not exist.");
                        }
                    }
                    catch (Exception ex)
                    {
                        Console.WriteLine(string.Format("ConvertRelativePathToAbsolute: exception '{0}'", ex.ToString()));
                        return null;
                    }
                }
            }
            return res;
        }

        static bool CheckDriver()
        {
            try
            {
                CBFilter filter = new CBFilter();

                int moduleStatus = filter.GetDriverStatus(mGuid);
                long moduleVersion = filter.GetDriverVersion(mGuid);
                int versionHigh = (int)(moduleVersion >> 32);
                int versionLow = (int)(moduleVersion & 0xFFFFFFFF);

                String driverInfo;
                bool res = moduleStatus != 0;

                if (moduleStatus != 0)
                {
                    driverInfo = String.Format("Driver version: {0}.{1}.{2}.{3}",
                            versionHigh >> 16, versionHigh & 0xFFFF, versionLow >> 16, versionLow & 0xFFFF);
                }
                else
                {
                    driverInfo = "Driver: not installed";
                }

                Console.WriteLine(driverInfo);

                return res;
            }
            catch (Exception ex)
            {
                Console.WriteLine("Error in checkDriver: " + ex.Message);
                return false;
            }
        }

        static void Install(String fileName)
        {
            if (!File.Exists(fileName))
                return;

            try
            {
                Console.WriteLine("Installing the driver from " + fileName);

                CBFilter filter = new CBFilter();

                var reboot = filter.Install(fileName, mGuid, "", ALTITUDE_FAKE_VALUE_FOR_DEBUG, 0, "");

                CheckDriver();

                if (reboot)
                    Console.WriteLine("Please, reboot the system for the changes to take effect");
                else
                    Console.WriteLine("Driver installed successfully");

            }
            catch (CBFSFilterException e)
            {
                if (e.Code == 1314/*ERROR_PRIVILEGE_NOT_HELD*/ || e.Code == 5/*ERROR_ACCESS_DENIED*/)
                    Console.WriteLine("Installation requires administrator rights. Run the app as administrator");
            else
                    Console.WriteLine(e.Message);
            }
        }

        static void Uninstall(String fileName)
        {
            if (!File.Exists(fileName))
                return;
            try
            {
                Console.WriteLine("Uninstalling the driver from " + fileName);

                CBFilter filter = new CBFilter();

                var reboot = filter.Uninstall(fileName, mGuid, "", 0);

                CheckDriver();

                if (reboot)
                    Console.WriteLine("Please, reboot the system for the changes to take effect");
                else
                    Console.WriteLine("Driver uninstalled successfully");

            }
            catch (CBFSFilterException e)
            {
                if (e.Code == 1314/*ERROR_PRIVILEGE_NOT_HELD*/ || e.Code == 5/*ERROR_ACCESS_DENIED*/)
                    Console.WriteLine("Uninstallation requires administrator rights. Run the app as administrator");
                else
                    Console.WriteLine(e.Message);
            }
        }
    }

    #region Internal classes
    public class FileContext : IDisposable
    {
        public const int MASTER_KEY_SIZE = 16;

        CBFilter filter;
        string fileName;
        byte[] masterKey = new byte[MASTER_KEY_SIZE];
        IntPtr fileHandle = new IntPtr(-1);
        long fileSize = 0;
        FileHeader fileHeader;

        public FileContext(CBFilter filter, string fileName, string password)
        {
            this.filter = filter;
            this.fileName = fileName;

            var keyBytes = Encoding.UTF8.GetBytes(password + "abcdefghijklmnopqrst");

            Array.Copy(keyBytes, masterKey, masterKey.Length);

            fileHeader = new FileHeader(masterKey);
        }

        public string FileName         => fileName;
        public bool   HeaderPresent    => fileHeader.Present;
        public long   ActualFileSize   => fileSize;
        public long   CalcedFileSize   => HeaderPresent ? ActualFileSize - FileHeader.HEADER_SIZE : ActualFileSize;
        public int    CalcedHeaderSize => HeaderPresent ? FileHeader.HEADER_SIZE : 0;

        public int Initialize(int desiredAccess, int shareMode, int creationDisposition, int flagsAndAttributes)
        {
            int errorCode = NO_ERROR;
            IntPtr auxiHandle = new IntPtr(-1);
            IntPtr operHandle;

            try
            {
                // Use original input parameters to perform share access check.
                long handleVaule = filter.CreateFileDirect(fileName, false,
                    desiredAccess, shareMode, creationDisposition, flagsAndAttributes,
                    false, false);

                fileHandle = new IntPtr(handleVaule);
            }
            catch (CBFSFilterException e)
            {
                errorCode = e.Code;
                goto end;
            }

            operHandle = fileHandle;

            if ((desiredAccess & FILE_READ_ATTRIBUTES) == 0)
            {
                try
                {
                    var auxiHandleValue = filter.CreateFileDirect(fileName, false,
                            GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE |
                            Constants.CBFILTER_IGNORE_SHARE_ACCESS_CHECK
                            ,
                            OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS,
                            false, false);

                    auxiHandle = new IntPtr(auxiHandleValue);
                }
                catch (CBFSFilterException e)
                {
                    errorCode = e.Code;
                    goto end;
                }

                operHandle = auxiHandle;
            }

            BY_HANDLE_FILE_INFORMATION fileInfo;
            if (!GetFileInformationByHandle(operHandle, out fileInfo))
            {
                errorCode = Marshal.GetLastWin32Error();
                goto end;
            }

            if ((fileInfo.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) == FILE_ATTRIBUTE_DIRECTORY)
            {
                errorCode = ERROR_NOT_SUPPORTED;
                goto end;
            }

            fileSize = (((long)fileInfo.nFileSizeHigh) << 32) | ((long)fileInfo.nFileSizeLow);

            if (fileSize >= FileHeader.HEADER_SIZE)
            {
                if (auxiHandle == new IntPtr(-1) && (desiredAccess & FILE_READ_DATA) == 0)
                {
                    try
                    {
                        var auxiHandleValue = filter.CreateFileDirect(fileName, false,
                                GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE |
                                Constants.CBFILTER_IGNORE_SHARE_ACCESS_CHECK
                                ,
                                OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS,
                                false, false);

                        auxiHandle = new IntPtr(auxiHandleValue);
                    }
                    catch (CBFSFilterException e)
                    {
                        errorCode = e.Code;
                        goto end;
                    }

                    operHandle = auxiHandle;
                }

                errorCode = fileHeader.ParseHeader(operHandle);
                if (errorCode != NO_ERROR)
                    goto end;
            }

            if (HeaderPresent == false)
            {
                errorCode = fileHeader.SetupHeader();
                if (errorCode != NO_ERROR)
                    goto end;
            }

        end:

            if (auxiHandle != new IntPtr(-1))
                CloseHandle(auxiHandle);

            if (errorCode != NO_ERROR)
                Close();

            return errorCode;
        }

        public int OnNtQueryFileInfo(IntPtr buffer, int length, FILE_INFORMATION_CLASS fileInfoClass, out int bytesWritten)
        {
            var ioStatus = new IO_STATUS_BLOCK();

            var status = NtQueryInformationFile(fileHandle, ref ioStatus, buffer, length, fileInfoClass);

            bytesWritten = (int)ioStatus.information;

            if (status == 0 || status == STATUS_BUFFER_OVERFLOW)
            {
                if (fileInfoClass == FILE_INFORMATION_CLASS.FileAllInformation)
                {
                    /*
                     typedef struct _FILE_ALL_INFORMATION {
                        FILE_BASIC_INFORMATION BasicInformation;
                        FILE_STANDARD_INFORMATION StandardInformation;
                        ...
                     } FILE_ALL_INFORMATION, *PFILE_ALL_INFORMATION;
                     */
                    if ((int)ioStatus.information >= Marshal.SizeOf(typeof(FILE_BASIC_INFORMATION)) + Marshal.SizeOf(typeof(FILE_STANDARD_INFORMATION)))
                    {
                        var standInfo = (FILE_STANDARD_INFORMATION)Marshal.PtrToStructure(buffer + Marshal.SizeOf(typeof(FILE_BASIC_INFORMATION)), typeof(FILE_STANDARD_INFORMATION));

                        standInfo.EndOfFile = CalcedFileSize;
                        standInfo.AllocationSize = isolatedencryptionDemo.ROUND_TO_CLUSTER(standInfo.EndOfFile);

                        Marshal.StructureToPtr(standInfo, buffer + Marshal.SizeOf(typeof(FILE_BASIC_INFORMATION)), false);
                    }
                }
                else
                if (fileInfoClass == FILE_INFORMATION_CLASS.FileStandardInformation)
                {
                    var standInfo = (FILE_STANDARD_INFORMATION)Marshal.PtrToStructure(buffer, typeof(FILE_STANDARD_INFORMATION));

                    standInfo.EndOfFile = CalcedFileSize;
                    standInfo.AllocationSize = isolatedencryptionDemo.ROUND_TO_CLUSTER(standInfo.EndOfFile);

                    Marshal.StructureToPtr(standInfo, buffer, false);
                }
            }

            return status;
        }

        public int OnNtSetFileInfo(IntPtr buffer, int length, FILE_INFORMATION_CLASS fileInfoClass)
        {
            var newActualSize = (long?)null;
            if (fileInfoClass == FILE_INFORMATION_CLASS.FileAllocationInformation)
            {
                if (length >= Marshal.SizeOf(typeof(FILE_ALLOCATION_INFORMATION)))
                {
                    var allocInfo = (FILE_ALLOCATION_INFORMATION)Marshal.PtrToStructure(buffer, typeof(FILE_ALLOCATION_INFORMATION));
                    if (allocInfo.AllocationSize < CalcedFileSize)
                    {
                        allocInfo.AllocationSize += CalcedHeaderSize;
                        newActualSize = allocInfo.AllocationSize;

                        Debug.Assert(Marshal.SizeOf(typeof(FILE_ALLOCATION_INFORMATION)) == Marshal.SizeOf(typeof(FILE_END_OF_FILE_INFORMATION)));

                        //
                        // We directly replace it with FileEndOfFileInformation.
                        //

                        fileInfoClass = FILE_INFORMATION_CLASS.FileEndOfFileInformation;
                    }
                    else
                    {
                        allocInfo.AllocationSize += CalcedHeaderSize;
                        allocInfo.AllocationSize = isolatedencryptionDemo.ROUND_TO_CLUSTER(allocInfo.AllocationSize);
                    }

                    Marshal.StructureToPtr(allocInfo, buffer, false);
                }
                else
                {
                    return STATUS_INVALID_PARAMETER;
                }
            }
            else
            if (fileInfoClass == FILE_INFORMATION_CLASS.FileEndOfFileInformation)
            {
                if (length >= Marshal.SizeOf(typeof(FILE_END_OF_FILE_INFORMATION)))
                {
                    var eofInfo = (FILE_END_OF_FILE_INFORMATION)Marshal.PtrToStructure(buffer, typeof(FILE_END_OF_FILE_INFORMATION));

                    eofInfo.EndOfFile += CalcedHeaderSize;

                    newActualSize = eofInfo.EndOfFile;

                    Marshal.StructureToPtr(eofInfo, buffer, false);
                }
                else
                {
                    return STATUS_INVALID_PARAMETER;
                }
            }

            var ioStatus = new IO_STATUS_BLOCK();

            var status = NtSetInformationFile(fileHandle, ref ioStatus, buffer, length, fileInfoClass);
            if (status == 0 && newActualSize != null)
            {
                fileSize = newActualSize.Value;
            }

            return status;
        }

        public int OnNtQuerySecurity(int securityInfo, IntPtr securityDescriptor, int length, out int lenNeeded)
        {
            var status = NtQuerySecurityObject(fileHandle, securityInfo, securityDescriptor, length, out lenNeeded);

            return status;
        }

        public int OnNtSetSecurity(int securityInfo, IntPtr securityDescriptor)
        {
            var status = NtSetSecurityObject(fileHandle, securityInfo, securityDescriptor);

            return status;
        }

        public int OnCachedWriteExtendFileSize(long size)
        {
            var fileInfo = new FileInformation();

            var newActualSize = size + CalcedHeaderSize;

            fileInfo.EndOfFileInfo.EndOfFile = newActualSize;

            if (SetFileInformationByHandle(fileHandle, FileInformationClass.FileEndOfFileInfo, ref fileInfo, 8) == false)
            {
                return Marshal.GetLastWin32Error();
            }

            fileSize = newActualSize;

            return NO_ERROR;
        }

        public int OnNonCachedRead(IntPtr buffer, long position, int bytesToRead, out int bytesRead)
        {
            int bufferLength = bytesToRead;

            Debug.Assert(bufferLength % 512 == 0);

            bytesRead = 0;

            if (position > CalcedFileSize)
                return (int)ERROR_HANDLE_EOF;

            if (position + bytesToRead >= CalcedFileSize)
            {
                bytesToRead = (int)(CalcedFileSize - position);
            }

            var overlapped = new System.Threading.NativeOverlapped();

            position += CalcedHeaderSize;

            overlapped.EventHandle = IntPtr.Zero;
            overlapped.OffsetLow   = (int)position;
            overlapped.OffsetHigh  = (int)(position >> 32);

            if (ReadFile(fileHandle, buffer, bytesToRead, out bytesRead, ref overlapped) == false)
            {
                return Marshal.GetLastWin32Error();
            }

            if (HeaderPresent)
            {
                fileHeader.DecryptBuffer(buffer, bytesRead, bufferLength);
            }

            return NO_ERROR;
        }

        public int OnNonCachedWrite(IntPtr buffer, long position, int bytesToWrite, out int bytesWritten)
        {
            int bufferLength = bytesToWrite;

            Debug.Assert(bufferLength % 512 == 0);

            bytesWritten = 0;

            Debug.Assert(HeaderPresent);

            if (position > CalcedFileSize)
                return NO_ERROR;

            if (position + bytesToWrite >= CalcedFileSize)
            {
                bytesToWrite = (int)(CalcedFileSize - position);
            }

            fileHeader.EncryptBuffer(buffer, bytesToWrite, bufferLength);

            var overlapped = new System.Threading.NativeOverlapped();

            position += CalcedHeaderSize;

            overlapped.EventHandle = IntPtr.Zero;
            overlapped.OffsetLow   = (int)position;
            overlapped.OffsetHigh  = (int)(position >> 32);

            if (WriteFile(fileHandle, buffer, bytesToWrite, out bytesWritten, ref overlapped) == false)
            {
                return Marshal.GetLastWin32Error();
            }

            return NO_ERROR;
        }

        public int EncryptFile()
        {
            Debug.Assert(HeaderPresent == false);

            var dataSize = fileSize;

            var fileInfo = new FileInformation();

            fileInfo.EndOfFileInfo.EndOfFile = dataSize + FileHeader.HEADER_SIZE;
            if (SetFileInformationByHandle(fileHandle, FileInformationClass.FileEndOfFileInfo, ref fileInfo, 8) == false)
            {
                return Marshal.GetLastWin32Error();
            }

            var buffer = new byte[FileHeader.HEADER_SIZE];

            var position = dataSize / buffer.Length * buffer.Length;
            var validDataLen = dataSize % buffer.Length;

            var overlapped = new System.Threading.NativeOverlapped();

            while (true)
            {
                if (validDataLen != 0)
                {
                    int bytesRead;
                    int bytesWritten;

                    overlapped.EventHandle = IntPtr.Zero;
                    overlapped.OffsetLow   = (int)position;
                    overlapped.OffsetHigh  = (int)(position >> 32);

                    if (ReadFile(fileHandle, buffer, (int)validDataLen, out bytesRead, ref overlapped) == false)
                    {
                        return Marshal.GetLastWin32Error();
                    }

                    fileHeader.EncryptBuffer(buffer, (int)validDataLen);

                    var writePos = position + FileHeader.HEADER_SIZE;

                    overlapped.EventHandle = IntPtr.Zero;
                    overlapped.OffsetLow   = (int)writePos;
                    overlapped.OffsetHigh  = (int)(writePos >> 32);

                    if (WriteFile(fileHandle, buffer, (int)validDataLen, out bytesWritten, ref overlapped) == false)
                    {
                        return Marshal.GetLastWin32Error();
                    }
                }

                position -= FileHeader.HEADER_SIZE;
                validDataLen = FileHeader.HEADER_SIZE;

                if (position < 0)
                    break;
            }

            var result = fileHeader.WriteHeader(fileHandle);

            if (result == NO_ERROR)
                fileSize += FileHeader.HEADER_SIZE;

            return result;
        }

        public int OnRenameOrMoveFile(string newFileName, bool replaceIfExists)
        {
            var renameInfo = new FILE_RENAME_INFORMATION();

            renameInfo.Flags = replaceIfExists/*FILE_RENAME_REPLACE_IF_EXISTS */ ? 1 : 0;
            renameInfo.FileName = newFileName;
            renameInfo.FileNameLength = (uint)newFileName.Length * 2;

            IntPtr ptr = IntPtr.Zero;
            try
            {
                int size = Marshal.SizeOf(renameInfo);
                byte[] renameData = new byte[size];

                ptr = Marshal.AllocHGlobal(size);

                Marshal.StructureToPtr(renameInfo, ptr, true);
                Marshal.Copy(ptr, renameData, 0, size);

                //
                // When renaming file with direct handle, we need to use SetFileInformationDirect method.
                //

                filter.SetFileInformationDirect(fileHandle.ToInt64(), 10/*FileRenameInformation*/, renameData);
            }
            catch (CBFSFilterException e)
            {
                return e.Code;
            }
            finally
            {
                Marshal.FreeHGlobal(ptr);
            }

            fileName = newFileName;

            return 0;
        }

        private void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (fileHandle != new IntPtr(-1))
                {
                    CloseHandle(fileHandle);
                    fileHandle = new IntPtr(-1);
                }
            }
        }

        ~FileContext()
        {
            Dispose(false);
        }

        public void Dispose()
        {
            Dispose(true);
        }

        public void Close()
        {
            Dispose(true);
        }
    }

    public class FileHeader
    {
        public const int HEADER_SIZE = 512;

        static byte[] headerTag = Encoding.UTF8.GetBytes("@@@!!!cbfilter isolatedencryption!!!@@@");

        const byte authTag = (byte)'x';

        byte masterXoxKey;
        byte sessionKey;

        bool present = false;

        byte[] headerData = new byte[HEADER_SIZE];

        public bool Present => present;

        public FileHeader(byte[] masterKey)
        {
            masterXoxKey = 0;
            foreach (var keyData in masterKey)
            {
                masterXoxKey += keyData;
            }

            sessionKey = (byte)DateTime.Now.Ticks;
        }

        public int ParseHeader(IntPtr fileHandle)
        {
            long position = 0;
            int  bytesToRead = HEADER_SIZE;
            int  bytesRead = 0;

            var overlapped = new System.Threading.NativeOverlapped();

            overlapped.EventHandle = IntPtr.Zero;
            overlapped.OffsetLow = (int)position;
            overlapped.OffsetHigh = (int)(position >> 32);

            if (ReadFile(fileHandle, headerData, bytesToRead, out bytesRead, ref overlapped) == false)
            {
                return Marshal.GetLastWin32Error();
            }

            for (var i = 0; i < headerTag.Length; ++i)
            {
                // It is not an encrypted file.
                if (headerTag[i] != headerData[i])
                    return NO_ERROR;
            }

            if ((headerData[headerTag.Length] ^ masterXoxKey) != authTag)
            {
                return ERROR_INVALID_PASSWORD;
            }

            sessionKey = (byte)(headerData[headerTag.Length + 1] ^ masterXoxKey);

            present = true;

            return NO_ERROR;
        }

        public int SetupHeader()
        {
            headerData = new byte[HEADER_SIZE];

            Array.Copy(headerTag, headerData, headerTag.Length);

            headerData[headerTag.Length] = (byte)(authTag ^ masterXoxKey);
            headerData[headerTag.Length + 1] = (byte)(sessionKey ^ masterXoxKey);

            present = false;

            return NO_ERROR;
        }

        public int WriteHeader(IntPtr fileHandle)
        {
            long position = 0;
            int  bytesToWrite = HEADER_SIZE;
            int  bytesWritten = 0;

            var overlapped = new System.Threading.NativeOverlapped();

            overlapped.EventHandle = IntPtr.Zero;
            overlapped.OffsetLow   = (int)position;
            overlapped.OffsetHigh  = (int)(position >> 32);

            if (WriteFile(fileHandle, headerData, bytesToWrite, out bytesWritten, ref overlapped) == false)
            {
                return Marshal.GetLastWin32Error();
            }

            present = true;

            return NO_ERROR;
        }

        public void EncryptBuffer(byte[] buffer, int bytesToWrite)
        {
            Debug.Assert(buffer.Length % 512 == 0);

            for (var i = 0; i < bytesToWrite; ++i)
            {
                var data = buffer[i];

                buffer[i] = (byte)(data ^ sessionKey);
            }
        }

        public void EncryptBuffer(IntPtr buffer, int bytesToWrite, int bufferLength)
        {
            Debug.Assert(bufferLength % 512 == 0);

            for (var i = 0; i < bytesToWrite; ++i)
            {
                var data = Marshal.ReadByte(buffer, i);

                Marshal.WriteByte(buffer, i, (byte)(data ^ sessionKey));
            }
        }

        public void DecryptBuffer(IntPtr buffer, int bytesRead, int bufferLength)
        {
            Debug.Assert(bufferLength % 512 == 0);

            for (var i = 0; i < bytesRead; ++i)
            {
                var data = Marshal.ReadByte(buffer, i);

                Marshal.WriteByte(buffer, i, (byte)(data ^ sessionKey));
            }
        }
    }

    public static class Win32Import
    {
        public const int FILE_ATTRIBUTE_NORMAL = 0x00000080;
        public const int FILE_ATTRIBUTE_DIRECTORY = 0x00000010;

        public const int FILE_FLAG_BACKUP_SEMANTICS = 0x02000000;
        public const int FILE_FLAG_NO_BUFFERING = 0x20000000;

        public const int NO_ERROR = 0;

        public const int ERROR_ACCESS_DENIED      = 5;
        public const int ERROR_NOT_SAME_DEVICE    = 17;
        public const int ERROR_SHARING_VIOLATION  = 32;
        public const int ERROR_HANDLE_EOF         = 38;
        public const int ERROR_NOT_SUPPORTED      = 50;
        public const int ERROR_INVALID_PASSWORD   = 86;
        public const int ERROR_PRIVILEGE_NOT_HELD = 1314;

        //create file disposition constants
        public const int CREATE_NEW    = 1;
        public const int CREATE_ALWAYS = 2;
        public const int OPEN_EXISTING = 3;
        public const int OPEN_ALWAYS   = 4;
        public const int TRUNCATE_EXISTING = 5;

        const uint INVALID_FILE_SIZE = 0xFFFFFFFF;

        public const int FILE_READ_DATA = 0x0001;
        public const int FILE_WRITE_DATA = 0x0002;
        public const int FILE_APPEND_DATA = 0x0004;

        public const int DELETE = 0x00010000;
        public const int WRITE_DAC = 0x00040000;
        public const int WRITE_OWNER = 0x00080000;

        public const int FILE_READ_EA = 0x0008;
        public const int FILE_WRITE_EA = 0x0010;

        public const int FILE_READ_ATTRIBUTES = 0x0080;
        public const int FILE_WRITE_ATTRIBUTES = 0x0100;

        public const int SYNCHRONIZE = 0x00100000;

        public const int READ_CONTROL = 0x00020000;
        public const int STANDARD_RIGHTS_READ = READ_CONTROL;
        public const int STANDARD_RIGHTS_WRITE = READ_CONTROL;

        public const int GENERIC_READ = STANDARD_RIGHTS_READ |
                                            FILE_READ_DATA |
                                            FILE_READ_ATTRIBUTES |
                                            FILE_READ_EA |
                                            SYNCHRONIZE;

        public const int GENERIC_WRITE = STANDARD_RIGHTS_WRITE |
                                            FILE_WRITE_DATA |
                                            FILE_WRITE_ATTRIBUTES |
                                            FILE_WRITE_EA |
                                            FILE_APPEND_DATA |
                                            SYNCHRONIZE;

        public const int FILE_SHARE_READ = 0x00000001;
        public const int FILE_SHARE_WRITE = 0x00000002;
        public const int FILE_SHARE_DELETE = 0x00000004;

        public const int PAGE_READWRITE = 0x04;
        public const int MEM_COMMIT = 0x1000;
        public const int MEM_RESERVE = 0x2000;
        public const int MEM_DECOMMIT = 0x4000;
        public const int MEM_RELEASE = 0x8000;

        [DllImport("kernel32", SetLastError = true)]
        public static extern IntPtr VirtualAlloc(IntPtr lpStartAddr, UInt32 dwSize, UInt32 dwAllocationType, UInt32 dwProtect);

        [DllImport("kernel32", SetLastError = true)]
        public static extern int VirtualFree(IntPtr lpAddress, UInt32 dwSize, UInt32 dwFreeType);

        [DllImport("kernel32.dll")]
        public static extern void SetLastError(uint dwErrCode);

        [DllImport("kernel32.dll", SetLastError = true, CharSet = CharSet.Auto)]
        public static extern bool GetDiskFreeSpace(string lpRootPathName, out uint lpSectorsPerCluster, out uint lpBytesPerSector, out uint lpNumberOfFreeClusters, out uint lpTotalNumberOfClusters);

        [DllImport("kernel32.dll", SetLastError = true)]
        public static extern bool ReadFile(IntPtr hFile, byte[] lpBuffer, int nNumberOfBytesToRead, out int lpNumberOfBytesRead, [In] ref System.Threading.NativeOverlapped lpOverlapped);

        [DllImport("kernel32.dll", SetLastError = true)]
        public static extern bool ReadFile(IntPtr hFile, byte[] lpBuffer, int nNumberOfBytesToRead, out int lpNumberOfBytesRead, IntPtr lpOverlapped);

        [DllImport("kernel32.dll", SetLastError = true)]
        public static extern bool ReadFile(IntPtr hFile, IntPtr lpBuffer, int nNumberOfBytesToRead, out int lpNumberOfBytesRead, [In] ref System.Threading.NativeOverlapped lpOverlapped);

        [DllImport("kernel32.dll", SetLastError = true)]
        public static extern bool ReadFile(IntPtr hFile, IntPtr lpBuffer, int nNumberOfBytesToRead, out int lpNumberOfBytesRead, IntPtr lpOverlapped);

        [DllImport("kernel32.dll", SetLastError = true)]
        public static extern bool WriteFile(IntPtr hFile, byte[] lpBuffer, int nNumberOfBytesToWrite, out int lpNumberOfBytesWritten, [In] ref System.Threading.NativeOverlapped lpOverlapped);

        [DllImport("kernel32.dll", SetLastError = true)]
        public static extern bool WriteFile(IntPtr hFile, byte[] lpBuffer, int nNumberOfBytesToWrite, out int lpNumberOfBytesWritten, IntPtr lpOverlapped);

        [DllImport("kernel32.dll", SetLastError = true)]
        public static extern bool WriteFile(IntPtr hFile, IntPtr lpBuffer, int nNumberOfBytesToWrite, out int lpNumberOfBytesWritten, [In] ref System.Threading.NativeOverlapped lpOverlapped);

        [DllImport("kernel32.dll", SetLastError = true)]
        public static extern bool WriteFile(IntPtr hFile, IntPtr lpBuffer, int nNumberOfBytesToWrite, out int lpNumberOfBytesWritten, IntPtr lpOverlapped);

        [DllImport("advapi32.dll", EntryPoint = "InitiateSystemShutdown")]
        public static extern int InitiateSystemShutdown(string lpMachineName, string lpMessage, int dwTimeout, int bForceAppsClosed, int bRebootAfterShutdown);

        [DllImport("kernel32.dll", SetLastError = true)]
        public static extern bool ReadFile(IntPtr hFile, IntPtr lpBuffer, uint nNumberOfBytesToRead, out uint lpNumberOfBytesRead, [In] ref System.Threading.NativeOverlapped lpOverlapped);

        [DllImport("kernel32.dll", SetLastError = true)]
        public static extern bool WriteFile(IntPtr hFile, IntPtr lpBuffer, uint nNumberOfBytesToWrite, out uint lpNumberOfBytesWritten, [In] ref System.Threading.NativeOverlapped lpOverlapped);

        [DllImport("kernel32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool CloseHandle(IntPtr hObject);

        [StructLayout(LayoutKind.Sequential)]
        public struct SYSTEM_INFO
        {
            public uint dwOemId;
            public uint dwPageSize;
            public uint lpMinimumApplicationAddress;
            public uint lpMaximumApplicationAddress;
            public uint dwActiveProcessorMask;
            public uint dwNumberOfProcessors;
            public uint dwProcessorType;
            public uint dwAllocationGranularity;
            public uint dwProcessorLevel;
            public uint dwProcessorRevision;
        }

        [StructLayout(LayoutKind.Sequential)]
        public struct WIN32_FILE_ATTRIBUTE_DATA
        {
            public FileAttributes dwFileAttributes;
            public System.Runtime.InteropServices.ComTypes.FILETIME ftCreationTime;
            public System.Runtime.InteropServices.ComTypes.FILETIME ftLastAccessTime;
            public System.Runtime.InteropServices.ComTypes.FILETIME ftLastWriteTime;
            public uint nFileSizeHigh;
            public uint nFileSizeLow;
        }

        public enum GET_FILEEX_INFO_LEVELS
        {
            GetFileExInfoStandard,
            GetFileExMaxInfoLevel
        }

        [DllImport("kernel32.dll", SetLastError = true, CharSet = CharSet.Auto)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool GetFileAttributesEx(string lpFileName, GET_FILEEX_INFO_LEVELS fInfoLevelId, out WIN32_FILE_ATTRIBUTE_DATA fileData);

        [DllImport("kernel32.dll", SetLastError = true)]
        public static extern void GetSystemInfo(out SYSTEM_INFO lpSystemInfo);

        [DllImport("kernel32.dll", SetLastError = true)]
        public static extern bool GetFileSizeEx(SafeFileHandle hFile, out long lpFileSize);

        [DllImport("kernel32.dll", SetLastError = true)]
        public static extern bool GetFileSizeEx(IntPtr hFile, out long lpFileSize);

        [DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)]
        public static extern uint GetFileAttributes(string fileName);

        public enum FileInformationClass : int
        {
            FileBasicInfo = 0,
            FileStandardInfo = 1,
            FileNameInfo = 2,
            FileRenameInfo = 3,
            FileDispositionInfo = 4,
            FileAllocationInfo = 5,
            FileEndOfFileInfo = 6,
            FileStreamInfo = 7,
            FileCompressionInfo = 8,
            FileAttributeTagInfo = 9,
            FileIdBothDirectoryInfo = 10, // 0xA
            FileIdBothDirectoryRestartInfo = 11, // 0xB
            FileIoPriorityHintInfo = 12, // 0xC
            FileRemoteProtocolInfo = 13, // 0xD
            FileFullDirectoryInfo = 14, // 0xE
            FileFullDirectoryRestartInfo = 15, // 0xF
            FileStorageInfo = 16, // 0x10
            FileAlignmentInfo = 17, // 0x11
            FileIdInfo = 18, // 0x12
            FileIdExtdDirectoryInfo = 19, // 0x13
            FileIdExtdDirectoryRestartInfo = 20, // 0x14
        }

        [StructLayout(LayoutKind.Sequential)]
        public struct FILE_ALLOCATION_INFO
        {
            public long AllocationSize;
        }

        [StructLayout(LayoutKind.Sequential)]
        public struct FILE_END_OF_FILE_INFO
        {
            public long EndOfFile;
        }

        [StructLayout(LayoutKind.Explicit)]
        public struct FileInformation
        {
            [FieldOffset(0)]
            public FILE_ALLOCATION_INFO AllocationInfo;
            [FieldOffset(0)]
            public FILE_END_OF_FILE_INFO EndOfFileInfo;
        }

        [DllImport("Kernel32.dll", SetLastError = true)]
        public static extern bool SetFileInformationByHandle(IntPtr hFile, FileInformationClass FileInformationClass, ref FileInformation FileInformation, Int32 dwBufferSize);

        [StructLayout(LayoutKind.Sequential, Pack = 4)]
        public struct BY_HANDLE_FILE_INFORMATION
        {
            public uint dwFileAttributes;
            public System.Runtime.InteropServices.ComTypes.FILETIME ftCreationTime;
            public System.Runtime.InteropServices.ComTypes.FILETIME ftLastAccessTime;
            public System.Runtime.InteropServices.ComTypes.FILETIME ftLastWriteTime;
            public uint dwVolumeSerialNumber;
            public uint nFileSizeHigh;
            public uint nFileSizeLow;
            public uint nNumberOfLinks;
            public uint nFileIndexHigh;
            public uint nFileIndexLow;
        };

        [DllImport("Kernel32.dll", SetLastError = true)]
        public static extern bool GetFileInformationByHandle(IntPtr hFile, out BY_HANDLE_FILE_INFORMATION fileInformation);

        [StructLayout(LayoutKind.Sequential, Pack = 0)]
        public struct IO_STATUS_BLOCK
        {
            public uint status;
            public IntPtr information;
        }

        public enum FILE_INFORMATION_CLASS
        {
            FileDirectoryInformation = 1,
            FileFullDirectoryInformation = 2,
            FileBothDirectoryInformation = 3,
            FileBasicInformation = 4,
            FileStandardInformation = 5,
            FileInternalInformation = 6,
            FileEaInformation = 7,
            FileAccessInformation = 8,
            FileNameInformation = 9,
            FileRenameInformation = 10,
            FileLinkInformation = 11,
            FileNamesInformation = 12,
            FileDispositionInformation = 13,
            FilePositionInformation = 14,
            FileFullEaInformation = 15,
            FileModeInformation = 16,
            FileAlignmentInformation = 17,
            FileAllInformation = 18,
            FileAllocationInformation = 19,
            FileEndOfFileInformation = 20,
            FileAlternateNameInformation = 21,
            FileStreamInformation = 22,
            FilePipeInformation = 23,
            FilePipeLocalInformation = 24,
            FilePipeRemoteInformation = 25,
            FileMailslotQueryInformation = 26,
            FileMailslotSetInformation = 27,
            FileCompressionInformation = 28,
            FileObjectIdInformation = 29,
            FileCompletionInformation = 30,
            FileMoveClusterInformation = 31,
            FileQuotaInformation = 32,
            FileReparsePointInformation = 33,
            FileNetworkOpenInformation = 34,
            FileAttributeTagInformation = 35,
            FileTrackingInformation = 36,
            FileIdBothDirectoryInformation = 37,
            FileIdFullDirectoryInformation = 38,
            FileValidDataLengthInformation = 39,
            FileShortNameInformation = 40,
            FileHardLinkInformation = 46,
            FileDispositionInformationEx = 64,
        }

        [DllImport("ntdll.dll")]
        public static extern int NtQueryInformationFile(IntPtr fileHandle, ref IO_STATUS_BLOCK IoStatusBlock, IntPtr pInfoBlock, int length, FILE_INFORMATION_CLASS fileInformation);

        [DllImport("ntdll.dll")]
        public static extern int NtSetInformationFile(IntPtr fileHandle, ref IO_STATUS_BLOCK IoStatusBlock, IntPtr pInfoBlock, int length, FILE_INFORMATION_CLASS fileInformation);

        [StructLayout(LayoutKind.Sequential, Pack = 4)]
        public struct FILE_BASIC_INFORMATION
        {
            public long CreationTime;
            public long LastAccessTime;
            public long LastWriteTime;
            public long ChangeTime;
            public uint FileAttributes;
        }

        [StructLayout(LayoutKind.Sequential, Pack = 4)]
        public struct FILE_STANDARD_INFORMATION
        {
            public long AllocationSize;
            public long EndOfFile;
            public uint NumberOfLinks;
            public byte DeletePending;
            public byte Directory;
        }

        [StructLayout(LayoutKind.Sequential, Pack = 4)]
        public struct FILE_END_OF_FILE_INFORMATION
        {
            public long EndOfFile;
        }

        [StructLayout(LayoutKind.Sequential, Pack = 4)]
        public struct FILE_ALLOCATION_INFORMATION
        {
            public long AllocationSize;
        }

        //
        // Summary:
        //     {Buffer Overflow} The data was too large to fit into the specified buffer.
        public const int STATUS_BUFFER_OVERFLOW = unchecked((int)2147483653);
        //
        // Summary:
        //     An invalid parameter was passed to a service or function.
        public const int STATUS_INVALID_PARAMETER = unchecked((int)3221225485);

        [DllImport("ntdll.dll")]
        public static extern int NtQuerySecurityObject(IntPtr FileHandle, int SecurityInformation, IntPtr SecurityDescriptor, int length, out int lenNeeded);

        [DllImport("ntdll.dll")]
        public static extern int NtSetSecurityObject(IntPtr FileHandle, int SecurityInformation, IntPtr SecurityDescriptor);

        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
        public struct FILE_RENAME_INFORMATION
        {
            public int    Flags;
            public IntPtr RootDirectory;
            //The size of FileName in bytes, not including the NUL-termination.
            public uint   FileNameLength;
            [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 512)]
            public string FileName;
        }

        public static long ROUND_UP_OFFSET(long Value, uint Pow2)
        {
            return (Int64)((Value + (long)Pow2 - 1) & ~(Pow2 - 1));
        }

        public static uint ROUND_UP_SIZE(uint Value, uint Pow2)
        {
            return (Value + (uint)Pow2 - 1) & ~(Pow2 - 1);
        }
    }

    #endregion
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