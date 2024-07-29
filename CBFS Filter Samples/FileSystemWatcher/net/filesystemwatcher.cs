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

using System;
using System.ComponentModel.Design;
using System.ComponentModel;
using System.Runtime.InteropServices;

using callback.CBFSFilter;

namespace callback.Demos
{
    class filesystemwatcherDemo
    {
        [DllImport("advapi32.dll", EntryPoint = "InitiateSystemShutdown")]
        private static extern int InitiateSystemShutdown(string lpMachineName, string lpMessage, int dwTimeout, int bForceAppsClosed, int bRebootAfterShutdown);

        [DllImport("kernel32.dll", SetLastError = true)]
        static extern bool CloseHandle(IntPtr hHandle);

        private static CBFilter mFilter = new CBFilter();
        private static FileSystemWatcher mFSWatcher = new FileSystemWatcher();

        private const string mGuid = "{713CC6CE-B3E2-4fd9-838D-E28F558F6866}";
        private const uint ERROR_ACCESS_DENIED = 5;
        private const uint ERROR_PRIVILEGE_NOT_HELD = 1314;
        private const string ALTITUDE_FAKE_VALUE_FOR_DEBUG = "360000.24";

        static void Banner()
        {
            Console.WriteLine("CBFS Filter File System Watcher Demo Copyright (c) 2017-2024, Callback Technologies, Inc.\n\n");
        }

        static void Usage()
        {
            Console.WriteLine("Usage: filesystemwatcher [-<switch 1> ... -<switch N>] [<path to monitor> [<filename mask to monitor>]]\n");
            Console.WriteLine("<Switches>");
            Console.WriteLine("  -drv <path to cbfilter.cab> - Install the driver from the specified CAB file");
            Console.WriteLine("  -undrv <path to cbfilter.cab> - Uninstall the driver from the specified CAB file");
            Console.WriteLine("  -- Stop switches scanning\n");
        }

        private static void Install(string fileName)
        {
            bool Reboot = false;
            try
            {
                Console.WriteLine($"Installing the driver from '{fileName}'");
                Reboot = mFilter.Install(fileName, mGuid, null, ALTITUDE_FAKE_VALUE_FOR_DEBUG, 0, null);//INSTALL_REMOVE_OLD_VERSIONS);

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
                Reboot = mFilter.Uninstall(fileName, mGuid, null, 0);

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

        private static void SetFilter(string path, string mask)
        {
            mFSWatcher.Changed += FSWatcher_Changed;
            mFSWatcher.Created += FSWatcher_Created;
            mFSWatcher.Deleted += FSWatcher_Deleted;
            mFSWatcher.Error += FSWatcher_Error;
            mFSWatcher.Renamed += FSWatcher_Renamed;

            mFSWatcher.Guid = mGuid;
            mFSWatcher.Path = path;
            mFSWatcher.Filter = mask;
            mFSWatcher.IncludeSubdirectories = true;

            mFSWatcher.EnableRaisingEvents = true;
        }

        private static void FSWatcher_Renamed(object sender, RenamedEventArgs e)
        {
            AddToLog((FileSystemWatcher)sender, "Renamed", string.Format("{0} -> {1}", e.OldFullPath, e.FullPath));
        }

        private static void FSWatcher_Error(object sender, ErrorEventArgs e)
        {
            AddToLog((FileSystemWatcher)sender, "Error", e.GetException().ToString());
        }

        private static void FSWatcher_Deleted(object sender, FileSystemEventArgs e)
        {
            AddToLog((FileSystemWatcher)sender, "Deleted", e.FullPath);
        }

        private static void FSWatcher_Created(object sender, FileSystemEventArgs e)
        {
            AddToLog((FileSystemWatcher)sender, "Created", e.FullPath);
        }

        private static void FSWatcher_Changed(object sender, FileSystemEventArgs e)
        {
            AddToLog((FileSystemWatcher)sender, "Changed", e.FullPath);
        }

        private static void DeleteFilter()
        {
            mFSWatcher.EnableRaisingEvents = false;
        }

        private static bool CheckDriver(bool writeInfo = true)
        {
            uint versionHigh, versionLow;
            int moduleStatus;
            ulong moduleVersion;

            moduleStatus = mFilter.GetDriverStatus(mGuid);
            moduleVersion = (ulong)mFilter.GetDriverVersion(mGuid);

            versionHigh = (uint)(moduleVersion >> 32);
            versionLow = (uint)(moduleVersion & 0xFFFFFFFF);

            string driverInfo;
            bool res = moduleStatus != 0;

            if (moduleStatus != 0)
            {
                driverInfo = string.Format("Driver version: {0}.{1}.{2}.{3}", versionHigh >> 16, versionHigh & 0xFFFF, versionLow >> 16, versionLow & 0xFFFF);
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

        private static void AddToLog(FileSystemWatcher Watcher, string Operation, string FileName)
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

            String message = $"<LogInfo> Operation: {Operation}, FilePath: {FileName}, ProcessName: {ProcessName}, ProcessID: {ProcessID}";
            Console.WriteLine(message);
        }

        private static void AskForReboot()
        {
            Console.WriteLine("System restart is needed in order to install the drivers. Reboot now? (Input 'Yes' to reboot or anything else to decline)");
            string inputValue = Console.ReadLine();
            if (!String.IsNullOrEmpty(inputValue) && inputValue.Equals("Yes"))
                InitiateSystemShutdown("", "", 10000, 0, 1);
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
            string FilenameMask = "*.*";

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
                        bool install = String.Equals(arg, "-drv", StringComparison.OrdinalIgnoreCase);

                        if (install || String.Equals(arg, "-undrv", StringComparison.OrdinalIgnoreCase))
                        {
                            arg = ConvertRelativePathToAbsolute(args[++argi]);
                            if (String.IsNullOrEmpty(arg))
                            {
                                Console.WriteLine("Invalid Driver Path");
                                return;
                            }
                            if (argi < args.Length)
                            {
                                if (install)
                                    Install(arg);
                                else
                                    Uninstall(arg);
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
                            if (String.IsNullOrEmpty(PathToMonitor))
                            {
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

            try
            {
                SetFilter(PathToMonitor, FilenameMask);

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

    public struct WaitForChangedResult
    {
        private WatcherChangeTypes mChangeType;
        private string mName;
        private string mOldName;
        private bool mTimedOut;

        internal static readonly WaitForChangedResult TimedOutResult = new WaitForChangedResult(0, null, true);

        internal WaitForChangedResult(WatcherChangeTypes changeType, string name, bool timedOut)
            : this(changeType, name, null, timedOut)
        {
        }

        internal WaitForChangedResult(WatcherChangeTypes changeType, string name, string oldName, bool timedOut)
        {
            this.mChangeType = changeType;
            this.mName = name;
            this.mOldName = oldName;
            this.mTimedOut = timedOut;
        }

        public WatcherChangeTypes ChangeType
        {
            get { return mChangeType; }
            set { mChangeType = value; }
        }

        public string Name
        {
            get { return mName; }
            set { mName = value; }
        }

        public string OldName
        {
            get { return mOldName; }
            set { mOldName = value; }
        }

        public bool TimedOut
        {
            get { return mTimedOut; }
            set { mTimedOut = value; }
        }
    }

    public class FileSystemWatcher : Component, ISupportInitialize
    {
        private string m_Guid = "";

        private CBFilter m_CBFilter = null;
        private string m_Directory;
        private string m_Filter;
        private bool m_Enabled = false;
        private bool m_Initializing = false;
        private bool m_StopListening = false;
        // Flag to watch subtree of this directory
        private bool m_IncludeSubdirectories = false;

        // Used for synchronization
        private WaitForChangedResult m_ChangedResult;
        private bool m_IsChanged = false;
        private ISynchronizeInvoke m_SynchronizingObject;
        private bool disposed;

        // Current "session" ID to ignore old events whenever we stop then
        // restart. 
        private int m_CurrentSession;

        private static int m_NotifyFiltersValidMask = 0;

        private const NotifyFilters m_DefaultNotifyFilters = NotifyFilters.LastWrite | NotifyFilters.FileName | NotifyFilters.DirectoryName;
        private const long m_DefaultNotifyFlags = callback.CBFSFilter.Constants.FS_NE_RENAME |
                                                     callback.CBFSFilter.Constants.FS_NE_SET_SIZES |
                                                     callback.CBFSFilter.Constants.FS_NE_WRITE |
                                                     callback.CBFSFilter.Constants.FS_NE_SET_SECURITY;

        private const long m_DefaultCallbacksFlags = callback.CBFSFilter.Constants.FS_CE_AFTER_CREATE |
                                                     callback.CBFSFilter.Constants.FS_CE_AFTER_OPEN |
                                                     callback.CBFSFilter.Constants.FS_CE_AFTER_SET_ATTRIBUTES |
                                                     callback.CBFSFilter.Constants.FS_CE_AFTER_DELETE;


        private NotifyFilters m_NotifyFilters = m_DefaultNotifyFilters;
        private long m_NotifyFlags = m_DefaultNotifyFlags;
        private long m_CallbackFlags = m_DefaultCallbacksFlags;

        //private static uint INVALID_FILE_ATTRIBUTES = 0xFFFFFFFF;
        private static readonly char[] m_Wildcards = new char[] { '?', '*' };

        //
        // Event handlers
        //
        private FileSystemEventHandler m_OnChangedHandler = null;
        private FileSystemEventHandler m_OnCreatedHandler = null;
        private FileSystemEventHandler m_OnDeletedHandler = null;
        private RenamedEventHandler m_OnRenamedHandler = null;
        private ErrorEventHandler m_OnErrorHandler = null;

        //private IntPtr m_iHandle = IntPtr.Zero + 1;


        [DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)]
        static extern uint GetFileAttributes(string lpFileName);

        private bool SkipProcessing(object Sender)
        {
            if (m_StopListening)
            {
                return true;
            }
            // Ignore any events that occurred before this "session",
            // so we don't get changed or error events after we
            // told FSW to stop.
            if ((int)((CBFilter)Sender).Tag != m_CurrentSession)
                return true;

            return false;
        }

        #region CFBSNotifyEvents
        /*======================================================================*/
        /*==   private notify events                                          ==*/
        /*======================================================================*/

        private void CBFSFltAfterCreateFile(
            object Sender,
            CBFilterAfterCreateFileEventArgs args
            )
        {
            if (SkipProcessing(Sender)) return;

            /**
             * this code can be used to check the opening / closing of files
             **/
            /*  IntPtr handleValue = HandleContext;
                lock (this)
                {
                    if (handleValue == IntPtr.Zero)
                    {
                        handleValue = m_iHandle;
                        m_iHandle += 1;
                    }
                }

                HandleContext = handleValue;

                Console.WriteLine("Create: " + FileName + " context 0x" + fileContext.ToString("X") + " handle 0x" + handleValue.ToString("X"));
            */
            try
            {
                if (0 == args.Status)
                {

                    FileContext fsContext = null;
                    if (args.FileContext != IntPtr.Zero)
                    {
                        fsContext = FileContext.Reference(args.FileContext);
                    }
                    else
                    {
                        IntPtr ctx = args.FileContext;
                        fsContext = FileContext.Alloc(ref ctx);
                        args.FileContext = ctx;
                    }
                    fsContext.Attributes = args.Attributes;

                    if ((args.Options & 0x04000000) == 0x04000000) /* FILE_FLAG_DELETE_ON_CLOSE */
                    {
                        fsContext.DeleteOnClose = true;
                    }

                    if (((((int)m_NotifyFilters & (int)NotifyFilters.DirectoryName) != 0) && fsContext.IsDirectory) || ((((int)m_NotifyFilters & (int)NotifyFilters.FileName) != 0) && !fsContext.IsDirectory))
                    {
                        OnCreated(new FileSystemEventArgs(WatcherChangeTypes.Created, InternalGetDirectoryName(args.FileName), System.IO.Path.GetFileName(args.FileName)));
                    }
                }
            }
            catch (Exception /*ex*/)
            {
                args.ResultCode = 1064; /*  ERROR_EXCEPTION_IN_SERVICE */
            }
        }

        private void CBFSFltAfterOpenFile(
            object Sender,
            CBFilterAfterOpenFileEventArgs args)
        {
            if (SkipProcessing(Sender))
                return;

            try
            {

                /**
                * this code can be used to check the opening / closing of files
                **/
                /*
                    IntPtr handleValue = HandleContext;
                    lock (this)
                    {
                        if (handleValue == IntPtr.Zero)
                        {
                            handleValue = m_iHandle;
                            m_iHandle += 1;
                }
                    }

                    HandleContext = handleValue;

                    Console.WriteLine("Open: " + FileName + " context 0x" + fileContext.ToString("X") + " handle 0x" + handleValue.ToString("X"));
                */

                FileContext fsContext = null;
                if (args.FileContext != IntPtr.Zero)
                {
                    fsContext = FileContext.Reference(args.FileContext);
                }
                else
                {
                    IntPtr ctx = args.FileContext;
                    fsContext = FileContext.Alloc(ref ctx);
                    args.FileContext = ctx;
                }

                if (fsContext != null)
                {
                    fsContext.Attributes = args.Attributes;
                    if ((args.Options & 0x04000000) == 0x04000000) /* FILE_FLAG_DELETE_ON_CLOSE */
                    {
                        fsContext.DeleteOnClose = true;
                    }
                }

                if (((Disposition.CREATE_ALWAYS == args.CreateDisposition)) && (args.Status == 0))
                {
                    if (((((int)m_NotifyFilters & (int)NotifyFilters.DirectoryName) != 0) && fsContext.IsDirectory) || ((((int)m_NotifyFilters & (int)NotifyFilters.FileName) != 0) && !fsContext.IsDirectory))
                        OnCreated(new FileSystemEventArgs(WatcherChangeTypes.Created, InternalGetDirectoryName(args.FileName), System.IO.Path.GetFileName(args.FileName)));
                }
            }
            catch (Exception /*ex*/)
            {
                args.ResultCode = 1064; /*  ERROR_EXCEPTION_IN_SERVICE */
            }

        }

        private void CBFSFltAfterDeleteFile(object Sender, CBFilterAfterDeleteFileEventArgs args)
        {
            if (SkipProcessing(Sender))
                return;

            bool isDir = false;

            FileContext fsContext = null;

            if (args.FileContext != IntPtr.Zero)
            {
                fsContext = FileContext.Reference(args.FileContext);
                isDir = fsContext.IsDirectory;
                bool deleted = false;
                fsContext.Dereference(args.FileContext, out deleted);
            }

            if ((isDir == false) && (((int)m_NotifyFilters & (int)NotifyFilters.FileName) != 0) ||
                (isDir == true) && (((int)m_NotifyFilters & (int)NotifyFilters.DirectoryName) != 0))
            {
                this.OnDeleted(
                  new FileSystemEventArgs(
                    WatcherChangeTypes.Deleted,
                    InternalGetDirectoryName(args.FileName),
                    System.IO.Path.GetFileName(args.FileName)));
            }
        }

        private void CBFSFltNotifySetAllocationSize(
            object Sender,
            CBFilterNotifySetAllocationSizeEventArgs args
            /*string FileName, Int64 AllocationSize, Int32 Status*/)
        {
            if (SkipProcessing(Sender)) return;

            if ((0 == args.Status) &&
                (((int)m_NotifyFilters & (int)NotifyFilters.Size) != 0))
            {
                OnChanged(
                        new FileSystemEventArgs(
                        WatcherChangeTypes.Changed,
                            InternalGetDirectoryName(args.FileName),
                            System.IO.Path.GetFileName(args.FileName)));
            }
        }

        private void CBFSFltNotifySetFileSize(object Sender,
            CBFilterNotifySetFileSizeEventArgs args
            /*string FileName, Int64 Size, Int32 Status*/)
        {
            if (SkipProcessing(Sender)) return;

            if ((0 == args.Status) &&
                (((int)m_NotifyFilters & (int)NotifyFilters.Size) != 0))
            {
                OnChanged(
                    new FileSystemEventArgs(
                        WatcherChangeTypes.Changed,
                        InternalGetDirectoryName(args.FileName),
                        System.IO.Path.GetFileName(args.FileName)));
            }
        }

        private void CBFSFltAfterSetFileAttributes(object Sender,
            CBFilterAfterSetFileAttributesEventArgs args)
        {
            if (SkipProcessing(Sender)) return;
            if (0 == args.Status)
            {
                FileContext fsContext = FileContext.Reference(args.FileContext);
                if (args.Attributes != fsContext.Attributes && (((int)m_NotifyFilters & (int)NotifyFilters.Attributes) != 0))
                {
                    OnChanged(
                        new FileSystemEventArgs(
                            WatcherChangeTypes.Changed,
                            InternalGetDirectoryName(args.FileName),
                            System.IO.Path.GetFileName(args.FileName)));
                    fsContext.Attributes = args.Attributes;
                }
                fsContext.Dereference(args.FileContext, out _);
                if (args.CreationTime != DateTime.FromFileTimeUtc(0) && (((int)m_NotifyFilters & (int)NotifyFilters.CreationTime) != 0))
                    OnChanged(
                        new FileSystemEventArgs(
                            WatcherChangeTypes.Changed,
                            InternalGetDirectoryName(args.FileName),
                            System.IO.Path.GetFileName(args.FileName)));
                if (args.LastWriteTime != DateTime.FromFileTimeUtc(0) && (((int)m_NotifyFilters & (int)NotifyFilters.LastWrite) != 0))
                    OnChanged(
                        new FileSystemEventArgs(
                            WatcherChangeTypes.Changed,
                            InternalGetDirectoryName(args.FileName),
                            System.IO.Path.GetFileName(args.FileName)));
                if (args.LastAccessTime != DateTime.FromFileTimeUtc(0) && (((int)m_NotifyFilters & (int)NotifyFilters.LastAccess) != 0))
                    OnChanged(
                        new FileSystemEventArgs(
                            WatcherChangeTypes.Changed,
                            InternalGetDirectoryName(args.FileName),
                            System.IO.Path.GetFileName(args.FileName)));
            }
        }

        private void CBFSFltNotifyRenameOrMoveFile(object Sender,
            CBFilterNotifyRenameOrMoveFileEventArgs args)
        {
            if (SkipProcessing(Sender)) return;

            uint attr = GetFileAttributes(args.NewFileName);
            bool dirFile = ((attr & 0x10) == 0x10);// FILE_DIRECTORY_FILE

            // If the file is renamed to another directory, we need to split the event into delete and create events.
            if ((0 == args.Status) &&
                (((((int)m_NotifyFilters & (int)NotifyFilters.DirectoryName) != 0) && dirFile) || ((((int)m_NotifyFilters & (int)NotifyFilters.FileName) != 0) && !dirFile)))
            {
                string oldDirName = InternalGetDirectoryName(args.FileName);
                string newDirName = InternalGetDirectoryName(args.NewFileName);
                if (string.Compare(oldDirName, newDirName, true) == 0)
                {
                    OnRenamed(
                        new RenamedEventArgs(
                            WatcherChangeTypes.Renamed,
                            oldDirName,
                            System.IO.Path.GetFileName(args.NewFileName),
                            System.IO.Path.GetFileName(args.FileName)));
                }
                else
                {
                    if ((Sender as CBFilter).IsFileFiltered(args.FileName))
                        OnDeleted(
                            new FileSystemEventArgs(
                                WatcherChangeTypes.Deleted,
                                oldDirName,
                                System.IO.Path.GetFileName(args.FileName)));
                    if ((Sender as CBFilter).IsFileFiltered(args.NewFileName))
                        OnCreated(
                            new FileSystemEventArgs(
                                WatcherChangeTypes.Created,
                                newDirName,
                                System.IO.Path.GetFileName(args.NewFileName)));
                }
            }
        }

        private void CBFSFltNotifyWriteFile(object Sender,
            CBFilterNotifyWriteFileEventArgs args)
        {
            if (SkipProcessing(Sender)) return;

            if ((0 == args.Status) &&
                (args.Direction == callback.CBFSFilter.Constants.FS_REQUEST_DIR_USER_CACHED) &&
                (((int)m_NotifyFilters & (int)NotifyFilters.LastWrite) != 0))
            {
                OnChanged(
                    new FileSystemEventArgs(
                        WatcherChangeTypes.Changed,
                        InternalGetDirectoryName(args.FileName),
                        System.IO.Path.GetFileName(args.FileName)));
            }
        }

        void CBFSFltNotifySetFileSecurity(object Sender,
            CBFilterNotifySetFileSecurityEventArgs args)
        {
            if (SkipProcessing(Sender)) return;

            if ((0 == args.Status) &&
                (((int)m_NotifyFilters & (int)NotifyFilters.Security) != 0))
            {
                OnChanged(
                    new FileSystemEventArgs(
                        WatcherChangeTypes.Changed,
                        InternalGetDirectoryName(args.FileName),
                        System.IO.Path.GetFileName(args.FileName)));
            }
        }

        #endregion  // CFBSNotifyEvents

        /// <summary>Initializes a new instance of the <see cref="T:Callback.CBFSFilter.FileSystemWatcher.FileSystemWatcher" /> class.</summary>
        public FileSystemWatcher()
        {
            m_Directory = String.Empty;
            m_Filter = "*.*";

            disposed = false;
        }

        /// <summary>Initializes a new instance of the <see cref="T:Callback.CBFSFilter.FileSystemWatcher.FileSystemWatcher" /> class, given the specified directory to monitor.</summary>
        /// <param name="path">The directory to monitor, in standard or Universal Naming Convention (UNC) notation. </param>
        /// <exception cref="T:System.ArgumentNullException">The <paramref name="path" /> parameter is null. </exception>
        /// <exception cref="T:System.ArgumentException">The <paramref name="path" /> parameter is empty string ("").-or- The path specified through the <paramref name="path" /> parameter does not exist. </exception>
        public FileSystemWatcher(string path) : this(path, "*.*")
        {
        }

        /// <summary>Initializes a new instance of the <see cref="T:Callback.CBFSFilter.FileSystemWatcher.FileSystemWatcher" /> class, given the specified directory and type of files to monitor.</summary>
        /// <param name="path">The directory to monitor, in standard or Universal Naming Convention (UNC) notation. </param>
        /// <param name="filter">The type of files to watch. For example, "*.txt" watches for changes to all text files. </param>
        /// <exception cref="T:System.ArgumentNullException">The <paramref name="path" /> parameter is null.-or- The <paramref name="filter" /> parameter is null. </exception>
        /// <exception cref="T:System.ArgumentException">The <paramref name="path" /> parameter is empty string ("").-or- The path specified through the <paramref name="path" /> parameter does not exist. </exception>
        public FileSystemWatcher(string path, string filter)
        {

            if (null == path)
                throw new ArgumentNullException("path");

            if (null == filter)
                throw new ArgumentNullException("filter");

            if (0 == path.Length || !Directory.Exists(path))
                throw new ArgumentException(string.Format("Invalid directory name ({0})", path));

            disposed = false;

            m_Directory = path;
            m_Filter = filter;
        }

        static FileSystemWatcher()
        {
            foreach (int enumValue in Enum.GetValues(typeof(NotifyFilters)))
                m_NotifyFiltersValidMask |= enumValue;


        }

        /// <summary>
        /// Gets or sets a ProductGUID, with which CBFS Filter drivers were installed
        /// This GUID is used to initialize the internal CBFilter component
        /// </summary>
        /// [DefaultValue("")]
        public string Guid
        {
            get
            {
                return m_Guid;
            }
            set
            {
                m_Guid = value;
            }
        }


        /// <summary>Gets or sets a value indicating whether the component is enabled.</summary>
        /// <returns>true if the component is enabled; otherwise, false. The default is false. If you are using the component on a designer in Visual Studio 2005, the default is true.</returns>
        /// <exception cref="T:System.ObjectDisposedException">The <see cref="T:Callback.CBFSFilter.FileSystemWatcher.FileSystemWatcher" /> object has been disposed.</exception>
        /// <exception cref="T:System.PlatformNotSupportedException">The current operating system is not Microsoft Windows NT or later.</exception>
        /// <exception cref="T:System.IO.FileNotFoundException">The directory specified in <see cref="P:Callback.CBFSFilter.FileSystemWatcher.FileSystemWatcher.Path" /> could not be found.</exception>
        /// <exception cref="T:System.ArgumentException">
        ///   <see cref="P:Callback.CBFSFilter.FileSystemWatcher.FileSystemWatcher.Path" /> has not been set or is invalid.</exception>
        /// <filterpriority>2</filterpriority>
        [DefaultValue(false)]
        public bool EnableRaisingEvents
        {
            get
            {
                return m_Enabled;
            }

            set
            {
                if (m_Enabled == value)
                {
                    return;
                }

                m_Enabled = value;

                if (!IsSuspended())
                {
                    if (m_Enabled)
                    {
                        StartRaisingEvents();
                        return;
                    }

                    StopRaisingEvents();

                }
            }
        }


        [DefaultValue(m_DefaultNotifyFilters)]
        public NotifyFilters NotifyFilter
        {
            get
            {
                return m_NotifyFilters;
            }

            set
            {
                if (((int)value & ~m_NotifyFiltersValidMask) != 0)
                    throw new InvalidEnumArgumentException("value", (int)value, typeof(NotifyFilters));

                if (m_NotifyFilters != value)
                {
                    m_NotifyFilters = value;

                    Restart();
                }
            }
        }

        /// <summary>Gets or sets a value indicating whether subdirectories within the specified path should be monitored.</summary>
        /// <returns>true if you want to monitor subdirectories; otherwise, false. The default is false.</returns>
        /// <filterpriority>2</filterpriority>
        [DefaultValue(false)]
        public bool IncludeSubdirectories
        {
            get
            {
                return m_IncludeSubdirectories;
            }

            set
            {
                if (m_IncludeSubdirectories != value)
                {
                    m_IncludeSubdirectories = value;

                    Restart();
                }
            }
        }

        /// <summary>Gets or sets the filter string used to determine what files are monitored in a directory.</summary>
        /// <returns>The filter string. The default is "*.*" (Watches all files.) </returns>
        /// <filterpriority>2</filterpriority>
        [DefaultValue("*.*"), SettingsBindable(true)]
        public string Filter
        {
            get
            {
                return m_Filter;
            }

            set
            {
                if (string.IsNullOrEmpty(value))
                {
                    value = "*.*";
                }
                if (String.Compare(m_Filter, value, StringComparison.OrdinalIgnoreCase) != 0)
                {
                    m_Filter = value;
                }
            }
        }

        /// <summary>Gets or sets the path of the directory to watch.</summary>
        /// <returns>The path to monitor. The default is empty string ("").</returns>
        /// <exception cref="T:System.ArgumentException">The specified path does not exist or could not be found.-or- The specified path contains wildcard characters.-or- The specified path contains invalid path characters.</exception>
        /// <filterpriority>2</filterpriority>
        [DefaultValue(""),]
        public string Path
        {
            get
            {
                return m_Directory;
            }
            set
            {
                value = ((value == null) ? string.Empty : value);

                if (String.Compare(m_Directory, value, StringComparison.OrdinalIgnoreCase) != 0)
                {
                    if (!Directory.Exists(value))
                        throw new ArgumentException(string.Format("Invalid directory name: {0}", value));

                    m_Directory = value;
                    Restart();
                }
            }
        }

        /// <summary>Gets or sets an <see cref="T:System.ComponentModel.ISite" /> for the <see cref="T:Callback.CBFSFilter.FileSystemWatcher.FileSystemWatcher" />.</summary>
        /// <returns>An <see cref="T:System.ComponentModel.ISite" /> for the <see cref="T:Callback.CBFSFilter.FileSystemWatcher.FileSystemWatcher" />.</returns>
        /// <filterpriority>2</filterpriority>
        [Browsable(false)]
        public override ISite Site
        {
            get
            {
                return base.Site;
            }
            set
            {
                base.Site = value;

                // set EnableRaisingEvents to true at design time so the user
                // doesn't have to manually. We can't do this in 
                // the constructor because in code it should
                // default to false. 
                if (Site != null && this.Site.DesignMode)
                {
                    EnableRaisingEvents = true;
                }
            }
        }

        [
        Browsable(false),
        DefaultValue(null)
        ]
        public ISynchronizeInvoke SynchronizingObject
        {
            get
            {
                if (this.m_SynchronizingObject == null && base.DesignMode)
                {
                    IDesignerHost designerHost = (IDesignerHost)this.GetService(typeof(IDesignerHost));
                    if (designerHost != null)
                    {
                        object rootComponent = designerHost.RootComponent;
                        if (rootComponent != null && rootComponent is ISynchronizeInvoke)
                        {
                            this.m_SynchronizingObject = (ISynchronizeInvoke)rootComponent;
                        }
                    }
                }
                return this.m_SynchronizingObject;
            }

            set
            {
                this.m_SynchronizingObject = value;
            }
        }

        #region Public methods
        public string GetOriginatorProcessName()
        {
            return m_CBFilter.GetOriginatorProcessName();
        }

        public int GetOriginatorProcessId()
        {
            return m_CBFilter.GetOriginatorProcessId();
        }

        public long GetOriginatorToken()
        {
            return m_CBFilter.GetOriginatorToken();
        }

        public int GetOriginatorThreadId()
        {
            return m_CBFilter.GetOriginatorThreadId();
        }

        #endregion

        /// <summary>Begins the initialization of a <see cref="T:Callback.CBFSFilter.FileSystemWatcher.FileSystemWatcher" /> used on a form or used by another component. The initialization occurs at run time.</summary>
        /// <filterpriority>2</filterpriority>
        public void BeginInit()
        {
            bool oldEnabled = m_Enabled;
            StopRaisingEvents();
            m_Enabled = oldEnabled;
            m_Initializing = true;
        }

        public void EndInit()
        {
            m_Initializing = false;
            // Unless user told us NOT to start after initialization, we'll start listening 
            // to events
            if (m_Directory.Length != 0 && m_Enabled == true)
                StartRaisingEvents();
        }

        private bool IsSuspended()
        {
            return m_Initializing;
        }

        #region Public events

        public event FileSystemEventHandler Changed
        {
            add
            {
                m_OnChangedHandler += value;
            }
            remove
            {
                m_OnChangedHandler -= value;
            }
        }

        public event FileSystemEventHandler Created
        {
            add
            {
                m_OnCreatedHandler += value;
            }
            remove
            {
                m_OnCreatedHandler -= value;
            }
        }

        public event FileSystemEventHandler Deleted
        {
            add
            {
                m_OnDeletedHandler += value;
            }
            remove
            {
                m_OnDeletedHandler -= value;
            }
        }

        [Browsable(false)]
        public event ErrorEventHandler Error
        {
            add
            {
                m_OnErrorHandler += value;
            }
            remove
            {
                m_OnErrorHandler -= value;
            }
        }

        public event RenamedEventHandler Renamed
        {
            add
            {
                m_OnRenamedHandler += value;
            }
            remove
            {
                m_OnRenamedHandler -= value;
            }
        }

        #endregion

        #region Event firing methods 
        protected void OnChanged(FileSystemEventArgs e)
        {
            // To avoid race between remove handler and raising the event 
            FileSystemEventHandler changedHandler = m_OnChangedHandler;

            if (changedHandler != null)
            {
                if (this.SynchronizingObject != null && this.SynchronizingObject.InvokeRequired)
                    this.SynchronizingObject.BeginInvoke(changedHandler, new object[] { this, e });
                else
                    changedHandler(this, e);
            }
        }

        protected void OnCreated(FileSystemEventArgs e)
        {
            FileSystemEventHandler createdHandler = m_OnCreatedHandler;
            if (createdHandler != null)
            {
                if (this.SynchronizingObject != null && this.SynchronizingObject.InvokeRequired)
                    this.SynchronizingObject.BeginInvoke(createdHandler, new object[] { this, e });
                else
                    createdHandler(this, e);
            }
        }

        protected void OnDeleted(FileSystemEventArgs e)
        {
            FileSystemEventHandler deletedHandler = m_OnDeletedHandler;

            if (deletedHandler != null)
            {
                if (this.SynchronizingObject != null && this.SynchronizingObject.InvokeRequired)
                    this.SynchronizingObject.BeginInvoke(deletedHandler, new object[] { this, e });
                else
                    deletedHandler(this, e);
            }
        }

        protected void OnError(ErrorEventArgs e)
        {
            ErrorEventHandler errorHandler = m_OnErrorHandler;

            if (errorHandler != null)
            {
                if (this.SynchronizingObject != null && this.SynchronizingObject.InvokeRequired)
                    this.SynchronizingObject.BeginInvoke(errorHandler, new object[] { this, e });
                else
                    errorHandler(this, e);
            }
        }

        protected void OnRenamed(RenamedEventArgs e)
        {
            RenamedEventHandler renamedHandler = m_OnRenamedHandler;

            if (renamedHandler != null)
            {
                if (this.SynchronizingObject != null && this.SynchronizingObject.InvokeRequired)
                    this.SynchronizingObject.BeginInvoke(renamedHandler, new object[] { this, e });
                else
                    renamedHandler(this, e);
            }
        }
        #endregion

        protected override void Dispose(bool disposing)
        {
            try
            {

                if (disposing)
                {
                    //Stop raising events cleans up managed and
                    //unmanaged resources. 
                    StopRaisingEvents();

                    // Clean up managed resources
                    m_OnChangedHandler = null;
                    m_OnCreatedHandler = null;
                    m_OnDeletedHandler = null;
                    m_OnRenamedHandler = null;
                    m_OnErrorHandler = null;
                }
                else
                {
                    m_StopListening = true;
                    this.disposed = true;
                    // Clean up unmanaged resources
                }

            }
            finally
            {
                base.Dispose(disposing);
            }
        }

        private void Restart()
        {
            if ((!IsSuspended()) && m_Enabled)
            {
                StopRaisingEvents();
                StartRaisingEvents();
            }
        }

        private void StartRaisingEvents()
        {
            if (this.disposed)
                throw new ObjectDisposedException(GetType().Name);

            if (Environment.OSVersion.Platform != PlatformID.Win32NT)
            {
                throw new PlatformNotSupportedException("Windows NT required");
            }

            // If we're called when "Initializing" is true, set enabled to true 
            if (IsSuspended())
            {
                m_Enabled = true;
                return;
            }

            // If we're attached, don't do anything.
            if (m_CBFilter != null && m_CBFilter.Active)
            {
                return;
            }

            //
            // combine directory and filter name
            //
            string lFilter = string.IsNullOrEmpty(m_Filter) ? "*.*" : m_Filter;
            string ioFilterPath = System.IO.Path.Combine(m_Directory, lFilter);

            m_CBFilter = new CBFilter();

            m_CBFilter.Initialize(Guid);


            //
            // synchronous events
            //
            m_CBFilter.OnAfterCreateFile += CBFSFltAfterCreateFile;
            m_CBFilter.OnAfterOpenFile += CBFSFltAfterOpenFile;
            m_CBFilter.OnAfterDeleteFile += CBFSFltAfterDeleteFile;
            m_CBFilter.OnAfterSetFileAttributes += CBFSFltAfterSetFileAttributes;

            //
            // asynchronous events
            //
            m_CBFilter.OnNotifySetAllocationSize += CBFSFltNotifySetAllocationSize;
            m_CBFilter.OnNotifySetFileSize += CBFSFltNotifySetFileSize;
            m_CBFilter.OnNotifyRenameOrMoveFile += CBFSFltNotifyRenameOrMoveFile;
            m_CBFilter.OnNotifyWriteFile += CBFSFltNotifyWriteFile;
            m_CBFilter.OnNotifySetFileSecurity += CBFSFltNotifySetFileSecurity;

            m_StopListening = false;

            // Start ignoring all events that were initiated before this.
            Interlocked.Increment(ref m_CurrentSession);
            m_CBFilter.Tag = m_CurrentSession;

            m_CBFilter.AddFilterRule(ioFilterPath, callback.CBFSFilter.Constants.ACCESS_NONE, m_CallbackFlags, m_NotifyFlags);

            if (!m_IncludeSubdirectories)
            {
                m_CBFilter.AddPassthroughRule(System.IO.Path.Combine(InternalGetDirectoryName(ioFilterPath), @"\*\?*"), callback.CBFSFilter.Constants.ACCESS_NONE, m_CallbackFlags, m_NotifyFlags);
            }

            m_Enabled = true;

            // start callback mechanism
            m_CBFilter.ProcessFailedRequests = true;
            m_CBFilter.Config("AllowFileAccessInBeforeOpen=false");
            m_CBFilter.FileFlushingBehavior = 0;
            m_CBFilter.ProcessCachedIORequests = true;

            m_CBFilter.StartFilter(0);
        }

        private string InternalGetDirectoryName(string dirName)
        {
            dirName = ((dirName == null) ? string.Empty : dirName);

            string directoryName = "";
            if (dirName.Length > 0)                                  // input path is null
            {
                if ((dirName.Length == 1) && (dirName[0] == '\\'))   // input path is a root dirrectory
                {
                    directoryName = "\\";
                }

                directoryName = System.IO.Path.GetDirectoryName(dirName);
            }

            return directoryName;
        }

        private void StopRaisingEvents()
        {
            if (IsSuspended())
            {
                m_Enabled = false;
                return;
            }

            // If we're not attached, do nothing.
            if (m_CBFilter == null || !m_CBFilter.Active)
            {
                return;
            }

            m_StopListening = true;
            m_CBFilter.StopFilter();

            m_CBFilter.Dispose();
            m_CBFilter = null;

            // Start ignoring all events occurring after this.
            Interlocked.Increment(ref m_CurrentSession);

            // Set enabled to false 
            m_Enabled = false;
        }

        //Internal method used for synchronous notification.
        private void OnInternalFileSystemEventArgs(object sender, FileSystemEventArgs e)
        {
            bool acquiredLock = false;
            try
            {
                Monitor.Enter(this, ref acquiredLock);
                // Only change the state of the changed result if it doesn't contain a previous one. 
                if (m_IsChanged != true)
                {
                    m_ChangedResult = new WaitForChangedResult(e.ChangeType, e.Name, false);
                    m_IsChanged = true;
                    Monitor.Pulse(this);
                }
            }
            finally
            {
                if (acquiredLock)
                {
                    Monitor.Exit(this);
                }
            }
        }

        //Internal method used for synchronous notification. 
        private void OnInternalRenameEventArgs(object sender, RenamedEventArgs e)
        {
            bool acquiredLock = false;
            try
            {
                Monitor.Enter(this, ref acquiredLock);
                if (m_IsChanged != true)
                {
                    m_ChangedResult = new WaitForChangedResult(e.ChangeType, e.Name, e.OldName, false);
                    m_IsChanged = true;
                    Monitor.Pulse(this);
                }
            }
            finally
            {
                if (acquiredLock)
                {
                    Monitor.Exit(this);
                }
            }

        }
        /// <summary>A synchronous method that returns a structure that contains specific information on the change that occurred, given the type of change you want to monitor.</summary>
        /// <returns>A <see cref="T:FileSystemWatcher.WaitForChangedResult" /> that contains specific information on the change that occurred.</returns>
        /// <param name="changeType">The <see cref="T:FileSystemWatcher.WatcherChangeTypes" /> to watch for. </param>
        /// <filterpriority>2</filterpriority>
        public WaitForChangedResult WaitForChanged(WatcherChangeTypes changeType)
        {
            return WaitForChanged(changeType, -1);
        }

        public WaitForChangedResult WaitForChanged(WatcherChangeTypes changeType, int timeout)
        {
            FileSystemEventHandler dirHandler = new FileSystemEventHandler(this.OnInternalFileSystemEventArgs);
            RenamedEventHandler renameHandler = new RenamedEventHandler(this.OnInternalRenameEventArgs);

            this.m_IsChanged = false;
            this.m_ChangedResult = WaitForChangedResult.TimedOutResult;

            // Register the internal event handler from the given change types. 
            if ((changeType & WatcherChangeTypes.Created) != 0)
            {
                this.Created += dirHandler;
            }
            if ((changeType & WatcherChangeTypes.Deleted) != 0)
            {
                this.Deleted += dirHandler;
            }
            if ((changeType & WatcherChangeTypes.Changed) != 0)
            {
                this.Changed += dirHandler;
            }
            if ((changeType & WatcherChangeTypes.Renamed) != 0)
            {
                this.Renamed += renameHandler;
            }

            bool savedEnabled = EnableRaisingEvents;
            if (savedEnabled == false)
            {
                EnableRaisingEvents = true;
            }

            WaitForChangedResult retVal = WaitForChangedResult.TimedOutResult;
            lock (this)
            {
                if (timeout == -1)
                {
                    while (!m_IsChanged)
                    {
                        Monitor.Wait(this);
                    }
                }
                else
                {
                    Monitor.Wait(this, timeout, true);
                }

                retVal = m_ChangedResult;
            }

            EnableRaisingEvents = savedEnabled;

            if ((changeType & WatcherChangeTypes.Created) != 0)
            {
                this.Created -= dirHandler;
            }
            if ((changeType & WatcherChangeTypes.Deleted) != 0)
            {
                this.Deleted -= dirHandler;
            }
            if ((changeType & WatcherChangeTypes.Changed) != 0)
            {
                this.Changed -= dirHandler;
            }
            if ((changeType & WatcherChangeTypes.Renamed) != 0)
            {
                this.Renamed -= renameHandler;
            }

            // Return the struct. 
            return retVal;
        }


        //
        // since we are dealing with a filter, the handles we saved are possible and not deleted. 
        // The situation can be represented as follows: the file was opened, but not closed before the filter was turned off.
        //
        private class FileContext
        {
            private long m_bRefCount;

            public bool DeleteOnClose { get; set; }
            public int Attributes { get; set; }
            public bool IsDirectory { get => (Attributes & 0x10) != 0; }

            private FileContext()
            {
                m_bRefCount = 1;
            }

            static public FileContext Alloc(ref IntPtr ptr)
            {
                ptr = IntPtr.Zero;

                FileContext fileContext = new FileContext();

                GCHandle handle = GCHandle.Alloc(fileContext);

                ptr = GCHandle.ToIntPtr(handle);

                if (ptr == IntPtr.Zero)
                    return null;

                return fileContext;
            }

            static public FileContext Reference(IntPtr ptr)
            {
                FileContext fileContext = (FileContext)
                    GCHandle.FromIntPtr(ptr).Target;

                if (fileContext != null)
                {
                    Interlocked.Increment(ref fileContext.m_bRefCount);
                }

                return fileContext;
            }

            public void Dereference(IntPtr ptr, out bool deleted)
            {
                deleted = false;

                long previos = Interlocked.Decrement(ref m_bRefCount);

                if (previos < 1)
                {
                    deleted = true;
                    GCHandle.FromIntPtr(ptr).Free();
                }
            }

        }
    }

    internal static class Disposition
    {
        public const int CREATE_NEW = 1;
        public const int CREATE_ALWAYS = 2;
        public const int OPEN_EXISTING = 3;
        public const int OPEN_ALWAYS = 4;
        public const int TRUNCATE_EXISTING = 5;
    }

    internal static class Direct
    {
        // All possible file dispositon flags


        // All possible notifications flags 
        public const int FILE_NOTIFY_CHANGE_FILE_NAME = 0x00000001;
        public const int FILE_NOTIFY_CHANGE_DIR_NAME = 0x00000002;
        public const int FILE_NOTIFY_CHANGE_NAME = 0x00000003;
        public const int FILE_NOTIFY_CHANGE_ATTRIBUTES = 0x00000004;
        public const int FILE_NOTIFY_CHANGE_SIZE = 0x00000008;
        public const int FILE_NOTIFY_CHANGE_LAST_WRITE = 0x00000010;
        public const int FILE_NOTIFY_CHANGE_LAST_ACCESS = 0x00000020;
        public const int FILE_NOTIFY_CHANGE_CREATION = 0x00000040;
        public const int FILE_NOTIFY_CHANGE_SECURITY = 0x00000100;
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