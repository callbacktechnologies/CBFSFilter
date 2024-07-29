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
using System.Text;
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
    class regmonDemo
    {
        private const int ERROR_SUCCESS = 0;
        private const int ERROR_FILE_NOT_FOUND = 2;
        private const int ERROR_ACCESS_DENIED = 5;
        private const int ERROR_INSUFFICIENT_BUFFER = 122;
        private const int ERROR_MORE_DATA = 234;
        private const int ERROR_NO_MORE_ITEMS = 259;
        private const int ERROR_REPARSE = 741;
        private const int ERROR_PRIVILEGE_NOT_HELD = 1314;

        private const string mHKLM = @"\Registry\Machine\";
        private const string mHKU = @"\Registry\User\";
        private static readonly string mHKCU = @"\Registry\User\{0}\";
        private static readonly string mHKCR = @"\Registry\User\{0}_Classes\";

        private const string mProductGuid = "{713CC6CE-B3E2-4fd9-838D-E28F558F6866}";

        private static string mCabFileLocation = null;
        private static bool mDriverRunning = false;
        private static bool mFilterWorking = false;

        private static ulong mTicksStarted;
        private static CBRegistry mFilter;
        private static long mLogEntries;

        public regmonDemo()
        {
            mLogEntries = 0;

            mFilter = CreateFilter();
            CheckDriver(mFilter);
            DisposeFilter(ref mFilter);
        }

        private class KeyContext
        {
            public string Name;
            public int Access;
            public string NewName;
            public DateTime LastWriteTime;
            public string ValueName;
            public int ValueType = 0;
            public long IntegerValue = 0;
            public string StringValue = null;
            public IntPtr BinaryValue = IntPtr.Zero;
            public int BinaryValueSize = 0;

            public void ClearValue()
            {
                ValueName = null;
                ValueType = 0;
                IntegerValue = 0;
                StringValue = null;
                BinaryValue = IntPtr.Zero;
                BinaryValueSize = 0;
            }
        }

        [DllImport("kernel32.dll", EntryPoint = "GetTickCount64")]
        static extern ulong GetTickCount();

        [DllImport("advapi32.dll", EntryPoint = "InitiateSystemShutdown")]
        static extern int InitiateSystemShutdown(string MachineName, string Message, int Timeout, int ForceAppsClosed, int RebootAfterShutdown);

        static regmonDemo()
        {
            mHKCU = String.Format(mHKCU, Environment.UserName);
            mHKCR = String.Format(mHKCR, Environment.UserName);

        }

        private void Install(string fileName)
        {
            bool rebootNeeded = false;

            mCabFileLocation = Path.GetDirectoryName(fileName);

            using (var filter = CreateFilter())
            {
                try
                {
                    Console.WriteLine($"Installing the driver from '{fileName}'");
                    rebootNeeded = filter.Install(fileName, mProductGuid, null, Constants.INSTALL_REMOVE_OLD_VERSIONS, null);
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
            bool rebootNeeded = false;

            mCabFileLocation = Path.GetDirectoryName(fileName);

            using (var filter = CreateFilter())
            {
                try
                {
                    rebootNeeded = filter.Uninstall(fileName, mProductGuid, null, Constants.UNINSTALL_VERSION_ALL);
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

        private static void AskForReboot()
        {
            Console.WriteLine("System restart is needed in order to install the drivers. Reboot now? (Input 'Yes' to reboot or anything else to decline)");
            string inputValue = Console.ReadLine();
            if (!String.IsNullOrEmpty(inputValue) && inputValue.Equals("Yes"))
                InitiateSystemShutdown("", "", 10000, 0, 1);
        }

        private CBRegistry CreateFilter()
        {
            var filter = new CBRegistry();
            filter.OnCleanupKeyContext += Filter_OnCleanupUserContext;

            filter.OnBeforeCreateKey += Filter_OnBeforeCreateKey;
            filter.OnBeforeOpenKey += Filter_OnBeforeOpenKey;
            filter.OnBeforeRenameKey += Filter_OnBeforeRenameKey;
            filter.OnBeforeSetKey += Filter_OnBeforeSetKey;

            filter.OnAfterCloseKey += Filter_OnAfterCloseKey;
            filter.OnAfterCreateKey += Filter_OnAfterCreateKey;
            filter.OnAfterDeleteKey += Filter_OnAfterDeleteKey;
            filter.OnAfterEnumerateKey += Filter_OnAfterEnumerateKey;
            filter.OnAfterOpenKey += Filter_OnAfterOpenKey;
            filter.OnAfterQueryKey += Filter_OnAfterQueryKey;
            filter.OnAfterRenameKey += Filter_OnAfterRenameKey;
            filter.OnAfterSetKey += Filter_OnAfterSetKey;

            filter.OnBeforeDeleteValue += Filter_OnBeforeDeleteValue;
            filter.OnBeforeSetValue += Filter_OnBeforeSetValue;

            filter.OnAfterDeleteValue += Filter_OnAfterDeleteValue;
            filter.OnAfterEnumerateValue += Filter_OnAfterEnumerateValue;
            filter.OnAfterQueryValue += Filter_OnAfterQueryValue;
            filter.OnAfterSetValue += Filter_OnAfterSetValue;

            return filter;
        }

        private void CheckDriver(CBRegistry filter = null, bool writeInfo = true)
        {
            string message;
            try
            {
                if (filter == null)
                {
                    filter = CreateFilter();
                }
                var status = (DriverStatus)filter.GetDriverStatus(mProductGuid);
                mDriverRunning = (status == DriverStatus.Running);

                if (status == DriverStatus.NotInstalled)
                    message = "Driver is not installed";
                else
                {
                    var version = filter.GetDriverVersion(mProductGuid);
                    var major = version >> 48;
                    var minor = (version >> 32) & 0xffff;
                    var release = (version >> 16) & 0xffff;
                    var build = version & 0xffff;
                    message = string.Format("Driver is installed (ver {0}.{1}.{2}.{3}), service is {4}",
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
                mDriverRunning = false;
            }
        }

        private void SetFilter(string processName)
        {
            if (String.IsNullOrEmpty(processName))
            {
                Console.WriteLine("No process specified for filtering");
                return;
            }
            mLogEntries = 0;

            mTicksStarted = GetTickCount();
            mFilter = CreateFilter();
            try
            {
                mFilter.Initialize(mProductGuid);
                mFilter.SerializeEvents = CBRegistrySerializeEvents.seOnOneWorkerThread;
                mFilter.StartFilter(30000);
                mFilter.AddFilteredProcessByName(processName, false);
            }
            catch (CBFSFilterException err)
            {
                DisposeFilter(ref mFilter);
                Console.WriteLine(String.Format("Filter not started.\n{0}", err.Message));
                return;
            }

            mFilterWorking = true;
        }

        private void DeleteFilter()
        {
            if (mFilter == null)
            {
                return;
            }
            try
            {
                mFilter.StopFilter();
                mFilter.Dispose();
                mFilter = null;
            }
            catch (Exception ex)
            {

            }

            mFilterWorking = false;
        }

        private static void DisposeFilter(ref CBRegistry filter)
        {
            if (filter == null)
                return;

            filter.StopFilter();
            filter.Dispose();
            filter = null;
        }

        private static string ResultToStr(int result)
        {
            switch (result)
            {
                case ERROR_SUCCESS:
                    return "SUCCESS";

                case ERROR_FILE_NOT_FOUND:
                    return "NOT FOUND";

                case ERROR_INSUFFICIENT_BUFFER:
                    return "SMALL BUFFER";

                case ERROR_MORE_DATA:
                    return "MORE DATA";

                case ERROR_REPARSE:
                    return "REPARSE";

                default:
                    return result.ToString();
            }
        }

        private string NtKeyNameToWinApiKeyName(string keyName)
        {
            if (String.IsNullOrEmpty(keyName))
                return String.Empty;

            if (mHKLM.StartsWith(keyName, StringComparison.CurrentCultureIgnoreCase))
                return "HKLM";

            if (mHKCU.StartsWith(keyName, StringComparison.CurrentCultureIgnoreCase))
                return "HKCU";

            if (mHKCR.StartsWith(keyName, StringComparison.CurrentCultureIgnoreCase))
                return "HKCR";

            if (mHKU.StartsWith(keyName, StringComparison.CurrentCultureIgnoreCase))
                return "HKU";

            if (keyName.StartsWith(mHKLM, StringComparison.CurrentCultureIgnoreCase))
                return @"HKLM\" + keyName.Substring(mHKLM.Length);

            if (keyName.StartsWith(mHKCU, StringComparison.CurrentCultureIgnoreCase))
                return @"HKCU\" + keyName.Substring(mHKCU.Length);

            if (keyName.StartsWith(mHKCR, StringComparison.CurrentCultureIgnoreCase))
                return @"HKCR\" + keyName.Substring(mHKCR.Length);

            if (keyName.StartsWith(mHKU, StringComparison.CurrentCultureIgnoreCase))
                return @"HKU\" + keyName.Substring(mHKU.Length);

            return keyName;
        }

        private string ValueTypeToStr(int valueType)
        {
            switch (valueType)
            {
                case Constants.REG_VALUETYPE_SZ:
                    return "SZ";

                case Constants.REG_VALUETYPE_EXPAND_SZ:
                    return "EXPAND_SZ";

                case Constants.REG_VALUETYPE_BINARY:
                    return "BINARY";

                case Constants.REG_VALUETYPE_DWORD:
                    return "DWORD";

                case Constants.REG_VALUETYPE_MULTI_SZ:
                    return "MULTI_SZ";

                case Constants.REG_VALUETYPE_QWORD:
                    return "QWORD";

                default:
                    return String.Format("unknown ({0})", valueType);
            }
        }

        private byte[] CopyData(IntPtr ptr, int size)
        {
            var data = new byte[size];
            if (size != 0)
                Marshal.Copy(ptr, data, 0, size);
            return data;
        }

        private string DumpBinaryValue(IntPtr data, int size, int maxBytes)
        {
            if (data == IntPtr.Zero || size == 0)
                return String.Empty;

            var buffer = CopyData(data, Math.Min(size, maxBytes));
            string result = BitConverter.ToString(buffer).Replace('-', ' ');
            if (size > maxBytes)
                result += "...";

            return result;
        }

        private bool IsBitSet(int value, int bit)
        {
            return (value & bit) != 0;
        }

        private IntPtr CreateContext(string keyName, int access)
        {
            var context = new KeyContext();
            context.Name = NtKeyNameToWinApiKeyName(keyName);
            context.Access = access;

            var ptr = GCHandle.ToIntPtr(GCHandle.Alloc(context));
            return ptr;
        }

        private KeyContext GetContext(IntPtr ptr)
        {
            if (ptr == IntPtr.Zero)
                return null;
            return (KeyContext)GCHandle.FromIntPtr(ptr).Target;
        }

        static void Banner()
        {
            Console.WriteLine("CBFS Filter Registry Monitor Demo Copyright (c) 2017-2023, Callback Technologies, Inc.\n");
        }

        static void Usage()
        {
            Console.WriteLine("Usage: regmon [-<switch 1> ... -<switch N>] [<process name whose requests should be filtered>]\n");
            Console.WriteLine("<Switches>");
            Console.WriteLine("  -drv <path to cbregistry.cab> - Install the driver from the specified CAB file\n");
            Console.WriteLine("Example: regmon notepad.exe");
        }

        private static void Log(string operation, string key, int result, string details)
        {
            long number = System.Threading.Interlocked.Increment(ref mLogEntries);
            ulong time = GetTickCount() - mTicksStarted;

            Console.WriteLine($"<LogInfo> Operation: {operation}. Key: {key}. Number {number}. Result: {ResultToStr(result)}. Details: {details}");
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

        #region CBRegistry event handlers

        private void Filter_OnCleanupUserContext(object sender, CBRegistryCleanupKeyContextEventArgs e)
        {
            if (e.KeyContext == IntPtr.Zero)
                return;

            var handle = GCHandle.FromIntPtr(e.KeyContext);
            var context = (KeyContext)handle.Target;
            handle.Free();
        }

        private void Filter_OnBeforeCreateKey(object sender, CBRegistryBeforeCreateKeyEventArgs e)
        {
            e.KeyContext = CreateContext(e.FullName, e.DesiredAccess);
        }

        private void Filter_OnBeforeOpenKey(object sender, CBRegistryBeforeOpenKeyEventArgs e)
        {
            e.KeyContext = CreateContext(e.FullName, e.DesiredAccess);
        }

        private void Filter_OnBeforeRenameKey(object sender, CBRegistryBeforeRenameKeyEventArgs e)
        {
            var context = GetContext(e.KeyContext);
            if (context == null)
                return;
            context.NewName = e.NewName;
        }

        private void Filter_OnBeforeSetKey(object sender, CBRegistryBeforeSetKeyEventArgs e)
        {
            var context = GetContext(e.KeyContext);
            if (context == null)
                return;
            context.LastWriteTime = e.LastWriteTime;
        }

        private void Filter_OnAfterCloseKey(object sender, CBRegistryAfterCloseKeyEventArgs e)
        {
            var context = GetContext(e.KeyContext);
            if (context == null)
                return;

            Log("CloseKey", context.Name, ERROR_SUCCESS, null);
        }

        private void Filter_OnAfterCreateKey(object sender, CBRegistryAfterCreateKeyEventArgs e)
        {
            var context = GetContext(e.KeyContext);
            if (context == null)
                return;

            var details = String.Format("Access: 0x{0:X}", (e.GrantedAccess == 0) ? context.Access : e.GrantedAccess);
            Log("CreateKey", context.Name, e.Status, details);
        }

        private void Filter_OnAfterDeleteKey(object sender, CBRegistryAfterDeleteKeyEventArgs e)
        {
            var context = GetContext(e.KeyContext);
            if (context == null)
                return;

            Log("DeleteKey", context.Name, e.Status, null);
        }

        private void Filter_OnAfterEnumerateKey(object sender, CBRegistryAfterEnumerateKeyEventArgs e)
        {
            if (e.Status == ERROR_NO_MORE_ITEMS)
                return;

            var context = GetContext(e.KeyContext);
            if (context == null)
                return;

            var sb = new StringBuilder();
            if (e.Status == ERROR_SUCCESS)
            {
                sb.Append(e.Index.ToString());
                sb.Append(": ");
                if (IsBitSet(e.ValidFields, Constants.REG_KEYFIELD_NAME))
                    sb.Append(e.Name);
                if (IsBitSet(e.ValidFields, Constants.REG_KEYFIELD_SUBKEYS))
                    sb.AppendFormat(", Subkeys: {0}", e.SubKeys);
                if (IsBitSet(e.ValidFields, Constants.REG_KEYFIELD_VALUES))
                    sb.AppendFormat(", Values: {0}", e.Values);
            }
            Log("EnumKey", context.Name, e.Status, sb.ToString());
        }

        private void Filter_OnAfterOpenKey(object sender, CBRegistryAfterOpenKeyEventArgs e)
        {
            var context = GetContext(e.KeyContext);
            if (context == null)
                return;

            var details = String.Format("Access: 0x{0:X}", (e.GrantedAccess == 0) ? context.Access : e.GrantedAccess);
            Log("OpenKey", context.Name, e.Status, details);
        }

        private void Filter_OnAfterQueryKey(object sender, CBRegistryAfterQueryKeyEventArgs e)
        {
            var context = GetContext(e.KeyContext);
            if (context == null)
                return;

            var sb = new StringBuilder();
            if (e.Status == ERROR_SUCCESS)
            {
                if (IsBitSet(e.ValidFields, Constants.REG_KEYFIELD_SUBKEYS))
                    sb.AppendFormat("Subkeys: {0}", e.SubKeys);
                if (IsBitSet(e.ValidFields, Constants.REG_KEYFIELD_VALUES))
                {
                    if (sb.Length != 0)
                        sb.Append("; ");
                    sb.AppendFormat("Values: {0}", e.Values);
                }
            }
            Log("QueryKey", context.Name, e.Status, sb.ToString());
        }

        private void Filter_OnAfterRenameKey(object sender, CBRegistryAfterRenameKeyEventArgs e)
        {
            var context = GetContext(e.KeyContext);
            if (context == null)
                return;

            var details = String.Format("New name: {0}", context.NewName);
            context.NewName = null;
            Log("RenameKey", context.Name, e.Status, details);
        }

        private void Filter_OnAfterSetKey(object sender, CBRegistryAfterSetKeyEventArgs e)
        {
            var context = GetContext(e.KeyContext);
            if (context == null)
                return;

            var details = context.LastWriteTime.ToOADate() != 0 ? String.Format("Last write time: {0}", context.LastWriteTime) : String.Empty;
            Log("SetKey", context.Name, e.Status, details);
        }

        private void Filter_OnBeforeDeleteValue(object sender, CBRegistryBeforeDeleteValueEventArgs e)
        {
            var context = GetContext(e.KeyContext);
            if (context == null)
                return;
            context.ValueName = e.ValueName;
        }

        private void Filter_OnBeforeSetValue(object sender, CBRegistryBeforeSetValueEventArgs e)
        {
            var context = GetContext(e.KeyContext);
            if (context == null)
                return;
            context.ValueName = e.ValueName;
            context.ValueType = e.ValueType;
            context.IntegerValue = e.IntegerValue;
            context.StringValue = e.StringValue;
            context.BinaryValue = e.BinaryValue;
            context.BinaryValueSize = e.BinaryValueSize;
        }

        private void Filter_OnAfterDeleteValue(object sender, CBRegistryAfterDeleteValueEventArgs e)
        {
            var context = GetContext(e.KeyContext);
            if (context == null)
                return;

            Log("DeleteValue", context.Name + '\\' + context.ValueName, e.Status, null);
            context.ValueName = null;
        }

        private void Filter_OnAfterEnumerateValue(object sender, CBRegistryAfterEnumerateValueEventArgs e)
        {
            if (e.Status == ERROR_NO_MORE_ITEMS)
                return;

            var context = GetContext(e.KeyContext);
            if (context == null)
                return;

            var sb = new StringBuilder();
            sb.Append(e.Index.ToString());
            sb.Append(": ");
            if (IsBitSet(e.ValidFields, Constants.REG_VALUEFIELD_NAME))
                sb.Append(String.IsNullOrEmpty(e.ValueName) ? "(Default)" : e.ValueName);
            if (IsBitSet(e.ValidFields, Constants.REG_VALUEFIELD_TYPE))
            {
                sb.Append("; Type: ");
                sb.Append(ValueTypeToStr(e.ValueType));

                if (IsBitSet(e.ValidFields, Constants.REG_VALUEFIELD_DATA))
                {
                    sb.Append("; Data: ");

                    switch (e.ValueType)
                    {
                        case Constants.REG_VALUETYPE_SZ:
                        case Constants.REG_VALUETYPE_EXPAND_SZ:
                            if (!String.IsNullOrEmpty(e.StringValue))
                                sb.Append(e.StringValue);
                            break;

                        case Constants.REG_VALUETYPE_BINARY:
                            sb.AppendFormat("[{0}] {1}", e.BinaryValueSize, DumpBinaryValue(e.BinaryValue, e.BinaryValueSize, 20));
                            break;

                        case Constants.REG_VALUETYPE_DWORD:
                        case Constants.REG_VALUETYPE_QWORD:
                            sb.Append(e.IntegerValue.ToString());
                            break;

                        case Constants.REG_VALUETYPE_MULTI_SZ:
                            if (!String.IsNullOrEmpty(e.StringValue))
                                sb.Append(e.StringValue.Replace('\u0017', ' '));
                            break;
                    }
                }
            }

            Log("EnumValue", context.Name, e.Status, sb.ToString());
        }

        private void Filter_OnAfterQueryValue(object sender, CBRegistryAfterQueryValueEventArgs e)
        {
            var context = GetContext(e.KeyContext);
            if (context == null)
                return;

            var sb = new StringBuilder();
            if (IsBitSet(e.ValidFields, Constants.REG_VALUEFIELD_TYPE))
            {
                sb.Append("Type: ");
                sb.Append(ValueTypeToStr(e.ValueType));

                if (IsBitSet(e.ValidFields, Constants.REG_VALUEFIELD_DATA))
                {
                    sb.Append("; Data: ");

                    switch (e.ValueType)
                    {
                        case Constants.REG_VALUETYPE_SZ:
                        case Constants.REG_VALUETYPE_EXPAND_SZ:
                            if (!String.IsNullOrEmpty(e.StringValue))
                                sb.Append(e.StringValue);
                            break;

                        case Constants.REG_VALUETYPE_BINARY:
                            sb.AppendFormat("[{0}] {1}", e.BinaryValueSize, DumpBinaryValue(e.BinaryValue, e.BinaryValueSize, 20));
                            break;

                        case Constants.REG_VALUETYPE_DWORD:
                        case Constants.REG_VALUETYPE_QWORD:
                            sb.Append(e.IntegerValue.ToString());
                            break;

                        case Constants.REG_VALUETYPE_MULTI_SZ:
                            if (!String.IsNullOrEmpty(e.StringValue))
                                sb.Append(e.StringValue.Replace('\u0017', ' '));
                            break;
                    }
                }
            }

            Log("QueryValue", context.Name + '\\' + (String.IsNullOrEmpty(e.ValueName) ? "(Default)" : e.ValueName),
                e.Status, sb.ToString());
        }

        private void Filter_OnAfterSetValue(object sender, CBRegistryAfterSetValueEventArgs e)
        {
            var context = GetContext(e.KeyContext);
            if (context == null)
                return;

            var sb = new StringBuilder();
            if (context.ValueType != 0)
            {
                sb.Append("Type: ");
                sb.Append(ValueTypeToStr(context.ValueType));
                sb.Append("; Data: ");

                switch (context.ValueType)
                {
                    case Constants.REG_VALUETYPE_SZ:
                    case Constants.REG_VALUETYPE_EXPAND_SZ:
                        if (!String.IsNullOrEmpty(context.StringValue))
                            sb.Append(context.StringValue);
                        break;

                    case Constants.REG_VALUETYPE_BINARY:
                        sb.AppendFormat("[{0}] {1}", context.BinaryValueSize, DumpBinaryValue(context.BinaryValue, context.BinaryValueSize, 20));
                        break;

                    case Constants.REG_VALUETYPE_DWORD:
                    case Constants.REG_VALUETYPE_QWORD:
                        sb.Append(context.IntegerValue.ToString());
                        break;

                    case Constants.REG_VALUETYPE_MULTI_SZ:
                        if (!String.IsNullOrEmpty(context.StringValue))
                            sb.Append(context.StringValue.Replace('\u0017', ' '));
                        break;
                }
            }

            Log("SetValue", context.Name + '\\' + (String.IsNullOrEmpty(context.ValueName) ? "(Default)" : context.ValueName),
                e.Status, sb.ToString());
            context.ClearValue();
        }

        #endregion

        static void Main(string[] args)
        {
            bool drvReboot = false;
            bool optTerminate = false;

            string processName = null;

            int argi, argLen;
            bool stop_opt = false;

            Banner();

            try
            {
                regmonDemo regMonitor = new regmonDemo();
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
                                arg = ConvertRelativePathToAbsolute(args[++argi]);
                                if (String.IsNullOrEmpty(arg)){
                                    Console.WriteLine("Invalid Driver Path");
                                    return;
                                }
                                if (argi < args.Length)
                                {
                                    regMonitor.Install(arg);
                                }
                            }
                            else
                                Console.WriteLine("Error: Invalid option " + arg);
                        }
                        else
                        {
                            // if we have not set process name, do this now.
                            processName = arg.ToString();
                            break;

                        }
                    }
                }

                // Probably, we have just installed the driver, so we can quit without monitoring anything
                if (String.IsNullOrEmpty(processName))
                    return;

                Console.WriteLine("Press any key to start monitoring, then press any key to stop and quit.");

                Console.ReadKey();

                try
                {

                    regMonitor.SetFilter(processName);
                    Console.WriteLine("Press any key to stop monitoring and quit.");
                    Console.ReadKey();
                    regMonitor.DeleteFilter();
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