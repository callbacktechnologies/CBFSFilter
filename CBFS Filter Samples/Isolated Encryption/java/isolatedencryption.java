/*
 * CBFS Filter 2024 Java Edition - Sample Project
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

import java.io.*;
import java.io.*;
import java.io.File;
import java.nio.*;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;

import cbfsfilter.*;

public class isolatedencryption implements cbfsfilter.CBFilterEventListener {

    private static final String mGuid = "{713CC6CE-B3E2-4fd9-838D-E28F558F6866}";
    private static final String ALTITUDE_FAKE_VALUE_FOR_DEBUG = "149995.24";

    private static final String Password = "isolatedencryptionDemo";

    private static int DEFAULT_CLUSTER_SIZE = 4096; // this value is specific to the system configuration and must be obtained on initialization

    private static final int ERROR_NOT_SUPPORTED = 50;

    private CBFilter mFilter = new CBFilter();

    private String[] whitelistProcs = new String[0];

    private boolean Logging = false;

    public isolatedencryption() {

        try {
            mFilter.addCBFilterEventListener(this);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }

    // ----------------------------------------------------------------

    public static void main(String[] args) {

        String pathToIsolate = null;
        String whitelistProcs = null;
        boolean logging = false;

        int argi, argLen;
        boolean stopOpt = false;

        banner();

        try {

            if (args.length < 2) {
                usage();
                return;
            }

            for (argi = 0; argi < args.length; argi++) {
                String arg = args[argi];
                argLen = arg.length();
                if (argLen > 0) {
                    if ((arg.charAt(0) == '-') && !stopOpt) {
                        if (arg.equalsIgnoreCase("-drv")) {
                            arg = ConvertRelativePathToAbsolute(args[++argi]);
                            if (isNullOrEmpty(arg)) {
                                System.out.println("Invalid Driver Path");
                                return;
                            }
                            if (argi < args.length) {
                                install(arg);
                            }
                        }
                        else if (arg.equalsIgnoreCase("-log")) {
                            logging = true;
                        }
                        else if (arg.equals("--")) {
                            stopOpt = true;
                        }
                        else {
                            System.out.println("Error: Invalid option " + arg);
                        }
                    } else {
                        // if we have not set the path yet, do this now. Otherwise, set the mask
                        if (pathToIsolate == null) {
                            pathToIsolate = ConvertRelativePathToAbsolute(arg);
                            if (isNullOrEmpty(pathToIsolate)) {
                                System.out.println("Invalid Filter Path");
                                return;
                            }
                            if (!new File(pathToIsolate).exists()) {
                                System.out.println("ERROR: the specified path '" + pathToIsolate
                                        + "' does not point to an existing directory");
                                return;
                            }
                        }
                        else if (whitelistProcs == null){

                            whitelistProcs = arg;
                            break; // no more parameters expected
                        }
                    }
                }
            }

            if (!checkDriver()) { // driver not installed, so we must quit
                return;
            }

            // Probably, we have just installed the driver, so we can quit without monitoring anything
            if (pathToIsolate == null || whitelistProcs == null)
                return;

            //System.out.println("Press 'Enter' to start monitoring, then press 'Enter' and quit.");

            //System.in.read();

            try {
                isolatedencryption instance = new isolatedencryption();
                instance.start(pathToIsolate, whitelistProcs, logging);
                System.out.println("Press 'Enter' to stop monitoring and quit.");
                System.in.read();
                instance.stop();
            } catch (Exception ex) {
                System.out.println("Error: " + ex.getMessage());
            }
        } catch (Exception e) {
            System.out.println("Error in Main: " + e.getMessage());
        }
    }

    static void banner() {
        System.out.println("CBFS Filter isolatedencryption Demo Copyright (c) 2017-2024, Callback Technologies, Inc.\n\n");
    }

    static void usage() {
        System.out.println("Usage: isolatedencryption [-<switch 1> ... -<switch N>] [<path to isolate>] [<whitelist processes, comma separated>]\n");
        System.out.println("<Switches>");
        System.out.println("  -drv <path to cbfilter.cab> - Install the driver from the specified CAB file");
        System.out.println("  -- Stop switches scanning\n");
    }

    static String ConvertRelativePathToAbsolute(String path) {
        String res = null;
        if (path != null && !path.isEmpty()) {
            res = path;

            // Linux-specific case of using a home directory
            if (path.equals("~") || path.startsWith("~/"))
            {
                String homeDir = System.getProperty("user.home");
                if (path.equals("~"))
                    return homeDir;
                else
                    return homeDir + path.substring(1);
            }
            else
            if (!isDriveLetter(path)) {
                try {
                    Path fullPath = Paths.get(path).toAbsolutePath().normalize();
                    res = fullPath.toString();

                    File file = new File(res);

                    if (res.startsWith("\\\\") && !file.exists()) {
                        System.out.println("The network folder '" + res + "' does not exist.");
                    } else if (!file.exists()) {
                        System.out.println("The path '" + res + "' does not exist.");
                    }
                } catch (Exception ex) {
                    System.out.println(String.format("ConvertRelativePathToAbsolute: exception '%s'", ex.toString()));
                    return null;
                }
            }
        }
        return res;
    }

    static boolean isDriveLetter(String path) {
        if (path == null || path.isEmpty() || path.length() != 2)
            return false;

        char c = path.charAt(0);
        if ((((int)c >= (int) 'A' && (int)c <= (int) 'Z') ||
                ((int)c >= (int) 'a' && (int)c <= (int) 'z')) &&
                (path.charAt(1) == ':'))
            return true;
        else
            return false;
    }

    static boolean isNullOrEmpty(String s) {
        if (s == null)
            return true;
        if (s.length() == 0)
            return true;
        return false;
    }

    static boolean checkDriver() {
        return checkDriver(true);
    }

    static boolean checkDriver(boolean writeInfo)
    {
        try {
            CBFilter filter = new CBFilter();

            int moduleStatus = filter.getDriverStatus(mGuid);
            long moduleVersion = filter.getDriverVersion(mGuid);
            int versionHigh = (int) (moduleVersion >> 32);
            int versionLow = (int) (moduleVersion & 0xFFFFFFFF);

            String driverInfo;
            boolean res = moduleStatus != 0;

            if (moduleStatus != 0) {
                driverInfo = String.format("Driver version: %d.%d.%d.%d",
                        versionHigh >> 16, versionHigh & 0xFFFF, versionLow >> 16, versionLow & 0xFFFF);
            } else {
                driverInfo = "Driver: not installed";
            }

            if (writeInfo) {
                System.out.println(driverInfo);
            }
            return res;
        } catch (Exception ex) {
            System.out.println("Error in checkDriver: " + ex.getMessage());
            return false;
        }

    }

    static void install(String fileName)
    {
        if (!new File(fileName).exists())
            return;

        try
        {
            System.out.println("Installing the driver from " + fileName);

            CBFilter filter = new CBFilter();

            boolean reboot = filter.install(fileName, mGuid, "", ALTITUDE_FAKE_VALUE_FOR_DEBUG, 0, "");

            checkDriver();

            if (reboot)
                System.out.println("Please, reboot the system for the changes to take effect");
            else
                System.out.println("Driver installed successfully");

        }
        catch (CBFSFilterException e)
        {
            if (e.getCode() == 1314/*ERROR_PRIVILEGE_NOT_HELD*/ || e.getCode() == 5/*ERROR_ACCESS_DENIED*/)
                System.out.println("Installation requires administrator rights. Run the app as administrator");
            else
                System.out.println(e.getMessage());
        }
    }

    static void uninstall(String fileName)
    {
        if (!new File(fileName).exists())
            return;
        try
        {
            System.out.println("Uninstalling the driver from " + fileName);

            CBFilter filter = new CBFilter();

            boolean reboot = filter.uninstall(fileName, mGuid, "", 0);

            checkDriver();

            if (reboot)
                System.out.println("Please, reboot the system for the changes to take effect");
            else
                System.out.println("Driver uninstalled successfully");

        }
        catch (CBFSFilterException e)
        {
            if (e.getCode() == 1314/*ERROR_PRIVILEGE_NOT_HELD*/ || e.getCode() == 5/*ERROR_ACCESS_DENIED*/)
                System.out.println("Uninstallation requires administrator rights. Run the app as administrator");
            else
                System.out.println(e.getMessage());
        }
    }

    // ----------------------------------------------------------------

    public void start(String pathToIsolate, String whitelistProcs, boolean logging)
    {
        Logging = logging;

        this.whitelistProcs = whitelistProcs.split(",");

        try {
            mFilter.initialize(mGuid);

            mFilter.addFilterRuleEx(pathToIsolate,
                    "",
                    Constants.ACCESS_NONE,
                    Constants.FS_CE_AFTER_ENUMERATE_DIRECTORY,
                    Constants.FS_NE_NONE,
                    -1, -1, Constants.FILE_SYS_ATTR_DIRECTORY, 0
            );

            mFilter.addFilterRuleEx(pathToIsolate + "\\*.*",
                    "",
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
                    -1, -1, 0, Constants.FILE_SYS_ATTR_DIRECTORY
            );

            mFilter.setProcessCachedIORequests(true);

            mFilter.initialize(mGuid);
            mFilter.config("AllowFileAccessInBeforeOpen=false");
            mFilter.config("ModifiableReadWriteBuffers=true");

            mFilter.startFilter(0);

        } catch (CBFSFilterException e) {
            System.out.println("Error in start: " + e.getMessage());
        }

    }

    public void stop()
    {
        try {
            mFilter.deleteAllFilterRules();
            mFilter.deleteAllPassthroughRules();
            if (mFilter.isActive())
                mFilter.stopFilter();
        }catch (CBFSFilterException e) {
            System.out.println("Error in start: " + e.getMessage());
        }
    }

    private static long ROUND_TO_CLUSTER(long Value)
    {
        return ((Value + DEFAULT_CLUSTER_SIZE - 1) & ~(DEFAULT_CLUSTER_SIZE - 1));
    }

    private String CacheTypeToString(int type)
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
                throw new IllegalArgumentException();
        }
    }

    private void AddToLog(String Value)
    {
        if (!Logging) return;

        System.out.println(Value);
    }

    // ----------------------------------------------------------------

    @Override
    public void beforeCreateFile(CBFilterBeforeCreateFileEvent e) {

        FileContext fileContext;

        String procName = null;
        try {
            procName = mFilter.getOriginatorProcessName();
        } catch (CBFSFilterException e1) {
            e1.printStackTrace();
        }

        FileContext[] fileContextHolder = new FileContext[1];

        e.resultCode = onCreateOrOpenFile(e.fileName, e.desiredAccess, e.shareMode, e.createDisposition, e.options, e.attributes, procName, fileContextHolder);

        fileContext = fileContextHolder[0];

        if (fileContext != null)
        {
            assert(e.resultCode == 0);

            e.isolate = true;
            e.backendFileName = "";

            e.fileContext = Contexts.alloc(fileContext);

            e.processRequest = true;
        }
    }

    @Override
    public void beforeOpenFile(CBFilterBeforeOpenFileEvent e) {

        FileContext fileContext;

        String procName = null;
        try {
            procName = mFilter.getOriginatorProcessName();
        } catch (CBFSFilterException e1) {
            e1.printStackTrace();
        }

        FileContext[] fileContextHolder = new FileContext[1];

        e.resultCode = onCreateOrOpenFile(e.fileName, e.desiredAccess, e.shareMode, e.createDisposition, e.options, e.attributes, procName, fileContextHolder);

        fileContext = fileContextHolder[0];

        if (fileContext != null)
        {
            assert(e.resultCode == 0);

            e.isolate = true;
            e.backendFileName = "";

            e.fileContext = Contexts.alloc(fileContext);

            e.processRequest = true;
        }
    }

    int onCreateOrOpenFile(String fileName, int desiredAccess, int shareMode, int disposition, int options, int attributes, String procName, FileContext[] fileContextHolder)
    {
        int result = 0;
        boolean isolated = false;

        if ((attributes & Constants.FILE_SYS_ATTR_DIRECTORY) != 0)
            return result;

        procName = procName.toLowerCase();
        for (String whitelistProc : whitelistProcs)
        {
            if (procName.endsWith("\\" + whitelistProc.toLowerCase()))
            {
                isolated = true;
                break;
            }
        }

        if (isolated == false)
            return result;

        AddToLog(String.format("BeforeCreate/OpenFile %s %s", procName, fileName));

        FileContext context = new FileContext(mFilter, fileName, Password);

        int intResult = context.initialize(desiredAccess, shareMode, disposition, options | attributes);

        if (intResult == ERROR_NOT_SUPPORTED)
            return result;

        result = intResult;

        if (result == 0)
            fileContextHolder[0] = context;

        return result;
    }

    @Override
    public void beforeQueryFileInfo(CBFilterBeforeQueryFileInfoEvent e) {

        FileContext fileContext;

        if (e.fileContext == 0)
            return;

        AddToLog(String.format("BeforeQueryFileInfo %s", e.fileName));

        fileContext = (FileContext)Contexts.get(e.fileContext);

        int[] bytesWrittenHolder = new int[1];

        e.resultCode = fileContext.onQueryFileInfo(e.buffer, e.bufferLength, e.fileInformationClass, bytesWrittenHolder);

        e.validBytes     = bytesWrittenHolder[0];
        e.processRequest = false;
    }

    @Override
    public void beforeSetFileInfo(CBFilterBeforeSetFileInfoEvent e) {

        FileContext fileContext;

        //
        // Rename operations will be processed in CBFSFltBeforeRenameOrMoveFile
        //

        if (e.fileInformationClass == 10/*FileRenameInformation*/ ||
            e.fileInformationClass == 65/*FileRenameInformationEx*/)
            return;

        if (e.fileContext == 0)
            return;

        AddToLog(String.format("BeforeSetFileInfo %s", e.fileName));

        fileContext = (FileContext)Contexts.get(e.fileContext);

        e.resultCode = fileContext.onSetFileInfo(e.buffer, e.bufferLength, e.fileInformationClass);
        e.processRequest = false;
    }

    @Override
    public void beforeGetFileSecurity(CBFilterBeforeGetFileSecurityEvent e) {

        FileContext fileContext;

        if (e.fileContext == 0)
            return;

        AddToLog(String.format("BeforeGetFileSecurity %s", e.fileName));

        fileContext = (FileContext)Contexts.get(e.fileContext);

        int[] lengthNeededHolder = new int[1];

        e.resultCode = fileContext.onQuerySecurity(e.securityInformation, e.securityDescriptor, e.length, lengthNeededHolder);

        e.lengthNeeded = lengthNeededHolder[0];
        e.processRequest = false;
    }

    @Override
    public void beforeSetFileSecurity(CBFilterBeforeSetFileSecurityEvent e) {

        FileContext fileContext;

        if (e.fileContext == 0)
            return;

        AddToLog(String.format("BeforeSetFileSecurity %s", e.fileName));

        fileContext = (FileContext)Contexts.get(e.fileContext);

        e.resultCode = fileContext.onSetSecurity(e.securityInformation, e.securityDescriptor, e.length);
        e.processRequest = false;
    }

    @Override
    public void beforeReadFile(CBFilterBeforeReadFileEvent e) {

        FileContext fileContext;

        if (e.fileContext == 0)
            return;

        AddToLog(String.format("BeforeReadFile [%s] %s pos(%d) read(%d)", CacheTypeToString(e.direction), e.fileName, e.position, e.bytesToRead));

        fileContext = (FileContext)Contexts.get(e.fileContext);

        if (e.direction == Constants.FS_REQUEST_DIR_USER_CACHED ||
            e.direction == Constants.FS_REQUEST_DIR_SYSTEM_CACHED)
        {

        }
        else
        {
            int[] bytesReadHolder = new int[1];

            e.resultCode = fileContext.onNonCachedRead(e.buffer, e.position, e.bytesToRead, bytesReadHolder);

            e.bytesToRead    = bytesReadHolder[0];
            e.processRequest = false;
        }
    }

    @Override
    public void beforeWriteFile(CBFilterBeforeWriteFileEvent e) {

        FileContext fileContext;

        if (e.fileContext == 0)
            return;

        AddToLog(String.format("BeforeWriteFile [%s] %s Offset(%d) Size(%d)", CacheTypeToString(e.direction), e.fileName, e.position, e.bytesToWrite));

        fileContext = (FileContext)Contexts.get(e.fileContext);

        if (fileContext.getHeaderPresent() == false)
        {
            e.resultCode = fileContext.encryptFile();
            if (e.resultCode != 0)
                return;
        }

        if (e.direction == Constants.FS_REQUEST_DIR_USER_CACHED ||
            e.direction == Constants.FS_REQUEST_DIR_SYSTEM_CACHED)
        {
            if (e.position + e.bytesToWrite > fileContext.getCalcedFileSize())
            {
                e.resultCode = fileContext.onCachedWriteExtendFileSize(e.position + e.bytesToWrite);
            }
        }
        else
        {
            int[] bytesWrittenHolder = new int[1];

            e.resultCode = fileContext.onNonCachedWrite(e.buffer, e.position, e.bytesToWrite, bytesWrittenHolder);

            e.bytesToWrite   = bytesWrittenHolder[0];
            e.processRequest = false;
        }
    }

    @Override
    public void beforeRenameOrMoveFile(CBFilterBeforeRenameOrMoveFileEvent e) {

        FileContext fileContext;

        String fileName = e.fileName;
        String newFileName = e.newFileName;

        AddToLog(String.format("BeforeRenameOrMoveFile %s => %s", fileName, newFileName));

        if (e.fileContext != 0)
        {
            try {
                if (mFilter.isFileFiltered(newFileName) == false)
                {
                    e.resultCode = 17/*ERROR_NOT_SAME_DEVICE*/;
                    return;
                }
            } catch (CBFSFilterException e1) {
                e1.printStackTrace();
            }

            fileContext = (FileContext)Contexts.get(e.fileContext);

            e.resultCode = fileContext.onRenameOrMoveFile(newFileName, e.replaceIfExists);

            e.processRequest = false;
        }
        else
        {
            String procName = null;

            try {
                procName = mFilter.getOriginatorProcessName();
            } catch (CBFSFilterException e1) {
                e1.printStackTrace();
            }

            procName = procName.toLowerCase();

            boolean isWhiteProc = false;
            for (String whitelistProc : whitelistProcs)
            {
                if (procName.endsWith("\\" + whitelistProc.toLowerCase()))
                {
                    isWhiteProc = true;
                    break;
                }
            }

            if (isWhiteProc == true)
            {
                e.resultCode = 17/*ERROR_NOT_SAME_DEVICE*/;
                return;
            }

            //
            // The event is in filter mode at this point and we let the request go forward.
            //

            e.processRequest = true;
        }
    }

    @Override
    public void afterEnumerateDirectory(CBFilterAfterEnumerateDirectoryEvent e) {

        assert(e.directoryContext == 0);

        String procName = null;
        try {
            procName = mFilter.getHandleCreatorProcessName();
        } catch (CBFSFilterException e1) {
            e1.printStackTrace();
        }

        procName = procName.toLowerCase();

        // we presume that all files in directory are filtered and encrypted,
        // but we provide the real size of decrypted data only to those processes that will receive this decrypted data
        for (String whitelistProc : whitelistProcs)
        {
            if (procName.endsWith("\\" + whitelistProc.toLowerCase()))
            {
                if (e.size >= FileHeader.HEADER_SIZE)
                    e.size -= FileHeader.HEADER_SIZE;

                break;
            }
        }
    }

    @Override
    public void cleanupContext(CBFilterCleanupContextEvent e) {

        FileContext fileContext;

        if (e.fileContext != 0)
        {
            fileContext = (FileContext)Contexts.get(e.fileContext);

            AddToLog(String.format("CleanupContext %s", fileContext.getFileName()));

            fileContext.close();

            Contexts.free(e.fileContext);
        }
    }

    // ----------------------------------------------------------------

    class FileContext implements Closeable {

        public final int MASTER_KEY_SIZE = 16;

        private CBFilter filter;
        private String fileName;
        private byte[] masterKey = new byte[MASTER_KEY_SIZE];
        private CBFSFilterStream fileHandle;
        private long fileHandleRaw = -1;
        private long fileSize = 0;
        private FileHeader fileHeader;


        public FileContext(CBFilter filter, String fileName, String password) {
            this.filter = filter;
            this.fileName = fileName;

            try {
                byte[] keyBytes = (password + "abcdefghijklmnopqrst").getBytes("UTF-8");

                System.arraycopy(keyBytes, 0, masterKey, 0, MASTER_KEY_SIZE);

                fileHeader = new FileHeader(masterKey);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

        public String getFileName() {
            return fileName;
        }

        public boolean getHeaderPresent() {
            return fileHeader.getPresent();
        }

        public long getActualFileSize() {
            return fileSize;
        }

        public long getCalcedFileSize() {
            return getHeaderPresent() ? getActualFileSize() - FileHeader.HEADER_SIZE : getActualFileSize();
        }

        public int getCalcedHeaderSize() {
            return  getHeaderPresent() ? FileHeader.HEADER_SIZE : 0;
        }

        public int initialize(int desiredAccess, int shareMode, int creationDisposition, int flagsAndAttributes) {
            int errorCode = 0;
            CBFSFilterStream auxiHandle = null;
            CBFSFilterStream operHandle = null;
            long operHandleRaw = -1;

            do {

                long[] handleHolder = new long[1];

                try {
                    fileHandle = filter.createFileDirectAsStream(fileName, false,
                            desiredAccess, shareMode, creationDisposition, flagsAndAttributes, false, handleHolder);
                    fileHandleRaw = handleHolder[0];
                } catch (CBFSFilterException e) {
                    errorCode = e.getCode();
                    break;
                }

                operHandle = fileHandle;
                operHandleRaw = fileHandleRaw;

                if ((desiredAccess & Constants.DESIRED_ACCESS_FILE_READ_ATTRIBUTES) == 0)
                {
                    try
                    {
                        auxiHandle = filter.createFileDirectAsStream(fileName, false,
                                Constants.DESIRED_ACCESS_FILE_GENERIC_READ,
                                Constants.FILESYS_SHARE_READ | Constants.FILESYS_SHARE_WRITE | Constants.FILESYS_SHARE_DELETE |
                                        Constants.CBFILTER_IGNORE_SHARE_ACCESS_CHECK
                                ,
                                Constants.FILE_DISPOSITION_OPEN_EXISTING, 0x02000000/*FILE_FLAG_BACKUP_SEMANTICS*/,
                                false, handleHolder);

                    }
                    catch (CBFSFilterException e)
                    {
                        errorCode = e.getCode();
                        break;
                    }

                    operHandle = auxiHandle;
                    operHandleRaw = handleHolder[0];
                }

                /*
                    typedef struct _FILE_BASIC_INFORMATION {
                        LARGE_INTEGER CreationTime;
                        LARGE_INTEGER LastAccessTime;
                        LARGE_INTEGER LastWriteTime;
                        LARGE_INTEGER ChangeTime;
                        ULONG FileAttributes;
                    } FILE_BASIC_INFORMATION, *PFILE_BASIC_INFORMATION;
                */
                ByteBuffer basicInfoBuff = ByteBuffer.allocate(5 * 8);
                basicInfoBuff.order(ByteOrder.nativeOrder());

                try {
                    int[] bytesWritten = new int[1];
                    filter.queryFileInformationDirect(operHandleRaw, basicInfoBuff.array(), 4/*FileBasicInformation*/, bytesWritten);
                } catch (CBFSFilterException e) {
                    errorCode = e.getCode();
                    break;
                }

                IntBuffer intBasicInfoBuff = basicInfoBuff.asIntBuffer();
                if ((intBasicInfoBuff.get(8) & Constants.FILE_SYS_ATTR_DIRECTORY) == Constants.FILE_SYS_ATTR_DIRECTORY)
                {
                    errorCode = ERROR_NOT_SUPPORTED;
                    break;
                }

                try {
                    fileSize = operHandle.getLength();
                } catch (IOException e) {
                    e.printStackTrace();
                    errorCode = 5/*ERROR_ACCESS_DENIED*/;
                    break;
                }

                if (fileSize >= FileHeader.HEADER_SIZE) {

                    if (auxiHandle == null && (desiredAccess & Constants.DESIRED_ACCESS_FILE_READ_DATA) == 0)
                    {
                        try
                        {
                            auxiHandle = filter.createFileDirectAsStream(fileName, false,
                                    Constants.DESIRED_ACCESS_FILE_GENERIC_READ,
                                    Constants.FILESYS_SHARE_READ | Constants.FILESYS_SHARE_WRITE | Constants.FILESYS_SHARE_DELETE |
                                            Constants.CBFILTER_IGNORE_SHARE_ACCESS_CHECK
                                    ,
                                    Constants.FILE_DISPOSITION_OPEN_EXISTING, 0x02000000/*FILE_FLAG_BACKUP_SEMANTICS*/,
                                    false, handleHolder);
                        }
                        catch (CBFSFilterException e)
                        {
                            errorCode = e.getCode();
                            break;
                        }

                        operHandle = auxiHandle;
                        operHandleRaw = handleHolder[0];
                    }

                    errorCode = fileHeader.parseHeader(operHandle);
                    if (errorCode != 0)
                        break;
                }

                if (getHeaderPresent() == false)
                {
                    errorCode = fileHeader.setupHeader();
                    if (errorCode != 0)
                        break;
                }

            } while (false);

            if (auxiHandle != null) {
                try {
                    auxiHandle.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }

            if (errorCode != 0)
                close();

            return errorCode;
        }

        public int onQueryFileInfo(ByteBuffer buffer, int length, int fileInfoClass, int[] bytesWritten)
        {
            int errorCode = 0;

            ByteBuffer infoBuff = ByteBuffer.allocate(length);
            infoBuff.order(ByteOrder.nativeOrder());

            try {
                filter.queryFileInformationDirect(fileHandleRaw, infoBuff.array(), fileInfoClass, bytesWritten);
            } catch (CBFSFilterException e) {
                errorCode = e.getCode();
            }

            if (errorCode == 0 || errorCode == 234/*ERROR_MORE_DATA*/)
            {
                /*
                    typedef struct _FILE_ALL_INFORMATION {
                       FILE_BASIC_INFORMATION BasicInformation;
                       FILE_STANDARD_INFORMATION StandardInformation;
                        ...
                    } FILE_ALL_INFORMATION, *PFILE_ALL_INFORMATION;

                    typedef struct _FILE_BASIC_INFORMATION {
                        LARGE_INTEGER CreationTime;
                        LARGE_INTEGER LastAccessTime;
                        LARGE_INTEGER LastWriteTime;
                        LARGE_INTEGER ChangeTime;
                        ULONG FileAttributes;
                    } FILE_BASIC_INFORMATION, *PFILE_BASIC_INFORMATION;

                    typedef struct _FILE_STANDARD_INFORMATION {
                        LARGE_INTEGER AllocationSize;
                        LARGE_INTEGER EndOfFile;
                        ULONG NumberOfLinks;
                        BOOLEAN DeletePending;
                        BOOLEAN Directory;
                    } FILE_STANDARD_INFORMATION, *PFILE_STANDARD_INFORMATION;
                */

                if (fileInfoClass == 18/*FileAllInformation*/)
                {
                    LongBuffer longBuffer = infoBuff.asLongBuffer();

                    longBuffer.put(5, ROUND_TO_CLUSTER(getCalcedFileSize())); //AllocationSize
                    longBuffer.put(6, getCalcedFileSize()); //EndOfFile
                }
                else
                if (fileInfoClass == 5/*FileStandardInformation*/)
                {
                    LongBuffer longBuffer = infoBuff.asLongBuffer();

                    longBuffer.put(0, ROUND_TO_CLUSTER(getCalcedFileSize())); //AllocationSize
                    longBuffer.put(1, getCalcedFileSize()); //EndOfFile
                }
            }

            buffer.put(infoBuff.array(), 0, length);

            return errorCode;
        }

        public int onSetFileInfo(ByteBuffer buffer, int length, int fileInfoClass)
        {
            long newActualSize = -1;

            ByteBuffer infoBuff = ByteBuffer.allocate(length);

            infoBuff.order(ByteOrder.nativeOrder());
            infoBuff.put(buffer);

            infoBuff.position(0);

            if (fileInfoClass == 19/*FileAllocationInformation*/)
            {
                /*
                    typedef struct _FILE_ALLOCATION_INFORMATION {
                        LARGE_INTEGER AllocationSize;
                    } FILE_ALLOCATION_INFORMATION, *PFILE_ALLOCATION_INFORMATION;
                */
                if (length >= 8)
                {
                    LongBuffer longBuffer = infoBuff.asLongBuffer();
                    if (longBuffer.get(0) < getCalcedFileSize())
                    {
                        longBuffer.put(0, longBuffer.get(0) + getCalcedHeaderSize());
                        newActualSize = longBuffer.get(0);

                        //
                        // We directly replace it with FileEndOfFileInformation.
                        //

                        fileInfoClass = 20/*FileEndOfFileInformation*/;
                    }
                    else
                    {
                        longBuffer.put(0, longBuffer.get(0) + getCalcedHeaderSize());
                        longBuffer.put(0, ROUND_TO_CLUSTER(longBuffer.get(0)));
                    }
                }
                else
                {
                    return 87/*ERROR_INVALID_PARAMETER*/;
                }
            }
            else
            if (fileInfoClass == 20/*FileEndOfFileInformation*/)
            {
                /*
                    typedef struct _FILE_END_OF_FILE_INFORMATION {
                        LARGE_INTEGER EndOfFile;
                    } FILE_END_OF_FILE_INFORMATION, *PFILE_END_OF_FILE_INFORMATION;
                */
                if (length >= 8)
                {
                    LongBuffer longBuffer = infoBuff.asLongBuffer();

                    longBuffer.put(0, longBuffer.get(0) + getCalcedHeaderSize());

                    newActualSize = longBuffer.get(0);
                }
                else
                {
                    return 87/*ERROR_INVALID_PARAMETER*/;
                }
            }

            int errorCode = 0;
            try {
                filter.setFileInformationDirect(fileHandleRaw, fileInfoClass, infoBuff.array());
            } catch (CBFSFilterException e) {
                errorCode = e.getCode();
            }

            if (errorCode == 0 && newActualSize != -1)
            {
                fileSize = newActualSize;
            }

            return errorCode;
        }

        public int onQuerySecurity(int securityInfo, ByteBuffer securityDescriptor, int length, int[] lenNeeded)
        {
            int errorCode = 0;

            ByteBuffer secuBuff = ByteBuffer.allocate(length);

            try {
                filter.queryFileSecurityDirect(fileHandleRaw, securityInfo, secuBuff.array(), lenNeeded);
            } catch (CBFSFilterException e) {
                errorCode = e.getCode();
            }

            securityDescriptor.put(secuBuff.array(), 0, length);

            return errorCode;
        }

        public int onSetSecurity(int securityInfo, ByteBuffer securityDescriptor, int length)
        {
            int errorCode = 0;

            ByteBuffer secuBuff = ByteBuffer.allocate(length);
            secuBuff.put(securityDescriptor);

            try {
                filter.setFileSecurityDirect(fileHandleRaw, securityInfo, secuBuff.array());
            } catch (CBFSFilterException e) {
                errorCode = e.getCode();
            }

            return errorCode;
        }

        public int onCachedWriteExtendFileSize(long size)
        {
            long newActualSize = size + getCalcedHeaderSize();

            try {
                fileHandle.setLength(newActualSize);
            } catch (IOException e) {
                return 5/*ERROR_ACCESS_DENIED*/;
            }

            fileSize = newActualSize;

            return 0;
        }

        public int onNonCachedRead(ByteBuffer buffer, long position, int bytesToRead, int[] bytesRead)
        {
            int bufferLength = bytesToRead;

            assert(bufferLength % 512 == 0);

            if (position > getCalcedFileSize())
                return 38/*ERROR_HANDLE_EOF*/;

            if (position + bytesToRead >= getCalcedFileSize())
            {
                bytesToRead = (int)(getCalcedFileSize() - position);
            }

            position += getCalcedHeaderSize();

            try {
                fileHandle.setPosition(position);
                byte[] data = fileHandle.read(bytesToRead);
                bytesRead[0] = data.length;
                buffer.put(data);
            } catch (IOException e) {
                e.printStackTrace();
                return 5/*ERROR_ACCESS_DENIED*/;
            }

            if (getHeaderPresent())
            {
                fileHeader.decryptBuffer(buffer, bytesRead[0], bufferLength);
            }

            return 0;
        }

        public int onNonCachedWrite(ByteBuffer buffer, long position, int bytesToWrite, int[] bytesWritten)
        {
            int bufferLength = bytesToWrite;

            assert(bufferLength % 512 == 0);

            assert(getHeaderPresent());

            if (position > getCalcedFileSize())
                return 0;

            if (position + bytesToWrite >= getCalcedFileSize())
            {
                bytesToWrite = (int)(getCalcedFileSize() - position);
            }

            fileHeader.encryptBuffer(buffer, bytesToWrite, bufferLength);

            position += getCalcedHeaderSize();

            try {
                fileHandle.setPosition(position);

                byte[] bytes = new byte[bytesToWrite];
                buffer.position(0);
                buffer.get(bytes, 0, bytes.length);
                fileHandle.write(bytes);

                bytesWritten[0] = bytesToWrite;
            } catch (IOException e) {
                e.printStackTrace();
                return 5/*ERROR_ACCESS_DENIED*/;
            }

            return 0;
        }

        public int encryptFile()
        {
            assert(getHeaderPresent() == false);

            long dataSize = fileSize;

            try {
                fileHandle.setLength(dataSize + FileHeader.HEADER_SIZE);
            } catch (IOException e) {
                e.printStackTrace();
                return 5/*ERROR_ACCESS_DENIED*/;
            }

            byte[] buffer = new byte[FileHeader.HEADER_SIZE];

            long position = dataSize / buffer.length * buffer.length;
            long validDataLen = dataSize % buffer.length;


            while (true)
            {
                if (validDataLen != 0)
                {
                    try {
                        fileHandle.setPosition(position);

                        byte[] data = fileHandle.read((int)validDataLen);

                        System.arraycopy(data, 0, buffer, 0, data.length);

                        fileHeader.encryptBuffer(buffer, (int)validDataLen, buffer.length);

                        System.arraycopy(buffer, 0, data, 0, data.length);

                        long writePos = position + FileHeader.HEADER_SIZE;

                        fileHandle.setPosition(writePos);

                        fileHandle.write(data);

                    } catch (Exception e) {
                        e.printStackTrace();
                        return 5/*ERROR_ACCESS_DENIED*/;
                    }
                }

                position -= FileHeader.HEADER_SIZE;
                validDataLen = FileHeader.HEADER_SIZE;

                if (position < 0)
                    break;
            }

            int result = fileHeader.writeHeader(fileHandle);

            if (result == 0)
                fileSize += FileHeader.HEADER_SIZE;

            return result;
        }

        public int onRenameOrMoveFile(String newFileName, boolean replaceIfExists)
        {
            /*
                typedef struct _FILE_RENAME_INFORMATION {
                    BOOLEAN ReplaceIfExists;
                    HANDLE RootDirectory;
                    ULONG FileNameLength;
                    WCHAR FileName[1];
                } FILE_RENAME_INFORMATION, *PFILE_RENAME_INFORMATION;
            */
            try {
                ByteBuffer buffer = ByteBuffer.allocate(0x20 + newFileName.length() * 2);

                buffer.order(ByteOrder.nativeOrder());

                buffer.array()[0] = replaceIfExists ? (byte) 1 : (byte)0;

                buffer.position(0);
                buffer.asIntBuffer().put(4, newFileName.length() * 2);

                buffer.position(0);
                CharBuffer charBuffer = buffer.asCharBuffer();

                for (int i = 0; i < newFileName.length(); ++i)
                {
                    charBuffer.put(10 + i, newFileName.charAt(i));
                }

                filter.setFileInformationDirect(fileHandleRaw, 10/*FileRenameInformation*/, buffer.array());
            } catch (CBFSFilterException e) {
                return e.getCode();
            }

            return 0;
        }

        public void close()
        {
            if (fileHandle != null)
            {
                try {
                    fileHandle.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
                fileHandle = null;
                fileHandleRaw = -1;
            }
        }
    }

    // ----------------------------------------------------------------

    class FileHeader {

        public static final int HEADER_SIZE = 512;

        private byte[] headerTag;

        private static final byte authTag = (byte)'x';

        byte masterXoxKey;
        byte sessionKey;

        private boolean present = false;

        byte[] headerData = new byte[HEADER_SIZE];

        public FileHeader(byte[] masterKey)
        {
            try {
                headerTag = "@@@!!!cbfilter isolatedencryption!!!@@@".getBytes("UTF-8");
            } catch (UnsupportedEncodingException e) {
                e.printStackTrace();
            }

            masterXoxKey = 0;
            for (byte keyData : masterKey) {
                masterXoxKey += keyData;
            }

            sessionKey = (byte)System.currentTimeMillis();
        }

        public boolean getPresent() {
            return present;
        }

        public int parseHeader(CBFSFilterStream fileHandle)
        {
            long position = 0;
            int  bytesToRead = HEADER_SIZE;

            try {
                fileHandle.setPosition(position);

                byte[] data = fileHandle.read(bytesToRead);

                System.arraycopy(data, 0, headerData, 0, data.length);

            } catch (Exception e) {
                e.printStackTrace();
                return 5/*ERROR_ACCESS_DENIED*/;
            }

            for (int i = 0; i < headerTag.length; ++i)
            {
                // It is not an encrypted file.
                if (headerTag[i] != headerData[i])
                    return 0;
            }

            if ((headerData[headerTag.length] ^ masterXoxKey) != authTag)
            {
                return 86/*ERROR_INVALID_PASSWORD*/;
            }

            sessionKey = (byte)(headerData[headerTag.length + 1] ^ masterXoxKey);

            present = true;

            return 0;
        }

        public int setupHeader()
        {
            headerData = new byte[HEADER_SIZE];

            System.arraycopy(headerTag, 0, headerData, 0, headerTag.length);

            headerData[headerTag.length] = (byte)(authTag ^ masterXoxKey);
            headerData[headerTag.length + 1] = (byte)(sessionKey ^ masterXoxKey);

            present = false;

            return 0;
        }

        public int writeHeader(CBFSFilterStream fileHandle)
        {
            long position = 0;
            int  bytesToWrite = HEADER_SIZE;
            int  bytesWritten = 0;


            try {
                fileHandle.setPosition(position);
                fileHandle.write(headerData);
            } catch (IOException e) {
                e.printStackTrace();
                return 5/*ERROR_ACCESS_DENIED*/;
            }

            present = true;

            return 0;
        }

        public void encryptBuffer(byte[] buffer, int bytesToWrite, int bufferLength)
        {
            assert(bufferLength % 512 == 0);

            for (int i = 0; i < bytesToWrite; ++i)
            {
                buffer[i] = (byte)(buffer[i] ^ sessionKey);
            }
        }

        public void encryptBuffer(ByteBuffer buffer, int bytesToWrite, int bufferLength)
        {
            assert(bufferLength % 512 == 0);

            for (int i = 0; i < bytesToWrite; ++i)
            {
                buffer.put(i, (byte)(buffer.get(i) ^ sessionKey));
            }
        }

        public void decryptBuffer(ByteBuffer buffer, int bytesRead, int bufferLength)
        {
            assert(bufferLength % 512 == 0);

            for (int i = 0; i < bytesRead; ++i)
            {
                buffer.put(i, (byte)(buffer.get(i) ^ sessionKey));
            }
        }
    }

    // ----------------------------------------------------------------

    @Override
    public void afterCanFileBeDeleted(CBFilterAfterCanFileBeDeletedEvent e) {

    }

    @Override
    public void afterCleanupFile(CBFilterAfterCleanupFileEvent e) {

    }

    @Override
    public void afterCloseEnumeration(CBFilterAfterCloseEnumerationEvent e) {

    }

    @Override
    public void afterCloseFile(CBFilterAfterCloseFileEvent e) {

    }

    @Override
    public void afterCreateFile(CBFilterAfterCreateFileEvent e) {

    }

    @Override
    public void afterCreateHardLink(CBFilterAfterCreateHardLinkEvent e) {

    }

    @Override
    public void afterDeleteFile(CBFilterAfterDeleteFileEvent e) {

    }

    @Override
    public void afterDeleteReparsePoint(CBFilterAfterDeleteReparsePointEvent e) {

    }

    @Override
    public void afterFilterAttachToVolume(CBFilterAfterFilterAttachToVolumeEvent e) {

    }

    @Override
    public void afterFilterDetachFromVolume(CBFilterAfterFilterDetachFromVolumeEvent e) {

    }

    @Override
    public void afterFsctl(CBFilterAfterFsctlEvent e) {

    }

    @Override
    public void afterGetFileSecurity(CBFilterAfterGetFileSecurityEvent e) {

    }

    @Override
    public void afterGetFileSizes(CBFilterAfterGetFileSizesEvent e) {

    }

    @Override
    public void afterGetReparsePoint(CBFilterAfterGetReparsePointEvent e) {

    }

    @Override
    public void afterIoctl(CBFilterAfterIoctlEvent e) {

    }

    @Override
    public void afterLock(CBFilterAfterLockEvent e) {

    }

    @Override
    public void afterOpenFile(CBFilterAfterOpenFileEvent e) {

    }

    @Override
    public void afterQueryEa(CBFilterAfterQueryEaEvent e) {

    }

    @Override
    public void afterQueryFileInfo(CBFilterAfterQueryFileInfoEvent e) {

    }

    @Override
    public void afterReadFile(CBFilterAfterReadFileEvent e) {

    }

    @Override
    public void afterRenameOrMoveFile(CBFilterAfterRenameOrMoveFileEvent e) {

    }

    @Override
    public void afterSetAllocationSize(CBFilterAfterSetAllocationSizeEvent e) {

    }

    @Override
    public void afterSetEa(CBFilterAfterSetEaEvent e) {

    }

    @Override
    public void afterSetFileAttributes(CBFilterAfterSetFileAttributesEvent e) {

    }

    @Override
    public void afterSetFileInfo(CBFilterAfterSetFileInfoEvent e) {

    }

    @Override
    public void afterSetFileSecurity(CBFilterAfterSetFileSecurityEvent e) {

    }

    @Override
    public void afterSetFileSize(CBFilterAfterSetFileSizeEvent e) {

    }

    @Override
    public void afterSetReparsePoint(CBFilterAfterSetReparsePointEvent e) {

    }

    @Override
    public void afterUnlockAll(CBFilterAfterUnlockAllEvent e) {

    }

    @Override
    public void afterUnlockAllByKey(CBFilterAfterUnlockAllByKeyEvent e) {

    }

    @Override
    public void afterUnlockSingle(CBFilterAfterUnlockSingleEvent e) {

    }

    @Override
    public void afterWriteFile(CBFilterAfterWriteFileEvent e) {

    }

    @Override
    public void beforeCanFileBeDeleted(CBFilterBeforeCanFileBeDeletedEvent e) {

    }

    @Override
    public void beforeCleanupFile(CBFilterBeforeCleanupFileEvent e) {

    }

    @Override
    public void beforeCloseFile(CBFilterBeforeCloseFileEvent e) {

    }

    @Override
    public void beforeCreateHardLink(CBFilterBeforeCreateHardLinkEvent e) {

    }

    @Override
    public void beforeDeleteFile(CBFilterBeforeDeleteFileEvent e) {

    }

    @Override
    public void beforeDeleteReparsePoint(CBFilterBeforeDeleteReparsePointEvent e) {

    }

    @Override
    public void beforeEnumerateDirectory(CBFilterBeforeEnumerateDirectoryEvent e) {

    }

    @Override
    public void beforeFilterAttachToVolume(CBFilterBeforeFilterAttachToVolumeEvent e) {

    }

    @Override
    public void beforeFsctl(CBFilterBeforeFsctlEvent e) {

    }

    @Override
    public void beforeGetReparsePoint(CBFilterBeforeGetReparsePointEvent e) {

    }

    @Override
    public void beforeIoctl(CBFilterBeforeIoctlEvent e) {

    }

    @Override
    public void beforeLock(CBFilterBeforeLockEvent e) {

    }

    @Override
    public void beforeQueryEa(CBFilterBeforeQueryEaEvent e) {

    }

    @Override
    public void beforeSetAllocationSize(CBFilterBeforeSetAllocationSizeEvent e) {

    }

    @Override
    public void beforeSetEa(CBFilterBeforeSetEaEvent e) {

    }

    @Override
    public void beforeSetFileAttributes(CBFilterBeforeSetFileAttributesEvent e) {

    }

    @Override
    public void beforeSetFileSize(CBFilterBeforeSetFileSizeEvent e) {

    }

    @Override
    public void beforeSetReparsePoint(CBFilterBeforeSetReparsePointEvent e) {

    }

    @Override
    public void beforeUnlockAll(CBFilterBeforeUnlockAllEvent e) {

    }

    @Override
    public void beforeUnlockAllByKey(CBFilterBeforeUnlockAllByKeyEvent e) {

    }

    @Override
    public void beforeUnlockSingle(CBFilterBeforeUnlockSingleEvent e) {

    }

    @Override
    public void error(CBFilterErrorEvent e) {
        AddToLog(String.format("%s %d", e.description, e.errorCode));
    }

    @Override
    public void filterStart(CBFilterFilterStartEvent e) {

    }

    @Override
    public void filterStop(CBFilterFilterStopEvent e) {

    }

    @Override
    public void notifyCanFileBeDeleted(CBFilterNotifyCanFileBeDeletedEvent e) {

    }

    @Override
    public void notifyCleanupFile(CBFilterNotifyCleanupFileEvent e) {

    }

    @Override
    public void notifyCloseFile(CBFilterNotifyCloseFileEvent e) {

    }

    @Override
    public void notifyCreateFile(CBFilterNotifyCreateFileEvent e) {

    }

    @Override
    public void notifyCreateHardLink(CBFilterNotifyCreateHardLinkEvent e) {

    }

    @Override
    public void notifyDeleteFile(CBFilterNotifyDeleteFileEvent e) {

    }

    @Override
    public void notifyDeleteReparsePoint(CBFilterNotifyDeleteReparsePointEvent e) {

    }

    @Override
    public void notifyEnumerateDirectory(CBFilterNotifyEnumerateDirectoryEvent e) {

    }

    @Override
    public void notifyFilterAttachToVolume(CBFilterNotifyFilterAttachToVolumeEvent e) {

    }

    @Override
    public void notifyFilterDetachFromVolume(CBFilterNotifyFilterDetachFromVolumeEvent e) {

    }

    @Override
    public void notifyFsctl(CBFilterNotifyFsctlEvent e) {

    }

    @Override
    public void notifyGetFileSecurity(CBFilterNotifyGetFileSecurityEvent e) {

    }

    @Override
    public void notifyGetFileSizes(CBFilterNotifyGetFileSizesEvent e) {

    }

    @Override
    public void notifyGetReparsePoint(CBFilterNotifyGetReparsePointEvent e) {

    }

    @Override
    public void notifyIoctl(CBFilterNotifyIoctlEvent e) {

    }

    @Override
    public void notifyLock(CBFilterNotifyLockEvent e) {

    }

    @Override
    public void notifyOpenFile(CBFilterNotifyOpenFileEvent e) {

    }

    @Override
    public void notifyQueryEa(CBFilterNotifyQueryEaEvent e) {

    }

    @Override
    public void notifyQueryFileInfo(CBFilterNotifyQueryFileInfoEvent e) {

    }

    @Override
    public void notifyReadFile(CBFilterNotifyReadFileEvent e) {

    }

    @Override
    public void notifyRenameOrMoveFile(CBFilterNotifyRenameOrMoveFileEvent e) {

    }

    @Override
    public void notifySetAllocationSize(CBFilterNotifySetAllocationSizeEvent e) {

    }

    @Override
    public void notifySetEa(CBFilterNotifySetEaEvent e) {

    }

    @Override
    public void notifySetFileAttributes(CBFilterNotifySetFileAttributesEvent e) {

    }

    @Override
    public void notifySetFileInfo(CBFilterNotifySetFileInfoEvent e) {

    }

    @Override
    public void notifySetFileSecurity(CBFilterNotifySetFileSecurityEvent e) {

    }

    @Override
    public void notifySetFileSize(CBFilterNotifySetFileSizeEvent e) {

    }

    @Override
    public void notifySetReparsePoint(CBFilterNotifySetReparsePointEvent e) {

    }

    @Override
    public void notifyUnlockAll(CBFilterNotifyUnlockAllEvent e) {

    }

    @Override
    public void notifyUnlockAllByKey(CBFilterNotifyUnlockAllByKeyEvent e) {

    }

    @Override
    public void notifyUnlockSingle(CBFilterNotifyUnlockSingleEvent e) {

    }

    @Override
    public void notifyWriteFile(CBFilterNotifyWriteFileEvent e) {

    }

    @Override
    public void reparseFileName(CBFilterReparseFileNameEvent e) {

    }

    @Override
    public void reparseWithTag(CBFilterReparseWithTagEvent e) {

    }

    @Override
    public void workerThreadCreation(CBFilterWorkerThreadCreationEvent e) {

    }

    @Override
    public void workerThreadTermination(CBFilterWorkerThreadTerminationEvent e) {

    }

    // ----------------------------------------------------------------

    /**
     * This class is used to keep all the contexts in this demo.
     */
    private static class Contexts {
        private static long counter = 0;
        private static final HashMap<Long, Handle> contexts = new HashMap<Long, Handle>();

        /**
         * Retrives an object by its ID, also increases the object's usage counter by 1.
         * @param id An ID of an object to acquire.
         * @return An acquired object or {@code null} if not object found by the specified ID.
         */
        static Object acquire(long id) {
            synchronized (contexts) {
                Handle handle = contexts.get(id);
                if (handle == null)
                    return null;
                handle.counter++;
                return handle.target;
            }
        }

        /**
         * Stores an object in the internal list and returns its ID. The usage counter for the object is set to 1.
         * @param target An object reference to store in the internal list.
         * @return An ID of a stored object.
         */
        static long alloc(Object target) {
            synchronized (contexts) {
                long id = ++counter;
                Handle handle = new Handle(target);
                contexts.put(id, handle);
                return id;
            }
        }

        /**
         * Removes a stored object by its ID despite the value of the usage counter.
         * @param id An ID of an object to remove from the internal list.
         * @return A reference to a stored object, or {@code null} if the ID was not found in the internal list.
         */
        static Object free(long id) {
            synchronized (contexts) {
                Handle handle = contexts.remove(id);
                if (handle == null)
                    return null;
                Object target = handle.target;
                handle.clear();
                return target;
            }
        }

        /**
         * Gets an object by its ID.
         * @param id An ID returned by {@link #alloc}.
         * @return An object corresponding to the ID, or {@code null} if no object stored with the specified ID.
         */
        static Object get(long id) {
            synchronized (contexts) {
                Handle handler = contexts.get(id);
                if (handler != null)
                    return handler.target;
                return null;
            }
        }

        /**
         * Decreases the object's usage counter by 1. If the counter reaches 0, the object is removed from
         * the internal list.
         * @param id An ID of an object to release.
         * @return {@code true} if the object has been removed from the internal list, and {@code false} otherwise.
         */
        static boolean release(long id) {
            synchronized (contexts) {
                Handle handle = contexts.get(id);
                if (handle == null)
                    return false;
                if (--handle.counter > 0)
                    return false;
                contexts.remove(id);
                handle.clear();
                return true;
            }
        }

        /**
         * Stores a new object with an existing ID. If no object found for the specified ID,
         * the new object is not stored.
         * @param id An ID to update.
         * @param target A new object to store in the internal list.
         * @return A reference to the previously stored object, or {@code null} if no object
         * has been found for the specified ID.
         */
        static Object set(long id, Object target) {
            synchronized (contexts) {
                Handle handle = contexts.get(id);
                if (handle == null)
                    return null;
                Object old = handle.target;
                handle.target = target;
                return old;
            }
        }

        private static class Handle {
            int counter;
            Object target;

            Handle(Object target) {
                super();
                this.counter = 1;
                this.target = target;
            }

            void clear() {
                counter = 0;
                target = null;
            }
        }
    }
}




