/*
 * CBFS Filter 2022 Java Edition - Sample Project
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
import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.regex.Pattern;

import cbfsfilter.*;

public class filemon implements cbfsfilter.CbmonitorEventListener {
    private final Cbmonitor mWatcher = new Cbmonitor();

    private final String guid = "{713CC6CE-B3E2-4fd9-838D-E28F558F6866}";
    private final String ALTITUDE_FAKE_VALUE_FOR_DEBUG = "360000";
    private final int ERROR_ACCESS_DENIED = 5;
    private final int ERROR_PRIVILEGE_NOT_HELD = 1314;
    private static final Pattern DRIVE_LETTER_PATTERN = Pattern.compile("^[A-Za-z]:$");
    public filemon() {
        try {
            mWatcher.addCbmonitorEventListener(this);
            checkDriver();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }


    static void banner() {
        System.out.println("CBFS Filter FileMon Demo Copyright (c) 2017-2023, Callback Technologies, Inc.\n\n");
    }

    static void usage() {
        System.out.println("Usage: filemon [-<switch 1> ... -<switch N>] [<path to monitor> [<filename mask to monitor>]]\n");
        System.out.println("<Switches>");
        System.out.println("  -drv <path to cbfilter.cab> - Install the driver from the specified CAB file");
        System.out.println("  -- Stop switches scanning\n");
    }

    protected boolean checkDriver() {
        return checkDriver(true);
    }

    protected boolean checkDriver(boolean writeInfo)
    {
        try {
            int moduleStatus = mWatcher.getDriverStatus(guid);
            long moduleVersion = mWatcher.getDriverVersion(guid);
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

    protected void addToLog(String operation, String filename, int status)
    {
        String processName;
        try {
            processName = mWatcher.getOriginatorProcessName();
            if (processName.length() == 0)
                processName = "System";
        } catch (CBFSFilterException e) {
            processName = "< ERROR >";
        }

        int processID;
        try {
            processID = mWatcher.getOriginatorProcessId();
        } catch (CBFSFilterException e) {
            processID = 0;
        }

        System.out.printf("<LogInfo> Operation: %s, Path: %s, Originator Process: %s, Process ID: %d, User Name: %s, Result: %d%n",
                operation, filename, processName, processID, "", status);
    }

    protected void install(String fileName)
    {
        if (!new File(fileName).exists())
            return;
        try
        {
            System.out.println("Installing the driver from " + fileName);
            boolean reboot = mWatcher.install(fileName, guid, "", ALTITUDE_FAKE_VALUE_FOR_DEBUG, 0);

            checkDriver();

            if (reboot)
                System.out.println("Please, reboot the system for the changes to take effect");
            else
                System.out.println("Driver installed successfully");

        }
        catch (CBFSFilterException e)
        {
            if (e.getCode() == ERROR_PRIVILEGE_NOT_HELD || e.getCode() == ERROR_ACCESS_DENIED)
                System.out.println("Installation requires administrator rights. Run the app as administrator");
            else
                System.out.println(e.getMessage());
        }
    }

    protected void uninstall(String fileName)
    {
        if (!new File(fileName).exists())
            return;
        try
        {
            System.out.println("Uninstalling the driver from " + fileName);
            boolean reboot = mWatcher.uninstall(fileName, guid, "", 0);

            checkDriver();

            if (reboot)
                System.out.println("Please, reboot the system for the changes to take effect");
            else
                System.out.println("Driver uninstalled successfully");

        }
        catch (CBFSFilterException e)
        {
            if (e.getCode() == ERROR_PRIVILEGE_NOT_HELD || e.getCode() == ERROR_ACCESS_DENIED)
                System.out.println("Uninstallation requires administrator rights. Run the app as administrator");
            else
                System.out.println(e.getMessage());
        }
    }

    protected void setFilter(String mask)
    {
        try {
            mWatcher.initialize(guid);

            mWatcher.addFilterRule(mask,
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
                            Constants.FS_NE_SET_SECURITY);
            mWatcher.setProcessFailedRequests(true);

            mWatcher.startFilter();
        } catch (CBFSFilterException e) {
            System.out.println("Error in setFilter: " + e.getMessage());
        }

        checkDriver(false);
    }

    protected void deleteFilter()
    {
        try {
            mWatcher.deleteAllFilterRules();
            mWatcher.deleteAllPassthroughRules();
            if (mWatcher.isActive())
                mWatcher.stopFilter(false);
        } catch (CBFSFilterException e) {
            System.out.println("Error in deleteFilter: " + e.getMessage());
        }
    }

    public static void main(String[] args) {
        String pathToMonitor = null;
        String filenameMask = null;

        int argi, argLen;
        boolean stopOpt = false;

        banner();

        try {
            filemon fileMonitor = new filemon();

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
                            arg = fileMonitor.ConvertRelativePathToAbsolute(args[++argi]);
                            if (argi < args.length) {
                                fileMonitor.install(arg);
                            }
                        } else {
                            System.out.println("Error: Invalid option " + arg);
                        }
                    } else {
                        // if we have not set the path yet, do this now. Otherwise, set the mask
                        if (pathToMonitor == null) {
                            pathToMonitor = fileMonitor.ConvertRelativePathToAbsolute(arg);
                            if (!new File(pathToMonitor).exists()) {
                                System.out.println("ERROR: the specified path '" + pathToMonitor
                                        + "' does not point to an existing directory");
                                return;
                            }
                        } else {
                            filenameMask = arg;
                            break; // no more parameters expected
                        }
                    }
                }
            }

            if (!fileMonitor.checkDriver()) { // driver not installed, so we must quit
                return;
            }

            // Probably, we have just installed the driver, so we can quit without monitoring anything
            if (pathToMonitor == null)
                return;

            System.out.println("Press 'Enter' to start monitoring, then press 'Enter' and quit.");

            System.in.read();

            try {
                String mask = pathToMonitor + File.separator + filenameMask;
                fileMonitor.setFilter(mask);
                System.out.println("Press 'Enter' to stop monitoring and quit.");
                System.in.read();
                fileMonitor.deleteFilter();
            } catch (Exception ex) {
                System.out.println("Error: " + ex.getMessage());
            }
        } catch (Exception e) {
            System.out.println("Error in Main: " + e.getMessage());
        }
    }

    private boolean isDriveLetter(String path) {
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

    private boolean isNullOrEmpty(String s) {
        if (s == null)
            return true;
        if (s.length() == 0)
            return true;
        return false;
    }

    private String ConvertRelativePathToAbsolute(String path) {
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
                Path fullPath = Paths.get(path).toAbsolutePath().normalize();
                res = fullPath.toString();

                File file = new File(res);

                if (res.startsWith("\\\\") && !file.exists()) {
                    System.out.println("The network folder '" + res + "' does not exist.");
                } else if (!file.exists()) {
                    System.out.println("The path '" + res + "' does not exist.");
                }
            }
        }
        return res;
    }

    @Override
    public void afterFilterAttachToVolume(CbmonitorAfterFilterAttachToVolumeEvent cbfilterAfterFilterAttachToVolumeEvent) {

    }

    @Override
    public void afterFilterDetachFromVolume(CbmonitorAfterFilterDetachFromVolumeEvent cbfilterAfterFilterDetachFromVolumeEvent) {

    }

    @Override
    public void beforeFilterAttachToVolume(CbmonitorBeforeFilterAttachToVolumeEvent cbfilterBeforeFilterAttachToVolumeEvent) {

    }

    @Override
    public void error(CbmonitorErrorEvent cbfilterErrorEvent) {

    }

    @Override
    public void filterStart(CbmonitorFilterStartEvent cbfilterFilterStartEvent) {

    }

    @Override
    public void filterStop(CbmonitorFilterStopEvent cbfilterFilterStopEvent) {
        checkDriver();
    }

    @Override
    public void notifyCanFileBeDeleted(CbmonitorNotifyCanFileBeDeletedEvent cbfilterNotifyCanFileBeDeletedEvent) {

    }

    @Override
    public void notifyCleanupFile(CbmonitorNotifyCleanupFileEvent cbfilterNotifyCleanupFileEvent) {

    }

    @Override
    public void notifyCloseFile(CbmonitorNotifyCloseFileEvent e) {
        addToLog("notifyCloseFile", e.fileName, 0);
    }

    @Override
    public void notifyCreateFile(CbmonitorNotifyCreateFileEvent e) {
        addToLog("notifyCreateFile", e.fileName, e.status);
    }

    @Override
    public void notifyCreateHardLink(CbmonitorNotifyCreateHardLinkEvent e) {
        addToLog("notifyCreateHardLink", e.fileName, e.status);
    }

    @Override
    public void notifyDeleteFile(CbmonitorNotifyDeleteFileEvent e) {
        addToLog("notifyDeleteFile", e.fileName, 0);

    }

    @Override
    public void notifyDeleteReparsePoint(CbmonitorNotifyDeleteReparsePointEvent cbmonitorNotifyDeleteReparsePointEvent) {

    }

    @Override
    public void notifyEnumerateDirectory(CbmonitorNotifyEnumerateDirectoryEvent e) {
        addToLog("notifyEnumerateDirectory", e.directoryName + File.separator + e.fileName, e.status);
    }

    @Override
    public void notifyFilterAttachToVolume(CbmonitorNotifyFilterAttachToVolumeEvent cbfilterNotifyFilterAttachToVolumeEvent) {

    }

    @Override
    public void notifyFilterDetachFromVolume(CbmonitorNotifyFilterDetachFromVolumeEvent cbfilterNotifyFilterDetachFromVolumeEvent) {

    }

    @Override
    public void notifyFsctl(CbmonitorNotifyFsctlEvent cbfilterNotifyFsctlEvent) {

    }

    @Override
    public void notifyGetFileSecurity(CbmonitorNotifyGetFileSecurityEvent cbfilterNotifyGetFileSecurityEvent) {

    }

    @Override
    public void notifyGetFileSizes(CbmonitorNotifyGetFileSizesEvent cbfilterNotifyGetFileSizesEvent) {

    }

    @Override
    public void notifyGetReparsePoint(CbmonitorNotifyGetReparsePointEvent cbmonitorNotifyGetReparsePointEvent) {

    }

    @Override
    public void notifyIoctl(CbmonitorNotifyIoctlEvent cbfilterNotifyIoctlEvent) {

    }

    @Override
    public void notifyLock(CbmonitorNotifyLockEvent cbfilterNotifyLockEvent) {

    }

    @Override
    public void notifyOpenFile(CbmonitorNotifyOpenFileEvent e) {
        addToLog("notifyOpenFile", e.fileName, e.status);
    }

    @Override
    public void notifyQueryEa(CbmonitorNotifyQueryEaEvent cbmonitorNotifyQueryEaEvent) {

    }

    @Override
    public void notifyQueryFileInfo(CbmonitorNotifyQueryFileInfoEvent cbfilterNotifyQueryFileInfoEvent) {

    }

    @Override
    public void notifyReadFile(CbmonitorNotifyReadFileEvent e) {
        addToLog("notifyReadFile", e.fileName, e.status);
    }

    @Override
    public void notifyRenameOrMoveFile(CbmonitorNotifyRenameOrMoveFileEvent e) {
        addToLog("notifyRenameOrMoveFile", e.fileName, e.status);
    }

    @Override
    public void notifySetAllocationSize(CbmonitorNotifySetAllocationSizeEvent cbfilterNotifySetAllocationSizeEvent) {

    }

    @Override
    public void notifySetEa(CbmonitorNotifySetEaEvent cbmonitorNotifySetEaEvent) {

    }

    @Override
    public void notifySetFileSize(CbmonitorNotifySetFileSizeEvent cbfilterNotifySetFileSizeEvent) {

    }

    @Override
    public void notifySetReparsePoint(CbmonitorNotifySetReparsePointEvent cbmonitorNotifySetReparsePointEvent) {

    }

    @Override
    public void notifySetFileAttributes(CbmonitorNotifySetFileAttributesEvent e) {
        addToLog("notifySetFileAttributes", e.fileName, e.status);
    }

    @Override
    public void notifySetFileInfo(CbmonitorNotifySetFileInfoEvent cbmonitorNotifySetFileInfoEvent) {

    }

    @Override
    public void notifySetFileSecurity(CbmonitorNotifySetFileSecurityEvent e) {
        addToLog("notifySetFileSecurity", e.fileName, e.status);
    }

    @Override
    public void notifyUnlockAll(CbmonitorNotifyUnlockAllEvent cbfilterNotifyUnlockAllEvent) {

    }

    @Override
    public void notifyUnlockAllByKey(CbmonitorNotifyUnlockAllByKeyEvent cbfilterNotifyUnlockAllByKeyEvent) {

    }

    @Override
    public void notifyUnlockSingle(CbmonitorNotifyUnlockSingleEvent cbfilterNotifyUnlockSingleEvent) {

    }

    @Override
    public void notifyWriteFile(CbmonitorNotifyWriteFileEvent e) {
        addToLog("notifyWriteFile", e.fileName, e.status);
    }

    @Override
    public void workerThreadCreation(CbmonitorWorkerThreadCreationEvent cbfilterWorkerThreadCreationEvent) {

    }

    @Override
    public void workerThreadTermination(CbmonitorWorkerThreadTerminationEvent cbfilterWorkerThreadTerminationEvent) {

    }
}




