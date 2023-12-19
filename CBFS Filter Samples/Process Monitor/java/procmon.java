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
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Scanner;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.regex.Pattern;

import cbfsfilter.*;

public class procmon implements CbprocessEventListener {
    private enum DriverStatus {
        NOT_INSTALLED,
        STOPPED,
        START_PENDING,
        STOP_PENDING,
        RUNNING,
        CONTINUE_PENDING,
        PAUSE_PENDING,
        PAUSED;

        public static DriverStatus fromInt(int value) {
            switch (value) {
                case 0:
                    return NOT_INSTALLED;
                case 1:
                    return STOPPED;
                case 2:
                    return START_PENDING;
                case 3:
                    return STOP_PENDING;
                case 4:
                    return RUNNING;
                case 5:
                    return CONTINUE_PENDING;
                case 6:
                    return PAUSE_PENDING;
                case 7:
                    return PAUSED;
                default:
                    return null;
            }
        }

        @Override
        public String toString() {
            switch (this) {
                case NOT_INSTALLED:
                    return "not installed";
                case STOPPED:
                    return "stopped";
                case START_PENDING:
                    return "start pending";
                case STOP_PENDING:
                    return "stop pending";
                case RUNNING:
                    return "running";
                case CONTINUE_PENDING:
                    return "continue pending";
                case PAUSE_PENDING:
                    return "pause pending";
                case PAUSED:
                    return "paused";
                default:
                    return super.toString();
            }
        }
    }

    private static final String PRODUCT_GUID = "{713CC6CE-B3E2-4fd9-838D-E28F558F6866}";
    private static final int GAP = 6;
    private static final int DGAP = GAP * 2;
    private static final int ERROR_ACCESS_DENIED = 5;
    private static final int ERROR_PRIVILEGE_NOT_HELD = 1314;
    private static final int PROCESS_TERMINATE = 0x0001;
    private static final int PROCESS_SUSPEND_RESUME = 0x0800;
    private static final int THREAD_TERMINATE = 0x0001;

    private static final Pattern DRIVE_LETTER_PATTERN = Pattern.compile("^[A-Za-z]:$");

    private boolean driverRunning;
    private String cabFileLocation;

    private boolean processExecutionDenied;

    private Cbprocess filter;
    private Process process;

    private boolean denyExecute = false;
    private boolean denyTerminate = false;
    private String noExecOpName;
    private String noTermOpName;
    private int noTermOpPID;

    private long started;
    private static long currentProcessId;

    public procmon() {
        filter = createFilter();
        checkDriver(filter);
        disposeFilter(filter);
    }

    private Cbprocess createFilter() {
        Cbprocess filter = new Cbprocess();
        try {
            filter.addCbprocessEventListener(this);
        }
        catch (Exception err) {
            disposeFilter(filter);
            filter = null;
        }
        return filter;
    }

    private void disposeFilter(Cbprocess filter) {
        if (filter == null)
            return;

        try {
            filter.dispose();
        }
        catch (CBFSFilterException inner) {
            //
        }
    }

    private void stopFilter() {
        if (filter != null) {
            try {
                filter.stopFilter();
                filter.dispose();
            } catch (CBFSFilterException err) {
                //
            }
            filter = null;
            log("Filter stopped");
        }
        process = null;
    }

    private void checkDriver(Cbprocess filter) {
        try {
            DriverStatus status = DriverStatus.fromInt(filter.getDriverStatus(PRODUCT_GUID));
            driverRunning = (status == DriverStatus.RUNNING);

            if (status == null)
                System.out.println("Error - failed to get driver status");
            else
            if (status == DriverStatus.NOT_INSTALLED)
                System.out.println("Driver is not installed");
            else {
                long version = filter.getDriverVersion(PRODUCT_GUID);
                String text = String.format("Driver (ver %d.%d.%d.%d) installed, service %s",
                        version >> 48, (version >> 32) & 0xffff, (version >> 16) & 0xffff, version & 0xffff, status);
                System.out.println(text);
            }
        }
        catch (CBFSFilterException err) {
            System.out.println("Error - " + err.getMessage());
            driverRunning = false;
        }
    }

    private static boolean isNullOrEmpty(String s) {
        if (s == null)
            return true;
        if (s.length() == 0)
            return true;
        return false;
    }

    static void banner() {
        System.out.println("CBFS Filter Process Monitor Demo Copyright (c) 2017-2023, Callback Technologies, Inc.\n");
    }

    static void usage() {
        System.out.println("Usage: procmon [-<switch 1> ... -<switch N>]\n");
        System.out.println("<Switches>");
        System.out.println("  -mon - monitor all processes in the system");
        System.out.println("  -drv <path to cbprocess.cab> - Install the driver from the specified CAB file");
        System.out.println("  -noexec <EXE name> - Prevent execution of the process from the given EXE name (may contain a path)");
        System.out.println("  -noterm {<EXE name>|<PID>} - Prevent termination of the process with the given EXE name (may contain a path) or process ID");
        System.out.println("IMPORTANT: termination protection doesn't prevent closing of process windows and graceful exit. It just removes the permission from the handle\n");
    }

    private void install(String fileName) {
        boolean rebootNeeded;

        Cbprocess filter = createFilter();
        try {
            System.out.println("Installing the driver from '" + fileName + "'");
            rebootNeeded = filter.install(fileName, PRODUCT_GUID, null, Constants.INSTALL_REMOVE_OLD_VERSIONS);
            checkDriver(filter);
            System.out.print("Drivers installed successfully");

            if (rebootNeeded) {
                System.out.print(", reboot is required\n");
                return;
            }
            System.out.print("\n");

        } catch (CBFSFilterException err) {
            String message;
            System.out.println("The driver could not be installed");

            if (err.getCode() == ERROR_ACCESS_DENIED || err.getCode() == ERROR_PRIVILEGE_NOT_HELD)
                message = "Installation requires administrator rights. Run the app as administrator";
            else
                message = err.getMessage();

            System.out.println("Error: " + message);
            return;
        } finally {
            if (filter != null) {
                disposeFilter(filter);
            }
        }
    }

    private void setFilter() {
        filter = createFilter();
        try
        {
            filter.initialize(PRODUCT_GUID);

            // Make the demo can receive notifications triggered by itself.
            filter.config("FilterOwnRequests=true");
            filter.config("EventsToFire=-1");
            started = System.currentTimeMillis();
            filter.startFilter(30000);
            log("Filter started");
        }
        catch (CBFSFilterException err)
        {
            System.out.println("Error: " + err.getMessage());
            return;
        }

        try
        {
            filter.addFilteredProcessById(-1, true);
        } catch (CBFSFilterException err)
        {
            System.out.println("Error while trying to add the filtering rule: " + err.getMessage());
            return;
        }
    }

    public static void main(String[] args) {
        boolean drvReboot = false;
        boolean optTerminate = false;

        String KeyToMonitor = null;

        int resCode = 0, argi, argLen;
        boolean stop_opt = false;

        Scanner scanner = new Scanner(System.in);

        banner();

        // if no parameters specified, show usage and quit
        try {
            procmon procMonitor = new procmon();
            if (args.length < 1) {
                usage();
                return;
            }

            for (argi = 0; argi < args.length; argi++) {
                String arg = args[argi];
                argLen = arg.length();
                if (argLen > 0) {
                    if ((arg.charAt(0) == '-') && !stop_opt) {
                        if (arg.equalsIgnoreCase("-drv")) {
                            if (argi < args.length) {
                                arg = ConvertRelativePathToAbsolute(args[++argi]);
                                procMonitor.install(arg);
                            }
                        } else if (arg.equalsIgnoreCase("-noexec")) {
                            if (argi < args.length) {
                                arg = args[++argi];
                                procMonitor.noExecOpName = arg;
                                procMonitor.denyExecute = true;
                            } else {
                                System.out.println("Error: -noexec parameter requires a file name as the next argument");
                                return;
                            }
                        } else if (arg.equalsIgnoreCase("-noterm")) {
                            if (argi < args.length) {
                                arg = args[++argi];
                                procMonitor.noTermOpName = arg;
                                try {
                                    procMonitor.noTermOpPID = Integer.parseInt(procMonitor.noTermOpName);
                                } catch (Exception ex) {

                                }
                                procMonitor.denyTerminate = true;
                            } else {
                                System.out.println("Error: -noterm parameter requires a file name or a process ID as the next argument");
                                return;
                            }
                        } else if (arg.equalsIgnoreCase("-mon")) {
                            // nothing here, execution will pass through
                        } else {
                            System.out.println("Error: Invalid option " + arg);
                        }
                    } else {
                        // if we have not set the path yet, do this now. Otherwise, set the mask
                        KeyToMonitor = arg.toString();
                        break;
                    }
                }
            }

            System.out.println("Press Enter to start monitoring, then press any key to stop and quit.");
            scanner.nextLine();

            procMonitor.setFilter();
            System.out.println("Press Enter to stop monitoring and quit.");
            scanner.nextLine();
            procMonitor.stopFilter();
        } catch (Exception e) {
            System.out.println("Error in Main: " + e.getMessage());
        }
    }

    private static boolean isDriveLetter(String path) {
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

    private static String ConvertRelativePathToAbsolute(String path) {
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

    public static long getCurrentProcessId() {
        if (currentProcessId != 0)
            return currentProcessId;

        String processName = java.lang.management.ManagementFactory.getRuntimeMXBean().getName();
        if (processName != null && processName.length() > 0) {
            try {
                currentProcessId = Long.parseLong(processName.split("@")[0]);
                return currentProcessId;
            }
            catch (Exception e) {
                return 0;
            }
        }

        return 0;
    }

    private void log(final String text) {
        final long elapsed = System.currentTimeMillis() - started;
        System.out.println("<LogInfo> Message: " + text + ", Time Elapsed: " + elapsed);
    }

    private boolean PreventTerminate(int PID) {
        if (denyTerminate) {
            if ((noTermOpPID != 0) && (PID == noTermOpPID))
                return true;
            String procName;
            try {
                procName = getProcessNameByPID(PID);
            } catch (Exception ex) {
                return false;
            }
            if (procName == null || procName.isEmpty())
                return false;
            return procName.equals(noTermOpName);
        } else {
            return false;
        }
    }

    public static String getProcessNameByPID(int pid) {
        BufferedReader reader = null;
        try {
            Process process = Runtime.getRuntime().exec("tasklist /FI \"PID eq " + pid + "\" /FO CSV");
            reader = new BufferedReader(new InputStreamReader(process.getInputStream()));
            String line;
            while ((line = reader.readLine()) != null) {
                if (line.contains("\"")) {
                    String[] parts = line.split("\",\"");
                    if (parts.length >= 2) {
                        return parts[0].replace("\"", "");
                    }
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
        return null;
    }

    public void processCreation(CbprocessProcessCreationEvent e) {
        // Make sure the process is being started by this app
        if (e.creatingProcessId != getCurrentProcessId()) {
            log(String.format("Process %d created by another app -> no action required", e.processId));
            return;
        }

        log(String.format("Process %d created from %s", e.processId, e.imageFileName));

        // Don't refer to the checkbox directly as the method is called from a secondary thread
        if (!processExecutionDenied)
            log("Process creation allowed");
        else {
            e.resultCode = ERROR_ACCESS_DENIED;
            log("PROCESS CREATION BLOCKED");
        }

        if (denyExecute &&
                (!isNullOrEmpty(noExecOpName) &&
                        e.processName.equalsIgnoreCase(noExecOpName))) {
            e.resultCode = ERROR_ACCESS_DENIED;
        }

        String message = (e.resultCode == ERROR_ACCESS_DENIED) ? "Process creation denied." : "Process created.";
        log( String.format("%s. Id: %d, Process Name: %s", message, e.processId, e.processName));
    }

    public void processHandleOperation(CbprocessProcessHandleOperationEvent e) {
        if (PreventTerminate(e.processId))
        {
            e.desiredAccess &= ~(int)PROCESS_TERMINATE;
            String processName = getProcessNameByPID(e.processId);
            log("Process handle operation; removed PROCESS_TERMINATED flag." + " Id: " + e.processId + ", Name: " + processName);
        }
    }

    public void processTermination(CbprocessProcessTerminationEvent e) {
        log(String.format("Process %d terminated", e.processId));

        stopFilter();
    }

    public void threadCreation(CbprocessThreadCreationEvent e) {
        log(String.format("Thread %d created", e.threadId));
    }

    public void threadHandleOperation(CbprocessThreadHandleOperationEvent e) {
        log(String.format("Thread %d is being opened with rights 0x%08X", e.threadId, e.desiredAccess));

        if (PreventTerminate(e.processId))
        {
            e.desiredAccess &= ~(int)THREAD_TERMINATE;
            String processName = getProcessNameByPID(e.processId);
            log("Thread handle operation; removed PROCESS_TERMINATED flag." + " Id: " + e.processId + ", Name: " + processName);
        }
    }

    public void threadTermination(CbprocessThreadTerminationEvent e) {
        log(String.format("Thread %d terminated", e.threadId));
    }

    public void error(CbprocessErrorEvent e) {

    }

    public void workerThreadCreation(CbprocessWorkerThreadCreationEvent e) {

    }

    public void workerThreadTermination(CbprocessWorkerThreadTerminationEvent e) {

    }
}




