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
import java.io.InputStreamReader;
import java.nio.ByteBuffer;
import java.util.Date;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.regex.Pattern;

import cbfsfilter.*;

/**
 * The storage for global objects (such as contexts) which are returned to a driver
 * and must not be processed by GC until explicitly released.
 */
class globals {
    private static long counter = 0;
    private static final Map<Long, Item> storage = new HashMap<Long, Item>();

    /**
     * Increases the reference counter for a specified global object by its ID.
     * @param id    ID of a stored object to increase the reference counter
     * @return A stored object that corresponds to the specified ID,
     * or {@code null} if no object with the specified ID exists.
     * @see globals#release(long)
     */
    static Object acquire(long id) {
        synchronized (storage) {
            Item item = storage.get(id);
            if (item == null)
                return null;
            item.refs++;
            return item.target;
        }
    }

    /**
     * Adds a new object and initializes its reference counter to 1.
     * @param target An object to add.
     * @return An ID of the stored object.
     * @see globals#free(long)
     */
    static long alloc(Object target) {
        synchronized (storage) {
            long id = ++counter;
            Item item = new Item(target);
            storage.put(id, item);
            return id;
        }
    }

    /**
     * Removes all the global items from the storage.
     */
    static void clear() {
        synchronized (storage) {
            for (long id: storage.keySet()) {
                Item item = storage.get(id);
                item.clear();
            }
            storage.clear();
        }
    }

    /**
     * Removes a stored object by its ID despite the value of its reference counter.
     * @param id ID of an object to remove.
     * @return The removed object, or {@code null} if the specified ID not found.
     * @see globals#alloc(Object)
     */
    static Object free(long id) {
        synchronized (storage) {
            Item item = storage.remove(id);
            if (item == null)
                return null;
            Object target = item.target;
            item.clear();
            return target;
        }
    }

    /**
     * Gets a stored object by its ID. The object's reference counter is not updated.
     * @param id ID of an object to retrieve.
     * @return A stored object that corresponds to the specified ID,
     * or {@code null} if no object with the specified ID exists in the storage.
     * @see globals#set(long, Object)
     */
    static Object get(long id) {
        synchronized (storage) {
            Item item = storage.get(id);
            return (item == null) ? null : item.target;
        }
    }

    /**
     * Decreases the reference counter of an object by its ID in the storage, and removes the object
     * from the storage if its reference counter become 0.
     * @param id An object ID to release.
     * @return The removed object if its reference counter became 0, or {@code null} if either no object
     * with the specified ID exists in the storage or the reference counter for the object is still greater than 0.
     * @see globals#acquire(long)
     */
    static Object release(long id) {
        synchronized (storage) {
            Item item = storage.get(id);
            if (item == null)
                return null;
            if (--item.refs > 0)
                return null;
            storage.remove(id);
            Object target = item.target;
            item.clear();
            return target;
        }
    }

    /**
     * Stores a new object by existing ID. The object's reference counter is not updated.
     * @param id     ID of an object to replace.
     * @param target A new object to store.
     * @return A previously stored object, or {@code null} if either the specified ID not found or
     * the {@code target} is already stored with the specified ID.
     */
    static Object set(long id, Object target) {
        synchronized (storage) {
            Item item = storage.get(id);
            if (item == null)
                return null;
            if (item.target == target)
                return null;
            Object old = item.target;
            item.target = target;
            return old;
        }
    }

    private static class Item {
        int refs;
        Object target;

        Item(Object target) {
            super();
            this.refs = 1;
            this.target = target;
        }

        void clear() {
            refs = 0;
            target = null;
        }
    }
}

public class regmon implements CbregistryEventListener {

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
    private static final char[] hexChars = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'};

    private static final int ERROR_SUCCESS =0;
    private static final int ERROR_FILE_NOT_FOUND = 2;
    private static final int ERROR_ACCESS_DENIED = 5;
    private static final int ERROR_INSUFFICIENT_BUFFER = 122;
    private static final int ERROR_MORE_DATA = 234;
    private static final int ERROR_NO_MORE_ITEMS = 259;
    private static final int ERROR_REPARSE = 741;
    private static final int ERROR_PRIVILEGE_NOT_HELD = 1314;

    private static final String keyHKLM = "\\registry\\machine\\";
    private static final String keyHKU = "\\registry\\user\\";
    private static final String keyHKCU;
    private static final String keyHKCR;
    private static final String templateHKCU = "\\registry\\user\\%s\\";
    private static final String templateHKCR = "\\registry\\user\\%s_classes\\";

    private static final Pattern DRIVE_LETTER_PATTERN = Pattern.compile("^[A-Za-z]:$");

    private boolean driverRunning;
    private boolean filterWorking;
    private String cabFileLocation;

    private Cbregistry filter;
    private AtomicInteger eventsCount;
    private long ticksStarted;

    private regmon() {
        eventsCount = new AtomicInteger();

        Cbregistry filter = createFilter();
        checkDriver(filter);
        disposeFilter(filter);
    }

    private Cbregistry createFilter() {
        Cbregistry filter = new Cbregistry();
        try {
            filter.addCbregistryEventListener(this);
        }
        catch (Exception err) {
            disposeFilter(filter);
            filter = null;
        }
        return filter;
    }

    private void disposeFilter(Cbregistry filter) {
        if (filter == null)
            return;

        try {
            filter.dispose();
        }
        catch (CBFSFilterException inner) {
            //
        }
    }

    private void checkDriver(Cbregistry filter) {
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

    static {
        String sid = getCurrentUserSid();
        if (isNullOrEmpty(sid)) {
            keyHKCU = null;
            keyHKCR = null;
        }
        else {
            String lowered = sid.toLowerCase();
            keyHKCU = String.format(templateHKCU, lowered);
            keyHKCR = String.format(templateHKCR, lowered);
        }
    }
    private static String getCurrentUserSid() {
        String command = "wmic useraccount where name='" + System.getProperty("user.name") + "' get sid";
        try {
            Process process = Runtime.getRuntime().exec(command);
            BufferedReader input = new BufferedReader(new InputStreamReader(process.getInputStream()));
            String line;
            int i = 0;
            while ((line = input.readLine()) != null) {
                line = line.trim();
                if (line.startsWith("S-")) {
                    return line;
                }
            }
            input.close();
        }
        catch (Exception ignored) {
        }
        return null;
    }

    private static boolean isNullOrEmpty(String s) {
        return s == null || s.length() == 0;
    }

    private static void banner() {
        System.out.println("CBFS Filter Registry Monitor Demo (c) 2017-2023, Callback Technologies, Inc.\n");
    }

    private static void usage() {
        System.out.println("Usage: regmon [-<switch 1> ... -<switch N>] [<process name whose requests should be filtered>]\n");
        System.out.println("<Switches>");
        System.out.println("  -drv <path to cbregistry.cab> - Install the driver from the specified CAB file\n");
        System.out.println("Example: regmon notepad.exe");
    }

    private void install(String fileName) {
        boolean rebootNeeded = false;

        Cbregistry filter = createFilter();
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

    private void setFilter(String processName) {
        if (isNullOrEmpty(processName)) {
            System.out.println("No process specified for filtering");
            return;
        }

        eventsCount.set(0);
        ticksStarted = System.currentTimeMillis();

        filter = createFilter();
        try {
            filter.initialize(PRODUCT_GUID);

            filter.setSerializeEvents(1);
            filter.startFilter(30000);
            filter.addFilteredProcessByName(processName, false);
        }
        catch (CBFSFilterException err) {
            disposeFilter(filter);
            filter = null;
            System.out.println("<Error> Filter not started or filtered process not added. Error message: " + err.getMessage());
            return;
        }

        filterWorking = true;
    }

    private void stopFilter() {
        if (filter == null)
            return;
        try {
            filter.stopFilter();
            filter.dispose();
            filter = null;
        }
        catch (CBFSFilterException inner) {
            //
        }
        filterWorking = false;
    }

    public static void main(String[] args) {
        boolean drvReboot = false;
        boolean optTerminate = false;

        String processName = null;

        int argi, argLen;
        boolean stop_opt = false;

        Scanner scanner = new Scanner(System.in);

        banner();

        try {
            regmon regMonitor = new regmon();
            // if no parameters specified, show usage and quit
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
                            arg = ConvertRelativePathToAbsolute(args[++argi]);
                            if (argi < args.length) {
                                regMonitor.install(arg);
                            }
                        } else {
                            System.out.println("Error: Invalid option " + arg);
                        }
                    } else {
                        processName = arg;
                        break;
                    }
                }
            }

            // Probably, we have just installed the driver, so we can quit without monitoring anything
            if (processName == null || processName.isEmpty())
                return;

            System.out.println("Press Enter to start monitoring, then press any key to stop and quit.");
            scanner.nextLine();

            regMonitor.setFilter(processName);
            System.out.println("Press Enter to stop monitoring and quit.");
            scanner.nextLine();
            regMonitor.stopFilter();
        } catch (Exception ex) {
            System.out.println("Error in main: " + ex.getMessage());
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

    private String ntKeyNameToWinApiKeyname(String name) {
        if (isNullOrEmpty(name))
            return "";

        String loweredName = name.toLowerCase();

        if (keyHKLM.startsWith(loweredName))
            return "HKLM";

        if (keyHKCU.startsWith(loweredName))
            return "HKCU";

        if (keyHKCR.startsWith(loweredName))
            return "HKCR";

        if (keyHKU.startsWith(loweredName))
            return "HKU";

        if (loweredName.startsWith(keyHKLM))
            return "HKLM\\" + name.substring(keyHKLM.length());

        if (loweredName.startsWith(keyHKCU))
            return "HKCU\\" + name.substring(keyHKCU.length());

        if (loweredName.startsWith(keyHKCR))
            return "HKCR\\" + name.substring(keyHKCR.length());

        if (loweredName.startsWith(keyHKU))
            return "HKU\\" + name.substring(keyHKU.length());

        return name;
    }

    private String resultToStr(int result) {
        switch (result) {
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
                return String.valueOf(result);
        }
    }

    private void log(final String operation, final String name, final int result, final String details) {
        final int number = eventsCount.incrementAndGet();
        final long elapsed = System.currentTimeMillis() - ticksStarted;
        System.out.println(String.format("<LogInfo> Number: %d, Time Elapsed: %d, Operation: %s, Name: %s, Result: %s, Details: %s",
                number, elapsed, operation, ntKeyNameToWinApiKeyname(name), resultToStr(result), details));
    }

    private static boolean isBitSet(int value, int bit) {
        return (value & bit) != 0;
    }

    private String valueTypeToStr(int valueType) {
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
                return String.format("unknown (%d)", valueType);
        }
    }

    private static String replaceChar(String s, char original, char replaceWith) {
        if (isNullOrEmpty(s))
            return s;
        char[] chars = s.toCharArray();
        for (int i = 0; i < chars.length; i++) {
            if (chars[i] == original)
                chars[i] = replaceWith;
        }
        return new String(chars);
    }

    private static String dumpBinaryValue(ByteBuffer data, int size, int maxSize) {
        if (data == null || size == 0 || maxSize == 0)
            return "";

        int count = Math.min(size, maxSize);
        char[] result;
        if (count >= size)
            result = new char[count * 3 - 1];
        else {
            // append '...' to the end
            result = new char[count * 3 + 2];
            int k = result.length - 1;
            result[k--] = '.';
            result[k--] = '.';
            result[k] = '.';
        }
        int j = 0;
        for (int i = 0; i < count; i++) {
            int b = data.get() & 0xff;
            result[j++] = hexChars[b >>> 4];
            result[j++] = hexChars[b & 0x0f];
            if (j < result.length && result[j] != '.')
                result[j++] = ' ';
        }
        return new String(result);
    }

    private static String dumpBinaryValue(byte[] data, int maxSize) {
        if (data == null || data.length == 0 || maxSize == 0)
            return "";
        return dumpBinaryValue(ByteBuffer.wrap(data), data.length, maxSize);
    }

    private static class KeyContext {
        String keyName;
        int access;
        String newName;
        Date lastWriteTime;
        String valueName;
        int valueType;
        long integerValue;
        String stringValue;
        byte[] binaryValue;

        KeyContext(String keyName, int access) {
            this.keyName = keyName;
            this.access = access;
        }

        void clearValue() {
            valueName = null;
            valueType = 0;
            integerValue = 0;
            stringValue = null;
            binaryValue = null;
        }

        void setBinaryValue(ByteBuffer data, int size) {
            if (data == null || size == 0) {
                binaryValue = null;
                return;
            }

            binaryValue = new byte[size];
            data.get(binaryValue);
        }
    }

    private KeyContext getContext(long id) {
        if (id == 0)
            return null;
        Object context = globals.get(id);
        if (!(context instanceof KeyContext))
            return null;
        return (KeyContext)context;
    }

    public void afterCloseKey(CbregistryAfterCloseKeyEvent e) {
        KeyContext context = getContext(e.keyContext);
        if (context == null)
            return;
        log("CloseKey", context.keyName, ERROR_SUCCESS, null);
    }

    public void afterCreateKey(CbregistryAfterCreateKeyEvent e) {
        KeyContext context = getContext(e.keyContext);
        if (context == null)
            return;
        String details = String.format("Access: 0x%X", (e.grantedAccess == 0) ? context.access : e.grantedAccess);
        log("CreateKey", context.keyName, e.status, details);
    }

    public void afterDeleteKey(CbregistryAfterDeleteKeyEvent e) {
        KeyContext context = getContext(e.keyContext);
        if (context == null)
            return;
        log("DeleteKey", context.keyName, e.status, null);
    }

    public void afterDeleteValue(CbregistryAfterDeleteValueEvent e) {
        KeyContext context = getContext(e.keyContext);
        if (context == null)
            return;
        log("DeleteValue", context.keyName + "\\" + context.valueName, e.status, null);
        context.valueName = null;
    }

    public void afterEnumerateKey(CbregistryAfterEnumerateKeyEvent e) {
        if (e.status == ERROR_NO_MORE_ITEMS)
            return;

        KeyContext context = getContext(e.keyContext);
        if (context == null)
            return;

        StringBuilder sb = new StringBuilder();
        if (e.status == ERROR_SUCCESS) {
            sb.append(e.index);
            sb.append(": ");
            if (isBitSet(e.validFields, Constants.REG_KEYFIELD_NAME))
                sb.append(e.name);
            if (isBitSet(e.validFields, Constants.REG_KEYFIELD_SUBKEYS))
                sb.append(String.format(", Subkeys: %d", e.subKeys));
            if (isBitSet(e.validFields, Constants.REG_KEYFIELD_VALUES))
                sb.append(String.format(", Values: %d", e.values));
        }
        log("EnumKey", context.keyName, e.status, sb.toString());
    }

    public void afterEnumerateValue(CbregistryAfterEnumerateValueEvent e) {
        if (e.status == ERROR_NO_MORE_ITEMS)
            return;

        KeyContext context = getContext(e.keyContext);
        if (context == null)
            return;

        StringBuilder sb = new StringBuilder();
        sb.append(e.index);
        sb.append(": ");
        if (isBitSet(e.validFields, Constants.REG_VALUEFIELD_NAME))
            sb.append(isNullOrEmpty(e.valueName) ? "(Default)" : e.valueName);
        if (isBitSet(e.validFields, Constants.REG_VALUEFIELD_TYPE)) {
            sb.append("; Type: ");
            sb.append(valueTypeToStr(e.valueType));
            if (isBitSet(e.validFields, Constants.REG_VALUEFIELD_DATA)) {
                sb.append("; Data: ");
                switch (e.valueType) {
                    case Constants.REG_VALUETYPE_SZ:
                    case Constants.REG_VALUETYPE_EXPAND_SZ:
                        if (!isNullOrEmpty(e.stringValue))
                            sb.append(e.stringValue);
                        break;

                    case Constants.REG_VALUETYPE_BINARY:
                        sb.append(String.format("[%d] %s", e.binaryValueSize, dumpBinaryValue(e.binaryValue, e.binaryValueSize, 20)));
                        break;

                    case Constants.REG_VALUETYPE_DWORD:
                    case Constants.REG_VALUETYPE_QWORD:
                        sb.append(e.integerValue);
                        break;

                    case Constants.REG_VALUETYPE_MULTI_SZ:
                        if (!isNullOrEmpty(e.stringValue))
                            sb.append(replaceChar(e.stringValue, '\u0017', ' '));
                        break;
                }
            }
        }
        log("EnumValue", context.keyName, e.status, sb.toString());
    }

    @Override
    public void afterGetKeySecurity(CbregistryAfterGetKeySecurityEvent cbregistryAfterGetKeySecurityEvent) {

    }

    public void afterOpenKey(CbregistryAfterOpenKeyEvent e) {
        KeyContext context = getContext(e.keyContext);
        if (context == null)
            return;
        String details = String.format("Access: 0x%X", (e.grantedAccess == 0) ? context.access : e.grantedAccess);
        log("OpenKey", context.keyName, e.status, details);
    }

    public void afterQueryKey(CbregistryAfterQueryKeyEvent e) {
        KeyContext context = getContext(e.keyContext);
        if (context == null)
            return;

        StringBuilder sb = new StringBuilder();
        if (e.status == ERROR_SUCCESS) {
            if (isBitSet(e.validFields, Constants.REG_KEYFIELD_SUBKEYS))
                sb.append(String.format("Subkeys: %d", e.subKeys));
            if (isBitSet(e.validFields, Constants.REG_KEYFIELD_VALUES)) {
                if (sb.length() != 0)
                    sb.append(", ");
                sb.append(String.format("Values: %d", e.values));
            }
        }
        log("QueryKey", context.keyName, e.status, sb.toString());
    }

    public void afterQueryValue(CbregistryAfterQueryValueEvent e) {
        KeyContext context = getContext(e.keyContext);
        if (context == null)
            return;

        StringBuilder sb = new StringBuilder();
        if (isBitSet(e.validFields, Constants.REG_VALUEFIELD_TYPE)) {
            sb.append("Type: ");
            sb.append(valueTypeToStr(e.valueType));
            if (isBitSet(e.validFields, Constants.REG_VALUEFIELD_DATA)) {
                sb.append("; Data: ");
                switch (e.valueType) {
                    case Constants.REG_VALUETYPE_SZ:
                    case Constants.REG_VALUETYPE_EXPAND_SZ:
                        if (!isNullOrEmpty(e.stringValue))
                            sb.append(e.stringValue);
                        break;

                    case Constants.REG_VALUETYPE_BINARY:
                        sb.append(String.format("[%d] %s", e.binaryValueSize, dumpBinaryValue(e.binaryValue, e.binaryValueSize, 20)));
                        break;

                    case Constants.REG_VALUETYPE_DWORD:
                    case Constants.REG_VALUETYPE_QWORD:
                        sb.append(e.integerValue);
                        break;

                    case Constants.REG_VALUETYPE_MULTI_SZ:
                        if (!isNullOrEmpty(e.stringValue))
                            sb.append(replaceChar(e.stringValue, '\u0017', ' '));
                        break;
                }
            }
        }
        log("QueryValue",
                context.keyName + "\\" + (isNullOrEmpty(e.valueName) ? "(Default)" : e.valueName),
                e.status, sb.toString());
    }

    public void afterRenameKey(CbregistryAfterRenameKeyEvent e) {
        KeyContext context = getContext(e.keyContext);
        if (context == null)
            return;
        String details = String.format("New name: %s", context.newName);
        context.newName = null;
        log("RenameKey", context.keyName, e.status, details);
    }

    public void afterSetKey(CbregistryAfterSetKeyEvent e) {
        KeyContext context = getContext(e.keyContext);
        if (context == null)
            return;
        String details = String.format("Last write time: %s", context.lastWriteTime);
        context.lastWriteTime = null;
        log("SetKey", context.keyName, e.status, details);
    }

    @Override
    public void afterSetKeySecurity(CbregistryAfterSetKeySecurityEvent cbregistryAfterSetKeySecurityEvent) {

    }

    public void afterSetValue(CbregistryAfterSetValueEvent e) {
        KeyContext context = getContext(e.keyContext);
        if (context == null)
            return;

        StringBuilder sb = new StringBuilder();
        if (context.valueType != 0) {
            sb.append("Type: ");
            sb.append(valueTypeToStr(context.valueType));
            sb.append("; Data: ");
            switch (context.valueType) {
                case Constants.REG_VALUETYPE_SZ:
                case Constants.REG_VALUETYPE_EXPAND_SZ:
                    if (!isNullOrEmpty(context.stringValue))
                        sb.append(context.stringValue);
                    break;

                case Constants.REG_VALUETYPE_BINARY:
                    sb.append(String.format("[%d] %s",
                            (context.binaryValue == null) ? 0 : context.binaryValue.length,
                            dumpBinaryValue(context.binaryValue, 20)));
                    break;

                case Constants.REG_VALUETYPE_DWORD:
                case Constants.REG_VALUETYPE_QWORD:
                    sb.append(context.integerValue);
                    break;

                case Constants.REG_VALUETYPE_MULTI_SZ:
                    if (!isNullOrEmpty(context.stringValue))
                        sb.append(replaceChar(context.stringValue, '\u0017', ' '));
                    break;
            }
        }
        log("SetValue", isNullOrEmpty(context.valueName) ? "(Default)" : context.valueName, e.status, sb.toString());
        context.clearValue();
    }

    public void beforeCloseKey(CbregistryBeforeCloseKeyEvent e) {
        // not needed in this demo
    }

    public void beforeCreateKey(CbregistryBeforeCreateKeyEvent e) {
        e.keyContext = globals.alloc(new KeyContext(e.fullName, e.desiredAccess));
    }

    public void beforeDeleteKey(CbregistryBeforeDeleteKeyEvent e) {
        // not needed in this demo
    }

    public void beforeDeleteValue(CbregistryBeforeDeleteValueEvent e) {
        KeyContext context = getContext(e.keyContext);
        if (context == null)
            return;
        context.valueName = e.valueName;
    }

    public void beforeEnumerateKey(CbregistryBeforeEnumerateKeyEvent e) {
        // not needed in this demo
    }

    public void beforeEnumerateValue(CbregistryBeforeEnumerateValueEvent e) {
        // not needed in this demo
    }

    @Override
    public void beforeGetKeySecurity(CbregistryBeforeGetKeySecurityEvent cbregistryBeforeGetKeySecurityEvent) {

    }

    public void beforeOpenKey(CbregistryBeforeOpenKeyEvent e) {
        e.keyContext = globals.alloc(new KeyContext(e.fullName, e.desiredAccess));
    }

    public void beforeQueryKey(CbregistryBeforeQueryKeyEvent e) {
        // not needed in this demo
    }

    public void beforeQueryValue(CbregistryBeforeQueryValueEvent e) {
        // not needed in this demo
    }

    public void beforeRenameKey(CbregistryBeforeRenameKeyEvent e) {
        KeyContext context = getContext(e.keyContext);
        if (context == null)
            return;
        context.newName = e.newName;
    }

    public void beforeSetKey(CbregistryBeforeSetKeyEvent e) {
        KeyContext context = getContext(e.keyContext);
        if (context == null)
            return;
        context.lastWriteTime = e.lastWriteTime;
    }

    @Override
    public void beforeSetKeySecurity(CbregistryBeforeSetKeySecurityEvent cbregistryBeforeSetKeySecurityEvent) {

    }

    public void beforeSetValue(CbregistryBeforeSetValueEvent e) {
        KeyContext context = getContext(e.keyContext);
        if (context == null)
            return;
        context.valueName = e.valueName;
        context.valueType = e.valueType;
        context.integerValue = e.integerValue;
        context.stringValue = e.stringValue;
        context.setBinaryValue(e.binaryValue, e.binaryValueSize);
    }

    public void cleanupKeyContext(CbregistryCleanupKeyContextEvent e) {
        if (e.keyContext != 0) {
            globals.free(e.keyContext);
            e.keyContext = 0;
        }
    }

    public void closeKeyHandle(CbregistryCloseKeyHandleEvent e) {
        // not needed in this demo
    }

    public void error(CbregistryErrorEvent e) {
        System.err.println(String.format("UNEHANDLED EXCEPTION: (%d) %s", e.errorCode, e.description));
    }

    public void workerThreadCreation(CbregistryWorkerThreadCreationEvent e) {
        // not needed in this demo
    }

    public void workerThreadTermination(CbregistryWorkerThreadTerminationEvent e) {
        // not needed in this demo
    }

}




