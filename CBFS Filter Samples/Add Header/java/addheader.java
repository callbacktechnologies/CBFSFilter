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
import javax.swing.*;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;
import javax.swing.filechooser.FileNameExtensionFilter;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.EOFException;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.TooManyListenersException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.regex.Pattern;

import cbfsfilter.*;

public class addheader implements cbfsfilter.CbfilterEventListener {
    private Cbfilter cbfFilter = new Cbfilter();

    public static final String appName = "Add Header";
    private final String guid = "{713CC6CE-B3E2-4fd9-838D-E28F558F6866}";
    private final String ALTITUDE_FAKE_VALUE_FOR_DEBUG = "149995";
    private final int ERROR_SUCCESS = 0;
    private final int ERROR_HANDLE_EOF = 38;
    private final int ERROR_SHARING_VIOLATION = 32;
    private final int ERROR_ACCESS_DENIED = 5;
    private final int ERROR_PRIVILEGE_NOT_HELD = 1314;
    private final int FILE_ATTRIBUTE_DIRECTORY = 0x00000010;
    private final int ENCRYPTED_FILE = 111;
    private final int DEFAULT_HEADER_SIZE = 0x1000;

    private final int FILE_READ_DATA = 0x0001;
    private final int FILE_WRITE_DATA = 0x0002;
    private final int FILE_APPEND_DATA = 0x0004;

    private final int DELETE = 0x00010000;
    private final int WRITE_DAC = 0x00040000;
    private final int WRITE_OWNER = 0x00080000;

    private final int FILE_READ_EA = 0x0008;
    private final int FILE_WRITE_EA = 0x0010;

    private final int FILE_READ_ATTRIBUTES = 0x0080;
    private final int FILE_WRITE_ATTRIBUTES = 0x0100;

    public static final int OPEN_EXISTING = 3;
    public static final long GENERIC_READ = -2147483648L;
    public static final int GENERIC_WRITE = 1073741824;
    public static final int FILE_FLAG_NO_BUFFERING = 536870912;
    public static final int CREATE_ALWAYS = 2;
    public static final int TRUNCATE_EXISTING = 5;

    private static final Pattern DRIVE_LETTER_PATTERN = Pattern.compile("^[A-Za-z]:$");

    private JPanel panelRoot;
    private JButton btnInstall;
    private JButton btnUninstall;
    private JTextField textPath;
    private JButton btnClearScreen;
    private JButton btnSetFilter;
    private JButton btnDeleteFilter;
    private JLabel lblDriverStatus;
    private JScrollPane tableScroll;
    private JCheckBox chkAutoScroll;
    private JList listLog;
    private JCheckBox chkLogging;
    private JCheckBox chkCached;

    private DefaultListModel listModel;
    private HashMap<String, EncryptContext> encryptedFiles;
    private boolean closing;

    public addheader() {

        encryptedFiles = new HashMap<String, EncryptContext>();

        btnClearScreen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                onClearLogClick();
            }
        });
        btnInstall.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                onInstallClick();
            }
        });
        btnUninstall.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                onUninstallClick();
            }
        });
        btnSetFilter.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                onSetFilterClick();
            }
        });
        btnDeleteFilter.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                onDeleteFilterClick();
            }
        });
    }

    public static void main(String[] args) {
        JFrame frame = new JFrame(addheader.appName);
        addheader addheaderDemo = new addheader();
        frame.setContentPane(addheaderDemo.panelRoot);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        addheaderDemo.initComponent(frame);
        frame.pack();
        frame.setVisible(true);
    }


    public void initComponent(JFrame frame) {
        listModel = new DefaultListModel();
        listLog.setModel(listModel);
        listModel.addListDataListener(new ListDataListener() {
            @Override
            public void intervalAdded(ListDataEvent e) {
                btnClearScreen.setEnabled(listModel.getSize() > 0);
            }

            @Override
            public void intervalRemoved(ListDataEvent e) {
                btnClearScreen.setEnabled(listModel.getSize() > 0);
            }

            @Override
            public void contentsChanged(ListDataEvent e) {
                btnClearScreen.setEnabled(listModel.getSize() > 0);
            }
        });

        try {
            cbfFilter.addCbfilterEventListener(this);
        } catch (TooManyListenersException e) {
            e.printStackTrace();
        }

        frame.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                onFormClosing();
            }
        });
        updateButtons();
    }


    protected void showMessageBox(String message) {
        JOptionPane.showMessageDialog(null, message, appName, JOptionPane.INFORMATION_MESSAGE);
    }

    protected void updateButtons() {
        try {
            int moduleStatus = cbfFilter.getDriverStatus(guid);
            long moduleVersion = cbfFilter.getDriverVersion(guid);
            int versionHigh = (int)(moduleVersion >> 32);
            int versionLow = (int)(moduleVersion & 0xFFFFFFFF);

            btnSetFilter.setEnabled(!cbfFilter.isActive() && moduleStatus != 0);
            btnDeleteFilter.setEnabled(cbfFilter.isActive());
            btnInstall.setEnabled(moduleStatus == 0);
            btnUninstall.setEnabled(moduleStatus != 0);
            btnClearScreen.setEnabled(listModel.getSize() > 0);
            if (moduleStatus != 0)
                lblDriverStatus.setText(String.format("Driver version: %d.%d.%d.%d",
                        versionHigh >> 16, versionHigh & 0xFFFF, versionLow >> 16, versionLow & 0xFFFF));
            else
                lblDriverStatus.setText("Driver: not installed");
        } catch (CBFSFilterException e) {
            showMessageBox(e.getMessage());
        }
    }


    protected void onInstallClick() {
        JFileChooser fc = new JFileChooser();
        fc.setCurrentDirectory(new File(System.getProperty("user.dir")));
        FileNameExtensionFilter filter = new FileNameExtensionFilter(
                "CBFilter driver package", "cab");
        fc.setFileFilter(filter);
        fc.setSelectedFile(new File("cbfilter.cab"));
        int returnVal = fc.showDialog(null, "Install");
        if (returnVal != JFileChooser.APPROVE_OPTION)
            return;
        if (!fc.getSelectedFile().exists())
            return;
        try  {
            boolean reboot = cbfFilter.install(fc.getSelectedFile().getPath(), guid, "", ALTITUDE_FAKE_VALUE_FOR_DEBUG, 0);

            updateButtons();

            if (reboot)
                showMessageBox("Please, reboot the system for the changes to take effect");
            else
                showMessageBox("Driver installed successfully");

        } catch (CBFSFilterException e) {
            if (e.getCode() == ERROR_PRIVILEGE_NOT_HELD)
                showMessageBox("Installation requires administrator rights. Run the app as administrator");
            else
                showMessageBox(e.getMessage());
        }
    }

    protected void onUninstallClick() {
        JFileChooser fc = new JFileChooser();
        fc.setCurrentDirectory(new File(System.getProperty("user.dir")));
        FileNameExtensionFilter filter = new FileNameExtensionFilter(
                "CBFilter driver package", "cab");
        fc.setFileFilter(filter);
        fc.setSelectedFile(new File("cbfilter.cab"));
        int returnVal = fc.showDialog(null, "Uninstall");
        if (returnVal != JFileChooser.APPROVE_OPTION)
            return;
        if (!fc.getSelectedFile().exists())
            return;
        try {
            boolean reboot = cbfFilter.uninstall(fc.getSelectedFile().getPath(), guid, "", 0);

            updateButtons();

            if (reboot)
                showMessageBox("Please, reboot the system for the changes to take effect");
            else
                showMessageBox("Driver uninstalled successfully");

        }
        catch (CBFSFilterException e) {
            if (e.getCode() == ERROR_PRIVILEGE_NOT_HELD)
                showMessageBox("Uninstallation requires administrator rights. Run the app as administrator");
            else
                showMessageBox(e.getMessage());
        }
    }

    protected void onSetFilterClick() {
        String filterPath = ConvertRelativePathToAbsolute(textPath.getText());
        long callbacks = Constants.FS_CE_BEFORE_READ |
                Constants.FS_CE_AFTER_READ |
                Constants.FS_CE_BEFORE_WRITE |
                Constants.FS_CE_BEFORE_CREATE |
                Constants.FS_CE_AFTER_CREATE |
                Constants.FS_CE_AFTER_RENAME |
                Constants.FS_CE_BEFORE_SET_SIZES |
                Constants.FS_CE_AFTER_ENUMERATE_DIRECTORY |
                Constants.FS_CE_BEFORE_OPEN |
                Constants.FS_CE_AFTER_OPEN |
                Constants.FS_CE_BEFORE_CLOSE |
                Constants.FS_CE_AFTER_GET_SIZES;
        try {
            cbfFilter.addFilterRuleEx(filterPath,
                    null,
                    Constants.ACCESS_NONE,
                    Constants.FS_CE_AFTER_ENUMERATE_DIRECTORY,
                    Constants.FS_NE_NONE,
                    -1, -1, FILE_ATTRIBUTE_DIRECTORY, 0
            );

            cbfFilter.addFilterRuleEx(filterPath + "\\*.*",
                    null,
                    Constants.ACCESS_NONE,
                    callbacks,
                    Constants.FS_NE_NONE,
                    -1, -1, 0, FILE_ATTRIBUTE_DIRECTORY );

            // Add additional bytes to the write buffer in the corresponding events.
            // They are needed for the hidden file header implementation.
            // This value must be a multiple of the disk sector size
            cbfFilter.addBytesToWriteBuffer(DEFAULT_HEADER_SIZE);

            // When encryption is done without a hidden header,
            // you don't need to handle cached read/write operations.
            // Cached operations support should be enabled to support
            // an additional hidden header in files in the filtered directory
            cbfFilter.setProcessCachedIORequests(chkCached.isSelected());

            cbfFilter.initialize(guid);
            cbfFilter.config("AllowFileAccessInBeforeOpen=false");
            cbfFilter.config("ModifiableReadWriteBuffers=true");
            cbfFilter.config("CacheRemoteFilesLocally=true");
            cbfFilter.startFilter(0);
            cbfFilter.setFileFlushingBehavior(Constants.FS_SUPPORT_FILE_ENCRYPTION);

//            uint SectorsPerCluster, NumberOfFreeClusters, TotalNumberOfClusters;
//
//            string DiskName = textPath.Text.Substring(0, 3);
//
//            if (!GetDiskFreeSpace(DiskName, out SectorsPerCluster, out BYTES_PER_SECTOR,
//                    out NumberOfFreeClusters, out TotalNumberOfClusters))
//            {
//                MessageBox.Show(this, "Please specify filtered path related to local disk name", "CBFS Filter", MessageBoxButtons.OK, MessageBoxIcon.Error);
//                return;
//            }
        } catch (CBFSFilterException e) {
            showMessageBox(e.getMessage());
        }
        updateButtons();
    }

    protected void onDeleteFilterClick()
    {
        try {
            cbfFilter.deleteAllFilterRules();
            cbfFilter.deleteAllPassthroughRules();
            if (cbfFilter.isActive())
                cbfFilter.stopFilter(false);
            addToLog("Filter stopped.");
            btnDeleteFilter.setEnabled(false);
            btnSetFilter.setEnabled(true);
        } catch (CBFSFilterException e) {
            showMessageBox(e.getMessage());
        }
    }

    protected void addToLog(String value) {
        if (closing) return;

        System.out.println(value);

        if (listModel.getSize() > 1000)
            listModel.removeAllElements();
        listModel.add(listModel.getSize(), value);

        if (this.chkAutoScroll.isSelected()) {
            listLog.ensureIndexIsVisible(listModel.getSize() - 1);
        }
    }

    private boolean isNullOrEmpty(String s) {
        if (s == null)
            return true;
        if (s.length() == 0)
            return true;
        return false;
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

    private String getTitle() {
        return appName;
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
                    JOptionPane.showMessageDialog(null,
                            "The network folder '" + res + "' does not exist.", getTitle(), JOptionPane.ERROR_MESSAGE);
                } else if (!file.exists()) {
                    JOptionPane.showMessageDialog(null,
                            "The path '" + res + "' does not exist.", getTitle(), JOptionPane.ERROR_MESSAGE);
                }
            }
        }
        return res;
    }

    protected void onClearLogClick() {
        listModel.removeAllElements();
    }

    protected void onFormClosing() {
        closing = true;

        if (cbfFilter.isActive()) {
            try {
                cbfFilter.dispose();
            } catch (CBFSFilterException e) {
                showMessageBox(e.getMessage());
            }
        }
    }

    @Override
    public void cleanupContext(CbfilterCleanupContextEvent e) {

    }

    @Override
    public void afterCanFileBeDeleted(CbfilterAfterCanFileBeDeletedEvent e) {
        addToLog(String.format("afterCanFileBeDeleted %s", e.fileName));
    }

    @Override
    public void afterCleanupFile(CbfilterAfterCleanupFileEvent e) {
        addToLog(String.format("afterCleanupFile %s", e.fileName));
    }

    @Override
    public void afterCloseEnumeration(CbfilterAfterCloseEnumerationEvent e) {
        addToLog(String.format("afterCloseEnumeration %s", e.directoryName));
    }

    @Override
    public void afterCloseFile(CbfilterAfterCloseFileEvent e) {
        addToLog(String.format("afterCloseFile %s", e.fileName));
    }

    @Override
    public void afterCreateFile(CbfilterAfterCreateFileEvent e) {

        EncryptContext context = null;
        addToLog(String.format("afterCreateFile %s", e.fileName));

        if (e.fileContext != ENCRYPTED_FILE) {
            if (0 == (e.attributes & FILE_ATTRIBUTE_DIRECTORY)) {
                context = new EncryptContext(cbfFilter, e.fileName, e.desiredAccess, DEFAULT_HEADER_SIZE,false);
                if (context.isInitialized()) {
                    encryptedFiles.put(e.fileName.toLowerCase(), context);
                    e.fileContext = ENCRYPTED_FILE;
                }
                else {
                    e.fileContext = 0;
                    context.close();
                    return;
                }
            }
        }
        else {
            context = encryptedFiles.get(e.fileName.toLowerCase());
            //enable this code if you have closed file during rename callback
            context.openFile(e.fileName);
            context.incrementRef();
        }
        if (e.createDisposition == CREATE_ALWAYS || e.createDisposition == TRUNCATE_EXISTING) {

            context.setCurrentSize(0);
            try {
                context.writeHeader();
            } catch (IOException ex) {
                ex.printStackTrace();
            }
        }
    }

    @Override
    public void afterCreateHardLink(CbfilterAfterCreateHardLinkEvent e) {
        addToLog(String.format("afterCreateHardLink %s", e.fileName));
    }

    @Override
    public void afterDeleteFile(CbfilterAfterDeleteFileEvent e) {
        addToLog(String.format("afterDeleteFile %s", e.fileName));
    }

    @Override
    public void afterDeleteReparsePoint(CbfilterAfterDeleteReparsePointEvent e) {
        addToLog(String.format("afterDeleteReparsePoint %s", e.fileName));
    }

    @Override
    public void afterEnumerateDirectory(CbfilterAfterEnumerateDirectoryEvent e) {
        //addToLog(String.format("afterEnumerateDirectory %s", e.fileName));
        // assumed that all files in directory are filtered

        if (e.size >= DEFAULT_HEADER_SIZE)
            e.size -= DEFAULT_HEADER_SIZE;

        //e.FileFound = true;
    }

    @Override
    public void afterFilterAttachToVolume(CbfilterAfterFilterAttachToVolumeEvent e) {
        addToLog(String.format("afterFilterAttachToVolume %s", e.volumeName));
    }

    @Override
    public void afterFilterDetachFromVolume(CbfilterAfterFilterDetachFromVolumeEvent e) {
        addToLog(String.format("afterFilterDetachFromVolume %s", e.volumeName));
    }

    @Override
    public void afterFsctl(CbfilterAfterFsctlEvent e) {
        addToLog(String.format("afterFsctl %s", e.fileName));
    }

    @Override
    public void afterGetFileSecurity(CbfilterAfterGetFileSecurityEvent e) {
        addToLog(String.format("afterGetFileSecurity %s", e.fileName));
    }

    @Override
    public void afterGetFileSizes(CbfilterAfterGetFileSizesEvent e) {
        addToLog(String.format("afterGetFileSizes %s Size(%d) AllocationSize(%d)", e.fileName, e.size, e.allocationSize));

        if (e.fileContext == ENCRYPTED_FILE) {
            EncryptContext context = encryptedFiles.get(e.fileName.toLowerCase());
            if (context.isHeaderPresent()) {
                e.size -= context.getHeaderSize();
                e.allocationSize -= context.getHeaderSize();
            }
        }
    }

    @Override
    public void afterGetReparsePoint(CbfilterAfterGetReparsePointEvent e) {
        addToLog(String.format("afterGetReparsePoint %s", e.fileName));
    }

    @Override
    public void afterIoctl(CbfilterAfterIoctlEvent e) {
        addToLog(String.format("afterIoctl %s", e.fileName));
    }

    @Override
    public void afterLock(CbfilterAfterLockEvent e) {
        addToLog(String.format("afterLock %s", e.fileName));
    }

    @Override
    public void afterOpenFile(CbfilterAfterOpenFileEvent e) {

        EncryptContext context = null;
        addToLog(String.format("afterOpenFile %s attr %d ctx %d", e.fileName, e.attributes, e.fileContext));

        if (e.fileContext != ENCRYPTED_FILE) {
            if (0 == (e.attributes & FILE_ATTRIBUTE_DIRECTORY)) {
                context = new EncryptContext(cbfFilter, e.fileName, e.desiredAccess, DEFAULT_HEADER_SIZE, false);
                if (context.isInitialized()) {
                    encryptedFiles.put(e.fileName.toLowerCase(), context);
                    e.fileContext = ENCRYPTED_FILE;
                }
                else {
                    e.fileContext = 0;
                    context.close();
                    return;
                }
            }
        }
        else {
            context = encryptedFiles.get(e.fileName.toLowerCase());
            //enable this code if you have closed file during rename callback
            context.openFile(e.fileName);
            context.incrementRef();
        }
        if (e.createDisposition == CREATE_ALWAYS || e.createDisposition == TRUNCATE_EXISTING) {
            context.setCurrentSize(0);
            try {
                context.writeHeader();
            } catch (IOException ex) {
                ex.printStackTrace();
            }
        }
    }

    @Override
    public void afterQueryEa(CbfilterAfterQueryEaEvent e) {
        addToLog(String.format("afterQueryEa %s", e.fileName));
    }

    @Override
    public void afterQueryFileInfo(CbfilterAfterQueryFileInfoEvent e) {
        addToLog(String.format("afterQueryFileInfo %s", e.fileName));
    }

    @Override
    public void afterReadFile(CbfilterAfterReadFileEvent e) {

        if (e.fileContext != ENCRYPTED_FILE || e.direction == Constants.FS_REQUEST_DIR_USER_CACHED || e.direction == Constants.FS_REQUEST_DIR_SYSTEM_CACHED)
            return;

        addToLog(String.format("afterReadFile %s", e.fileName));

        EncryptContext context = encryptedFiles.get(e.fileName.toLowerCase());

        if (!context.headerPresent) return;

        byte[] buf = new byte[e.bytesRead];
        e.buffer.get(buf, 0, e.bytesRead);
        context.decryptBuffer(buf, e.bytesRead);
        e.buffer.position(0);
        e.buffer.put(buf, 0, e.bytesRead);
        buf = null;
    }

    @Override
    public void afterRenameOrMoveFile(CbfilterAfterRenameOrMoveFileEvent e) {
        addToLog(String.format("afterRenameOrMoveFile %s to %s", e.fileName, e.newFileName));
        boolean srcFiltered, dstFiltered;

        EncryptContext context;

        try {
            srcFiltered = cbfFilter.isFileFiltered(e.fileName) != false;
            dstFiltered = cbfFilter.isFileFiltered(e.newFileName) != false;
        } catch (CBFSFilterException ex) {
            addToLog(String.format("afterRenameOrMoveFile: exception - '%s'", ex.getMessage()));
            return;
        }

        if (srcFiltered ^ dstFiltered) {
            if (e.fileContext == ENCRYPTED_FILE) {

                context = encryptedFiles.get(e.fileName.toLowerCase());

                if (srcFiltered && context.headerPresent) {
                    try {
                        context.decryptFile();
                    } catch (FileNotFoundException ex) {
                        ex.printStackTrace();
                    }
                }
                if (dstFiltered)
                    context.encryptFile();

                if (!dstFiltered && context.headerPresent) {
                    try {
                        context.setEof(context.getCurrentSize() - DEFAULT_HEADER_SIZE);
                    } catch (IOException ex) {
                        ex.printStackTrace();
                    }
                    context.close();
                    encryptedFiles.remove(e.fileName.toLowerCase());
                    e.fileContext = 0;
                }
            }
            else
            {
                //
                // this is case when file is not in the filtered path and context was already freed.
                // Desired access we specified (FILE_WRITE_DATA) force the filtered file be encrypted in the place.
                //
                context = new EncryptContext(cbfFilter, e.newFileName, FILE_WRITE_DATA, DEFAULT_HEADER_SIZE, false);

                if (context.isInitialized())
                {
                    encryptedFiles.put(e.fileName.toLowerCase(), context);
                    e.fileContext = ENCRYPTED_FILE;
                    context.encryptFile();
                }
                else
                {
                    e.fileContext = 0;
                    context.close();
                }
            }
        }
    }

    @Override
    public void afterSetAllocationSize(CbfilterAfterSetAllocationSizeEvent e) {
        addToLog(String.format("afterSetAllocationSize %s", e.fileName));
    }

    @Override
    public void afterSetEa(CbfilterAfterSetEaEvent e) {
        addToLog(String.format("afterSetEa %s", e.fileName));
    }

    @Override
    public void afterSetFileSize(CbfilterAfterSetFileSizeEvent e) {
        addToLog(String.format("afterSetFileSize %s", e.fileName));
    }

    @Override
    public void afterSetReparsePoint(CbfilterAfterSetReparsePointEvent e) {
        addToLog(String.format("afterSetReparsePoint %s", e.fileName));
    }

    @Override
    public void afterSetFileAttributes(CbfilterAfterSetFileAttributesEvent e) {
        addToLog(String.format("afterSetFileAttributes %s", e.fileName));
    }

    @Override
    public void afterSetFileInfo(CbfilterAfterSetFileInfoEvent e) {
        addToLog(String.format("afterSetFileInfo %s", e.fileName));
    }

    @Override
    public void afterSetFileSecurity(CbfilterAfterSetFileSecurityEvent e) {
        addToLog(String.format("afterSetFileSecurity %s", e.fileName));
    }

    @Override
    public void afterUnlockAll(CbfilterAfterUnlockAllEvent e) {
        addToLog(String.format("afterUnlockAll %s", e.fileName));
    }

    @Override
    public void afterUnlockAllByKey(CbfilterAfterUnlockAllByKeyEvent e) {
        addToLog(String.format("afterUnlockAllByKey %s", e.fileName));
    }

    @Override
    public void afterUnlockSingle(CbfilterAfterUnlockSingleEvent e) {
        addToLog(String.format("afterUnlockSingle %s", e.fileName));
    }

    @Override
    public void afterWriteFile(CbfilterAfterWriteFileEvent e) {
        addToLog(String.format("afterWriteFile %s", e.fileName));
    }

    @Override
    public void beforeCanFileBeDeleted(CbfilterBeforeCanFileBeDeletedEvent e) {
        addToLog(String.format("beforeCanFileBeDeleted %s", e.fileName));
    }

    @Override
    public void beforeCleanupFile(CbfilterBeforeCleanupFileEvent e) {
        addToLog(String.format("beforeCleanupFile %s", e.fileName));
    }

    @Override
    public void beforeCloseFile(CbfilterBeforeCloseFileEvent e) {
        if (e.fileContext == ENCRYPTED_FILE) {
            EncryptContext context = encryptedFiles.get(e.fileName.toLowerCase());
            if (context.decrementRef() == 0) {
                context.close();
                encryptedFiles.remove(e.fileName.toLowerCase());
                e.fileContext = 0;
            }
        }
        addToLog(String.format("beforeCloseFile %s", e.fileName));
    }

    @Override
    public void beforeCreateFile(CbfilterBeforeCreateFileEvent e) {
        addToLog(String.format("beforeCreateFile %s", e.fileName));
        e.processRequest = true;
    }

    @Override
    public void beforeCreateHardLink(CbfilterBeforeCreateHardLinkEvent e) {
        addToLog(String.format("beforeCreateHardLink %s", e.fileName));
    }

    @Override
    public void beforeDeleteFile(CbfilterBeforeDeleteFileEvent e) {
        addToLog(String.format("beforeDeleteFile %s", e.fileName));
    }

    @Override
    public void beforeDeleteReparsePoint(CbfilterBeforeDeleteReparsePointEvent e) {
        addToLog(String.format("beforeDeleteReparsePoint %s", e.fileName));
    }

    @Override
    public void beforeEnumerateDirectory(CbfilterBeforeEnumerateDirectoryEvent e) {
        addToLog(String.format("beforeEnumerateDirectory %s", e.directoryName));
    }

    @Override
    public void beforeFilterAttachToVolume(CbfilterBeforeFilterAttachToVolumeEvent e) {
        addToLog(String.format("beforeFilterAttachToVolume %s", e.volumeName));
    }

    @Override
    public void beforeFsctl(CbfilterBeforeFsctlEvent e) {
        addToLog(String.format("beforeFsctl %s", e.fileName));
    }

    @Override
    public void beforeGetFileSecurity(CbfilterBeforeGetFileSecurityEvent e) {
        addToLog(String.format("beforeGetFileSecurity %s", e.fileName));
    }

    @Override
    public void beforeGetReparsePoint(CbfilterBeforeGetReparsePointEvent e) {
        addToLog(String.format("beforeGetReparsePoint %s", e.fileName));
    }

    @Override
    public void beforeIoctl(CbfilterBeforeIoctlEvent e) {
        addToLog(String.format("beforeIoctl %s", e.fileName));
    }

    @Override
    public void beforeLock(CbfilterBeforeLockEvent e) {
        addToLog(String.format("beforeLock %s", e.fileName));
    }

    @Override
    public void beforeOpenFile(CbfilterBeforeOpenFileEvent e) {
        addToLog(String.format("beforeOpenFile %s", e.fileName));
        e.processRequest = true;
    }

    @Override
    public void beforeQueryEa(CbfilterBeforeQueryEaEvent e) {
        addToLog(String.format("beforeQueryEa %s", e.fileName));
    }

    @Override
    public void beforeQueryFileInfo(CbfilterBeforeQueryFileInfoEvent e) {
        addToLog(String.format("beforeQueryFileInfo %s", e.fileName));
    }

    @Override
    public void beforeReadFile(CbfilterBeforeReadFileEvent e) {

        // this is a case when some other kernel component like an antivirus monitor tries
        // to read file within create/open file request. To prevent encrypted data
        // staying in the cache, we deny such request
        // The thrown error code may vary, you may change it for the tests
        if (e.fileContext != ENCRYPTED_FILE) {
            addToLog(String.format("BeforeReadFile ERROR_SHARING_VIOLATION !!!!"));
            e.resultCode = (int)ERROR_SHARING_VIOLATION;
            return;
        }

        if (e.fileContext == ENCRYPTED_FILE) {

            addToLog(String.format("beforeReadFile %s pos(%d read(%d))", e.fileName, e.position, e.bytesToRead));

            EncryptContext context = encryptedFiles.get(e.fileName.toLowerCase());

            int hsize = context.getHeaderSize();
            long cursize = context.getCurrentSize();

            if (!context.headerPresent && context.openedForModification) {
                context.encryptFile();
                context.headerPresent = true;
            }

            if ((e.direction == Constants.FS_REQUEST_DIR_USER_CACHED || e.direction == Constants.FS_REQUEST_DIR_SYSTEM_CACHED) &&
                    e.position >= cursize - hsize) {
                addToLog("ECBFSFltError(EOF) !!!!!");
                e.bytesToRead = 0;
                e.resultCode = (int)ERROR_HANDLE_EOF;
                return;
            }

            if (e.direction != Constants.FS_REQUEST_DIR_USER_CACHED &&
                    e.direction != Constants.FS_REQUEST_DIR_SYSTEM_CACHED) {
                e.position += hsize;
                addToLog(String.format("POS(%d) READ(%d)", e.position, e.bytesToRead));
            }
            else { //Cached read
                if (e.position + e.bytesToRead  > cursize - hsize)
                    e.bytesToRead = (int)(cursize - e.position - hsize);
            }
        }
    }

    @Override
    public void beforeRenameOrMoveFile(CbfilterBeforeRenameOrMoveFileEvent e) {
        boolean srcFiltered, dstFiltered;
    }

    @Override
    public void beforeSetAllocationSize(CbfilterBeforeSetAllocationSizeEvent e) {
        if (e.fileContext == ENCRYPTED_FILE) {
            EncryptContext context = encryptedFiles.get(e.fileName.toLowerCase());

            e.allocationSize += context.getHeaderSize();

            if (context.getCurrentSize() > e.allocationSize)
                context.setCurrentSize(e.allocationSize);
            addToLog(String.format("beforeSetAllocationSize %s", e.fileName));
        }
    }

    @Override
    public void beforeSetEa(CbfilterBeforeSetEaEvent e) {
        addToLog(String.format("beforeSetEa %s", e.fileName));
    }

    @Override
    public void beforeSetFileSize(CbfilterBeforeSetFileSizeEvent e) {
        if (e.fileContext == ENCRYPTED_FILE) {
            EncryptContext context = encryptedFiles.get(e.fileName.toLowerCase());
            e.size += context.getHeaderSize();
            context.setCurrentSize(e.size);
            addToLog(String.format("beforeSetFileSize %s EOF(%d)", e.fileName, e.size));
        }
    }

    @Override
    public void beforeSetReparsePoint(CbfilterBeforeSetReparsePointEvent e) {
        addToLog(String.format("beforeSetReparsePoint %s", e.fileName));
    }

    @Override
    public void beforeSetFileAttributes(CbfilterBeforeSetFileAttributesEvent e) {
        addToLog(String.format("beforeSetFileAttributes %s", e.fileName));
    }

    @Override
    public void beforeSetFileInfo(CbfilterBeforeSetFileInfoEvent e) {
        addToLog(String.format("beforeSetFileInfo %s", e.fileName));
    }

    @Override
    public void beforeSetFileSecurity(CbfilterBeforeSetFileSecurityEvent e) {
        addToLog(String.format("beforeSetFileSecurity %s", e.fileName));
    }

    @Override
    public void beforeUnlockAll(CbfilterBeforeUnlockAllEvent e) {
        addToLog(String.format("beforeUnlockAll %s", e.fileName));
    }

    @Override
    public void beforeUnlockAllByKey(CbfilterBeforeUnlockAllByKeyEvent e) {
        addToLog(String.format("beforeUnlockAllByKey %s", e.fileName));
    }

    @Override
    public void beforeUnlockSingle(CbfilterBeforeUnlockSingleEvent e) {
        addToLog(String.format("beforeUnlockSingle %s", e.fileName));
    }

    @Override
    public void beforeWriteFile(CbfilterBeforeWriteFileEvent e) {


        if (e.fileContext != ENCRYPTED_FILE ) {  return; }

        addToLog(String.format("beforeWriteFile %s Offset(%d) Size(%d) fctx(%d)", e.fileName, e.position, e.bytesToWrite, e.fileContext));

        EncryptContext context = encryptedFiles.get(e.fileName.toLowerCase());

        e.processRequest = true;

        if (e.direction == Constants.FS_REQUEST_DIR_USER_CACHED ||
                e.direction == Constants.FS_REQUEST_DIR_SYSTEM_CACHED) {
            if (!context.isHeaderPresent()) {
                context.encryptFile();
                context.headerPresent = true;
            }

            if (e.position + e.bytesToWrite >= context.getCurrentSize() - context.getHeaderSize())
            {
                long newEof = e.position + e.bytesToWrite + context.getHeaderSize();
                e.bytesToWrite += context.getHeaderSize();
                context.setCurrentSize(newEof);
            }

            return;
        }

        if (e.direction != Constants.FS_REQUEST_DIR_USER_CACHED &&
                e.direction != Constants.FS_REQUEST_DIR_SYSTEM_CACHED) {
            try {
                byte[] buf = new byte[e.bytesToWrite];
                e.buffer.get(buf, 0, e.bytesToWrite);
                context.encryptBuffer(buf, e.bytesToWrite);
                e.buffer.position(0);
                e.buffer.put(buf, 0, e.bytesToWrite);
                buf = null;
                e.position += context.getHeaderSize();
            }
            catch (Exception ex) {
                addToLog(String.format("beforeWriteFile: exception '%s'", ex.toString()));
            }
        }
    }

    @Override
    public void error(CbfilterErrorEvent e) {
        addToLog(String.format("Error: %d %s", e.errorCode, e.description));
    }

    @Override
    public void filterStart(CbfilterFilterStartEvent e) {
        addToLog("Filter started.");
    }

    @Override
    public void filterStop(CbfilterFilterStopEvent cbfilterFilterStopEvent) {
        if ( ! closing)
            updateButtons();
    }

    @Override
    public void notifyCanFileBeDeleted(CbfilterNotifyCanFileBeDeletedEvent cbfilterNotifyCanFileBeDeletedEvent) {

    }

    @Override
    public void notifyCleanupFile(CbfilterNotifyCleanupFileEvent cbfilterNotifyCleanupFileEvent) {

    }

    @Override
    public void notifyCloseFile(CbfilterNotifyCloseFileEvent cbfilterNotifyCloseFileEvent) {

    }

    @Override
    public void notifyCreateFile(CbfilterNotifyCreateFileEvent cbfilterNotifyCreateFileEvent) {

    }

    @Override
    public void notifyCreateHardLink(CbfilterNotifyCreateHardLinkEvent cbfilterNotifyCreateHardLinkEvent) {

    }

    @Override
    public void notifyDeleteFile(CbfilterNotifyDeleteFileEvent cbfilterNotifyDeleteFileEvent) {

    }

    @Override
    public void notifyDeleteReparsePoint(CbfilterNotifyDeleteReparsePointEvent cbfilterNotifyDeleteReparsePointEvent) {

    }

    @Override
    public void notifyEnumerateDirectory(CbfilterNotifyEnumerateDirectoryEvent cbfilterNotifyEnumerateDirectoryEvent) {

    }

    @Override
    public void notifyFilterAttachToVolume(CbfilterNotifyFilterAttachToVolumeEvent cbfilterNotifyFilterAttachToVolumeEvent) {

    }

    @Override
    public void notifyFilterDetachFromVolume(CbfilterNotifyFilterDetachFromVolumeEvent cbfilterNotifyFilterDetachFromVolumeEvent) {

    }

    @Override
    public void notifyFsctl(CbfilterNotifyFsctlEvent cbfilterNotifyFsctlEvent) {

    }

    @Override
    public void notifyGetFileSecurity(CbfilterNotifyGetFileSecurityEvent cbfilterNotifyGetFileSecurityEvent) {

    }

    @Override
    public void notifyGetFileSizes(CbfilterNotifyGetFileSizesEvent cbfilterNotifyGetFileSizesEvent) {

    }

    @Override
    public void notifyGetReparsePoint(CbfilterNotifyGetReparsePointEvent cbfilterNotifyGetReparsePointEvent) {

    }

    @Override
    public void notifyIoctl(CbfilterNotifyIoctlEvent cbfilterNotifyIoctlEvent) {

    }

    @Override
    public void notifyLock(CbfilterNotifyLockEvent cbfilterNotifyLockEvent) {

    }

    @Override
    public void notifyOpenFile(CbfilterNotifyOpenFileEvent cbfilterNotifyOpenFileEvent) {

    }

    @Override
    public void notifyQueryEa(CbfilterNotifyQueryEaEvent cbfilterNotifyQueryEaEvent) {

    }

    @Override
    public void notifyQueryFileInfo(CbfilterNotifyQueryFileInfoEvent cbfilterNotifyQueryFileInfoEvent) {

    }

    @Override
    public void notifyReadFile(CbfilterNotifyReadFileEvent cbfilterNotifyReadFileEvent) {

    }

    @Override
    public void notifyRenameOrMoveFile(CbfilterNotifyRenameOrMoveFileEvent cbfilterNotifyRenameOrMoveFileEvent) {

    }

    @Override
    public void notifySetAllocationSize(CbfilterNotifySetAllocationSizeEvent cbfilterNotifySetAllocationSizeEvent) {

    }

    @Override
    public void notifySetEa(CbfilterNotifySetEaEvent cbfilterNotifySetEaEvent) {

    }

    @Override
    public void notifySetFileSize(CbfilterNotifySetFileSizeEvent cbfilterNotifySetFileSizeEvent) {

    }

    @Override
    public void notifySetReparsePoint(CbfilterNotifySetReparsePointEvent cbfilterNotifySetReparsePointEvent) {

    }

    @Override
    public void notifySetFileAttributes(CbfilterNotifySetFileAttributesEvent cbfilterNotifySetFileAttributesEvent) {

    }

    @Override
    public void notifySetFileInfo(CbfilterNotifySetFileInfoEvent cbfilterNotifySetFileInfoEvent) {

    }

    @Override
    public void notifySetFileSecurity(CbfilterNotifySetFileSecurityEvent cbfilterNotifySetFileSecurityEvent) {

    }

    @Override
    public void notifyUnlockAll(CbfilterNotifyUnlockAllEvent cbfilterNotifyUnlockAllEvent) {

    }

    @Override
    public void notifyUnlockAllByKey(CbfilterNotifyUnlockAllByKeyEvent cbfilterNotifyUnlockAllByKeyEvent) {

    }

    @Override
    public void notifyUnlockSingle(CbfilterNotifyUnlockSingleEvent cbfilterNotifyUnlockSingleEvent) {

    }

    @Override
    public void notifyWriteFile(CbfilterNotifyWriteFileEvent cbfilterNotifyWriteFileEvent) {

    }

    @Override
    public void reparseFileName(CbfilterReparseFileNameEvent cbfilterReparseFileNameEvent) {

    }

    @Override
    public void reparseWithTag(CbfilterReparseWithTagEvent cbfilterReparseWithTagEvent) {

    }

    @Override
    public void workerThreadCreation(CbfilterWorkerThreadCreationEvent cbfilterWorkerThreadCreationEvent) {

    }

    @Override
    public void workerThreadTermination(CbfilterWorkerThreadTerminationEvent cbfilterWorkerThreadTerminationEvent) {

    }

    class EncryptContext {
        static final int FILE_ALLOCATION_INFORMATION = 19;

        private int refCnt;
        private boolean initialized;
        private CBFSFilterStream handle;

        private long handleValue;
        private String fileNameEncrypted;
        private int defaultHeaderSize;
        private long currentSize;
        private boolean headerPresent;
        private boolean openedForModification;

        private final String magicString = "!!!!!@@@@@@@@EncryptHeader@@@@@@@@@!!!!!";

        private final int defaultClusterSize = 4096; // this value is specific to the system configuration and must be obtained on initialization

        EncryptContext(Cbfilter filter, String fileName, int desiredAccess, int headerSize, boolean nonFiltered) {

            defaultHeaderSize = headerSize;
            initialized = false;
            openedForModification = false;

            boolean error = false;

            //
            // uncomment this line of code if your intention to encrypt
            // unencrypted files when they first time opened/created
            //
            // mOpenedForModification = true;

            if ((desiredAccess & (DELETE | FILE_WRITE_DATA | FILE_APPEND_DATA | WRITE_DAC | WRITE_OWNER | FILE_WRITE_EA | FILE_WRITE_ATTRIBUTES)) != 0) {
                openedForModification = true;
            }

            try {
                long[] handleRef = new long[1];

                if (nonFiltered) {
                    handle = cbfFilter.createFileDirectAsStream(fileName, false,
                            (int) (GENERIC_READ | GENERIC_WRITE),
                            (int) OPEN_EXISTING,
                            (int) FILE_FLAG_NO_BUFFERING,
                            0,
                            handleRef);
                } else {
                    handle = cbfFilter.createFileDirectAsStream(fileName, true, 0, 0, 0, 0, handleRef);
                }

                handleValue = handleRef[1];

                if (!nonFiltered) {

                    headerPresent = findHeader();
                }
                else {
                    headerPresent = false;
                }

                fileNameEncrypted = fileName;
                currentSize = handle.getLength();

                if (!headerPresent && !nonFiltered && openedForModification) {

                    if (currentSize == 0) {

                        currentSize = writeHeader();
                        headerPresent = true;
                        initialized = currentSize != 0;
                    }
                    else {
                        //
                        // uncomment this code if your intention to encrypt
                        // unencrypted files when they first time opened,
                        // otherwise,
                        // it will be encrypted conditionally in
                        // read/write callbacks:
                        //
                        // a) file was created/opened for write access
                        // b) reead/write callback event was triggered
                        //
                        // error = encryptFile();
                        //  if (!error)
                        //      headerPresent = true;
                    }
                }

                if (!error) {
                    initialized = true;
                }
                incrementRef();
            } catch (CBFSFilterException | IOException e) {
                e.printStackTrace();
                initialized = false;
            }
        }

        public void close() {

            try {
                if (handle != null) {

                    handle.close();
                    handle = null;
                }
            }
            catch (IOException e)
            {
                e.printStackTrace();
            }
        }

        public boolean encryptFile() {

            // we got here non-encrypted file
            // try add header and encrypt it
            //
            byte[] rbuf;
            byte[] wbuf;
            long curPos;
            int buflen = 0x10000;
            int written = 0;
            boolean error = false;

            byte[][] arrbuf = new byte[2][];
            //arrbuf[0] = new byte[buflen];
            //arrbuf[1] = new byte[buflen];

            curPos = 0;

            try {
                handle.seek(curPos, CBFSFilterStream.SeekOrigin.FROM_BEGIN);
                arrbuf[0] = handle.read(buflen);
            }
            catch (EOFException e) {
            }
            catch (Exception e) {
                error = true;
            }
            if (arrbuf[0] == null)
                return false;

            int i = 0;

            while ( !error ) {
                rbuf = null;
                wbuf = arrbuf[i & 1];

                try {
                    handle.seek(curPos + buflen, CBFSFilterStream.SeekOrigin.FROM_BEGIN);
                    arrbuf[(i + 1) & 1] = handle.read(buflen);

                    rbuf = arrbuf[(i + 1) & 1];
                } catch (EOFException e) {
                } catch (Exception e) {
                    error = true;
                }

                encryptBuffer(wbuf, wbuf.length);

                if (!headerPresent) {
                    try {
                        setAllocation(roundToCluster(currentSize + defaultHeaderSize));
                        setEof(currentSize + defaultHeaderSize);
                        if (writeHeader() != 0)
                            headerPresent = true;
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                }

                try {
                    handle.seek(curPos + defaultHeaderSize, CBFSFilterStream.SeekOrigin.FROM_BEGIN);
                    written = handle.write(wbuf);
                } catch (Exception e) {
                    error = true;
                }

                if (error) break;

                curPos += buflen;
                ++i;

                if (written < buflen) break;

                if ((rbuf == null) || (rbuf.length == 0)) break;

                if (rbuf.length < buflen) {
                    //
                    // write the last portion of file
                    //
                    encryptBuffer(rbuf, rbuf.length);

                    try {
                        handle.seek(curPos + defaultHeaderSize, CBFSFilterStream.SeekOrigin.FROM_BEGIN);
                        handle.write(rbuf);
                    } catch (Exception e) {
                        error = true;
                    }
                    break;
                }
            }
            return error;
        }


        public void decryptFile() throws FileNotFoundException {

            if (handle != null) {
                final int bufferSize = 8192;
                byte[] proxyBuffer;

                try {
                    int completed = 0;
                    while (true) {
                        proxyBuffer = null;
                        try {
                            handle.seek(completed + defaultHeaderSize, CBFSFilterStream.SeekOrigin.FROM_BEGIN);
                            proxyBuffer = handle.read(bufferSize);
                        }
                        catch (Exception e) {
                            e.printStackTrace();
                        }
                        if (proxyBuffer == null)
                            break;

                        decryptBuffer(proxyBuffer, proxyBuffer.length);

                        try {
                            handle.seek(completed, CBFSFilterStream.SeekOrigin.FROM_BEGIN);
                            handle.write(proxyBuffer);
                        }
                        catch (Exception e) {
                            e.printStackTrace();
                        }
                        completed += proxyBuffer.length;

                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }

        public int writeHeader() throws IOException {
            if (currentSize < defaultHeaderSize)
                setEof(getCurrentSize() + defaultHeaderSize);

            try {

                byte [] buf = new byte[DEFAULT_HEADER_SIZE];
                Arrays.fill(buf, (byte) 0);
                System.arraycopy(magicString.getBytes(), 0, buf, 0, magicString.getBytes().length);
                handle.seek(0, CBFSFilterStream.SeekOrigin.FROM_BEGIN);
                return handle.write(buf);

            } catch (IOException e) {
                e.printStackTrace();
            }
            return 0;
        }

        public void closeFile() {

            try {
                if (handle != null) {
                    handle.close();
                    handle = null;
                }
            }
            catch (IOException ex) {

                ex.printStackTrace();
            }

        }

        public void encryptBuffer(byte[] buffer, int length) {
            for (int i = 0; i < length; i++)
                buffer[i] ^= 0xFF;
        }

        public void decryptBuffer(byte[] buffer, int length) {
            for (int i = 0; i < length; i++)
                buffer[i] ^= 0xFF;
        }

        public int incrementRef() {
            refCnt++;
            return refCnt;
        }

        public int decrementRef() {
            if (refCnt > 0)
                refCnt--;
            return refCnt;
        }

        public boolean isInitialized() {
            return initialized;
        }

        public void setInitialized(boolean value) {
            initialized = value;
        }

        public long getCurrentSize() {
            return currentSize;
        }

        public void setCurrentSize(long value) {
            currentSize = value;
        }

        public int getHeaderSize() {
            return defaultHeaderSize;
        }

        public boolean isHeaderPresent() {
            return headerPresent;
        }

        public boolean findHeader() {

            try {

                byte [] srcBuf;
                byte [] strBuf;
                handle.seek(0, CBFSFilterStream.SeekOrigin.FROM_BEGIN);
                try {
                    srcBuf = handle.read(defaultHeaderSize);
                }
                catch (Exception ex) {
                    addToLog(String.format("read header FAILED!!!! Error(%s)", ex.getMessage()));
                    return false;
                }

                if (srcBuf.length == defaultHeaderSize) {

                    strBuf = magicString.getBytes();
                    return Arrays.equals(srcBuf, strBuf);
                }

            } catch (IOException e) {
                e.printStackTrace();
            }
            return false;
        }

        public long roundToCluster(long Value) {

            return ((Value + defaultClusterSize - 1) & ~(defaultClusterSize - 1));
        }

        public void setAllocation(long value) throws IOException {
            try {
                byte[] valueBuf = longToBuf(value);
                cbfFilter.setFileInformationDirect(handleValue, FILE_ALLOCATION_INFORMATION, valueBuf);
            } catch (Exception e) {
                e.printStackTrace();
                addToLog("SetAllocation ERROR !!!!");
            }
        }

        public void setEof(long value) throws IOException {
            try {
                handle.setLength(value);
                setCurrentSize(value);

            } catch (IOException e) {
                e.printStackTrace();
                addToLog("SetEOF ERROR !!!!");
            }
        }

        public boolean openFile(String fileName) {
            try {
                if (handle == null) {
                    long[] handleRef = new long[1];
                    handle = cbfFilter.createFileDirectAsStream(fileName, true, 0, 0, 0, 0, handleRef);
                    handleValue = handleRef[0];
                }
                return handle !=null;
            } catch (CBFSFilterException e) {
                e.printStackTrace();
            }
            return false;
        }

        private byte[] longToBuf(long value) {
            return new byte[] {
                    (byte) value,
                    (byte) (value >> 8),
                    (byte) (value >> 16),
                    (byte) (value >> 24),
                    (byte) (value >> 32),
                    (byte) (value >> 40),
                    (byte) (value >> 48),
                    (byte) (value >> 56)
            };
        }
    }
}





