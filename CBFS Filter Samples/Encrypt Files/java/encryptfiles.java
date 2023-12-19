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
import java.io.*;
import java.nio.ByteBuffer;
import java.util.HashMap;
import java.util.TooManyListenersException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.regex.Pattern;

import cbfsfilter.*;

public class encryptfiles implements cbfsfilter.CbfilterEventListener {
    private Cbfilter cbfFilter = new Cbfilter();

    public static final String appName = "EncryptFiles Files";
    private final String guid = "{713CC6CE-B3E2-4fd9-838D-E28F558F6866}";
    private final String ALTITUDE_FAKE_VALUE_FOR_DEBUG = "149995";
    private final int ERROR_ACCESS_DENIED = 5;
    private final int ERROR_PRIVILEGE_NOT_HELD = 1314;
    private final int FILE_ATTRIBUTE_DIRECTORY = 0x00000010;
    private final int ENCRYPTED_FILE = 111;
    public static final int FILE_SHARE_READ = 1;
    public static final int FILE_SHARE_WRITE = 2;
    public static final int OPEN_EXISTING = 3;
    public static final long GENERIC_READ = -2147483648L;
    public static final int GENERIC_WRITE = 1073741824;
    public static final int FILE_FLAG_NO_BUFFERING = 536870912;
    public static final int CREATE_ALWAYS = 2;


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
    private boolean closing;

    private DefaultListModel listModel;
    private HashMap<String, EncryptFilesContext> encryptedFiles;

    public encryptfiles() {
        encryptedFiles = new HashMap<String, EncryptFilesContext>();

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
        JFrame frame = new JFrame(encryptfiles.appName);
        encryptfiles encryptfilesDemo = new encryptfiles();
        frame.setContentPane(encryptfilesDemo.panelRoot);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        encryptfilesDemo.initComponent(frame);
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

    protected void updateButtons()  {
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
        try
        {
            boolean reboot = cbfFilter.install(fc.getSelectedFile().getPath(), guid, "", ALTITUDE_FAKE_VALUE_FOR_DEBUG, 0);

            updateButtons();

            if (reboot)
                showMessageBox("Please, reboot the system for the changes to take effect");
            else
                showMessageBox("Driver installed successfully");

        }
        catch (CBFSFilterException e)
        {
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
        try
        {
            boolean reboot = cbfFilter.uninstall(fc.getSelectedFile().getPath(), guid, "", 0);

            updateButtons();

            if (reboot)
                showMessageBox("Please, reboot the system for the changes to take effect");
            else
                showMessageBox("Driver uninstalled successfully");

        }
        catch (CBFSFilterException e)
        {
            if (e.getCode() == ERROR_PRIVILEGE_NOT_HELD)
                showMessageBox("Uninstallation requires administrator rights. Run the app as administrator");
            else
                showMessageBox(e.getMessage());
        }
    }

    protected void onSetFilterClick() {
        String filterPath = ConvertRelativePathToAbsolute(textPath.getText());
        long callbacks = Constants.FS_CE_AFTER_READ |
                Constants.FS_CE_BEFORE_WRITE |
                Constants.FS_CE_BEFORE_CREATE |
                Constants.FS_CE_AFTER_CREATE |
                Constants.FS_CE_BEFORE_RENAME |
                Constants.FS_CE_BEFORE_SET_SIZES |
                Constants.FS_CE_BEFORE_DELETE |
                Constants.FS_CE_BEFORE_SET_ATTRIBUTES |
                Constants.FS_CE_BEFORE_OPEN |
                Constants.FS_CE_AFTER_OPEN |
                Constants.FS_CE_BEFORE_CLOSE;
        try
        {
            cbfFilter.addFilterRule(filterPath,
                    Constants.ACCESS_NONE,
                    callbacks,
                    Constants.FS_NE_NONE
            );

            cbfFilter.addFilterRule(filterPath + "\\*.*",
                    Constants.ACCESS_NONE,
                    callbacks,
                    Constants.FS_NE_NONE);

            cbfFilter.initialize(guid);
            cbfFilter.config("AllowFileAccessInBeforeOpen=false");
            cbfFilter.config("ModifiableReadWriteBuffers=true");
            cbfFilter.startFilter(30000);
            cbfFilter.setFileFlushingBehavior(Constants.FS_SUPPORT_FILE_ENCRYPTION);
        } catch (CBFSFilterException e) {
            showMessageBox(e.getMessage());
        }
        updateButtons();
    }

    protected void onDeleteFilterClick() {
        try {
            cbfFilter.deleteAllFilterRules();
            cbfFilter.deleteAllPassthroughRules();
            if (cbfFilter.isActive())
                cbfFilter.stopFilter(false);
            btnDeleteFilter.setEnabled(false);
            btnSetFilter.setEnabled(true);
        } catch (CBFSFilterException e) {
            showMessageBox(e.getMessage());
        }
    }

    protected void addToLog(String value) {
        if (closing) return;

        if (listModel.getSize() > 1000)
            listModel.removeAllElements();
        listModel.add(listModel.getSize(), value);

        if (this.chkAutoScroll.isSelected())
        {
            listLog.ensureIndexIsVisible(listModel.getSize() - 1);
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

    private String getTitle(){
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

    protected void onClearLogClick()
    {
        listModel.removeAllElements();
    }

    protected void onFormClosing() {
        closing = true;

        if (cbfFilter.isActive())
            try {
                cbfFilter.dispose();
            } catch (CBFSFilterException e) {
                showMessageBox(e.getMessage());
            }
    }

    @Override
    public void cleanupContext(CbfilterCleanupContextEvent cbfilterCleanupContextEvent) {

    }

    @Override
    public void afterCanFileBeDeleted(CbfilterAfterCanFileBeDeletedEvent cbfilterAfterCanFileBeDeletedEvent) {

    }

    @Override
    public void afterCleanupFile(CbfilterAfterCleanupFileEvent cbfilterAfterCleanupFileEvent) {

    }

    @Override
    public void afterCloseEnumeration(CbfilterAfterCloseEnumerationEvent e) {
        addToLog(String.format("afterCloseEnumeration %s", e.directoryName));
    }

    @Override
    public void afterCloseFile(CbfilterAfterCloseFileEvent cbfilterAfterCloseFileEvent) {

    }

    @Override
    public void afterCreateFile(CbfilterAfterCreateFileEvent e) {
        if (e.fileContext != ENCRYPTED_FILE) {
            if (0 == (e.attributes & FILE_ATTRIBUTE_DIRECTORY)) {
                EncryptFilesContext context = new EncryptFilesContext(cbfFilter, e.fileName, false);
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
            EncryptFilesContext context = encryptedFiles.get(e.fileName.toLowerCase());
            //enable this code if you have closed file during rename callback
            context.openFile(e.fileName);

            context.incrementRef();
        }
        addToLog(String.format("afterCreateFile %s", e.fileName));
    }

    @Override
    public void afterCreateHardLink(CbfilterAfterCreateHardLinkEvent cbfilterAfterCreateHardLinkEvent) {

    }

    @Override
    public void afterDeleteFile(CbfilterAfterDeleteFileEvent cbfilterAfterDeleteFileEvent) {

    }

    @Override
    public void afterDeleteReparsePoint(CbfilterAfterDeleteReparsePointEvent cbfilterAfterDeleteReparsePointEvent) {

    }

    @Override
    public void afterEnumerateDirectory(CbfilterAfterEnumerateDirectoryEvent e) {
        addToLog(String.format("afterEnumerateDirectory %s", e.fileName));
        //e.fileFound = true;
    }

    @Override
    public void afterFilterAttachToVolume(CbfilterAfterFilterAttachToVolumeEvent cbfilterAfterFilterAttachToVolumeEvent) {

    }

    @Override
    public void afterFilterDetachFromVolume(CbfilterAfterFilterDetachFromVolumeEvent cbfilterAfterFilterDetachFromVolumeEvent) {

    }

    @Override
    public void afterFsctl(CbfilterAfterFsctlEvent cbfilterAfterFsctlEvent) {

    }

    @Override
    public void afterGetFileSecurity(CbfilterAfterGetFileSecurityEvent cbfilterAfterGetFileSecurityEvent) {

    }

    @Override
    public void afterGetFileSizes(CbfilterAfterGetFileSizesEvent cbfilterAfterGetFileSizesEvent) {

    }

    @Override
    public void afterGetReparsePoint(CbfilterAfterGetReparsePointEvent cbfilterAfterGetReparsePointEvent) {

    }

    @Override
    public void afterIoctl(CbfilterAfterIoctlEvent cbfilterAfterIoctlEvent) {

    }

    @Override
    public void afterLock(CbfilterAfterLockEvent cbfilterAfterLockEvent) {

    }

    @Override
    public void afterOpenFile(CbfilterAfterOpenFileEvent e) {
        if (e.fileContext != ENCRYPTED_FILE) {
            if (0 == (e.attributes & FILE_ATTRIBUTE_DIRECTORY)) {
                EncryptFilesContext context = new EncryptFilesContext(cbfFilter, e.fileName, false);
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
            EncryptFilesContext context = encryptedFiles.get(e.fileName.toLowerCase());
            //enable this code if you have closed file during rename callback
            context.openFile(e.fileName);

            context.incrementRef();
        }
        addToLog(String.format("afterOpenFile %s", e.fileName));
    }

    @Override
    public void afterQueryEa(CbfilterAfterQueryEaEvent cbfilterAfterQueryEaEvent) {

    }

    @Override
    public void afterQueryFileInfo(CbfilterAfterQueryFileInfoEvent cbfilterAfterQueryFileInfoEvent) {

    }

    @Override
    public void afterReadFile(CbfilterAfterReadFileEvent e) {

        EncryptFilesContext context;

        if ( e.fileContext == ENCRYPTED_FILE
                && e.direction != Constants.FS_REQUEST_DIR_USER_CACHED
                && e.direction != Constants.FS_REQUEST_DIR_SYSTEM_CACHED ) {
            context = encryptedFiles.get(e.fileName.toLowerCase());
            try {
                if (e.status == 0 && e.bytesRead != 0 && e.buffer != null) {

                    byte[] decryptionBuffer = new byte[e.bytesRead];
                    e.buffer.get(decryptionBuffer, 0, e.bytesRead);
                    context.encryptDecryptBuffer(decryptionBuffer, e.bytesRead);
                    e.buffer.position(0);
                    e.buffer.put(decryptionBuffer, 0, e.bytesRead);
                }
            }
            catch (Exception ex) {
                addToLog(String.format("afterReadFile: exception '%s'", ex.toString()));
            }
        }
        addToLog(String.format("afterReadFile %s", e.fileName));
    }

    @Override
    public void afterRenameOrMoveFile(CbfilterAfterRenameOrMoveFileEvent cbfilterAfterRenameOrMoveFileEvent) {

    }

    @Override
    public void afterSetAllocationSize(CbfilterAfterSetAllocationSizeEvent cbfilterAfterSetAllocationSizeEvent) {

    }

    @Override
    public void afterSetEa(CbfilterAfterSetEaEvent cbfilterAfterSetEaEvent) {

    }

    @Override
    public void afterSetFileSize(CbfilterAfterSetFileSizeEvent cbfilterAfterSetFileSizeEvent) {

    }

    @Override
    public void afterSetReparsePoint(CbfilterAfterSetReparsePointEvent cbfilterAfterSetReparsePointEvent) {

    }

    @Override
    public void afterSetFileAttributes(CbfilterAfterSetFileAttributesEvent cbfilterAfterSetFileAttributesEvent) {

    }

    @Override
    public void afterSetFileInfo(CbfilterAfterSetFileInfoEvent cbfilterAfterSetFileInfoEvent) {

    }

    @Override
    public void afterSetFileSecurity(CbfilterAfterSetFileSecurityEvent cbfilterAfterSetFileSecurityEvent) {

    }

    @Override
    public void afterUnlockAll(CbfilterAfterUnlockAllEvent cbfilterAfterUnlockAllEvent) {

    }

    @Override
    public void afterUnlockAllByKey(CbfilterAfterUnlockAllByKeyEvent cbfilterAfterUnlockAllByKeyEvent) {

    }

    @Override
    public void afterUnlockSingle(CbfilterAfterUnlockSingleEvent cbfilterAfterUnlockSingleEvent) {

    }

    @Override
    public void afterWriteFile(CbfilterAfterWriteFileEvent e) {
        addToLog(String.format("afterWriteFile %s", e.fileName));
    }

    @Override
    public void beforeCanFileBeDeleted(CbfilterBeforeCanFileBeDeletedEvent e) {
        addToLog(String.format("beforeCanFileBeDeleted %s", e.fileName));
        e.processRequest = true;
    }

    @Override
    public void beforeCleanupFile(CbfilterBeforeCleanupFileEvent cbfilterBeforeCleanupFileEvent) {

    }

    @Override
    public void beforeCloseFile(CbfilterBeforeCloseFileEvent e) {
        if (e.fileContext == ENCRYPTED_FILE) {
            EncryptFilesContext context = encryptedFiles.get(e.fileName.toLowerCase());
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
    public void beforeCreateHardLink(CbfilterBeforeCreateHardLinkEvent cbfilterBeforeCreateHardLinkEvent) {

    }

    @Override
    public void beforeDeleteFile(CbfilterBeforeDeleteFileEvent cbfilterBeforeDeleteFileEvent) {

    }

    @Override
    public void beforeDeleteReparsePoint(CbfilterBeforeDeleteReparsePointEvent cbfilterBeforeDeleteReparsePointEvent) {

    }

    @Override
    public void beforeEnumerateDirectory(CbfilterBeforeEnumerateDirectoryEvent cbfilterBeforeEnumerateDirectoryEvent) {

    }

    @Override
    public void beforeFilterAttachToVolume(CbfilterBeforeFilterAttachToVolumeEvent cbfilterBeforeFilterAttachToVolumeEvent) {

    }

    @Override
    public void beforeFsctl(CbfilterBeforeFsctlEvent cbfilterBeforeFsctlEvent) {

    }

    @Override
    public void beforeGetFileSecurity(CbfilterBeforeGetFileSecurityEvent cbfilterBeforeGetFileSecurityEvent) {

    }

    @Override
    public void beforeGetReparsePoint(CbfilterBeforeGetReparsePointEvent cbfilterBeforeGetReparsePointEvent) {

    }

    @Override
    public void beforeIoctl(CbfilterBeforeIoctlEvent cbfilterBeforeIoctlEvent) {

    }

    @Override
    public void beforeLock(CbfilterBeforeLockEvent cbfilterBeforeLockEvent) {

    }

    @Override
    public void beforeOpenFile(CbfilterBeforeOpenFileEvent e) {
        addToLog(String.format("beforeOpenFile %s", e.fileName));
        e.processRequest = true;
    }

    @Override
    public void beforeQueryEa(CbfilterBeforeQueryEaEvent cbfilterBeforeQueryEaEvent) {

    }

    @Override
    public void beforeQueryFileInfo(CbfilterBeforeQueryFileInfoEvent cbfilterBeforeQueryFileInfoEvent) {

    }

    @Override
    public void beforeReadFile(CbfilterBeforeReadFileEvent e) {

        addToLog(String.format("beforeReadFile %s", e.fileName));
    }

    @Override
    public void beforeRenameOrMoveFile(final CbfilterBeforeRenameOrMoveFileEvent e) {
        boolean srcFiltered, dstFiltered;

        addToLog(String.format("beforeRenameOrMoveFile %s to %s", e.fileName, e.newFileName));

        try {
            srcFiltered = cbfFilter.isFileFiltered(e.fileName) != false;
            dstFiltered = cbfFilter.isFileFiltered(e.newFileName) != false;
        } catch (CBFSFilterException ex) {
            addToLog(String.format("beforeRenameOrMoveFile: exception '%s'", ex.toString()));
            return;
        }

        if (srcFiltered ^ dstFiltered) {
            EncryptFilesContext context = null;
            if (e.fileContext == ENCRYPTED_FILE) {
                context = encryptedFiles.get(e.fileName.toLowerCase());
            }
            else {
                context = new EncryptFilesContext(cbfFilter, e.fileName, !srcFiltered);

                if (false == context.isInitialized()) {
                    context.close();
                    return;
                }
            }
            try {
                context.moveFileTo(e.fileName, e.newFileName, srcFiltered, dstFiltered);
            } catch (FileNotFoundException ex) {
                addToLog(String.format("beforeRenameOrMoveFile: exception '%s'", ex.toString()));
            }

            if(e.fileContext != ENCRYPTED_FILE) {
                context.close();
            }
            if(!dstFiltered) {
                context.close();
                encryptedFiles.remove(e.fileName.toLowerCase());
                e.fileContext = 0;
            }
            e.processRequest = false;

            new Thread(new Runnable() {
                @Override public void run() {
                    // do stuff in this thread
                    new File(e.fileName).delete();
                }
            }).start();
        }
    }

    @Override
    public void beforeSetAllocationSize(CbfilterBeforeSetAllocationSizeEvent e) {
        addToLog(String.format("beforeSetAllocationSize %s", e.fileName));
        e.processRequest = true;
    }

    @Override
    public void beforeSetEa(CbfilterBeforeSetEaEvent cbfilterBeforeSetEaEvent) {

    }

    @Override
    public void beforeSetFileSize(CbfilterBeforeSetFileSizeEvent e) {
        addToLog(String.format("beforeReadFile %s", e.fileName));
        if (e.fileContext == ENCRYPTED_FILE) {
            EncryptFilesContext context = encryptedFiles.get(e.fileName.toLowerCase());
        }
        addToLog(String.format("beforeSetFileSize %s EOF(%d)", e.fileName, e.size));
        e.processRequest = true;
    }

    @Override
    public void beforeSetReparsePoint(CbfilterBeforeSetReparsePointEvent cbfilterBeforeSetReparsePointEvent) {

    }

    @Override
    public void beforeSetFileAttributes(CbfilterBeforeSetFileAttributesEvent e) {
        addToLog(String.format("beforeSetFileAttributes %s", e.fileName));
        e.processRequest = true;
    }

    @Override
    public void beforeSetFileInfo(CbfilterBeforeSetFileInfoEvent cbfilterBeforeSetFileInfoEvent) {

    }

    @Override
    public void beforeSetFileSecurity(CbfilterBeforeSetFileSecurityEvent cbfilterBeforeSetFileSecurityEvent) {

    }

    @Override
    public void beforeUnlockAll(CbfilterBeforeUnlockAllEvent cbfilterBeforeUnlockAllEvent) {

    }

    @Override
    public void beforeUnlockAllByKey(CbfilterBeforeUnlockAllByKeyEvent cbfilterBeforeUnlockAllByKeyEvent) {

    }

    @Override
    public void beforeUnlockSingle(CbfilterBeforeUnlockSingleEvent cbfilterBeforeUnlockSingleEvent) {

    }

    @Override
    public void beforeWriteFile(CbfilterBeforeWriteFileEvent e) {
        int written = 0;
        if ( e.fileContext == ENCRYPTED_FILE
                && e.direction != Constants.FS_REQUEST_DIR_USER_CACHED
                && e.direction != Constants.FS_REQUEST_DIR_SYSTEM_CACHED ) {
            EncryptFilesContext context = encryptedFiles.get(e.fileName.toLowerCase());
            byte[] encryptionBuffer = new byte[e.bytesToWrite];
            e.buffer.get(encryptionBuffer, 0, e.bytesToWrite);
            context.encryptDecryptBuffer(encryptionBuffer, e.bytesToWrite);
            e.buffer.position(0);
            e.buffer.put(encryptionBuffer, 0, e.bytesToWrite);
        }

        addToLog(String.format("beforeWriteFile %s", e.fileName));
    }

    @Override
    public void error(CbfilterErrorEvent cbfilterErrorEvent) {

    }

    @Override
    public void filterStart(CbfilterFilterStartEvent cbfilterFilterStartEvent) {

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

    class EncryptFilesContext {
        private int refCnt;
        private boolean initialized;
        private CBFSFilterStream stream;
        private String fileNameEncrypted;
        private long[] handle;

        EncryptFilesContext(Cbfilter filter, String fileName, boolean nonFiltered) {

            initialized = false;

            handle = new long[]{ 0 };

            try {
                if (nonFiltered) {
                    stream = cbfFilter.createFileDirectAsStream(fileName, false,
                            (int) (GENERIC_READ | GENERIC_WRITE),
                            (int) OPEN_EXISTING,
                            (int) FILE_FLAG_NO_BUFFERING,
                            0,
                            handle);
                }
                else
                    stream = cbfFilter.createFileDirectAsStream(fileName, true, 0, 0, 0, 0, handle);
                fileNameEncrypted = fileName;
                initialized = true;
                incrementRef();
            } catch (CBFSFilterException e) {
                e.printStackTrace();
                initialized = false;
            }
        }

        public void close() {

            try {
                if (stream != null) {

                    stream.close();
                    stream = null;
                }
            }
            catch (IOException e)
            {
                e.printStackTrace();
            }
        }

        public void moveFileTo(String fileName, String newFileName, boolean decrypt, boolean encrypt) throws FileNotFoundException {

            CBFSFilterStream renameHandle;
            try {
                renameHandle = cbfFilter.createFileDirectAsStream(newFileName, false, (int)GENERIC_WRITE, (int)CREATE_ALWAYS, (int)FILE_FLAG_NO_BUFFERING, 0, handle);
            } catch (CBFSFilterException e) {
                e.printStackTrace();
                addToLog(String.format("moveTo! OpenTargetName FAILED!!!! Error(%s)", e.getMessage()));
                return;
            }

            if (stream != null) {
                final int bufferSize = 8192;
                byte[] proxyBuffer;

                try {
                    int completed = 0;
                    while (true) {
                        proxyBuffer = null;
                        try {
                            stream.seek(completed, CBFSFilterStream.SeekOrigin.FROM_BEGIN);
                            proxyBuffer = stream.read(bufferSize);
                        }
                        catch (Exception e) {
                            e.printStackTrace();
                        }
                        if (proxyBuffer == null)
                            break;

                        if (encrypt)
                            encryptDecryptBuffer(proxyBuffer, proxyBuffer.length);
                        if (decrypt)
                            encryptDecryptBuffer(proxyBuffer, proxyBuffer.length);

                        renameHandle.seek(completed, CBFSFilterStream.SeekOrigin.FROM_BEGIN);
                        renameHandle.write(proxyBuffer);

                        completed += proxyBuffer.length;

                    }

                    renameHandle.close();
                    renameHandle = null;

                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }

        protected void encryptDecryptBuffer(byte[] buffer, int length) {
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

        public boolean openFile(String fileName) {
            try {
                if (stream == null)
                    stream = cbfFilter.createFileDirectAsStream(fileName, true, 0, 0, 0, 0, handle);
                return stream !=null;
            } catch (CBFSFilterException e) {
                e.printStackTrace();
            }
            return false;
        }
    }

}






