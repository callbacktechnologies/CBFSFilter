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
import javax.swing.*;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.table.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.util.TooManyListenersException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.regex.Pattern;

import cbfsfilter.*;

public class dirhide implements cbfsfilter.CBFilterEventListener {
    private CBFilter cbfFilter = new CBFilter();

    public static final String appName = "Directory Hider";
    private final String guid = "{713CC6CE-B3E2-4fd9-838D-E28F558F6866}";
    private final String ALTITUDE_FAKE_VALUE_FOR_DEBUG = "360000.24";
    private final int ERROR_FILE_NOT_FOUND = 2;
    private final int ERROR_PATH_NOT_FOUND = 3;
    private final int ERROR_ACCESS_DENIED = 5;
    private final int ERROR_PRIVILEGE_NOT_HELD = 1314;

    private static final Pattern DRIVE_LETTER_PATTERN = Pattern.compile("^[A-Za-z]:$");

    private String strPathToHide;
    private String strOwnNameToHide;

    private JPanel panelRoot;
    private JButton btnInstall;
    private JButton btnUninstall;
    private JTextField textPath;
    private JButton btnClearLog;
    private JTable tableLog;
    private JButton btnHide;
    private JButton btnUnhide;
    private JLabel lblDriverStatus;
    private JScrollPane tableScroll;
    private JCheckBox chkAutoScroll;

    private DefaultTableModel tableModel;
    private boolean closing;

    public dirhide() {
        btnClearLog.addActionListener(new ActionListener() {
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
        btnHide.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                onHideClick();
            }
        });
        btnUnhide.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                onUnhideClick();
            }
        });
    }

    public static void main(String[] args) {
        JFrame frame = new JFrame(dirhide.appName);
        dirhide dirhideDemo = new dirhide();
        frame.setContentPane(dirhideDemo.panelRoot);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        dirhideDemo.initComponent(frame);
        frame.pack();
        frame.setVisible(true);
    }


    public void initComponent(JFrame frame)
    {
        tableModel = new DefaultTableModel();
        tableModel.addColumn("Operation");
        tableModel.addColumn("Path");
        tableModel.addColumn("Originator Process");
        tableModel.addColumn("Process ID");
        tableModel.addColumn("User Name");
        tableModel.addColumn("Result");
        tableLog.setModel(tableModel);
        tableLog.setDefaultEditor(Object.class, null);
        tableModel.addTableModelListener(new TableModelListener() {
            @Override
            public void tableChanged(TableModelEvent tme) {
                btnClearLog.setEnabled(tableModel.getRowCount() > 0);
            }
        });

        try {
            cbfFilter.addCBFilterEventListener(this);
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


    protected void showMessageBox(String message)
    {
        JOptionPane.showMessageDialog(null, message, appName, JOptionPane.INFORMATION_MESSAGE);
    }

    protected void updateButtons()
    {
        try {
            int moduleStatus = cbfFilter.getDriverStatus(guid);
            long moduleVersion = cbfFilter.getDriverVersion(guid);
            int versionHigh = (int)(moduleVersion >> 32);
            int versionLow = (int)(moduleVersion & 0xFFFFFFFF);

            btnHide.setEnabled(!cbfFilter.isActive() && moduleStatus != 0);
            btnUnhide.setEnabled(cbfFilter.isActive());
            btnInstall.setEnabled(moduleStatus == 0);
            btnUninstall.setEnabled(moduleStatus != 0);
            btnClearLog.setEnabled(tableModel.getRowCount() > 0);
            if (moduleStatus != 0)
                lblDriverStatus.setText(String.format("Driver version: %d.%d.%d.%d",
                        versionHigh >> 16, versionHigh & 0xFFFF, versionLow >> 16, versionLow & 0xFFFF));
            else
                lblDriverStatus.setText("Driver: not installed");
        } catch (CBFSFilterException e) {
            showMessageBox(e.getMessage());
        }
    }


    protected void onInstallClick()
    {
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
            boolean reboot = cbfFilter.install(fc.getSelectedFile().getPath(), guid, "", ALTITUDE_FAKE_VALUE_FOR_DEBUG, 0, "");

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

    protected void onUninstallClick()
    {
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

    protected void onHideClick()
    {
        if (cbfFilter.isActive())
        {
            showMessageBox( "Some directory is already hidden. Unhide the previous directory first.");
            return;
        }
        strPathToHide = ConvertRelativePathToAbsolute(textPath.getText());
        if (isNullOrEmpty(strPathToHide)) {
            showMessageBox("Invalid Path To Hide");
            return;
        }

        File pathToHide = new File(strPathToHide);

        if (!(pathToHide.exists()))
        {
            showMessageBox("The specified directory does not exist, nothing to hide.");
            return;
        }
        // strip ending backslashes
        while (strPathToHide.endsWith(File.separator))
            strPathToHide = strPathToHide.substring(0, strPathToHide.length() - 1);

        String parent = pathToHide.getParent();
        // Check that it's not a root to be hidden
        if (parent == null)
        {
            showMessageBox("The specified directory is root, and roots cannot be hidden.");
            return;
        }

        strOwnNameToHide = pathToHide.getName();

        try {
            cbfFilter.initialize(guid);

            // Hide directory from the parent's enumerations
            cbfFilter.addFilterRule(parent,
                    Constants.ACCESS_NONE,
                    Constants.FS_CE_AFTER_ENUMERATE_DIRECTORY,
                    Constants.FS_NE_NONE);

            // Prevent direct opening of the directory
            cbfFilter.addFilterRule(strPathToHide,
                    Constants.ACCESS_NONE,
                    Constants.FS_CE_BEFORE_OPEN |
                            Constants.FS_CE_BEFORE_CREATE,
                    Constants.FS_NE_NONE);

            // Prevent direct operations on files and subdirectories in the directory
            cbfFilter.addFilterRule(strPathToHide + File.separator + "*.*",
                    Constants.ACCESS_NONE,
                    Constants.FS_CE_BEFORE_OPEN |
                            Constants.FS_CE_BEFORE_CREATE,
                    Constants.FS_NE_NONE);

            cbfFilter.setProcessFailedRequests(false);

            cbfFilter.startFilter(30000);
        } catch (CBFSFilterException e) {
            showMessageBox(e.getMessage());
        }
        updateButtons();
    }

    protected void onUnhideClick()
    {
        try {
            cbfFilter.deleteAllFilterRules();
            cbfFilter.deleteAllPassthroughRules();
            if (cbfFilter.isActive())
                cbfFilter.stopFilter();
            btnHide.setEnabled(true);
            btnUnhide.setEnabled(false);
        } catch (CBFSFilterException e) {
            showMessageBox(e.getMessage());
        }
    }

    protected void addToLog(String operation, String filename, int status)
    {
        if (closing) return;

        String processName;
        try {
            processName = cbfFilter.getOriginatorProcessName();
            if (processName.length() == 0)
                processName = "System";
        } catch (CBFSFilterException e) {
            processName = "< ERROR >";
        }

        int processID;
        long userTok;
        try {
            processID = cbfFilter.getOriginatorProcessId();
            userTok = cbfFilter.getOriginatorToken();
        } catch (CBFSFilterException e) {
            processID = 0;
            userTok = 0;
        }

        // TODO get real user name
        String userName = "< unknown >";

        tableModel.addRow(new Object[] {operation, filename, processName, String.format("%d", processID), userName, String.format("%d", status)});
        tableModel.fireTableRowsInserted(tableModel.getRowCount() - 1, tableModel.getRowCount());
        Dimension d = new Dimension(tableLog.getPreferredScrollableViewportSize().width, tableLog.getRowHeight() * tableLog.getRowCount());
        tableLog.setPreferredSize(d);
        if (this.chkAutoScroll.isSelected())
        {
            int last = tableModel.getRowCount() - 1;
            Rectangle r = tableLog.getCellRect(last, 0, true);
            tableLog.scrollRectToVisible(r);
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

    private String getTitle() {
        return  appName;
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
                try {
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
                } catch (Exception ex) {
                    showMessageBox(String.format("ConvertRelativePathToAbsolute: exception '%s'", ex.toString()));
                    return null;
                }
            }
        }
        return res;
    }

    protected void onClearLogClick()
    {
        tableModel.getDataVector().removeAllElements();
        tableModel.fireTableDataChanged();
        Dimension d = new Dimension(tableLog.getPreferredScrollableViewportSize().width, tableLog.getRowHeight() * tableLog.getRowCount());
        tableLog.setPreferredSize(d);
    }

    protected void onFormClosing()
    {
        closing = true;

        if (cbfFilter.isActive())
            try {
                cbfFilter.dispose();
            } catch (CBFSFilterException e) {
                showMessageBox(e.getMessage());
            }
    }

    @Override
    public void cleanupContext(CBFilterCleanupContextEvent cbfilterCleanupContextEvent) {

    }

    @Override
    public void afterCanFileBeDeleted(CBFilterAfterCanFileBeDeletedEvent cbfilterAfterCanFileBeDeletedEvent) {

    }

    @Override
    public void afterCleanupFile(CBFilterAfterCleanupFileEvent cbfilterAfterCleanupFileEvent) {

    }

    @Override
    public void afterCloseEnumeration(CBFilterAfterCloseEnumerationEvent cbfilterAfterCloseEnumerationEvent) {

    }

    @Override
    public void afterCloseFile(CBFilterAfterCloseFileEvent cbfilterAfterCloseFileEvent) {

    }

    @Override
    public void afterCreateFile(CBFilterAfterCreateFileEvent cbfilterAfterCreateFileEvent) {

    }

    @Override
    public void afterCreateHardLink(CBFilterAfterCreateHardLinkEvent cbfilterAfterCreateHardLinkEvent) {

    }

    @Override
    public void afterDeleteFile(CBFilterAfterDeleteFileEvent cbfilterAfterDeleteFileEvent) {

    }

    @Override
    public void afterDeleteReparsePoint(CBFilterAfterDeleteReparsePointEvent cbfilterAfterDeleteReparsePointEvent) {

    }

    @Override
    public void afterEnumerateDirectory(CBFilterAfterEnumerateDirectoryEvent e) {
        // if the filesystem returned the name of the directory that we aim to hide, then hide it
        if (e.fileName.equalsIgnoreCase(strOwnNameToHide))
        {
            addToLog("afterEnumerateDirectory", e.fileName, ERROR_FILE_NOT_FOUND);
            e.processRequest = false;
        }
    }

    @Override
    public void afterFilterAttachToVolume(CBFilterAfterFilterAttachToVolumeEvent cbfilterAfterFilterAttachToVolumeEvent) {

    }

    @Override
    public void afterFilterDetachFromVolume(CBFilterAfterFilterDetachFromVolumeEvent cbfilterAfterFilterDetachFromVolumeEvent) {

    }

    @Override
    public void afterFsctl(CBFilterAfterFsctlEvent cbfilterAfterFsctlEvent) {

    }

    @Override
    public void afterGetFileSecurity(CBFilterAfterGetFileSecurityEvent cbfilterAfterGetFileSecurityEvent) {

    }

    @Override
    public void afterGetFileSizes(CBFilterAfterGetFileSizesEvent cbfilterAfterGetFileSizesEvent) {

    }

    @Override
    public void afterGetReparsePoint(CBFilterAfterGetReparsePointEvent cbfilterAfterGetReparsePointEvent) {

    }

    @Override
    public void afterIoctl(CBFilterAfterIoctlEvent cbfilterAfterIoctlEvent) {

    }

    @Override
    public void afterLock(CBFilterAfterLockEvent cbfilterAfterLockEvent) {

    }

    @Override
    public void afterOpenFile(CBFilterAfterOpenFileEvent cbfilterAfterOpenFileEvent) {

    }

    @Override
    public void afterQueryEa(CBFilterAfterQueryEaEvent cbfilterAfterQueryEaEvent) {

    }

    @Override
    public void afterQueryFileInfo(CBFilterAfterQueryFileInfoEvent cbfilterAfterQueryFileInfoEvent) {

    }

    @Override
    public void afterReadFile(CBFilterAfterReadFileEvent cbfilterAfterReadFileEvent) {

    }

    @Override
    public void afterRenameOrMoveFile(CBFilterAfterRenameOrMoveFileEvent cbfilterAfterRenameOrMoveFileEvent) {

    }

    @Override
    public void afterSetAllocationSize(CBFilterAfterSetAllocationSizeEvent cbfilterAfterSetAllocationSizeEvent) {

    }

    @Override
    public void afterSetEa(CBFilterAfterSetEaEvent cbfilterAfterSetEaEvent) {

    }

    @Override
    public void afterSetFileSize(CBFilterAfterSetFileSizeEvent cbfilterAfterSetFileSizeEvent) {

    }

    @Override
    public void afterSetReparsePoint(CBFilterAfterSetReparsePointEvent cbfilterAfterSetReparsePointEvent) {

    }

    @Override
    public void afterSetFileAttributes(CBFilterAfterSetFileAttributesEvent cbfilterAfterSetFileAttributesEvent) {

    }

    @Override
    public void afterSetFileInfo(CBFilterAfterSetFileInfoEvent cbfilterAfterSetFileInfoEvent) {

    }

    @Override
    public void afterSetFileSecurity(CBFilterAfterSetFileSecurityEvent cbfilterAfterSetFileSecurityEvent) {

    }

    @Override
    public void afterUnlockAll(CBFilterAfterUnlockAllEvent cbfilterAfterUnlockAllEvent) {

    }

    @Override
    public void afterUnlockAllByKey(CBFilterAfterUnlockAllByKeyEvent cbfilterAfterUnlockAllByKeyEvent) {

    }

    @Override
    public void afterUnlockSingle(CBFilterAfterUnlockSingleEvent cbfilterAfterUnlockSingleEvent) {

    }

    @Override
    public void afterWriteFile(CBFilterAfterWriteFileEvent cbfilterAfterWriteFileEvent) {

    }

    @Override
    public void beforeCanFileBeDeleted(CBFilterBeforeCanFileBeDeletedEvent cbfilterBeforeCanFileBeDeletedEvent) {

    }

    @Override
    public void beforeCleanupFile(CBFilterBeforeCleanupFileEvent cbfilterBeforeCleanupFileEvent) {

    }

    @Override
    public void beforeCloseFile(CBFilterBeforeCloseFileEvent cbfilterBeforeCloseFileEvent) {

    }

    @Override
    public void beforeCreateFile(CBFilterBeforeCreateFileEvent e) {
        if (e.fileName.equalsIgnoreCase(strPathToHide))
        {
            addToLog("beforeCreateFile", e.fileName, ERROR_FILE_NOT_FOUND);
            e.resultCode = ERROR_FILE_NOT_FOUND;
            return;
        }
        String dir = new File(e.fileName).getParent();
        if (dir.equalsIgnoreCase(strPathToHide))
        {
            e.resultCode = ERROR_PATH_NOT_FOUND;
        }
    }

    @Override
    public void beforeCreateHardLink(CBFilterBeforeCreateHardLinkEvent cbfilterBeforeCreateHardLinkEvent) {

    }

    @Override
    public void beforeDeleteFile(CBFilterBeforeDeleteFileEvent cbfilterBeforeDeleteFileEvent) {

    }

    @Override
    public void beforeDeleteReparsePoint(CBFilterBeforeDeleteReparsePointEvent cbfilterBeforeDeleteReparsePointEvent) {

    }

    @Override
    public void beforeEnumerateDirectory(CBFilterBeforeEnumerateDirectoryEvent cbfilterBeforeEnumerateDirectoryEvent) {

    }

    @Override
    public void beforeFilterAttachToVolume(CBFilterBeforeFilterAttachToVolumeEvent cbfilterBeforeFilterAttachToVolumeEvent) {

    }

    @Override
    public void beforeFsctl(CBFilterBeforeFsctlEvent cbfilterBeforeFsctlEvent) {

    }

    @Override
    public void beforeGetFileSecurity(CBFilterBeforeGetFileSecurityEvent cbfilterBeforeGetFileSecurityEvent) {

    }

    @Override
    public void beforeGetReparsePoint(CBFilterBeforeGetReparsePointEvent cbfilterBeforeGetReparsePointEvent) {

    }

    @Override
    public void beforeIoctl(CBFilterBeforeIoctlEvent cbfilterBeforeIoctlEvent) {

    }

    @Override
    public void beforeLock(CBFilterBeforeLockEvent cbfilterBeforeLockEvent) {

    }

    @Override
    public void beforeOpenFile(CBFilterBeforeOpenFileEvent e) {
        if (e.fileName.equalsIgnoreCase(strPathToHide))
        {
            addToLog("beforeOpenFile", e.fileName, ERROR_FILE_NOT_FOUND);
            e.resultCode = ERROR_FILE_NOT_FOUND;
            return;
        }
        String dir = new File(e.fileName).getParent();
        if (dir.equalsIgnoreCase(strPathToHide))
        {
            e.resultCode = ERROR_PATH_NOT_FOUND;
        }
    }

    @Override
    public void beforeQueryEa(CBFilterBeforeQueryEaEvent cbfilterBeforeQueryEaEvent) {

    }

    @Override
    public void beforeQueryFileInfo(CBFilterBeforeQueryFileInfoEvent cbfilterBeforeQueryFileInfoEvent) {

    }

    @Override
    public void beforeReadFile(CBFilterBeforeReadFileEvent cbfilterBeforeReadFileEvent) {

    }

    @Override
    public void beforeRenameOrMoveFile(CBFilterBeforeRenameOrMoveFileEvent cbfilterBeforeRenameOrMoveFileEvent) {

    }

    @Override
    public void beforeSetAllocationSize(CBFilterBeforeSetAllocationSizeEvent cbfilterBeforeSetAllocationSizeEvent) {

    }

    @Override
    public void beforeSetEa(CBFilterBeforeSetEaEvent cbfilterBeforeSetEaEvent) {

    }

    @Override
    public void beforeSetFileSize(CBFilterBeforeSetFileSizeEvent cbfilterBeforeSetFileSizeEvent) {

    }

    @Override
    public void beforeSetReparsePoint(CBFilterBeforeSetReparsePointEvent cbfilterBeforeSetReparsePointEvent) {

    }

    @Override
    public void beforeSetFileAttributes(CBFilterBeforeSetFileAttributesEvent cbfilterBeforeSetFileAttributesEvent) {

    }

    @Override
    public void beforeSetFileInfo(CBFilterBeforeSetFileInfoEvent cbfilterBeforeSetFileInfoEvent) {

    }

    @Override
    public void beforeSetFileSecurity(CBFilterBeforeSetFileSecurityEvent cbfilterBeforeSetFileSecurityEvent) {

    }

    @Override
    public void beforeUnlockAll(CBFilterBeforeUnlockAllEvent cbfilterBeforeUnlockAllEvent) {

    }

    @Override
    public void beforeUnlockAllByKey(CBFilterBeforeUnlockAllByKeyEvent cbfilterBeforeUnlockAllByKeyEvent) {

    }

    @Override
    public void beforeUnlockSingle(CBFilterBeforeUnlockSingleEvent cbfilterBeforeUnlockSingleEvent) {

    }

    @Override
    public void beforeWriteFile(CBFilterBeforeWriteFileEvent cbfilterBeforeWriteFileEvent) {

    }

    @Override
    public void error(CBFilterErrorEvent cbfilterErrorEvent) {

    }

    @Override
    public void filterStart(CBFilterFilterStartEvent cbfilterFilterStartEvent) {

    }

    @Override
    public void filterStop(CBFilterFilterStopEvent cbfilterFilterStopEvent) {
        if (! closing)
            updateButtons();
    }

    @Override
    public void notifyCanFileBeDeleted(CBFilterNotifyCanFileBeDeletedEvent cbfilterNotifyCanFileBeDeletedEvent) {

    }

    @Override
    public void notifyCleanupFile(CBFilterNotifyCleanupFileEvent cbfilterNotifyCleanupFileEvent) {

    }

    @Override
    public void notifyCloseFile(CBFilterNotifyCloseFileEvent cbfilterNotifyCloseFileEvent) {

    }

    @Override
    public void notifyCreateFile(CBFilterNotifyCreateFileEvent cbfilterNotifyCreateFileEvent) {

    }

    @Override
    public void notifyCreateHardLink(CBFilterNotifyCreateHardLinkEvent cbfilterNotifyCreateHardLinkEvent) {

    }

    @Override
    public void notifyDeleteFile(CBFilterNotifyDeleteFileEvent cbfilterNotifyDeleteFileEvent) {

    }

    @Override
    public void notifyDeleteReparsePoint(CBFilterNotifyDeleteReparsePointEvent cbfilterNotifyDeleteReparsePointEvent) {

    }

    @Override
    public void notifyEnumerateDirectory(CBFilterNotifyEnumerateDirectoryEvent cbfilterNotifyEnumerateDirectoryEvent) {

    }

    @Override
    public void notifyFilterAttachToVolume(CBFilterNotifyFilterAttachToVolumeEvent cbfilterNotifyFilterAttachToVolumeEvent) {

    }

    @Override
    public void notifyFilterDetachFromVolume(CBFilterNotifyFilterDetachFromVolumeEvent cbfilterNotifyFilterDetachFromVolumeEvent) {

    }

    @Override
    public void notifyFsctl(CBFilterNotifyFsctlEvent cbfilterNotifyFsctlEvent) {

    }

    @Override
    public void notifyGetFileSecurity(CBFilterNotifyGetFileSecurityEvent cbfilterNotifyGetFileSecurityEvent) {

    }

    @Override
    public void notifyGetFileSizes(CBFilterNotifyGetFileSizesEvent cbfilterNotifyGetFileSizesEvent) {

    }

    @Override
    public void notifyGetReparsePoint(CBFilterNotifyGetReparsePointEvent cbfilterNotifyGetReparsePointEvent) {

    }

    @Override
    public void notifyIoctl(CBFilterNotifyIoctlEvent cbfilterNotifyIoctlEvent) {

    }

    @Override
    public void notifyLock(CBFilterNotifyLockEvent cbfilterNotifyLockEvent) {

    }

    @Override
    public void notifyOpenFile(CBFilterNotifyOpenFileEvent cbfilterNotifyOpenFileEvent) {

    }

    @Override
    public void notifyQueryEa(CBFilterNotifyQueryEaEvent cbfilterNotifyQueryEaEvent) {

    }

    @Override
    public void notifyQueryFileInfo(CBFilterNotifyQueryFileInfoEvent cbfilterNotifyQueryFileInfoEvent) {

    }

    @Override
    public void notifyReadFile(CBFilterNotifyReadFileEvent cbfilterNotifyReadFileEvent) {

    }

    @Override
    public void notifyRenameOrMoveFile(CBFilterNotifyRenameOrMoveFileEvent cbfilterNotifyRenameOrMoveFileEvent) {

    }

    @Override
    public void notifySetAllocationSize(CBFilterNotifySetAllocationSizeEvent cbfilterNotifySetAllocationSizeEvent) {

    }

    @Override
    public void notifySetEa(CBFilterNotifySetEaEvent cbfilterNotifySetEaEvent) {

    }

    @Override
    public void notifySetFileSize(CBFilterNotifySetFileSizeEvent cbfilterNotifySetFileSizeEvent) {

    }

    @Override
    public void notifySetReparsePoint(CBFilterNotifySetReparsePointEvent cbfilterNotifySetReparsePointEvent) {

    }

    @Override
    public void notifySetFileAttributes(CBFilterNotifySetFileAttributesEvent cbfilterNotifySetFileAttributesEvent) {

    }

    @Override
    public void notifySetFileInfo(CBFilterNotifySetFileInfoEvent cbfilterNotifySetFileInfoEvent) {

    }

    @Override
    public void notifySetFileSecurity(CBFilterNotifySetFileSecurityEvent cbfilterNotifySetFileSecurityEvent) {

    }

    @Override
    public void notifyUnlockAll(CBFilterNotifyUnlockAllEvent cbfilterNotifyUnlockAllEvent) {

    }

    @Override
    public void notifyUnlockAllByKey(CBFilterNotifyUnlockAllByKeyEvent cbfilterNotifyUnlockAllByKeyEvent) {

    }

    @Override
    public void notifyUnlockSingle(CBFilterNotifyUnlockSingleEvent cbfilterNotifyUnlockSingleEvent) {

    }

    @Override
    public void notifyWriteFile(CBFilterNotifyWriteFileEvent cbfilterNotifyWriteFileEvent) {

    }

    @Override
    public void reparseFileName(CBFilterReparseFileNameEvent cbfilterReparseFileNameEvent) {

    }

    @Override
    public void reparseWithTag(CBFilterReparseWithTagEvent cbfilterReparseWithTagEvent) {

    }

    @Override
    public void workerThreadCreation(CBFilterWorkerThreadCreationEvent cbfilterWorkerThreadCreationEvent) {

    }

    @Override
    public void workerThreadTermination(CBFilterWorkerThreadTerminationEvent cbfilterWorkerThreadTerminationEvent) {

    }
}






