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

import cbfsfilter.*;

public class filter implements cbfsfilter.CBFilterEventListener {
    private CBFilter cbfFilter = new CBFilter();

    private static final String appName = "Filter";
    private final String guid = "{713CC6CE-B3E2-4fd9-838D-E28F558F6866}";
    private final String ALTITUDE_FAKE_VALUE_FOR_DEBUG = "360000.24";
    private final int ERROR_ACCESS_DENIED = 5;
    private final int ERROR_PRIVILEGE_NOT_HELD = 1314;

    private JPanel panelRoot;
    private JButton btnInstall;
    private JButton btnUninstall;
    private JTextField textMask;
    private JButton btnAcceptRule;
    private JTable listRules;
    private JButton btnAttachFilter;
    private JButton btnDetachFilter;
    private JLabel lblDriverStatus;
    private JScrollPane tableScroll;
    private JComboBox cbFilters;
    private JButton btnRemove;

    private DefaultTableModel rulesModel;
    private int notifyFlagsFirst;
    private int controlFlagsFirst;
    private boolean closing;

    public filter() {
        btnAcceptRule.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                onAcceptRuleClick();
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
        btnAttachFilter.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                onSetFilterClick();
            }
        });
        btnDetachFilter.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                onDeleteFilterClick();
            }
        });
        btnRemove.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                onRemoveClick();
            }
        });
        btnAcceptRule.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                onAcceptRuleClick();
            }
        });
    }

    private void initComponent(JFrame frame)
    {
        rulesModel = new DefaultTableModel();
        rulesModel.addColumn("Filter Mask");
        rulesModel.addColumn("Access Flags");
        rulesModel.addColumn("Notify Flags");
        rulesModel.addColumn("Control Flags");
        listRules.setModel(rulesModel);
        listRules.setDefaultEditor(Object.class, null);
        rulesModel.addTableModelListener(new TableModelListener() {
            @Override
            public void tableChanged(TableModelEvent tme) {
                btnRemove.setEnabled(listRules.getSelectedRowCount() > 0);
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

        for (RuleFlag flag : AccessFlags)
            cbFilters.addItem(flag);

        notifyFlagsFirst = cbFilters.getItemCount();
        for (RuleFlag flag : NotifyFlags)
            cbFilters.addItem(flag);

        controlFlagsFirst = cbFilters.getItemCount();
        for (RuleFlag flag : ControlFlags)
            cbFilters.addItem(flag);

        cbFilters.setSelectedIndex(0);
        updateButtons();
        updateFilterList();
    }


    private void showMessageBox(String message)
    {
        JOptionPane.showMessageDialog(null, message, appName, JOptionPane.INFORMATION_MESSAGE);
    }

    private void updateButtons()
    {
        try {
            int moduleStatus = cbfFilter.getDriverStatus(guid);
            long moduleVersion = cbfFilter.getDriverVersion(guid);
            int versionHigh = (int)(moduleVersion >> 32);
            int versionLow = (int)(moduleVersion & 0xFFFFFFFF);

            btnAttachFilter.setEnabled(!cbfFilter.isActive() && moduleStatus != 0);
            btnDetachFilter.setEnabled(cbfFilter.isActive());
            btnInstall.setEnabled(moduleStatus == 0);
            btnUninstall.setEnabled(moduleStatus != 0);
            btnAcceptRule.setEnabled(moduleStatus != 0);
            if (moduleStatus != 0)
                lblDriverStatus.setText(String.format("Driver version: %d.%d.%d.%d",
                        versionHigh >> 16, versionHigh & 0xFFFF, versionLow >> 16, versionLow & 0xFFFF));
            else
                lblDriverStatus.setText("Driver: not installed");
        } catch (CBFSFilterException e) {
            showMessageBox(e.getMessage());
        }
    }


    private void onInstallClick()
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
            if (e.getCode() == ERROR_PRIVILEGE_NOT_HELD || e.getCode() == ERROR_ACCESS_DENIED)
                showMessageBox("Installation requires administrator rights. Run the app as administrator");
            else
                showMessageBox(e.getMessage());
        }
    }

    private void onUninstallClick()
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
            if (e.getCode() == ERROR_PRIVILEGE_NOT_HELD || e.getCode() == ERROR_ACCESS_DENIED)
                showMessageBox("Uninstallation requires administrator rights. Run the app as administrator");
            else
                showMessageBox(e.getMessage());
        }
    }

    private void onSetFilterClick()
    {
        try {
            cbfFilter.initialize(guid);
            cbfFilter.setProcessFailedRequests(true);
            cbfFilter.startFilter(30000);
        } catch (CBFSFilterException e) {
            showMessageBox(e.getMessage());
        }

        updateButtons();
        updateFilterList();
    }

    private void onDeleteFilterClick()
    {
        try {
            cbfFilter.deleteAllFilterRules();
            cbfFilter.deleteAllPassthroughRules();
            if (cbfFilter.isActive())
                cbfFilter.stopFilter();
        } catch (CBFSFilterException e) {
            showMessageBox(e.getMessage());
        }
    }

    private void onAcceptRuleClick()
    {
        try {
            int index = cbFilters.getSelectedIndex();

            if (index < notifyFlagsFirst)
                cbfFilter.addFilterRule(textMask.getText(), (int)((RuleFlag)cbFilters.getItemAt(index)).value, Constants.FS_CE_NONE, Constants.FS_NE_NONE);
            else
            if (index < controlFlagsFirst)
                cbfFilter.addFilterRule(textMask.getText(), Constants.ACCESS_NONE, Constants.FS_CE_NONE, ((RuleFlag)cbFilters.getItemAt(index)).value);
            else
                cbfFilter.addFilterRule(textMask.getText(), Constants.ACCESS_NONE, ((RuleFlag)cbFilters.getItemAt(index)).value, Constants.FS_NE_NONE);

            updateFilterList();
        } catch (CBFSFilterException e) {
            showMessageBox(e.getMessage());
        }
    }

    private void onRemoveClick()
    {
        try {
            int[] selected = listRules.getSelectedRows();
            for (int i = 0; i < selected.length; i++) {
                cbfFilter.deleteFilterRule((String)listRules.getValueAt(selected[i], 0), Constants.ACCESS_ALL_FLAGS, Constants.FS_CE_ALL, Constants.FS_NE_ALL);
            }
            updateFilterList();
        } catch (CBFSFilterException e) {
            showMessageBox(e.getMessage());
        }
    }

    private void updateFilterList()
    {
        rulesModel.getDataVector().removeAllElements();

        for (int i = 0; i < cbfFilter.getFilterRules().size(); i++) {
            rulesModel.addRow(new Object[]{cbfFilter.getFilterRules().item(i).getMask(),
                    String.format("%02X", cbfFilter.getFilterRules().item(i).getAccessFlags()),
                    String.format("%012X", cbfFilter.getFilterRules().item(i).getControlFlags()),
                    String.format("%06X", cbfFilter.getFilterRules().item(i).getNotifyFlags())});
        }

        rulesModel.fireTableDataChanged();
        Dimension d = new Dimension(listRules.getPreferredScrollableViewportSize().width, listRules.getRowHeight() * listRules.getRowCount());
        listRules.setPreferredSize(d);
    }

    private void onFormClosing()
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
    public void afterCanFileBeDeleted(CBFilterAfterCanFileBeDeletedEvent e) {
        /* insert your code here */
    }

    @Override
    public void afterCleanupFile(CBFilterAfterCleanupFileEvent e) {
        /* insert your code here */
    }

    @Override
    public void afterCloseEnumeration(CBFilterAfterCloseEnumerationEvent e) {
        /* insert your code here */
    }

    @Override
    public void afterCloseFile(CBFilterAfterCloseFileEvent e) {
        /* insert your code here */
    }

    @Override
    public void afterCreateFile(CBFilterAfterCreateFileEvent e) {
        /* insert your code here */
    }

    @Override
    public void afterCreateHardLink(CBFilterAfterCreateHardLinkEvent e) {
        /* insert your code here */
    }

    @Override
    public void afterDeleteFile(CBFilterAfterDeleteFileEvent e) {
        /* insert your code here */
    }

    @Override
    public void afterDeleteReparsePoint(CBFilterAfterDeleteReparsePointEvent cbfilterAfterDeleteReparsePointEvent) {

    }

    @Override
    public void afterEnumerateDirectory(CBFilterAfterEnumerateDirectoryEvent e) {
        /* insert your code here */
    }

    @Override
    public void afterFilterAttachToVolume(CBFilterAfterFilterAttachToVolumeEvent e) {

    }

    @Override
    public void afterFilterDetachFromVolume(CBFilterAfterFilterDetachFromVolumeEvent e) {

    }

    @Override
    public void afterFsctl(CBFilterAfterFsctlEvent e) {
        /* insert your code here */
    }

    @Override
    public void afterGetFileSecurity(CBFilterAfterGetFileSecurityEvent e) {
        /* insert your code here */
    }

    @Override
    public void afterGetFileSizes(CBFilterAfterGetFileSizesEvent e) {
        /* insert your code here */
    }

    @Override
    public void afterGetReparsePoint(CBFilterAfterGetReparsePointEvent cbfilterAfterGetReparsePointEvent) {

    }

    @Override
    public void afterIoctl(CBFilterAfterIoctlEvent e) {
        /* insert your code here */
    }

    @Override
    public void afterLock(CBFilterAfterLockEvent e) {
        /* insert your code here */
    }

    @Override
    public void afterOpenFile(CBFilterAfterOpenFileEvent e) {
        /* insert your code here */
    }

    @Override
    public void afterQueryEa(CBFilterAfterQueryEaEvent cbfilterAfterQueryEaEvent) {

    }

    @Override
    public void afterQueryFileInfo(CBFilterAfterQueryFileInfoEvent e) {
        /* insert your code here */
    }

    @Override
    public void afterReadFile(CBFilterAfterReadFileEvent e) {
        /* insert your code here */
    }

    @Override
    public void afterRenameOrMoveFile(CBFilterAfterRenameOrMoveFileEvent e) {
        /* insert your code here */
    }

    @Override
    public void afterSetAllocationSize(CBFilterAfterSetAllocationSizeEvent e) {
        /* insert your code here */
    }

    @Override
    public void afterSetEa(CBFilterAfterSetEaEvent cbfilterAfterSetEaEvent) {

    }

    @Override
    public void afterSetFileSize(CBFilterAfterSetFileSizeEvent e) {
        /* insert your code here */
    }

    @Override
    public void afterSetReparsePoint(CBFilterAfterSetReparsePointEvent cbfilterAfterSetReparsePointEvent) {

    }

    @Override
    public void afterSetFileAttributes(CBFilterAfterSetFileAttributesEvent e) {
        /* insert your code here */
    }

    @Override
    public void afterSetFileInfo(CBFilterAfterSetFileInfoEvent cbfilterAfterSetFileInfoEvent) {

    }

    @Override
    public void afterSetFileSecurity(CBFilterAfterSetFileSecurityEvent e) {
        /* insert your code here */
    }

    @Override
    public void afterUnlockAll(CBFilterAfterUnlockAllEvent e) {
        /* insert your code here */
    }

    @Override
    public void afterUnlockAllByKey(CBFilterAfterUnlockAllByKeyEvent e) {
        /* insert your code here */
    }

    @Override
    public void afterUnlockSingle(CBFilterAfterUnlockSingleEvent e) {
        /* insert your code here */
    }

    @Override
    public void afterWriteFile(CBFilterAfterWriteFileEvent e) {
        /* insert your code here */
    }

    @Override
    public void beforeCanFileBeDeleted(CBFilterBeforeCanFileBeDeletedEvent e) {
        /* insert your code here */
        e.processRequest = true;
    }

    @Override
    public void beforeCleanupFile(CBFilterBeforeCleanupFileEvent e) {
        /* insert your code here */
    }

    @Override
    public void beforeCloseFile(CBFilterBeforeCloseFileEvent e) {
        /* insert your code here */
    }

    @Override
    public void beforeCreateFile(CBFilterBeforeCreateFileEvent e) {
        /* insert your code here */
        e.processRequest = true;
    }

    @Override
    public void beforeCreateHardLink(CBFilterBeforeCreateHardLinkEvent e) {
        /* insert your code here */
        e.processRequest = true;
    }

    @Override
    public void beforeDeleteFile(CBFilterBeforeDeleteFileEvent e) {
        /* insert your code here */
    }

    @Override
    public void beforeDeleteReparsePoint(CBFilterBeforeDeleteReparsePointEvent cbfilterBeforeDeleteReparsePointEvent) {

    }

    @Override
    public void beforeEnumerateDirectory(CBFilterBeforeEnumerateDirectoryEvent cbfilterBeforeEnumerateDirectoryEvent) {

    }

    @Override
    public void beforeFilterAttachToVolume(CBFilterBeforeFilterAttachToVolumeEvent e) {

    }

    @Override
    public void beforeFsctl(CBFilterBeforeFsctlEvent e) {
        /* insert your code here */
        e.processRequest = true;
    }

    @Override
    public void beforeGetFileSecurity(CBFilterBeforeGetFileSecurityEvent e) {
        /* insert your code here */
        e.processRequest = true;
    }

    @Override
    public void beforeGetReparsePoint(CBFilterBeforeGetReparsePointEvent cbfilterBeforeGetReparsePointEvent) {

    }

    @Override
    public void beforeIoctl(CBFilterBeforeIoctlEvent e) {
        /* insert your code here */
        e.processRequest = true;
    }

    @Override
    public void beforeLock(CBFilterBeforeLockEvent e) {
        /* insert your code here */
        e.processRequest = true;
    }

    @Override
    public void beforeOpenFile(CBFilterBeforeOpenFileEvent e) {
        /* insert your code here */
        e.processRequest = true;
    }

    @Override
    public void beforeQueryEa(CBFilterBeforeQueryEaEvent cbfilterBeforeQueryEaEvent) {

    }

    @Override
    public void beforeQueryFileInfo(CBFilterBeforeQueryFileInfoEvent e) {
        /* insert your code here */
        e.processRequest = true;
    }

    @Override
    public void beforeReadFile(CBFilterBeforeReadFileEvent e) {
        /* insert your code here */
        e.processRequest = true;
    }

    @Override
    public void beforeRenameOrMoveFile(CBFilterBeforeRenameOrMoveFileEvent e) {
        /* insert your code here */
        e.processRequest = true;
    }

    @Override
    public void beforeSetAllocationSize(CBFilterBeforeSetAllocationSizeEvent e) {
        /* insert your code here */
        e.processRequest = true;
    }

    @Override
    public void beforeSetEa(CBFilterBeforeSetEaEvent cbfilterBeforeSetEaEvent) {

    }

    @Override
    public void beforeSetFileSize(CBFilterBeforeSetFileSizeEvent e) {
        /* insert your code here */
        e.processRequest = true;
    }

    @Override
    public void beforeSetReparsePoint(CBFilterBeforeSetReparsePointEvent cbfilterBeforeSetReparsePointEvent) {

    }

    @Override
    public void beforeSetFileAttributes(CBFilterBeforeSetFileAttributesEvent e) {
        /* insert your code here */
        e.processRequest = true;
    }

    @Override
    public void beforeSetFileInfo(CBFilterBeforeSetFileInfoEvent cbfilterBeforeSetFileInfoEvent) {

    }

    @Override
    public void beforeSetFileSecurity(CBFilterBeforeSetFileSecurityEvent e) {
        /* insert your code here */
        e.processRequest = true;
    }

    @Override
    public void beforeUnlockAll(CBFilterBeforeUnlockAllEvent e) {
        /* insert your code here */
    }

    @Override
    public void beforeUnlockAllByKey(CBFilterBeforeUnlockAllByKeyEvent e) {
        /* insert your code here */
    }

    @Override
    public void beforeUnlockSingle(CBFilterBeforeUnlockSingleEvent e) {
        /* insert your code here */
    }

    @Override
    public void beforeWriteFile(CBFilterBeforeWriteFileEvent e) {
        /* insert your code here */
        e.processRequest = true;
    }

    @Override
    public void cleanupContext(CBFilterCleanupContextEvent e) {
        /* insert your code here */
    }

    @Override
    public void error(CBFilterErrorEvent e) {

    }

    @Override
    public void filterStart(CBFilterFilterStartEvent e) {

    }

    @Override
    public void filterStop(CBFilterFilterStopEvent e) {
        if ( ! closing)
            updateButtons();
    }

    @Override
    public void notifyCanFileBeDeleted(CBFilterNotifyCanFileBeDeletedEvent e) {
        /* insert your code here */
    }

    @Override
    public void notifyCleanupFile(CBFilterNotifyCleanupFileEvent e) {
        /* insert your code here */
    }

    @Override
    public void notifyCloseFile(CBFilterNotifyCloseFileEvent e) {
        /* insert your code here */
    }

    @Override
    public void notifyCreateFile(CBFilterNotifyCreateFileEvent e) {
        /* insert your code here */
    }

    @Override
    public void notifyCreateHardLink(CBFilterNotifyCreateHardLinkEvent e) {
        /* insert your code here */
    }

    @Override
    public void notifyDeleteFile(CBFilterNotifyDeleteFileEvent e) {
        /* insert your code here */
    }

    @Override
    public void notifyDeleteReparsePoint(CBFilterNotifyDeleteReparsePointEvent cbfilterNotifyDeleteReparsePointEvent) {

    }

    @Override
    public void notifyEnumerateDirectory(CBFilterNotifyEnumerateDirectoryEvent e) {
        /* insert your code here */
    }

    @Override
    public void notifyFilterAttachToVolume(CBFilterNotifyFilterAttachToVolumeEvent e) {
        /* insert your code here */
    }

    @Override
    public void notifyFilterDetachFromVolume(CBFilterNotifyFilterDetachFromVolumeEvent e) {
        /* insert your code here */
    }

    @Override
    public void notifyFsctl(CBFilterNotifyFsctlEvent e) {
        /* insert your code here */
    }

    @Override
    public void notifyGetFileSecurity(CBFilterNotifyGetFileSecurityEvent e) {
        /* insert your code here */
    }

    @Override
    public void notifyGetFileSizes(CBFilterNotifyGetFileSizesEvent e) {
        /* insert your code here */
    }

    @Override
    public void notifyGetReparsePoint(CBFilterNotifyGetReparsePointEvent cbfilterNotifyGetReparsePointEvent) {

    }

    @Override
    public void notifyIoctl(CBFilterNotifyIoctlEvent e) {
        /* insert your code here */
    }

    @Override
    public void notifyLock(CBFilterNotifyLockEvent e) {
        /* insert your code here */
    }

    @Override
    public void notifyOpenFile(CBFilterNotifyOpenFileEvent e) {
        /* insert your code here */
    }

    @Override
    public void notifyQueryEa(CBFilterNotifyQueryEaEvent cbfilterNotifyQueryEaEvent) {

    }

    @Override
    public void notifyQueryFileInfo(CBFilterNotifyQueryFileInfoEvent e) {
        /* insert your code here */
    }

    @Override
    public void notifyReadFile(CBFilterNotifyReadFileEvent e) {
        /* insert your code here */
    }

    @Override
    public void notifyRenameOrMoveFile(CBFilterNotifyRenameOrMoveFileEvent e) {
        /* insert your code here */
    }

    @Override
    public void notifySetAllocationSize(CBFilterNotifySetAllocationSizeEvent e) {
        /* insert your code here */
    }

    @Override
    public void notifySetEa(CBFilterNotifySetEaEvent cbfilterNotifySetEaEvent) {

    }

    @Override
    public void notifySetFileSize(CBFilterNotifySetFileSizeEvent e) {
        /* insert your code here */
    }

    @Override
    public void notifySetReparsePoint(CBFilterNotifySetReparsePointEvent cbfilterNotifySetReparsePointEvent) {

    }

    @Override
    public void notifySetFileAttributes(CBFilterNotifySetFileAttributesEvent e) {
        /* insert your code here */
    }

    @Override
    public void notifySetFileInfo(CBFilterNotifySetFileInfoEvent cbfilterNotifySetFileInfoEvent) {

    }

    @Override
    public void notifySetFileSecurity(CBFilterNotifySetFileSecurityEvent e) {
        /* insert your code here */
    }

    @Override
    public void notifyUnlockAll(CBFilterNotifyUnlockAllEvent e) {
        /* insert your code here */
    }

    @Override
    public void notifyUnlockAllByKey(CBFilterNotifyUnlockAllByKeyEvent e) {
        /* insert your code here */
    }

    @Override
    public void notifyUnlockSingle(CBFilterNotifyUnlockSingleEvent e) {
        /* insert your code here */
    }

    @Override
    public void notifyWriteFile(CBFilterNotifyWriteFileEvent e) {
        /* insert your code here */
    }

    @Override
    public void reparseFileName(CBFilterReparseFileNameEvent e) {
        /* insert your code here */
    }

    @Override
    public void reparseWithTag(CBFilterReparseWithTagEvent e) {
        /* insert your code here */
    }

    @Override
    public void workerThreadCreation(CBFilterWorkerThreadCreationEvent e) {

    }

    @Override
    public void workerThreadTermination(CBFilterWorkerThreadTerminationEvent e) {

    }

    static class RuleFlag {
        final String caption;
        final long value;

        public RuleFlag(String caption, long value) {
            this.caption = caption;
            this.value = value;
        }

        @Override
        public String toString() {
            return caption;
        }
    }

    private static final RuleFlag[] AccessFlags = {
            new RuleFlag("Access: DeleteProtect", Constants.ACCESS_DELETE_PROTECT),
            new RuleFlag("Access: DenyAll", Constants.ACCESS_DENY_ALL),
            new RuleFlag("Access: ReadOnly", Constants.ACCESS_READ_ONLY),
            new RuleFlag("Access: WriteOnly", Constants.ACCESS_WRITE_ONLY),
    };

    private static final RuleFlag[] ControlFlags = {
            new RuleFlag("Control: All", Constants.FS_CE_ALL),
            new RuleFlag("Control: BeforeCanDelete", Constants.FS_CE_BEFORE_CAN_DELETE),
            new RuleFlag("Control: BeforeCleanup", Constants.FS_CE_BEFORE_CLEANUP),
            new RuleFlag("Control: BeforeClose", Constants.FS_CE_BEFORE_CLOSE),
            new RuleFlag("Control: BeforeCreate", Constants.FS_CE_BEFORE_CREATE),
            new RuleFlag("Control: BeforeCreateHardLink", Constants.FS_CE_BEFORE_CREATE_HARD_LINK),
            new RuleFlag("Control: BeforeDelete", Constants.FS_CE_BEFORE_DELETE),
            new RuleFlag("Control: BeforeFsctl", Constants.FS_CE_BEFORE_FSCTL),
            new RuleFlag("Control: BeforeGetSecurity", Constants.FS_CE_BEFORE_GET_SECURITY),
            new RuleFlag("Control: BeforeIoctl", Constants.FS_CE_BEFORE_IOCTL),
            new RuleFlag("Control: BeforeLockControl", Constants.FS_CE_BEFORE_LOCK_CONTROL),
            new RuleFlag("Control: BeforeOpen", Constants.FS_CE_BEFORE_OPEN),
            new RuleFlag("Control: BeforeQueryFileInfo", Constants.FS_CE_BEFORE_QUERY_FILE_INFO),
            new RuleFlag("Control: BeforeRead", Constants.FS_CE_BEFORE_READ),
            new RuleFlag("Control: BeforeRename", Constants.FS_CE_BEFORE_RENAME),
            new RuleFlag("Control: BeforeSetAttributes", Constants.FS_CE_BEFORE_SET_ATTRIBUTES),
            new RuleFlag("Control: BeforeSetSecurity", Constants.FS_CE_BEFORE_SET_SECURITY),
            new RuleFlag("Control: BeforeSetSizes", Constants.FS_CE_BEFORE_SET_SIZES),
            new RuleFlag("Control: BeforeWrite", Constants.FS_CE_BEFORE_WRITE),
            new RuleFlag("Control: AfterCanDelete", Constants.FS_CE_AFTER_CAN_DELETE),
            new RuleFlag("Control: AfterCleanup", Constants.FS_CE_AFTER_CLEANUP),
            new RuleFlag("Control: AfterClose", Constants.FS_CE_AFTER_CLOSE),
            new RuleFlag("Control: AfterCreate", Constants.FS_CE_AFTER_CREATE),
            new RuleFlag("Control: AfterCreateHardLink", Constants.FS_CE_AFTER_CREATE_HARD_LINK),
            new RuleFlag("Control: AfterDelete", Constants.FS_CE_AFTER_DELETE),
            new RuleFlag("Control: AfterEnumerateDirectory", Constants.FS_CE_AFTER_ENUMERATE_DIRECTORY),
            new RuleFlag("Control: AfterFsctl", Constants.FS_CE_BEFORE_FSCTL),
            new RuleFlag("Control: AfterGetSecurity", Constants.FS_CE_AFTER_GET_SECURITY),
            new RuleFlag("Control: AfterGetSizes", Constants.FS_CE_AFTER_GET_SIZES),
            new RuleFlag("Control: AfterIoctl", Constants.FS_CE_AFTER_IOCTL),
            new RuleFlag("Control: AfterLockControl", Constants.FS_CE_AFTER_LOCK_CONTROL),
            new RuleFlag("Control: AfterOpen", Constants.FS_CE_AFTER_OPEN),
            new RuleFlag("Control: AfterQueryFileInfo", Constants.FS_CE_AFTER_QUERY_FILE_INFO),
            new RuleFlag("Control: AfterRead", Constants.FS_CE_AFTER_READ),
            new RuleFlag("Control: AfterRename", Constants.FS_CE_AFTER_RENAME),
            new RuleFlag("Control: AfterSetAttributes", Constants.FS_CE_AFTER_SET_ATTRIBUTES),
            new RuleFlag("Control: AfterSetSecurity", Constants.FS_CE_AFTER_SET_SECURITY),
            new RuleFlag("Control: AfterSetSizes", Constants.FS_CE_AFTER_SET_SIZES),
            new RuleFlag("Control: AfterWrite", Constants.FS_CE_AFTER_WRITE),
            new RuleFlag("Control: ReparseFilename", Constants.FS_CE_REPARSE_FILENAME),
            new RuleFlag("Control: ReparseTag", Constants.FS_CE_REPARSE_TAG)
    };

    private static final RuleFlag[] NotifyFlags = {
            new RuleFlag("Notify: All", Constants.FS_NE_ALL),
            new RuleFlag("Notify: CanDelete", Constants.FS_NE_CAN_DELETE),
            new RuleFlag("Notify: Cleanup", Constants.FS_NE_CLEANUP),
            new RuleFlag("Notify: Close", Constants.FS_NE_CLOSE),
            new RuleFlag("Notify: Create", Constants.FS_NE_CREATE),
            new RuleFlag("Notify: CreateHardLink", Constants.FS_NE_CREATE_HARD_LINK),
            new RuleFlag("Notify: Delete", Constants.FS_NE_DELETE),
            new RuleFlag("Notify: EnumerateDirectory", Constants.FS_NE_ENUMERATE_DIRECTORY),
            new RuleFlag("Notify: Fsctl", Constants.FS_NE_FSCTL),
            new RuleFlag("Notify: GetSecurity", Constants.FS_NE_GET_SECURITY),
            new RuleFlag("Notify: GetSizes", Constants.FS_NE_GET_SIZES),
            new RuleFlag("Notify: Ioctl", Constants.FS_NE_IOCTL),
            new RuleFlag("Notify: LockControl", Constants.FS_NE_LOCK_CONTROL),
            new RuleFlag("Notify: Open", Constants.FS_NE_OPEN),
            new RuleFlag("Notify: QueryFileInfo", Constants.FS_NE_QUERY_FILE_INFO),
            new RuleFlag("Notify: Read", Constants.FS_NE_READ),
            new RuleFlag("Notify: Rename", Constants.FS_NE_RENAME),
            new RuleFlag("Notify: SetAttributes", Constants.FS_NE_SET_ATTRIBUTES),
            new RuleFlag("Notify: SetSecurity", Constants.FS_NE_SET_SECURITY),
            new RuleFlag("Notify: SetSizes", Constants.FS_NE_SET_SIZES),
            new RuleFlag("Notify: Write", Constants.FS_NE_WRITE)
    };

    public static void main(String[] args) {
        JFrame frame = new JFrame(filter.appName);
        filter filterDemo = new filter();
        frame.setContentPane(filterDemo.panelRoot);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        filterDemo.initComponent(frame);
        frame.pack();
        frame.setVisible(true);
    }
}





