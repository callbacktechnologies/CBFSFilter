# 
# CBFS Filter 2022 Python Edition - Sample Project
# 
# This sample project demonstrates the usage of CBFS Filter in a 
# simple, straightforward way. It is not intended to be a complete 
# application. Error handling and other checks are simplified for clarity.
# 
# www.callback.com/cbfsfilter
# 
# This code is subject to the terms and conditions specified in the 
# corresponding product license agreement which outlines the authorized 
# usage and restrictions.
# 

import sys
import string
from cbfsfilter import *

input = sys.hexversion<0x03000000 and raw_input or input

def ensureArg(args, prompt, index):
  if len(args) <= index:
    while len(args) <= index:
      args.append(None)
    args[index] = input(prompt)
  elif args[index] == None:
    args[index] = input(prompt)

import os
import re
from cbfsfilter import *


class Filemon:
    def __init__(self):
        self.mWatcher = CBMonitor()
        self.guid = "{713CC6CE-B3E2-4fd9-838D-E28F558F6866}"
        self.ALTITUDE_FAKE_VALUE_FOR_DEBUG = "360000"
        self.ERROR_ACCESS_DENIED = 5
        self.ERROR_PRIVILEGE_NOT_HELD = 1314
        try:
            self.check_driver()
        except Exception as e:
            print(e)

    @staticmethod
    def banner():
        print("CBFS Filter FileMon Demo Copyright (c) 2017-2023, Callback Technologies, Inc.\n\n")

    @staticmethod
    def usage():
        print("Usage: filemon [-<switch 1> ... -<switch N>] [<path to monitor> [<filename mask to monitor>]]\n")
        print("<Switches>")
        print("  -drv <path to cbfilter.cab> - Install the driver from the specified CAB file")
        print("  -- Stop switches scanning\n")

    @staticmethod
    def is_drive_letter(path_str):
        if not path_str:
            return False

        c = path_str[0]
        if c.isalpha() and len(path_str) == 2 and path_str[1] == ':':
            return True
        else:
            return False

    @staticmethod
    def convert_relative_path_to_absolute(path_str):
        res = None
        if path_str and path_str.strip():
            res = path_str
            # Linux-specific case of using a home directory
            if path_str == "~" or path_str.startswith("~/"):
                home_dir = os.path.expanduser("~")
                if path_str == "~":
                    return home_dir
                else:
                    return os.path.join(home_dir, path_str[1:])
            elif not Filemon.is_drive_letter(path_str):
                res = os.path.abspath(path_str)

                if res.startswith("\\\\") and not os.path.exists(res):
                    print(f"The network folder '{res}' does not exist.")
                elif not os.path.exists(res):
                    print(f"The path '{res}' does not exist.")

        return res

    def check_driver(self, write_info=True):
        try:
            moduleStatus = self.mWatcher.get_driver_status(self.guid)
            moduleVersion = self.mWatcher.get_driver_version(self.guid)
            versionHigh = (moduleVersion >> 32) & 0xFFFFFFFF
            versionLow = moduleVersion & 0xFFFFFFFF

            if moduleStatus != 0:
                driverInfo = "Driver version: {}.{}.{}.{}".format(
                    versionHigh >> 16, versionHigh & 0xFFFF, versionLow >> 16, versionLow & 0xFFFF)
            else:
                driverInfo = "Driver: not installed"

            if write_info:
                print(driverInfo)

            return moduleStatus != 0
        except Exception as ex:
            print("Error in check_driver:", ex)
            return False

    def add_to_log(self, operation, filename, status):
        try:
            processName = self.mWatcher.get_originator_process_name()
            if len(processName) == 0:
                processName = "System"
        except CBFSFilterError:
            processName = "< ERROR >"

        try:
            processID = self.mWatcher.get_originator_process_id()
        except CBFSFilterError:
            processID = 0

        print(
            "<LogInfo> Operation: {}, Path: {}, Originator Process: {}, Process ID: {}, User Name: {}, Result: {}".format(
                operation, filename, processName, processID, "", status))

    def install(self, file_name):
        if not os.path.exists(file_name):
            return

        try:
            print("Installing the driver from", file_name)
            reboot = self.mWatcher.install(file_name, self.guid, "", self.ALTITUDE_FAKE_VALUE_FOR_DEBUG, 0)

            self.check_driver()

            if reboot:
                print("Please, reboot the system for the changes to take effect")
            else:
                print("Driver installed successfully")

        except CBFSFilterError as e:
            if e.code == self.ERROR_PRIVILEGE_NOT_HELD or e.code == self.ERROR_ACCESS_DENIED:
                print("Installation requires administrator rights. Run the app as administrator")
            else:
                print(e.message)

    def add_event_listeners(self, fileFilter):
        fileFilter.on_after_filter_attach_to_volume = self.afterFilterAttachToVolume
        fileFilter.on_after_filter_detach_from_volume = self.afterFilterDetachFromVolume
        fileFilter.on_before_filter_attach_to_volume = self.beforeFilterAttachToVolume
        fileFilter.on_error = self.error
        fileFilter.on_filter_start = self.filterStart
        fileFilter.on_filter_stop = self.filterStop
        fileFilter.on_notify_can_file_be_deleted = self.notifyCanFileBeDeleted
        fileFilter.on_notify_cleanup_file = self.notifyCleanupFile
        fileFilter.on_notify_close_file = self.notifyCloseFile
        fileFilter.on_notify_create_file = self.notifyCreateFile
        fileFilter.on_notify_create_hard_link = self.notifyCreateHardLink
        fileFilter.on_notify_delete_file = self.notifyDeleteFile
        fileFilter.on_notify_delete_reparse_point = self.notifyDeleteReparsePoint
        fileFilter.on_notify_enumerate_directory = self.notifyEnumerateDirectory
        fileFilter.on_notify_filter_attach_to_volume = self.notifyFilterAttachToVolume
        fileFilter.on_notify_filter_detach_from_volume = self.notifyFilterDetachFromVolume
        fileFilter.on_notify_fsctl = self.notifyFsctl
        fileFilter.on_notify_get_file_security = self.notifyGetFileSecurity
        fileFilter.on_notify_get_file_sizes = self.notifyGetFileSizes
        fileFilter.on_notify_get_reparse_point = self.notifyGetReparsePoint
        fileFilter.on_notify_ioctl = self.notifyIoctl
        fileFilter.on_notify_lock = self.notifyLock
        fileFilter.on_notify_open_file = self.notifyOpenFile
        fileFilter.on_notify_query_ea = self.notifyQueryEa
        fileFilter.on_notify_query_file_info = self.notifyQueryFileInfo
        fileFilter.on_notify_read_file = self.notifyReadFile
        fileFilter.on_notify_rename_or_move_file = self.notifyRenameOrMoveFile
        fileFilter.on_notify_set_allocation_size = self.notifySetAllocationSize
        fileFilter.on_notify_set_ea = self.notifySetEa
        fileFilter.on_notify_set_file_size = self.notifySetFileSize
        fileFilter.on_notify_set_reparse_point = self.notifySetReparsePoint
        fileFilter.on_notify_set_file_attributes = self.notifySetFileAttributes
        fileFilter.on_notify_set_file_info = self.notifySetFileInfo
        fileFilter.on_notify_set_file_security = self.notifySetFileSecurity
        fileFilter.on_notify_unlock_all = self.notifyUnlockAll
        fileFilter.on_notify_unlock_all_by_key = self.notifyUnlockAllByKey
        fileFilter.on_notify_unlock_single = self.notifyUnlockSingle
        fileFilter.on_notify_write_file = self.notifyWriteFile
        fileFilter.on_worker_thread_creation = self.workerThreadCreation
        fileFilter.on_worker_thread_termination = self.workerThreadTermination
        fileFilter.set_fire_volume_events = cbfsfilter.FS_MOUNT_BOTH

    def uninstall(self, file_name):
        if not os.path.exists(file_name):
            return

        try:
            print("Uninstalling the driver from " + file_name)
            # Assuming mWatcher is an instance variable
            reboot = self.mWatcher.uninstall(file_name, self.guid, "", 0)

            self.check_driver()

            if reboot:
                print("Please, reboot the system for the changes to take effect")
            else:
                print("Driver uninstalled successfully")

        except CBFSFilterError as e:
            if e.code() == self.ERROR_PRIVILEGE_NOT_HELD or e.code() == self.ERROR_ACCESS_DENIED:
                print("Uninstallation requires administrator rights. Run the app as administrator")
            else:
                print(e.message)

    def setFilter(self, mask):
        try:
            self.mWatcher.initialize(self.guid)

            filterRules = (
                    cbfsfilter.FS_NE_READ |
                    cbfsfilter.FS_NE_WRITE |
                    cbfsfilter.FS_NE_CREATE |
                    cbfsfilter.FS_NE_RENAME |
                    cbfsfilter.FS_NE_CREATE_HARD_LINK |
                    cbfsfilter.FS_NE_SET_SIZES |
                    cbfsfilter.FS_NE_DELETE |
                    cbfsfilter.FS_NE_SET_ATTRIBUTES |
                    cbfsfilter.FS_NE_ENUMERATE_DIRECTORY |
                    cbfsfilter.FS_NE_OPEN |
                    cbfsfilter.FS_NE_CLOSE |
                    cbfsfilter.FS_NE_SET_SECURITY
            )

            self.mWatcher.add_filter_rule(mask, filterRules)
            self.mWatcher.set_process_failed_requests(True)

            self.mWatcher.start_filter()

        except CBFSFilterError as e:
            print("Error in setFilter: " + e.message)

        self.check_driver(False)

    def deleteFilter(self):
        try:
            self.mWatcher.delete_all_filter_rules()
            self.mWatcher.delete_all_passthrough_rules()
            if self.mWatcher.active:
                self.mWatcher.stop_filter(False)

        except CBFSFilterError as e:
            print("Error in deleteFilter: " + e.message)

    def afterFilterAttachToVolume(self, cbfilterAfterFilterAttachToVolumeEvent):
        pass

    def afterFilterDetachFromVolume(self, cbfilterAfterFilterDetachFromVolumeEvent):
        pass

    def beforeFilterAttachToVolume(self, cbfilterBeforeFilterAttachToVolumeEvent):
        pass

    def error(self, cbfilterErrorEvent):
        pass

    def filterStart(self, cbfilterFilterStartEvent):
        pass

    def filterStop(self, cbfilterFilterStopEvent):
        self.check_driver()

    def notifyCanFileBeDeleted(self, cbfilterNotifyCanFileBeDeletedEvent):
        pass

    def notifyCleanupFile(self, cbfilterNotifyCleanupFileEvent):
        pass

    def notifyCloseFile(self, cbfilterNotifyCloseFileEvent):
        self.add_to_log("notifyCloseFile", cbfilterNotifyCloseFileEvent.file_name, 0)

    def notifyCreateFile(self, cbfilterNotifyCreateFileEvent):
        self.add_to_log("notifyCreateFile", cbfilterNotifyCreateFileEvent.file_name, cbfilterNotifyCreateFileEvent.status)

    def notifyCreateHardLink(self, cbfilterNotifyCreateHardLinkEvent):
        self.add_to_log("notifyCreateHardLink", cbfilterNotifyCreateHardLinkEvent.file_name, cbfilterNotifyCreateHardLinkEvent.status)

    def notifyDeleteFile(self, cbfilterNotifyDeleteFileEvent):
        self.add_to_log("notifyDeleteFile", cbfilterNotifyDeleteFileEvent.file_name, 0)

    def notifyDeleteReparsePoint(self, cbmonitorNotifyDeleteReparsePointEvent):
        pass

    def notifyEnumerateDirectory(self, cbfilterNotifyEnumerateDirectoryEvent):
        self.add_to_log("notifyEnumerateDirectory", os.path.join(cbfilterNotifyEnumerateDirectoryEvent.directory_name, cbfilterNotifyEnumerateDirectoryEvent.file_name), cbfilterNotifyEnumerateDirectoryEvent.status)

    def notifyFilterAttachToVolume(self, cbfilterNotifyFilterAttachToVolumeEvent):
        pass

    def notifyFilterDetachFromVolume(self, cbfilterNotifyFilterDetachFromVolumeEvent):
        pass

    def notifyFsctl(self, cbfilterNotifyFsctlEvent):
        pass

    def notifyGetFileSecurity(self, cbfilterNotifyGetFileSecurityEvent):
        pass

    def notifyGetFileSizes(self, cbfilterNotifyGetFileSizesEvent):
        pass

    def notifyGetReparsePoint(self, cbmonitorNotifyGetReparsePointEvent):
        pass

    def notifyIoctl(self, cbfilterNotifyIoctlEvent):
        pass

    def notifyLock(self, cbfilterNotifyLockEvent):
        pass

    def notifyOpenFile(self, cbfilterNotifyOpenFileEvent):
        self.add_to_log("notifyOpenFile", cbfilterNotifyOpenFileEvent.file_name, cbfilterNotifyOpenFileEvent.status)

    def notifyQueryEa(self, cbmonitorNotifyQueryEaEvent):
        pass

    def notifyQueryFileInfo(self, cbfilterNotifyQueryFileInfoEvent):
        pass

    def notifyReadFile(self, cbfilterNotifyReadFileEvent):
        self.add_to_log("notifyReadFile", cbfilterNotifyReadFileEvent.file_name, cbfilterNotifyReadFileEvent.status)

    def notifyRenameOrMoveFile(self, cbfilterNotifyRenameOrMoveFileEvent):
        self.add_to_log("notifyRenameOrMoveFile", cbfilterNotifyRenameOrMoveFileEvent.file_name, cbfilterNotifyRenameOrMoveFileEvent.status)

    def notifySetAllocationSize(self, cbfilterNotifySetAllocationSizeEvent):
        pass

    def notifySetEa(self, cbmonitorNotifySetEaEvent):
        pass

    def notifySetFileSize(self, cbfilterNotifySetFileSizeEvent):
        pass

    def notifySetReparsePoint(self, cbmonitorNotifySetReparsePointEvent):
        pass

    def notifySetFileAttributes(self, cbfilterNotifySetFileAttributesEvent):
        self.add_to_log("notifySetFileAttributes", cbfilterNotifySetFileAttributesEvent.file_name, cbfilterNotifySetFileAttributesEvent.status)

    def notifySetFileInfo(self, cbmonitorNotifySetFileInfoEvent):
        pass

    def notifySetFileSecurity(self, cbfilterNotifySetFileSecurityEvent):
        self.add_to_log("notifySetFileSecurity", cbfilterNotifySetFileSecurityEvent.file_name, cbfilterNotifySetFileSecurityEvent.status)

    def notifyUnlockAll(self, cbfilterNotifyUnlockAllEvent):
        pass

    def notifyUnlockAllByKey(self, cbfilterNotifyUnlockAllByKeyEvent):
        pass

    def notifyUnlockSingle(self, cbfilterNotifyUnlockSingleEvent):
        pass

    def notifyWriteFile(self, cbfilterNotifyWriteFileEvent):
        self.add_to_log("notifyWriteFile", cbfilterNotifyWriteFileEvent.file_name, cbfilterNotifyWriteFileEvent.status)

    def workerThreadCreation(self, cbfilterWorkerThreadCreationEvent):
        pass

    def workerThreadTermination(self, cbfilterWorkerThreadTerminationEvent):
        pass


def main(args):
    pathToMonitor = None
    filenameMask = None

    argi = 0
    argLen = 0
    stopOpt = False

    Filemon.banner()

    try:
        fileMonitor = Filemon()

        if len(args) < 1:
            Filemon.usage()
            return

        while argi < len(args):
            arg = args[argi]
            argLen = len(arg)
            if argLen > 0:
                if arg[0] == '-' and not stopOpt:
                    if arg.lower() == "-drv":
                        arg = Filemon.convert_relative_path_to_absolute(args[argi + 1])
                        argi += 1
                        if argi < len(args):
                            fileMonitor.install(arg)
                    else:
                        print("Error: Invalid option " + arg)
                else:
                    if pathToMonitor is None:
                        pathToMonitor = Filemon.convert_relative_path_to_absolute(arg)
                        if not os.path.exists(pathToMonitor):
                            print(
                                "ERROR: the specified path '" + pathToMonitor + "' does not point to an existing "
                                                                                "directory")
                            return
                    else:
                        filenameMask = arg
                        break

            argi += 1

        if not fileMonitor.check_driver():
            return

        if pathToMonitor is None:
            return

        print("Press any key to start monitoring, then press any key to stop and quit.")
        sys.stdin.read(1)
        fileMonitor.add_event_listeners(fileMonitor.mWatcher)
        try:
            mask = os.path.join(pathToMonitor, filenameMask)
            fileMonitor.setFilter(mask)
            print("Press any key to stop monitoring and quit.")
            sys.stdin.read(1)
            fileMonitor.deleteFilter()
        except Exception as ex:
            print("Error: " + str(ex))
    except Exception as e:
        print("Error in Main: " + str(e))


if __name__ == "__main__":
    main(sys.argv[1:])


