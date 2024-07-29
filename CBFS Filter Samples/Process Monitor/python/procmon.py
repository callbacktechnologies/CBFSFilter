# 
# CBFS Filter 2024 Python Edition - Sample Project
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
  elif args[index] is None:
    args[index] = input(prompt)


import enum

import psutil


class ProcMon:
    class DriverStatus(enum.Enum):
        NOT_INSTALLED = 0
        STOPPED = 1
        START_PENDING = 2
        STOP_PENDING = 3
        RUNNING = 4
        CONTINUE_PENDING = 5
        PAUSE_PENDING = 6
        PAUSED = 7

        @staticmethod
        def from_int(value):
            for status in ProcMon.DriverStatus:
                if status.value == value:
                    return status
            return None

        def __str__(self):
            if self == ProcMon.DriverStatus.NOT_INSTALLED:
                return "not installed"
            elif self == ProcMon.DriverStatus.STOPPED:
                return "stopped"
            elif self == ProcMon.DriverStatus.START_PENDING:
                return "start pending"
            elif self == ProcMon.DriverStatus.STOP_PENDING:
                return "stop pending"
            elif self == ProcMon.DriverStatus.RUNNING:
                return "running"
            elif self == ProcMon.DriverStatus.CONTINUE_PENDING:
                return "continue pending"
            elif self == ProcMon.DriverStatus.PAUSE_PENDING:
                return "pause pending"
            elif self == ProcMon.DriverStatus.PAUSED:
                return "paused"
            else:
                return super().__str__()

    PRODUCT_GUID = "{713CC6CE-B3E2-4fd9-838D-E28F558F6866}"
    GAP = 6
    DGAP = GAP * 2
    ERROR_ACCESS_DENIED = 5
    ERROR_PRIVILEGE_NOT_HELD = 1314
    PROCESS_TERMINATE = 0x0001
    PROCESS_SUSPEND_RESUME = 0x0800
    THREAD_TERMINATE = 0x0001
    CURRENT_PROCESS_ID = 0

    def __init__(self):
        self.driverRunning = False
        self.cabFileLocation = None
        self.processExecutionDenied = False
        self.filter = None
        self.process = None
        self.denyExecute = False
        self.denyTerminate = False
        self.noExecOpName = None
        self.noTermOpName = None
        self.noTermOpPID = 0
        self.started = 0

        self.filter = self.create_filter()
        self.check_driver(self.filter)
        self.dispose_filter(self.filter)

    def create_filter(self):
        proc_filter = CBProcess()
        try:
            self.add_event_listeners(proc_filter)
        except CBFSFilterError:
            self.dispose_filter(proc_filter)
            proc_filter = None
        return proc_filter

    def add_event_listeners(self, proc_filter):
        proc_filter.on_process_creation = self.processCreation
        proc_filter.on_process_handle_operation = self.processHandleOperation
        proc_filter.on_process_termination = self.processTermination
        proc_filter.on_thread_creation = self.threadCreation
        proc_filter.on_thread_handle_operation = self.threadHandleOperation
        proc_filter.on_worker_thread_termination = self.threadTermination
        proc_filter.on_error = self.error
        proc_filter.on_worker_thread_creation = self.workerThreadCreation
        proc_filter.on_worker_thread_termination = self.workerThreadTermination


    @staticmethod
    def dispose_filter(proc_filter):
        if proc_filter is None:
            return

        try:
            proc_filter = None
        except CBFSFilterError:
            pass

    def stop_filter(self):
        if self.filter is not None:
            try:
                self.filter.stop_filter()
            except CBFSFilterError:
                pass
            self.filter = None
            self.log("Filter stopped")
        self.process = None

    def check_driver(self, proc_filter):
        try:
            status = ProcMon.DriverStatus.from_int(proc_filter.get_driver_status(ProcMon.PRODUCT_GUID))
            self.driverRunning = (status == ProcMon.DriverStatus.RUNNING)

            if status is None:
                print("Error - failed to get driver status")
            elif status == ProcMon.DriverStatus.NOT_INSTALLED:
                print("Driver is not installed")
            else:
                version = proc_filter.get_driver_version(ProcMon.PRODUCT_GUID)
                text = "Driver (ver {}.{}.{}.{}) installed, service {}".format(
                    version >> 48, (version >> 32) & 0xffff, (version >> 16) & 0xffff, version & 0xffff, status)
                print(text)
        except CBFSFilterError as err:
            print("Error - " + err.message)
            self.driverRunning = False

    @staticmethod
    def is_null_or_empty(s):
        if s is None:
            return True
        if len(s) == 0:
            return True
        return False

    @staticmethod
    def banner():
        print("CBFS Filter Process Monitor Demo Copyright (c) 2017-2023, Callback Technologies, Inc.\n")

    @staticmethod
    def usage():
        print("Usage: procmon [-<switch 1> ... -<switch N>]\n")
        print("<Switches>")
        print("  -mon - monitor all processes in the system")
        print("  -drv <path to cbprocess.cab> - Install the driver from the specified CAB file")
        print("  -noexec <EXE name> - Prevent execution of the process from the given EXE name (may contain a path)")
        print(
            "  -noterm {<EXE name>|<PID>} - Prevent termination of the process with the given EXE name (may contain a path) or process ID")
        print(
            "IMPORTANT: termination protection doesn't prevent closing of process windows and graceful exit. It just removes the permission from the handle\n")

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
            elif ProcMon.is_drive_letter(path_str):
                try:
                    res = os.path.abspath(path_str)

                    if res.startswith("\\\\") and not os.path.exists(res):
                        print(f"The network folder '{res}' does not exist.")
                    elif not os.path.exists(res):
                        print(f"The path '{res}' does not exist.")
                except Exception as e:
                    print(f"Error while converting to absolute path: {e}")
                    res = None
        return res

    def install(self, file_name):
        reboot_needed = False

        proc_filter = self.create_filter()
        try:
            print("Installing the driver from '" + file_name + "'")
            reboot_needed = proc_filter.install(file_name, self.PRODUCT_GUID, None,
                                               cbfsfilter.INSTALL_REMOVE_OLD_VERSIONS, None)
            self.check_driver(proc_filter)
            print("Drivers installed successfully", end="")

            if reboot_needed:
                print(", reboot is required")
                return
            print()

        except CBFSFilterError as err:
            print("The driver could not be installed")

            if err.code == self.ERROR_ACCESS_DENIED or err.code == self.ERROR_PRIVILEGE_NOT_HELD:
                message = "Installation requires administrator rights. Run the app as administrator"
            else:
                message = err.message

            print("Error: " + message)
            return

        finally:
            if proc_filter is not None:
                self.dispose_filter(proc_filter)

    def current_time_mill(self):
        current_date = datetime.now().date()
        epoch = datetime(1970, 1, 1).date()
        milliseconds = (current_date - epoch).total_seconds() * 1000
        return milliseconds

    def set_filter(self):
        proc_filter = self.create_filter()
        try:
            proc_filter.initialize(self.PRODUCT_GUID)

            # Make the demo can receive notifications triggered by itself.
            proc_filter.config("FilterOwnRequests=true")
            proc_filter.config("EventsToFire=-1")
            self.started = self.current_time_mill()
            proc_filter.start_filter(30000)
            self.log("Filter started")

        except CBFSFilterError as err:
            print("Error: " + err.message)
            return

        try:
            proc_filter.add_filtered_process_by_id(-1, True)

        except CBFSFilterError as err:
            print("Error while trying to add the filtering rule: " + err.message)
            return

    @staticmethod
    def get_current_process_id():
        if ProcMon.CURRENT_PROCESS_ID == 0:
            ProcMon.CURRENT_PROCESS_ID = os.getpid()
        return ProcMon.CURRENT_PROCESS_ID

    def log(self, text):
        elapsed = int(self.current_time_mill()) - self.started
        print("<LogInfo> Message: " + text + ", Time Elapsed: " + str(elapsed))

    def prevent_terminate(self, pid):
        if self.denyTerminate:
            if self.noTermOpPID != 0 and pid == self.noTermOpPID:
                return True
            proc_name = self.get_process_name_by_pid(pid)
            if proc_name is None or len(proc_name) == 0:
                return False
            return proc_name == self.noTermOpName
        else:
            return False

    @staticmethod
    def get_process_name_by_pid(pid):
        try:
            process = psutil.Process(pid)
            return process.name()
        except psutil.NoSuchProcess:
            return None

    def processCreation(self, e):
        # Make sure the process is being started by this app
        if e.creating_process_id != self.get_current_process_id():
            self.log(f"Process {e.process_id} created by another app -> no action required")
            return

        self.log(f"Process {e.process_id} created from {e.image_file_name}")

        if not self.processExecutionDenied:
            self.log("Process creation allowed")
        else:
            e.result_code = self.ERROR_ACCESS_DENIED
            self.log("PROCESS CREATION BLOCKED")

        if self.denyExecute and (
                not self.is_null_or_empty(self.noExecOpName) and e.process_name.equalsIgnoreCase(self.noExecOpName)):
            e.result_code = self.ERROR_ACCESS_DENIED

        message = "Process creation denied." if e.result_code == self.ERROR_ACCESS_DENIED else "Process created."
        self.log(f"{message}. Id: {e.process_id}, Process Name: {e.process_name}")

    def processHandleOperation(self, e):
        if self.prevent_terminate(e.process_id):
            e.desired_access &= ~self.PROCESS_TERMINATE
            processName = self.get_process_name_by_pid(e.process_id)
            self.log(
                f"Process handle operation; removed PROCESS_TERMINATED flag. Id: {e.process_id}, Name: {processName}")

    def processTermination(self, e):
        self.log(f"Process {e.process_id} terminated")
        self.stop_filter()

    def threadCreation(self, e):
        self.log(f"Thread {e.thread_id} created")

    def threadHandleOperation(self, e):
        self.log(f"Thread {e.thread_id} is being opened with rights 0x{e.desired_access:08X}")

        if self.prevent_terminate(e.process_id):
            e.desired_access &= ~self.THREAD_TERMINATE
            processName = self.get_process_name_by_pid(e.process_id)
            self.log(
                f"Thread handle operation; removed PROCESS_TERMINATED flag. Id: {e.process_id}, Name: {processName}")

    def threadTermination(self, e):
        self.log(f"Thread {e.thread_id} terminated")

    def error(self, e):
        pass

    def workerThreadCreation(self, e):
        pass

    def workerThreadTermination(self, e):
        pass


def main(args):
    stop_opt = False

    ProcMon.banner()

    # if no parameters specified, show usage and quit
    try:
        proc_monitor = ProcMon()
        if len(args) < 1:
            ProcMon.usage()
            return

        argi = 0
        while argi < len(args):
            arg = args[argi]
            argLen = len(arg)
            if argLen > 0:
                if (arg[0] == '-') and not stop_opt:
                    if arg.lower() == "-drv":
                        if argi < len(args) - 1:
                            argi += 1
                            arg = ProcMon.convert_relative_path_to_absolute(args[argi])
                            if arg is None:
                                print("Error: Invalid Driver Path.")
                                return
                            proc_monitor.install(arg)
                    elif arg.lower() == "-noexec":
                        if argi < len(args) - 1:
                            argi += 1
                            arg = args[argi]
                            proc_monitor.noExecOpName = arg
                            proc_monitor.denyExecute = True
                        else:
                            print("Error: -noexec parameter requires a file name as the next argument")
                            return
                    elif arg.lower() == "-noterm":
                        if argi < len(args) - 1:
                            argi += 1
                            arg = args[argi]
                            proc_monitor.noTermOpName = arg
                            try:
                                proc_monitor.noTermOpPID = int(proc_monitor.noTermOpName)
                            except:
                                pass
                            proc_monitor.denyTerminate = True
                        else:
                            print(
                                "Error: -noterm parameter requires a file name or a process ID as the next argument")
                            return
                    elif arg.lower() == "-mon":
                        # nothing here, execution will pass through
                        pass
                    else:
                        print("Error: Invalid option " + arg)
                else:
                    # if we have not set the path yet, do this now. Otherwise, set the mask
                    break
            argi += 1

        print("Press Enter to start monitoring, then press any key to stop and quit.")
        input()

        proc_monitor.set_filter()
        print("Press Enter to stop monitoring and quit.")
        input()
        proc_monitor.stop_filter()

    except Exception as e:
        print("Error in Main: " + repr(e))


if __name__ == "__main__":
    main(sys.argv[1:])


