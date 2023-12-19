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

import ctypes
import subprocess
import sys
import time
import os
import re
from cbfsfilter import *


class Globals:
    counter = 0
    storage = {}

    @staticmethod
    def acquire(identifier):
        item = Globals.storage.get(identifier)
        if item is None:
            return None
        item.refs += 1
        return item.target

    @staticmethod
    def alloc(target):
        Globals.counter += 1
        identifier = Globals.counter
        item = Globals.Item(target)
        Globals.storage[identifier] = item
        return identifier

    @staticmethod
    def clear():
        for identifier in Globals.storage.keys():
            item = Globals.storage.get(identifier)
            item.clear()
        Globals.storage.clear()

    @staticmethod
    def free(identifier):
        item = Globals.storage.pop(identifier, None)
        if item is None:
            return None
        target = item.target
        item.clear()
        return target

    @staticmethod
    def get(identifier):
        item = Globals.storage.get(identifier)
        return None if item is None else item.target

    @staticmethod
    def release(identifier):
        item = Globals.storage.get(identifier)
        if item is None:
            return None
        if item.refs > 1:
            item.refs -= 1
            return None
        Globals.storage.pop(identifier)
        target = item.target
        item.clear()
        return target

    @staticmethod
    def set(identifier, target):
        item = Globals.storage.get(identifier)
        if item is None:
            return None
        if item.target == target:
            return None
        old = item.target
        item.target = target
        return old

    class Item:
        def __init__(self, target):
            self.refs = 1
            self.target = target

        def clear(self):
            self.refs = 0
            self.target = None


class RegMon:
    class DriverStatus:
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
            return getattr(RegMon.DriverStatus, list(RegMon.DriverStatus.__dict__.keys())[value])

        def __str__(self):
            if self == RegMon.DriverStatus.NOT_INSTALLED:
                return "not installed"
            elif self == RegMon.DriverStatus.STOPPED:
                return "stopped"
            elif self == RegMon.DriverStatus.START_PENDING:
                return "start pending"
            elif self == RegMon.DriverStatus.STOP_PENDING:
                return "stop pending"
            elif self == RegMon.DriverStatus.RUNNING:
                return "running"
            elif self == RegMon.DriverStatus.CONTINUE_PENDING:
                return "continue pending"
            elif self == RegMon.DriverStatus.PAUSE_PENDING:
                return "pause pending"
            elif self == RegMon.DriverStatus.PAUSED:
                return "paused"
            else:
                return str(self)

    PRODUCT_GUID = "{713CC6CE-B3E2-4fd9-838D-E28F558F6866}"
    GAP = 6
    DGAP = GAP * 2
    HEX_CHARS = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F']

    ERROR_SUCCESS = 0
    ERROR_FILE_NOT_FOUND = 2
    ERROR_ACCESS_DENIED = 5
    ERROR_INSUFFICIENT_BUFFER = 122
    ERROR_MORE_DATA = 234
    ERROR_NO_MORE_ITEMS = 259
    ERROR_REPARSE = 741
    ERROR_PRIVILEGE_NOT_HELD = 1314

    KEY_HKLM = "\\registry\\machine\\"
    KEY_HKU = "\\registry\\user\\"
    KEY_HKCU = "\\registry\\user\\%s\\"
    KEY_HKCR = "\\registry\\user\\%s_classes\\"

    def __init__(self):
        self.driver_running = False
        self.filter_working = False
        self.cab_file_location = None
        self.filter = None
        self.events_count = 0
        self.ticks_started = 0
        self.driverRunning = False
        self.initialize()

    def initialize(self):
        self.events_count = 0

        regFilter = self.create_filter()
        self.check_driver(regFilter)
        self.dispose_filter(regFilter)

    def create_filter(self):
        regFilter = CBRegistry()
        try:
            self.add_event_listeners(regFilter)
            return regFilter
        except CBFSFilterError:
            self.dispose_filter(regFilter)
            return None

    def add_event_listeners(self, regFilter):
        regFilter.on_after_close_key = self.after_close_key
        regFilter.on_after_create_key = self.after_create_key
        regFilter.on_after_delete_key = self.after_delete_key
        regFilter.on_after_delete_value = self.after_delete_value
        regFilter.on_after_enumerate_key = self.after_enumerate_key
        regFilter.on_after_enumerate_value = self.after_enumerate_value
        regFilter.on_after_get_key_security = self.after_get_key_security
        regFilter.on_after_open_key = self.after_open_key
        regFilter.on_after_query_key = self.after_query_key
        regFilter.on_after_query_value = self.after_query_value
        regFilter.on_after_rename_key = self.after_rename_key
        regFilter.on_after_set_key = self.after_set_key
        regFilter.on_after_set_value = self.after_set_value
        regFilter.on_before_create_key = self.before_create_key
        regFilter.on_before_delete_value = self.before_delete_value
        regFilter.on_before_open_key = self.before_open_key
        regFilter.on_before_rename_key = self.before_rename_key
        regFilter.on_before_set_key = self.before_set_key
        regFilter.on_before_set_key_security = self.before_set_key_security
        regFilter.on_before_set_value = self.before_set_value
        regFilter.on_cleanup_key_context = self.cleanup_key_context
        regFilter.on_close_key_handle = self.close_key_handle
        regFilter.on_error = self.error
        regFilter.on_worker_thread_creation = self.worker_thread_creation
        regFilter.on_worker_thread_termination = self.worker_thread_termination

    @staticmethod
    def dispose_filter(regFilter):
        if regFilter is not None:
            try:
                regFilter = None
            except CBFSFilterError:
                pass

    def check_driver(self, regFilter):
        try:
            status = regFilter.get_driver_status(self.PRODUCT_GUID)

            self.driverRunning = (status == "RUNNING")

            if status is None:
                print("Error - failed to get driver status")
            elif status == "NOT_INSTALLED":
                print("Driver is not installed")
            else:
                version = regFilter.get_driver_version(self.PRODUCT_GUID)
                text = "Driver (ver {}.{}.{}.{}), installed, service {}".format(
                    version >> 48, (version >> 32) & 0xffff, (version >> 16) & 0xffff, version & 0xffff, status
                )
                print(text)
        except CBFSFilterError as err:
            print("Error - " + err.message)
            self.driverRunning = False

    @staticmethod
    def getCurrentUserSid():
        command = 'wmic useraccount where name="{}" get sid'.format(subprocess.check_output("echo %USERNAME%").strip())
        try:
            process = subprocess.Popen(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
            output, _ = process.communicate()
            for line in output.splitlines():
                line = line.strip()
                if line.startswith(b"S-"):
                    return line
        except Exception as ignored:
            pass
        return None

    @staticmethod
    def is_null_or_empty(s):
        return s is None or len(s) == 0

    @staticmethod
    def banner():
        print("CBFS Filter Registry Monitor Demo (c) 2017-2023, Callback Technologies, Inc.\n")

    @staticmethod
    def usage():
        print("Usage: regmon [-<switch 1> ... -<switch N>] [<process name whose requests should be filtered>]\n")
        print("<Switches>")
        print("  -drv <path to cbregistry.cab> - Install the driver from the specified CAB file\n")
        print('Example: regmon notepad.exe')

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
            elif not RegMon.is_drive_letter(path_str):
                res = os.path.abspath(path_str)

                if res.startswith("\\\\") and not os.path.exists(res):
                    print(f"The network folder '{res}' does not exist.")
                elif not os.path.exists(res):
                    print(f"The path '{res}' does not exist.")

        return res

    def install(self, fileName):
        filter = self.create_filter()
        try:
            print("Installing the driver from '{}'".format(fileName))
            rebootNeeded = filter.install(fileName, self.PRODUCT_GUID, None, cbfsfilter.INSTALL_REMOVE_OLD_VERSIONS)
            self.check_driver(filter)
            print("Drivers installed successfully", end="")
            if rebootNeeded:
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
        finally:
            if filter is not None:
                self.dispose_filter(filter)

    def current_time_mill(self):
        current_date = datetime.now().date()
        epoch = datetime(1970, 1, 1).date()
        milliseconds = (current_date - epoch).total_seconds() * 1000
        return milliseconds

    def set_filter(self, process_name):
        if self.is_null_or_empty(process_name):
            print("No process specified for filtering")
            return

        self.events_count = 0
        self.ticks_started = self.current_time_mill()

        self.filter = self.create_filter()
        try:
            self.filter.initialize(self.PRODUCT_GUID)

            self.filter.set_serialize_events(1)
            self.filter.start_filter(30000)
            self.filter.add_filtered_process_by_name(process_name, False)
        except CBFSFilterError as err:
            self.dispose_filter(self.filter)
            self.filter = None
            print("<Error> Filter not started or filtered process not added. Error message: " + err.message)
            return

        self.filter_working = True

    def stop_filter(self):
        if self.filter is None:
            return
        try:
            self.filter.stop_filter()
            self.filter = None
        except CBFSFilterError as inner:
            pass
        self.filter_working = False

    def ntKeyNameToWinApiKeyname(self, name):
        if name is None or len(name) == 0:
            return ""

        loweredName = name.lower()

        if RegMon.KEY_HKLM.startswith(loweredName):
            return "HKLM"

        if RegMon.KEY_HKCU.startswith(loweredName):
            return "HKCU"

        if RegMon.KEY_HKCR.startswith(loweredName):
            return "HKCR"

        if RegMon.KEY_HKU.startswith(loweredName):
            return "HKU"

        if loweredName.startswith(RegMon.KEY_HKLM):
            return "HKLM\\" + name[len(RegMon.KEY_HKLM):]

        if loweredName.startswith(RegMon.KEY_HKCU):
            return "HKCU\\" + name[len(RegMon.KEY_HKCU):]

        if loweredName.startswith(RegMon.KEY_HKCR):
            return "HKCR\\" + name[len(RegMon.KEY_HKCR):]

        if loweredName.startswith(RegMon.KEY_HKU):
            return "HKU\\" + name[len(RegMon.KEY_HKU):]

        return name

    def resultToStr(self, result):
        if result == RegMon.ERROR_SUCCESS:
            return "SUCCESS"

        if result == RegMon.ERROR_FILE_NOT_FOUND:
            return "NOT FOUND"

        if result == RegMon.ERROR_INSUFFICIENT_BUFFER:
            return "SMALL BUFFER"

        if result == RegMon.ERROR_MORE_DATA:
            return "MORE DATA"

        if result == RegMon.ERROR_REPARSE:
            return "REPARSE"

        return str(result)

    def log(self, operation, name, result, details):
        self.events_count += 1
        number = self.events_count
        elapsed = int(self.current_time_mill()) - self.ticks_started
        print("<LogInfo> Number: {}, Time Elapsed: {}, Operation: {}, Name: {}, Result: {}, Details: {}".format(
            number, elapsed, operation, self.ntKeyNameToWinApiKeyname(name), self.resultToStr(result), details))

    def is_bit_set(self, value, bit):
        return (value & bit) != 0

    def value_type_to_str(self, valueType):
        if valueType == cbfsfilter.REG_VALUETYPE_SZ:
            return "SZ"

        if valueType == cbfsfilter.REG_VALUETYPE_EXPAND_SZ:
            return "EXPAND_SZ"

        if valueType == cbfsfilter.REG_VALUETYPE_BINARY:
            return "BINARY"

        if valueType == cbfsfilter.REG_VALUETYPE_DWORD:
            return "DWORD"

        if valueType == cbfsfilter.REG_VALUETYPE_MULTI_SZ:
            return "MULTI_SZ"

        if valueType == cbfsfilter.REG_VALUETYPE_QWORD:
            return "QWORD"

        return f"unknown ({valueType})"

    def replace_char(self, s, original, replaceWith):
        if s is None or len(s) == 0:
            return s

        chars = list(s)
        for i in range(len(chars)):
            if chars[i] == original:
                chars[i] = replaceWith

        return "".join(chars)

    def dump_binary_value(self, data, size, max_size):
        if data is None or size == 0 or max_size == 0:
            return ""

        count = min(size, max_size)
        result = []
        if count >= size:
            result = [' '] * (count * 3 - 1)
        else:
            # append '...' to the end
            result = [' '] * (count * 3 + 2)
            k = len(result) - 1
            result[k] = '.'
            result[k - 1] = '.'
            result[k - 2] = '.'

        j = 0
        for i in range(count):
            b = ctypes.cast(data, ctypes.POINTER(ctypes.c_ubyte * count)).contents[i] & 0xff
            result[j] = RegMon.HEX_CHARS[b >> 4]
            result[j + 1] = RegMon.HEX_CHARS[b & 0x0f]
            if j + 2 < len(result) and result[j + 2] != '.':
                result[j + 2] = ' '
            j += 3

        return ''.join(result)

    class KeyContext:
        def __init__(self, key_name, access):
            self.key_name = key_name
            self.access = access
            self.new_name = None
            self.last_write_time = None
            self.value_name = None
            self.value_type = 0
            self.integer_value = 0
            self.string_value = None
            self.binary_value = None
            self.binary_value_size = 0

        def clear_value(self):
            self.value_name = None
            self.value_type = 0
            self.integer_value = 0
            self.string_value = None
            self.binary_value = None
            self.binary_value_size = 0

        def set_binary_value(self, data, size):
            if data is None or size == 0:
                self.binary_value = None
                return

            buffer = (ctypes.c_ubyte * size)()
            ctypes.memmove(buffer, data, size)
            self.binary_value = bytearray(size)

    def get_context(self, identifier):
        if identifier == 0:
            return None
        context = Globals.get(identifier)
        if not isinstance(context, self.KeyContext):
            return None
        return context

    def after_close_key(self, e):
        context = self.get_context(e.key_context)
        if context is None:
            return
        self.log("CloseKey", context.key_name, RegMon.ERROR_SUCCESS, None)

    def after_create_key(self, e):
        context = self.get_context(e.key_context)
        if context is None:
            return
        details = "Access: 0x%X" % (context.access if e.granted_access == 0 else e.granted_access)
        self.log("CreateKey", context.key_name, e.status, details)

    def after_delete_key(self, e):
        context = self.get_context(e.key_context)
        if context is None:
            return
        self.log("DeleteKey", context.key_name, e.status, None)

    def after_delete_value(self, e):
        context = self.get_context(e.key_context)
        if context is None:
            return
        self.log("DeleteValue", context.key_name + "\\" + context.value_name, e.status, None)
        context.value_name = None

    def after_enumerate_key(self, e):
        if e.status == RegMon.ERROR_NO_MORE_ITEMS:
            return

        context = self.get_context(e.key_context)
        if context is None:
            return

        sb = []
        if e.status == RegMon.ERROR_SUCCESS:
            sb.append(str(e.index) + ": ")
            if self.is_bit_set(e.valid_fields, cbfsfilter.REG_KEYFIELD_NAME):
                sb.append(e.name)
            if self.is_bit_set(e.valid_fields, cbfsfilter.REG_KEYFIELD_SUBKEYS):
                sb.append(", Subkeys: " + str(e.sub_keys))
            if self.is_bit_set(e.valid_fields, cbfsfilter.REG_KEYFIELD_VALUES):
                sb.append(", Values: " + str(e.values))
        self.log("EnumKey", context.key_name, e.status, ''.join(sb))

    def after_enumerate_value(self, e):
        if e.status == RegMon.ERROR_NO_MORE_ITEMS:
            return

        context = self.get_context(e.key_context)
        if context is None:
            return

        sb = [str(e.index) + ": "]
        if self.is_bit_set(e.valid_fields, cbfsfilter.REG_VALUEFIELD_NAME):
            if self.is_null_or_empty(e.value_name):
                sb.append("(Default)")
            else:
                sb.append(e.value_name)
        if self.is_bit_set(e.valid_fields, cbfsfilter.REG_VALUEFIELD_TYPE):
            sb.append("; Type: ")
            sb.append(self.value_type_to_str(e.value_type))
            if self.is_bit_set(e.valid_fields, cbfsfilter.REG_VALUEFIELD_DATA):
                sb.append("; Data: ")
                if e.value_type == cbfsfilter.REG_VALUETYPE_SZ or e.value_type == cbfsfilter.REG_VALUETYPE_EXPAND_SZ:
                    if not self.is_null_or_empty(e.string_value):
                        sb.append(e.string_value)
                elif e.value_type == cbfsfilter.REG_VALUETYPE_BINARY:
                    sb.append(
                        "[%d] %s" % (e.binary_value_size, self.dump_binary_value(e.binary_value, e.binary_value_size, 20)))
                elif e.value_type == cbfsfilter.REG_VALUETYPE_DWORD or e.value_type == cbfsfilter.REG_VALUETYPE_QWORD:
                    sb.append(str(e.integer_value))
                elif e.value_type == cbfsfilter.REG_VALUETYPE_MULTI_SZ:
                    if not self.is_null_or_empty(e.string_value):
                        sb.append(self.replace_char(e.string_value, '\u0017', ' '))
        self.log("EnumValue", context.key_name, e.status, ''.join(sb))

    def after_get_key_security(self, e):
        pass

    def after_open_key(self, e):
        context = self.get_context(e.key_context)
        if context is None:
            return
        details = "Access: 0x%X" % (context.access if e.granted_access == 0 else e.granted_access)
        self.log("OpenKey", context.key_name, e.status, details)

    def after_query_key(self, e):
        context = self.get_context(e.key_context)
        if context is None:
            return

        sb = []
        if e.status == RegMon.ERROR_SUCCESS:
            if self.is_bit_set(e.valid_fields, cbfsfilter.REG_KEYFIELD_SUBKEYS):
                sb.append("Subkeys: %d" % e.sub_keys)
            if self.is_bit_set(e.valid_fields, cbfsfilter.REG_KEYFIELD_VALUES):
                if len(sb) != 0:
                    sb.append(", ")
                sb.append("Values: %d" % e.values)
        self.log("QueryKey", context.key_name, e.status, ''.join(sb))

    def after_query_value(self, e):
        context = self.get_context(e.key_context)
        if context is None:
            return

        sb = []
        if self.is_bit_set(e.valid_fields, cbfsfilter.REG_VALUEFIELD_TYPE):
            sb.append("Type: ")
            sb.append(self.value_type_to_str(e.value_type))
            if self.is_bit_set(e.valid_fields, cbfsfilter.REG_VALUEFIELD_DATA):
                sb.append("; Data: ")
                if e.value_type in [cbfsfilter.REG_VALUETYPE_SZ, cbfsfilter.REG_VALUETYPE_EXPAND_SZ]:
                    if not self.is_null_or_empty(e.string_value):
                        sb.append(e.string_value)
                elif e.value_type == cbfsfilter.REG_VALUETYPE_BINARY:
                    sb.append(
                        "[{}] {}".format(e.binary_value_size,
                                         self.dump_binary_value(e.binary_value, e.binary_value_size, 20)))
                elif e.value_type in [cbfsfilter.REG_VALUETYPE_DWORD, cbfsfilter.REG_VALUETYPE_QWORD]:
                    sb.append(str(e.integer_value))
                elif e.value_type == cbfsfilter.REG_VALUETYPE_MULTI_SZ:
                    if not self.is_null_or_empty(e.string_value):
                        sb.append(self.replace_char(e.string_value, '\u0017', ' '))

        self.log("QueryValue",
                 context.key_name + "\\" + ("(Default)" if self.is_null_or_empty(e.value_name) else e.value_name),
                 e.status, ''.join(sb))

    def after_rename_key(self, e):
        context = self.get_context(e.key_context)
        if context is None:
            return
        details = "New name: {}".format(context.newName)
        context.newName = None
        self.log("RenameKey", context.key_name, e.status, details)

    def after_set_key(self, e):
        context = self.get_context(e.key_context)
        if context is None:
            return
        details = "Last write time: {}".format(context.last_write_time)
        context.last_write_time = None
        self.log("SetKey", context.key_name, e.status, details)

    def after_set_value(self, e):
        context = self.get_context(e.key_context)
        if context is None:
            return

        sb = []
        if context.value_type != 0:
            sb.append("Type: ")
            sb.append(self.value_type_to_str(context.value_type))
            sb.append("; Data: ")
            if context.value_type in [cbfsfilter.REG_VALUETYPE_SZ, cbfsfilter.REG_VALUETYPE_EXPAND_SZ]:
                if not self.is_null_or_empty(context.string_value):
                    sb.append(context.string_value)
            elif context.value_type == cbfsfilter.REG_VALUETYPE_BINARY:
                sb.append("[{}] {}".format(context.binary_value_size,
                                           self.dump_binary_value(context.binary_value, context.binary_value_size, 20)))
            elif context.value_type in [cbfsfilter.REG_VALUETYPE_DWORD, cbfsfilter.REG_VALUETYPE_QWORD]:
                sb.append(str(context.integer_value))
            elif context.value_type == cbfsfilter.REG_VALUETYPE_MULTI_SZ:
                if not self.is_null_or_empty(context.string_value):
                    sb.append(self.replace_char(context.string_value, '\u0017', ' '))

        self.log("SetValue", "(Default)" if self.is_null_or_empty(context.value_name) else context.value_name, e.status,
                 ''.join(sb))
        context.clear_value()

    def before_create_key(self, e):
        e.key_context = Globals.alloc(self.KeyContext(e.full_name, e.desired_access))

    def before_delete_value(self, e):
        context = self.get_context(e.key_context)
        if context is None:
            return
        context.value_name = e.value_name

    def before_open_key(self, e):
        e.key_context = Globals.alloc(self.KeyContext(e.full_name, e.desired_access))

    def before_rename_key(self, e):
        context = self.get_context(e.key_context)
        if context is None:
            return
        context.newName = e.new_name

    def before_set_key(self, e):
        context = self.get_context(e.key_context)
        if context is None:
            return
        context.last_write_time = e.last_write_time

    def before_set_key_security(self, e):
        pass

    def before_set_value(self, e):
        context = self.get_context(e.key_context)
        if context is None:
            return
        context.value_name = e.value_name
        context.value_type = e.value_type
        context.integer_value = e.integer_value
        context.string_value = e.string_value
        context.binary_value = e.binary_value
        context.binary_value_size = e.binary_value_size

    def cleanup_key_context(self, e):
        if e.key_context != 0:
            Globals.free(e.key_context)

    def close_key_handle(self, e):
        pass

    def error(self, e):
        print("UNHANDLED EXCEPTION: ({}) {}".format(e.error_code, e.description), file=sys.stderr)

    def worker_thread_creation(self, e):
        pass

    def worker_thread_termination(self, e):
        pass


def main(args):
    drv_reboot = False
    opt_terminate = False
    process_name = None
    argi = 0
    arg_len = 0
    stop_opt = False

    RegMon.banner()

    try:
        reg_monitor = RegMon()
        # if no parameters specified, show usage and quit
        if len(args) < 1:
            RegMon.usage()
            return

        while argi < len(args):
            arg = args[argi]
            arg_len = len(arg)
            if arg_len > 0:
                if arg[0] == '-' and not stop_opt:
                    if arg.lower() == "-drv":
                        arg = RegMon.convert_relative_path_to_absolute(args[argi + 1])
                        if argi < len(args):
                            reg_monitor.install(arg)
                    else:
                        print("Error: Invalid option " + arg)
                else:
                    # if we have not set the path yet, do this now.
                    process_name = arg
                    break
            argi += 1

        # Probably, we have just installed the driver, so we can quit without monitoring anything
        if process_name is None or process_name == "":
            return

        print("Press Enter to start monitoring, then press any key to stop and quit.")
        input()

        reg_monitor.set_filter(process_name)
        print("Press Enter to stop monitoring and quit.")
        input()
        reg_monitor.stop_filter()
    except Exception as ex:
        print("Error in main: " + str(ex))


if __name__ == "__main__":
    main(sys.argv[1:])


