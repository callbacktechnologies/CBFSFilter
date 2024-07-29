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


import os

PROGRAM_NAME = "{713CC6CE-B3E2-4fd9-838D-E28F558F6866}"
FAKE_ALTITUDE_FOR_DEBUG = "360000.24"
ERROR_PRIVILEGE_NOT_HELD = 0x0522
ERROR_FILE_NOT_FOUND = 2
ERROR_PATH_NOT_FOUND = 3
ERROR_INVALID_NAME = 123

filter : CBFilter = None
folder_path_to_hide = ""
folder_name_to_hide = ""


# ---------------------------------
#      Helper functions
# ---------------------------------

def six_input(prompt):
    print(prompt)
    if sys.version_info[0] == 3:
        return input()
    #else:
    #    return raw_input(prompt)


def is_drive_letter(path_str):
    if not path_str:
        return False

    c = path_str[0]
    if c.isalpha() and len(path_str) == 2 and path_str[1] == ':':
        return True
    else:
        return False


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
        elif not is_drive_letter(path_str):
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


# ---------------------------------
#        Event handlers
# ---------------------------------

def cb_after_enumerate_directory(params: CBFilterAfterEnumerateDirectoryEventParams):
    global filter
    global folder_name_to_hide

    # if the filesystem returns the name of the "hidden" directory, then block it
    if params.file_name == folder_name_to_hide:
        params.process_request = False
        process_name = filter.get_originator_process_name()
        print("[enumerate_directory]: %s (by %s) -> BLOCKED" % (params.file_name, process_name))


# ---------------------------------

def cb_before_create_file(params: CBFilterBeforeCreateFileEventParams):
    global filter
    global folder_name_to_hide, folder_path_to_hide

    file_name = params.file_name
    if platform.system() == "Windows":
        file_name = params.file_name.lower()

    # block attempts to create a new folder with the same name as the hidden folder
    if file_name == folder_path_to_hide:
        params.result_code = ERROR_INVALID_NAME
        process_name = filter.get_originator_process_name()
        print("[create_file]: %s (by %s) -> INVALID_NAME" % (params.file_name, process_name))
        return

    # block attempts to create any object in the hidden folder and its children
    folder = folder_path_to_hide + os.sep
    if file_name.startswith(folder):
        params.result_code = ERROR_PATH_NOT_FOUND
        process_name = filter.get_originator_process_name()
        print("[create_file]: %s (by %s) -> PATH_NOT_FOUND" % (params.file_name, process_name))


# ---------------------------------

def cb_before_open_file(params: CBFilterBeforeOpenFileEventParams):
    global filter, folder_name_to_hide, folder_path_to_hide

    file_name = params.file_name.lower()

    # block attempts to open the hidden folder itself
    if file_name == folder_path_to_hide:
        params.result_code = ERROR_FILE_NOT_FOUND
        process_name = filter.get_originator_process_name()
        print("[open_file]: %s (by %s) -> FILE_NOT_FOUND" % (params.file_name, process_name))
        return

    # block attempts to open any object in the hidden folder and its children
    folder = folder_path_to_hide + os.sep
    if file_name.startswith(folder):
        params.result_code = ERROR_PATH_NOT_FOUND
        process_name = filter.get_originator_process_name()
        print("[open_file]: %s (by %s) -> PATH_NOT_FOUND" % (params.file_name, process_name))


# ---------------------------------
#           Main code
# ---------------------------------

def banner():
    print("CBFS Filter Directory Hider Demo")
    print("The sample shows how to hide a directory from access by other programs.")


# ---------------------------------

def print_usage():
    print()
    print("Usage: %s <command> <parameters>" % os.path.basename(sys.argv[0]))
    print()
    print("Commands:")
    print("  i <cab_file>    - install the CBFilter driver from the CAB file")
    print("  u <cab_file>    - uninstall the CBFilter driver using the CAB file")
    print("  d               - check if the CBFilter driver is installed")
    print("  h <folder_name> - hide a folder")


# ---------------------------------

def check_driver():
    l_filter = CBFilter()
    state = l_filter.get_driver_status(PROGRAM_NAME)
    if state == MODULE_STATUS_NOT_PRESENT:
        print("CBFilter driver not installed")
    else:
        version = l_filter.get_driver_version(PROGRAM_NAME)
        ver_high, ver_low = version >> 32, version & 0xffffffff
        print("CBFilter driver version: %d.%d.%d.%d" % (
        ver_high >> 16, ver_high & 0xffff, ver_low >> 16, ver_low & 0xffff))
        if state != MODULE_STATUS_RUNNING:
            print("NOTE: the driver is NOT running; please restart the computer to let the driver start")


# ---------------------------------

def get_cab_file():  # -> str:
    if len(sys.argv) < 3:
        print("ERROR: No CAB file specified")
        print_usage()
        return ""

    cab_file = convert_relative_path_to_absolute(sys.argv[2])
    if cab_file is None:
        print("Error: Invalid Driver Path.")
        return
    if not os.path.isfile(cab_file):
        print("ERROR: CAB file not found")
        print("  ->", cab_file)
        return ""

    return cab_file


# ---------------------------------

def install():
    cab_file = get_cab_file()
    if cab_file == "":
        return

    print("Installing driver...")
    print("  ->", cab_file)
    print()

    l_filter = CBFilter()
    try:
        reboot_needed = l_filter.install(cab_file, PROGRAM_NAME, None, FAKE_ALTITUDE_FOR_DEBUG, INSTALL_REMOVE_OLD_VERSIONS, None)

        print("Driver installed successfully")
        version = l_filter.get_driver_version(PROGRAM_NAME)
        ver_high, ver_low = version >> 32, version & 0xffffffff
        print("  -> driver version: %d.%d.%d.%d" % (ver_high >> 16, ver_high & 0xffff, ver_low >> 16, ver_low & 0xffff))
        if reboot_needed:
            print("NOTE: Reboot the computer for the changes to take effect")
    except CBFSFilterError as err:
        if err.code == ERROR_PRIVILEGE_NOT_HELD:
            print("ERROR: administrator rights are required; please run the script as Administrator")
        else:
            print("ERROR: driver NOT installed")
            print("  -> (%d) %s" % (err.code, err.message))


# ---------------------------------

def uninstall():
    l_filter = CBFilter()

    state = l_filter.get_driver_status(PROGRAM_NAME)
    if state == MODULE_STATUS_NOT_PRESENT:
        print("Driver not installed")
        return

    cab_file = get_cab_file()
    if cab_file == "":
        return

    print("Uninstalling driver...")
    print("  ->", cab_file)
    print()

    try:
        reboot_needed = l_filter.uninstall(cab_file, PROGRAM_NAME, None, UNINSTALL_VERSION_ALL)

        print("Driver uninstalled successfully")
        if reboot_needed:
            print("NOTE: Reboot the computer for the changes to take effect")
    except CBFSFilterError as err:
        if err.code == ERROR_PRIVILEGE_NOT_HELD:
            print("ERROR: administrator rights are required; please run the script as Administrator")
        else:
            print("ERROR: failed to uninstall the driver")
            print("  -> (%d) %s" % (err.code, err.message))


# ---------------------------------

def hide():
    global filter, folder_path_to_hide, folder_name_to_hide

    if len(sys.argv) < 3:
        print("ERROR: No folder specified")
        print_usage()
        return ""

    folder_path_to_hide = convert_relative_path_to_absolute(os.path.normpath(sys.argv[2].lower()))
    if not os.path.exists(folder_path_to_hide):
        print("ERROR: folder does not exist - nothing to hide")
        print("  ->", folder_path_to_hide)
        return

    if not os.path.isdir(folder_path_to_hide):
        print("ERROR: the name doesn't seem to point to a folder")
        print("  ->", folder_path_to_hide)
        return

    # if the path ends with \ it's needed to remove it
    if folder_path_to_hide[-1] == '\\':
        folder_path_to_hide = folder_path_to_hide[:-1]

    parent = os.path.dirname(folder_path_to_hide)
    if parent == folder_path_to_hide:
        print("ERROR: cannot hide the root of a drive")
        print("  ->", parent)
        return

    folder_name_to_hide = os.path.basename(folder_path_to_hide)
    if platform.system() == "Windows":
        folder_name_to_hide = folder_name_to_hide.lower()

    filter = CBFilter()
    filter.initialize(PROGRAM_NAME)
    filter.on_after_enumerate_directory = cb_after_enumerate_directory
    filter.on_before_create_file = cb_before_create_file
    filter.on_before_open_file = cb_before_open_file

    print("Configuring the filter...")
    try:
        filter.add_filter_rule(parent, ACCESS_NONE,
                               FS_CE_AFTER_ENUMERATE_DIRECTORY,
                               FS_NE_NONE)
        print("  - filtered enumeration of folder:", parent)

        filter.add_filter_rule(folder_path_to_hide, ACCESS_NONE,
                               FS_CE_BEFORE_CREATE |
                               FS_CE_BEFORE_OPEN,
                               FS_NE_NONE)
        print("  - filtered opening and creation of folder:", folder_path_to_hide)

        file_mask = folder_path_to_hide + os.sep + "*.*"
        filter.add_filter_rule(file_mask, ACCESS_NONE,
                               FS_CE_BEFORE_CREATE |
                               FS_CE_BEFORE_OPEN,
                               FS_NE_NONE)
        print("  - filtered opening and creation of objects:", file_mask)

        filter.process_failed_requests = False
    except CBFSFilterError as err:
        print("ERROR: failed to configure the filter")
        print("  -> (%d) %s" % (err.code, err.message))
        return

    print("Configured")
    print("Starting the filter...")
    try:
        filter.start_filter(10000)
    except CBFSFilterError as err:
        print("ERROR: failed to start the filter")
        print("  -> (%d) %s" % (err.code, err.message))
        return

    print("The filter is working and the folder is hidden")
    print("  ->", folder_path_to_hide)

    while True:
        six_input("Press [Enter] to finish")
        try:
            print("Stopping the filter...")
            filter.delete_all_filter_rules()
            if filter.active:
                filter.stop_filter()
            print("Filter stopped")
            break
        except CBFSFilterError as err:
            print("ERROR: Failed to stop the filter")
            print("  -> (%d) %s" % (err.code, err.message))
            print("Will try again...")
            continue

    print("\nDemo finished")


# ---------------------------------

def test():
    folder = "C:\\Temp"

    print(os.path.basename(folder).lower())

    # pass


# ---------------------------------
#   Program body

banner()

if len(sys.argv) < 2:
    print_usage()
else:
    command = sys.argv[1]
    if command == "i":
        install()
    elif command == "u":
        uninstall()
    elif command == "d":
        check_driver()
    elif command == "h":
        hide()
    elif command == "t":
        test()
    else:
        print("ERROR: unknown command '%s'" % command)
        print_usage()

print()


