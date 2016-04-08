#!/usr/bin/python

import os
from os import path
import fnmatch
import sys, logging
import subprocess
from HashCalculator import get_saved_hash, get_file_hash, save_hash

# Uncomment this line to print the debug log
# logging.basicConfig(stream=sys.stderr, level=logging.DEBUG)

# Location of the source files to be processed
prj_dir = 'Source'

# Compile the VHDL project to ensure the files are up to date
try:
    subprocess.check_call("RunVUnit.py --compile", shell=True)
except:
    print ("VUnit compilation failed")
    sys.exit(2)

# Contains the files whose testbench will be executed.
files_to_test = []
# Contains the latest hashes of the files whose will be executed.
hash_dict = {}
# Contains the path of the files whose will be executed.
file_path_dict = {}
# Search the workspace for all VHDL files
file_list = os.listdir(prj_dir)
for file in file_list:
    if fnmatch.fnmatch(file, '*.vhd'):
        # Obtain the previous and current hashes from the file
        file_path = path.join(prj_dir, file)
        previous_hash = get_saved_hash(file_path)
        current_hash = get_file_hash(file_path)
        logging.debug('Source file: {}'.format(file_path))
        logging.debug('Previous Hash: {}'.format(previous_hash))
        logging.debug('Current Hash: {}'.format(current_hash))
        # Add the file to files_to_test when the hashes do not match,
        # store its current hash in the hash dictionary, and store its path
        # into a dictionary too.
        if previous_hash != current_hash:
            files_to_test.append(file)
            hash_dict[file] = current_hash
            file_path_dict[file] = file_path

logging.debug(files_to_test)

if files_to_test != []:
    # Execute the tests for the files with whose hash mismatched
    for f in files_to_test:
        vunit_command = "RunVUnit.py "
        # Remove the .vhd extension, make all file names lower case (required by
        # VUnit) and, concatenate it to the vunit_command
        test_entity = str(f)
        test_entity = test_entity.lower()
        vunit_command += (" *" + test_entity.rstrip(".vhd") + "*")
        # Run the selected tests
        try:
            subprocess.check_call(vunit_command, shell=True)
            # Update the hash file when the tests complete successfully
            try:
                save_hash(file_path_dict[f], hash_dict[f])
            except:
                print ("save_hash failed")
        except:
            print ("VUnit tests failed")
else:
    print ("Files have not changed. No tests were required to run.")




