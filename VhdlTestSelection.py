#!/usr/bin/python

import os
import argparse
import time
import fnmatch
import sys, logging
import subprocess
from os import path
from vunit import VUnit
from vunit.vunit_cli import VUnitCLI
from HashManager import HashManager

parser = argparse.ArgumentParser(description='VHDL Test selection.')

parser.add_argument('source', help='Directory containing source files.')
parser.add_argument('lib', help='Library name.')
parser.add_argument('--hash', default='saved_hashes', help='Directory where computed hashes are saved.')
parser.add_argument('--object_ext', default='o', help='Object file extension.')
parser.add_argument('--hash_ext', default='xtc_hash', help='Saved hash file extension.')

args = vars(parser.parse_args())
<<<<<<< HEAD
=======

>>>>>>> b978c6be46c9ab37797f326ba0c451145a837fa1

# Uncomment this line to print the debug log
# logging.basicConfig(stream=sys.stderr, level=logging.DEBUG)

# root directory for this file
root = path.dirname(__file__)

# Location of the source files to be processed
prj_dir = args['source']
lib_name = args['lib']
hash_dir = args['hash']
hash_ext = args['hash_ext']
object_ext = args['object_ext']
compiled_libraries = './vunit_out/ghdl/libraries' # TODO: we should get this dynamically
hash_manager = HashManager(lib_name, compiled_libraries, prj_dir, hash_dir, hash_ext, object_ext)

# Compile the VHDL project to ensure the files are up to date
try:
    runVUnitPath = path.normpath(path.abspath('./RunVUnit.py'))
    subprocess.check_call('{} --compile --ProjectDirectory {} --ProjectLibrary {}'.format(runVUnitPath, prj_dir, lib_name), shell=True)
except:
    logging.error('VUnit compilation failed')
    sys.exit(2)


# Contains the latest hashes of the files whose will be executed.
hash_dict = {}
start_time = time.time()
analysis_time = 0
execution_time = 0

# All because we want to know the compile order of a source file.
args = VUnitCLI().parse_args(['--compile'])
ui = VUnit.from_args(args)
uilib = ui.add_library(lib_name)
for rootDir, dirs, files in os.walk(prj_dir):
    uilib.add_source_files(path.join(rootDir,'*.vhd'),allow_empty=True)

def get_file_dependencies(source_file):
    """
    Returns a list of file dependencies, based on the compile order
    """
    ui_src_file = uilib.get_source_file(source_file)
    dependencies = ui.get_compile_order(ui_src_file)
    dependency_list = []
    logging.debug('Get file dependencies from: ' + source_file)
    for dependency in dependencies:
        dependency_list.append(dependency.name)
        logging.debug(dependency.name)
    return dependency_list

def source_file_in_project(source_file):
    return fnmatch.fnmatch(source_file, prj_dir + '/*.vhd')

def filter_file_dependencies(dependency_list):
    """
    Returns a list of dependencies that are only in the current project directory.
    """
    return filter(source_file_in_project, dependency_list)

def analysis_stage():
    # Contains the files whose testbench will be executed.
    files_to_test = []
    # Search the workspace for all VHDL files
    for root, dirs, files in os.walk(prj_dir):
        for file in files:
            if fnmatch.fnmatch(file, '*.vhd'):
                # Obtain the previous and current hashes from the file
                file_path = path.join(root, file)

                dependecy_list = get_file_dependencies(file_path)
                dependecy_list = filter_file_dependencies(dependecy_list)

                logging.debug('Source file: {}'.format(file_path))

                for dep in dependecy_list:
                    previous_hash = hash_manager.get_saved_hash(dep)
                    current_hash = hash_manager.get_file_hash(dep)

                    logging.debug('Dependency file: {}'.format(dep))
                    logging.debug('Previous Hash: {}'.format(previous_hash))
                    logging.debug('Current Hash: {}'.format(current_hash))
                    # Add the file to files_to_test when the hashes do not match,
                    # store its current hash in the hash dictionary, and store its path
                    # into a dictionary too.
                    if previous_hash != current_hash:
                        files_to_test.append(file_path)
                        hash_dict[dep] = current_hash
    return set(files_to_test)

def save_computed_hashes():
    for source_file, computed_hash in hash_dict.items():
        # Update the hash file when the tests complete successfully
        try:
            logging.debug('Trying to save hash for: ' + source_file)
            hash_manager.save_hash(source_file, computed_hash)
        except Error as e:
            logging.error('save_hash failed')
            logging.debug(e)

def execute_stage(files_to_test):
    all_tests_passed = True
    logging.debug('Tests to run:' + str(files_to_test))
    for file_source in files_to_test:
        # I want to be able to run it on unix too.
        # let me know if this still works on windows.
        vunit_command = path.normpath(path.abspath('./RunVUnit.py'))
        # Remove the .vhd extension, make all file names lower case (required by
        # VUnit) and, concatenate it to the vunit_command
        test_entity = str(file_source)
        test_entity = test_entity.lower()
        test_entity = path.basename(test_entity)
        # TODO: Find a better way to send the specific test.
        pattern = ' *' + test_entity.rstrip('.vhd') + '*'
        vunit_command += pattern
        vunit_command += ' --ProjectDirectory {} --ProjectLibrary {}'.format(prj_dir, lib_name)
        # Run the selected tests
        try:
            subprocess.check_call(vunit_command, shell=True)
        except:
            all_tests_passed = False
            print ("VUnit tests failed")
    if all_tests_passed:
        # Collection stage
        save_computed_hashes()
    if len(files_to_test) == 0:
        print('Files have not changed. No tests were required to run.')


selected_tests = analysis_stage()
analysis_time = time.time()
logging.debug('Files that changed:' + str(hash_dict))
execute_stage(selected_tests)
execution_time = time.time()

print("Analysis time: {:f} seconds".format(analysis_time - start_time))
print("Execution time: {:f} seconds".format(execution_time - analysis_time))

print("Total time: {:f} seconds".format(execution_time - start_time))