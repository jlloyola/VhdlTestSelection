#!/usr/bin/python

import sys
from os.path import join, dirname
from vunit import VUnit
from vunit.vunit_cli import VUnitCLI 

root = dirname(__file__)

argv = sys.argv
args = VUnitCLI().parse_args(argv)

ui = VUnit.from_args(args)
lib = ui.add_library("lib")
SourcePath = "Source"
lib.add_source_files(join(root, SourcePath, "*.vhd"))

# Get all the .vhd source files
# SrcFiles = ui.get_source_files('*.vhd')
# Create a list with the files to be tested
# PrjFiles = []
# for file in SrcFiles:
    # if SourcePath in file.name:
        # print (file.name)
        # PrjFiles.append(file.name)
        # Verify checksum

        # If != Append to list of test to run
    #......
# print (PrjFiles)

# From what I have found we have two choices to do test selection:
# + Create two separate VUnit objects (the only way to execute specific tests
#   is to pass them as arguments).
# + Create a separate parent script that calls RunProject.py with the tests
#   two run as arguments. This might imply that we need to do the checksum test
#   without VUnit. This option sounds better.

# FifoAluModule = lib.get_source_file("Source/StdFifo.vhd")
# Dependencies = ui.get_compile_order(FifoAluModule)
# for i in Dependencies:
    # print (i.name)
ui.main()
# testEntities = ui._create_tests(None)
# for i in testEntities:
    # print (i.name)
    # print (i.test_cases)
    # for j in i.test_cases:
        # print (j)
# report = TestReport(printer=ui._printer)
# ui._run_test(testEntities, report)
