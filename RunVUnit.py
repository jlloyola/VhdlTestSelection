#!/usr/bin/python

import sys
from os.path import join, dirname
import os
from vunit import VUnit
from vunit.vunit_cli import VUnitCLI

root = dirname(__file__)

cli = VUnitCLI()
cli.parser.add_argument('--ProjectDirectory')
cli.parser.add_argument('--ProjectLibrary')
args = cli.parse_args()

# Create VUNit instance from custom arguments
ui = VUnit.from_args(args=args)

# Use args.custom_arg here ...
print(args)

ui = VUnit.from_args(args)
lib = ui.add_library(args.ProjectLibrary)
SourcePath = args.ProjectDirectory
for rootDir, dirs, files in os.walk(root, SourcePath):
    lib.add_source_files(join(rootDir,'*.vhd'),allow_empty=True)

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
