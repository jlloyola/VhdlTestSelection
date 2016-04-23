#!/usr/bin/python

import os
import sys
import logging
from os.path import join, dirname
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
logging.debug(args)

ui = VUnit.from_args(args)
lib = ui.add_library(args.ProjectLibrary)
SourcePath = args.ProjectDirectory
for rootDir, dirs, files in os.walk(root, SourcePath):
    lib.add_source_files(join(rootDir,'*.vhd'), allow_empty=True)

ui.main()