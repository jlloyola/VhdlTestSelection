#!/usr/bin/python

import hashlib
import os
import fnmatch
from os import path

class HashManager :
    """
    Class that manages computing a hash from object files generated from source files.
    As well as saving those computed hashes into an specified directory.
    """

    # Constructor
    def __init__(self, lib_name, obj_dir, src_dir, saved_hash_dir, hash_ext, obj_ext):
        self.lib_name = lib_name
        self.obj_dir = obj_dir
        self.src_dir = src_dir
        self.saved_hash_dir = saved_hash_dir
        self.hash_ext = hash_ext
        self.obj_ext = obj_ext

    # Private Methods
    def _get_hash_file_path(self, source_file):
        """
        Returns the file path that contains the hash associated with source_file.
        """
        head, tail = path.split(source_file)
        hash_file_name = '{}.{}'.format(path.splitext(tail)[0], self.hash_ext)
        hash_file_path = path.join(self.saved_hash_dir, head, hash_file_name)
        return hash_file_path

    def _get_object_file_path(self, source_file):
        """
        Returns the file path to the object file associated with source_file.
        """
        head, tail = path.split(source_file)
        object_file_name = '{}.{}'.format(path.splitext(tail)[0], self.obj_ext)
        object_file_path = path.join(self.obj_dir, self.lib_name, object_file_name)
        return object_file_path

    # Public Methods
    def get_saved_hash(self, source_file):
        """
        Gets the saved hash associated with source_file. Return None if no
        hash was saved previously.
        """
        hash_file_path = self._get_hash_file_path(source_file)
        if (path.isfile(hash_file_path)):
            with open(hash_file_path, 'r') as afile:
                return afile.read()
        # Not found
        return None

    def get_file_hash(self, source_file):
        """
        Computes the hash of the object file associated with source_file.
        """
        file_path = self._get_object_file_path(source_file)
        # on windows object files are not generated
        # so we use source files instead.
        file_path = file_path if os.path.exists(file_path) else source_file
        m = hashlib.md5()
        with open(file_path, 'rb') as afile:
            buf = afile.read()
            m.update(buf)
        return m.hexdigest()

    def save_hash(self, source_file, hash):
        """
        Saves a computed hash into a file that mimics source_file relative path
        hierarchy, but in saved_hash_directory. Creates all needed directories.
        """
        hash_file = self._get_hash_file_path(source_file)
        print(hash_file)
        # print(source_file)
        # print(path.dirname(hash_file))
        try:
            # Create directory structure if needed.
            os.makedirs(os.path.dirname(hash_file), exist_ok=True)
            with open(hash_file, 'w+') as afile:
                afile.write(hash)
        except Error as e:
            print(e)
