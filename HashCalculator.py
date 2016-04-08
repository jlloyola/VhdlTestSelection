#!/usr/bin/python

import hashlib
import os
import fnmatch
from os import path

library_name = 'lib'
object_dir = './vunit_out/ghdl/libraries'
source_dir = './Source'
saved_hash_dir = './saved_hashes'
hash_ext = 'xtc_hash'
object_ext = 'o'

def ensure_dir_exists(file_path):
    head = file_path
    to_create = []
    while not path.isdir(head) and not head == '':
        head, tail = path.split(head)
        print (head)
        print (tail)
        to_create.append(tail)
    while len(to_create) > 0:
        head = path.join(head, to_create.pop())
        os.mkdir(head)

def get_hash_file(source_file):
    head, tail = path.split(source_file)
    hash_file_name = '{}.{}'.format(path.splitext(tail)[0], hash_ext)
    return path.join(saved_hash_dir, head, hash_file_name)

def get_object_file(source_file):
    head, tail = path.split(source_file)
    object_file_name = '{}.{}'.format(path.splitext(tail)[0], object_ext)
    return path.join(object_dir, library_name, object_file_name)

# public methods
def get_saved_hash(source_file):
    hash_file = get_hash_file(source_file)
    if (path.isfile(hash_file)):
        with open(hash_file, 'r') as afile:
            return afile.read()
    return None

def get_file_hash(source_file):
    file_path = get_object_file(source_file)
    # on windows object files are not generated
    # so we use source files instead.
    file_path = file_path if os.path.exists(file_path) else source_file
    m = hashlib.md5()
    with open(file_path, 'rb') as afile:
        buf = afile.read()
        m.update(buf)
    return m.hexdigest()

def save_hash(source_file, hash):
    hash_file = get_hash_file(source_file)
    # ensure_dir_exists(hash_file)
    os.makedirs(os.path.dirname(hash_file), exist_ok=True)
    with open(hash_file, 'w+') as afile:
        afile.write(hash)
#end public methods

def get_hashes():
    # vunit_out/ghdl/libraries/lib/
    file_list = os.listdir(source_dir)
    for file in file_list:
        if fnmatch.fnmatch(file, '*.vhd'):
            file_path = path.join(source_dir, file)
            print ('Source file: {}'.format(file_path))
            print ('Hash file: {}'.format(get_hash_file(file_path)))
            print ('Object file: {}'.format(get_object_file(file_path)))

if __name__ == "__main__":
    get_hashes()