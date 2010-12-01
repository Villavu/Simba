#!/usr/bin/env python

from ctypes import *
from mouse import Mouse
from color import Color
from time import sleep
from mtypes import PINTEGER

class MMLCoreException(Exception):
    def __init__(self, err):
        Exception.__init__(self, err)

class MMLCore(object):
    '''
        The MMLCore object is to be opened only once per Python instance.
        It opens the libmml library, and calls init().
    '''
    def __init__(self, dllpath):
        self.dll = CDLL(dllpath)

        self.dll.init.restype = c_int
        self.dll.init.argtypes = None
        self.dll.create_client.restype = c_ulong
        self.dll.create_client.argtypes = None

        self.dll.get_last_error.restype = c_char_p
        self.dll.get_last_error.argtypes = None

        self.dll.free_ptr.restype = c_bool
        self.dll.free_ptr.argtypes = [c_void_p]

        if self.dll.init() != 0:
            del self.dll
            raise MMLCoreException("Could not initialize the DLL")

    def get_last_error(self):
        t = self.dll.get_last_error()
        s = str(t)
        del t
        return s

    def free(self, ptr):
        _ptr = cast(ptr, c_void_p)
        self.dll.free_ptr(_ptr)

    def __del__(self):
        del self.dll

