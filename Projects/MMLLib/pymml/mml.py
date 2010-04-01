#!/usr/bin/env python

from ctypes import *
import platform
from mmlmouse import Mouse

class MMLCoreException(Exception):
    def __init__(self, err):
        Exception.__init__(self, err)

class MMLCore(object):
    def __init__(self, dllpath):
        self.dll = CDLL(dllpath)

        self.dll.init.restype = c_int
        self.dll.init.argtypes = None
        if self.dll.init() != 0:
            del self.dll
            raise MMLCoreException("Could not initialize the DLL")

    def __del__(self):
        del self.dll

DLL = MMLCore('../libmml.so')

m = Mouse(DLL)
print m._getMousePos()

del DLL


