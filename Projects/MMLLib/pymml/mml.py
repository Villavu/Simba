#!/usr/bin/env python

from ctypes import *
import platform
from mmlmouse import Mouse
from time import sleep

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
        if self.dll.init() != 0:
            del self.dll
            raise MMLCoreException("Could not initialize the DLL")

    def __del__(self):
        del self.dll

DLL = MMLCore('../libmml.so')

m = Mouse(DLL)


print m[(Mouse.Pos, Mouse.Left, Mouse.Right)]
m[(Mouse.Pos, Mouse.Right)] = ((300,300), True)
print m.getButtonStates()

sleep(2)

# Reset all buttons..
m[(Mouse.Left, Mouse.Right, Mouse.Middle)] = [False for x in range(3)]
for v in zip((Mouse.Left, Mouse.Right), m[(Mouse.Left, Mouse.Right)]):
    print v
print m.getPos()


del DLL


