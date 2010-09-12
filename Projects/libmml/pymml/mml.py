#!/usr/bin/env python

from ctypes import *
from mmlmouse import Mouse
from mmlcolor import Color
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
        self.dll.create_client.restype = c_ulong
        self.dll.create_client.argtypes = None

        self.dll.get_last_error.restype = c_char_p
        self.dll.get_last_error.argtypes = None

        if self.dll.init() != 0:
            del self.dll
            raise MMLCoreException("Could not initialize the DLL")

    def get_last_error(self):
        t = self.dll.get_last_error()
        s = str(t)
        del t
        return s

    def __del__(self):
        del self.dll


if __name__ == '__main__':
    DLL = MMLCore('../libmml.so')

    client = DLL.dll.create_client()
    print 'Python Client: %d' % client
    if client in (0, 1):
        raise Exception('Could create a client');

    c = Color(DLL, client)


    ret = c.find((0, 0, 100000, 10000), 0)

    ret = c.find((0, 0, 100, 100), 0)
    print ret

    ret = c.findAll((0, 0, 100, 100), 0)
    print ret
    
    m = Mouse(DLL, client)
    
   
    print m[(Mouse.Pos, Mouse.Left, Mouse.Right)]
    m[(Mouse.Pos, Mouse.Right)] = ((300,300), True)

    print m.getButtonStates()
    sleep(0.5)
    m.setPos((200,200))
   
    sleep(2)
    print 'Done'

    m[(Mouse.Left, Mouse.Right, Mouse.Middle)] = [False for x in range(3)]
    for v in zip((Mouse.Left, Mouse.Right), m[(Mouse.Left, Mouse.Right)]):
        print v
    print m.getPos()
    
    del DLL
