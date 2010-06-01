from ctypes import *

class POINT(Structure):
    '''
        POINT represents a point with two variables, x and y.
        POINT is mainly used for Python <-> MML communication.
    '''
    _fields_ = [('x', c_int),
                ('y', c_int)]

class PascalArray(object):
    def __init__(self, pastype, ptr, MC):
        self._type = pastype
        self._p = ptr
        self._mc = MC

    def __del__(self):
        self._mc.dll.fpc_freemem_(self._p)

    def __len__(self):
        return cast(self._p, POINTER(c_ulong))[0]

    def __getitem__(self, pos):
        if pos > len(self):
            print 'Out of range'
            return None
        return cast(self._p, POINTER(self._type))[pos+1]
    
    def __setitem__(self, pos, item):
        if pos > len(self):
            print 'Out of range'
            return
        if sizeof(item) != sizeof(self._type):
            print 'Incorrect structure'
            return
        cast(self._p, POINTER(self._type))[pos] = item

PPOINT = POINTER(POINT)
PINTEGER = POINTER(c_int)

isiterable = lambda x: hasattr(x, '__iter__')
