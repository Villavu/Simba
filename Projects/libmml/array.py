#!/usr/bin/env python
from ctypes import *
import platform

if platform.system() == 'Windows':
    dll = CDLL('./libmml.dll')
else:
    dll = CDLL('./libmml.so')

class PascalArray(object):
    def __init__(self, pastype, ptr):
        self._type = pastype
        self._p = ptr

    def __del__(self):
        print 'Freeing now: %i' % self._p
        dll.freearr(self._p)
        print 'Freed'

    def __len__(self):
        return cast(self._p, POINTER(c_ulong))[-1] + 1

    def __getitem__(self, pos):
        if pos > len(self):
            print 'Out of range'
            return None
        return cast(self._p, POINTER(self._type))[pos]
    
    def __setitem__(self, pos, item):
        if pos > len(self):
            print 'Out of range'
            return
        if sizeof(item) != sizeof(self._type):
            print 'Incorrect structure'
            return
        cast(self._p, POINTER(self._type))[pos] = item


class POINT(Structure):
        _fields_ = [('x', c_int),
                    ('y', c_int)]
PPOINT = POINTER(POINT)

dll.returnpoints.restype = POINTER(POINT)
dll.returnpoints.argtypes = None

dll.fpc_freemem_.restype = None
dll.fpc_freemem_.argtypes = [POINTER(c_int)]

dll.fpc_allocmem_.restype = POINTER(c_int)
dll.fpc_allocmem_.argtypes = [c_int]

mem = dll.fpc_allocmem_(8)
print 'allocated'
dll.fpc_freemem_(mem)
print 'freed'

myarr = PascalArray(POINT, dll.returnarray())
print 'Allocated array'

print len(myarr)
for i in range(len(myarr)):
    print myarr[i].x, myarr[i].y
p = POINT()
p.x = 42;
p.y = 42;

myarr[0] = p
for i in range(len(myarr)):
    print myarr[i].x, myarr[i].y

print 'Freeing'
del myarr
#print myarr
#print 'freeing'
#dll.freearr.argtypes = [POINTER(c_int)]
#dll.freearr.restype = None
#
#dll.freearr(cast(myarr._p, POINTER(c_int)))

#print myarr[0].x
