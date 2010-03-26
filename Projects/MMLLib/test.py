#!/usr/bin/env python
from ctypes import *

dll = CDLL('./libmml.so')
dll.test.restype = c_char_p
a = dll.test()
print a

dll.init.restype = None
dll.init()

class POINT(Structure):
		_fields_ = [('x', c_int),
					('y', c_int)]

dll.getmousepos.restype = POINT
b = dll.getmousepos()

print b.x, b.y

PPOINT = POINTER(POINT)

dll.returnpoints.restype = PPOINT
c = dll.returnpoints()

print c[0].x

dll.printpoints.restype = c_int
dll.printpoints.argtypes = [PPOINT, c_int]

d = dll.printpoints(c, 2)

dll.hoi.restype = None
dll.hoi.argtypes = [POINTER(c_int)]

e = c_int(5)
dll.hoi(byref(e))

print e


class DTM(Structure):
        _fields_ = [('l' , c_int),
                    ('p' , PPOINT),
                    ('c' , POINTER(c_int)),
                    ('t' , POINTER(c_int)),
                    ('asz' , POINTER(c_int)),
                    ('ash' , POINTER(c_int)),
                    ('bp' , POINTER(c_int)),
                    ('n' , c_char_p)]

PDTM = POINTER(DTM)

dll.givedtm.restype = PDTM
dll.givedtm.argtypes = None

f = dll.givedtm()
print f
print f.contents.l
