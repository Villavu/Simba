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
