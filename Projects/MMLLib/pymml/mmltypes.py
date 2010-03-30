from ctypes import *

class POINT(Structure):
        _fields_ = [('x', c_int),
                    ('y', c_int)]

PPOINT = POINTER(POINT)


