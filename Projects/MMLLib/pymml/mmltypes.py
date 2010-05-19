from ctypes import *

class POINT(Structure):
    '''
        POINT represents a point with two variables, x and y.
        POINT is mainly used for Python <-> MML communication.
    '''
    _fields_ = [('x', c_int),
                ('y', c_int)]

PPOINT = POINTER(POINT)

isiterable = lambda x: hasattr(x, '__iter__')
