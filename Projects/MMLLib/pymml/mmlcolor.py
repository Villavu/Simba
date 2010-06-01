from ctypes import *
from mmltypes import isiterable
from mmltypes import POINT, PPOINT, PINTEGER


class ColorException(Exception):
    def __init__(self, err):
        Exception.__init__(self, err)

# FIXME: Complete...
class Color(object):
    '''
        The Color class.
    '''

    _mc = None

    def __init__(self, MC):
        self._mc = MC
        self._initialiseDLLFuncs()

    def find(self, box, color):
        x, y = (c_int(-1), c_int(-1))
        self._mc.dll.findColor(byref(x), byref(y), color, *box)
        return (x, y)

    def _initialiseDLLFuncs(self):
        self._mc.dll.findColor.restype = c_int
        self._mc.dll.findColor.argtypes = [PINTEGER, PINTEGER, c_int, c_int,
                c_int, c_int, c_int]
