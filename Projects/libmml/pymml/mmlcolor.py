from ctypes import *
from mmltypes import isiterable
from mmltypes import POINT, PPOINT, PINTEGER
from mmltypes import RESULT_OK, RESULT_FALSE, RESULT_ERROR

"""
The Color Class
---------------

This class does the color finding.
"""

class ColorException(Exception):
    def __init__(self, err):
        Exception.__init__(self, err)



# FIXME: Complete...
class Color(object):
    '''
        The Color class.
    '''

    _mc = None

    def __init__(self, MC, cli):
        """
            Initialise the Color object.
        """
        self._mc = MC
        self._cli = cli
        self._initialiseDLLFuncs()

    def find(self, box, color, tol = 0):
        """
            find a color in a box, with a specific tolerance.
            returns a tuple of the x, y value of a matched color.
            None if no color was found.
        """
        x, y = (c_int(-1), c_int(-1))
        if tol is 0:
            ret = self._mc.dll.find_color(self._cli, byref(x), byref(y),
                    color, *box)
        else:
            ret = self._mc.dll.find_color_tolerance(self._cli, byref(x),
                    byref(y), color, tol, *box)

        if ret is RESULT_OK:
            return (x, y)

        return None

    def findAll(self, box, color, tol = 0):
        """
            find all colors in a box, with a specific tolerance.
            returned are all the matching points
        """
        ptr, _len = PPOINT(), c_int(42)
        print type(_len)
        if tol is 0:
            self._mc.dll.find_colors(self._cli, byref(ptr), byref(_len),
                    color, *box)
        else:
            self._mc.dll.find_colors_tolerance(self._cli, byref(ptr),
                    byref(_len), color, tol, *box)

#        print 'Length:', _len
#        for x in range(_len.value):
#            print ptr[x].x
#        print ptr
        # FIXME return python list?
        return ''

    def _initialiseDLLFuncs(self):
        self._mc.dll.find_color.restype = c_int
        self._mc.dll.find_color.argtypes = [c_ulong, PINTEGER, PINTEGER, c_int,
                c_int, c_int, c_int, c_int]
        self._mc.dll.find_color_tolerance.restype = c_int
        self._mc.dll.find_color_tolerance.argtypes = [c_ulong, PINTEGER,
                PINTEGER, c_int, c_int, c_int, c_int, c_int, c_int]
        self._mc.dll.find_colors.restype = c_int
        self._mc.dll.find_colors.argtypes = [c_ulong, POINTER(PPOINT),
                POINTER(c_int), c_int, c_int, c_int, c_int, c_int]
        self._mc.dll.find_colors_tolerance.restype = c_int
        self._mc.dll.find_colors_tolerance.argtypes = [c_ulong,
                POINTER(PPOINT), POINTER(c_int), c_int, c_int,
                c_int, c_int, c_int, c_int]
