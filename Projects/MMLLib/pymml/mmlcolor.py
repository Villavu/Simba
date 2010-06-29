from ctypes import *
from mmltypes import isiterable
from mmltypes import POINT, PPOINT, PINTEGER
from mmltypes import PascalArray
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

    def __init__(self, MC):
        """
            Initialise the Color object.
        """
        self._mc = MC
        self._initialiseDLLFuncs()

    def find(self, box, color, tol = 0):
        """
            find a color in a box, with a specific tolerance.
            returns a tuple of the x, y value of a matched color.
            None if no color was found.
        """
        x, y = (c_int(-1), c_int(-1))
        if tol is 0:
            ret = self._mc.dll.findColor(byref(x), byref(y), color, *box)
        else:
            ret = self._mc.dll.findColorTolerance(byref(x), byref(y), color,
                    tol, *box)

        if ret is RESULT_OK:
            return (x, y)

        return None

    def findAll(self, box, color, tol = 0):
        """
            find all colors in a box, with a specific tolerance.
            returned are all the matching points
        """
        ptr = PPOINT()
        if tol is 0:
            self._mc.dll.findColors(byref(ptr), color, *box)
        else:
            self._mc.dll.findColorsTolerance(byref(ptr), color, tol, *box)

        arr = PascalArray(POINT, ptr, self._mc)
#        print 'Length:', len(arr)
#        for i in range(len(arr)):
#            print i, arr[i].x, arr[i].y
        # FIXME return python list?
        return arr

    def _initialiseDLLFuncs(self):
        self._mc.dll.findColor.restype = c_int
        self._mc.dll.findColor.argtypes = [PINTEGER, PINTEGER, c_int, c_int,
                c_int, c_int, c_int]
        self._mc.dll.findColorTolerance.restype = c_int
        self._mc.dll.findColorTolerance.argtypes = [PINTEGER, PINTEGER, c_int, c_int,
                c_int, c_int, c_int, c_int]
        self._mc.dll.findColors.restype = c_int
        self._mc.dll.findColors.argtypes = [POINTER(PPOINT), c_int, c_int,
                c_int, c_int, c_int]
        self._mc.dll.findColorsTolerance.restype = c_int
        self._mc.dll.findColorsTolerance.argtypes = [POINTER(PPOINT), c_int, c_int,
                c_int, c_int, c_int, c_int]
