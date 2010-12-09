from ctypes import *
from mtypes import isiterable
from mtypes import POINT, PPOINT, PINTEGER
from mtypes import RESULT_OK, RESULT_FALSE, RESULT_ERROR
from mtypes import MMLException

"""
The Color Class
---------------

This class does the color finding.
"""

class ColorException(MMLException):
    def __init__(self, err):
        MMLException.__init__(self, err)


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
        self._initialise_dll_funcs()

    def get(self, pt):
        """
        Gets color at pt[0], pt[1].
        Yields integer.
        """
        col = c_int(-1)
        self._mc.dll.get_color(self._cli, pt[0], pt[1], byref(col))
        if col is RESULT_OK:
            return col
        elif ret is RESULT_ERROR:
            raise ColorException(self._mc.get_last_error())
        return None
        
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
        elif ret is RESULT_ERROR:
            raise ColorException(self._mc.get_last_error())

        return None

    def find_all(self, box, color, tol = 0):
        """
            find all colors in a box, with a specific tolerance.
            returned are all the matching points
        """
        ptr, _len = PPOINT(), c_int(42)
        if tol is 0:
            self._mc.dll.find_colors(self._cli, byref(ptr), byref(_len),
                    color, *box)
        else:
            self._mc.dll.find_colors_tolerance(self._cli, byref(ptr),
                    byref(_len), color, tol, *box)

        # Construct list
        l = [(ptr[x].x, ptr[x].y) for x in range(_len.value)]

        # Free PPOINT
        self._mc.free(ptr)

        return l
        
    def find_spiral(self, col, box, tol = 0):
        """
        Find a color in a box, searching in the direction of a spiral, with a
            specific tolerance.
        Yields a tuple of x, y values of found color.
        """
        x, y = (c_int(-1), c_int(-1))
        if tol is 0:
            ret = self._mc.dll.find_color_spiral(self._cli, byref(x), byref(y),
                                                 col, *box)
        else:
            ret = self._mc.dll.find_color_spiral_tolerance(self._cli, byref(x),
                                                           byref(y), col, *box,
                                                           tol)
        if ret is RESULT_OK:
            return (x, y)
        elif ret is RESULT_ERROR:
            raise ColorException(self._mc.get_last_error())
        return None
        
    def find_area(self, col, box, min_a, tol = 0):
        """
        Finds a colored area in box with min area min_a with a specific
            tolerance.
        Yields a tuple of x, y values of found area.
        """
        x, y = (c_int(-1), c_int(-1))
        if tol is 0:
            ret = self._mc.dll.find_colored_area(self._cli, byref(x), byref(y),
                                                 col, *box, min_a)
        else:
            ret = self._mc.dll.find_colored_area_tolerance(self._cli, byref(x),
                                                           byref(y), col, *box,
                                                           min_a, tol)
        if ret is RESULT_OK:
            return (x, y)
        elif ret is RESULT_ERROR:
            raise ColorException(self._mc.get_last_error())
        return None
        
    def count_color(self, count, col, box, tol = 0):
        """
        Counts color col in box with tol.
        Yields integer of count.
        """
        count = 0
        if tol is 0:
            ret = self._mc.dll.count_color(self._cli, count, col, *box)
        else:
            ret = self._mc.dll.count_color_tolerance(self._cli, count, col,
                                                     *box, tol)
        if ret is RESULT_OK:
            return count
        elif ret is RESULT_ERROR:
            raise ColorException(self._mc.get_last_error())
        return None
    
    def similar_colors(self, col1, col2, tol = 0):
        """
        Compares col1 and col2 with tol.
        Yields boolean
        """
        ret = self._mc.dll.similar_colors(col1, col2, tol)
        if ret is RESULT_OK:
            return True
        else:
            return False
    
    def set_tolerance_speed(self, ncts = 0):
        """
        Sets CTS to ncts.
        """
        self._mc.dll.set_tolerance_speed(self._cli, ncts)
        
    def get_tolerance_speed(self):
        """
        Gets CTS.
        Yields CTS.
        """
        self._mc.dll.get_tolerance_speed(self._cli, out_cts)
        return out_cts
        
    def set_tolerance_speed_2_modifiers(self, hue = 0, sat = 0):
        """
        Sets CTS2 modifiers with hue, sat.
        """
        self._mc.dll.set_tolerance_speed_2_modifiers(self._cli, hue, sat)
    
    def get_tolerance_speed_2_modifiers(self):
        """
        Gets CTS2 modifiers.
        Yields tuple of hue and sat mods.
        """
        self._mc.dll.get_tolerance_speed_2_modifiers(self._cli, h, s)
        return (h, s)
        
    def _initialise_dll_funcs(self):
        self._mc.dll.find_color.restype = c_int
        self._mc.dll.find_color.argtypes = [c_ulong, PINTEGER, PINTEGER, c_int,
                                            c_int, c_int, c_int, c_int]
        
        self._mc.dll.find_color_tolerance.restype = c_int
        self._mc.dll.find_color_tolerance.argtypes = [c_ulong, PINTEGER,
                                                      PINTEGER, c_int, c_int,
                                                      c_int, c_int, c_int,
                                                      c_int]
        
        self._mc.dll.find_colors.restype = c_int
        self._mc.dll.find_colors.argtypes = [c_ulong, POINTER(PPOINT),
                                             POINTER(c_int), c_int, c_int,
                                             c_int, c_int, c_int]
        
        self._mc.dll.find_colors_tolerance.restype = c_int
        self._mc.dll.find_colors_tolerance.argtypes = [c_ulong,
                                                       POINTER(PPOINT),
                                                       POINTER(c_int), c_int,
                                                       c_int, c_int, c_int,
                                                       c_int, c_int]
        
        self._mc.dll.find_color_spiral.restype = c_int
        self._mc.dll.find_color_spiral.argtypes = [c_ulong, PINTEGER, PINTEGER,
                                                   c_int, c_int, c_int, c_int,
                                                   c_int]
        
        self._mc.dll.find_color_spiral_tolerance.restype = c_int
        self._mc.dll.find_color_spiral_tolerance.argtypes = [c_ulong, PINTEGER,
                                                             PINTEGER, c_int,
                                                             c_int, c_int,
                                                             c_int, c_int,
                                                             c_int]
        
        self._mc.dll.find_colored_area.restype = c_int
        self._mc.dll.find_colored_area.argtypes = [c_ulong, PINTEGER, PINTEGER,
                                                   c_int, c_int, c_int, c_int,
                                                   c_int, c_int]
        
        self._mc.dll.find_colored_area_tolerance.restype = c_int
        self._mc.dll.find_colored_area_tolerance.argtypes = [c_ulong, PINTEGER,
                                                             PINTEGER, c_int,
                                                             c_int, c_int,
                                                             c_int, c_int,
                                                             c_int, c_int]
                                                             
        self._mc.dll.count_color.restype = c_int
        self._mc.dll.count_color.argtypes = [c_ulong, PINTEGER, c_int, c_int,
                                             c_int, c_int, c_int]
                                             
        self._mc.dll.count_color_tolerance.restype = c_int
        self._mc.dll.count_color_tolerance.argtypes = [c_ulong, PINTEGER,
                                                       c_int, c_int, c_int,
                                                       c_int, c_int, c_int]
        
        self._mc.dll.similar_colors.restype = c_int
        self._mc.dll.similar_colors.argtypes = [c_ulong, c_int, c_int, c_int]
        
        self._mc.dll.set_tolerance_speed.restype = c_int
        self._mc.dll.set_tolerance_speed.argtypes = [c_ulong, c_int]
        
        self._mc.dll.get_tolerance_speed.restype = c_int
        self._mc.dll.get_tolerance_speed.argtypes = [c_ulong, PINTEGER]
        
        self._mc.dll.set_tolerance_speed_2_modifiers.restype = c_int
        self._mc.dll.set_tolerance_speed_2_modifiers.argtypes = [c_ulong,
                                                                 c_int, c_int]
                                                                 
        self._mc.dll.get_tolerance_speed_2_modifiers.restype = c_int
        self._mc.dll.get_tolerance_speed_2_modifiers.argtypes = [c_ulong,
                                                                 PINTEGER,
                                                                 PINTEGER]