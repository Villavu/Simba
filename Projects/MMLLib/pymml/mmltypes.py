from ctypes import *

RESULT_OK, RESULT_FALSE, RESULT_ERROR = (0, 1, -1)

class POINT(Structure):
    """
        POINT represents a point with two variables, x and y.
        POINT is mainly used for Python <-> MML communication.
    """
    _fields_ = [('x', c_int),
                ('y', c_int)]

class PascalArray(object):
    """
        PascalArray is a class that allows one to easily use a Pascal-style
        array. It has been changed to fit my own Pascal-style arrays. (The
        length is no longer stored at -1, but at 0, and the data starts at 1.)
        This makes freeing the data much easier.

        The implementation is limited to reading and writing data.
        It cannot resize arrays nor can it create them.

        This class is more like a temporary solution to passing arrays and such.
        The actual user should not be bothered by the external memory, so most
        likely we will simply turn this data into python lists. The only
        drawback would be the overhead created by doing so.
    """
    def __init__(self, pastype, ptr, MC):
        """
            Set the type of the data we are holding to _pastype_,
            save the pointer _ptr_ and store the reference to the MMLCore.
        """
        self._type = pastype
        self._p = ptr
        self._mc = MC

    def __del__(self):
        """
            Free the array. Perhaps we should do reference counting on the
            pointer?
        """
        self._mc.dll.fpc_freemem_(self._p)

    def __len__(self):
        """
            Return the length of the array.
        """
        return cast(self._p, POINTER(c_ulong))[0]

    def __getitem__(self, pos):
        """
            Get an item at a specific position _pos_.
        """
        if pos > len(self):
            print 'Out of range'
            return None
        return cast(self._p, POINTER(self._type))[pos+1]
    
    def __setitem__(self, pos, item):
        """
            Set an item at a specific position _pos_.
        """
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
