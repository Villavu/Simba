from ctypes import *
from mml import 
from mmltypes import POINT

# Usage:
class Mouse(object):
    # _mc = MMLCore reference.
    _mc = None

    # last pointer position
    _lpp = (0, 0)

    def __init__(self, MC):
        '''Initialize the Mouse object'''
        self._mc = MC
        self._initialiseDLLFuncs()
        pass

    def _initialiseDLLFuncs(self):
        self._mc.dll.getmousepos.restype = c_int
        self._mc.dll.getmousepos.argtypes = [PPOINT]
        pass

    # Will be used to get the states of the mouse
    def __getitem__(self, item):
        pass

    # Will be used to set states of the mouse
    def __setitem__(self, item, value):
        pass

    # internal function
    def _getMousePos(self):
        ret = POINT()
        ok = self._mc.dll.getmousepos(byref(ret))
        # FIXME: Perhaps use some sort of assertion?
        # We should print dll.last_error is ok != 0

        self._lpp = (ret.x, ret.y)
        return (ret.x, ret.y)

    # internal function
    def _getMouseButtonState(self, button):
        pass
