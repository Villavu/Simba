from ctypes import *
from mmltypes import POINT, PPOINT
from mmltypes import isiterable

class MouseException(Exception):
    def __init__(self, err):
        Exception.__init__(self, err)

# Usage:
class Mouse(object):
    # _mc = MMLCore reference.
    _mc = None
    Left ='Left'
    Right = 'Right'
    Middle = 'Middle'
    Pos = 'Pos'

    # last pointer position
    _lpp = (0, 0)

    def __init__(self, MC):
        '''Initialize the Mouse object'''
        self._mc = MC
        self._initialiseDLLFuncs()
        pass

    def _initialiseDLLFuncs(self):
        self._mc.dll.getMousePos.restype = c_int
        self._mc.dll.getMousePos.argtypes = [PPOINT]

        self._mc.dll.setMousePos.restype = c_int
        self._mc.dll.setMousePos.argtypes = [PPOINT]

        self._mc.dll.getMouseButtonState.restype = c_int
        self._mc.dll.getMouseButtonState.argtypes = [c_int]

        self._mc.dll.setMouseButtonState.restype = c_int
        self._mc.dll.setMouseButtonState.argtypes = [c_int, c_int, c_int, c_int]
        pass

    def __getitem__(self, item):
        if item == self.Pos:
            return self._getMousePos() 
        if isiterable(item):
            for i in item:
                if i not in self._getButtons().keys():
                    raise MouseException('Invalid mouse button')

            bs = [self._buttonToInt(x) for x in item]
            return [self._getMouseButtonState(x) for x in bs]

        raise MouseException('item is not iterable, nor a (valid) string')

    def __setitem__(self, item, value):
        ak = self._getButtons().keys() + [self.Pos]
        isfalse = lambda x: True if not x in ak else False

        for i in map(isfalse, item):
            if i:
                raise MouseException('One of the items is not valid. Items:', item)

        if isiterable(item) and isiterable(value):
            if len(item) != len(value):
                raise MouseException('Not enough values for items')

            for i, v in dict(zip(item, value)).iteritems():
                if i == self.Pos:
                    self._setMousePos(v)
                if i in (self.Left, self.Right, self.Middle):
                    self._setMouseButtonState(self._buttonToInt(i), \
                                              1 if v else 0)
            return
        

        raise MouseException('FIXME')

    # internal functions

    def _getButtons(self):
        return {self.Left : 0, self.Right : 1, self.Middle : 2}

    def _buttonToInt(self, button):
        return self._getButtons()[button]

    def _getMousePos(self):
        ret = POINT()
        ok = self._mc.dll.getMousePos(byref(ret))
        # FIXME: Perhaps use some sort of assertion?
        # We should print dll.last_error is ok != 0

        self._lpp = (ret.x, ret.y)
        return (ret.x, ret.y)

    def _setMousePos(self, p):
        ret = POINT()
        ret.x, ret.y = p
        ok = self._mc.dll.setMousePos(byref(ret))
        return ok

    def _getMouseButtonState(self, button):
        ok = self._mc.dll.getMouseButtonState(button)
        if ok < 0:
            pass #Raise exception
        return ok == 1

    def _setMouseButtonState(self, button, state):
        return self._mc.dll.setMouseButtonState(c_int(button), c_int(state), *map(lambda x: c_int(x), self._getMousePos()))


