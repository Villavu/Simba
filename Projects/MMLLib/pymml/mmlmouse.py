from ctypes import *
from mmltypes import POINT, PPOINT
from mmltypes import isiterable

class MouseException(Exception):
    def __init__(self, err):
        Exception.__init__(self, err)

# Usage:
class Mouse(object):
    """
        The MML Mouse object communicates directly with libmml,
        but wraps it around a nice and easy to use layer.
        It will allow several ways to set mouse positions and
        buttons. __getitem__ and __setitem__ are also implemented,
        so one can access mouse buttons states and positions with [].
    """
    # _mc = MMLCore reference.
    _mc = None
    Left, Right, Middle, Pos = 'Left', 'Right', 'Middle', 'Pos'

    # last mouse pointer position
    _lpp = (0, 0)

    def __init__(self, MC):
        """ Initialize the Mouse object. Needs a DLL Mufasa Core object 
            (which contains the dll reference.)"""
        self._mc = MC
        self._initialiseDLLFuncs()
        pass
    
    def setPos(self, pos):
        """
            Set the mouse position to the tuple _pos_.
        """
        return self.__setitem__(Mouse.Pos, pos)

    def getPos(self):
        """
            Get the current mouse position as a tuple.
        """
        return self._getMousePos()

    def getButtonStates(self):
        """
            Get the current states of the mouse buttons.
        """
        return zip(self._getButtons().keys(), \
                    self.__getitem__(self._getButtons().keys()))

    def setButtonState(self, button, downup):
        """
            Set the states of the mouse buttons.
        """
        return self.__setitem__(button, downup)

    def __getitem__(self, item):
        """Can currently return the state of mouse buttons as well as the
            mouse position. Supports iterable arguments"""
        if isiterable(item):
            res = []
            for i in item:
                if i == self.Pos:
                    res.append(self._getMousePos())
                elif i in self._getButtons().keys():
                    res.append(self._getMouseButtonState(self._buttonToInt(i)))
                else:
                    raise MouseException('Invalid mouse button')
            return res

        else:
            if item == self.Pos:
                return self._getMousePos() 
            if item in self._getButtons().keys():
                return self._getMouseButtonState(self_buttonToInt(item))

        raise MouseException('item is not iterable nor a (valid) string')

    def __setitem__(self, item, value):
        """Can currently set the state of mouse buttons as well as the
            mouse position. Supports iterable arguments"""
        ak = self._getButtons().keys() + [self.Pos]

        if isiterable(item) and isiterable(value):
            isfalse = lambda x: True if not x in ak else False
            for i in map(isfalse, item):
                if i:
                    raise MouseException('One of the items is not valid. Items:', item)
            if len(item) != len(value):
                raise MouseException('Not enough values for items')

            for i, v in dict(zip(item, value)).iteritems():
                if i == self.Pos:
                    self._setMousePos(v)
                elif i in self._getButtons().keys():
                    self._setMouseButtonState(self._buttonToInt(i), \
                                              1 if v else 0)
            return
        else:
            if item in ak:
                if item == self.Pos:
                    self._setMousePos(value)
                elif item in self._getButtons().keys():
                    self._setMouseButtonState(self._buttonToInt(item), \
                                              1 if value else 0)
                return
            else:
                raise MouseException('Invalid item / value')

                
            
        raise MouseException('FIXME')

    # Tools
    def _getButtons(self):
        """Return mouse buttons with their corresponding button DLL number as dict"""
        return {self.Left : 0, self.Right : 1, self.Middle : 2}

    def _buttonToInt(self, button):
        """Return button number for button"""
        return self._getButtons()[button]
    

    # Internal DLL stuff
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
        if ok != 0:
            pass # Raise exception
        self._lpp = (ret.x, ret.y)
        return ok

    def _getMouseButtonState(self, button):
        ok = self._mc.dll.getMouseButtonState(button)
        if ok < 0:
            pass #Raise exception
        return ok == 1

    def _setMouseButtonState(self, button, state):
        ok = self._mc.dll.setMouseButtonState(c_int(button), c_int(state), 
                *map(lambda x: c_int(x), self._getMousePos()))
        if ok != 0:
            pass # Raise exception
        return ok

    def _initialiseDLLFuncs(self):
        """Define all mouse related DLL-calls"""
        self._mc.dll.getMousePos.restype = c_int
        self._mc.dll.getMousePos.argtypes = [PPOINT]

        self._mc.dll.setMousePos.restype = c_int
        self._mc.dll.setMousePos.argtypes = [PPOINT]

        self._mc.dll.getMouseButtonState.restype = c_int
        self._mc.dll.getMouseButtonState.argtypes = [c_int]

        self._mc.dll.setMouseButtonState.restype = c_int
        self._mc.dll.setMouseButtonState.argtypes = [c_int, c_int, c_int, c_int]
        pass

