from ctypes import *

from mtypes import RESULT_OK, RESULT_ERROR

"""
The Window class
----------------

The Window class controls window events.
"""

class WindowException(Exception):
  def __init__(self, err):
    Exception.__init__(self, err)

class Window(object):
  """
    TODO: Information
  """
  def __init__(self, MC, cli):
    self._mc = MC
    self._cli = cli
    self._initialiseDLLFuncs()

  def setDesktopAsClient(self):
    self._setDesktopAsClient()

  def freeTarget(self, idx):
    self._freeTarget(idx)

  def getClientDimensions(self):
    #test_w = test_h = -1
    return self._getClientDimensions()
    #return (test_w, test_h)

  def getClientPosition(self):
    #test_top = test_left = -1
    return self._getClientPosition()
    #return (test_top, test_left)

  def freeze(self):
    self._freeze()

  def unfreeze(self):
    self._unfreeze()

  def activateClient(self):
    self._activateClient()

  def isTargetValid(self):
    return self._isTargetValid()
    #return test

  def getNativeWindow(self):
    test = -1
    test = self._getNativeWindow(test)
    return test

  def _setDesktopAsClient(self):
    ok = self._mc.dll.set_desktop_as_client(self._cli)
    if ok != RESULT_OK:
      pass # error
    return ok

  def _freeTarget(self, idx):
    ok = self._mc.dll.free_target(self._cli)
    if ok != RESULT_OK:
      pass # error
    return ok

  def _getClientDimensions(self):
    _w = _h = c_short()
    ok = self._mc.dll.get_client_dimensions(self._cli, byref(_w), byref(_h))
    if ok != RESULT_OK:
      pass # error
    return (_w.value, _h.value)

  def _getClientPosition(self):
    _top = _left = c_short()
    ok = self._mc.dll.get_client_position(self._cli, byref(_top), byref(_left))
    if ok != RESULT_OK:
      pass # error
    return (_top.value, _left.value)

  def _freeze(self):
    ok = self._mc.dll.freeze(self._cli)
    if ok != RESULT_OK:
      pass # error
    return ok

  def _unfreeze(self):
    ok = self._mc.dll.unfreeze(self._cli)
    if ok != RESULT_OK:
      pass # error
    return ok

  def _activateClient(self):
    ok = self._mc.dll.activate_client(self._cli)
    if ok != RESULT_OK:
      pass # error
    return ok

  def _isTargetValid(self):
    ret = c_bool()
    ok = self._mc.dll.is_target_valid(self._cli, byref(ret))
    if ok != RESULT_OK:
      pass # error
    return ret.value

  def _getNativeWindow(self):
    ret = c_short()
    ok = self._mc.dll.get_native_window(self._cli, byref(ret))
    if ok != RESULT_OK:
      pass # error
    return ret.value

  def _initialiseDLLFuncs(self):
    self._mc.dll.set_desktop_as_client.restype = c_int
    self._mc.dll.set_desktop_as_client.argtypes = [c_int]

    self._mc.dll.free_target.restype = c_int
    self._mc.dll.free_target.argtypes = [c_int, c_int]

    self._mc.dll.get_client_dimensions.restype = c_int
    self._mc.dll.get_client_dimensions.argtypes = [c_int]
    
    self._mc.dll.get_client_position.restype = c_int
    self._mc.dll.get_client_position.argtypes = [c_int]
    
    self._mc.dll.freeze.restype = c_int
    self._mc.dll.freeze.argtypes = [c_int]
    
    self._mc.dll.unfreeze.restype = c_int
    self._mc.dll.unfreeze.argtypes = [c_int]
    
    self._mc.dll.activate_client.restype = c_int
    self._mc.dll.activate_client.argtypes = [c_int]
    
    self._mc.dll.is_target_valid.restype = c_int
    self._mc.dll.is_target_valid.argtypes = [c_int]
    
    self._mc.dll.get_native_window.restype = c_int
    self._mc.dll.get_native_window.argtypes = [c_int]