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

  def setTargetArray(self, p, w, h):
    return self._setTargetArray(p, w, h)

  def setTargetBitmap(self, bmp):
    return self._setTargetBitmap(bmp)

  def setEIOSTarget(self, name, args):
    return self._setEIOSTarget(name, args)

  def mouseSetClientArea(self, x1, y1, x2, y2):
    return self._mouseSetClientArea(x1, y1, x2, y2)

  def mouseResetClientArea(self):
    self._mouseResetClientArea()

  def imageSetClientArea(self, x1, y1, x2, y2):
    return self._imageSetClientArea(x1, y1, x2, y2)

  def imageResetClientArea(self):
    self._imageResetClientArea()

  def setImageTarget(self):
    return self._setImageTarget()

  def setKeyMouseTarget(self):
    return self._setKeyMouseTarget()

  def getImageTarget(self):
    return self._getImageTarget()

  def getKeyMouseTarget(self):
    return self._getKeyMouseTarget()

  def exportImageTarget(self):
    return self._exportImageTarget()

  def exportKeyMouseTarget(self):
    return self._exportKeyMouseTarget()

  def freeTarget(self, idx):
    self._freeTarget(idx)

  def getClientDimensions(self):
    return self._getClientDimensions()

  def getClientPosition(self):
    return self._getClientPosition()

  def freeze(self):
    self._freeze()

  def unfreeze(self):
    self._unfreeze()

  def activateClient(self):
    self._activateClient()

  def isTargetValid(self):
    return self._isTargetValid()

  def _setDesktopAsClient(self):
    ok = self._mc.dll.set_desktop_as_client(self._cli)
    if ok != RESULT_OK:
      pass # error
    return ok

  def _setTargetArray(self, p, w, h):
    ret = c_short()
    ok = self._mc.dll.set_target_array(self._cli, p, w, h, byref(ret))
    if ok != RESULT_OK:
      pass # error
    return ret.value

  def _setTargetBitmap(self, bmp):
    ret = c_short()
    ok = self._mc.dll.set_target_array(self._cli, bmp, byref(ret))
    if ok != RESULT_OK:
      pass # error
    return ret.value

  def _setEIOSTarget(self, name, args):
    ret = c_short()
    ok = self._mc.dll.set_eios_target(self._cli, name, args, byref(ret))
    if ok != RESULT_OK:
      pass # error
    return ret.value

  def _mouseSetClientArea(self, x1, y1, x2, y2):
    ret = c_bool()
    ok = self._mc.dll.mouse_set_client_area(self._cli, x1, y1, x2, y2, byref(ret))
    if ok != RESULT_OK:
      pass # error
    return ret.value

  def _mouseResetClientArea(self):
    ok = self._mc.dll.mouse_reset_client_area(self._cli)
    if ok != RESULT_OK:
      pass # error
    return ok

  def _imageSetClientArea(self, x1, y1, x2, y2):
    ret = c_bool()
    ok = self._mc.dll.image_set_client_area(self._cli, x1, y1, x2, y2, byref(ret))
    if ok != RESULT_OK:
      pass # error
    return ret.value

  def _imageResetClientArea(self):
    ok = self._mc.dll.image_reset_client_area(self._cli)
    if ok != RESULT_OK:
      pass # error
    return ok

  def _setImageTarget(self):
    ret = c_short()
    ok = self._mc.dll.set_image_target(self._cli, byref(ret))
    if ok != RESULT_OK:
      pass # error
    return ret.value

  def _setKeyMouseTarget(self):
    ret = c_short()
    ok = self._mc.dll.set_key_mouse_target(self._cli, byref(ret))
    if ok != RESULT_OK:
      pass # error
    return ret.value

  def _getImageTarget(self):
    ret = c_short()
    ok = self._mc.dll.get_image_target(self._cli, byref(ret))
    if ok != RESULT_OK:
      pass # error
    return ret.value

  def _getKeyMouseTarget(self):
    ret = c_short()
    ok = self._mc.dll.get_key_mouse_target(self._cli, byref(ret))
    if ok != RESULT_OK:
      pass # error
    return ret.value

# TODO: FIX - TTarget_Exported ???
  def _exportImageTarget(self):
    """
      Currently always returns None.
    """
    return None

    ret = None
    ok = self._mc.dll.export_image_target(self._cli, byref(ret))
    if ok != RESULT_OK:
      pass # error
    return ret.value

# TODO: FIX - TTarget_Exported ???
  def _exportKeyMouseTarget(self):
    """
      Currently always returns None.
    """
    return None

    ret = None
    ok = self._mc.dll.export_key_mouse_target(self._cli, byref(ret))
    if ok != RESULT_OK:
      pass # error
    return ret.value

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