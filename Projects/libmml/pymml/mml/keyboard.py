from ctypes import *

from mtypes import *

"""
The Keyboard class
------------------

The Keyboard class controls keyboard events.
"""

class KeyboardException(Exception):
  def __init__(self, err):
    Exception.__init__(self, err)

# Usage:
class Keyboard(object):
  """
      The MML Keyboard object communicates directly with libmml, but wraps it
      around a nice and easy to use layer.

      It will allow several ways to use the keyboard.
  """
  def __init__(self, MC, cli):
    """ Initialize the Keyboard object. Needs a DLL Mufasa Core object
        (which contains the dll reference.)"""
    self._mc = MC
    self._cli = cli
    self._initialiseDLLFuncs()

  def keyDown(self, key):
    """
        Press key key down.
    """
    self._keyDown(key)

  def keyUp(self, key):
    """
        Release key key up.
    """
    self._keyUp(key)

  def sendKeys(self, s, w, mw):
    """
        Send keys s with wait w +/- mw.
    """
    self._sendKeys(s, w, mw)

  def keyPress(self, key):
    """
        Press key key.
    """
    self._keyPress(key)

  def isKeyDown(self, key):
    """
        Returns true if key key is down.
    """
    return self._isKeyDown(key)

  def getKeyCode(self, key):
    """
        Get the keycode for key.
    """
    return self._getKeyCode(key)

  def _keyDown(self, key):
    ok = self._mc.dll.key_down(self._cli, key)
    if ok != RESULT_OK:
      pass # Exception
    return ok

  def _keyUp(self, key):
    ok = self._mc.dll.key_up(self._cli, key)
    if ok != RESULT_OK:
      pass # Exception
    return ok

  def _sendKeys(self, s, w, mw):
    ok = self._mc.dll.send_keys(self._cli, s, w, mw)
    if ok != RESULT_OK:
      pass # Exception
    return ok

  def _keyPress(self, key):
    ok = self._mc.dll.key_press(self._cli, key)
    if ok != RESULT_OK:
      pass # Exception
    return ok

  def _isKeyDown(self, key):
    ret = c_bool()
    ok = self._mc.dll.is_key_down(self._cli, key, byref(ret))
    if ok != RESULT_OK:
      pass # Exception
    return ret.value

  def _getKeyCode(self, key):
    ret = c_ushort()
    ok = self._mc.dll.get_key_code(self._cli, key, byref(ret))
    
    if ok != RESULT_OK:
      pass # Exception
    return ret

  def _initialiseDLLFuncs(self): # I don't think c_char is the best option `key`; c_ushort?
    """ Define all keyboard related DLL calls. """
    self._mc.dll.key_down.restype = c_int
    self._mc.dll.key_down.argtypes = [c_ulong, c_char]

    self._mc.dll.key_up.restype = c_int
    self._mc.dll.key_up.argtypes = [c_ulong, c_char]

    self._mc.dll.send_keys.restype = c_int
    self._mc.dll.send_keys.argtypes = [c_ulong, c_char_p, c_int, c_int]

    self._mc.dll.key_press.restype = c_int
    self._mc.dll.key_press.argtypes = [c_ulong, c_ushort]

    self._mc.dll.is_key_down.restype = c_int
    self._mc.dll.is_key_down.argtypes = [c_ulong, c_ushort]

    self._mc.dll.get_key_code.restype = c_int
    self._mc.dll.get_key_code.argtypes = [c_ulong, c_char]