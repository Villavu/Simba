from mml import MMLCore
from mml import Keyboard
from mml import Window
from mml import Bitmap
from mml import Mouse

from ctypes import *
from time import sleep
import os

DLL = MMLCore('../mml.dll')

client = DLL.dll.create_client()

# keyboard = Keyboard(DLL, client)

#sleep(1)

# keyboard.keyPress(c_ushort(keyboard.getKeyCode('h').value))
# keyboard.keyPress(c_ushort(keyboard.getKeyCode('i').value))
# keyboard.keyPress(c_ushort(keyboard.getKeyCode(' ').value))

# keyboard.sendKeys(c_char_p('this message brought to you by python via libmml!'), 0, 0)

# print keyboard.getKeyCode('f')
# print keyboard.getKeyCode('f').value
# print c_ushort(keyboard.getKeyCode('f').value)
# print keyboard.isKeyDown(keyboard.getKeyCode('f'))

# window = Window(DLL, client)

# window.setDesktopAsClient()
# window.activateClient() # is this needed? I have no idea.
# print "isTargetValid: %s" % str(window.isTargetValid()) # What makes a target valid?
# print "dimensions: %s" % str(window.getClientDimensions())  # Why is this (900, 900)?
# print "position: %s" % str(window.getClientPosition())

bitmap = Bitmap(DLL, client)
mouse = Mouse(DLL, client)

i = bitmap.loadBitmap('bmp_test.bmp')

# bitmap.saveBitmap(i, c_char_p('test.bmp'))
# print "bitmap saved!"

a = bitmap.findBitmap(i)

if (a[2]): # "Found"
  print "bitmap (%d) found at (%d, %d)!" % (i, a[0], a[1])
  mouse.setPos((a[0], a[1]))

bitmap.freeBitmap(i)
print "bitmaps freed!"

del DLL