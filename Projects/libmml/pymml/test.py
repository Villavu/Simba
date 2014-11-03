from mml import MMLCore
from mml import Keyboard
from mml import Window

from ctypes import *
from time import sleep

DLL = MMLCore('../mml.dll')

client = DLL.dll.create_client()

# keyboard = Keyboard(DLL, client)

sleep(1) # To click on skype

# keyboard.keyPress(c_ushort(keyboard.getKeyCode('h').value))
# keyboard.keyPress(c_ushort(keyboard.getKeyCode('i').value))
# keyboard.keyPress(c_ushort(keyboard.getKeyCode(' ').value))

# keyboard.sendKeys(c_char_p('this message brought to you by python via libmml!'), 0, 0)

# print keyboard.getKeyCode('f')
# print keyboard.getKeyCode('f').value
# print c_ushort(keyboard.getKeyCode('f').value)
# print keyboard.isKeyDown(keyboard.getKeyCode('f'))

window = Window(DLL, client)

window.setDesktopAsClient()
window.activateClient() # is this needed? I have no idea.
print "isTargetValid: %s" % str(window.isTargetValid()) # What makes a target valid?
print "dimensions: %s" % str(window.getClientDimensions())  # Why is this (900, 900)?
print "position: %s" % str(window.getClientPosition())

del DLL