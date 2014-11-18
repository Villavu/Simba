from mml import MMLCore
from mml import Bitmap
from mml import Mouse

from ctypes import *

DLL = MMLCore('../mml.dll')
client = DLL.dll.create_client()

bitmap = Bitmap(DLL, client)
mouse = Mouse(DLL, client)

i = bitmap.loadBitmap('flickr_test.bmp')

a = bitmap.findBitmap(i)

if (a[2]): # "Found"
  print "bitmap (%d) found at (%d, %d)!" % (i, a[0], a[1])
  mouse.setPos((a[0], a[1]))

bitmap.freeBitmap(i)
print "bitmaps freed!"

del DLL