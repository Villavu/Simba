#!/usr/bin/env python

from ctypes import *
from mtypes import *

"""
The Bitmap class
----------------

The Bitmap class deals with bitmaps.
"""

class BitmapException(Exception):
  def __init__(self, err):
    Exception.__init__(self, err)

class Bitmap(object):
    
    def __init__(self, MC, cli):
        self._mc = MC
        self._cli = cli
        self._initialiseDLLFuncs()

    def __getitem__(self, item):
        pass

    def createBitmapString(self, bmp):
        return self._createBitmapString(bmp)

    def getMufasaBitmap(self, bmp):
        return self._getMufasaBitmap(bmp)

    def createBitmap(self, w, h):
        return self._createBitmap(w, h)

    def freeBitmap(self, bmp):
        self._freeBitmap(bmp)

    def saveBitmap(self, bmp, path):
        self._saveBitmap(bmp, path)

    def bitmapFromString(self, w, h, data):
        return self._bitmapFromString(w, h, data)

    def loadBitmap(self, path):
        return self._loadBitmap(path)

    def setBitmapSize(self, bmp, w, h):
        self._setBitmapSize(bmp, w, h)

    def stretchBitmapResize(self, bmp, w, h):
        self._stretchBitmapResize(bmp, w, h)

    def getBitmapSize(self, bmp):
        return self._getBitmapSize(bmp)

    def setBitmapName(self, bmp, name):
        self._setBitmapName(bmp, name)

    def createMirroredBitmap(self, bmp):
        return self._createMirroredBitmap(bmp)

    # XXX: TBMPMirrorStyle ???
    def createMirroredBitmapEx(self, bmp, ms):
        return self._createMirroredBitmapEx(bmp, ms)

    def fastGetPixel(self, bmp, x, y):
        return self._fastGetPixel(bmp, x, y)

    def fastGetPixels(self, bmp, tpa):
        return self._fastGetPixels(bmp, tpa)

    def getBitmapAreaColors(self, bmp, xs, ys, xe, ye):
        return self._getBitmapAreaColors(bmp, xs, ys, xe, ye)

    def fastSetPixel(self, bmp, x, y, col):
       self._fastSetPixel(bmp, x, y, col)

    def fastSetPixels(self, bmp, tpa, cols):
        self._fastSetPixels(bmp, tpa, cols)

    def drawTPABitmap(self, bmp, tpa, col):
        self._drawTPABitmap(bmp, tpa, col)

    def drawATPABitmap(self, bmp, atpa):
        self._drawATPABitmap(bmp, atpa)

    def drawATPABitmapEx(self, bmp, atpa, cols):
        self._drawATPABitmapEx(bmp, atpa, cols)

    def fastDrawClear(self, bmp, col):
        self._fastDrawClear(bmp, col)

    def drawBitmap(self, bmp, dest, x, y):
        self._drawBitmap(bmp, dest, x, y)

    def fastDrawTransparent(self, x, y, src, tgt):
        self._fastDrawTransparent(x, y, src, tgt)

    def setTransparentColor(self, bmp, col):
        self._setTransparentColor(bmp, col)

    def getTransparentColor(self, bmp):
        return self._getTransparentColor(bmp)

    def fastReplaceColor(self, bmp, old, new):
        self._fastReplaceColor(bmp, old, new)

    def copyClientToBitmap(self, bmp, xs, ys, xe, ye):
        self._copyClientToBitmap(bmp, xs, ys, xe, ye)

    def bitmapFromClient(self, xs, ys, xe, ye):
        return self._bitmapFromClient(xs, ys, xe, ye)

    def findBitmap(self, bmp):
        return self._findBitmap(bmp)

    def findBitmapIn(self, bmp, xs, ys, xe, ye):
        return self._findBitmapIn(bmp, xs, ys, xe, ye)

    def findBitmapToleranceIn(self, bmp, xs, ys, xe, ye, tol):
        return self._findBitmapToleranceIn(bmp, xs, ys, xe, ye, tol)

    def findBitmapSpiral(self, bmp, xs, ys, xe, ye):
        return self._findBitmapSpiral(bmp, xs, ys, xe, ye)

    def findBitmapsSpiralTolerance(self, bmp, xs, ys, xe, ye, tol):
        return self._findBitmapsSpiralTolerance(bmp, xs, ys, xe, ye, tol)

    def findBitmapSpiralTolerance(self, bmp, xs, ys, xe, ye, tol):
        return self._findBitmapSpiralTolerance(bmp, xs, ys, xe, ye, tol)

    def rotateBitmap(self, bmp, angle):
        return self._rotateBitmap(bmp, angle)

    def desaturate(self, bmp):
        return self._desaturate(bmp)

    def invertBitmap(self, bmp):
        self._invertBitmap(bmp)

    def copyBitmap(self, bmp):
        return self._copyBitmap(bmp)

    def greyScaleBitmap(self, bmp):
        return self._greyScaleBitmap(bmp)

    def brightnessBitmap(self, bmp, br):
        return self._brightnessBitmap(bmp, br)

    def contrastBitmap(self, bmp, co):
        return self._contrastBitmap(bmp, co)

    def posterizeBitmap(self, bmp, po):
        return self._posterizeBitmap(bmp, po)

    def createMaskFromBitmap(self, bmp):
        return self._createMaskFromBitmap(bmp)

    def findMaskTolerance(self, mask, xs, ys, xe, ye, tol, contourTol):
        return self._findMaskTolerance(mask, xs, ys, xe, ye, tol, contourTol)

    def findBitmapMaskTolerance(self, mask, xs, ys, xe, ye, tol, contourTol):
        return self._findBitmapMaskTolerance(mask, xs, ys, xe, ye, tol, contourTol)

    def findDeformedBitmapToleranceIn(self, bmp, xs, ys, xe, ye, tol, rangex, partialAcc):
        return self._findDeformedBitmapToleranceIn(bmp, xs, ys, xe, ye, tol, rangex, partialAcc)

    def rectangleBitmap(self, bmp, box, col):
        self._rectangleBitmap(bmp, box, col)

    def floodFillBitmap(self, bmp, start, searchCol, replaceCol):
        self._floodFillBitmap(bmp, start, searchCol, replaceCol)

    def convoluteBitmap(self, bmp, matrix):
        return self._convoluteBitmap(bmp, matrix)

    def calculatePixelShift(self, bmp1, bmp2, compareBox):
        return self._calculatePixelShift(bmp1, bmp2, compareBox)

    def calculatePixelShiftTPA(self, bmp1, bmp2, pts):
        return self._calculatePixelShiftTPA(bmp1, bmp2, pts)

    def calculatePixelTolerance(self, bmp1, bmp2, compareBox, cts):
        return self._calculatePixelTolerance(bmp1, bmp2, compareBox, cts)

    def calculatePixelToleranceTPA(self, bmp1, bmp2, pts, cts):
        return self._calculatePixelToleranceTPA(bmp1, bmp2, pts, cts)

    def bitmapExists(self, bmp):
        return self._bitmapExists(bmp)

    def _createBitmapString(self, bmp):
        ret = c_char_p()
        ok = self._mc.dll.create_bitmap_string(self._cli, bmp, byref(ret))
        if ok != RESULT_OK:
            pass # error
        return ret.value

    def _getMufasaBitmap(self, bmp):
        ret = c_int()
        ok = self._mc.dll.get_mufasa_bitmap(self._cli, bmp, byref(ret))
        if ok != RESULT_OK:
            pass # error
        return ret.value

    def _createBitmap(self, w, h):
        ret = c_int()
        ok = self._mc.dll.create_bitmap(self._cli, w, h, byref(ret))
        if ok != RESULT_OK:
            pass # error
        return ret.value

    def _freeBitmap(self, bmp):
        ok = self._mc.dll.free_bitmap(self._cli, bmp)
        if ok != RESULT_OK:
            pass # error
        return ok

    def _saveBitmap(self, bmp, path):
        ok = self._mc.dll.save_bitmap(self._cli, bmp, path)
        if ok != RESULT_OK:
            pass # error
        return ok

    def _bitmapFromString(self, w, h, data):
        ret = c_int()
        ok = self._mc.dll.bitmap_from_string(self._cli, w, h, data, byref(ret))
        if ok != RESULT_OK:
            pass # error
        return ret.value

    def _loadBitmap(self, path):
        ret = c_int()
        ok = self._mc.dll.load_bitmap(self._cli, path, byref(ret))
        if ok != RESULT_OK:
            pass # error
        return ret.value

    def _setBitmapSize(self, bmp, w, h):
        ok = self._mc.dll.set_bitmap_size(self._cli, bmp, w, h)
        if ok != RESULT_OK:
            pass # error
        return ok

    def _stretchBitmapResize(self, bmp, w, h):
        ok = self._mc.dll.stretch_bitmap_resize(self._cli, bmp, w, h)
        if ok != RESULT_OK:
            pass # error
        return ok

    def _getBitmapSize(self, bmp):
        retW = retH = c_int()
        ok = self._mc.dll.get_bitmap_size(self._cli, bmp, byref(retW), byref(retH))
        if ok != RESULT_OK:
            pass # error
        return (retW.value, retH.value)

    def _setBitmapName(self, bmp, name):
        ok = self._mc.dll.set_bitmap_name(self._cli, bmp, name)
        if ok != RESULT_OK:
            pass # error
        return ok

    def _createMirroredBitmap(self, bmp):
        ret = c_int()
        ok = self._mc.dll.create_mirrored_bitmap(self._cli, bmp, byref(ret))
        if ok != RESULT_OK:
            pass # error
        return ret.value

    def _createMirroredBitmapEx(self, bmp, ms):
        ret = c_int()
        ok = self._mc.dll.create_mirrored_bitmap_ex(self._cli, bmp, ms, byref(ret))
        if ok != RESULT_OK:
            pass # error
        return ret.value

    def _fastGetPixel(self, bmp, x, y):
        ret = c_int()
        ok = self._mc.dll.fast_get_pixel(self._cli, bmp, x, y, byref(ret))
        if ok != RESULT_OK:
            pass # error
        return ret.value

    def _fastGetPixels(self, bmp, tpa):
        ret = None
        ok = self._mc.dll.fast_get_pixels(self._cli, bmp, x, y, byref(ret))
        if ok != RESULT_OK:
            pass # error
        return ret.value

    def _getBitmapAreaColors(self, bmp, xs, ys, xe, ye):
        ret = None
        ok = self._mc.dll.getBitmapAreaColors(self._cli, bmp, xs, ys, xe, ye, byref(ret))
        if ok != RESULT_OK:
            pass # error
        return ret.value

    def _fastSetPixel(self, bmp, x, y, col):
        ok = self._mc.dll.fast_set_pixel(self._cli, bmp, x, y, col)
        if ok != RESULT_OK:
            pass # error
        return ok

    def _fastSetPixels(self, bmp, tpa, cols):
        ok = self._mc.dll.fast_set_pixels(self._cli, bmp, tpa, cols)
        if ok != RESULT_OK:
            pass # error
        return ok

    def _drawTPABitmap(self, bmp, tpa, col):
        ok = self._mc.dll.draw_tpa_bitmap(self._cli, bmp, tpa, col)
        if ok != RESULT_OK:
            pass # error
        return ok

    def _drawATPABitmap(self, bmp, atpa):
        ok = self._mc.dll.draw_atpa_bitmap(self._cli, bmp, atpa)
        if ok != RESULT_OK:
            pass # error
        return ok

    def _drawATPABitmapEx(self, bmp, atpa, cols):
        ok = self._mc.dll.draw_atpa_bitmap_ex(self._cli, bmp, atpa, cols)
        if ok != RESULT_OK:
            pass # error
        return ok

    def _fastDrawClear(self, bmp, col):
        ok = self._mc.dll.fast_draw_clear(self._cli, bmp, col)
        if ok != RESULT_OK:
            pass # error
        return ok

    def _drawBitmap(self, bmp, dest, x, y):
        ok = self._mc.dll.draw_bitmap(self._cli, bmp, dest)
        if ok != RESULT_OK:
            pass # error
        return ok

    def _fastDrawTransparent(self, x, y, src, tgt):
        ok = self._mc.dll.fast_draw_transparent(self._cli, x, y, src, tgt)
        if ok != RESULT_OK:
            pass # error
        return ok

    def _setTransparentColor(self, bmp, col):
        ok = self._mc.dll.set_transparent_color(self._cli, bmp, col)
        if ok != RESULT_OK:
            pass # error
        return ok

    def _getTransparentColor(self, bmp):
        ret = c_int()
        ok = self._mc.dll.get_transparent_color(self._cli, bmp, byref(ret))
        if ok != RESULT_OK:
            pass # error
        return ret.value

    def _fastReplaceColor(self, bmp, old, new):
        ok = self._mc.dll.fast_replace_color(self._cli, bmp, old, new)
        if ok != RESULT_OK:
            pass # error
        return ok

    def _copyClientToBitmap(self, bmp, xs, ys, xe, ye):
        ok = self._mc.dll.copy_client_to_bitmap(self._cli, bmp, xs, ys, xe, ye)
        if ok != RESULT_OK:
            pass # error
        return ok

    def _bitmapFromClient(self, xs, ys, xe, ye):
        ret = c_int()
        ok = self._mc.dll.bitmap_from_client(self._cli, xs, ys, xe, ye, byref(ret))
        if ok != RESULT_OK:
            pass # error
        return ret.value

    def _findBitmap(self, bmp):
        x = c_int()
        y = c_int()
        ret = c_bool()
        ok = self._mc.dll.find_bitmap(self._cli, bmp, byref(x), byref(y), byref(ret))
        if ok != RESULT_OK:
            pass # error
        return (x.value, y.value, ret.value)

    def _findBitmapIn(self, bmp, xs, ys, xe, ye):
        x = c_int()
        y = c_int()
        ret = c_bool()
        ok = self._mc.dll.find_bitmap_in(self._cli, bmp, byref(x), byref(y), xs, ys, xe, ye, byref(ret))
        if ok != RESULT_OK:
            pass # error
        return (x.value, y.value, ret.value)

    def _findBitmapToleranceIn(self, bmp, xs, ys, xe, ye, tol):
        x = c_int()
        y = c_int()
        ret = c_bool()
        ok = self._mc.dll.find_bitmap_tolerance_in(self._cli, bmp, byref(x), byref(y), xs, ys, xe, ye, tol, byref(ret))
        if ok != RESULT_OK:
            pass # error
        return (x.value, y.value, ret.value)

    def _findBitmapSpiral(self, bmp, xs, ys, xe, ye):
        x = c_int()
        y = c_int()
        ret = c_bool()
        ok = self._mc.dll.find_bitmap_spiral(self._cli, bmp, byref(x), byref(y), xs, ys, xe, ye, byref(ret))
        if ok != RESULT_OK:
            pass # error
        return (x.value, y.value, ret.value)

    # TODO: Deal with TPA
    def _findBitmapsSpiralTolerance(self, bmp, xs, ys, xe, ye, tol):
        x = c_int()
        y = c_int()
        pts = PPOINT()
        ret = c_bool()
        ok = self._mc.dll.find_bitmaps_spiral_tolerance(self._cli, bmp, byref(x), byref(y), byref(pts), xs, ys, xe, ye, tol, byref(ret))
        if ok != RESULT_OK:
            pass # error
        return (x.value, y.value, pts, ret.value)

    def _findBitmapSpiralTolerance(self, bmp, xs, ys, xe, ye, tol):
        x = c_int()
        y = c_int()
        ret = c_bool()
        ok = self._mc.dll.find_bitmap_spiral_tolerance(self._cli, bmp, byref(x), byref(y), xs, ys, xe, ye, tol, byref(ret))
        if ok != RESULT_OK:
            pass # error
        return (x.value, y.value, ret.value)

    def _rotateBitmap(self, bmp, angle):
        ret = c_int()
        ok = self._mc.dll.rotate_bitmap(self._cli, bmp, angle, byref(ret))
        if ok != RESULT_OK:
            pass # error
        return ret.value

    def _desaturate(self, bmp):
        ret = c_int()
        ok = self._mc.dll.desaturate(self._cli, bmp, byref(ret))
        if ok != RESULT_OK:
            pass # error
        return ret.value

    # TODO: This currently performs the inversion "inline" and returns nothing
    # but I don't think it should.
    def _invertBitmap(self, bmp):
        ok = self._mc.dll.invert_bitmap(self._cli, bmp)
        if ok != RESULT_OK:
            pass # error
        return ok

    def _copyBitmap(self, bmp):
        ret = c_int()
        ok = self._mc.dll.copy_bitmap(self._cli, bmp, byref(ret))
        if ok != RESULT_OK:
            pass # error
        return ret.value

    def _greyScaleBitmap(self, bmp):
        ret = c_int()
        ok = self._mc.dll.grey_scale_bitmap(self._cli, bmp, byref(ret))
        if ok != RESULT_OK:
            pass # error
        return ret.value

    def _brightnessBitmap(self, bmp, br):
        ret = c_int()
        ok = self._mc.dll.brightness_bitmap(self._cli, bmp, br, byref(ret))
        if ok != RESULT_OK:
            pass # error
        return ret.value

    def _contrastBitmap(self, bmp, co):
        ret = c_int()
        ok = self._mc.dll.contrast_bitmap(self._cli, bmp, co, byref(ret))
        if ok != RESULT_OK:
            pass # error
        return ret.value

    def _posterizeBitmap(self, bmp, po):
        ret = c_int()
        ok = self._mc.dll.posterize_bitmap(self._cli, bmp, po, byref(ret))
        if ok != RESULT_OK:
            pass # error
        return ret.value

    # TODO: create_mask_from_bitmap returns a TMask.
    def _createMaskFromBitmap(self, bmp):
        ret = None
        ok = self._mc.dll.create_mask_from_bitmap(self._cli, bmp, byref(ret))
        if ok != RESULT_OK:
            pass # error
        return ret.value

    # TODO: find_mask_tolerance takes a TMask.
    def _findMaskTolerance(self, mask, xs, ys, xe, ye, tol, contourTol):
        x = c_int()
        y = c_int()
        ret = c_bool()
        ok = self._mc.dll.find_mask_tolerance(self._cli, mask, byref(x), byref(y), xs, ys, xe, ye, tol, contourTol, byref(ret))
        if ok != RESULT_OK:
            pass # error
        return (x.value, y.value, ret.value)

    def _findBitmapMaskTolerance(self, mask, xs, ys, xe, ye, tol, contourTol):
        x = c_int()
        y = c_int()
        ret = c_bool()
        ok = self._mc.dll.find_bitmap_mask_tolerance(self._cli, mask, byref(x), byref(y), xs, ys, xe, ye, tol, contourTol, byref(ret))
        if ok != RESULT_OK:
            pass # error
        return (x.value, y.value, ret.value)

    def _findDeformedBitmapToleranceIn(self, bmp, xs, ys, xe, ye, tol, rangex, partialAcc):
        x = c_int()
        y = c_int()
        acc = c_longdouble()
        ret = c_bool()
        ok = self._mc.dll.find_deformed_bitmap_tolerance_in(self._cli, bmp, byref(x), byref(y), xs, ys, xe, ye, tol, rangex, partialAcc, byref(acc), byref(ret))
        if ok != RESULT_OK:
            pass # error
        return (x.value, y.value, acc.value, ret.value)

    def _rectangleBitmap(self, bmp, box, col):
        ok = self._mc.dll.rectangle_bitmap(self._cli, bmp, box, col)
        if ok != RESULT_OK:
            pass # error
        return ok

    def _floodFillBitmap(self, bmp, start, searchCol, replaceCol):
        ok = self._mc.dll.flood_fill_bitmap(self._cli, bmp, start, searchCol, replaceCol)
        if ok != RESULT_OK:
            pass # error
        return ok

    def _convoluteBitmap(self, bmp, matrix):
        ret = c_int()
        ok = self._mc.dll.convolute_bitmap(self._cli, bmp, matrix, byref(ret))
        if ok != RESULT_OK:
            pass # error
        return ok

    def _calculatePixelShift(self, bmp1, bmp2, compareBox):
        ret = c_int()
        ok = self._mc.dll.calculate_pixel_shift(self._cli, bmp1, bmp2, compareBox, byref(ret))
        if ok != RESULT_OK:
            pass # error
        return ret.value

    def _calculatePixelShiftTPA(self, bmp1, bmp2, pts):
        ret = c_int()
        ok = self._mc.dll.calculate_pixel_shift_tpa(self._cli, bmp1, bmp2, pts, byref(ret))
        if ok != RESULT_OK:
            pass # error
        return ret.value

    def _calculatePixelTolerance(self, bmp1, bmp2, compareBox, cts):
        ret = c_longdouble()
        ok = self._mc.dll.calculate_pixel_tolerance(self._cli, bmp1, bmp2, compareBox, cts, byref(ret))
        if ok != RESULT_OK:
            pass # error
        return ret.value

    def _calculatePixelToleranceTPA(self, bmp1, bmp2, pts, cts):
        ret = c_longdouble()
        ok = self._mc.dll.calculate_pixel_tolerance_tpa(self._cli, bmp1, bmp2, pts, cts, byref(ret))
        if ok != RESULT_OK:
            pass # error
        return ret.value

    def _bitmapExists(self, bmp):
        ret = c_bool()
        ok = self._mc.dll.bitmap_exists(self._cli, bmp, byref(ret))
        if ok != RESULT_OK:
            pass # error
        return ret.value

    def _initialiseDLLFuncs(self):
        self._mc.dll.create_bitmap_string.restype = c_int
        self._mc.dll.create_bitmap_string.argtypes = [c_ulong, c_int]

        self._mc.dll.get_mufasa_bitmap.restype = c_int
        self._mc.dll.get_mufasa_bitmap.argtypes = [c_ulong, c_int]

        self._mc.dll.create_bitmap.restype = c_int
        self._mc.dll.create_bitmap.argtypes = [c_ulong, c_int, c_int]

        self._mc.dll.free_bitmap.restype = c_int
        self._mc.dll.free_bitmap.argtypes = [c_ulong, c_int]

        self._mc.dll.save_bitmap.restype = c_int
        self._mc.dll.save_bitmap.argtypes = [c_ulong, c_int, c_char_p]

        self._mc.dll.bitmap_from_string.restype = c_int
        self._mc.dll.bitmap_from_string.argtypes = [c_ulong, c_int, c_int, c_char_p]

        self._mc.dll.load_bitmap.restype = c_int
        self._mc.dll.load_bitmap.argtypes = [c_ulong, c_char_p]

        self._mc.dll.set_bitmap_size.restype = c_int
        self._mc.dll.set_bitmap_size.argtypes = [c_ulong, c_int, c_int, c_int]

        self._mc.dll.stretch_bitmap_resize.restype = c_int
        self._mc.dll.stretch_bitmap_resize.argtypes = [c_ulong, c_int, c_int, c_int]

        self._mc.dll.get_bitmap_size.restype = c_int
        self._mc.dll.get_bitmap_size.argtypes = [c_ulong, c_int]

        self._mc.dll.set_bitmap_name.restype = c_int
        self._mc.dll.set_bitmap_name.argtypes = [c_ulong, c_int, c_char_p]

        self._mc.dll.create_mirrored_bitmap.restype = c_int
        self._mc.dll.create_mirrored_bitmap.argtypes = [c_ulong, c_int]

        self._mc.dll.create_mirrored_bitmap_ex.restype = c_int
        self._mc.dll.create_mirrored_bitmap_ex.argtypes = [c_ulong, c_int, c_void_p]

        self._mc.dll.fast_get_pixel.restype = c_int
        self._mc.dll.fast_get_pixel.argtypes = [c_ulong, c_int, c_int, c_int]

        self._mc.dll.fast_get_pixels.restype = c_int
        self._mc.dll.fast_get_pixels.argtypes = [c_ulong, c_int, c_void_p]

        self._mc.dll.get_bitmap_area_colors.restype = c_int
        self._mc.dll.get_bitmap_area_colors.argtypes = [c_ulong, c_int, c_int, c_int, c_int, c_int]

        self._mc.dll.fast_set_pixel.restype = c_int
        self._mc.dll.fast_set_pixel.argtypes = [c_ulong, c_int, c_int, c_int, c_int]

        self._mc.dll.fast_set_pixels.restype = c_int
        self._mc.dll.fast_set_pixels.argtypes = [c_ulong, c_int, c_void_p, c_void_p]

        self._mc.dll.draw_tpa_bitmap.restype = c_int
        self._mc.dll.draw_tpa_bitmap.argtypes = [c_ulong, c_int, c_void_p, c_int]

        self._mc.dll.draw_atpa_bitmap.restype = c_int
        self._mc.dll.draw_atpa_bitmap.argtypes = [c_ulong, c_int, c_void_p]

        self._mc.dll.draw_atpa_bitmap_ex.restype = c_int
        self._mc.dll.draw_atpa_bitmap_ex.argtypes = [c_ulong, c_int, c_void_p, c_void_p]

        self._mc.dll.fast_draw_clear.restype = c_int
        self._mc.dll.fast_draw_clear.argtypes = [c_ulong, c_int, c_int]

        self._mc.dll.draw_bitmap.restype = c_int
        self._mc.dll.draw_bitmap.argtypes = [c_ulong, c_int, c_int, c_int, c_int]

        self._mc.dll.fast_draw_transparent.restype = c_int
        self._mc.dll.fast_draw_transparent.argtypes = [c_ulong, c_int, c_int, c_int, c_int]

        self._mc.dll.set_transparent_color.restype = c_int
        self._mc.dll.set_transparent_color.argtypes = [c_ulong, c_int, c_int]

        self._mc.dll.get_transparent_color.restype = c_int
        self._mc.dll.get_transparent_color.argtypes = [c_ulong, c_int]

        self._mc.dll.fast_replace_color.restype = c_int
        self._mc.dll.fast_replace_color.argtypes = [c_ulong, c_int, c_int, c_int]

        self._mc.dll.copy_client_to_bitmap.restype = c_int
        self._mc.dll.copy_client_to_bitmap.argtypes = [c_ulong, c_int, c_int, c_int, c_int, c_int]

        self._mc.dll.bitmap_from_client.restype = c_int
        self._mc.dll.bitmap_from_client.argtypes = [c_ulong, c_int, c_int, c_int, c_int]

        self._mc.dll.find_bitmap.restype = c_int
        self._mc.dll.find_bitmap.argtypes = [c_ulong, c_int]

        self._mc.dll.find_bitmap_in.restype = c_int
        self._mc.dll.find_bitmap_in.argtypes = [c_ulong, c_int, c_int, c_int, c_int, c_int]

        self._mc.dll.find_bitmap_tolerance_in.restype = c_int
        self._mc.dll.find_bitmap_tolerance_in.argtypes = [c_ulong, c_int, c_int, c_int, c_int, c_int, c_int]

        self._mc.dll.find_bitmap_spiral.restype = c_int
        self._mc.dll.find_bitmap_spiral.argtypes = [c_ulong, c_int, c_int, c_int, c_int, c_int]

        self._mc.dll.find_bitmaps_spiral_tolerance.restype = c_int
        self._mc.dll.find_bitmaps_spiral_tolerance.argtypes = [c_ulong, c_int, c_int, c_int, c_int, c_int, c_int]

        self._mc.dll.find_bitmap_spiral_tolerance.restype = c_int
        self._mc.dll.find_bitmap_spiral_tolerance.argtypes = [c_ulong, c_int, c_int, c_int, c_int, c_int, c_int]

        self._mc.dll.rotate_bitmap.restype = c_int
        self._mc.dll.rotate_bitmap.argtypes = [c_ulong, c_int, c_int]

        self._mc.dll.desaturate.restype = c_int
        self._mc.dll.desaturate.argtypes = [c_ulong, c_int]

        self._mc.dll.invert_bitmap.restype = c_int
        self._mc.dll.invert_bitmap.argtypes = [c_ulong, c_int]

        self._mc.dll.copy_bitmap.restype = c_int
        self._mc.dll.copy_bitmap.argtypes = [c_ulong, c_int]

        self._mc.dll.grey_scale_bitmap.restype = c_int
        self._mc.dll.grey_scale_bitmap.argtypes = [c_ulong, c_int]

        self._mc.dll.brightness_bitmap.restype = c_int
        self._mc.dll.brightness_bitmap.argtypes = [c_ulong, c_int, c_int]

        self._mc.dll.contrast_bitmap.restype = c_int
        self._mc.dll.contrast_bitmap.argtypes = [c_ulong, c_int, c_int]

        self._mc.dll.posterize_bitmap.restype = c_int
        self._mc.dll.posterize_bitmap.argtypes = [c_ulong, c_int, c_int]

        self._mc.dll.create_mask_from_bitmap.restype = c_int
        self._mc.dll.create_mask_from_bitmap.argtypes = [c_ulong, c_int]

        self._mc.dll.find_mask_tolerance.restype = c_int
        self._mc.dll.find_mask_tolerance.argtypes = [c_ulong, c_void_p, c_int, c_int, c_int, c_int, c_int, c_int]

        self._mc.dll.find_bitmap_mask_tolerance.restype = c_int
        self._mc.dll.find_bitmap_mask_tolerance.argtypes = [c_ulong, c_int, c_int, c_int, c_int, c_int, c_int, c_int]

        self._mc.dll.find_deformed_bitmap_tolerance_in.restype = c_int
        self._mc.dll.find_deformed_bitmap_tolerance_in.argtypes = [c_ulong, c_int, c_int, c_int, c_int, c_int, c_int, c_int, c_bool]

        self._mc.dll.rectangle_bitmap.restype = c_int
        self._mc.dll.rectangle_bitmap.argtypes = [c_ulong, c_int, c_void_p, c_int]

        self._mc.dll.flood_fill_bitmap.restype = c_int
        self._mc.dll.flood_fill_bitmap.argtypes = [c_ulong, c_int, POINT, c_int, c_int]

        self._mc.dll.convolute_bitmap.restype = c_int
        self._mc.dll.convolute_bitmap.argtypes = [c_ulong, c_int, c_void_p]

        self._mc.dll.calculate_pixel_shift.restype = c_int
        self._mc.dll.calculate_pixel_shift.argtypes = [c_ulong, c_int, c_int, c_void_p]

        self._mc.dll.calculate_pixel_shift_tpa.restype = c_int
        self._mc.dll.calculate_pixel_shift_tpa.argtypes = [c_ulong, c_int, c_int, c_void_p]

        self._mc.dll.calculate_pixel_tolerance.restype = c_int
        self._mc.dll.calculate_pixel_tolerance.argtypes = [c_ulong, c_int, c_int, c_void_p, c_int]

        self._mc.dll.calculate_pixel_tolerance_tpa.restype = c_int
        self._mc.dll.calculate_pixel_tolerance_tpa.argtypes = [c_ulong, c_int, c_int, c_void_p, c_int]

        self._mc.dll.bitmap_exists.restype = c_int
        self._mc.dll.bitmap_exists.argtypes = [c_ulong, c_int]

    # TODO: Use something nice and pythonic like this instead of my gross function calls.
    def find(self, searchbox = (), tol = 0, frm = (), _type = 'default', client
            = None):
        if iterable(searchbox):
            if len(searchbox) != 4:
                raise Exception("Invalid argument")
        # Hier volgen gewoon de juiste calls

