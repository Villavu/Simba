from ctypes import *
from mtypes import RESULT_OK, RESULT_FALSE, RESULT_ERROR

# There are several problems with DTM and libMML.
# We could create them in the libMML DTM manager, but we'll have to keep track
# of the DTMs; and if Python garbage collector deletes then, we could try to use
# __del__ to clear it from the DTM Manager.
# 
# Or we could go for a bit of overhead and create the DTM every time we want to
# 'find' it. 

# TMDTMPoint Structure
class _DTMPoint(Structure):
    _fields_ = [('x', c_int), ('y', c_int), ('c', c_int), ('t', c_int), \
            ('asz', c_int), ('bp', c_int)]

class DTMPoint(object):
    def __init__(self, x, y, c, t, asz, bp):
        self.x, self.y, self.c, self.t, self.asz, self.bp = x, y, c, t, asz, bp

class DTMException(Exception):
    def __init__(self, err):
        Exception.__init__(self, err)

class DTM(object):

    def __init__(self, MC, cli, points, name = 'Unnamed DTM'):
        if type(points) not in [list, tuple]:
            raise DTMException('points is not a list or tuple')
        for i in points:
            if type(i) is not DTMPoint:
                raise DTMException('Each point in points should be a DTMPoint')

        self._mc = MC
        self._cli = cli
        self._points = points
        self._name = name

        self._id = -1 # Should we treat each DTM as an int (id) or DTM itself?

    def __del__(self):
        pass

    def __repr__(self):
        return '<DTM: %s>' % self._name

    def to_str(self):
        pass

    def from_str(self):
        pass

    def find(self):
        pass

    def set_name(self, name):
        self._name = name