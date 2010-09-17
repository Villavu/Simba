#!/usr/bin/env python

class Bitmap(object):
    
    # Pixels, alleen ter illustratie atm
    pixels = None

    # Index van de managed bitmaps
    _index = None
    def __init__(self):
        self.pixels = [range(100) for x in range(100)]
        pass

    def __getitem__(self, item):
        if iterable(item):
            return self.pixels[item[0]][item[1]]

    def find(self, searchbox = (), tol = 0, frm = (), _type = 'default', client
            = None):
        if iterable(searchbox):
            if len(searchbox) != 4:
                raise Exception("Invalid argument")
        # Hier volgen gewoon de juiste calls

