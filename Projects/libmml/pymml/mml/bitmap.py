#!/usr/bin/env python

class Bitmap(object):
    
    def __init__(self):
        pass

    def __getitem__(self, item):
        pass

    def find(self, searchbox = (), tol = 0, frm = (), _type = 'default', client
            = None):
        if iterable(searchbox):
            if len(searchbox) != 4:
                raise Exception("Invalid argument")
        # Hier volgen gewoon de juiste calls

