#!/usr/bin/env python
from mml import  MMLCore
from mmlmouse import Mouse
import sys
import time

DLL = MMLCore('../libmml.so')

m = Mouse(DLL)

while True: 
    print 'Readline again'
    cmd = sys.stdin.readline()[:-1].lower()
    if cmd is '':
        print 'Stopping...'
        m[(Mouse.Left, Mouse.Right, Mouse.Middle)] = [False for x in range(3)]
        for v in zip((Mouse.Left, Mouse.Right), m[(Mouse.Left, Mouse.Right)]):
            print v
        break

    print 'Doing:', cmd

    if cmd in ['left', 'l']:
        m[Mouse.Left] = True
        time.sleep(0.1)
        m[Mouse.Left] = False
        time.sleep(0.1)
    elif cmd in ['right','r']:
        m[Mouse.Right] = True
        time.sleep(0.1)
        m[Mouse.Right] = False
        time.sleep(0.1)

    for v in zip((Mouse.Left, Mouse.Right), m[(Mouse.Left, Mouse.Right)]):
        print v

    
del DLL
