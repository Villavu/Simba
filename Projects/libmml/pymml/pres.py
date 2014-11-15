#!/usr/bin/env python
from mml import MMLCore
from mml import Mouse
from mml import Color
import sys
import time

from mml import mtypes

DLL = MMLCore('../mml.dll')

client = DLL.dll.create_client()

m = Mouse(DLL, client)

while False: 
    print('Readline again')
    cmd = sys.stdin.readline()[:-1].lower()
    if cmd is '':
        print('Stopping...')
        m[(Mouse.Left, Mouse.Right, Mouse.Middle)] = [False for x in range(3)]
        for v in zip((Mouse.Left, Mouse.Right), m[(Mouse.Left, Mouse.Right)]):
            print(v)
        break

    print('Doing:', cmd)

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
        print(v)


c = Color(DLL, client)

print c.get(mtypes.POINT(500, 500))

del DLL
