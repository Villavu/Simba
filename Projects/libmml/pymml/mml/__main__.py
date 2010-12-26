from __init__ import *

if __name__ == '__main__':
    DLL = MMLCore('../libmml.so')

    client = DLL.dll.create_client()
    print 'Python Client: %d' % client
    if client in (0, 1):
        raise Exception('Could create a client');

    c = Color(DLL, client)


    ret = c.find((0, 0, 100, 100), 0)
    print ret

    ret = c.find_all((0, 0, 100, 100), 0, tol=100)
    print ret

    m = Mouse(DLL, client)
    
   
    print m[(Mouse.Pos, Mouse.Left, Mouse.Right)]
    m[(Mouse.Pos, Mouse.Right)] = ((300,300), True)

    print m.getButtonStates()
    sleep(0.5)
    m.setPos((200,200))
   
    sleep(2)
    print 'Done'

    m[(Mouse.Left, Mouse.Right, Mouse.Middle)] = [False for x in range(3)]
    for v in zip((Mouse.Left, Mouse.Right), m[(Mouse.Left, Mouse.Right)]):
        print v
    print m.getPos()
    
    del DLL
