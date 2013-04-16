import re
from sys import argv

files = argv[1:]

commentregex = re.compile('\(\*.+?\*\)', re.DOTALL)

for file in files:
    print file
    f = open(file)
    p = file.rfind('/')
    filetrim = file[p+1:]
    p = filetrim.rfind('.pas')
    filetrim2 = filetrim[:p]

    o = open('sphinx/mmlref/%s.rst' % filetrim2, 'w+')
    c = ''.join([x for x in f])
    res = commentregex.findall(c)
    for y in res:
        o.write(y[2:][:-2])
    o.close()
    f.close()
