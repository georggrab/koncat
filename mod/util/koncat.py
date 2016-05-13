#!/bin/python2.7
# Pythonic Koncat API

import json
from inspect import getmembers, isfunction
from sys import modules

def register_func(fname, desc, args, maxexec="60s", permissions="ANY"):
    opt = ""
    if maxexec:
        opt += "%E=" + maxexec
    if permissions:
        opt += "%P=" + permissions
    return (fname, "%s:[%s](%s)%s" % (fname.func_name, desc, args, opt))

def set_author(author):
    return "$A=%s" % author

def set_desc(desc):
    return "$D=%s" % desc

def _build_hs(hs):
    buffer = ""
    for i in hs:
        if type(i) == tuple:
            continue
        else:
            buffer += i
    buffer += "$F={"
    for i in filter(lambda x: type(x) == tuple, hs):
        buffer += i[1] + ";"
    buffer += "}"
    print buffer

def acquire(hs):
    in0 = raw_input()
    if in0 == 'HELO':
        _build_hs(hs)
    else:
        j = json.loads(raw_input())
        for i in [o for o in getmembers(modules['__main__'], isfunction)]:
            if i[0] in [m[0].func_name for m in filter(lambda x: type(x)==tuple, hs)]:
                if i[0] == in0:
                   i[1](*j['args']) 

#acquire(
#    [ set_author("Awesome Programmer")
#    , set_desc("A Test Module")
#    , register_func(add, "Adds two numbers", "Int Int", maxexec="2s", permissions="ANY") 
#    ]
#)


