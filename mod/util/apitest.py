#!/bin/python2.7

import koncat

def subtract(a,b):
    print a - b

def getInput():
    print 'Entering'
    print '\\askforinput{Please enter something}'
    print 'Now waiting'
    resp = raw_input()
    print "You entered: " + resp

def tryjoin(channel):
    print '\\joinchannel{%s}' % channel
    print 'OK, joined channel!'

if __name__ == '__main__':
    koncat.acquire(
        [ koncat.set_author("Some Guy")
        , koncat.set_desc("A Module for testing the Koncat API")  
        , koncat.register_func(subtract, "Subtracts two numbers", "Int Int", maxexec="2s", permissions="ANY")
        , koncat.register_func(getInput, "Tests the askforinput API function", "")
        , koncat.register_func(tryjoin, "Join a Channel", "String")
        ]
    )
