#!/usr/bin/python2.7
# encoding=latin-1

#\ $ echo "HELO" | ./util/test.py
#\ $D=Test Module$F={some_func:[Some Function],other_func:[A Function printing "Hello World!"]}

import json

def dispatch(what):
    if what == 'HELO':
        send_handshake()
    else:
        meta = json.loads(raw_input())
        if what == 'helloworld':
            helloworld(meta['args'])
        elif what == 'add':
            doadding(meta['args'][0], meta['args'][1])
        elif what == 'testbool':
            testbool(meta['args'][0])


def helloworld(args):
    for i in args:
        print i[::-1]

def doadding(a, b):
    print a + b

def testbool(b):
    if b:
        print "It is True!"
    else:
        print "It is False!"

def send_handshake():
    print "$D=A Test Module" \
         +"$F={helloworld:[A Hello World Function](String)%P=Registered;add:[Add two numbers](Int Int)%P=ANY;testbool:[Test Boolean functionality](Bool)}" \
         +"$A=bro"

if __name__ == '__main__':
    dispatch(raw_input())
