#!/usr/bin/python2.7
# encoding=latin-1

#\ $ echo "HELO" | ./util/test.py
#\ $D=Test Module$F={some_func:[Some Function],other_func:[A Function printing "Hello World!"]}
from time import sleep
def dispatch(what):
    if what == 'HELO':
        send_handshake()
    elif what == 'init':
        args = raw_input().split(' ')
        inittest(args)

    elif what == 'triggertimeout':
        args = raw_input().split(' ')
        triggertimeout(args)

def inittest(args):
   print "Ohai my son!" 
   sleep(5)
   print "Life is no pony trail!"
   sleep(2)
   print "(I Think)"
   print "bai!"
   sleep(1)

def triggertimeout(args):
   sleep(65)
   print "ohai"

def send_handshake():
    print "$D=A Test Module" \
         +"$F={init:[inittest](String)%E=10s;triggertimeout:[Triggers a timeout](Void)%E=5s%P=Admin;}" \
         +"$A=bro"
if __name__ == '__main__':
    dispatch(raw_input())
