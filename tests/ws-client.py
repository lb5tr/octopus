import websocket
import sys
import datetime

def basic_test():
     ws = websocket.create_connection(sys.argv[2])
     print "* [Starting basic test]"
     print "* [Load string ~ 1kB]"
     hit_factor = 300
     load_string = "LOAD_TEST"*100
     then = datetime.datetime.now()
     
     for i in range(1,hit_factor):
              ws.send(load_string)

     now = datetime.datetime.now()
     print "* [Done]"
     delta = now - then

     print "* [Took {0}s]".format(delta.total_seconds())
     print "* [Sent {0}kB]".format(hit_factor * len(load_string) / 1000)
     print "* [Throughtput {0}kBps]".format(hit_factor/delta.total_seconds())

     ws.close()

def parse_args():
    if sys.argv[1] == 'basic':
        basic_test();

parse_args()
