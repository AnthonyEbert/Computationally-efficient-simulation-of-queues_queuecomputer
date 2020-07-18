
import simpy
import time
import csv
import numpy 
import getopt
import sys
from mpmath import *

benchmark = 1

arginput = sys.argv[1]

for o in sys.argv[1:]:
	if o == "-v":
		benchmark = 0

arginput = int(float(arginput))

if benchmark == 0:
    input_data = numpy.loadtxt('output/input_for_simpy.csv', skiprows = 1, delimiter=',')
    interarrivals = input_data[:,0]
    service = input_data[:,1]
    interarrivals = numpy.append(interarrivals, 1e6)
    service = numpy.append(service, 1e6)
else:
    interarrivals = numpy.random.exponential(1, 10**arginput)
    service = numpy.random.exponential(0.9, 10**arginput) 

service_next = iter(service)
arrival_next = iter(interarrivals)

data = []

maxNumber = 10**arginput
maxTime = 1e9 # minutes

## Model components ------------------------
start_time = time.time()
def source(env, number, counter):
    """Source generates customers randomly"""
    yield env.timeout(next(arrival_next))
    for i in range(number):
        try:
            t = next(arrival_next)
        except StopIteration:
            return
        c = customer(env, 'Customer%02d' % i, counter)
        env.process(c)
        yield env.timeout(t)

def customer(env, name, counter):
    """ Customer arrives, is served and leaves """
    arrive = env.now
    #print('%7.4f %s: Here I am' % (arrive, name))
    with counter.request() as req:
        # Wait for the counter or abort at the end of our tether
        results = yield req
        wait = env.now - arrive
        try:
            tib = next(service_next)
        except StopIteration:
            return
        yield env.timeout(tib)
        #print('%7.4f %s: Finished' % (env.now, name))
        data.append(env.now)

	
## Experiment data -------------------------

env = simpy.Environment()

# Start processes and run
counter = simpy.Resource(env, capacity=2)

env.process(source(env, maxNumber, counter))
env.run()

if benchmark == 1:
	print(time.time() - start_time)

dataarray = numpy.asarray(data)


if benchmark == 0:
	numpy.savetxt("output/simpyoutput.csv", dataarray, delimiter = ",")
	



