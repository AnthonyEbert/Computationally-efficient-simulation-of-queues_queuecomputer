
import simpy
from rpy2.robjects.packages import importr
import rpy2.robjects as robjects
import time
import csv
import numpy 
import getopt
import sys
from mpmath import *

robjects.r('''
	initialisation <- function(n){
	set.seed(1)
	n_customers <- 10^n
	lambda_a <- 1/1
	lambda_s <- 1/0.9
	interarrivals <<- rexp(n_customers, lambda_a)
	arrivals <<- cumsum(interarrivals)
	arrival_df <- data.frame(ID = c(1:n_customers), times = arrivals)
	service <<- rexp(n_customers, lambda_s)
}''')


benchmark = 1

arginput = sys.argv[1]

for o in sys.argv[1:]:
	if o == "-v":
		benchmark = 0

arginput = int(float(arginput))

r_initialisation = robjects.globalenv['initialisation']
r_initialisation(arginput)

interarrivals = robjects.r('interarrivals')
service = robjects.r('service')

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
        t = next(arrival_next)
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
        tib = next(service_next)
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
	



