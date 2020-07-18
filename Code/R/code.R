cat("Run 'python3 simpy_script.py 4 -v' before this R script \n")

library("simmer")
library("queuecomputer")
library("ggplot2")
library("microbenchmark")

suppressWarnings(RNGversion("3.5"))

do_benchmarks <- TRUE # Do time-consuming benchmarks
progress_bar <- TRUE # Have a progress bar for the benchmarks

if (progress_bar) {
  pb <- txtProgressBar(min = 0, max = 6)
}

# Validation (Replication) ---------------------

initialisation <- function(n) {
  set.seed(1)
  n_customers <<- 10^(n)
  lambda_a <<- 1/1
  lambda_s <<- 1/0.9
  interarrivals <<- rexp(n_customers, lambda_a)
  arrivals <<- cumsum(interarrivals)
  service <<- rexp(n_customers, lambda_s)
}

initialisation(4)

## simmer --------------

consume <- function(x) {
	i <- 0
	function() {
		i <<- i + 1
		x[[i]]
	}
}


simmer_step <- function(arrivals, service, servers) {
	mm2.trajectory <- trajectory() %>%
		seize("resource", amount = 1) %>%
		timeout(consume(service)) %>%
		release("resource", amount = 1)

	mat <- simmer() %>%
		add_resource("resource", capacity = servers, queue_size = Inf, mon = FALSE, preemptive = TRUE, preempt_order = "fifo") %>%
		add_generator("arrival", mm2.trajectory, at(arrivals)) %>%
		run(until = Inf) %>%
		get_mon_arrivals()

	return(mat[, 3])
}



cat("simmer output \n")

simmer_output <- simmer_step(arrivals = arrivals, service = service, servers = 2)
simmer_output %>% head

## SimPy -----------------------

cat("simpy output \n")

simpy_output <- read.csv("output/simpyoutput.csv", header = FALSE) %>% t %>% as.numeric
simpy_output %>% head

## queuecomputer ------------------

length_out <- min(length(simmer_output), length(simpy_output)) # Last few customers need to be deleted 

simmer_output <- sort(simmer_output)[1:length_out]
simpy_output <- sort(simpy_output)[1:length_out]

queuecomputer_output <- sort(queue(
  arrivals = arrivals, 
  service = service, 
  servers = 2
  ))[1:length_out]

cat("queuecomputer output \n")

queuecomputer_output %>% head

## Overall -------------

cat("Is queuecomputer output same as simmer output? \n")

all(round(queuecomputer_output, 5) == round(simmer_output, 5))

cat("Is queuecomputer output same as simpy output? \n")

all(round(queuecomputer_output, 5) == round(simpy_output, 5))

# Validation (Versus theoretical) ---------------

## Functions --------------------------

P_0_func <- function(rhoK, k) {
  sum_i <- rep(NA, k)
  
  for (i in 0:I(k-1)) {
    sum_i[i+1] <- rhoK^i / factorial(i)
  }
  
  p_0 <- 1/(sum(sum_i) + rhoK^k/(factorial(k - 1) * (k - rhoK)))
  return(p_0)
}

P_n <- function(rhoK, n, k) {
  
  p_0 <- P_0_func(rhoK, k)
  if (n <= (k-1)) {
    output <- rhoK^n / factorial(n) * p_0
  } else {
    output <- rhoK^n / (factorial(k) * k^(n-k)) * p_0
  }
  return(output)
}

Lq <- function(rhoK, k) {
  p_0 <- P_0_func(rhoK, k)
  
  output <- p_0 * rhoK^{k+1} / (factorial(k-1) * (k - rhoK)^2)
  return(output)
}


## Initialisation ------------------

set.seed(1) # For reproduciblitity
n_customers <- 5e6
lambda <- 2
mu <- 1
interarrivals <- rexp(n_customers, lambda)
arrivals <- cumsum(interarrivals)
service <- rexp(n_customers, mu)
K <- 3

## Theoretical --------------------

cat("Theoretical results for M/M/2 queueing system ########### \n")

rho <- (1/mu) / (K/lambda)
cat("rho", rho, "\n")
rhoK <- rho * K

p_0 <- P_n(rhoK, 0, K)

### System lengths
theoretical_system <- Vectorize(P_n, "n")(rhoK = rhoK, n = 0:30, k = K)

### Estimated queue length
LQ <- Lq(rhoK, K)

### Estimated units in system
cat("Expected customers in system", Lq(rhoK, K) + rhoK, "\n")

Ws <- 1/mu
Wq <- LQ / lambda

cat("Expected waiting time", Wq, "\n")

## Observed -------------------------

cat("Simulated results for M/M/2 queueing system ########### \n")

MM3 <- queue_step(arrivals = arrivals, service = service, servers = K)
summary(MM3)
MM3_summary <- summary(MM3)
cat("rho", MM3_summary$utilization, "\n")

cat("Average customers in system", MM3_summary$slength_mean, "\n")

cat("Average waiting time", MM3_summary$mwt, "\n")


## Plots --------------------

queue_system <- data.frame(
  theoretical = theoretical_system[1:11],
  observed = MM3_summary$slength_sum$proportion[1:11],
  N = as.factor(c(0:10)))

ggplot(queue_system, aes(x = theoretical, y = observed, col = N)) +
  geom_point(size = 2) +
  geom_abline(slope = 1, intercept = 0, size = 0.2) +
  ggtitle("Comparison of Theoretical and Observed P(N)") +
  xlab("Theoretical P(N) for M/M/2") +
  ylab("Observed P(N) from simulation")

# Benchmark ---------------------

if (do_benchmarks == TRUE) {

  ## queuecomputer --------------
  
  cat("Start queuecomputer benchmark\n")
  
  queuecomputer_times <- rep(NA, 6)
  
  for (i in 1:6) {
      initialisation(i+1)
      queuecomputer_times[i] <- (microbenchmark(
  	  queue(
  	    arrivals = arrivals, 
  	    service = service, 
  	    servers = 2
  	    ), 
  	  unit = "ms")$time / 1e6) %>% median
      if (progress_bar) {
          setTxtProgressBar(pb, i)
      }
  }
  
  if (progress_bar) {
      close(pb)
  }
  
  cat("queuecomputer benchmark complete\n")
  
  write.csv(queuecomputer_times, file = "output/queuecomputer_times.csv")
  
  ## simmer ------------------
  
  cat("Start simmer benchmark\n")

  simmer_times <- rep(NA, 4)
  
  for (i in 1:4) {
  	initialisation(i+1)
    
  	simmer_times[i] <- (microbenchmark(
  	  simmer_step(
  	    arrivals = arrivals, 
  	    service = service, 
  	    servers = 2
  	    ), 
  	  unit = "ms")$time / 1e6) %>% median
  	
    gc()
      
    if (progress_bar) {
        setTxtProgressBar(pb, i)
    }
  }
  
  initialisation(5+1)
  
  simmer_times[5] <- (microbenchmark(
    simmer_step(
      arrivals = arrivals, 
      service = service, 
      servers = 2
      ), 
    unit = "ms", times = 10)$time / 1e6) %>% median
  
  write.csv(simmer_times, file = "output/simmer_times.csv")
  
  if (progress_bar) {
      close(pb)
  }
  
  cat("simmer benchmark complete\n")


  ## process raw simpy output ---------------

  simpy_rawtimes <- read.csv("output/raw_simpy_times.csv", header = FALSE) * 1000 # convert to milliseconds
  
  names(simpy_rawtimes) <- "times"
  
  simpy_rawtimes$Passengers <- c(rep(2:5, each = 100), rep(6, 10))
  
  simpy_output <- dplyr::summarise(dplyr::group_by(simpy_rawtimes, Passengers), median = median(times))
  
  write.csv(simpy_output, file = "output/simpy_times.csv")
  
}

# Plotting ---------------

queuecomputer_times <- read.csv("output/queuecomputer_times.csv")$x
simmer_times <- read.csv("output/simmer_times.csv")$x
simpy_times <- read.csv("output/simpy_times.csv")$median


benchmark_dataset <- data.frame(
  customers = c(10^c(2:7), 10^rep(c(2:6), 2)), 
  package = c(rep("queuecomputer", 6), rep("simmer", 5), rep("simpy", 5)), 
  time = c(queuecomputer_times, simmer_times, simpy_times)
  )

write.csv(benchmark_dataset, file = "output/benchmark_dataset.csv")

simmer_times / queuecomputer_times[1:5]
simpy_times / queuecomputer_times[1:5]

myplot <- ggplot(benchmark_dataset) + 
  aes(x = customers, y = time, col = package) + 
  geom_point() + 
  scale_x_log10() + 
  scale_y_log10() + 
  xlab("Number of Customers") + 
  ylab("Computation time (ms)") + 
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) + 
  ggtitle("Benchmark of packages for M/M/2 simulation")

myplot

# Examples -------------
set.seed(2)
cat("Examples ############## \n")

cat("call-centre \n")

set.seed(3)
source("R/call_centre.R", print.eval = TRUE)

cat("airport \n")
source("R/airport.R", print.eval = TRUE)



















