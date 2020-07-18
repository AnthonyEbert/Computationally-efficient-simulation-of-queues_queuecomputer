
cat("Usage \n")

set.seed(1)

library("queuecomputer")

arrivals <- cumsum(rexp(100))
head(arrivals)

service <- rexp(100)
departures <- queue_step(arrivals, service = service, servers = 2)

print(departures$departures_df)

print(summary(departures))