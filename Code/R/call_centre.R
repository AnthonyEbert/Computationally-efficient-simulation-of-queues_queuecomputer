library("queuecomputer")
library("randomNames")
library("ggplot2")

suppressWarnings(RNGversion("3.5"))
set.seed(1) 

interarrivals <- rexp(20, 1)
arrivals <- cumsum(interarrivals)
head(arrivals)

customers <- randomNames(20, which.names = "first")

service <- rexp(20, 0.5)

head(service)

queue_obj <- queue_step(arrivals, service, servers = 2, labels = customers)
head(queue_obj$departures_df)

firstcustomers <- arrivals[1:2] + service[1:2]
firstcustomers

firstcustomers[2] + service[3]

depart(queue_obj)[1:3] - arrivals[1:3] - service[1:3]

summary(queue_obj)

plot(queue_obj, which = c(2, 4, 5, 6))
