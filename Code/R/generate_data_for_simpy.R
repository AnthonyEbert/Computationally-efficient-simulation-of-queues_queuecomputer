initialisation <- function(n){
	set.seed(1)
	n_customers <- 10^n
	lambda_a <- 1/1
	lambda_s <- 1/0.9
	interarrivals <<- rexp(n_customers, lambda_a)
	arrivals <<- cumsum(interarrivals)
	arrival_df <- data.frame(ID = c(1:n_customers), times = arrivals)
	service <<- rexp(n_customers, lambda_s)
}

initialisation(4)

input_for_simpy <- data.frame(interarrivals = interarrivals, service = service)

write.csv(input_for_simpy, "output/input_for_simpy.csv", row.names = FALSE)
