


library(queuecomputer)
library(dplyr)
library(magrittr)
library(randomNames)
library(FAdist)
library(ggplot2)
library(tidyr)

set.seed(1)


# Functions --------------------

RandomFlightNumbers <- function(n){
  working_df <- data.frame(
    First_Letter = sample(LETTERS, n, replace = TRUE),
    Second_Letter = sample(LETTERS, n, replace = TRUE),
    Third_Letter = sample(LETTERS, n, replace = TRUE),
    First_Number = sample(c(1:9), n, replace = TRUE),
    Second_Number = sample(c(1:9), n, replace = TRUE), 
    Third_Number = sample(c(1:9), n, replace = TRUE)
  )
  
  output <- working_df %>% transmute(FlightNo = paste(
    First_Letter, Second_Letter, Third_Letter, First_Number, 
    Second_Number, Third_Number, sep = ""
  ))
  
  output$Passengers = rbinom(n, size = 260, prob = 0.8)
  output$arrival = round(rbeta(n, 2, 4) * 960 + 360, 2)
  output %<>% mutate(chocks = 
      queue(arrival, service = rep(35, n), servers = 12, serveroutput = TRUE) - 30, 
    gate = attr(chocks, "server")
  )
  
  output$type = factor(sample(letters[1:2], n, replace = TRUE, prob = c(0.4, 0.6)))
  
  
  return(output)
}

PassengerExpand <- function(input){
  output <- data.frame(
    ID = randomNames(input$Passengers),
    FlightNo = as.factor(rep(input$FlightNo, input$Passengers)),
    arrival = rep(input$chocks, input$Passengers),
    type = rep(input$type, input$Passengers),
    route_imm = sample(c("smart gate", "manual"), prob = c(input$r_prob, 1 - input$r_prob), replace = TRUE, size = input$Passengers),
    distance = rep(input$distance, input$Passengers), 
    bag_rate = rep(bag_rate, input$Passengers)
  ) %>%
    mutate(route_imm = factor(route_imm))
  
  return(output)
}

# Paramaterised Data --------------------------------------

bag_rate = 10

gate_df <- data.frame(
  gate = c(1:15), 
  distance = c(rep(20,10), rep(20.1,5))
)

routing_df <- data.frame(
  type = c("a", "b"),
  r_prob = c(0.5, 0.5)
)

parameter_df <- data.frame(
  type = c("a", "b", "a", "b"), 
  route_imm = c("smart gate", "smart gate", "manual", "manual"),
  walking_shape = c(0.1, 0.1, 0.1, 0.1),
  service_imm = c(6, 6, 2.5, 2.5)
)

## Build Flight Schedule ----------------------

FlightSchedule <- RandomFlightNumbers(120) %>%
  left_join(gate_df) %>%
  left_join(routing_df) %>%
  group_by(FlightNo)

if(
  (FlightSchedule %>% group_by(FlightNo) %>% 
      summarise(numberp = n()) %>% 
      dim())[1] != 120
){
  warning("Duplicate flight numbers detected")
}

FlightSchedule %<>% group_by(FlightNo)

# Passenger Data --------------------

Passenger_df <- FlightSchedule %>% 
  do(PassengerExpand(.)) %>%
  left_join(parameter_df) %>%
  group_by(type, route_imm) %>%
  mutate(arrive_imm = rllog(n(), shape = walking_shape[1], scale = log(distance[1]/10)) + arrival) %>%
  group_by(FlightNo) %>%
  mutate(bag_time = cumsum(rexp(n(), rate = bag_rate)) + arrival + 9) %>%
  group_by(type, route_imm) %>%
  mutate(service_imm = rexp(n(), service_imm[1])) %>%
  ungroup() %>%
  select(ID, FlightNo, arrival, 
    route_imm, arrive_imm, service_imm, bag_time)

Passenger_df$FlightNo <- as.factor(Passenger_df$FlightNo)


##%%% Can show this on JSS paper

Passenger_df

server_df <- data.frame(
  route_imm = c("smart gate", "manual")
)

server_df$servers <- list(5, as.server.stepfun(x = c(600,780), y = c(10,12,8)))

Passenger_df <- left_join(Passenger_df, server_df)
Passenger_df <- Passenger_df %>% 
  group_by(route_imm) %>%
  mutate(
    departures_imm = queue(arrive_imm, service_imm, servers = servers[[1]])
  ) %>% 
  ungroup() %>% 
  mutate(departures_bc = pmax.int(departures_imm, bag_time))

Passenger_df %>% select(FlightNo, arrive_imm, departures_imm, departures_bc)

## Summaries ----------------------------

Passenger_df %>% 
  group_by(FlightNo, route_imm) %>%
  summarise(
    waiting_imm = mean(departures_imm - service_imm - arrive_imm), 
    waiting_bc = mean(departures_bc - departures_imm)
  )

Passenger_df %>% 
  group_by(route_imm) %>%
  summarise(
    waiting_imm = mean(departures_imm - service_imm - arrive_imm), 
    waiting_bc = mean(departures_bc - departures_imm)
  )



Passenger_df %<>% ungroup()

PQ_df <- Passenger_df %>%
  mutate(start_imm = departures_imm - service_imm) %>%
  select(FlightNo, arrive_imm, start_imm, route_imm) %>%
  gather(key, value, -FlightNo, -route_imm) %>%
  mutate(key = factor(key))

state_df <- data.frame(
  key = c("arrive_imm", "start_imm"),
  state = c(1, -1)
)

PQ_df <- left_join(PQ_df, state_df) %>% 
  arrange(value) %>%
  group_by(route_imm)

ggplot(PQ_df) + 
  aes(x = value, y = cumsum(state)) +
  geom_step() + 
  facet_wrap(~route_imm) + 
  xlab("Time (min since 12am)") + 
  ylab("Queue length")

# ggsave("examples/queuelength_dplyr.pdf")

Passenger_df %<>% ungroup()

PBox_df <- Passenger_df %<>%
  mutate(
    immigration = departures_imm - service_imm - arrive_imm,
    baggage = departures_bc - departures_imm,
    entire_terminal = departures_bc - arrival
  ) %>%
  select(immigration, baggage, entire_terminal, FlightNo, route_imm) %>%
  gather(key, value, -FlightNo, -route_imm)

PBox_df

PBox_df$key <- factor(PBox_df$key, levels = c("immigration", "baggage", "entire_terminal"))

ggplot(PBox_df) +
  aes(y = value, x = key, col = route_imm) +
  geom_boxplot() + 
  xlab("Area of airport terminal") + 
  ylab("Waiting time (min)") + 
  ggtitle("Boxplot of waiting times")

# ggsave("examples/boxplot_dplyr.pdf")







