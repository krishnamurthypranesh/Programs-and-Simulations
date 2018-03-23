
# This script contains the code for simulating the Nagel-Schreckenberg model.
# The said model is a cellular automation model which simulates one way traffic
# using cars.

# Function to create vehicles ----------------------------------------------

create_vehicle <- function(name, size = 3) {
  stopifnot(is.character(name), length(name) == 1)
  
  vehicle <- structure(list(name = name,
                            size = size,
                            speed = sample(1:5, size = 1)), 
                       class = "vehicle")
  
  vehicle
}

car1 <- create_vehicle("car")

car1$name

cars <- c("mustang", "ragera", "chiron", "veyron", "gallardo", "gtr", 
          "berlinetta", "phantom", "huracan", "sesto elemento")

vapply(cars, create_vehicle, FUN.VALUE = c(list(name = "car", size = 0,
                                                speed = 0)))


# Function to reduce speed ------------------------------------------------

reduce_speed <- function(vehicle1, vehicle2) {
  speed1 <- vehicle1$speed
  speed2 <- vehicle2$speed
  
  vehicle2$speed <- speed1
  
  # "The speed of ", car2$name, "has been reduced to", speed1
  
  print(paste(paste("The speed of ", vehicle2$name), 
              paste("has been reduced to", speed1, "from", speed2)))
  
  return(invisible(vehicle2))
}


# Function to get distance travelled --------------------------------------

distance_travelled <- function(vehicle, time_duration) {
  time <- seq(time_duration)
  distance <- 0
  i <- 0
  while (i < length(time)) {
    distance <- distance + vehicle[["speed"]]
    i <- i + 1
  }
  
  distance
}
# Function to check distance between two cars -----------------------------

red_speed <- function(vehicle1, vehicle2) {
  distance_car1 <- distance_travelled(car1)
  
  distance_car2 <- distance_travelled(car2)
  
  distance_between <- distance_car1 - distance_car2
  
  if (distance_betwee < 0) {
    reduce_speed(vehicle2, vehicle1)
  }
  else {
    reduce_speed(vehicle1, vehicle2)
  }
}
# Function to simulate vehicles ---------------------------------------------

sim_traffic <- function(length_sim = 10) {
  cars_list <- lapply(rep("car", length_sim), create_vehicle)
  
  # getting distance travelled 
  distances <- vapply(cars_list, distance_travelled, 
                      time_duration = length_sim, FUN.VALUE = c(distance = 0))
  
  (sim_data <- data.frame(vehicles = as.list(cars_list),
                          distance = distances))
}

sim_traffic()
