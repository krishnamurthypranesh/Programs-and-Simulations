# This script contains the code for simulating the Nagel-Schreckenberg model.
# The said model is a cellular automation model which simulates one way traffic
# using cars.

# Function to store constraints -------------------------------------------
constraints <- function(vmin = 1, vmax = 5, distance = 100) {
  (constraints <- list(vmin = vmin, vmax = vmax, distance = distance))
}

const_list <- constraints()

# Function to create vehicles ----------------------------------------------
create_vehicle <- function(name, size = 3) {
  vehicle <- vehicle(name, size, const_list$vmin, const_list$vmax)
  
  vehicle
}

#=====----------==============---------------================--------
# car1 <- create_vehicle("car")
# 
# car1$name
# 
# cars <- c("mustang", "ragera", "chiron", "veyron", "gallardo", "gtr", 
#           "berlinetta", "phantom", "huracan", "sesto elemento")
# 
# vapply(cars, create_vehicle, FUN.VALUE = c(list(name = "car", size = 0,
#                                                 speed = 0)))
# 
# cars_list <- lapply(cars, create_vehicle)


# Function to increase speed ----------------------------------------------
increase_speed <- function(vehicle) {
  stopifnot(class(vehicle) == "vehicle")
  
  if (vehicle$speed < const_list$vmax) {
    vehicle$speed <- vehicle$speed + 1
  }
  
  vehicle$speed
}

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


# Function to randomly reduce speed ---------------------------------------

rand_speed_reduce <- function(vehicle, const_list = const_list, ...) {
  logi_val <- sample(c(TRUE, FALSE), size = 1)
  
  if (isTRUE(logi_val) && vehicle$speed > const_list$vmin) {
    vehicle$speed <- vehicle$speed - 1
  }
  
  vehicle$speed
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
sim_traffic <- function(length_sim = 10, ...) {
  cars <- c("mustang", "ragera", "chiron", "veyron", "gallardo", "gtr", 
            "berlinetta", "phantom", "huracan", "sesto elemento")
  
  cars_list <- lapply(sample(cars, size = length_sim, replace = TRUE), 
                      create_vehicle)
  names(cars_list) <- sample(cars, size = length_sim, replace = T)
  
  # getting distance travelled 
  distances <- vapply(cars_list, distance_travelled, 
                      time_duration = length_sim, FUN.VALUE = c(distance = 0))
  # increasing speeds
  increased_speeds <- vapply(cars_list, increase_speed, FUN.VALUE = c(x = 0))
  #random reduction in speeds
  reduced_speeds <- vapply(cars_list, rand_speed_reduce, FUN.VALUE = c(x = 0))
  vehicle_stats <- list(vehicles = cars_list, 
                        distances = distances, 
                        final_speeds = increased_speeds,
                        random_speed_reductions = reduced_speeds)
  
  vehicle_stats
}

sim_traffic()
