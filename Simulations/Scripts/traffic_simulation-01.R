
# create_vehicle ----------------------------------------------------------

create_vehicle <- function() {
  x <- c("car", "bike")
  y <- sample(x, size = 1, prob = c(0.35, 0.65))

  y
}

# get_speed ---------------------------------------------------------------


get_speed <- function(object) {
  stopifnot(is.character(object), length(object) == 1)
  
  if (object == "car") speed <- 2
  if (object == "bike") speed <- 3
  
  speed
}

# get_distance ------------------------------------------------------------


get_distance <- function(time, object = create_vehicle(),
                         speed = get_speed(object)) {
  
  stopifnot(is.numeric(time), is.numeric(speed), length(speed) == 1, 
            is.character(object), length(object) == 1)
  distance_covered <- 0
  i <- 1
  
  while (i <= length(time)) {
    distance_covered <- distance_covered + speed
    i <- i + 1
    }
  distance_covered
}


# increase_distance -------------------------------------------------------

increase_distance <- function(time, object = create_vehicle(),
                              speed = get_speed(object)) {
  stopifnot(is.numeric(time), is.numeric(speed), length(speed) == 1, 
            is.character(object), length(object) == 1)
  
  distances <- vector("numeric", length(time))
  distances[[1]] <- 0
  time <- time[order(time)]
  i <- 2
  
  while (i <= length(time)) {
    distances[[i]] <- distances[[i - 1]] + speed
    i <- i + 1
  }
  
  distances
}

increase_distance(1:100)


# advance_speed -----------------------------------------------------------

advance_speed <- function(object = create_vehicle(),
                          speed = get_speed(object)) {
  stopifnot(is.numeric(speed), length(speed) == 1,is.character(object), 
            length(object) == 1)
  final_speed <- speed + 1
  
  final_speed
}


# reduce_speed ------------------------------------------------------------

reduce_speed <- function(object = create_vehicle(), 
                         speed = get_speed(object)) {
  stopifnot(is.numeric(speed), length(speed) == 1,is.character(object), 
            length(object) == 1) 
  reduce <- sample(c(TRUE, FALSE), size = 1, prob = c(0.5, 0.5))
  
  if (reduce) final_speed <- speed - 1 
  else final_speed = speed
  
  list(vehicle = object, init_speed = speed, final_speed = final_speed)
}

# sim_traffic -------------------------------------------------------------

sim_traffic <- function(time) {
  stopifnot(is.numeric(time))
  
  vehicles <- vector("character", length(time))
  init_speed <- vector("numeric", length(time))
  distance_covered <- vector("numeric",length(time))
  
  for (i in 1:length(time)) {
    vehicles[[i]] <- create_vehicle()
    init_speed[[i]] <- get_speed(vehicles[[i]])
    distance_covered[[i]] <- purrr::map2_dbl(.x = vehicles[[i]],
                                             .y = init_speed[[i]],
                                             get_distance, time = time)
  }
  
  # get_distance(time, object = vehicles[[i]],
  #              speed = init_speed[[i]])
  sim_data <- data.frame(
    vehicle = vehicles,
    init_speed = init_speed,
    distance_covered = distance_covered
  )
  
  sim_data
}


# simulated traffic with reduced speed ------------------------------------

sim_traffic2 <- function(time) {
  stopifnot(is.numeric(time))
  
  vehicles <- vector("character", length(time))
  init_speed <- vector("numeric", length(time))
  distance_covered <- vector("numeric",length(time))
  
  for (i in 1:length(time)) {
    vehicles[[i]] <- create_vehicle()
    init_speed[[i]] <- get_speed(vehicles[[i]])
  }
  
  # get reduced speeds
  
  reduced_speeds <- vector("list", length(time))
  for (i in 1:length(time)) {
    reduced_speeds[[i]] <- reduce_speed(vehicles[[i]], init_speed[[i]])
  }
  
  
}

sim_traffic2(1:10)
