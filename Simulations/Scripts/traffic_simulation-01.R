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


get_distance <- function(time_sim, object = create_vehicle(),
                         speed = get_speed(object)) {
  
  stopifnot(is.numeric(time_sim), is.numeric(speed), length(speed) == 1, 
            is.character(object), length(object) == 1)
  distance_covered <- 0
  i <- 1
  
  while (i <= length(time_sim)) {
    distance_covered <- distance_covered + speed
    i <- i + 1
  }
  distance_covered
}


# increase_distance -------------------------------------------------------

increase_distance <- function(time_sim, object = create_vehicle(),
                              speed = get_speed(object)) {
  stopifnot(is.numeric(time_sim), is.numeric(speed), length(speed) == 1, 
            is.character(object), length(object) == 1)
  
  distances <- vector("numeric", length(time_sim))
  distances[[1]] <- 0
  time_sim <- time_sim[order(time_sim)]
  i <- 2
  
  while (i <= length(time_sim)) {
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
  
  final_speed
}

# distance_reduce ---------------------------------------------------------

distance_reduce <- function(time_sim, object = create_vehicle(),
                            init_speed = get_speed(object)) {
  reduced_speed <- reduce_speed(object, init_speed)
  
  if (reduced_speed == init_speed) distance <- get_distance(time_sim, object,
                                                            init_speed)
  
  else distance <- get_distance(time_sim, object, reduced_speed)
  
  distance
}

# sim_traffic -------------------------------------------------------------

sim_traffic <- function(time_sim) {
  stopifnot(is.numeric(time_sim))
  
  vehicles <- vector("character", length(time_sim))
  init_speed <- vector("numeric", length(time_sim))
  distance_covered <- vector("numeric",length(time_sim))
  
  reduced_speeds <- vector("numeric", length(time_sim))
  
  for (i in 1:length(time_sim)) {
    vehicles[[i]] <- create_vehicle()
    init_speed[[i]] <- get_speed(vehicles[[i]])
    reduced_speeds[[i]] <- reduce_speed(vehicles[[i]],
                                        init_speed[[i]])
    if (init_speed[[i]] != reduced_speeds[[i]]) {
      distance_covered[[i]] <- purrr::map2_dbl(.x = vehicles[[i]],
                                               .y = reduced_speeds[[i]],
                                               get_distance, 
                                               time_sim = time_sim) # fixed distance_covered error
    }
    
    else {
      distance_covered[[i]] <- purrr::map2_dbl(.x = vehicles[[i]],
                                               .y = init_speed[[i]],
                                               get_distance, 
                                               time_sim = time_sim)
    }
    
    # distance_covered[[i]] <- distance_reduce(time_sim, vehicles[[i]],
    #                                          init_speed[[i]])
  }
  
  sim_data <- data.frame(
    vehicle = vehicles,
    init_speed = init_speed,
    distance_covered = distance_covered,
    reduced_speeds = reduced_speeds
  )
  
  sim_data
}

sim_traffic(1:10)
