
# Constructor function ----------------------------------------

vehicle <- function(name, size, vmin = 1, vmax = 5) {
  stopifnot(is.character(name), length(name) == 1, 
            is.numeric(size), length(size) == 1)
  
  speed_vector <- seq(vmin, vmax)
  vehicle <- structure(list(name = name,
                            size = size,
                            speed = sample(speed_vector, 1)))
  
  vehicle
}


# data.frame method for class vehicle -------------------------------------

# as.data.frame.vehicle <- function() {
# }
