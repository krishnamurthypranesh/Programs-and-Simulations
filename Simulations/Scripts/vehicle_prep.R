
# Constructor function ----------------------------------------

vehicle <- function(type, size, vmin, vmax) {
  stopifnot(is.character(type), length(type) == 1, 
            is.numeric(size), length(size) == 1)
  
  speed_vector <- seq(vmin, vmax)
  vehicle <- structure(list(type = type,
                            size = size,
                            speed = sample(speed_vector, size = 1)),
                       class = "vehicle")
  
  vehicle
}

# Function to get element by name -----------------------------------------

extractor <- function(x, element_name) {
  stopifnot(!is.null(names(x)), is.character(element_name), 
            length(element_name) == 1) 
  
  x[[element_name]]
}

# # as.data.frame method for class vehicle -------------------------------------
# 
# get_data.frame <- function(vehicle, ...) {
#   if(is.data.frame(vehicle)) {
#     return(vehicle)
#   } else if (!is.list(vehicle)) {
#     vehicle <- as.list(vehicle)
#   }
#   
#   names <- vapply(vehicle, extractor, element_name = "name",
#                   FUN.VALUE = c(name = "a"))
#   sizes <- vapply(vehicle, extractor, element_name = "size",
#                   FUN.VALUE = c(size = 0))
#   speeds <- vapply(vehicle, extractor, element_name = "speed",
#                    FUN.VALUE = c(speed = 0))
#   
#   (vehicle_df <- data.frame(name = names, size = sizes, speed = speeds))
# }
