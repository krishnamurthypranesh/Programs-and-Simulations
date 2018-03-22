


create_vehicle <- function() {
  x <- sample(c("car", "bike"), size = 1, prob = c(0.35, 0.65))
   
  if (x == "car") y <- create_car()
  if(x == "bike") y <- create_bike()
  
  y
  }

create_car <- function() {
  obj <- "car"
  attributes(obj) <- list(class = "vehicle", size = 3)  
  obj
}

create_bike <- function() {
  obj <- "bike"
  attributes(obj) <- list(class = "vehicle", size = 2)
  obj
}

vehicles <- vector("list", 10)

for (i in 1:10) vehicles[[i]] <- create_vehicle()

vehicles

vehicles[[1]]

df <- data.frame()

mat <- matrix()
for (i in 1:10) {
  df[i, 1] <- vehicles[[i]]
}

# get_attr <- function(obj, which) {
#   if (which %in% attributes(obj)) 
# }
