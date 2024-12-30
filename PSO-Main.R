# Loading in initial data
tsp_data <- read.csv('C:\\Users\\hotke\\OneDrive\\Documents\\JHU\\Optimization\\Homework\\tsp_data.csv', header = FALSE)

# Create vector of all distances between cities. UPDATE: DEPRECATED!
distances <- tsp_data[lower.tri(tsp_data, diag = TRUE)]
distances <- if(length(which(distances==0)!=0)) distances[-which(distances==0)]


# Build random tour vector
RandomTour <- function(n){
  # Use sample() function to generate vector of integers between [1,n] without replacement
  # Drop any values of 1, and add city 1 to the beginning and end
  partial_tour <- sample(n)
  partial_tour <- partial_tour[partial_tour != 1]
  random_tour <- c(1, partial_tour, 1)
  
  return(random_tour)
}


# Calculate total distance of a tour built usinf RandomTour()
TourDistance <- function(tour){
  dist_vector <- c()
  
  # I need to round the values to be able to grab distance info from distance matrix using row and column indexes
  # Any particle that drifts above 12 or below 2 needs to be brought back
  tour <- round(tour)
  tour[2:(length(tour) - 1)][tour[2:(length(tour) - 1)] > 12] <- 12
  tour[2:(length(tour) - 1)][tour[2:(length(tour) - 1)] < 2] <- 2
  
  # Grab distances of each edge from distance matrix
  for(i in 1:(ncol(tsp_data))){
    temp <- tour[i:(i+1)]
    dist_vector[i] <- tsp_data[temp[1],temp[2]]
  }
  
  # Calculate sum of distances
  total_distance <- sum(dist_vector)
  
  return(total_distance)
}


# Calculate the inertia, formula is based on the slides form class
CalcInertia <- function(x, y){
  new_inertia <- .9 - ((.9 - .4) / y) * x
  return(new_inertia)
}


# Function to track location, velocity, and tour distance of each particle
ParticleTracking <- function(x, v, r1, r2, c, P, G, inertia=.4){
  location <- x
  current_dist <- TourDistance(x)
  distance <- current_dist
  
  # Calc inertia. Broken into 3 components for readability
  v1 <- inertia * v
  v2 <- c * r1 * (P - location)
  v3 <- c * r2 * (G - location)
  velocity <- v1 + v2 + v3
  
  # Move current location based on velocity (don't touch City 1 values)
  location[2:(length(location)-1)] <- location[2:(length(location)-1)] + velocity[-c(1, length(velocity))]
  
  # Calc distance
  new_location <- location
  new_distance <- TourDistance(new_location)
  
  # Replace location and distance values if a better solution is found
  if(new_distance < current_dist){
    location <- new_location
    distance <- new_distance
  }
  
  output <- list(location, distance, velocity, inertia)
  names(output) <- c('location','distance', 'velocity', 'inertia')
  
  return(output)
}


# Main function to run PSO based on tsp_data
PSO <- function(df, n=20){
  # Dataframes to store initial particle locations, and record best locations
  initial_locations <- data.frame()
  # best_locations <- data.frame()
  
  # Initialize variables to store velocities of each particle, updated at each i
  velocities <- data.frame()
  
  # Initialize empty vector to be added to later
  best_distances <- c()
  
  # List of all particles and their attributes (current location, velocity, etc.)
  particles <- list()
  
  # Initialize variables for static parameters in our PSO
  r1 <- runif(1)
  r2 <- runif(1)
  learning_rate = 2
  i <- 0
  inertia <- .4
  
  # Create dataframes of initial locations, velocities, and solutions of each particle
  for(i in 1:n){
    temp_loc <- RandomTour(ncol(df))
    
    initial_locations <- rbind(initial_locations, temp_loc)
    best_distances[i] <- TourDistance(temp_loc)
    velocities <- rbind(velocities, rep(0,ncol(df)+1))
  }
  
  best_locations <- initial_locations
  
  # Iterations of algorithm to run.
  iterations <- 10
  
  # Loop through each iteration and update dataframes with new best locations solutions for each particle
  for(i in 1:iterations){
    for(i in 1:n){
      
      # Creating vectors to pass to ParticleTracking() function
      temp_location <- as.vector(unlist(initial_locations[i, ]))
      temp_velocity <- as.vector(unlist(velocities[i, ]))
      
      # Get updated location, velocity, and solution values for a particle
      new_particle <- ParticleTracking(x = temp_location,
                                       v = temp_velocity,
                                       r1 = r1,
                                       r2 = r2,
                                       c = learning_rate,
                                       P = as.vector(unlist(best_locations[i, ])),
                                       G = as.vector(unlist(best_locations[which.min(best_distances), ])),
                                       inertia = CalcInertia(x = i, y = iterations))
      
      # Create vectors with new best location and velocity found by ParticleTracking()
      temp_new_location <- new_particle[[1]]
      temp_new_velocity <- new_particle[[3]]
      
      # If a better solution is found update the best_distances and best_locations variables
      if(new_particle[[2]] < best_distances[i]){
        best_distances[i] <- new_particle[[2]]
        best_locations[i,] <- temp_new_location
      }
      
      # Update initial_locations and velocities variables, regardless of solution value
      initial_locations[i,] <- temp_new_location
      velocities[i, ] <- temp_new_velocity
      
    }
  }
  
  output <- list(best_locations, best_distances) 
  names(output) <- c('best_locations', 'best_distances')
  
  return(output)
}


MainPSO <- function(df){
  # Creating tsp_pso object to store dataframe of final locations and tour distances of each particle
  tsp_pso <- PSO(df)
  min_distance <- min(tsp_pso$best_distances)
  location_of_min_distance <-  as.vector(unlist(tsp_pso$best_locations[which.min(tsp_pso$best_distances), ]))
  
  output <- list(min_distance, location_of_min_distance)
  names(output) <- c('Minimum Distance', 'Location')
  
  return(output)
  
}


