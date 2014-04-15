
piMonteCarlo = function(num_points) {
  print(date());
  result = 0.0;
  x = runif(num_points);
  y = runif(num_points);
  hits = vector();
  num_hits = 0;
  #hyp = (x^2 + y^2)^.5;
  hypVec = (x^2 + y^2)^.5
  num_hits = length(which(hypVec < 1));
  result = num_hits / num_points;
  print(date());
  return(result * 4);
}


richer <- function(numDots = 10, numRuns = 10, verbose = FALSE) {
  #set.seed(12345);
  dots <- runif(numDots);
  total <- sum(dots);
  probs <- dots / total;
  cumProbs <- cumulativeProbs(dots);#cumsum(probs);
  
  print("dots:");
  print(dots);
  #print(probs);
  #print(cumProbs);
  
  for(i in 1:numRuns) {
    if(verbose) {
      print("dots:");
      print(dots);
      print("cumProbs:"); print(cumProbs);
    }
    rand <- runif(1);
    
    index <- min(which((cumsum(probs) > rand) == T));
    
    if(verbose) {
      print(rand);
      print(index);
      print(cumProbs[index]);
      print(dots[index]);
    }
    
    dots[index] <- dots[index] + rand;
    cumProbs <- cumulativeProbs(dots);
  }
  print(dots);
  return(dots);
}

#finds the cumulative probabilites
#x must be a numeric vector
cumulativeProbs <- function(x) {
  if(!is.numeric(x)) {
    stop("input must be numeric")
  }
  probs <- getProportions(x);
  return(cumsum(probs));
}

#finds the proportiion of the total that each entry is
#x must be a numeric vector
proportions <- function(x) {
  if(!is.numeric(x)) {
    stop("input must be numeric")
  }
  total <- sum(x);
  probs <- x / total;
  return(probs);
}