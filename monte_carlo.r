
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

coinFlips1 <- function(numFlips = 10, numHeads = 1) {
  numRuns <- 100000;
  count <- 0;
  for(i in 1:numRuns) {
    if(sum(rbinom(numFlips, 1, .5)) == numHeads) {
      count <- count + 1;
    }
  }
  return(count/numRuns);
}

coinFlips <- function(numFlips=10, prob = .5) {
  numRuns <- 100000;
  counts <- rep(0, numFlips + 1);
  #set.seed(2);
  for(i in 1:numRuns) {
    heads <- sum(rbinom(numFlips, 1, prob));
    #print(heads);
    counts[heads + 1] <- counts[heads + 1] + 1;
    #The +1 offset (and length of numFlips + 1) is needed because there are numFlips + 1 combinations
    #if numFlips = 10, the number of heads could be 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 or 10
  }
  return(counts);
}

randomWalkBinomial <- function(numSteps = 1000, prob = .5) {
  steps <- rbinom(numSteps, 1, prob);
  steps[steps == 0] <- -1;
  plot(cumsum(steps), type="l", main="Random Walk with Binomial Distribution", xlab="step", ylab="distance traveled");
  return(steps);
}

randomWalkNormal <- function(numSteps = 1000, sd = 1) {
  steps <- rnorm(numSteps, 0, sd);
  plot(cumsum(rnorm(steps, 0, sd)), type="l", main="Random Walk with Normal Distribution", xlab="step", ylab="distance travled")
  return(steps);
}