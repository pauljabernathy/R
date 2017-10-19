
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

probSim <- function() {
  pA <- .1;
  pB <- .5;
  pCGivenA <- .9;
  pCGivenB <- .8;
  
  CAandB <- vector();
  NotCAandB <- vector();
  CAorB <- vector();
  NotCAorB <- vector();
  numRuns <- 10000;
  
  for(i in 1:numRuns) {
    a <- F;
    b <- F;
    c <- F;
    if(runif(1) < pA) {
      a <- F;
    }
    if(runif(1) < pB) {
      b <- F;
    }
    if(a == F & b == F) {
      #C will be false
    }
    if(a & runif(1) < pCGivenA) {
      if(runif(1) < pCGivenB) {
        #A and B are true - so what is the prob C is true?
      } else {
        CAandB <- c(CAandB, F)
      }
    }
  }
  return(0);
}

#seems to give an exponential distribution but is much slower than rexp
randExp <- function(count, r) {
  nums <- vector();
  f <- 1000;
  n <- 1 / f;
  r <- f * r;
  while(length(nums) <= count) {
    #current <- runif(1);
    current <- rbinom(1, 1, 1/r);
    while(current == 0) {
      n <- n + 1 / f;
      current <- rbinom(1, 1, 1/r);
    }
    nums <- c(nums, n);
    n <- 1 / f;
  }
  return(nums);
}

#This seems to give an exponential distribution also, and is seems comparable in speed to rexp
randExp2 <- function(count, lambda) {
  nums <- vector();
  alphas <- runif(count);
  for(i in 1:count) {
    current <- (-1 / lambda) * log(1 - alphas[i]);
    nums <- c(nums, current);
  }
  return(nums);
}

randNorm <- function(count, mu, sigma) {
  nums <- vector();
  alphas <- runif(count);
  for(i in 1:count) {
    current <- normIntegralInverse(alphas[i], mu, sigma);
    nums <- c(nums, current);
  }
  return(nums);
}

sgn <- function(x) {
  if(x < 0) {
    -1;
  } else if(x == 0) {
    0;
  } else {
    1;
  }
}

normIntegral <- function(x, mu, sigma) {
  y <- (x - mu) / sqrt(2 * sigma ^ 2);
  if(y >= 0) {
    return(.5 + .5 * erf(y));
  } else {
    return(.5 - .5 * erf(-y));
  }
}

erf <- function(x) {
  p = 0.3275911;
  a1 = 0.254829592;
  a2 = -0.284496736;
  a3 = 1.421413741;
  a4 = -1.453152027;
  a5 = 1.061405429;
  
  t <- 1 /(1 + p * x);
  
  erfx <- 1 - (a1 * t + a2 * t^2 + a3 * t^3 + a4 * t^4 + a5 * t^5) * exp(-x^2);
  erfx;
}

normIntegralInverse <- function(alpha, mu, sigma) {
  erfInverse(2 * alpha - 1) * sigma * sqrt(2) + mu;
}

#not quite as close an approximation as I would like but OK
erfInverse <- function(x) {
  a <- 8*(pi - 3) / (3*pi*(4 - pi));
  part1 <- (2/(pi * a) + .5*log(1 - x^2))^2;
  part2 <- (log(1 - x^2))/a;
  part3 <- 2/(pi * a) + .5*log(1 - x^2); #just the square root of part1
  
  result <- sgn(x) * sqrt(sqrt(part1 - part2) - part3);
  result;
}