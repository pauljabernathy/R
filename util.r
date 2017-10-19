
DUMMY_PROBABILITY = .0001;
EPSILON <- 0.000001;

#TODO:  handle NAs
factorHist = function(x) {
  factors = levels(as.factor(x));
  for(i in 1:length(factors)) {
    #print(factors[i]);
  }
  counts = vector();
  for(i in 1:length(factors)) {
    counts[i] = length(which(x == factors[i]));
  }
  #return(counts);
  #h = data.frame("counts", "percents");
  #h = cbind(h, counts);
  #h[,1] = counts;
  #h = cbind(h, counts / length(x));
  #h[,2] = counts / length(x);
  h = data.frame(factors, counts, counts / length(x));
  names(h) = c("item", "counts", "percents");
  return(h);
}



doubleHist = function(x, y) {
  #if(class(x) != 'numeric' | class(y) != 'numeric') {
  #  return;
  #}

  combos = vector();
  counts = vector();
  current = paste(x[1], y[1], sep=',');
  #print(current);
  combos = c(combos, current);
  counts = c(counts, 1);
  
  for(i in 2:length(x)) {
    current = paste(x[i], y[i], sep=',');
    #print(current);
    index = indexOf(combos, current);
    if(index == -1) {
      combos = c(combos, current);
      counts = c(counts, 1);
    } else {
      counts[index] = counts[index] + 1;
    }
  }
  combos = combos[order(counts,decreasing = TRUE)];
  counts = counts[order(counts,decreasing = TRUE)];
  for(j in 1:length(combos)) {
    print(paste(combos[j], counts[j], sep=': '));
  }
  #pie(combos);
  #barplot(combos)
}

indexOf = function(x, s) {
  for(i in 1:length(x)) {
    if(x[i] == s) {
      return(i);
    }
  }
  return(-1);
}
#TODO:  change name
#See also the table function.  This function gives frequencies/probabilities, which table does not do, and prints the data differently.
breakdown = function(x, y) {
  t1 = table(x, y, useNA="ifany");
  t2 = table(y, x, useNA="ifany")
  rownames(t1) = replace(rownames(t1), is.na(rownames(t1)), "NA or unknown");
  colnames(t1) = replace(colnames(t1), is.na(colnames(t1)), "NA or unknown");
  rownames(t2) = replace(rownames(t2), is.na(rownames(t2)), "NA or unknown");
  colnames(t2) = replace(colnames(t2), is.na(colnames(t2)), "NA or unknown");
  
  #barplot(t1, legend=rownames(t1));
  #barplot(t2, legend=rownames(t2))
  #print(t);
  numRows = length(row.names(t1));
  numCols = length(t1) / numRows;
  
  tf = as.data.frame(t1);
  tf = tf[order(tf$x),];
  names(tf) = c("x", "y", "counts");
  #print(tf);
  #xGivenY = tf$counts[which(tf$x == tf$x && tf$y == tf$y)] / sum(tf$counts[which(tf$y == tf$y)]);
  #xGivenY = sapply(tf$counts, function(i) { i  });
  xGivenY = apply(tf, 1, function(i) { total <- sum(tf$counts[which(tf$y == i[2])]);
                                       prob <- as.numeric(i[3]) / total; 
                                       return(prob);
                                      });
  
  yGivenX = apply(tf, 1, function(i) { total <- sum(tf$counts[which(tf$x == i[1])]); 
                                       as.numeric(i[3]) / total });
  tf <- cbind(tf, xGivenY);
  tf <- cbind(tf, yGivenX);
  #print(tf);
  #print(sum(tf$counts));
  barplot(t1, legend=rownames(t1));
  barplot(t2, legend=rownames(t2));
  return(tf);
}

summary2 <- function(x) {
  print(summary(x));
  print(paste("sd: ", sd(x)));
  #print(paste("entropy: ", obsEntropy(x)));
  if(is.numeric(x)) {
    hist(x);
  } else if(is.factor(x)) {
    factorHist(x);
  }
}

#gives what the joint distribution would be if the two variables were indep
jointDistIfIndep <- function(x, y) {
  xHist <- factorHist(x);
  yHist <- factorHist(y);
  #print(xHist);
  #print(yHist);
  names <- vector();
  counts <- vector();
  for(i in 1:length(xHist$item)) {
    #print(as.character(xHist$item[i]));
    for(j in 1:length(yHist$item)) {
      
      #print(as.character(yHist$item[j]));
      names <- c(names, paste(as.character(xHist$item[i]), as.character(yHist$item[j]), sep=" "));
      #names <- c(names, "x");
      counts <- c(counts, xHist$percents[i] * yHist$counts[j]);
    }
  }
  #print(names);
  probs <- counts / sum(counts);
  result <- data.frame(names, counts, probs);
  return(result);
}

chiSquare <- function(observed, expected) {
  #TODO:  check that both are of the same length and are both numeric
  cs <- (observed - expected)^2/expected;
  print(pchisq(sum(cs), df=(length(expected) - 1), lower.tail=F));
}

#uses chi square to estimate if the two variables are independent by printing the p value from the chi square distribution
#uses the breakdown function, so requires "paired" values of x and y in that x[i] and y[i] should be variables of the same observation (ex. in the same row of a table)
#inupt is individual data points, not summations
chiSquareIndepTest <- function(x, y) {
  chiSquare(breakdown(x, y)$counts, jointDistIfIndep(x, y)$counts);
}

contains = function(x, s) {
  if(length(x[which(!is.na(x))]) < length(x) & is.na(s)) {
    return(TRUE)
  }
  if(sum(x == s, na.rm = TRUE) > 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

priorProb = function(vec, value) {
  return(sum(vec == value) / length(vec))
}

priorProbDF = function(data, col, value) {
  #todo - test that data is a frame, col is a valid column, value is a valid value for that column
  return (sum(data[col] == value) / (sum((data[col] == value) | (data[col] != value)))) #length(data[col]) was giving 1
}

#TODO better handling of NAs; for now just ignore them; assume that the probs for NA observations is the same as the prob of
#the known observations
#TODO - check that the NAs being removed match up between dataVec and givenVec
conditionalProb = function(dataVec, value, givenVec, givenValue) {
  #TODO - check for correct types; check that vectors are same length
  if(is.na(value) & is.na(givenValue)) {
    return(probNAGivenNA(dataVec, givenVec));
  } else if(!is.na(value) & is.na(givenValue)) {
    return(conditionalProbGivenNA(dataVec, value, givenVec));
  } else if(is.na(value) & !is.na(givenValue)) {
    return(probNAGiven(dataVec, givenVec, givenValue));
  } else if(!contains(givenVec, givenValue)) {
    return(DUMMY_PROBABILITY);
  } else {
    return(sum((dataVec == value) & (givenVec == givenValue), na.rm = TRUE) / sum(givenVec == givenValue, na.rm = TRUE))
  }
}

#calculates probability of specified value, given NA
conditionalProbGivenNA = function(dataVec, value, givenVec) {
  #TODO - check for correct types; check that vectors are same length
  if(is.na(value)) {
    return(probNAGivenNA(dataVec, givenVec));
  }
  if(sum(is.na(givenVec)) < 1) {
    #print("no nas");
    return(DUMMY_PROBABILITY);
  } else {
    return(sum((dataVec == value) & is.na(givenVec), na.rm=TRUE) / sum(is.na(givenVec)));
  }
}

#calculate the probatility of NA in dataVec given a specific value in givenVec
probNAGiven = function(dataVec, givenVec, givenValue) {
  if(is.na(givenValue)) {
    #need a probNaGivenNA function
  }
  if(!contains(givenVec, givenValue)) {
    return(DUMMY_PROBABILITY);
  } else {
    return(sum(is.na(dataVec) & (givenVec == givenValue), na.rm = TRUE) / sum(givenVec == givenValue, na.rm = TRUE))
  }
}

probNAGivenNA = function(dataVec, givenVec) {
  if(sum(is.na(givenVec)) == 0) {
    return(DUMMY_PROBABILITY);
  } else {
    return(sum(is.na(dataVec) & is.na(givenVec), na.rm = TRUE) / sum(is.na(givenVec)));
  }
}

condProb = function(dataVec, value, givenVec, givenValue) {
  return(conditionalProb(dataVec, value, givenVec, givenValue));
}

isNAGiven = function(dataVec, givenValue, givenVec) {
  if(!contains(givenVec, givenValue)) {
    return(DUMMY_PROBABILITY);
  } else {
    return(sum((is.na(dataVec) & (givenVec == givenValue)), na.rm=TRUE) / sum((givenVec == givenValue), na.rm=TRUE));
  }
}

conditionalProbNoNAs = function(dataVec, value, givenVec, givenValue) {
  #TODO - check for correct types; check that vectors are same length
  if(!contains(givenVec, givenValue)) {
    return(DUMMY_PROBABILITY);
  } else {
    return(sum((dataVec == value) & (givenVec == givenValue), na.rm = TRUE) / sum((givenVec == givenValue) & !is.na(dataVec), na.rm = TRUE))
  }
}

condProbNoNA = function(dataVec, value, givenVec, givenValue) {
  return(conditionalProbNoNAs(dataVec, value, givenVec, givenValue));
}

conditionalProbDF = function(data, col, value, givenCol, givenValue) {
  #todo - also try with sqldf
  #TODO - need to check that the vectors are the same length
  
  if(!contains(data[givenCol], givenValue)) {
    return(DUMMY_PROBABILITY);     #TODO - what to return here?
  } else {
    return (sum((data[col] == value) & data[givenCol] == givenValue) / (sum(data[givenCol] == givenValue)))
  }
}

fractionNA <- function(x) {
  sum(is.na(x)) / length(x);
}

fractionBlank <- function(x) {
  sum(x == "") / length(x);
}

fractionEmpty <- function(x) {
  empty <- sum(is.na(x));
  #print(empty);
  empty <- empty + sum(x == "", na.rm=TRUE);
  #print(empty);
  empty / length(x);
  #fraction <- empty / length(x);
  #print(fraction);
  #return(fraction);
}

obsEntropy = function(dataVec) {
  values = levels(as.factor(dataVec));
  #TODO:  learn how to write your own objects so you can return something here from factorHist
  probs = vector();
  probs <- sapply(1:length(values), function(j) { 
    priorProb(dataVec, values[j]);
  });
  return(entropy(probs));
}

entropy = function(probs) {
  if((sum(probs) - EPSILON > 1.0) | (sum(probs) + EPSILON < 1.0)) {
    stop("the probabilities must sum to 1")
  }
  return(-sum(probs * log2(probs)));
}

#shows a double sided confidence interval
#percent - usually 95; given as a percent, not a decimal (so 95, not .95)
confInt <- function(percent, mean, sd, sampleSize = 1) {
  sd <- sd / sampleSize ^ .5;
  lower <- (1 - percent/100)/2;
  upper <- 1 - lower;
  #print(paste(lower, upper));
  print(paste(qnorm(lower, mean, sd), qnorm(upper, mean, sd)));
}

confIntProportion <- function(percent, p, sampleSize) {
  if(p < 0 | p > 1) {
    stop("proportion must be between 0 and 1");
  }
  if(p * sampleSize < 10 | (1 - p) * sampleSize < 10) {
    stop("need 10 or more expected successes and failures");
  }
  stderr <- sqrt(p * (1 - p) / sampleSize);
  confInt(percent, p, stderr);
}

confIntDiffProps <- function(percent, p1, p2, n1, n2) {
  stderr <- sqrt(p1*(1 - p1)/n1 + p2*(1 - p2)/n2)
  confInt(percent, p1 - p2, stderr);
}

power <- function(percent, obsMean, obsSD, popMean, popSD, sampleSize = 1) {
  obsSD <- obsSD / sampleSize ^ .5;
  #print(obsSD);
  lower <- (1 - percent/100)/2;
  upper <- 1 - lower;
  #print(lower);
  #print(upper);
  #print(paste(lower, mean, obsSD));
  #print(paste(upper, mean, obsSD));
  lowerTail = qnorm(lower, obsMean, obsSD);
  upperTail = qnorm(upper, obsMean, obsSD);
  #print(paste(lowerTail, upperTail));
  popSD <- popSD / sampleSize ^ .5;
  lowerProb = pnorm(lowerTail, popMean, popSD);
  upperProb = pnorm(upperTail, popMean, popSD, lower.tail=F);
  return(lowerProb + upperProb);
}

echo <- function(x) {
  x
}

pOfB1 <- function(x, y) {
  beta1 <- cor(y, x) * sd(y) / sd(x)
  beta0 <- mean(y) - beta1 * mean(x)
  e <- y - beta0 - beta1 * x
  sigma <- sqrt(sum(e^2) / (n-2)) 
  ssx <- sum((x - mean(x))^2)
  seBeta0 <- (1 / n + mean(x) ^ 2 / ssx) ^ .5 * sigma 
  seBeta1 <- sigma / sqrt(ssx)
  tBeta0 <- beta0 / seBeta0; 
  tBeta1 <- beta1 / seBeta1
  pBeta0 <- 2 * pt(abs(tBeta0), df = n - 2, lower.tail = FALSE)
  pBeta1 <- 2 * pt(abs(tBeta1), df = n - 2, lower.tail = FALSE)
  pprint(2 * pt(abs(tBeta1), df = n - 2, lower.tail = FALSE))
  coefTable <- rbind(c(beta0, seBeta0, tBeta0, pBeta0), c(beta1, seBeta1, tBeta1, pBeta1))
  colnames(coefTable) <- c("Estimate", "Std. Error", "t value", "P(>|t|)")
  rownames(coefTable) <- c("(Intercept)", "x")
  coefTable
}

#TODO: error checking
pNumHeads <- function(numHeads, numTosses, pHeads) {
  choose(numTosses, numHeads) * pHeads^numHeads *  (1 - pHeads)^(numTosses - numHeads);
}

#probability of a loaded coin, given n heads in a row
pFairGivenNHeads <- function(n, priorPFair=.99, phGivenl = .6) {
  phGivenf <- .5;
  
  pFairGivenNh <- (phGivenf ^ n * priorPFair) / (phGivenf ^ n * priorPFair + phGivenl ^ n * (1 - priorPFair));
  return(pFairGivenNh);
}

#tosses should be a binary vector
pFair <- function(tosses, priorPFair=.99, phGivenL = .6) {
  n <- length(tosses);
  h <- sum(tosses);
  t <- n - h;
  numCombos <- choose(n, h);
  pTossesGivenFair <- numCombos * .5^n;
  pTossesGivenLoaded <- numCombos * phGivenL ^ h * (1 - phGivenL)^t;
  
  pFairGivenTosses <- pTossesGivenFair * priorPFair / (pTossesGivenFair * priorPFair + pTossesGivenLoaded * (1-priorPFair));
  return(pFairGivenTosses);
}

#b is branching factor
treeSize <- function(b, depth) {
  level <- 0:depth;
  #i <- 0;
  #s <- sapply(n, function(n) { i <- i + 1;print(i); b ^ i; });
  level_size <- vector();
  for(i in 0:depth) {
    level_size <- c(level_size, b ^ i);
  }
#   total <- sapply(s, function(s) { 
#     if(i == 1) { 
#       total[i] <- s[i];
#     } else {
#       total[i] <- total[i - 1] + s[i]};
#     })
  #total <- vector();
  total = level_size[1];
  #total <- c(level_size[1], level_size[1] + level_size[2]);
  for(i in 2:(depth + 1)) {
    total <- c(total, total[i - 1] + level_size[i]);
  }
  result <- data.frame(level, level_size, total);
  return(result);
}

coinTossBatches <- function(pHeads, numTosses, numBatches) {
  numHeads <- vector();
  for(i in 1:numBatches) {
    numHeads <- c(numHeads, sum(rbinom(numTosses, 1, pHeads)));
  }
  #sum(numHeads == targetNumHeads) / numTosses;
  numHeads;
}

coinTossBatchesSim <- function(pHeads, nTosses, numBatches) {
  total <- 0;
  results <- vector();
  for(i in 0:nTosses) {
    current <- sum(coinTossBatches(pHeads, nTosses, numBatches) == i)/numBatches;
    results <- c(results, current);
    #total <- total + current;
    #print(current);
  }
  #print(total);
  results;
}

pXGivenTheta <- function(numHeads, numTosses) {
  h <- numHeads;
  n <- numTosses;
  choose(n, h) * (1 / factorial(h + 1) - 1 / factorial(n + 1));
}

chooseSteps <- function(size = 10, returnList = FALSE) {
  result <- choose(size, 0:size);
  plot(0:size, result, type="l");
  if(returnList) {
    return(result);
  }
}