
DUMMY_PROBABILITY = .0001;

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

#See also the table function.  This function gives frequencies/probabilities, which table does not do, and prints the data differently.
breakdown = function(x, y) {
  #if(class(x) != 'numeric' | class(y) != 'numeric') {
  #  return(-1);
  #}
  if(length(x) != length(y)) {
    return(-1);
  }
  
  xValues = levels(as.factor(x));
  if(sum(is.na(x) > 0)) {
    xValues = c(xValues, NA)
  }
  
  yValues = levels(as.factor(y));
  if(sum(is.na(y)) > 0) {
    yValues = c(yValues, NA);
  }
  #print(xValues);
  #print(yValues);
  #output = data.frame(ncol=4);
  #names(output) = c("x", "y", "p(x | y)", "p(y | x)");
  #output = data.frame(c("x", "y", "p(x | y)", "p(y | x)"));
  #print(paste(paste(paste("x", "y", sep = " "), "P(x | y)", sep=", "), "p(y | x)", sep=", "));
  xNames = vector();
  yNames = vector();
  counts = vector();
  xGivenY = vector();
  yGivenX = vector();
  for(i in 1:length(xValues)) {
    for(j in 1:length(yValues)) {
      xGivenY = c(xGivenY, condProb(x, xValues[i], y, yValues[j]));
      yGivenX = c(yGivenX, condProb(y, yValues[j], x, xValues[i]));
      counts = c(counts, sum((x == xValues[i]) & (y == yValues[j])));
      #print(paste(paste(paste(xValues[i], yValues[j], sep = " "), xGivenY, sep=", "), yGivenX, sep=", "));
      #xNames[i + j - 1] = xValues[i];
      #yNames[i + j - 1] = yValues[j];
      #TODO:  figure out why above two lines don't work
      xNames = c(xNames, xValues[i]);
      yNames = c(yNames, yValues[j]);
      #print(paste(xNames[i + j - 1], yNames[i + j - 1], xGivenY[i + j - 1], yGivenX[i + j - 1], sep = " "));
    }
  }
  #print(xNames);
  #print(yNames);
  #print(xGivenY);
  #print(yGivenX);
  output = data.frame(xNames, yNames, counts, xGivenY, yGivenX);
  #names(output) = c("x", "y", "p(x | y)", "p(y | x)");
  #print(output);
  #print("___");
  #doubleHist(x, y);
  return(output)
}

breakdown2 = function(x, y) {
  t1 = table(x, y, useNA="ifany");
  t2 = table(y, x, useNA="ifany")
  rownames(t1) = replace(rownames(t1), is.na(rownames(t1)), "NA or unknown");
  colnames(t1) = replace(colnames(t1), is.na(colnames(t1)), "NA or unknown");
  rownames(t2) = replace(rownames(t2), is.na(rownames(t2)), "NA or unknown");
  colnames(t2) = replace(colnames(t2), is.na(colnames(t2)), "NA or unknown");
  
  print(colnames(t1));
  print(colnames(t2));
  
  barplot(t1, legend=rownames(t1));
  barplot(t2, legend=rownames(t2))
  #print(t);
  numRows = length(row.names(t1));
  numCols = length(t1) / numRows;
  
  tf = as.data.frame(t1);
  tf = tf[order(tf$x),];
  names(tf) = c("x", "y", "counts");
  #print(tf);
  xGivenY = tf$counts[which(tf$x == tf$x && tf$y == tf$y)] / sum(tf$counts[which(tf$y == tf$y)]);
  xGivenY = sapply(tf$counts, function(i) { i  });
  xGivenY = apply(tf, 1, function(i) { total <- sum(tf$counts[which(tf$y == i[2])]);
                                       prob <- as.numeric(i[3]) / total; 
                                       return(prob);
                                      });
  
  yGivenX = apply(tf, 1, function(i) { total <- sum(tf$counts[which(tf$x == i[1])]); 
                                       as.numeric(i[3]) / total });
  tf <- cbind(tf, xGivenY);
  tf <- cbind(tf, yGivenX);
  print(tf);
  print(sum(counts))
  
}

#containsOld = function(x, s) {
#  for(i in 1:length(x)) {
#    if(x[i] == s) {
#      return(TRUE);
#    }
#  }
#  return(FALSE);
#}

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


obsEntropy = function(dataVec) {
  values = levels(as.factor(dataVec));
  #TODO:  learn how to write your own objects so you can return something here from factorHist
  probs = vector();
  #for(i in 1:length(values)) {
  #  probs[i] = priorProb(dataVec, values[i]);
  #  print(paste(values[i], probs[i], sep="  "));
  #}
  probs <- sapply(1:length(values), function(j) { 
    print(paste(values[j], priorProb(dataVec, values[j]), sep="  "));
    priorProb(dataVec, values[j]);
    });
  return(entropy(probs));
}

entropy = function(probs) {
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