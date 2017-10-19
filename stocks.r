SEED_VALUE <- 857902;

findDeviations <- function(x, increments=c(1,7,30,365)) {
  #print(increments);
  maxes <- vector();
  mins <- vector();
  means <- vector();
  sds <- vector();
  for(i in 1:length(increments)) {
    #print(increments[i]);
    #print(paste('will do diffs for ', increments[i]));
    if(increments[i] >= length(x)) {
      next;
    }
    indeces <- seq(1, length(x), by=increments[i]);
    #print(indeces);
    diffs <- vector();
    #for(i in 2:length(x)) {
    for(j in 2:length(indeces)) {
      #for(i in 2:length(x)) {
      #print(paste('subtracting', x[indeces[j - 1]], "from", x[indeces[j]], "to get", x[indeces[j]] - x[indeces[j - 1]]));
      diffs <- c(diffs, x[indeces[j]] - x[indeces[j - 1]]);
      #}
    }
    print(paste('diffs for ', increments[i]));
    #print(diffs);
    s <- summary(diffs);
    print(s);
    print(paste("sd:  ", sd(diffs)));
    mins <- c(mins, s[1]);
    means <- c(means, s[4]);
    maxes <- c(maxes, s[6]);
    sds <- c(sds, sd(diffs))
  }
  plot(mins, pch=19);
  plot(means, pch=19);
  plot(maxes, pch=19);
  plot(sds, pch=19);
}

findDiffs <- function(x, indeces = 1:length(x)) {
  diffs <- vector();
  for(j in 2:length(indeces)) {
    diffs <- c(diffs, x[indeces[j]] - x[indeces[j - 1]]);
  }
  return(diffs);
}

findDiffRatios <- function(x, indeces = 1:length(x)) {
  diffs <- vector();
  for(j in 2:length(indeces)) {
    diffs <- c(diffs, x[indeces[j]] / x[indeces[j - 1]]);
  }
  return(diffs);
}

findDistributions <- function(x, increments=c(1,7,30,365)) {
  for(i in 1:length(increments)) {
    if(increments[i] >= length(x)) {
      next;
    }
    indeces <- seq(1, length(x), by=increments[i]);
    diffs <- findDiffs(x, seq(1, length(x), by=increments[i])); #vector();
    #for(j in 2:length(indeces)) {
    #  diffs <- c(diffs, x[indeces[j]] - x[indeces[j - 1]]);
    #}
    print(paste('diffs for ', increments[i]));
    
    s <- summary(diffs);
    print(s);
    print(paste("sd:  ", sd(diffs)));
    hist(diffs, main=paste(increments[i], " days"));
    qqnorm(diffs);
    qqline(diffs);
  }
  
  
}

#Does a monte carlo simulation to track the S&P 500 and shows a series of plots
doSNPPlotSimulation <- function() {
  snp <- read.csv("sp500.csv");
  snp <- snp[order(snp$Date),]  #reverse it because the file gives the latest days first
  oneday <- findDiffs(snp$Close);
  plot(cumsum(oneday), type="l", main="Plot of S&P 500 from Jan. 3, 1950 to April 11, 2014", 
       ylab="S & P 500", xlab="days after Jan. 1, 1950");
  
  #now do a series of plots, randomly drawing from the one day difference in closing prices
  set.seed(SEED_VALUE);
  for(i in 1:10) {
    plot(cumsum(sample(oneday, length(oneday), T)), type="l", main="randomly generated plot of S&P 500 from Jan. 3, 1950 to April 11, 2014, by difference", 
         ylab="S & P 500", xlab="days after Jan. 1, 1950")
  }
  set.seed(SEED_VALUE);
  onedayr <- findDiffRatios(snp$Close);
  for(i in 1:10) {
    plot(16.66 * cumprod(sample(onedayr, length(onedayr), T)), type="l", main="randomly generated plot of S&P 500 from Jan. 3, 1950 to April 11, 2014 by percent", 
         ylab="S & P 500", xlab="days after Jan. 1, 1950")
  }
}

##Does a simulation of the end value of the S&P 500
#similar to doSNPPlotSimulation except it shows the amount you would have on the last day instead of showing the plots
doSNPReturnSimulation <- function(numRuns = 1000, initial = 16.66, setSeed = F) {
  snp <- read.csv("sp500.csv");
  snp <- snp[order(snp$Date),]  #reverse it because the file gives the latest days first
  oneDayRatios <- findDiffRatios(snp$Close);
  if(setSeed) {
    set.seed(SEED_VALUE);
  }
  
  endValues <- vector();
  for(i in 1:numRuns) {
    endValue <- initial * cumprod(sample(oneDayRatios, length(oneDayRatios), T));
    #print(endValue[length(endValue)]);
    endValues <- c(endValues, endValue[length(endValue)]);
  }
  return(endValues);
}

timeToDouble <- function(rate) {
  log(2, rate);
}

#rate is the rate for an interval of 1, granularity is how often it is compounded for an interval of 1, so if daily compounding and rate is yearly rate, it would be 365
getValueAfter <- function(time, initialInvestment, rate, rateGranularity = 365, contribution, contributionInterval) {
  #check that rateGranulatiry is in integer and greater than 1
  rate <- 1 + rate/rateGranularity;
  intervals <- time * rateGranularity;
  total <- initialInvestment;
  
  for(i in 1:intervals) {
    if(i %% contributionInterval == 0) {
      total <- total + contribution;
      #print(paste("   ", total));
    }
    total <- total * rate;
    #print(paste(i, total, sep = " "));
  }
  total;
}

getTimeToValue <- function(targetValue, initialInvestment, rate, rateGranularity = 365, contribution, contributionInterval) {
  if(targetValue < initialInvestment) {
    return(getTimeToValueDecreasing(targetValue, initialInvestment, rate, rateGranularity, contribution, contributionInterval));
  }
  rate <- 1 + rate/rateGranularity;
  total <- initialInvestment;
  interval = 0;
  while(total < targetValue) {
    if(interval %% contributionInterval == 0) {
      total <- total + contribution;
    }
    total <- total * rate;
    interval <- interval + 1;
  }
  interval / rateGranularity;
}

getTimeToValueDecreasing <- function(targetValue, initialInvestment, rate, rateGranularity = 365, payment, contributionInterval) {
  rate <- 1 + rate/rateGranularity;
  total <- initialInvestment;
  interval <- 0;
  while(total > targetValue) {
    if(interval %% contributionInterval == 0) {
      total <- total - payment;
    }
    total <- total * rate;
    interval <- interval + 1;
  }
  interval / rateGranularity;
}

getRequiredContribution <- function(time, targetValue, initialInvestment, rate, rateGranularity = 365, contributionInterval) {
  rate <- (1 + rate/rateGranularity)^rateGranularity;
  intervals <- time * rateGranularity;
  
}

getRequiredContributionOld <- function(time, targetValue, initialInvestment, rate, rateGranularity = 365, contributionInterval) {
  rate <- 1 + rate/rateGranularity;
  intervals <- time * rateGranularity;
  #interval <- 0;
  #total <- targetValue;
  
  #calculate backwards from the target value
  #contribution <- getRequiredContribution(time - interval / rateGranularity, total, rate, rateGranularity, contributionInterval);
  if(intervals <= contributionInterval * rateGranularity) {
    #at the very beginning of the investment period
    total <- initialInvestment;
    for(interval in 1:intervals) {
      total <- total * rate;
    }
    #total is the amount that you will have after the initial period, before the first contribtion (say in the first two weeks of biweekly stock purchases)
    #this amount is known based on the parameters and is not based on the contribution amount
    #so the amount you need to invest is the difference between this known total based in interest and the target value
    return(targetValue - initialInvestment);
  }
  
  total <- targetValue;
  for(interval in intervals:1) {
    total <- total/rate;
    if(interval %% contributionInterval == 0) {
      #total <- total - contribution;
      contribution <- getRequiredContribution((time - (intervals - interval)) / rateGranularity, total, initialInvestment, rate, rateGranularity, contributionInterval);
      return(contribution);
    }
  }
}
  