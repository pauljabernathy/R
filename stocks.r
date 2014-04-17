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