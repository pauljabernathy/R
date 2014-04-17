#sum should be an integer
partitionOneNumProblems = function(sum, filename = 'partitions.csv') {
  nums = sample(sum, sum, replace=FALSE);
  otherNums = sum - nums;
  for(i in 1:sum) {
    #print(paste(sum, " = ", nums[i], " + ", otherNums[i]));
    
  }
  problems = data.frame(sum, '=', nums, rep('+', sum), otherNums);
  names(problems) = c('sum', 'equals','first', 'plus', 'second');
  problems;
  write.csv(problems, filename);
}

#sumsVector is a vector of integers; need to do partitioning for each number in it
partitionProblems = function(sumsVector, filename = 'partitions.csv') {
  
}