sapierskyCarpet <- function(depth = 4) {
  if(depth < 1) {
    stop("can't have a depth less than 1");
  }
  width = 3 ^ depth;
  carpet <- matrix(1, nrow=width, ncol=width);
  carpet[(width/3 + 1):(2 * width/3),(width/3 + 1):(2 * width/3)] <- 0;
  if(depth > 1) {
    for(i in 0:2) {
      for(j in 0:2) {
        if(i != 1 || j != 1) {
        iStart <- 1 + (width / 3) * i;
        iEnd <- iStart + width / 3 - 1;
        jStart <- 1 + (width / 3) * j;
        jEnd <- jStart + width / 3 - 1;
        #print(paste(iStart, iEnd, jStart, jEnd, sep=" "));
        #print(carpet[iStart:iEnd, jStart:jEnd]);
        carpet[iStart:iEnd, jStart:jEnd] <- sapierskyCarpet(depth - 1);
        #print(sapierskyCarpet(depth - 1));
        }
      }
    }
  }
  image(carpet, col=c("white", "black"));
  return(carpet);
}