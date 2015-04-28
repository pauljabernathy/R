sierpinskiTriangle = function(numPoints = 10000) {
  currentX <- runif(1);
  currentY <- runif(1);
  
  xCorners <- c(0, 1, .5);
  yCorners <- c(0, 0, 3^.5 / 2);
  
  xPoints <- vector();
  yPoints <- vector();
  
  xPoints <- c(xPoints, currentX);
  yPoints <- c(yPoints, currentY);
  
  for(i in 1:numPoints) {
    corner <- randomInt(3);
    currentX <- (currentX + xCorners[corner]) / 2;
    currentY <- (currentY + yCorners[corner]) / 2;
    
    xPoints <- c(xPoints, currentX);
    yPoints <- c(yPoints, currentY);
  }
  plot(xPoints, yPoints, pch=20);
}

sierpinskiCarpet <- function(depth = 4, shouldReturn = FALSE) {
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
        carpet[iStart:iEnd, jStart:jEnd] <- sierpinskiCarpet(depth - 1, TRUE);
        }
      }
    }
  }
  if(shouldReturn) {
    return(carpet);
  } else {
    image(carpet, col=c("white", "black"));
  }
}