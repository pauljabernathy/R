
triangle = function(numPoints = 10000) {
  print(date());
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
    #points(currentX, currentY);
  }
  plot(xPoints, yPoints, pch=20);
  print(date());
}

#triangle2 seems to take almost 4 times as long as triangle
triangle2 = function(numPoints = 10000) {
  print(date());
  currentX <- .3#runif(1);
  currentY <- .8#runif(1);
  
  xCorners <- c(0, 1, .5);
  yCorners <- c(0, 0, 3^.5 / 2);
  
  xPoints <- vector();
  yPoints <- vector();
  
  xPoints <- c(xPoints, currentX);
  yPoints <- c(yPoints, currentY);
  thePoints <- list();
  thePoints[[1]] <- xPoints;
  thePoints[[2]] <- yPoints;
  
  for(i in 1:numPoints) {
    p <- addOnePoint(thePoints, xCorners, yCorners)
    thePoints[[1]] <- c(thePoints[[1]], p[1]);
    thePoints[[2]] <- c(thePoints[[2]], p[2]);
  }  
  plot(thePoints[[1]], thePoints[[2]], pch=20);
  print(date());
}

#tried to use sapply or lapply and it is not working, 
#apparently because Xapply cannot handle the requirement to keep track of current point
triangle3 = function(numPoints = 10000) {
  currentX <- runif(1);
  currentY <- .8#runif(1);
  
  xCorners <- c(0, 1, .5);
  yCorners <- c(0, 0, 3^.5 / 2);
  
  xPoints <- vector();
  yPoints <- vector();
  
  xPoints <- c(xPoints, currentX);
  yPoints <- c(yPoints, currentY);
  thePoints <- list();
  thePoints[[1]] <- xPoints;
  thePoints[[2]] <- yPoints;
  
  thePoints <- lapply(1:10, addOnePoint, thePoints, xCorners, yCorners);
  return(thePoints);
}

addOnePoint <- function(thePoints, xCorners, yCorners) {
  #print(thePoints[[1]])
  currentX <- thePoints[[1]][length(thePoints[[1]])];
  currentY <- thePoints[[2]][length(thePoints[[2]])];
  
  corner <- randomInt(3);
  currentX <- (currentX + xCorners[corner]) / 2;
  currentY <- (currentY + yCorners[corner]) / 2;
  #print(paste(currentX, currentY, sep=" ,"));
  #xPoints <- c(xPoints, currentX);
  #yPoints <- c(yPoints, currentY);
  xPoints <- thePoints[[1]];
  yPoints <- thePoints[[2]];
  xPoints <- c(xPoints, currentX);
  yPoints <- c(yPoints, currentY);
  #print(length(xPoints));
  #print(length(thePoints[[1]]));
  #points(currentX, currentY);
  #thePoints[[1]] <- c(thePoints[[1]], currentX);
  #thePoints[[2]] <- c(thePoints[[2]], currentY);
  thePoints[[1]] <- xPoints;
  #print(length(thePoints[[1]]));
  thePoints[[2]] <- yPoints;
  #return(thePoints);
  return(c(currentX, currentY));
}

randomInt <- function(max) {
  return(ceiling(runif(1) * max));
}