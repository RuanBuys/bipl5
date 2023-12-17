#' Obtain Ticksmarks of the biplot axes
#'
#' @param ellip Smaller ellipse dictating axes lengths
#' @param gradient Vector of axes gradients
#' @param p Number of axes
#' @param V V matrix px2 from SVD
#' @param mu Vector of Column Means
#' @param stddev Vector of Column standard deviations
#' @param ticks Vector dictiating the number of tickmarks. In order of columns
#'
#' @noRd
tickmarks<-function(ellip,gradient,p,V,mu,stddev,ticks=4){
  #V,x,tickoverride
  ticks<-rep(4,p)
  #first need to construct rotation matrix to determine upper and lower bounds
  #of each line segment

  #need to use the gradient vector
  thetas<-atan(gradient)
  RotMatrix<-RotationConstructor(thetas)
  RotatedElip<-ellip%*%RotMatrix

  #now need to find min and max value of each x-coordinate
  Ranges<-matrix(nrow=2,ncol=2)
  axes<-list()
  for(i in 1:p){
    Ranges[1,]<-c(min(RotatedElip[,2*i-1]),0)
    Ranges[2,]<-c(max(RotatedElip[,2*i-1]),0)
    Xhats<-(Ranges%*%RotationConstructor(-thetas[i])%*%t(V)[,i])*stddev[i]+mu[i]
    #okay get a pretty sequence of tickmarks and make sure lies on axis
    interval<-pretty(Xhats,n=ticks[i])
    ticks_x<-interpolate(interval,Ranges[,1],Xhats)
    ticks_coors<-cbind(ticks_x,rep(0,length(ticks_x)))
    #hos tokkelos rotate them back

    if(Xhats[2,1]-Xhats[1,1]<0)
      #need to reverse ordering cause pretty only ascending
      interval<- interval[order(interval,decreasing = TRUE)]
    axes[[i]]<-cbind(ticks_coors%*%RotationConstructor(-thetas[i]),interval)

  }
  return(axes)
}





#' Interpolate sequence of points
#'
#' Used to interpolate the tickmarks on the axes. Called by tickmarks function
#'
#' @param vect Sequence of points from wich we want to
#'             interpolate -> y-coordinates
#' @param Range1 x-coordinates of endpoints of interpolation
#' @param Range2 y-coordinates of endpoints of interpolation
#'
#' @return Vector of x-coordinates corresponding to y-coordinates
#' @noRd
interpolate<-function(vect,Range1,Range2){
  range_1<-max(Range1)-min(Range1)
  range_2<-max(Range2)-min(Range2)
  interps<-numeric()
  n<-length(vect)
  for(i in 1:n){
    interps[i]<-(vect[i]-min(Range2))/range_2*range_1+min(Range1)
  }
  return(interps)
}



#' Get equation of line spanned by two points
#'
#' @param p1 Point 1 vector
#' @param p2 Point 2 vector
#'
#' @return vector containing slope and intercept
#' @noRd
equation<-function(p1,p2){
  #need to include possibility of INF slope!
  b<-(p1[2]-p2[2])/(p1[1]-p2[1])
  a<-p1[2]-b*p1[1]
  return(c(b,a))
}





