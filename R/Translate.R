#' Translate lines parallel out of ellipse
#'
#' @param elip Elip to translate lines out of
#' @param m Vector of gradients of axes
#' @param quadrant Quadrant in which orginal vector loading fell
#' @param d Distance to shift
#' @param initial_ends Initial endpoints of untranslated axes
#' @param swop Swop the quadrants to which must rotated - up instead of down
#'
#' @noRd
MoveLines<-function(elip,m,quadrant,d,initial_ends,swop,cols){
  p<-length(m)
  thetas<-atan(m)
  RotMatrix<-RotationConstructor(thetas)
  RotatedElip<-elip%*%RotMatrix

  distShifted<-numeric(p)

  FinalPos2<-matrix(NA,nrow=1,ncol=2)
  final_enders<-list()
  #customise the ordering
  ordering<-order(m,decreasing = TRUE)
  Axis<-list() #just stores the axis names
  for(i in ordering){
    ends<-initial_ends[[i]][,-3]%*%RotMatrix[,(2*i-1):(2*i)]
    translate_deets<-translate(RotatedElip[,(2*i-1):(2*i)],quadrant[i],
                               FinalPos2,d,ends,thetas[i],swop)
    FinalPos2<-rbind(FinalPos2,translate_deets$ends)
    distShifted[i]<-translate_deets$distance
    final_enders[[i]]<-cbind(translate_deets$ends,initial_ends[[i]][,3])
    Axis[[i]]<-rep(cols[i],nrow(final_enders[[i]]))
  }
  retVals<-list(ShiftDist=distShifted,ends=final_enders,Axes=Axis)
  return(retVals)
}






#' Translate densities outward toward respective axes
#'
#' @param Z Z coordinates of the datapoints
#' @param m vector of slopes
#' @param endpoints endpoints of each axis
#' @param dist distance each axis is moves
#' @param dinflation inflation factor for the densities
#' @param group grouping vector of data
#' @importFrom stats density
#' @return list giving the coordinates of the densities per group
#' @noRd
MoveDensities<-function(Z,m,endpoints,dist,dinflation,group,densityargs=NULL){
  if(is.null(densityargs$n)) densityargs$n<-128

  num<-length(unique(group))
  p<-length(m)
  thetas<-atan(m)
  RotMatrix<-RotationConstructor(thetas)
  RotatedZ<-Z%*%RotMatrix

  #first rotate the endpoints to find min and max value
  #then take density per category
  #get the x,y coordinates of that density, shift up or down, back rotate it

  Density_per_group<-list()
  for(i in 1:p){
    rotend<-endpoints[[i]][,-3]%*%RotMatrix[,(2*i-1):(2*i)]
    low<-min(rotend[,1])
    up<-max(rotend[,1])
    back_rotate<-RotationConstructor(-thetas[i])
    for(j in 1:num){
      densityargs$x<-RotatedZ[group==unique(group)[j],(2*i-1)]
      densityargs$from<-low
      densityargs$to<-up
      densdetails<-do.call(density,densityargs)
      y_coo<-(densdetails$y*dinflation+dist[i])
      coors<-cbind(densdetails$x,y_coo)

      if(length(Density_per_group)<j)
        Density_per_group[[j]]<-coors%*%back_rotate
      else
        Density_per_group[[j]]<-cbind(Density_per_group[[j]],
                                      coors%*%back_rotate)
    }
  }
  return(Density_per_group)
}




















