#' Contruct Rotation Matrix
#'
#' This function appends multiple rotation matrices against one another
#'
#' @param angles Vector of angles to which plot must be rotated
#'
#' @return rotation matrix n x 2*p
#' @noRd
RotationConstructor<-function(angles){
  mat<-matrix(NA,nrow=2,ncol=2*length(angles))
  n<-length(angles)
  for(i in 1:n){
    mat[1,(2*i-1):(2*i)]<-c(cos(angles[i]),-sin(angles[i]))
    mat[2,(2*i-1):(2*i)]<-c(sin(angles[i]),cos(angles[i]))
  }
  mat
}

#' Translate lines up or down out of big ellipse
#'
#' Translate lines out of rotated ellipse up or down such that no intersections
#' occur
#'
#' @param elip Rotated ellipse
#' @param quadrant quadrant in which orginal vector loading fell
#' @param other details of other lines' coordinates. Matrix px2
#' @param d minimum distance to shift
#' @param endpoints endpoints of current rotated line
#' @param theta angle of axis
#' @param swop Swop the quadrants to which must rotated - up instead of down
#'
#' @return List containing distance shifted, as well as final endpoints
#' @noRd
translate<-function(elip,quadrant,other,d,endpoints,theta,swop=FALSE){

  other<-other%*%RotationConstructor(theta)
  q1<-c(2,3)
  q2<-c(1,4)
  if(swop){
    q2<-c(2,3)
    q1<-c(1,4)
  }

  if(quadrant%in%q1){
    #shift line downward and subtract d
    btm<-min(elip[,2])
    endpoints[,2]<-min(other[,2],btm,na.rm=TRUE)-d
  }
  if(quadrant%in%q2){
    #shift line upward and add d
    top<-max(elip[,2])
    endpoints[,2]<-max(top,other[,2],na.rm=TRUE)+d
  }
  shifted<-endpoints[1,2]
  true_endpoints<-endpoints%*%RotationConstructor(-theta)
  returnvalues<-list(distance=shifted,ends=true_endpoints)
  return(returnvalues)
}


#' bipl5 default color scales
#'
#' @param number Integer - number of distinct colors to return. Ranges from
#'              1 to 16.
#'
#' @return Character vector of default colors in \code{bipl5}. There are
#'         sixteen unique colors defined.
#' @importFrom grDevices rgb
#' @export
#' @examples
#' colorpal(number=7)
colorpal<-function(number=16){
  if(!is.numeric(number))
    stop("Enter a numeric integer")
  if(number>16)
    stop("Only 16 unique colors are available")
  if(number==0)
    stop("Enter integer between 1 and 16")
  pal <- c(
    rgb(57 / 255, 106 / 255, 177 / 255),
    rgb(218 / 255, 124 / 255, 48 / 255),
    rgb(62 / 255, 150 / 255, 81 / 255),
    rgb(204 / 255, 37 / 255, 41 / 255),
    rgb(83 / 255, 81 / 255, 84 / 255),
    rgb(107 / 255, 76 / 255, 154 / 255),
    rgb(146 / 255, 36 / 255, 40 / 255),
    rgb(148 / 255, 139 / 255, 61 / 255)
  )
  pal2<-c(
    rgb(114 / 255, 147 / 255, 203 / 255),
    rgb(255 / 255, 151 / 255, 76 / 255),
    rgb(132 / 255,186 / 255, 91 / 255),
    rgb(211 / 255, 94 / 255, 96 / 255),
    rgb(128 / 255, 133 / 255, 133 / 255),
    rgb(144/ 255, 103 / 255, 167 / 255),
    rgb(171 / 255, 104 / 255, 87 / 255),
    rgb(204 / 255, 194/ 255, 16 / 255)

  )
  return(c(pal,pal2)[1:number])
}

