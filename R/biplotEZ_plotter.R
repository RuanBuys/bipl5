#' Plot object from the biplotEZ package in plotly
#'
#' @param x An object of class 'biplot' from the biplotEZ package
#'
#' @return A plotly graph containing the biplot
#' @export plot_bipl5
#'
#' @examples
#' library(biplotEZ)
#' x<-biplot(data = iris) |> PCA() |> plot_bipl5()
#' x<-biplot(iris[,1:4]) |> CVA(classes=iris[,5]) |> plot_bipl5()
#' x<-biplot(iris[,1:4]) |> PCO(dist.func = sqrtManhattan) |> plot_bipl5()
plot_bipl5<-function(x){
  if(x$dim.biplot != 2)
    stop("plot_bipl5 only accepts biplots of two dimensions")
  if(length(class(x))<2){
    if(!is.null(x$PCOaxes))
      class(x)<-c(class(x),"PCO")
  }
  UseMethod("plot_bipl5",x)
}



#' Plot a PCA biplot from the biplotEZ package
#'
#' @param x An object of class biplotEZ::biplot
#'
#' @return A plotly graph
#' @export
#' @method plot_bipl5 PCA
#' @S3method plot_bipl5 PCA
#'
#' @examples
#' x<-biplot(data = iris) |> PCA() |> plot_bipl5()
plot_bipl5.PCA<-function(x){
  if(is.null(x$samples))
    x<-biplotEZ::samples(x)
  #if(is.null(x$axes))
  #for now all the default settings of axes is supported. Might change later on
  x<-biplotEZ::axes(x)

  #next we need to obtain the coordinates of the other PC's
  #we first test if the current display is a correlation biplot:
  corr<-is_correlation(x)

  PC13<-biplotEZ::biplot(x$raw.X, center = x$center, scaled = x$scaled) |>
      biplotEZ::PCA(e.vects = c(1,3),correlation.biplot = corr)
  PC23<-biplotEZ::biplot(x$raw.X, center = x$center, scaled = x$scaled) |>
    biplotEZ::PCA(e.vects = c(2,3),correlation.biplot = corr)



  color<-x$samples$col
  scale<-x$scaled
  symbol<-pch_to_plotly(x$samples$pch)
  group<-x$group.aes
  if(length(levels(x$group.aes))==1)
    group<-factor(rep("Data",x$n))
  Z<-x$Z
  n<-x$n
  p<-x$p
  basis<-x$e.vects
  ax.aes<-x$axes
  n<-x$n
  eigval <- x$eigenvalues
  fit.quality <- fit_quality(eigval,basis)

  #build scaffolding ->biplotEZ helper
  p_ly<-plot_scaffolding(fit.quality,basis,TRUE,TRUE,TRUE,TRUE)

  payl_13 <- payload_new()
  payl_13$fit_qual<-fit_quality(PC13$eigenvales,PC13$e.vects)
  payl_13 <- plot_scaffolding_payload(payl_13, dpquality = fit.quality, basis = basis,
                                      PC_toggle = TRUE, ax_pred = TRUE, TDA = TRUE, vec_dis = TRUE)

  #Insert any polygons to the plot -> EZ plotly layers

  if(!is.null(x$alpha.bags)){
    p_ly<-insert_polygon_EZ(p_ly,x$alpha.bags,x$alpha.bag.aes)
  }
  if(!is.null(x$conc.ellipses)){
    p_ly<-insert_polygon_EZ(p_ly,x$conc.ellipses,
                            x$conc.ellipse.aes,"Con. Ellipses")
  }

  if (!is.null(x$Lmat))
    if (nrow(x$Lmat) == ncol(x$Lmat))
      Xhat <- x$Z %*% solve(x$Lmat)[x$e.vects,]
  else Xhat <- x$X
  else
    Xhat <- x$X
  if (x$scaled) Xhat <- scale(Xhat, center=FALSE, scale=1/x$sd)
  if (x$center) Xhat <- scale(Xhat, center=-1*x$means, scale=FALSE)


  z.axes<- biplotEZ::axes_coordinates(x)

  #insert Z coordinates ->PCAbiplot_Helper
  obj<-list(Z=Z,group=group,n=x$n,x=as.matrix(x$X),XHat=Xhat)
  p_ly<-insert_Z_coo(p_ly,obj,symbol,color,TRUE)

  payl_13 <- insert_Z_coo_payload(payl_13,
                                  obj, p_ly_pch = symbol,
                                  Col = color, visible = TRUE)


  #insert class means if any
  if(x$class.means){
    if(is.null(x$means.aes))
      x<-biplotEZ::means(x)
    Mean_symbol<-pch_to_plotly(x$means.aes$pch)
    p_ly<-insert_class_means(p_ly,x$Zmeans,Mean_symbol,x$means.aes$col)
    payl_13<-insert_class_means_payload(payl_13,
                                        x$Zmeans,
                                        Mean_symbol,
                                        x$means.aes$col)
  }

  #insert Linear Axes
  update<-insert_linear_axes(z.axes,x,p_ly)
  p_ly<-update[[1]]
  grads<-update[[2]]

  out <- insert_linear_axes_payload(payl_13, z.axes, x)
  payl_13 <- out$payload
  grads <- out$grads


  #Unit circle
  p_ly <- insert_unit_circle(p_ly, visible = FALSE)
  payl_13 <- insert_unit_circle_payload(payl_13, visible = FALSE)

  #insert axis details table
  p_ly<-InsertAxisDeets(p_ly,x,EZ=TRUE)
  payl_13<-InsertAxisDeets_payload(payl_13,x,EZ=TRUE)
  #insert vector representation

  temp<-list(V=x$Vr,x=x$X,p=x$p)
  p_ly<-insert_vector_annots(p_ly,temp,NULL,NULL)
  payl_13<-insert_vector_annots_payload(payl_13,temp)

  #insert Translated Density Axes
  p_ly<-add_TDA(z.axes,x,Z=Z,group=group,p_ly=p_ly,Col=color)
  payl_13<-add_TDA_payload(payload=payl_13,
                           z.axes=z.axes,
                           x=x,
                           Z=Z,
                           group=group,
                           Col=color)

  p_ly<-insert_linear_js_v1(p_ly,
                         m=grads,p=p,cols=x$axes$tick.label.col,
                         payload=payl_13$payload)

  return(p_ly)

}



#' Plot a CVA biplot from the biplotEZ package
#'
#' @param x An object of class biplotEZ::biplot
#'
#' @return A plotly graph
#' @export
#' @method plot_bipl5 CVA
#' @S3method plot_bipl5 CVA
#' @import biplotEZ
#'
#' @examples
#' library(biplotEZ)
#' x<-biplotEZ::biplot(iris[,1:4]) |> biplotEZ::CVA(classes=iris[,5]) |> plot_bipl5()
plot_bipl5.CVA<-function(x){
  if(is.null(x$samples))
    x<-biplotEZ::samples(x)
  if(is.null(x$axes))
    x<-biplotEZ::axes(x)
  if(is.null(x$means.aes))
    x<-biplotEZ::means(x)

  color<-x$samples$col
  scale<-x$scaled
  symbol<-pch_to_plotly(x$samples$pch)
  group<-x$group.aes
  if(length(levels(x$group.aes))==1)
    group<-factor(rep("Data",x$n))
  Z<-x$Z
  n<-x$n
  p<-x$p
  basis<-x$e.vects
  ax.aes<-x$axes

  #build scaffolding
  p_ly<-plot_scaffolding("",basis,FALSE,FALSE,FALSE,FALSE)

  #Insert any polygons to the plot

  if(!is.null(x$alpha.bags)){
    p_ly<-insert_polygon_EZ(p_ly,x$alpha.bags,x$alpha.bag.aes)
  }
  if(!is.null(x$conc.ellipses)){
    p_ly<-insert_polygon_EZ(p_ly,x$conc.ellipses,
                            x$conc.ellipse.aes,"Con. Ellipses")
  }

  if (!is.null(x$Lmat))
    if (nrow(x$Lmat) == ncol(x$Lmat))
      Xhat <- x$Z %*% solve(x$Lmat)[x$e.vects,]
  else Xhat <- x$X
  else
    Xhat <- x$X
  if (x$scaled) Xhat <- scale(Xhat, center=FALSE, scale=1/x$sd)
  if (x$center) Xhat <- scale(Xhat, center=-1*x$means, scale=FALSE)

  z.axes <- lapply(1:p, biplotEZ:::.calibrate.axis, Xhat, x$means,
                   x$sd, x$ax.one.unit, 1:p,
                   ax.aes$ticks, ax.aes$orthogx, ax.aes$orthogy)
  z.axes<- biplotEZ::axes_coordinates(x)

  # for(i in 1:p){
  #   z.axes[[i]]<-z.axes[[i]][[1]]
  # }


  #insert Z coordinates
  obj<-list(Z=Z,group=group,n=x$n,x=x$X)
  #p_ly<-insert_Z_coo(p_ly,obj,symbol,color,TRUE)

  num_groups<-length(levels(group))
  x<-fit.measures(x)
  fit<-round(x$within.class.sample.predictivity,3)
  for(i in 1:num_groups){
    p_ly<-p_ly |>
      add_trace(data=Z,x=Z[group==levels(group)[i],1],
                y=Z[group==levels(group)[i],2],name=levels(group)[i],
                type = "scatter", mode = "markers",
                hovertext=paste(rownames(x$X)[group==levels(group)[i]],
                                "\n","Predictivity:",fit[group==levels(group)[i]]),
                hoverinfo="text+name",
                customdata=(1:x$n)[group==levels(group)[i]],
                meta="data",xaxis="x",yaxis="y",visible=TRUE,
                marker=list(symbol=symbol[i],color=color[i],opacity=1),
                legendgroup="data",
                legendgrouptitle=list(text="<b>Data</b>"))
  }

  #insert class means if any
  if(x$class.means){
    Mean_symbol<-pch_to_plotly(x$means.aes$pch)
    p_ly<-insert_class_means(p_ly,x$Zmeans,Mean_symbol,x$means.aes$col)
  }

  #insert Linear Axes
  update<-insert_linear_axes(z.axes,x,p_ly)
  p_ly<-update[[1]]
  grads<-update[[2]]

  #Unit circle
  p_ly<-p_ly|>
    add_trace(x=cos(seq(0,2*pi,length.out=200)),
              y=sin(seq(0,2*pi,length.out=200)), type="scatter",
              mode="lines",line = list(color = 'red',width=1.2),
              name="Unit Circle",showlegend=FALSE,
              meta='veccircle',xaxis="x",yaxis="y",
              hoverinfo='name',visible=FALSE)



  p_ly<-insert_linear_js_v1(p_ly,m=grads,p=p,cols=x$axes$tick.label.col,payload=NULL)

  return(p_ly)
}




#' Plot a PCO biplot from the biplotEZ package
#'
#' @param x An object of class biplotEZ::biplot
#'
#' @return A plotly graph
#' @export
#' @method plot_bipl5 PCO
#' @S3method plot_bipl5 PCO
#'
#' @examples
#' x<-biplot(iris[,1:4]) |> PCO(dist.func = sqrtManhattan) |> plot_bipl5()
plot_bipl5.PCO<-function(x){
  if(is.null(x$samples))
    x<-biplotEZ::samples(x)
  if(is.null(x$axes))
    x<-biplotEZ::axes(x)
  color<-x$samples$col
  scale<-x$scaled
  symbol<-pch_to_plotly(x$samples$pch)
  group<-x$group.aes
  if(length(levels(x$group.aes))==1)
    group<-factor(rep("Data",x$n))
  Z<-x$Z
  n<-x$n
  p<-x$p
  basis<-x$e.vects
  ax.aes<-x$axes


  if (!is.null(x$Lmat))
    if (nrow(x$Lmat) == ncol(x$Lmat))
      Xhat <- x$Z %*% solve(x$Lmat)[x$e.vects,]
  else Xhat <- x$X
  else
    Xhat <- x$X
  if (x$scaled) Xhat <- scale(Xhat, center=FALSE, scale=1/x$sd)
  if (x$center) Xhat <- scale(Xhat, center=-1*x$means, scale=FALSE)

  if(x$PCOaxes == "splines"){
      z.axes <- spsUtil::quiet(lapply(1:p,
                                      biplotEZ:::biplot.spline.axis, Z, Xhat,
                                      means=x$means, sd=x$sd, n.int=ax.aes$ticks,
                                      spline.control=x$spline.control))

  #build scaffolding
  p_ly<-plot_scaffolding("",basis,FALSE,FALSE,FALSE,FALSE)

  #insert Z coordinates
  obj<-list(Z=Z,group=group,n=n,x=x$X)
  p_ly<-insert_Z_coo(p_ly,obj,symbol,color,TRUE)




  radius<-max(abs(Z))*1.2
  theta<-seq(0,2*pi,length.out=200)
  elipcoords<-cbind(radius*cos(theta),radius*sin(theta))


  z.axes<-check_inside_circle(z.axes,radius,NULL)


  for(i in 1:p){
    AxName<-paste("<b>",colnames(x$X)[i],"</b>")
    endp<-z.axes[[i]][which.max(z.axes[[i]][,3]),1:2]
    pos<-"right"
    if(endp[1]<0){
      pos<-"left"
    }
    idx<-which(z.axes[[i]][,4]==1)

    full_m<-get_gradients(z.axes[[i]])
    m<-full_m[idx]
    if(any(is.na(m))){
      idx<-idx[!is.na(m)]
      m<-m[!is.na(m)]
    }

    p_ly<-p_ly |>
      add_trace(x=z.axes[[i]][,1],
                y=z.axes[[i]][,2],
                type="scatter",
                mode="lines",line = list(color = 'grey',width=1,simplify=FALSE),
                name=colnames(x$X)[i],legendgroup=paste("Ax",i,sep=""),
                meta='axis',xaxis="x",yaxis="y",customdata=full_m,
                visible=TRUE, hovertext=round(z.axes[[i]][,3],1),hoverinfo="text")|>

      add_annotations(x=z.axes[[i]][idx,1],y=z.axes[[i]][idx,2],
                      text=as.character(z.axes[[i]][idx,3]),
                      showarrow=FALSE,textangle=-atan(m)*180/pi,
                      visible=TRUE,yshift=-12*cos(atan(m)),
                      xshift=12*sin(atan(m)),meta='axis',
                      xaxis="x",yaxis="y",customdata=i,font=list(size=10))|>
      add_annotations(x=z.axes[[i]][idx,1],y=z.axes[[i]][idx,2],
                      text="&#124;",
                      showarrow=FALSE,textangle=-atan(m)*180/pi,
                      visible=TRUE,meta='axis',
                      xaxis="x",yaxis="y",customdata=i,font=list(size=8))|>
      add_trace(x=endp[1],
                y=endp[2],
                text=AxName,type="scatter",mode="text",textposition=pos,
                legendgroup=paste("Ax",i,sep=""),showlegend=FALSE,
                textfont=list(size=12),
                meta='axis',xaxis="x",yaxis="y",visible=TRUE)
  }


  p_ly<-p_ly|> add_trace(x=elipcoords[,1],y=elipcoords[,2], type="scatter",
                         mode="lines",line = list(color = 'green',width=0.6),
                         name="circle",showlegend=F,
                         meta='circle',xaxis="x",yaxis="y",
                         visible=TRUE,hoverinfo="none")
  p_ly<-insert_spline_js(p_ly,p)


  }


  if(x$PCOaxes == "regression"){
    z.axes <- lapply(1:p, biplotEZ:::.calibrate.axis, Xhat, x$means,
                     x$sd, x$ax.one.unit, 1:p,
                     ax.aes$ticks, ax.aes$orthogx, ax.aes$orthogy)


    for(i in 1:p){
      z.axes[[i]]<-z.axes[[i]][[1]]
    }
     Xhat<-x$Z %*% solve(t(x$Z) %*% x$Z) %*% t(x$Z) %*% x$X
     if (x$scaled) Xhat <- scale(Xhat, center=FALSE, scale=1/x$sd)
     if (x$center) Xhat <- scale(Xhat, center=-1*x$means, scale=FALSE)
    #build scaffolding
    p_ly<-plot_scaffolding("",basis,FALSE,FALSE,FALSE,FALSE)

    #insert Z coordinates
    obj<-list(Z=Z,group=group,n=n,x=x$X)
    p_ly<-insert_Z_coo(p_ly,obj,symbol,color,TRUE)


    update<-insert_linear_axes(z.axes,x,p_ly)
    p_ly<-update[[1]]
    grads<-update[[2]]

    p_ly<-insert_linear_js(p_ly,Xhat=Xhat,m=grads,p=p,cols=x$axes$tick.label.col)

    }





  return(p_ly)
}


