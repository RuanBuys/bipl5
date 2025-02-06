
#' Creates biplot with calibrated axes and vector loadings
#'
#' @param p_ly plotly object with only layout inserted already
#' @param x Object of class bipl5
#' @param symbol Plotting symbols per class group
#' @param color colors per class group
#' @param visible boolean: whether traces visible or not
#'
#' @return a list containing plotly graph, axes details, predicted values,
#'         number annotations, angles of axes
#' @noRd
add_vector_biplot<-function(p_ly,x,symbol,color,visible){

  Z<-x$Z
  p<-x$p
  n<-x$n
  mu<-x$mu
  stddev<-x$stddev
  group<-x$group
  m<-x$m
  quads<-x$quads
  Xhat<-Z%*%t(x$V) |> sweep(MARGIN = 2,STATS=stddev,FUN="*") |>
    sweep(MARGIN=2,STATS=mu,FUN="+")
  p_ly_pch<-symbol

  radius<-max(abs(Z))*1.2
  theta<-seq(0,2*pi,length.out=200)
  elipcoords<-cbind(radius*cos(theta),radius*sin(theta))
  endpoints<-tickmarks(ellip=elipcoords,gradient=m,p=p,
                       V=x$V, mu=mu,stddev=stddev)
  shift<-check_inside_circle(ticks=endpoints,r=radius,thetas=atan(m))

  #-------------------------------PLoTLY----------------------------
  #insert the Z coordinates
  if(is.null(color))
    Col<-colorpal(length(levels(group)))
  else
    Col<-color
  #insert the Z coordinates
  p_ly<-insert_Z_coo(p_ly,x,p_ly_pch,Col,visible)

  # Insert axes with the tick marks
  AnnotCounter<-numeric()
  angles<-list()
  for(i in 1:p){
    AnnotCounter[i]<-length(shift[[i]][,3])*2#peuter-------------------------
    index2<-which(shift[[i]][,3]== max(shift[[i]][,3],na.rm=TRUE))
    angle<-atan(shift[[i]][index2,2]/shift[[i]][index2,1])
    AxName<-paste("<b>",colnames(x$x)[i],"</b>")
    pos<-"right"
    quads<-getquad(x$V,x$m)

    if(quads[i]==3){
      angle<-angle-pi
      pos<-"left"
    }
    if(quads[i]==2){
      angle<-angle-pi
      pos<-"left"
    }

    angles[[i]]<-list(x=-10*sin(atan(x$m[i])),y=10*cos(atan(x$m[i])))
    p_ly<-p_ly |>
      add_trace(x=shift[[i]][,1],y=shift[[i]][,2],
                type="scatter", mode="markers",
                marker=list(color="grey",size=4),
                name=colnames(x$x)[i],legendgroup=paste("Ax",i,sep=""),
                meta='axis',xaxis="x",yaxis="y",customdata=i,
                hoverinfo='name',visible=F,showlegend=FALSE) |>

      add_trace(x=c(radius*cos(atan(m[i])),radius*cos(atan(m[i])-pi)),
                y=c(radius*sin(atan(m[i])),radius*sin(atan(m[i])-pi)),
                type="scatter",
                mode="lines",line = list(color = 'grey',width=1),
                name=colnames(x$x)[i],legendgroup=paste("Ax",i,sep=""),
                meta='axis',xaxis="x",yaxis="y",customdata=i,
                hoverinfo='name',visible=visible)|>

      add_annotations(x=shift[[i]][,1],y=shift[[i]][,2],
                      text=as.character(shift[[i]][,3]),
                      showarrow=FALSE,textangle=-atan(x$m[i])*180/pi,
                      visible=visible,yshift=-12*cos(atan(x$m[i])),
                      xshift=12*sin(atan(x$m[i])),meta='axis',
                      xaxis="x",yaxis="y",customdata=i,font=list(size=10) )|>
      #-----------------------------------------------------------PEUTER------------
      add_annotations(x=shift[[i]][,1],y=shift[[i]][,2],
                      text=" &#124;",
                      showarrow=FALSE,textangle=-atan(x$m[i])*180/pi,
                      visible=visible,meta='axis',
                      xaxis="x",yaxis="y",customdata=i,font=list(size=8) )|>
      #----------------------------------------------------------------------------
      add_trace(x=radius*cos(angle),y=radius*sin(angle),
                text=AxName,type="scatter",mode="text",textposition=pos,
                legendgroup=paste("Ax",i,sep=""),showlegend=FALSE,
                textfont=list(size=12),
                meta='axis',xaxis="x",yaxis="y",visible=visible)



  }
  p_ly<-p_ly|> add_trace(x=elipcoords[,1],y=elipcoords[,2], type="scatter",
                   mode="lines",line = list(color = 'green',width=0.6),
                   name="circle",showlegend=FALSE,
                   meta='circle',xaxis="x",yaxis="y",
                   visible=visible,hoverinfo="none")

  #---------Get equations of shifted axes for prediction lines------------
  slope<-numeric()
  intercept<-numeric()
  for(i in 1:p){
    deets<-equation(shift[[i]][1,-3],shift[[i]][2,-3])
    slope[i]<-deets[1]
    intercept[i]<-deets[2]
  }
  df<-data.frame(m=slope,c=intercept)



  return(list(p_ly,df,Xhat,counter=sum(AnnotCounter),angles))

}




#' Insert Z coordinates to plot
#'
#' @param p_ly Plotly object after scaffolding
#' @param x Biplot object containing Z, group
#' @param p_ly_pch plotting characters
#' @param Col Colors
#' @param visible Show trace or not
#' @noRd
insert_Z_coo<-function(p_ly,x,p_ly_pch,Col,visible=TRUE){
  num_groups<-length(levels(x$group))
  for(i in 1:num_groups){
    p_ly<-p_ly |>
      add_trace(data=x$Z,x=x$Z[x$group==levels(x$group)[i],1],
                y=x$Z[x$group==levels(x$group)[i],2],name=levels(x$group)[i],
                type = "scatter", mode = "markers",
                hovertext=rownames(x$x)[x$group==levels(x$group)[i]],
                hoverinfo="text+name",
                customdata=(1:x$n)[x$group==levels(x$group)[i]],
                meta="data",xaxis="x",yaxis="y",visible=visible,
                marker=list(symbol=p_ly_pch[i],color=Col[i],opacity=1),
                legendgroup="data",
                legendgrouptitle=list(text="<b>Data</b>"))
  }
  return(p_ly)
}

#' Check if tick mark is inside bounding circle
#'
#' @param ticks list of tick marks
#' @param r radius of the bounding circle
#' @param thetas the gradients of the axes
#'
#' @return list of tick marks which are inside bounding circle
#' @noRd
check_inside_circle<-function(ticks,r,thetas){
  n<-length(ticks)
  for(i in 1:n){
    inside<-ticks[[i]][,1]^2+ticks[[i]][,2]^2 <= r^2
    #bound1<-c(r*cos(thetas[i]),r*sin(thetas[i]),NA)
    #bound2<-c(r*cos(thetas[i]-pi),r*sin(thetas[i]-pi),NA)
    ticks[[i]]<-ticks[[i]][inside,]

  }
  return(ticks)
}



#' Create a biplot with vector loadings and calibrated axes.
#'
#' @param pc12 First two principal components
#' @param colorpalete Colors per class group
#' @param symbol plotting symbol per class group
#'
#' @return plotly graph
#' @noRd
make_biplot<-function(pc12,colorpalete=NULL,symbol="circle"){
  Title<-"Overall quality and axis predictivities (cumulative)"
  p_ly<-plot_ly() |>
    layout(legend=list(tracegroupgap=0,xref="container",
                       yref="container",x=1,y=0.82,
                       title=list(text='<b> PCA Biplot </b>')),
           xaxis=list(title=pc12$DisplQuality,showticklabels = FALSE,
                      zeroline=FALSE,showgrid = FALSE,domain=c(0,1)),
           yaxis=list(showticklabels = FALSE,zeroline=FALSE,
                      scaleanchor={'x'}, scaleratio=1,showgrid = FALSE),
           xaxis2=list(domain=c(0,0.15),zeroline=TRUE),
           yaxis2=list(zeroline=TRUE,side="left",position=0),
           xaxis3=list(domain=c(0.65,1),zeroline=TRUE,showgrid=TRUE,
                       anchor="y3",dtick=1,title="Dimension of Subspace"),
           yaxis3=list(zeroline=TRUE,side="left",position=0.65,
                       showgrid=TRUE,domain=c(0.15,0.85),layer="below traces",
                       title=Title),
           updatemenus = list(
             list(
               y = 0.8,
               x =0,
               buttons = list(

                 list(method = "skip",
                      args = list("type", "scatter"),
                      label = paste("PC:",pc12$basis[1],"&",pc12$basis[2])),

                 list(method = "skip",
                      args = list("type", "histogram2d"),
                      label = "PC: 1 & 3"),

                 list(method = "skip",
                      args = list("type", "histogram2d"),
                      label = "PC: 2 & 3")
               )
             ),
             list(
               y=0.73,
               x=0,
               active=1,
               type="buttons",
               buttons=list(

                 list(method="skip",
                      args=list("type", "scatter"),
                      label="Axis Predictivity",
                      name="AxisStats"

                 )
               )
             ),
             list(
               y=0.66,
               x=0,
               active=1,
               type="buttons",
               buttons=list(

                 list(method="skip",
                      args=list("type", "scatter"),
                      label="Fit Measures",
                      name="FitMeasures"

                 )
               )),
               list(
                 y=0.59,
                 x=0,
                 active=1,
                 type="buttons",
                 buttons=list(

                   list(method="skip",
                        args=list("type", "scatter"),
                        label="Vector Display",
                        name="vecload"

                   )
                 )
               )
             )
           )

  pc13<-PCAbiplot(pc12$x,group=pc12$group,basis=c(1,3),build_plot=FALSE)
  pc23<-PCAbiplot(pc12$x,group=pc12$group,basis=c(2,3),build_plot=FALSE)
  addpc12<-add_vector_biplot(p_ly=p_ly,x=pc12,symbol=symbol,
                             color=colorpalete,visible=TRUE)
  p_ly<-addpc12[[1]]
  addpc13<-add_vector_biplot(p_ly,pc13,symbol,colorpalete,visible=FALSE)
  p_ly<-addpc13[[1]]
  addpc23<-add_vector_biplot(p_ly,pc23,symbol,colorpalete,visible=FALSE)
  p_ly<-addpc23[[1]]



  Xhat<-list(addpc12[[3]],addpc13[[3]],addpc23[[3]])
  Xhat2<-list(t(addpc12[[3]]),t(addpc13[[3]]),t(addpc23[[3]]))
  df<-list(addpc12[[2]],addpc13[[2]],addpc23[[2]])

  #need to count the annotations as these are tick marks.
  #JS should toggle visibility
  counter<-c(addpc12[[4]],addpc13[[4]],addpc23[[4]])

  #also need the angles of all the tick marks as annotation for
  #new predict lines

  angles<-list(addpc12[[5]],addpc13[[5]],addpc23[[5]])
  numtraces<-length(levels(pc12$group))+3*pc12$p +1
  Dispquality<-c(pc12$DisplQuality,pc13$DisplQuality,pc23$DisplQuality)



  p_ly<-p_ly|>
    add_trace(x=cos(seq(0,2*pi,length.out=200)),
              y=sin(seq(0,2*pi,length.out=200)), type="scatter",
              mode="lines",line = list(color = 'red',width=1.2),
              name="Unit Circle",showlegend=FALSE,
              meta='veccircle',xaxis="x",yaxis="y",
              hoverinfo='name',visible=FALSE)

  p_ly<-InsertAxisDeets(p_ly,pc12)
  FitMeasures<-InsertFitMeasures(p_ly,pc12)
  p_ly<-FitMeasures[[1]]

  p_ly<-insert_vector_annots(p_ly,pc12,pc13,pc23)
  counter<-c(counter,rep(pc12$p,3))

  p_ly<- p_ly|> htmlwidgets::onRender("

     function(el,x,data) {

          el.bipl5 = {clicked: false,
                 hasbox: false,
                 unit_circle: 0,
                 arr1: new Array(data.Xhat[0][0].length).fill(0),
                 active: 0,
                 rel_but: [0,0,0],
                 is_visible: 0,
                 selected : 0,
                 bip_domain : [0,1],
                 table_visible : 0,
                 table2_visible : 0,
                 vect_visible : 0,
                 but_names : ['PC','AxisStats','FitMeasures','vecload'],
                 pred12 : el.data[el.data.length-3],
                 pred13 : el.data[el.data.length-2],
                 pred23 : el.data[el.data.length-1]
                 };







     //var el.bipl5.arr1 = new Array(data.Xhat[0][0].length).fill(0);
     //var el.bipl5.active = 0;
     //var el.bipl5.rel_but = [0,0,0];
     //var el.bipl5.is_visible=0;
     //var el.bipl5.selected = 0;
     //var el.bipl5.bip_domain = [0,1];
     //var el.bipl5.table_visible = 0;
     //var el.bipl5.table2_visible = 0;
     //var el.bipl5.vect_visible = 0;

     // trace for fit measure table
     //var el.bipl5.pred12 = el.data[el.data.length-3];
     //var el.bipl5.pred13 = el.data[el.data.length-2];
     //var el.bipl5.pred23 = el.data[el.data.length-1];
     Plotly.deleteTraces(el.id,
                        [el.data.length-1,el.data.length-2,el.data.length-3])
     var All_annot = el.layout.annotations;
     function myFunction(up,low) {
        for (let i = up; i < low; i++) {
              All_annot[i].visible = true;
          }
     }



//-------------- UPDATEMENU-----------------

        el.on('plotly_buttonclicked',function(d){
              // toggle selectibility

              var rel_but_sel = el.bipl5.rel_but[d.menu._index-1];
              if(d.menu._index==1){
              // that is, the axis predictivity is to be inserted
                  var update = {
                    'updatemenus[1].active': [0,1][rel_but_sel],
                    'xaxis.domain': [[0,0.5],[0,1]][el.bipl5.is_visible],
                    'yaxis3.zeroline':true
                  }
                  el.bipl5.bip_domain[1] = [0.5,1][el.bipl5.is_visible];
                  var update_traces = [];
                  el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta[0] === 'axis_pred') {
                          update_traces.push(index);
                      }
                  });

                  var plot_update ={
                    'visible':[true,false][el.bipl5.is_visible],
                    'xaxis':['x3','x'][el.bipl5.is_visible],
                    'yaxis':['y3','y'][el.bipl5.is_visible]
                  }
                  el.bipl5.is_visible=[1,0][el.bipl5.is_visible];
                  Plotly.restyle(el.id,plot_update,update_traces)
                  el.bipl5.rel_but[d.menu._index-1] = [1,0][rel_but_sel];
                  Plotly.relayout(el.id,update)
                  return;
              }

              if(d.menu._index==2){
                  // that is the fit measures table needs to be inserted
                  var idx = el.bipl5.table_visible+el.bipl5.table2_visible +1;
                  el.bipl5.table2_visible = [1,0][el.bipl5.table2_visible];
                  var update = {
                    'updatemenus[2].active': [0,1][rel_but_sel],
                    'yaxis.domain' : [[0.3,1],[0,1],[0.3,1]][idx-1],
                    'yaxis2.domain': [[0.15,0.85],[0.3,1],[0.15,0.85]][idx],
                    'yaxis3.domain': [[0.15,0.85],[0.3,1],[0.15,0.85]][idx],
                    'legend.y':[0.92,0.82,0.82][idx-1]
                  }
                  if(rel_but_sel === 0){
                    Plotly.addTraces(el.id,[el.bipl5.pred12,el.bipl5.pred13,el.bipl5.pred23][el.bipl5.selected])
                }
                if(rel_but_sel === 1){
                  var update_traces = [];
                  el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta[0] === 'PredTable') {
                          update_traces.push(index);
                      }
                  });
                  Plotly.deleteTraces(el.id,update_traces)
                }

                el.bipl5.rel_but[d.menu._index-1] = [1,0][rel_but_sel];
                Plotly.relayout(el.id,update);
                return;
              }
              if(d.menu._index==3){
              // that is need to insert/delete red circle and vectors

                if(rel_but_sel === 0){
                    //need to insert vects
                    // first remove prediction lines
                    if(el.bipl5.clicked){
                        var remove = [];
                        el.data.forEach(function (item, index, arr) {

                          if (arr[index].meta === 'predict') {
                             remove.push(index);
                          }
                        });
                    Plotly.deleteTraces(el.id, remove);
                    el.bipl5.clicked=false;
                    }
                    // next we need to insert red circle and vects pappa
                    var update = {
                      visible: true
                    };
                    var n = data.counts.length;
                    myFunction(data.counts[n-4+el.bipl5.active],data.counts[n-3+el.bipl5.active])
                    All_annot.slice(data.counts[n-3+el.bipl5.active],
                                    data.counts[n-2+el.bipl5.active])

                    Plotly.restyle(el.id, update, [3*data.num]);
                    console.log(el.bipl5.active)
                    console.log(data.counts)
                    var dp_update = {
                      annotations : All_annot.slice(data.counts[n-4+el.bipl5.active],
                                    data.counts[n-3+el.bipl5.active]),
                      'updatemenus[3].active': [0,1][rel_but_sel],
                    }
                    el.bipl5.vect_visible = 1;

                    //alright pappa now need to take away axes

                    var tr_index = []
                    for(let i = data.num*el.bipl5.active; i<data.num*(el.bipl5.active+1); i++){
                        if(el.data[i].meta === 'axis'){
                        tr_index.push(i)
                        }
                        if(el.data[i].meta[0] === 'axis'){
                          tr_index.push(i)
                        }
                    }
                    console.log(el.data)
                    console.log('hier kom axes traces aya')
                    console.log(tr_index)
                    var trace_update = {
                        visible: false
                    }
                    Plotly.update(el.id,trace_update,dp_update,tr_index)
                }
                if(rel_but_sel === 1){
                  //need to remove vects and insert axes once more
                    var update = {
                      visible: true
                    };

                    var tr_index = []
                    for(let i = data.num*el.bipl5.active; i<data.num*(el.bipl5.active+1); i++){
                        if(el.data[i].meta === 'axis'){
                        tr_index.push(i)
                        }
                        if(el.data[i].meta[0] === 'axis'){
                          tr_index.push(i)
                        }
                    }

                    el.bipl5.vect_visible=0;
                    dp_update = {
                      'updatemenus[3].active': [0,1][rel_but_sel],
                      annotations : All_annot.slice(data.counts[el.bipl5.active],
                                                    data.counts[el.bipl5.active+1])
                    }
                    el.data[3*data.num].visible = false;
                    Plotly.update(el.id,update,dp_update,tr_index)
                }

                el.bipl5.rel_but[d.menu._index-1] = [1,0][rel_but_sel];
                return;
              }


              // CHANGE PC's

              // first remove prediction lines
              if(el.bipl5.clicked){
                    var remove = [];
                    el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta === 'predict') {
                          remove.push(index);
                      }
                    });
                Plotly.deleteTraces(el.id, remove);
              }
            el.bipl5.clicked=false;
            el.bipl5.selected = d.active;
            var Activetraces = Array(data.num).fill().map((element, index) => index + data.num*el.bipl5.active);
            var NewActive = Array(data.num).fill().map((element, index) => index + data.num*el.bipl5.selected);
            if (el.bipl5.selected === el.bipl5.active){//basies hoef fokol te doen
              return;
            }

            if (el.bipl5.table2_visible === 1){
                  var update_traces = [];
                  el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta[0] === 'PredTable') {
                          update_traces.push(index);
                      }
                  });
                  Plotly.deleteTraces(el.id,update_traces)
                  Plotly.addTraces(el.id,[el.bipl5.pred12,el.bipl5.pred13,el.bipl5.pred23][el.bipl5.selected])
            }

            var update = {
                visible: false
            };
            var update2={
                visible: true
            }


            Plotly.restyle(el.id, update, Activetraces);
            Plotly.restyle(el.id, update2, NewActive);
            el.bipl5.active = el.bipl5.selected;

          //ensure the vector display button is unselected and red circle gone
            el.data[3*data.num].visible = false;
            el.bipl5.rel_but[2] = 0;
            dp_update = {
            'updatemenus[3].active': 1,
            'xaxis.title' : data.DP[el.bipl5.selected],
            annotations : All_annot.slice(data.counts[el.bipl5.active],
                                          data.counts[el.bipl5.active+1])
            }
            myFunction(data.counts[el.bipl5.active],data.counts[el.bipl5.active+1])
            Plotly.relayout(el.id,dp_update)
            return false;
        })


//------------LEGENDCLICK--------------------

       el.on('plotly_legendclick', function(dat){
          var Activetraces = Array(data.num).fill().map((element, index) => index + data.num*el.bipl5.active);
          // Delete predictive lines
          // NOTE: this must come first before rest otherwise error
          if(dat.data[dat.curveNumber].meta=== 'predict'){
            var remove = [];
            el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta === 'predict') {
                          remove.push(index);
                      }
                });
            //remove prediction lines annotations as well
            for(let i = 0; i < data.a[el.bipl5.active].length; i++){
                el.layout.annotations.pop();
            }
            Plotly.deleteTraces(el.id, remove);
            return false;
         }

          if(dat.data[dat.curveNumber].meta[0] === 'data'){
          return;
          }
          if(dat.data[dat.curveNumber].meta[0] === 'density'){
          return;
          }
          if(dat.data[dat.curveNumber].meta === 'box'){
            Plotly.deleteTraces(el.id,dat.curveNumber)
            el.bipl5.bip_domain[0] = 0;
            var update = {
                'xaxis.domain': el.bipl5.bip_domain,   // updates the xaxis range
                'yaxis2.side': 'left'
            };
            Plotly.relayout(el.id,update);
            return false;
          }


          // REMOVE AXES

          var axis = dat.data[dat.curveNumber].legendgroup;
          var num = dat.data[dat.curveNumber].customdata[0];
          var indeces =[];
          el.data.slice(data.num*el.bipl5.active,data.num*el.bipl5.active+data.num).forEach(function(item,idx,arr){
              if(arr[idx].legendgroup === undefined){
              return;
              }
              if(arr[idx].legendgroup === axis){
                  indeces.push(idx);
              }
              if(arr[idx].customdata === undefined){
              return;
              }
              if(arr[idx].customdata[0] === axis){
                indeces.push(idx);
              }
          });
          var old_annotations = el.layout.annotations;
          if(el.bipl5.active===0){
            old_annotations.slice(data.counts[el.bipl5.active],data.counts[el.bipl5.active+1]).forEach(function(item,idx,arr){
              if(arr[idx].customdata === num){
                old_annotations[idx].visible = !old_annotations[idx].visible;
              }
            });
          }else{
            old_annotations.forEach(function(item,idx,arr){
                if(arr[idx].customdata === num){
                  old_annotations[idx].visible = !old_annotations[idx].visible;
                }
            });
          }
          hidden = el.bipl5.arr1[num-1];
          var update = {'visible': ['legendonly',true][hidden]};
          hidden = [1,0][hidden];
          el.bipl5.arr1[num-1] = hidden;
          var new_annot = {annotations:old_annotations};
          Plotly.restyle(el.id,update,indeces.map((element, index)=>element +
                                          data.num*el.bipl5.active));
          Plotly.relayout(el.id,new_annot);
          return false;
        });

//-------------------POINTS CLICK--------------

       el.on('plotly_click', function(d) {
       console.log('punt begin pappa')
       if(d.points[0].meta === 'density'){
          return;
       }
       if(el.bipl5.vect_visible ===1){
          return;
       }
    //-------------BOXPLOT--------------------
       if(d.points[0].meta === 'axis'){
            if(el.bipl5.hasbox){
            var deleters = [];
            //need to remove current boxplot
                el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta === 'box') {
                          deleters.push(index);
                      }
                })

                Plotly.deleteTraces(el.id, deleters);
            }
            el.bipl5.bip_domain[0] = 0.15;
            var update = {
                'xaxis.domain': el.bipl5.bip_domain,   // updates the xaxis range
                'yaxis2.side': 'left'
            };

        var trace1 = {
            y: data.Xhat2[el.bipl5.active][d.points[0].customdata-1],
            type: 'box',
            name: 'Boxplot: <br>'+data.colnames[d.points[0].customdata-1],
            meta: 'box',
            marker: {
              color: 'rgb(7,40,89)'
            },
            jitter: 0.3,
            pointpos: -1.8,
            xaxis: 'x2',
            yaxis: 'y2',
            boxpoints: 'all'
        };


        Plotly.relayout(el.id,update);
        Plotly.addTraces(el.id, trace1);
        el.bipl5.hasbox = true;
        return;
       }
       console.log('boxplot klaar process')
  //-----------------PREDICTION LINES--------------

         if(el.bipl5.clicked){
         console.log('haal ou predict uit begin')
         var remove = [];
            el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta === 'predict') {
                          remove.push(index);
                      }
                });
            Plotly.deleteTraces(el.id, remove);
            for(let i = 0; i < data.a[el.bipl5.active].length; i++){
                el.layout.annotations.pop();
            }
          console.log('ou predict eindig')
         }
         var X = [];
         var Y = [];
         console.log('begin nuwes insit')
         var traces_to_be_added = [];
         for (let i = 0; i < data.a[el.bipl5.active].length; i++) {
            var c = d.points[0].y+1/data.a[el.bipl5.active][i].m*d.points[0].x;
            var x_new = (data.a[el.bipl5.active][i].c-c)/(-1/data.a[el.bipl5.active][i].m -
                                                data.a[el.bipl5.active][i].m);
            var y_new = data.a[el.bipl5.active][i].m*x_new+data.a[el.bipl5.active][i].c;
            var showleg = false;
            if(i === 0){showleg = true;}
            X.push(x_new);
            Y.push(y_new);
            var newtrace = {
                x: [d.points[0].x, x_new],
                y: [d.points[0].y, y_new],
                mode: 'lines',
                xaxis: 'x',
                yaxis: 'y',
                showlegend: showleg,
                name: 'Predicted Value',
                meta: 'predict',
                line: {
                  dash: 'dot',
                  color: 'gray',
                  width: 1
                      }
            };
            var newAnnotation = {
                x: x_new,
                y: y_new,
                text: data.Xhat[el.bipl5.active][d.points[0].customdata-1][i].toFixed(2),
                showarrow: false,
                textangle: -Math.atan(data.a[el.bipl5.active][i].m)*180/Math.PI,
                xshift: -10*Math.sin(Math.atan(data.a[el.bipl5.active][i].m)),
                yshift: 10*Math.cos(Math.atan(data.a[el.bipl5.active][i].m)),
                name: 'Predicted Value',
                meta: 'predict',
                visible: true,
                font: {
                  size:10
                }
            }
            traces_to_be_added.push(newtrace)
            el.layout.annotations.push(newAnnotation);
            //Plotly.addTraces(el.id, newtrace);
         }
         Plotly.addTraces(el.id, traces_to_be_added);
         console.log('eindig nuwes insit')
        el.bipl5.clicked=true;
        var markertrace = {
            x: X,
            y: Y,
            mode: 'markers',
            showlegend: false,
            xaxis: 'x',
            yaxis: 'y',
            meta: 'predict',
            marker: {
              color:'gray',
              size: 4
            }
        }
        Plotly.addTraces(el.id, markertrace);





       });



}

   ",data=list(a=df,Xhat=Xhat,Xhat2=Xhat2,colnames=colnames(pc12$x),
               num=numtraces,DP=Dispquality,counts=c(0,cumsum(counter)),
               Angles=angles))

  return(list(p_ly,FitMeasures[[2]],FitMeasures[[3]]))
}



#' Insert the vector loadings as annotations
#'
#' @param p_ly Plotly graph
#' @param PC12 First two PC's: class bipl5
#' @param PC13 First third PC: class bipl5
#' @param PC23 Second thrid PC: class bipl5
#'
#' @return Updated plotly graph with vector loadings inserted
#' @noRd
insert_vector_annots<-function(p_ly,PC12,PC13,PC23){
  p_ly<- p_ly |>  add_annotations( ax = PC12$V[,1],
                    ay = PC12$V[,2],
                    xref = "x", yref = "y",
                    axref = "x", ayref = "y",
                    text = colnames(PC12$x),
                    showarrow = TRUE,
                    x = rep(0,PC12$p),
                    y = rep(0,PC12$p),
                    arrowside="start",
                    visible=FALSE,
                    meta='vecload'
                    )
    if(!is.null(PC13)){
      p_ly<- p_ly |> add_annotations( ax = PC13$V[,1],
                          ay = PC13$V[,2],
                          xref = "x", yref = "y",
                          axref = "x", ayref = "y",
                          text = colnames(PC12$x),
                          showarrow = TRUE,
                          x = rep(0,PC12$p),
                          y = rep(0,PC12$p),
                          arrowside="start",
                          visible=FALSE
                      )
    }
    if(!is.null(PC23)){
      p_ly<-p_ly |> add_annotations( ax = PC23$V[,1],
                          ay = PC23$V[,2],
                          xref = "x", yref = "y",
                          axref = "x", ayref = "y",
                          text = colnames(PC12$x),
                          showarrow = TRUE,
                          x = rep(0,PC12$p),
                          y = rep(0,PC12$p),
                          arrowside="start",
                          visible=FALSE
                      )
    }
  return(p_ly)
}




