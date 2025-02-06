#' Add plotly scatter traces to current plotly object
#'
#' @inheritParams TDA
#' @importFrom cluster ellipsoidhull predict.ellipsoid
#' @return list with plotly object and data needed
#' @noRd
addPlotlyBiplot<-function(p_ly,x,visible,dist,inflate=1,alpha=0.95,
                          alpha_Elip=NULL,swop=FALSE,density.args=NULL,
                          color=NULL,symbol="circle"){
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

  #start of by drawing an ellipse over all the data... used to determine
  #how far axes shifted
  bigElip<-cluster::ellipsoidhull(Z)
  bigElipcoords<-cluster::predict.ellipsoid(bigElip,n.out=101)

  #next draw possible smaller ellipse.... used to get length of the axes
  if(!is.null(alpha_Elip)){
    bag<-do.call(alpha_Elip,list(alpha=alpha,x=Z))
    elip<-cluster::ellipsoidhull(bag)
    elipcoords<-cluster::predict.ellipsoid(elip,n.out=101)
  }
  else
    elipcoords<-bigElipcoords
  endpoints<-tickmarks(ellip=elipcoords,gradient=m,
                       p=p,V=x$V, mu=mu,stddev=stddev)
  shift<-MoveLines(elip=bigElipcoords,m=m,quadrant=quads,
                   d=dist,initial_ends=endpoints,swop=swop,cols=colnames(x$x))
  DensCoors<-MoveDensities(Z=Z,m=m,endpoints=shift$ends,
                           dist=shift$ShiftDist,dinflation=inflate,
                           group=group,densityargs=density.args)

  #----------Get equations of shifted axes for prediction lines---------
  slope<-numeric()
  intercept<-numeric()
  for(i in 1:p){
    deets<-equation(shift$ends[[i]][1,-3],shift$ends[[i]][2,-3])
    slope[i]<-deets[1]
    intercept[i]<-deets[2]
  }
  df<-data.frame(m=slope,c=intercept)

  #------------PLOTLY-------------------------------------

  if(is.null(color))
    Col<-colorpal(length(levels(group)))
  num_groups<-length(levels(x$group))
  for(i in 1:num_groups){
    p_ly<-p_ly |>
      add_trace(data=Z,x=Z[x$group==levels(x$group)[i],1],
                y=Z[x$group==levels(x$group)[i],2],name=levels(x$group)[i],
                type = "scatter", mode = "markers",
                hovertext=rownames(x$x)[x$group==levels(x$group)[i]],
                hoverinfo="text+name",
                customdata=(1:n)[x$group==levels(x$group)[i]],
                meta="data",xaxis="x",yaxis="y",visible=visible,
                marker=list(symbol=p_ly_pch[i],color=Col[i]))
  }
  # Insert axes with the tick marks
  AnnotCounter<-numeric()
  angles<-list()
  for(i in 1:p){
    AnnotCounter[i]<-length(shift$ends[[i]][,3])+1 +1
    index<-which(shift$ends[[i]][,1]== max(shift$ends[[i]][,1]))
    index2<-which(shift$ends[[i]][,3]== max(shift$ends[[i]][,3]))
    if(index==index2){
      AxName<-paste("  ",colnames(x$x)[i])
      pos<-"right"
    }
    else{
      index<-index2
      AxName<-paste(colnames(x$x)[i],"  ")
      pos<-"left"
    }
    AxName<-""
    if(quads[i] %in% c(1,4)){
      lab<-paste("<b>",colnames(x$x)[i]," &#129030; </b>",sep="")
      lab2<-"&#11166;"
    }
    if(quads[i] %in% c(2,3)){
      lab<-paste("<b> &#129028; ",colnames(x$x)[i]," </b>",sep="")
      lab2<-"&#11164;"
    }

    angles[[i]]<-list(x=-10*sin(atan(x$m[i])),y=10*cos(atan(x$m[i])))
    p_ly<-p_ly |>
      add_trace(x=shift$ends[[i]][,1],y=shift$ends[[i]][,2],
                text=as.character(shift$ends[[i]][,3]),type="scatter",
                mode="lines+markers",line = list(color = 'grey',width=1),
                marker=list(color="grey",size=4),name=colnames(x$x)[i],
                textposition='top', legendgroup=paste("Ax",i,sep=""),
                meta='axis',xaxis="x",yaxis="y",customdata=i,
                hoverinfo='name',visible=visible) |>

      add_annotations(x=shift$ends[[i]][,1],y=shift$ends[[i]][,2],
                      text=as.character(shift$ends[[i]][,3]),
                      showarrow=FALSE,textangle=-atan(x$m[i])*180/pi,
                      visible=visible,yshift=-10*cos(atan(x$m[i])),
                      xshift=10*sin(atan(x$m[i])),meta='axis',
                      xaxis="x",yaxis="y",customdata=i,font=list(size=10))|>

      add_trace(x=shift$ends[[i]][index,1],y=shift$ends[[i]][index,2],
                text=AxName,type="scatter",mode="text",textposition=pos,
                legendgroup=paste("Ax",i,sep=""),showlegend=FALSE,
                textfont=list(size=14),
                meta='axis',xaxis="x",yaxis="y",visible=visible)|>

      add_annotations(x=mean(shift$ends[[i]][,1]),y=mean(shift$ends[[i]][,2]),
                      text=paste("<b>",colnames(x$x)[i],"</b>"),
                      showarrow=FALSE,
                      textangle=-atan(x$m[i])*180/pi,
                      visible=visible,yshift=-20*cos(atan(x$m[i])),
                      xshift=20*sin(atan(x$m[i])),meta='axis',xaxis="x",
                      yaxis="y",customdata=i)|>

      add_annotations(x=shift$ends[[i]][index2,1],y=shift$ends[[i]][index2,2],
                      text=lab2, showarrow=FALSE,
                      textangle=-atan(x$m[i])*180/pi,visible=visible,
                      meta='axis',xaxis="x",yaxis="y",customdata=i,
                      font=list(size=18))



  }
  #insert the densities
  for(i in 1:num_groups){
    Dens<-DensCoors[[i]]
    for(j in 1:p){
      showleg<-FALSE #show legend... only true for first iteration
      if(j==1) showleg<-TRUE
      index_color<-which(levels(group)==unique(group)[i])
      p_ly<-p_ly|>
        add_trace(x=Dens[,2*j-1],y=Dens[,2*j],mode="lines",type="scatter",
                  line=list(dash="dot",color=Col[index_color],width=0.95),
                  legendgroup=unique(group)[i], showlegend=showleg,
                  name=unique(group)[i], meta='density', xaxis="x",
                  yaxis="y",hoverinfo="skip",customdata=paste("Ax",j,sep=""),
                  visible=visible)
    }
  }


  return(list(p_ly,df,Xhat,counter=sum(AnnotCounter),
              Dshift=shift$ShiftDist,angles))

}







#' Insert javascript reactivity code
#'
#' @param plotly_plot Plotly graph
#' @param dat data to be passed as argument
#'
#' @return plotly plot updated with javascript code
#' @noRd
insert_reactivity_TDA<-function(plotly_plot,dat){

  plotly_plot |> htmlwidgets::onRender("

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
                 table_trace : el.data[el.data.length-1],
                 pred12 : el.data[el.data.length-3],
                 pred13 : el.data[el.data.length-2],
                 pred23 : el.data[el.data.length-1]
                 };




     //var el.bipl5.clicked = false;
     //var el.bipl5.hasbox = false;
     //var el.bipl5.arr1 = new Array(data.Xhat[0][0].length).fill(0);
     //var el.bipl5.active = 0;
     //var el.bipl5.rel_but = [0,0,0];
     //var el.bipl5.is_visible=0;
     //var selected = 0;
     //var el.bipl5.bip_domain = [0,1];
     //var el.bipl5.table_visible = 0;
     //var el.bipl5.table2_visible = 0;
     //var el.bipl5.table_trace = el.data[el.data.length-1];
     //var el.bipl5.pred12 = el.data[el.data.length-3];
     //var el.bipl5.pred13 = el.data[el.data.length-2];
     //var el.bipl5.pred23 = el.data[el.data.length-1];
     Plotly.deleteTraces(el.id,[el.data.length-1,el.data.length-2,el.data.length-3])
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

                  el.bipl5.table2_visible = [1,0][el.bipl5.table2_visible];
                  var update = {
                    'updatemenus[2].active': [0,1][rel_but_sel],
                    'yaxis.domain' : [[0,1],[0.3,1],[0.3,1]][el.bipl5.table_visible+el.bipl5.table2_visible],
                    'yaxis2.domain': [[0.15,0.85],[0.3,1],[0.3,1]][el.bipl5.table_visible+el.bipl5.table2_visible],
                    'yaxis3.domain': [[0.15,0.85],[0.3,1],[0.3,1]][el.bipl5.table_visible+el.bipl5.table2_visible],
                    'legend.y':[0.82,0.92,0.92][el.bipl5.table_visible+el.bipl5.table2_visible]
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
                el.bipl5.table_visible = [1,0][el.bipl5.table_visible];
                  var update = {
                    'updatemenus[3].active': [0,1][rel_but_sel],
                    'yaxis.domain' : [[0,1],[0.3,1],[0.3,1]][el.bipl5.table_visible+el.bipl5.table2_visible],
                    'yaxis2.domain': [[0.15,0.85],[0.3,1],[0.3,1]][el.bipl5.table_visible+el.bipl5.table2_visible],
                    'yaxis3.domain': [[0.15,0.85],[0.3,1],[0.3,1]][el.bipl5.table_visible+el.bipl5.table2_visible],
                    'legend.y':[0.82,0.92,0.92][el.bipl5.table_visible+el.bipl5.table2_visible]
                  }
                if(rel_but_sel === 0){
                    Plotly.addTraces(el.id,el.bipl5.table_trace)
                }
                if(rel_but_sel === 1){
                  var update_traces = [];
                  el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta[0] === 'table') {
                          update_traces.push(index);
                      }
                  });
                  Plotly.deleteTraces(el.id,update_traces)
                }
                el.bipl5.rel_but[d.menu._index-1] = [1,0][rel_but_sel];
                Plotly.relayout(el.id,update);
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

            dp_update = {
            'xaxis.title' : data.DP[el.bipl5.selected],
            annotations : All_annot.slice(data.counts[el.bipl5.active],data.counts[el.bipl5.active+1])
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
          Plotly.restyle(el.id,update,indeces.map((element, index) => element + data.num*el.bipl5.active));
          Plotly.relayout(el.id,new_annot);
          return false;
        });

//-------------------POINTS CLICK--------------

       el.on('plotly_click', function(d) {
       if(d.points[0].meta === 'density'){
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
  //-----------------PREDICTION LINES--------------

         if(el.bipl5.clicked){
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
         }
         var X = [];
         var Y = [];
         for (let i = 0; i < data.a[el.bipl5.active].length; i++) {
            var c = d.points[0].y+1/data.a[el.bipl5.active][i].m*d.points[0].x;
            var x_new = (data.a[el.bipl5.active][i].c-c)/(-1/data.a[el.bipl5.active][i].m-data.a[el.bipl5.active][i].m);
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

            el.layout.annotations.push(newAnnotation);
            Plotly.addTraces(el.id, newtrace);
         }
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

   ",data=dat)

}
