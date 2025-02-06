# fig <- plot_ly(x=x$z.density$x,y=x$z.density$y,z = x$z.density$z, type = "contour",
#                autocontour = F,
#                #autocolorscale=F,
#                colorscale=list(seq(0,1,length.out=52),col.use),
#                contours=list(
#                  start=0.002,
#                  end=0.104,
#                  size=0.002
#                )
#                )
#
#
#  hh<- plot_ly(data=as.data.frame(x$Z), x=x$Z[,1],y=x$Z[,2],type="scatter")|>
#   add_trace(x=x$z.density$x,y=x$z.density$y,z = x$z.density$z, type = "contour",
#           autocontour = F,
#           #autocolorscale=F,
#           colorscale=list(seq(0,1,length.out=52),col.use),
#           contours=list(
#             start=0.002,
#             end=0.104,
#             size=0.002
#           )
#   )
#
# fig |> onRender("function(el){
#       console.log(el);
#                 }")
#
#
#
# col.use <- colorRampPalette(x$density.style$col)
# col.use <- col.use(length(levels.rect) - 1)


# "#FFFFFF" "#F9F1FE" "#F3E4FD" "#EED7FC" "#E8CAFB" "#E3BDFA" "#DDB0F9" "#D7A3F8"
# [9] "#D296F7" "#CC88F7" "#C77BF6" "#C16EF5" "#BB61F4" "#B654F3" "#B047F2" "#AB3AF1"
# [17] "#A52DF0" "#A020F0" "#961EF0" "#8D1CF1" "#831AF2" "#7A18F3" "#7016F4" "#6714F5"
# [25] "#5E12F6" "#5410F7" "#4B0FF7" "#410DF8" "#380BF9" "#2F09FA" "#2507FB" "#1C05FC"
# [33] "#1203FD" "#0901FE" "#0000FF" "#000FFF" "#001DFF" "#002DFF" "#003CFF" "#004AFF"
# [41] "#005AFF" "#0069FF" "#0077FF" "#0087FF" "#0096FF" "#00A4FF" "#00B4FF" "#00C2FF"
# [49] "#00D2FF" "#00E1FF" "#00EFFF" "#00FFFF"
#
#
#
# clll<-list()
#
# for(i in 1:length(levels.rect)){
#   clll[[i]]<-list(levels.rect[i]/0.104,col.use[i])
# }
