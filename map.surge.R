map.surge <- function(data.PSE, 
                      xlim=NULL, ylim=NULL, zlim=NULL, breaks=NULL,
                      color_low = "blue", color_mid="yellow", color_high="red",
                      mainTitle=NULL, legendTitle="PSE (m)"){
  
  require(sp)
  require(maptools)
  require(ggplot2)
  require(maps)
  require(scales)
  
  if(is.null(xlim) | is.null(ylim)){
    xlim = c(min(data.PSE$long), max(data.PSE$long))
    ylim = c(min(data.PSE$lat), max(data.PSE$lat))
  }


  # data.PSE = data.frame(long=long, lat=lat, value=PSE)
  # data.dry = data.frame(long=long, lat=lat)
  # LandFall = c(long, lat)
  
  ## plot world map with continuents marked
  world = maps::map("world", fill=TRUE, plot=FALSE)
  # convert the 'map' to something we can work with via geom_map
  IDs = sapply(strsplit(world$names, ":"), function(x) x[1])
  world = maptools::map2SpatialPolygons(world, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
  world_map = fortify(world)
  
  g = ggplot() + geom_map(data=world_map, map=world_map,
                          aes(map_id=id), fill="gray50")
  
  g = g + xlim(xlim) + ylim(ylim)
  
  #print(g)
  
  ## plot United States
  us_states = maps::map("state", fill=TRUE, plot=FALSE)
  # convert the 'map' to something we can work with via geom_map
  IDs = sapply(strsplit(us_states$names, ":"), function(x) x[1])
  us_states = maptools::map2SpatialPolygons(us_states, IDs=IDs, proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
  us_map = fortify(us_states)
  g = g + geom_map(data=us_map, map=us_map, aes(map_id=id),
                   fill="gray50", colour="white", size=0.1)
  
  ## plot peak surge elevation
  PSE = data.frame(long=data.PSE$long, lat=data.PSE$lat,
                    value=data.PSE$value)
  g = g + geom_point(data=PSE, shape=19, aes(x=long, y=lat, color=value), 
                     size=.5, alpha=.9) + geom_tile()  
     # scale_color_manual(values=diverging_hsv(5))
  if(is.null(breaks)){
    g = g + scale_colour_gradient2(midpoint=mean(PSE$value), 
                           low=color_low, mid=color_mid,
                           high=color_high, 
                           limits=zlim, name=legendTitle,
                           oob=squish)
  }else{
    g = g + scale_colour_gradient2(midpoint=mean(PSE$value), 
                           low=color_low, mid=color_mid,
                           high=color_high, 
                           limits=zlim, name=legendTitle,
                           oob=squish, breaks=breaks)
  }

  g = g + coord_quickmap() 
  #g = g + coord_map("albers", lat0=39, lat1=45)
  
  # g = g + theme(plot.title = element_text(size = rel(1.2)),
  #               #panel.background = element_blank(),
  #               panel.grid.major=element_blank(),
  #               panel.grid.minor=element_blank(),
  #               legend.key.size = unit(.2, "cm"),
  #               legend.key.width = unit(1.2,"cm"),
  #               legend.box.spacing = unit(0.1, "cm"),
  #               legend.box.margin = margin(0, 0, 0, 0, "cm"),
  #               plot.margin = margin(-1, -1, 0.05, -1, "cm"),
  #               axis.text=element_text(size=14),
  #                 axis.title=element_text(size=14),
  #                 axis.title.x=element_text(size=14),
  #                 axis.text.x=element_text(size=14),
  #                 axis.title.y=element_text(size=14),
  #                 axis.text.y=element_text(size=14),
  #                 legend.text=element_text(size=14),
  #                 legend.title=element_text(size=14),
  #                 legend.position = "top"
  #               )

  g = g + theme(plot.title = element_text(size = rel(1.2)),
                #panel.background = element_blank(),
                panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),
                legend.key.size = unit(1.0, "cm"),
                legend.key.width = unit(0.2,"cm"),
                legend.box.spacing = unit(0.1, "cm"),
                legend.box.margin = margin(0, 0, 0, 0, "cm"),
                plot.margin = margin(-1, -1, 0.05, -1, "cm"),
                axis.text=element_text(size=14),
                  axis.title=element_text(size=14),
                  axis.title.x=element_text(size=14),
                  axis.text.x=element_text(size=14),
                  axis.title.y=element_text(size=14),
                  axis.text.y=element_text(size=14),
                  legend.text=element_text(size=14),
                  legend.title=element_text(size=14)
                )
  
  g = g + labs(title=paste(mainTitle,"\n",sep=""), x="long", y="lat")
  
  
  
  return(g)
  
}
