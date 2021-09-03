library(rnaturalearth)
library(sf)
library(ggplot2)
library(dplyr)
library(cowplot)

exp <- c("Cameroon", "Nigeria", "Benin", "Jordan", "Gaza Strip","Egypt", "Sudan", "Ethiopia")
exp_dist <- c("Kriti")
exp_fv <- c("Las Palmas")

ne <- st_as_sf(ne_states())

download.file("https://naciscdn.org/naturalearth/50m/cultural/ne_50m_admin_0_breakaway_disputed_areas.zip","ne_50m_admin_0_breakaway_disputed_areas.zip")
unzip("ne_50m_admin_0_breakaway_disputed_areas.zip")
disputed <- read_sf("ne_50m_admin_0_breakaway_disputed_areas.shp")
disputed <- disputed[disputed$ADMIN=="Morocco",]
unlink(list.files(pattern="ne_50m_admin_0_breakaway_disputed_areas"))

fv <- st_cast(ne[ne$name == exp_fv,], "POLYGON")
ne <- ne[ne$name != exp_fv,]
ne <- rbind(ne,fv)

ne$eombio <- "bg"
ne[rownames(ne)=="2825.1","eombio"] <- "fv"
ne[ne$name %in% exp_dist | ne$admin %in% exp ,"eombio"] <- "exp"

ne_admin <- ne %>% group_by(admin) %>% summarize()
ne_bg <- ne %>% group_by() %>% summarize()

#drop tibble
class(ne_admin)[-c(2:3)] -> class(ne_admin)
names(ne_eomb)
ne_eomb <- ne[ne$eombio!="bg",]
ne_eomb <- ne_eomb %>% group_by(admin,eombio) %>% summarize()

ne_eomb$eombio <- factor(ne_eomb$eombio, levels = c("exp", "fv", "ntv"))

ntv_poly <- st_read("supp_area.gpkg")
ntv_poly_df <- as.data.frame(ntv_poly)
names(ntv_poly_df) <- "geometry"
ntv_poly <- st_sf(ntv_poly_df)
ntv_poly[,2:length(ne_eomb)] <- NA
names(ntv_poly)
names(ntv_poly) <- names(ne_eomb)[c(3,1,2)]
ntv_poly$eombio <- "ntv"
ne_eomb <- rbind(ne_eomb,ntv_poly)


bb <- st_bbox(c(ymax =  50,ymin = -36.3,  xmax = 60, xmin = -20), crs = st_crs(4326))
ne <- st_crop(ne, bb)

col_exp = "#FF5C5C"
col_ntv = "#F6E93C"
col_fv = "#4785C2"
col_bg = "#EEE5E9"
col_border = "grey40"
col_box = "grey60"
lab <- c("Supposed native area","Known distribution","New record")

em_map <- ggplot() +
    geom_sf(data = ne_admin,color = col_border,fill=col_bg, show.legend = FALSE,lwd=0.1) +
    geom_sf(data = ne_eomb, aes(fill = eombio,color=eombio,linetype=eombio,alpha=eombio,lwd=eombio), show.legend = "polygon") +
    scale_fill_manual(values = c(col_exp,col_fv,col_ntv),labels=lab[c(2,3,1)]) +
    scale_color_manual(values = c(col_border,col_border,col_ntv),labels=lab[c(2,3,1)]) +
    scale_linetype_manual(values = c("solid","solid","dashed"),labels=lab[c(2,3,1)])+
    scale_size_manual(values = c(0.2,0.2,1),labels=lab[c(2,3,1)])+
    scale_alpha_manual(values = c(1,1,0.5),labels=lab[c(2,3,1)])+
    geom_sf(data = disputed,color = col_border,fill=col_bg, show.legend = FALSE,lwd=0.1) +
    geom_sf(data = ne_bg,color = col_border,fill=NA, show.legend = FALSE,lwd=0.2) +
    coord_sf(crs = st_crs(3857),xlim = c(-2000000, 5700000), ylim = c(-1000000, 5800000)) +
    geom_segment(mapping = aes(x = -2050000,y = 3180000,xend = -2270000,yend = 100000), colour = col_box, size = 1,linetype="dotted")+
    geom_rect(mapping = aes(xmin = -2050000,ymin = 3180000,xmax = -1470000,ymax = 3445000),fill = NA, colour = col_box, size = 1)+
    theme_bw() +
    theme(text = element_text(size=16),legend.position = c(0.85, 0.92),legend.title=element_blank(),axis.title=element_blank(),legend.text = element_text(size = 16),legend.key.size = unit(1, "cm"),legend.key = element_rect(color = "white", fill = NA,size = 0),legend.box.background=element_rect(color=col_box,size=2.5)) 

inset_map <- em_map + 
    coord_sf(xlim =c(-2050000, -1470000), ylim = c(3180000, 3445000), crs = st_crs(3857), expand = FALSE) +
    theme(legend.position = "none",
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.border = element_rect(fill=NA, colour = col_box, size=2),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0,0,0,0),"cm")
        )

final_map <- ggdraw(em_map) +
      draw_plot(inset_map,
        x = 0.22, 
        y = 0.035,
        width = 0.22, 
        height =0.21
    )

ggsave(
  "map.png",
  final_map,
  width=20,
  height=10.9,
  dpi = 100
)
