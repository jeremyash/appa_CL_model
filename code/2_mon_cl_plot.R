library(tidyverse)
library(skimr)
library(patchwork)
library(readxl)
library(sp)
library(rgeos)
library(raster)
library(rgdal)
library(scales)
# library(units)
library(viridis)  
library(grid)
library(extrafont)
library(ggsn)
library(RColorBrewer)
library(gtable)
#----------------------------------------------------------------------------

########################################
## functions
########################################

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Ubuntu", color = "#22211d"),
      plot.title = element_text(face = "bold"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA), 
      panel.background = element_rect(fill = "white", color = NA), 
      legend.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank(),
      ...
    )
}

#----------------------------------------------------------------------------

########################################
## font miscellany
########################################

# For Windows - in each session
# Adjust the path to match your installation of Ghostscript
Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.23/bin/gswin64.exe")

# load font for plotting
windowsFonts(Times=windowsFont("Ubuntu"))

#----------------------------------------------------------------------------




########################################
## load data
########################################

mon_cl <- readRDS("data/mon_cl.RDS")
mon_cl_update <- mon_cl # duplicate to update CL calculations

# relief base map
mon_relief <- readRDS("GIS/mon_relief.RDS")

# mon border
mon_nf <- readOGR("gis/mon_nf")
mon_nf <- spTransform(mon_nf, CRS = proj4string(mon_relief))

# wilderness for mon
wild <- readOGR("GIS/wilderness")
mon_wild <- wild[wild$WILDERNE_1 %in% c("Dolly Sods Wilderness",
                                        "Cranberry Wilderness",
                                        "Laurel Fork South Wilderness",
                                        "Otter Creek Wilderness",
                                        "Spice Run Wilderness"), ]
mon_wild <- spTransform(mon_wild, CRS = proj4string(mon_relief))
mon_wild_df <- broom::tidy(mon_wild)


# mon_c1 <- wild[wild$WILDERNE_1 %in% c("Dolly Sods Wilderness",
#                                       "Otter Creek Wilderness"), ]
# library(plotKML)
# plotKML(mon_c1["WILDERNE_1"],
#         filename = "gis/mon_c1.kml",
#         colour_scale = c("darkgreen", "darkgreen"))
# 
# writeOGR(mon_c1,
#          "gis/mon_c1.kml",
#          "Mon_c1",
#          driver = "KML")
#----------------------------------------------------------------------------

########################################
## recalculate CL and exceedance for Mon based on Bill J's 8-10-2018 Email
########################################



mon_cl_update@data <- mon_cl@data %>% 
  mutate(CL_ANCt_mon = BCDEP + BCWAVG - BcUP_Unsuit - (ANCt*RUNOFF)) %>% 
  mutate(CL_ANCt_Exceed_mon = SDEP + (NO3_CONC/RUNOFF) - CL_ANCt_mon)


mon_cl_update_dat <- mon_cl_update@data %>% 
  dplyr::select(GridID, ANCt, CL_ANCt, CL_ANCt_Exceed, CL_ANCt_mon, CL_ANCt_Exceed_mon) %>% 
  mutate(id = as.character(GridID)) %>% 
  dplyr::select(-GridID)



# prep data for plotting
# mon_cl_df <- broom::tidy(mon_cl_update, region = "GridID") %>%
#   left_join(., mon_cl_update_dat, by = "id")
# saveRDS(mon_cl_df, "data/mon_cl_df.RDS")

mon_cl_df <- readRDS("data/mon_cl_df.RDS")

#----------------------------------------------------------------------------

########################################
## variable descriptions
########################################

# ANCt
# The ANC threshold (ANCt)  sed for calculating critical loads when binning the data. Values place the ANCpm5 (potential ANC minus 5 ueq/L) into the following bins: 10, 30, 50, and 100. The units are ueq/L. 
# 
# The bins are as follows:
#   10: >=10 - <30
# 30: >=30 - <50
# 50: >=50 - <100
# 100: >=100


# CL_ANCt
# The critical load (CL) of acid deposition (sulfur and nitrogen) that will still attain the ANC threshold (ANCt). The steady-state simple mass balance equation is used to calculate the critical load using the Field Calculator:
#   
#   [BCDEP] + [BCWAVG] - [BcUP_NoHarv] - ( [ANCt] * [RUNOFF])
# 
# Values of "8888" represent "high" CL and are associated with all catchments considered to be "high" ANC according to the ANC threshold model.


# CL_ANCt_Exceed 
# Exceedance of Critical load (CL) of sulfur deposition (meq/m^2/yr) for protecting stream ANC = ANCt in consideration of ambient nitrate leaching. Calculated using the Field Calculator as:
#   
#   ( [SDEP] + ( [NO3_CONC] / [RUNOFF] )) - [CL_ANCt]
# 
# Values of "-8888" represent "No Exceeedance" of the CL and are associated with all catchments considered to be "high" ANC according to the ANC threshold model.





#----------------------------------------------------------------------------

########################################
## prep data for plotting
########################################

# relief
relief_df <- as.data.frame(mon_relief, xy = TRUE) %>% 
  dplyr::select(x, y, relief = srgr48i0100a_Value) %>% 
  filter(!(is.na(relief)))

rm(mon_relief)
rm(mon_cl)
rm(mon_cl_update)
rm(mon_cl_update_dat)

# replace 8888 values with NA
mon_cl_df$CL_ANCt_mon[mon_cl_df$CL_ANCt_mon > 4500] <- NA
mon_cl_df$CL_ANCt_Exceed_mon[mon_cl_df$CL_ANCt_Exceed_mon < -4500] <- NA

# remove nas
mon_cl_df <- mon_cl_df %>% 
  filter(!(is.na(CL_ANCt_mon)))

# border
mon_nf_df <- broom::tidy(mon_nf)

# create breaks in data for plotting
brks  <- c(-50, 0, 50, 100, 150, 200, 250)
labels <- c( 0, 50, 100, 150, 200, 220)

mon_cl_df$CL_ANCt_mon_brks <- cut(mon_cl_df$CL_ANCt_mon,
                                  breaks = brks,
                                  include.lowest = TRUE,
                                  labels = labels)

brks_scale <- levels(mon_cl_df$CL_ANCt_mon_brks)
labels_scale <- rev(brks_scale)



#----------------------------------------------------------------------------

########################################
## plotting
########################################

# color pallete
# cbPalette <- c("#842F32",
#                "#DF6747",
#                "#E3E3CD",
#                "#878D92",
#                "#49494D")

cbPalette <- brewer.pal(6, "YlOrRd")





# CL_ANCt_mon
q <- ggplot() +
  
  geom_raster(data = relief_df, aes(x = x,
                                 y = y,
                                 alpha = relief)) +
  # use the "alpha hack"
  scale_alpha(name = "", range = c(0.6, 0), guide = F)  +
  # CL polygons
  geom_polygon(data = mon_cl_df, aes(fill = CL_ANCt_mon_brks, 
                                    x = long, 
                                    y = lat, 
                                    group = group)) +
  geom_polygon(data = mon_nf_df, 
               aes(long, lat, group = group),
               fill = NA,
               color = "grey15",
               size = 0.6) +
  geom_polygon(data = mon_wild_df, 
               aes(long, lat, group = group),
               fill = NA,
               color = "darkgreen",
               size = 1) +
  
  coord_equal() +
  
  # add the previously defined basic theme
  theme_map() +
  
  labs(x = NULL, 
       y = NULL, 
       title = "Monongahela NF Critical Loads",
       subtitle = "CL of acid deposition to attain ANC threshold (ANCt)",
       caption = "Data from B. Jackson & T. McDonnell, 2018") + 
  
  theme(legend.position = "bottom") +
  scale_fill_manual(
    # in manual scales, one has to define colors, well, manually
    # I can directly access them using viridis' magma-function
    values = rev(cbPalette),
    breaks = rev(brks_scale),
    name = expression(paste("CL ANCt (meq/", m^2, "/yr)")),
    drop = FALSE,
    labels = labels_scale,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2, units = "mm"),
      keywidth = unit(70 / length(labels), units = "mm"),
      title.position = 'top',
      # I shift the labels around, the should be placed 
      # exactly at the right end of each legend key
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      # also the guide needs to be reversed
      reverse = T,
      label.position = "bottom"
    )
  )

q

# update legend to show that first and last bin are smaller bins
q_reduceLegendWithExtremes <- function(p){
  p_grob <- ggplotGrob(q)
  legend <- gtable_filter(p_grob, "guide-box")
  legend_grobs <- legend$grobs[[1]]$grobs[[1]]
  # # grab the first key of legend
  # legend_first_key <- gtable_filter(legend_grobs, "key-3-1-1")
  # legend_first_key$widths <- unit(4.67, units = "mm")
  # # modify its width and x properties to make it shorter
  # legend_first_key$grobs[[1]]$width <- unit(4.67, units = "mm")
  # legend_first_key$grobs[[1]]$x <- unit(4.67, units = "mm")
  # 
  # # last key of legend
  # legend_last_key <- gtable_filter(legend_grobs, "key-3-6-1")
  # legend_last_key$widths <- unit(4.67, units = "mm")
  # # analogous
  # legend_last_key$grobs[[1]]$width <- unit(4.67, units = "mm")
  # legend_last_key$grobs[[1]]$x <- unit(4.67, units = "mm")

  # grab the last label so we can also shift its position
  legend_last_label <- gtable_filter(legend_grobs, "label-5-6")
  # legend_last_label$grobs[[1]]$x <- unit(4.67, units = "mm")

  # Insert new color legend back into the combined legend
  # legend_grobs$grobs[legend_grobs$layout$name == "key-3-1-1"][[1]] <-
  #   legend_first_key$grobs[[1]]
  # legend_grobs$grobs[legend_grobs$layout$name == "key-3-6-1"][[1]] <-
  #   legend_last_key$grobs[[1]]
  # legend_grobs$grobs[legend_grobs$layout$name == "label-5-6"][[1]] <-
  #   legend_last_label$grobs[[1]]

  # finally, I need to create a new label for the minimum value
  new_first_label <- legend_last_label$grobs[[1]]
  new_first_label$label <- -20
  new_first_label$x <- unit(-0.15, units = "cm")
  new_first_label$hjust <- 1

  legend_grobs <- gtable_add_grob(legend_grobs,
                                  new_first_label,
                                  t = 6,
                                  l = 2,
                                  name = "label-5-0",
                                  clip = "off")
  legend$grobs[[1]]$grobs[1][[1]] <- legend_grobs
  p_grob$grobs[p_grob$layout$name == "guide-box"][[1]] <- legend

  # the plot is now drawn using this grid function
  grid.newpage()
  grid.draw(p_grob)
}

q_reduceLegendWithExtremes(q)






ggsave("figures/mon_cl_anct_wilderness.pdf",
       height = 8,
       width = 7,
       units = "in")

# #  If you don't specify 'outfile', it will overwrite the original file
# embed_fonts("figures/mon_cl_anct.pdf", outfile="figures/mon_cl_anct_embed.pdf")


#------------

# CL_ANCt_Exceed_mon

ggplot() +
  
  geom_raster(data = relief_df, aes(x = x,
                                 y = y,
                                 alpha = relief)) +
  # use the "alpha hack"
  scale_alpha(name = "", range = c(0.6, 0), guide = F)  +
  # CL polygons
  geom_polygon(data = mon_cl_df, aes(fill = CL_ANCt_Exceed_mon, 
                                     x = long, 
                                     y = lat, 
                                     group = group)) +
  geom_polygon(data = mon_nf_df, 
               aes(long, lat, group = group),
               fill = NA,
               color = "grey15",
               size = 0.6) +
  
  coord_equal() +
  
  # add the previously defined basic theme
  theme_map() +
  
  labs(x = NULL, 
       y = NULL, 
       title = "Monongahela NF Critical Loads",
       subtitle = "Exceedance of critical load of sulfur deposition \nfor protecting stream ANC",
       caption = "Data from B. Jackson & T. McDonnell, 2018") + 
  
  theme(legend.position = "bottom") +
  # scale_fill_gradient2(
  #   low = "#999999",
  #   mid = "#ffffff",
  #   high = "#ef8a62",
  #   midpoint = 0,
  #   # direction = 1,
  #   name = expression(paste("CL exceedance (meq/", m^2, "/yr)")),
  #   # here we use guide_colourbar because it is still a continuous scale
  #   guide = guide_legend(
  #     direction = "horizontal",
  #     barheight = unit(2, units = "mm"),
  #     barwidth = unit(50, units = "mm"),
  #     draw.ulim = F,
  #     title.position = 'top',
  #     # some shifting around
  #     title.hjust = 0.5,
  #     label.hjust = 0.5
  #   ))
scale_fill_distiller(palette = "YlOrRd",
                     direction = 1,
                     name = expression(paste("CL ANCt (meq/", m^2, "/yr)")),
                     # here we use guide_colourbar because it is still a continuous scale
                     guide = guide_colorbar(direction = "horizontal",
                                            barheight = unit(2, units = "mm"),
                                            barwidth = unit(50, units = "mm"),
                                            draw.ulim = F,
                                            title.position = 'top',
                                            # some shifting around
                                            title.hjust = 0.5,
                                            label.hjust = 0.5)) 




ggsave("figures/mon_cl_anct_exceedance.jpg",
       height = 8,
       width = 7,
       units = "in")




ggplot() +
  
  geom_raster(data = relief_df, aes(x = x,
                                    y = y,
                                    alpha = relief)) +
  # # use the "alpha hack"
  scale_alpha(name = "", range = c(0.6, 0), guide = F)  +
  # CL polygons
  geom_polygon(data = mon_cl_df, aes(fill = CL_ANCt_Exceed_mon, 
                                     x = long, 
                                     y = lat, 
                                     group = group)) +
  geom_polygon(data = mon_nf_df, 
               aes(long, lat, group = group),
               fill = NA,
               color = "grey15",
               size = 0.6) +
 
  
  # add the previously defined basic theme
  theme_map() +
  coord_equal() +
  labs(x = NULL, 
       y = NULL, 
       title = "Monongahela NF Critical Loads",
       subtitle = "Exceedance of critical load of sulfur deposition \nfor protecting stream ANC",
       caption = "Data from B. Jackson & T. McDonnell, 2018") + 
  
  theme(legend.position = "right") +
  # scale_fill_gradient2(
  #   low = "#999999",
  #   mid = "#ffffff",
  #   high = "#ef8a62",
  #   midpoint = 0,
  #   # direction = 1,
  #   name = expression(paste("CL exceedance (meq/", m^2, "/yr)")),
  #   # here we use guide_colourbar because it is still a continuous scale
  #   guide = guide_legend(
  #     direction = "horizontal",
  #     barheight = unit(2, units = "mm"),
#     barwidth = unit(50, units = "mm"),
#     draw.ulim = F,
#     title.position = 'top',
#     # some shifting around
#     title.hjust = 0.5,
#     label.hjust = 0.5
#   ))
scale_fill_distiller(palette = "YlOrRd",
                     direction = 1,
                     name = expression(paste("CL ANCt (meq/", m^2, "/yr)")),
                     # here we use guide_colourbar because it is still a continuous scale
                     guide = guide_colorbar(direction = "horizontal",
                                            barheight = unit(4, units = "mm"),
                                            barwidth = unit(100, units = "mm"),
                                            draw.ulim = F,
                                            title.position = 'top',
                                            # some shifting around
                                            title.hjust = 0.5,
                                            label.hjust = 0.5)) +
  theme(plot.title = element_text(size = 36),
        plot.subtitle = element_text(size = 24),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18))


ggsave("figures/mon_cl_anct_exceedance.jpg", height = 8.5, width = 11, units = "in")


#----------------------------------------------------------------------------


########################################
## archivw
########################################

# continuous (non-breaked) CL
# CL_ANCt_mon
ggplot() +
  
  geom_raster(data = relief_df, aes(x = x,
                                    y = y,
                                    alpha = relief)) +
  # use the "alpha hack"
  scale_alpha(name = "", range = c(0.6, 0), guide = F)  +
  # CL polygons
  geom_polygon(data = mon_cl_df, aes(fill = CL_ANCt_mon, 
                                     x = long, 
                                     y = lat, 
                                     group = group)) +
  geom_polygon(data = mon_nf_df, 
               aes(long, lat, group = group),
               fill = NA,
               color = "grey15",
               size = 0.6) +
  
  coord_equal() +
  
  # add the previously defined basic theme
  theme_map() +
  
  labs(x = NULL, 
       y = NULL, 
       title = "Monongahela NF Critical Loads",
       subtitle = "CL of acid deposition to attain ANC threshold (ANCt)",
       caption = "Data from B. Jackson & T. McDonnell, 2018") + 
  
  theme(legend.position = "bottom") +
  scale_fill_distiller(palette = "YlOrRd",
                       direction = -1,
                       name = expression(paste("CL ANCt (meq/", m^2, "/yr)")),
                       # here we use guide_colourbar because it is still a continuous scale
                       guide = guide_colorbar(direction = "horizontal",
                                              barheight = unit(2, units = "mm"),
                                              barwidth = unit(50, units = "mm"),
                                              draw.ulim = F,
                                              title.position = 'top',
                                              # some shifting around
                                              title.hjust = 0.5,
                                              label.hjust = 0.5)) 


ggsave("figures/mon_cl_anct_exceedance.pdf",
       

#----------------------------------------------------------------------------




