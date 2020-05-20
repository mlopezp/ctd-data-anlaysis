# load packages ####
library(tidyverse)
library(here)
library(oce)
library(lubridate)
library(ggplot2)
library(cowplot)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(sf)
library(RColorBrewer)
library(zoo)

# importa data and merge into one table ####

# get a the names of .cnv files in the data folder and put them in a vector called temp
file.names <- list.files(path = "./", pattern = "*.cnv")

# read cnv files into a list of lists called ctd_objects
ctd_objects <- map(file.names, read.ctd)

# trim data to downcast and reduce data to 10 cm intervals. Only works on full data!! breaks when using averaged cnvs
ctd_trimmed <- ctd_objects %>%
  map(ctdTrim, method = "downcast") %>%
  map(ctdDecimate, p = 0.1)

# ctd_summaries <- map(ctd_trimmed, summary) # produces OCE summaries for each drop

# extract the data slot from each ctd_object and put it in a tibble called data
data <- map_dfr(ctd_trimmed, "data", .id = "file")

# extract the metadata from the metadata slot from each ctd object and put it in a tibble called metadata
metadata <- ctd_trimmed %>%
  map("metadata")

# extract location, site names and dates from metadata
mdata <- metadata %>%
  map_dfr(`[`, c("date", "station", "latitude", "longitude"), .id = "file") %>%
  mutate(Date = as_date(date),
         time = strftime(date, format = "%H:%M:%S"),
         site = station) %>%
  select(file, Date, time, site, latitude, longitude, -station, -date)

# Join data and metadata tables using file number for primary key.
df <- data %>%
  left_join(mdata, by = "file")

# clean the data ####
# standardize names

df$site <- df$site %>%
  str_replace("\"", "[]") %>%
  str_replace("^[(M|m)].*", "Maydan Mahzam") %>%
  str_replace("^[(F|f)].*", "Fasht East Halul") %>%
  str_replace("^[(B|b)].*", "Binzayan") %>%
  str_replace("^[(U|u)].*", "Umm Al Arshan") %>%
  str_replace("^[(S|s)].*", "Sheraoh") %>%
  str_replace("^[(NW|nw)].*", "NW Halul")

# add quarter column
df <- df %>%
  mutate(Quarter = as.yearqtr(Date, format = "%Y-%m-%d") %>%
           format(format = "Q%q-%Y")) %>%
  select(file, Date, Quarter, latitude, longitude, site, everything(), -time)

site.names <- unique(df$site)
site.names <- set_names(site.names) # use unique names for each site for labels in plots


# plots function ####
plots <- function(dataframe, sitename) { # plots function requires a dataframe and site name as parameters

# ggplot object - set defaults for following plots
  p <- ggplot(data = dataframe %>%
              na.omit() %>%
              filter(site == sitename),
              aes(y = depth, color = Quarter, group = Date)) + # used date for grouping because in some cases 2 drops were donde in one quarter
  geom_path() + # to draw profiles as lines 
  scale_x_continuous(position = "top") +
  scale_y_reverse(breaks = seq(0,40,5)) +
  theme_bw() +
  theme(axis.text = element_text(size = 12, colour = 1),
        axis.title = element_text(size = 14, colour = 1),
        legend.position = "none",
        plot.margin = unit(c(0,0.2,0,0),"cm")) +
  scale_color_brewer(palette = "Set1")

# temperature plot
temp <- p +
  aes(x = temperature) +
  scale_x_continuous(#breaks = seq(15, 36, 0.5) +
  labs(x = expression(~Temperature~(degree~C)), y = Depth~(m))

# salinity plot
salinity<- p +
  aes(x = salinity) +
  #scale_x_continuous(breaks = seq(39,41,.05) +
  labs(x = expression(~Salinity~(PSU))) +
  theme(axis.title.y.left = element_blank())

# dissolve oxygen plot
oxygen<- p +
  aes(x = oxygen) +
  #scale_x_continuous(breaks = seq(6.0,7.0,0.1) +
  labs(x = expression(~DO~(mgL^{-3}))) +
  theme(axis.title.y.left = element_blank())

# fluorescence plot
fluorescence<- p +
  aes(x = fluorescence) +
  #scale_x_continuous(breaks = seq(0, 1.1,0.2) +
  labs(x = expression(~Fluorescence~(mgm^{-3})),y = Depth~(m))

# turbidity plot
turbidity<- p +
  aes(x = turbidity) +
  #scale_x_continuous(breaks = seq(0.2,1.3,0.1) +
  labs(x = expression(~Turbidity~(NTU))) +
  theme(axis.title.y.left = element_blank())

# Map plot ####
theme_set(theme_bw())

# create the world map
world <- ne_countries(scale = "medium", returnclass = "sf",
                      country = c("Qatar", "Bahrain", "Saudi Arabia", "Iran"))
class(world)
world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

# plot the site location on the map
map<-ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(49, 54),
           ylim = c(24, 28),
           expand = FALSE,
           label_axes = list(top = "E", left = "N")) +
  geom_text(data= world_points,
            aes(x=X, y=Y, label=name))+
  geom_point(data = dataframe %>%
                    filter(site == sitename),
                    aes(x = longitude, y = latitude),
                    size = 3, shape = 16)+
  ggtitle("Sampling Location") +
  theme(plot.title= element_text(hjust = 0.5, vjust = -8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  annotate(geom = "text", 
           x = 52.5, 
           y = 26.5, 
           label = "Persian Gulf",
           fontface = "italic", 
           color = "grey22", 
           size = 4,
           angle = 325)

#Creating plot grid#####
# grid with all plots
P1 <- plot_grid(temp,salinity, map,fluorescence, turbidity,oxygen,
                nrow = 2, align = "h",label_size = 12)
# title object
title <- ggdraw() +
         draw_label(sitename)

# legend object
legend <- get_legend(temp + 
          theme(legend.position = "bottom"))

# grid with title, plots and legend
plot_grid(title, P1, legend, 
          ncol=1, 
          rel_heights = c(0.05, 1, 0.05))
}

# Loop to make separte pages for each site ####
site_plots <- map(site.names, ~plots(df, .x))

# to run this for one site replace site.names with the site name in quotes ####
# eg.
#map("Umm Al Arshan", ~plots(df, .x))

# save files ####

# save each grid as a page in  pdf in the Figures folder
#pdf("./../Figures/all sites.pdf", height = 5, width = 7)
#site_plots
#dev.off()

# save each grid as individual png
#plot.names <- paste0(site.names, ".png") #generate names for each plot
#walk2(plot.names, site_plots, ~ggsave(filename = .x, plot = .y,
#                                       path = "../Figures/",
#                                       height = 5, width = 7))


