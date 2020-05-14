# load packages
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

#### importa data and merge into one table ####

# get a the names of .cnv files in the data folder and put them in a vector called temp
file.names <- list.files(path = here("Data"), pattern = "*.cnv")

# set working directory to data folder
setwd(here("Data"))

# read the data into a list of lists called ctd_objects
ctd_objects <- map(file.names, read.ctd)

# trim the data to downcast and reduce data to 10 cm intervals. only works on full data!! breaks when using average
ctd_trimmed <- ctd_objects %>%
  map(ctdTrim, method = "downcast") %>%
  map(ctdDecimate, p = 0.1)

ctd_summaries <- map(ctd_trimmed, summary)

#set working directory back to root --- need to fix this
setwd(here())

# extract the data slot from each ctd object and put it in a tibble called data
data <- map_dfr(ctd_trimmed, "data", .id = "file")

# extract the metadata from the metadata slot from each octd bject and put it in a tibble called metadata
metadata <- ctd_trimmed %>%
  map("metadata")

# extract location, site names and dates from metadata
mdata <- metadata %>%
  map_dfr(`[`, c("date", "station", "latitude", "longitude"), .id = "file") %>%
  mutate(Date = as_date(date),
         time = strftime(date, format = "%H:%M:%S"),
         site = station) %>%
  select(file, Date, time, site, everything(),-station, -date)

# Join data and metadata using file number for primary key.
df <- data %>%
  left_join(mdata, by = "file") %>%
  select(file, Date, time, latitude, longitude, site, everything())


#### plots ####
#### #####individual parameter plots using ggplot####

plots <- function(dataframe, sitename) {
temp <- ggplot(data = dataframe %>% na.omit() %>% filter(site == sitename),
              aes(x = temperature, y = depth, group = Date)) +
  geom_path(aes(col = `Date`)) +
  scale_y_reverse(breaks = seq(0,40,5)) +
  scale_x_continuous(breaks = seq(20,36,2), position = "top") +
  theme_bw() +
  theme(axis.text = element_text(size = 12, colour = 1),
        axis.title = element_text(size = 14, colour = 1),
        legend.position = "none") +
  labs(x = expression(~Temperature~(degree~C)), y = Depth~(m))

salinity<- ggplot(data = dataframe %>% na.omit() %>% filter(site == sitename),
                  aes(x = salinity, y = depth, group=Date))+
  geom_path(aes(col = `Date`))+
  scale_y_reverse(breaks = seq(0,40,5))+
  scale_x_continuous(breaks = seq(40,41,.05), position = "top")+
  theme_bw()+
  theme(axis.text = element_text(size = 12, colour = 1),
        axis.title = element_text(size = 14, colour = 1),
        axis.title.y.left = element_blank(),
        legend.position = "none")+
  labs(x = expression(~Salinity~(PSU)))

oxygen<- ggplot(data = dataframe %>% na.omit() %>% filter(site == sitename),
                aes(x = oxygen, y = depth, group=Date))+
  geom_path(aes(col = `Date`))+
  scale_y_reverse(breaks = seq(0,40,5))+
  scale_x_continuous(breaks = seq(6.3,6.5,.05), position = "top")+
  theme_bw()+
  theme(axis.text = element_text(size = 12, colour = 1),
        axis.title = element_text(size = 14, colour = 1),
        axis.title.y.left = element_blank(),
        legend.position = "none")+
  labs(x = expression(~DO~(mgL^{-3})))

fluorescence<- ggplot(data = dataframe %>% na.omit() %>% filter(site == sitename),
                      aes(x = fluorescence, y = depth, group=Date))+
  geom_path(aes(col = `Date`))+
  scale_y_reverse(breaks = seq(0,40,5))+
  scale_x_continuous(breaks = seq(0.2,1.1,.1), position = "top")+
  theme_bw()+
  theme(axis.text = element_text(size = 12, colour = 1),
        axis.title = element_text(size = 14, colour = 1),
        legend.position = "none")+
  labs(x = expression(~Fluorescence~(mgm^{-3})),y = Depth~(m))


turbidity<- ggplot(data = dataframe %>% na.omit() %>% filter(site == sitename),
                   aes(x = turbidity, y = depth, group=Date))+
  geom_path(aes(col = `Date`))+
  scale_y_reverse(breaks = seq(0,40,5))+
  scale_x_continuous(breaks = seq(0.2,1.3,.1), position = "top")+
  theme_bw()+
  theme(axis.text = element_text(size = 12, colour = 1),
        axis.title = element_text(size = 14, colour = 1),
        axis.title.y.left = element_blank(),
        legend.position = "none")+
  labs(x = expression(~Turbidity~(NTU)))

legend <- get_legend(temp + theme(legend.position = "bottom"))

#####Creating maps#####
theme_set(theme_bw())

world <- ne_countries(scale = "medium",returnclass = "sf")
class(world)
world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

map<-ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(49, 54),
           ylim = c(24, 28), expand = FALSE)+
  geom_text(data= world_points,aes(x=X, y=Y, label=name))+
  geom_point(data = dataframe %>% filter(site == sitename), aes(x = longitude, y = latitude), size = 3,
             shape = 16)+
  #ggtitle("Sampling Location")+
  theme(plot.title= element_text(hjust = .5,vjust = 0.5), axis.title.x = element_blank(),axis.title.y = element_blank())+
  annotate(geom = "text", x = 52.5, y = 26.5, label = "Persian Gulf",
           fontface = "italic", color = "grey22", size = 4,angle = 325)

#####Creating plot grid#####
P1 <- plot_grid(temp,salinity, map,
              fluorescence, turbidity,oxygen,
              nrow = 2, align = "h",label_size = 12)

plot_grid(P1, legend, ncol=1, rel_heights = c(1,.1))
}
