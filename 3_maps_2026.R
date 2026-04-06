# ================================
# ==========  Libraries ==========
# ================================

library(httr)
library(jsonlite)
library(tidyverse)
library(gtsummary)
library(gghighlight)
library(png)
library(grid)
library("viridis")
library(ggplot2)
library(showtext)
library(ggrepel)
library(cowplot)
library(ggtext)
library(extrafont)
library(scales)
library(ggridges)
library(ggpubr)
library(magick)
library(sf)

theme_barcelona_trashmap <- function(scale_factor = 5) {
  theme_void() +
    theme(
      ## Background Off-white/Cream
      plot.background = element_rect(fill = "#FAF9F6", color = NA),
      panel.background = element_rect(fill = "#FAF9F6", color = NA),

      ## Using Montserrat for a "modern" feel
      text = element_text(family = "montserrat"),

      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      panel.grid = element_blank(),

      plot.caption = ggtext::element_markdown(
        hjust = 0, vjust = 1,
        face = "italic", size = scale_factor * 7,
        lineheight = 0.4,
        family = "montserrat"
      ),

      plot.title = ggtext::element_markdown(
        face = "bold", size = scale_factor * 18,
        hjust = 0,
        lineheight = 0.3,
        family = "montserrat"
      ),

      plot.subtitle = ggtext::element_markdown(
        face = "italic",
        size = scale_factor * 14,
        lineheight = 0.3,
        family = "montserrat",
        margin = margin(b = 10)
      ),

      legend.position = "inside",
      legend.position.inside = c(.2, .28),
      legend.title = element_text(size = scale_factor * 12, face = "bold"),
      legend.text = element_text(size = scale_factor * 11, lineheight = 0.5),
      legend.background = element_rect(fill = alpha('#FAF9F6', 0), color = NA),

      plot.margin = margin(20, 20, 20, -20, "pt")
    )
}


# ================================
# ============  SETUP ============
# ================================

## Export settings

# scale_factor = 5
# variable name      |n      or | or \ Symbol on Keyboard
# "forge-"
# e901 github
# e900 discord
font_add(family = "emojis", "./indata/emojis.ttf")
font_import()
n
showtext_auto(TRUE)

sysfonts::font_add_google("Montserrat", "montserrat")
showtext::showtext_auto()

## Read shapefiles
sf_AEB <- sf::st_read("./indata/Barcelona_shp/0301040100_AEB_UNITATS_ADM.shp")
sf_barris <- sf::st_read("./indata/Barcelona_shp/0301040100_Barris_UNITATS_ADM.shp")
sf_districts <- sf::st_read("./indata/Barcelona_shp/0301040100_Districtes_UNITATS_ADM.shp")
sf_roads <- sf::st_read("./indata/BCN_GrafVial_SHP/BCN_GrafVial_Trams_ETRS89_SHP.shp")

sf_cens <- sf::st_read("./indata/Barcelona_shp/0301040100_SecCens_UNITATS_ADM.shp")

## Read data
df <- read_csv("./outdata/geocoded_scrapped_data20260401.csv") %>%

  bind_rows(read_csv("./outdata/extra_geocoded_1.csv")) %>%
  bind_rows(read_csv("./outdata/extra_geocoded_2.csv")) %>%
  ## Clean variables
  janitor::clean_names() %>%
  dplyr::select(-c(x1,x2)) %>%

  ## Filter out "muebles y trastos viejos en el teléfono 010"
  filter(nchar(time) < 50) %>%

  ## Encode variable of interst
  mutate(Time_custom = case_when(time=="Lunes, de 20.00 a 22.00 h."~ "Lunes",
                                 time=="Martes, de 20.00 a 22.00 h."~ "Martes",
                                 time=="Miércoles, de 20.00 a 22.00 h."~ "Miércoles",
                                 time=="Jueves, de 20.00 a 22.00 h."~ "Jueves",
                                 time=="Viernes, de 20.00 a 22.00 h."~ "Viernes",
                                 TRUE ~ time)) %>%
  mutate(Time_custom = factor(Time_custom,
                              c("Lunes","Martes",
                                "Miércoles","Jueves","Viernes"),
                              c("Lunes","Martes",
                                "Miércoles","Jueves","Viernes"))) %>%

  ## Encode variable of interst
  mutate(Time_custom_eng = case_when(time=="Lunes, de 20.00 a 22.00 h."~ "Monday",
                                     time=="Martes, de 20.00 a 22.00 h."~ "Tuesday",
                                     time=="Miércoles, de 20.00 a 22.00 h."~ "Wednesday",
                                     time=="Jueves, de 20.00 a 22.00 h."~ "Thursday",
                                     time=="Viernes, de 20.00 a 22.00 h."~ "Friday",
                                     TRUE ~ time)) %>%
  mutate(Time_custom_eng = factor(Time_custom_eng,
                                  c("Monday","Tuesday",
                                    "Wednesday","Thursday","Friday"),
                                  c("Monday","Tuesday",
                                    "Wednesday","Thursday","Friday"))) %>%

  ## Encode variable of interst
  mutate(Time_custom_cat = case_when(time == "Lunes, de 20.00 a 22.00 h."    ~ "Dilluns",
                                     time == "Martes, de 20.00 a 22.00 h."   ~ "Dimarts",
                                     time == "Miércoles, de 20.00 a 22.00 h." ~ "Dimecres",
                                     time == "Jueves, de 20.00 a 22.00 h."  ~ "Dijous",
                                     time == "Viernes, de 20.00 a 22.00 h."    ~ "Divendres",
                                     TRUE ~ time)) %>%
  mutate(Time_custom_cat = factor(Time_custom_cat,
                                  c("Dilluns", "Dimarts",
                                    "Dimecres", "Dijous", "Divendres"),
                                  c("Dilluns", "Dimarts",
                                    "Dimecres", "Dijous", "Divendres"))) %>%

  ## For good measure from previous version
  distinct(df_name,k,df_name_specific,.keep_all = T)


# ==================================
# ============  PROCESS ============
# ==================================
#
##==##==##==##
##  By AEB  ##
##==##==##==##

# Lat/Long Points are transformed into spatial objects
sf_AEB_dots <- st_as_sf(df %>%
                          filter(!is.na(lon)),

                        coords=c("lon","lat"),
                        crs=4326, remove=F) %>%

  st_transform(st_crs(sf_AEB)) %>%

  # Lat/Long Points are classified depending on which AEB area they "fall into"
  mutate(area_class_1 = lengths(st_within(.,sf_AEB)))

sf_AEB <- sf_AEB_dots %>%

  group_by(lat,lon) %>%
  slice(1) %>%
  ungroup() %>%


  st_join(sf_AEB) %>%
  as.data.frame(.) %>%

  ## Count by AEB
  group_by(Time_custom,Time_custom_eng,Time_custom_cat, AEB) %>%
  tally() %>%

  ## Filter by AEB
  group_by(AEB) %>%
  filter(n==max(n)) %>%

  left_join(sf_AEB, by=c("AEB"))


##==##==##==##
## By Censo ##
##==##==##==##
#
# sf_censo <- st_as_sf(df %>%
#                       filter(!is.na(lon)),
#
#                     coords=c("lon","lat"),
#                     crs=4326, remove=F) %>%
#
#   st_transform(st_crs(sf_cens)) %>%
#   mutate(area_class_1 = lengths(st_within(.,sf_cens)))
#
# sf_final_censo <- sf_censo %>%
#
#
#   group_by(lat,lon) %>%
#   slice(1) %>%
#   ungroup() %>%
#
#
#   st_join(sf_cens) %>%
#   as.data.frame(.) %>%
#
#   ## Count by Census area and AEB
#   group_by(Time_custom, AEB,SEC_CENS) %>%
#   tally() %>%
#
#   ## Filter by Census area and AEB
#   group_by(AEB,SEC_CENS) %>%
#   filter(n==max(n))
#
#
# sf_cens <- sf_cens %>%
#   left_join(sf_final_censo, by=c("AEB","SEC_CENS"))

# ===================================
# ============  PLOTTING ============
# ===================================

## Others
colors <- c("#D40000","#FFCC00","#D97D4B","#006747","#005F7F")

##==##==##==##
## Dot plot ##
##==##==##==##
plot_dots <- ggplot()+

  ## Data layer
  geom_sf(data=sf_AEB_dots%>%
            filter(area_class_1==1) %>% # Inside Barcelona
            group_by(lat,lon) %>%
            slice(1) %>% # Keep unique LAT/LON
            ungroup(),

          aes(color=Time_custom,geometry=geometry),
          alpha=0.35,stroke=NA,size=1) +

  scale_color_manual(values = colors,
                     name="De 20.00h a 22.00h.")  +

  ## Roads
  geom_sf(data=sf_roads,fill=NA,color=alpha("grey50"))+
  ## AEB
  geom_sf(data=sf_AEB,aes(geometry=geometry), fill = alpha("white",.25),color=alpha("black", .2), linetype = "dotted") +
  ## Barris
  geom_sf(data=sf_barris, fill=NA, color = alpha("black",.6), linetype = "dotted") +
  ## Districts
  geom_sf(data=sf_districts, fill=NA, color = alpha("black"))+

  labs(title="Días de Recojo de Muebles y Trastos", subtitle="Siempre hay algo que encontrar.",
       caption=c("<p><span style='font-family:emojis'>&#xe901;</span> /jruizcabrejos/barcelona_trastos",
                 "<br>Última Actualización:  &emsp; 29/01/2025 &#40;dd/mm/yyyy&#41;",
                 "<br><br>Fuente: https:&#47;&#47;www&#46;ajuntament.barcelona.cat/cercador-de-residus</p>"))+

  guides(color = guide_legend(override.aes = list(size=5,alpha=1)))  +

  theme_barcelona_trashmap()


ggsave("./image/esp_Puntos_Mapa_Barcelona_Muebles_Trastos_Dias.png",plot_dots,
       width=10,height=10,units="in",device="png",dpi=300)


##==##==#==##==##
##  Filled plot ##
##==##==##==##==##

AEB_plot <- ggplot() +

  ## Roads
  geom_sf(data=sf_roads,fill=NA, color=alpha("grey50"))+

  ## Data layer
  geom_sf(data=sf_AEB, # Inside Barcelona

          aes(fill=Time_custom, geometry = geometry),alpha=0.7, size=0.5)+
  scale_fill_manual(values = colors,
                     name="De 20.00h a 22.00h.",na.translate = F)   +
  ## AEB
  geom_sf(data=sf_AEB,aes(geometry = geometry), fill=NA, color=alpha("black", .2), linetype = "dotted") +
  ## Barris
  geom_sf(data=sf_barris, fill=NA, color = alpha("black",.6), linewidth =0.2) +
  ## Districts
  geom_sf(data=sf_districts, fill=NA, color = alpha("black"),linewidth=0.8)+

  labs(title="Días de Recojo de Muebles y Trastos", subtitle="Siempre hay algo que encontrar.",
       caption=c("<p><span style='font-family:emojis'>&#xe901;</span> /jruizcabrejos/barcelona_trastos",
                 "<br>Última Actualización:  &emsp; 29/01/2025 &#40;dd/mm/yyyy&#41;",
                 "<br><br>Fuente: https:&#47;&#47;www&#46;ajuntament.barcelona.cat/cercador-de-residus</p>"))+

  guides(color = guide_legend(override.aes = list(size=5,alpha=1)))  +

  theme_barcelona_trashmap()

ggsave("./image/esp_Mapa_Barcelona_Muebles_Trastos_Dias.png",AEB_plot,
       width=10,height=10,units="in",device="png",dpi=300)


source("./image/_maps_eng_cat.R")

