library(here)
library(tidyverse)
library(magrittr)
library(stringr)
library(knitr)
library(hrbrthemes)
library(skimr)
library(systemfonts)
library(marquee)
library(ggforce)

Sys.setlocale("LC_TIME", "fr_FR.UTF-8")
options(OutDec = ",")
hrbrthemes::update_geom_font_defaults()
options(scipen=999)
options(dplyr.summarise.inform = FALSE)

library(paletteer)
library(colorspace)
library(stevethemes)
library(patchwork)
library(ggtext)
library(waffle)
library(directlabels)
library(geomtextpath)


### Load & wrangle ###
mms_load_data <- function(main = T) {
  if(main) {
    read_csv(here("Output", "data", "merged_MMS_AWYonly_files.csv"))
  } else {
    read_csv(here("Output", "data", "merged_MMS_complete_files.csv"))
  }
}
mms_df <- mms_load_data()












### VIZ SETTINGS ###
library(showtext)

## Load the font from the system ##
font_add("Roboto Condensed", 
         regular = "/Library/Fonts/RobotoCondensed-Regular.ttf",
         bold = "/Library/Fonts/RobotoCondensed-Bold.ttf")

font_add(font_rc_light, regular = "/Library/Fonts/RobotoCondensed-Light.ttf")
font_add(font_ps,  
         regular =  "/Users/duc-q.nguyen/Library/Fonts/IBMPlexSans-Regular.ttf",
         bold =  "/Users/duc-q.nguyen/Library/Fonts/IBMPlexSans-Bold.ttf")


font_add("Merriweather", 
         regular = systemfonts::match_font("Merriweather"),
         bold = "/Users/duc-q.nguyen/Library/Fonts/Merriweather-Bold.ttf"
)

font_add("Ratio", regular = systemfonts::match_font("Ratio")$path)
font_add("Ratio-Medium", regular = systemfonts::match_font("Ratio-Medium")$path)
showtext_auto()


## ggplot2 settings ##

rc_base_size <- 12
rc_plot_title_size <- 13
rc_subtitle_size <- 11
rc_plot_title_color <- "#665c70"
rc_plot_subtitle_color <- "#403d43"


rc_txtbox_plot.title <- function(nrow_txt = 1, fixed_top_margin = 0) element_textbox_simple(
  color = rc_plot_title_color, 
  lineheight = 1.3,
  padding = margin(0, 1, 5, 0), 
  margin = margin(0, 0, fixed_top_margin + (5 * nrow_txt), 0)
)

rc_txtbox_plot.subtitle <- function(nrow_txt = 1, fixed_top_margin = 0) element_textbox_simple(
  color = rc_plot_subtitle_color ,
  lineheight = 1.4,
  margin = margin(fixed_top_margin + (5 * nrow_txt), 0, 10, 0)
)          

rc_axis_tick_txt_color <- "#b3afb6"
pw_bg_color <- "#f2f2f2"

main_plot_bg_color <- "#ffffff" 

pw_padding <- margin(0, 0 , 0, 0)
pw_margin <- margin(10, 3, 10, 3)

plot_margin <- margin(15, 15, 10, 0)
legend_base_col <- "lightgrey"

