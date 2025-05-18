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

# utility generic yes/no reencoding function
mms_encode_yes_no_code <- function(x) {
  x = case_when(
    x == "1" ~ "Oui",
    x == "2" ~ "Non",
    TRUE ~ NA_character_
  )
}

# recode A6 arrival year
mms_encode_migration_year <- function(x, year) {
  year - (ifelse(x < 0, NA, x) -1)
}

migy2yearIntervals <- tibble(
  migy = 2005:2024,
  mig_2y = rep(seq(2005, 2024, 2), each = 2),
  mig_4y = rep(seq(2005,2024, 4), each = 4)
) %>% 
  mutate(
    mig_2y = str_c(mig_2y,"\n", mig_2y+1) %>% as.factor(),
    mig_4y = str_c(mig_4y,"\n", mig_4y+3) %>% as.factor()
  )








# https://projects.susielu.com/viz-palette?colors=%5B%22#66b8d4%22,%22%23f7d777%22,%22%23d1ab75%22,%22%23d97053%22,%22%238fd176%22,%22%23198f58%22,%22%236a7aaa%22,%22%230050b5%22,%22%236d5288%22,%22%23d17393%22,%22%23eca53a%22,%22%2354aa9f%22,%22%23b1b1b1%22%5D&backgroundColor=%22white%22&fontColor=%22black%22&mode=%22normal%22
pays2col <- tibble(
  pays = c("France", "Allemagne", "Espagne", "Autriche", "Italie", "Portugal",
           "Autre UE/AELE",
           "Autre Europe",
           "Autre OCDE",
           "Asie",
           "Amerique latine",
           "Afrique",
           "Autres pays"),
  
  color = c("#66b8d4","#f7d777","#d1ab75","#d97053",
            "#8fd176","#198f58","#6a7aaa","#0050b5",
            "#6d5288","#d17393","#eca53a","#855541",
            "#b1b1b1")
)

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
         regular = systemfonts::match_fonts("Merriweather"),
         bold = "/Users/duc-q.nguyen/Library/Fonts/Merriweather-Bold.ttf"
)

font_add("Ratio", regular = systemfonts::match_fonts("Ratio")$path)
font_add("Ratio-Medium", regular = systemfonts::match_fonts("Ratio-Medium")$path)
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


## Marimekko wrangle helper 

mms_mekko_wrangler <- function(df, 
                               # the var to stack the column on
                               var1, 
                               # the var to stack the rows on
                               var2, 
                               # var value
                               varv,
                               tot_n) {
#  browser()
  df %>% 
    group_by(!!sym(var1)) %>% 
    arrange(desc(!!sym(var2))) %>% 
    mutate(
      ymax = cumsum(!!sym(tot_n)) / sum(!!sym(tot_n)),
      ymin = (ymax - (!!sym(tot_n)/sum(!!sym(tot_n))))
    ) %>% 
    ungroup() %>%
    group_by(!!sym(var2)) %>%
    arrange(!!sym(var1)) %>%
    mutate(
      xmax = cumsum(!!sym(varv)), 
      xmin = xmax - !!sym(varv)
    ) %>%
    ungroup() %>%
    arrange(!!sym(var2)) 
}


