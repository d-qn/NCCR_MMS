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
font_add("roboto_slab", 
         regular = "/Users/duc-q.nguyen/Library/Fonts/RobotoSlab-VariableFont_wght.ttf",
         bold = "/Users/duc-q.nguyen/Library/Fonts/RobotoSlab-ExtraBold.ttf")

font_add("robot_slab_light", regular = "/Users/duc-q.nguyen/Library/Fonts/RobotoSlab-Light.ttf")

showtext_auto()


### New ggplot2 theme

mms_base_size <- 12
mms_base_family <- "roboto_slab"
mms_subtitle_family <- "robot_slab_light"
mms_plot_title_size <- 13
mms_subtitle_size <- 11
mms_plot_title_color <- "#403d43"  #"#665c70"
mms_plot_subtitle_color <- "#403d43"
mms_axis_tick_txt_color <- "#817986"
mms_plot_bg_color <- "#ffffff"
mms_plot_line_color <- "#ffffff" #"#d0d0d0"  #"#c8c8c8"

pw_bg_color <- mms_plot_bg_color
pw_padding <- margin(0, 0 , 3, 0)
pw_margin <- margin(30, 20, 20, 10)
mms_plot_margin <- margin(15, 20, 5, 10)
mms_plot_margin_l <- margin(15, 25, 5, 10)
mms_plot_margin_r <- margin(15, 10, 5, 25)
mms_legend_base_col <- "grey"

mms_txtbox_plot.title <- function(nrow_txt = 1, fixed_top_margin = 0) element_textbox_simple(
  color = mms_plot_title_color, 
  face = "bold",
  size = mms_plot_title_size,
  colour = mms_plot_title_color,
  lineheight = 1.3,
  padding = margin(0, 1, 4, 0), 
  margin = margin(0, 0, fixed_top_margin + (5 * nrow_txt), 0)
)

mms_txtbox_plot.subtitle <- function(nrow_txt = 1, fixed_top_margin = 0) element_textbox_simple(
  color = mms_plot_subtitle_color,
  lineheight = 1.4,
  size = mms_subtitle_size,
  colour = mms_plot_subtitle_color,
  margin = margin(fixed_top_margin + (5 * nrow_txt), 0, 10, 0)
)


theme_mms <- function(
    base_size = mms_base_size, 
    base_family = "roboto_slab", 
    subtitle_family = "robot_slab_light",
    mms_plot_title = mms_txtbox_plot.title(),
    mms_plot_subtitle = mms_txtbox_plot.subtitle(),
    plot_title_size = mms_plot_title_size,
    plot_subtitle_size = mms_subtitle_size,
    title.color = mms_plot_title_color,
    subtitle.color = mms_plot_subtitle_color,
    plot_margin = mms_plot_margin,
    main_plot_bg_color = mms_plot_bg_color,
    panel_bg_color = pw_bg_color,
    ...
) {
  hrbrthemes::theme_ipsum(
    base_size = mms_base_size,
    base_family = mms_base_family, 
    subtitle_family = subtitle_family,
    ...
  ) %+replace% 
    theme(
      plot.title = mms_plot_title,
      plot.title.position = "plot",
      plot.subtitle = mms_plot_subtitle,
      axis.text.x = element_text(color = mms_axis_tick_txt_color),
      axis.text.y = element_text(color = mms_axis_tick_txt_color),
      panel.background = element_rect(fill = mms_plot_bg_color, 
                                      #color = mms_plot_line_color,
                                      linewidth = 0),
      plot.background = element_rect(fill = mms_plot_bg_color, 
                                     color = mms_plot_line_color,
                                     linewidth = 1),
      plot.margin = plot_margin
    )
}


nccr_bi_colors <- c(
  "#B47B94", "#8C6074",
  "#70A4BA", "#578092",
  "#CFC666", "#9F994F",
  "#C8866F", "#9F6C59",
  "#8884A6", "#6E6C88",
  "#80AF8A", "#678E6F",
  "#b9c3c6", '#878f92'
) 


### END styleguide ###


# NCCR Color Scales for ggplot2
# Based on the NCCR "on the move" style guide


# =============================================================================
# CATEGORICAL COLOR SCALE (12 colors)
# =============================================================================

nccr_categorical <- function() {
  # Exact colors from NCCR style guide (measured with Apple Digital Color Meter)
  return(nccr_bi_colors)
}

# Function to get all 31 colors from style guide
# nccr_all_colors <- function() {
#   colors <- c(
#     # Gender category
#     "#CC66CC",    # Female: C=0, M=50, Y=0, K=20
#     "#994D99",    # Male: C=0, M=50, Y=0, K=40
#     "#B359B3",    # Base: C=0, M=50, Y=0, K=30
#     # Education category
#     "#80FFFF",    # None: C=50, M=0, Y=0, K=0
#     "#6CD9D9",    # Secondary I: C=50, M=0, Y=0, K=15
#     "#59B3B3",    # Secondary II: C=50, M=0, Y=0, K=30
#     "#468C8C",    # Tertiary: C=50, M=0, Y=0, K=45
#     # Migration category
#     "#D9D936",    # Professional only: C=0, M=0, Y=75, K=15
#     "#B3B32D",    # Family only: C=0, M=0, Y=75, K=30
#     "#8C8C23",    # Family+professional: C=0, M=0, Y=75, K=45
#     "#66661A",    # Other reasons: C=0, M=0, Y=75, K=60
#     # Age category
#     "#E67373",    # 24-40 light: C=0, M=50, Y=50, K=10
#     "#CC6666",    # Base: C=0, M=50, Y=50, K=20
#     "#B35959",    # 24-40 dark: C=0, M=50, Y=50, K=30
#     # Arrival category
#     "#9999FF",    # 2020-2024: C=40, M=40, Y=0, K=0
#     "#7A7ACC",    # 2015-2019: C=40, M=40, Y=0, K=20
#     "#6363A6",    # 2010-2014: C=40, M=40, Y=0, K=35
#     "#4D4D80",    # 2006-2009: C=40, M=40, Y=0, K=50
#     "#7ACCCC",    # Base: C=40, M=0, Y=0, K=20
#     # Countries category
#     "#59B359",    # Other Europe: C=50, M=0, Y=50, K=30
#     "#7DB37D",    # Italy: C=30, M=0, Y=30, K=30
#     "#66CC66",    # Other OECD: C=50, M=0, Y=50, K=20
#     "#4D994D",    # Other EU/EFTA: C=50, M=0, Y=50, K=40
#     "#6B996B",    # Austria: C=30, M=0, Y=30, K=40
#     "#80FF80",    # Africa: C=50, M=0, Y=50, K=0
#     "#408040",    # Germany: C=50, M=0, Y=50, K=50
#     "#598059",    # Portugal: C=30, M=0, Y=30, K=50
#     "#8AE68A",    # Latin America: C=40, M=0, Y=40, K=10
#     "#336633",    # France: C=50, M=0, Y=50, K=60
#     "#476647",    # Spain: C=30, M=0, Y=30, K=60
#     "#A1E6A1"     # Asia: C=30, M=0, Y=30, K=10
#   )
#   return(colors)
# }

# =============================================================================
# CATEGORICAL COLOR SCALE (12 colors)
# =============================================================================

# ggplot2 scale functions for categorical
scale_color_nccr_categorical <- function(order = 1:12, ...) {
  colors <- nccr_categorical()[order]
  ggplot2::discrete_scale("colour", "nccr_categorical", 
                         function(n) colors[1:n], ...)
}

scale_fill_nccr_categorical <- function(order = 1:12, ...) {
  colors <- nccr_categorical()[order]
  ggplot2::discrete_scale("fill", "nccr_categorical", 
                         function(n) colors[1:n], ...)
}

# Convenience function to see color positions
show_nccr_categorical_numbers <- function() {
  colors <- nccr_categorical()
  par(mar = c(3, 1, 2, 1))
  barplot(rep(1, 12), col = colors, 
          main = "NCCR Categorical Colors with Numbers", 
          names.arg = 1:12, axes = FALSE)
  par(mar = c(5, 4, 4, 2))
}


# =============================================================================
# SEQUENTIAL COLOR SCALES (12 colors each)
# =============================================================================

# Sequential scale 1: Education-based (from measured #5494A8)
nccr_sequential_cyan <- function() {
  # Base color: #5494A8 (measured)
  colors <- rev(c(
    '#406676', '#527788', '#658999', '#7a9baa', '#90adbb', '#a7bfcc', '#bfd2dc', '#d8e4eb', '#f4f7f9'
  ))
  return(colors)
}

# https://gka.github.io/palettes/#/12%7Cs%7C00536f,578092,528ea7,9a9a9a,a6ccd8%7Cffffe0,ff005e,93003a%7C1%7C1
nccr_sequential_blue <- function() {
  colors <- c(
    '#00536f', '#1f5d77', '#316780', '#3f7289', '#4a7d93', '#52889f', '#6992a5', '#829ba7', '#91a6af', '#9bb2ba', '#a2bfc8', '#a6ccd8'
  )
  return(colors)
}

pays2col <- tibble(
  pays = c("France", "Allemagne", "Espagne", "Autriche", "Italie", "Portugal",
           "Autre UE/AELE",
           "Autre Europe",
           "Autre OCDE",
           "Asie",
           "Amerique latine",
           "Afrique",
           "Autres pays"),
  
  color = c(nccr_sequential_blue()[3],
            nccr_sequential_blue()[4],
            nccr_sequential_blue()[5],
            nccr_sequential_blue()[6],
            nccr_sequential_blue()[7],
            nccr_sequential_blue()[8],
            
            nccr_sequential_blue()[1],
            nccr_sequential_blue()[2],
            
            nccr_sequential_cyan()[5],
            
            nccr_bi_colors[7],
            nccr_bi_colors[1],
            nccr_bi_colors[8],
            nccr_bi_colors[3])
)

# Sequential scale 2: Migration-based (from measured #B8AF4C)
nccr_sequential_yellow <- function() {
  # Base color: #B8AF4C (measured)
  colors <- c(
    "#FDFCF6",  # Very light
    "#FBF9ED",  # 
    "#F9F6E4",  # 
    "#F7F3DB",  # 
    "#F5F0D2",  # 
    "#F3EDC9",  # 
    "#E8D5B0",  # 
    "#DDBD97",  # 
    "#D2A57E",  # 
    "#B8AF4C",  # Base measured color
    "#9F944D",  # 
    "#86794E"   # Very dark
  )
  return(colors)
}

# ggplot2 scale functions for sequential
scale_color_nccr_seq_cyan <- function(...) {
  ggplot2::scale_color_gradientn(colors = nccr_sequential_cyan(), ...)
}

scale_fill_nccr_seq_cyan <- function(...) {
  ggplot2::scale_fill_gradientn(colors = nccr_sequential_cyan(), ...)
}

scale_color_nccr_seq_yellow <- function(...) {
  ggplot2::scale_color_gradientn(colors = nccr_sequential_yellow(), ...)
}

scale_fill_nccr_seq_yellow <- function(...) {
  ggplot2::scale_fill_gradientn(colors = nccr_sequential_yellow(), ...)
}

# =============================================================================
# DIVERGENT COLOR SCALES (12 colors each)
# =============================================================================

# Divergent scale 1: Gender to Education (measured colors)
nccr_divergent_magenta_cyan <- function() {
  colors <- c(
    "#8E5A6F",   # Dark gender
    "#94606B",   # 
    "#9A6667",   # 
    "#A06C63",   # 
    "#A6725F",   # 
    "#AC785B",   # Light gender
    "#7B8899",   # Light education
    "#758E9D",   # 
    "#6F94A1",   # 
    "#699AA5",   # 
    "#63A0A9",   # 
    "#5DA6AD"    # Dark education
  )
  return(colors)
}

# Divergent scale 2: Age to Countries (measured colors)
nccr_divergent_orange_purple <- function() {
  colors <- c(
    "#985F52",   # Dark age
    "#9D6857",   # 
    "#A2715C",   # 
    "#A77A61",   # 
    "#AC8366",   # 
    "#B18C6B",   # Light age
    "#7C8889",   # Light countries
    "#7B8B7E",   # 
    "#7A8E73",   # 
    "#799168",   # 
    "#78945D",   # 
    "#779752"    # Dark countries
  )
  return(colors)
}

# ggplot2 scale functions for divergent
scale_color_nccr_div_magcyan <- function(...) {
  ggplot2::scale_color_gradientn(colors = nccr_divergent_magenta_cyan(), ...)
}

scale_fill_nccr_div_magcyan <- function(...) {
  ggplot2::scale_fill_gradientn(colors = nccr_divergent_magenta_cyan(), ...)
}

scale_color_nccr_div_orpur <- function(...) {
  ggplot2::scale_color_gradientn(colors = nccr_divergent_orange_purple(), ...)
}

scale_fill_nccr_div_orpur <- function(...) {
  ggplot2::scale_fill_gradientn(colors = nccr_divergent_orange_purple(), ...)
}

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# Display all palettes including the measured colors
show_nccr_palettes <- function() {
  par(mfrow = c(6, 1), mar = c(1, 1, 2, 1))
  
  # Categorical (12 colors with measured base colors)
  barplot(rep(1, 12), col = nccr_categorical(), 
          main = "NCCR Categorical (measured base colors)", axes = FALSE)
  
  # Sequential Education-based
  barplot(rep(1, 12), col = nccr_sequential_cyan(), 
          main = "NCCR Sequential Education-based", axes = FALSE)
  
  barplot(rep(1, 12), col = nccr_sequential_blue(), 
          main = "NCCR Sequential Education-based", axes = FALSE)
  
  # Sequential Migration-based
  barplot(rep(1, 12), col = nccr_sequential_yellow(), 
          main = "NCCR Sequential Migration-based", axes = FALSE)
  
  # Divergent Gender-Education
  barplot(rep(1, 12), col = nccr_divergent_magenta_cyan(), 
          main = "NCCR Divergent Gender-Education", axes = FALSE)
  
  # Divergent Age-Countries
  barplot(rep(1, 12), col = nccr_divergent_orange_purple(), 
          main = "NCCR Divergent Age-Countries", axes = FALSE)
  
  par(mfrow = c(1, 1))
}

# Print color hex codes
# print_nccr_colors <- function() {
#   cat("NCCR Color Palettes (Based on Apple Digital Color Meter measurements)\n")
#   cat("====================================================================\n\n")
#   
#   cat("Measured Base Colors:\n")
#   cat("Gender:    #A86B85\n")
#   cat("Education: #5494A8\n") 
#   cat("Migration: #B8AF4C\n")
#   cat("Age:       #B37964\n")
#   cat("Arrival:   #7B7897\n")
#   cat("Countries: #749E7C\n\n")
#   
#   cat("Categorical (12 colors):\n")
#   cat(paste(nccr_categorical(), collapse = ", "), "\n\n")
#   
#   cat("Sequential Education-based:\n")
#   cat(paste(nccr_sequential_cyan(), collapse = ", "), "\n\n")
#   
#   cat("Sequential Migration-based:\n")
#   cat(paste(nccr_sequential_yellow(), collapse = ", "), "\n\n")
#   
#   cat("Divergent Gender-Education:\n")
#   cat(paste(nccr_divergent_magenta_cyan(), collapse = ", "), "\n\n")
#   
#   cat("Divergent Age-Countries:\n")
#   cat(paste(nccr_divergent_orange_purple(), collapse = ", "), "\n\n")
# }

# =============================================================================
# USAGE EXAMPLES
# =============================================================================

# Example usage:
# library(ggplot2)
# 
# # Categorical data
# ggplot(mtcars, aes(x = factor(cyl), fill = factor(gear))) +
#   geom_bar() +
#   scale_fill_nccr_categorical()
# 
# # Sequential data
# ggplot(mtcars, aes(x = wt, y = mpg, color = hp)) +
#   geom_point(size = 3) +
#   scale_color_nccr_seq_cyan()
# 
# # Divergent data
# ggplot(mtcars, aes(x = wt, y = mpg, color = hp - mean(hp))) +
#   geom_point(size = 3) +
#   scale_color_nccr_div_magcyan()













## ggplot2 settings old ##

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

## ggplot2 settings old END ##


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


