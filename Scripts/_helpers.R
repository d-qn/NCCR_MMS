library(here)
library(tidyverse)
library(magrittr)
library(stringr)
library(knitr)
library(hrbrthemes)
library(skimr)
library(systemfonts)
library(Cairo)
library(ragg)
library(cowplot)
#library(marquee)
library(ggforce)

Sys.setlocale("LC_TIME", "fr_FR.UTF-8")
options(OutDec = ",")
hrbrthemes::update_geom_font_defaults()
options(scipen=999)
options(dplyr.summarise.inform = FALSE)

#library(paletteer)
#library(stevethemes)
library(colorspace)
library(patchwork)
library(ggtext)
library(waffle)
library(directlabels)
library(geomtextpath)
library(gghighlight)


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
    mig_2y = str_c(mig_2y,"\n—\n", mig_2y+1) %>% as.factor(),
    mig_4y = str_c(mig_4y,"\n—\n", mig_4y+3) %>% as.factor()
  )


### VIZ SETTINGS ###
library(showtext)

## Load the font from the system ##
font_add("RobotoSlab",
         regular = "/Users/duc-q.nguyen/Library/Fonts/RobotoSlab-VariableFont_wght.ttf",
         bold = "/Users/duc-q.nguyen/Library/Fonts/RobotoSlab-ExtraBold.ttf")

font_add("RobotoSlabLight", regular = "/Users/duc-q.nguyen/Library/Fonts/RobotoSlab-Light.ttf")

library(systemfonts)
# Register your font with systemfonts
register_font(
  name = "RobotoSlab",
  plain = "/Users/duc-q.nguyen/Library/Fonts/RobotoSlab-VariableFont_wght.ttf",
  bold = "/Users/duc-q.nguyen/Library/Fonts/RobotoSlab-ExtraBold.ttf"
)

register_font(
  name = "RobotoSlabLight",
  plain = "/Users/duc-q.nguyen/Library/Fonts/RobotoSlab-Light.ttf"
  )


### export as vector files 

mms_export_vector <- function(
    plot, 
    filepath, 
    height = 11.7,
    width = 8.3, 
    bg_col ="ffffff"
)  {

  #browser()
  filepath_vec <- str_replace(filepath, "\\.svg$", "_vec.svg")
  filepath_pdf <- str_replace(filepath, "\\.svg$", ".pdf")
    
  # 1. vectorized fonts
  showtext_auto(TRUE)
  svglite::svglite(
    filename = filepath_vec,
    height = height,
    width = width,
    fix_text_size = F,
    bg = bg_col
  )
  print(plot)
  dev.off()
  showtext_auto(F)
    
  # 2. ggsave
  ggsave(
    filename = filepath,
    plot = plot,
    device = svglite::svglite,
    width = width,
    height = height,
    bg = bg_col,
    scaling = 1,
    fix_text_size = FALSE
  )
  
  # 3. pdf
  # CairoPDF(filepath_pdf,
  #          height = height,
  #          width = width,
  #          bg = bg_col)
  # print(plot)
  # dev.off()
}

### New ggplot2 theme


# Temporarily add borders to see the actual plot areas
debug_theme <- theme(
  plot.margin = margin(5.5, 5.5, 5.5, 5.5, "pt"),
  plot.background = element_rect(colour = "red", fill = NA),
  panel.background = element_rect(colour = "blue", fill = NA)
)


mms_base_size <- 12
mms_base_family <- "RobotoSlab"
mms_subtitle_family <- "RobotoSlabLight"
mms_plot_title_size <- 13
mms_subtitle_size <- 11
mms_plot_title_color <- "#262428"  #"#665c70"
mms_plot_subtitle_color <- "#403d43"
mms_axis_tick_txt_color <- "#878f92"
mms_plot_bg_color <- "#ffffff"
mms_plot_line_color <-  "#ffffff" #"#d0d0d0"  #"#c8c8c8"

pw_bg_color <- mms_plot_bg_color
pw_padding <- margin(0, 0 , 10, 0)
pw_margin <- margin(20, 20, 20, 10)
mms_plot_margin <- margin(15, 20, 5, 10)
mms_plot_margin_l <- margin(15, 25, 5, 10)
mms_plot_margin_r <- margin(15, 10, 5, 25)
mms_legend_base_col <- "grey"

mms_txtbox_plot.title <- function(nrow_txt = 1) element_textbox_simple(
  color = mms_plot_title_color, 
  face = "bold",
  size = mms_plot_title_size,
  colour = mms_plot_title_color,
  lineheight = 1.3,
  halign = 0,  # ⬅️ align with left side of chart panel
 # padding = margin(0, 0, 0, 0),  # no internal padding
  margin = margin(0, 15, nrow_txt * 10, 15)  # space *after* title
)

mms_txtbox_plot.subtitle <- function(nrow_txt = 1) element_textbox_simple(
  color = mms_plot_subtitle_color,
  lineheight = 1.4,
  size = mms_subtitle_size,
  colour = mms_plot_subtitle_color,
  halign = 0,  # ⬅️ align with left side of chart panel
 # padding = margin(0, 0, 0, 0),
  margin = margin(0, 15, 25, 15)  # space *after* subtitle before plot
)


theme_mms <- function(
    base_size = mms_base_size, 
    base_family = mms_base_family, 
    subtitle_family = mms_subtitle_family,
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
      axis.text.x = element_text(color = mms_axis_tick_txt_color, margin=margin(t=2)),
      axis.text.y = element_text(color = mms_axis_tick_txt_color, margin=margin(r=2)),
      panel.background = element_rect(fill = mms_plot_bg_color, 
                                     color = NA, #mms_plot_line_color,
                                      linewidth = 0),
      plot.background = element_rect(fill = mms_plot_bg_color, 
                                     color = NA, #mms_plot_line_color,
                                     linewidth = 0),
      plot.margin = plot_margin,
      plot.caption = element_textbox_simple(
        hjust = 1,
        margin = margin(0, 0, 15, 0),
        color = mms_axis_tick_txt_color, 
        size = mms_base_size * 0.8, 
        family = mms_base_family
      ),
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

# =============================================================================
# CATEGORICAL COLOR SCALE (12 colors)
# =============================================================================

# ggplot2 scale functions for categorical
scale_color_nccr_categorical <- function(order = 1:14, ...) {
  colors <- nccr_categorical()[order]
  ggplot2::discrete_scale("colour", "nccr_categorical", 
                         function(n) colors[1:n], ...)
}

scale_fill_nccr_categorical <- function(order = 1:14, ...) {
  colors <- nccr_categorical()[order]
  ggplot2::discrete_scale("fill", "nccr_categorical", 
                         function(n) colors[1:n], ...)
}

# Convenience function to see color positions
show_nccr_categorical_numbers <- function() {
  colors <- nccr_categorical()
  par(mar = c(3, 1, 2, 1))
  barplot(rep(1, 14), col = colors, 
          main = "NCCR Categorical Colors with Numbers", 
          names.arg = 1:14, axes = FALSE)
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
  country = c("France", "Germany", "Spain", 
           "Austria", "Italy", "Portugal",
           "Other EU/EFTA",
           "Other Europe",
           "Other OECD",
           "Asia",
           "Latin America",
           "Africa",
           "Other countries"),
  
  color = c(nccr_sequential_blue()[3],
            nccr_sequential_blue()[4],
            nccr_sequential_blue()[5],
            
            nccr_sequential_blue()[6],
            nccr_sequential_blue()[7],
            nccr_sequential_blue()[8],
            
            nccr_sequential_blue()[1],
            nccr_sequential_blue()[2],
            
            nccr_bi_colors[12],
            
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
nccr_divergent_orange_green <- function() {
  colors <- c(
    '#985d45', '#af7560', '#c19080', '#cdada4', '#d3cbcd', '#a5b9a6', '#7fa486', '#5f8d69', '#457651'
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
  barplot(rep(1, 14), col = nccr_categorical(), 
          main = "NCCR Categorical (measured base colors)", axes = FALSE)
  
  # Sequential Education-based
  barplot(rep(1, 14), col = nccr_sequential_cyan(), 
          main = "nccr_sequential_cyan", axes = FALSE)
  
  barplot(rep(1, 14), col = nccr_sequential_blue(), 
          main = "nccr_sequential_blue", axes = FALSE)
  
  # Sequential Migration-based
  barplot(rep(1, 14), col = nccr_sequential_yellow(), 
          main = "NCCR Sequential Migration-based", axes = FALSE)
  
  # Divergent Gender-Education
  barplot(rep(1, 9), col = nccr_divergent_orange_green(), 
          main = "nccr_divergent_orange_green", axes = FALSE)
  
  # Divergent Age-Countries
  barplot(rep(1, 14), col = nccr_divergent_orange_purple(), 
          main = "nccr_divergent_orange_purple", axes = FALSE)
  
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

# rc_base_size <- 12
# rc_plot_title_size <- 13
# rc_subtitle_size <- 11
# rc_plot_title_color <- "#665c70"
# rc_plot_subtitle_color <- "#403d43"
# 
# rc_txtbox_plot.title <- function(nrow_txt = 1, fixed_top_margin = 0) element_textbox_simple(
#   color = rc_plot_title_color, 
#   lineheight = 1.3,
#   padding = margin(0, 1, 5, 0), 
#   margin = margin(0, 0, fixed_top_margin + (5 * nrow_txt), 0)
# )
# 
# rc_txtbox_plot.subtitle <- function(nrow_txt = 1, fixed_top_margin = 0) element_textbox_simple(
#   color = rc_plot_subtitle_color ,
#   lineheight = 1.4,
#   margin = margin(fixed_top_margin + (5 * nrow_txt), 0, 10, 0)
# )          
# 
# rc_axis_tick_txt_color <- "#b3afb6"
# pw_bg_color <- "#f2f2f2"
# 
# main_plot_bg_color <- "#ffffff" 
# 
# pw_padding <- margin(0, 0 , 0, 0)
# pw_margin <- margin(10, 3, 10, 3)
# 
# plot_margin <- margin(15, 15, 10, 0)
# legend_base_col <- "lightgrey"

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


