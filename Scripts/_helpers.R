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

mms_base_size <- 13
mms_base_family <- "roboto_slab"
mms_subtitle_family <- "robot_slab_light"
mms_plot_title_size <- 14
mms_subtitle_size <- 14
mms_plot_title_color <- "#0d0d0d"  #"#665c70"
mms_plot_subtitle_color <- "#403d43"
mms_axis_tick_txt_color <- "#9a949e"
mms_plot_bg_color <- "#ffffff"

pw_bg_color <- mms_plot_bg_color
pw_padding <- margin(0, 0 , 0, 0)
pw_margin <- margin(10, 3, 10, 3)
mms_plot_margin <- margin(10, 10, 5, 10)
mms_legend_base_col <- "lightgrey"

mms_txtbox_plot.title <- function(nrow_txt = 1, fixed_top_margin = 0) element_textbox_simple(
  color = mms_plot_title_color, 
  face = "bold",
  size = mms_plot_title_size,
  colour = mms_plot_title_color,
  lineheight = 1.3,
  padding = margin(0, 1, 5, 0), 
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
      plot.subtitle = mms_plot_subtitle,
      axis.text.x = element_text(color = mms_axis_tick_txt_color),
      axis.text.y = element_text(color = mms_axis_tick_txt_color),
      panel.background = element_rect(fill = mms_plot_bg_color, 
                                      color = mms_plot_bg_color,
                                      linewidth = 0),
      plot.background = element_rect(fill = mms_plot_bg_color, 
                                     color = mms_plot_bg_color,
                                     linewidth = 0),
      plot.margin = plot_margin
    )
}


### Exact style guide colors ###

mms_colors_gender <- function() {
  c(
    Female = "#BE7895",   # C=0 M=50 Y=0 K=20
    Male   = "#945E75",   # C=0 M=50 Y=0 K=40
    Base   = "#A86B85"    # K=30
  )
}

mms_colors_education <- function() {
  c(
    Aucune         = "#71C8E6",  # 50/0/0/0
    `Secondaire I`  = "#65AEC7",  # K=15
    `Secondaire II` = "#5494A8",  # K=30
    Tertiaire     = "#44798C",  # K=45
    Base         = "#5494A8"   # K=30 (duplicate of Secondary_II)
  )
}

mms_colors_reasons <- function() {
  c(
    Professional_only      = "#DED159",  # 0/0/75/15
    Family_only            = "#B8AF4C",  # K=30
    Family_and_professional= "#958E3E",  # K=45
    Other_reasons          = "#777331",  # K=60
    Base                   = "#B8AF4C"
  )
}

mms_colors_arrival <- function() {
  c(
    "2020–2024" = "#948EB8",  # 40/40/0/0
    "2015–2019" = "#7C789A",  # K=20
    "2010–2014" = "#696582",  # K=35
    "2006–2009" = "#54526B",  # K=50
    Base        = "#7C789A"
  )
}

mms_colors_countries <- function() {
  c(
    Other_Europe     = "#5B8F6C",  # 50/0/50/30
    Italy            = "#996699",  # 30/0/30/30
    Germany          = "#804D80",  # 50/0/50/50
    Africa           = "#FFCCFF",  # 50/0/50/0
    France           = "#734D73"   # 50/0/50/60
    # Add others as needed
  )
}
mms_colors_age <- function() {
  c(
    "24–40" = "#D3826A",  # 0/50/50/0
    "25–34" = "#A86955",  # 0/50/50/20
  )
}


### END styleguide ###


# NCCR Color Scales for ggplot2
# Based on the NCCR "on the move" style guide

# Precise CMYK to RGB conversion (colorimeter standard)
cmyk_to_rgb <- function(c, m, y, k) {
  # Convert percentages to decimals
  c <- c / 100
  m <- m / 100
  y <- y / 100
  k <- k / 100
  
  # Standard colorimeter conversion formula
  r <- round(255 * (1 - c) * (1 - k))
  g <- round(255 * (1 - m) * (1 - k))
  b <- round(255 * (1 - y) * (1 - k))
  
  # Return hex code
  sprintf("#%02X%02X%02X", r, g, b)
}

# Exact base colors from style guide (measured with Apple Digital Color Meter)
base_colors <- list(
  gender = "#A86B85",      # Page 1 - Gender base
  education = "#5494A8",   # Page 2 - Education base
  migration = "#B8AF4C",   # Page 3 - Migration base
  age = "#B37964",         # Page 4 - Age base
  arrival = "#7B7897",     # Page 5 - Arrival base
  countries = "#749E7C"    # Page 6 - Countries base
)

# =============================================================================
# CATEGORICAL COLOR SCALE (12 colors)
# =============================================================================

nccr_categorical <- function() {
  # Exact colors from NCCR style guide (measured with Apple Digital Color Meter)
  colors <- c(
    "#A86B85",    # Gender base
    "#5494A8",    # Education base
    "#B8AF4C",    # Migration base
    "#B37964",    # Age base
    "#7B7897",    # Arrival base
    "#749E7C",    # Countries base
    # Harmonious variations based on measured colors
    "#8E5A6F",    # Darker gender variant
    "#477A8A",    # Darker education variant
    "#9A9240",    # Darker migration variant
    "#985F52",    # Darker age variant
    "#686080",    # Darker arrival variant
    "#62836A"     # Darker countries variant
  )
  return(colors)
}

# Function to get all 31 colors from style guide
nccr_all_colors <- function() {
  colors <- c(
    # Gender category
    "#CC66CC",    # Female: C=0, M=50, Y=0, K=20
    "#994D99",    # Male: C=0, M=50, Y=0, K=40
    "#B359B3",    # Base: C=0, M=50, Y=0, K=30
    # Education category
    "#80FFFF",    # None: C=50, M=0, Y=0, K=0
    "#6CD9D9",    # Secondary I: C=50, M=0, Y=0, K=15
    "#59B3B3",    # Secondary II: C=50, M=0, Y=0, K=30
    "#468C8C",    # Tertiary: C=50, M=0, Y=0, K=45
    # Migration category
    "#D9D936",    # Professional only: C=0, M=0, Y=75, K=15
    "#B3B32D",    # Family only: C=0, M=0, Y=75, K=30
    "#8C8C23",    # Family+professional: C=0, M=0, Y=75, K=45
    "#66661A",    # Other reasons: C=0, M=0, Y=75, K=60
    # Age category
    "#E67373",    # 24-40 light: C=0, M=50, Y=50, K=10
    "#CC6666",    # Base: C=0, M=50, Y=50, K=20
    "#B35959",    # 24-40 dark: C=0, M=50, Y=50, K=30
    # Arrival category
    "#9999FF",    # 2020-2024: C=40, M=40, Y=0, K=0
    "#7A7ACC",    # 2015-2019: C=40, M=40, Y=0, K=20
    "#6363A6",    # 2010-2014: C=40, M=40, Y=0, K=35
    "#4D4D80",    # 2006-2009: C=40, M=40, Y=0, K=50
    "#7ACCCC",    # Base: C=40, M=0, Y=0, K=20
    # Countries category
    "#59B359",    # Other Europe: C=50, M=0, Y=50, K=30
    "#7DB37D",    # Italy: C=30, M=0, Y=30, K=30
    "#66CC66",    # Other OECD: C=50, M=0, Y=50, K=20
    "#4D994D",    # Other EU/EFTA: C=50, M=0, Y=50, K=40
    "#6B996B",    # Austria: C=30, M=0, Y=30, K=40
    "#80FF80",    # Africa: C=50, M=0, Y=50, K=0
    "#408040",    # Germany: C=50, M=0, Y=50, K=50
    "#598059",    # Portugal: C=30, M=0, Y=30, K=50
    "#8AE68A",    # Latin America: C=40, M=0, Y=40, K=10
    "#336633",    # France: C=50, M=0, Y=50, K=60
    "#476647",    # Spain: C=30, M=0, Y=30, K=60
    "#A1E6A1"     # Asia: C=30, M=0, Y=30, K=10
  )
  return(colors)
}

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
  colors <- c(
    "#F4F7F9",  # Very light
    "#E8F0F4",  # 
    "#DCE8EF",  # 
    "#D0E1EA",  # 
    "#C4DAE5",  # 
    "#B8D2E0",  # 
    "#A0BDD5",  # 
    "#88A8CA",  # 
    "#7099B9",  # 
    "#5494A8",  # Base measured color
    "#4A7D8F",  # 
    "#406676"   # Very dark
  )
  return(colors)
}

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
  par(mfrow = c(5, 1), mar = c(1, 1, 2, 1))
  
  # Categorical (12 colors with measured base colors)
  barplot(rep(1, 12), col = nccr_categorical(), 
          main = "NCCR Categorical (measured base colors)", axes = FALSE)
  
  # Sequential Education-based
  barplot(rep(1, 12), col = nccr_sequential_cyan(), 
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
print_nccr_colors <- function() {
  cat("NCCR Color Palettes (Based on Apple Digital Color Meter measurements)\n")
  cat("====================================================================\n\n")
  
  cat("Measured Base Colors:\n")
  cat("Gender:    #A86B85\n")
  cat("Education: #5494A8\n") 
  cat("Migration: #B8AF4C\n")
  cat("Age:       #B37964\n")
  cat("Arrival:   #7B7897\n")
  cat("Countries: #749E7C\n\n")
  
  cat("Categorical (12 colors):\n")
  cat(paste(nccr_categorical(), collapse = ", "), "\n\n")
  
  cat("Sequential Education-based:\n")
  cat(paste(nccr_sequential_cyan(), collapse = ", "), "\n\n")
  
  cat("Sequential Migration-based:\n")
  cat(paste(nccr_sequential_yellow(), collapse = ", "), "\n\n")
  
  cat("Divergent Gender-Education:\n")
  cat(paste(nccr_divergent_magenta_cyan(), collapse = ", "), "\n\n")
  
  cat("Divergent Age-Countries:\n")
  cat(paste(nccr_divergent_orange_purple(), collapse = ", "), "\n\n")
}

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


