# setup-prepare-data.R

library(tidyverse)
library(readxl)
library(janitor)
library(gt)
library(gtExtras)
library(glue)
library(scales)
library(ggrepel)
library(patchwork)

theme_set(theme_light() +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.title = element_text(size = rel(2.0)),
                  plot.title.position = "plot")
)
my_caption <- "Data: Gavin M. Mudd; Analysis: Daniel Moul"
carolina_blue <- "#4B9CD3" #"#62C6F2"
n_groups <- 9 # number of facets in some plots

###### prepare summary yearly data ######

fname_prepared_data <- "./data/processed/summary_yearly.rds"

if(!file.exists(fname_prepared_data)) {
  
  fname <- "Aust-Mine-Prod-v01.4b-Master-dmoul.xlsx"
  fpath <- paste0("./data/src/22724081/", fname)
  fsheet <- "Annual Data"
  frange <- "A8:NK10"
  
  headings_raw <- readxl::read_xlsx(path = fpath,
                                    sheet = fsheet,
                                    range = frange,
                                    col_names = FALSE)
  
  my_colnames = character(ncol(headings_raw))
  for(i in 1: ncol(headings_raw)) {
    my_colnames[i] <- paste0(headings_raw[1, i], "_", headings_raw[2, i], "_", clean_names(headings_raw[3, i]))
    if(my_colnames[i] == "NA_NA_NA") {
      my_colnames[i] <- paste0(my_colnames[i], "_", i)
    }
  }
  
  frange <- "A11:NK233"
  
  dta_temp <- readxl::read_xlsx(path = fpath,
                                sheet = fsheet,
                                range = frange,
                                col_names = FALSE)
  
  names(dta_temp) <- my_colnames
  
  dta_yearly_all_columns <- dta_temp |>
    rename(year = 1) |>
    remove_empty(which = "cols")
  
  write_rds(dta_yearly_all_columns, fname_prepared_data,
            compress = "none")
  
} else {
  dta_yearly_all_columns <- read_rds(fname_prepared_data)
}

###### prepare inflation adjustment based on CPI ######

fname_prepared_data <- "./data/processed/cpi_australia.rds"

if(!file.exists(fname_prepared_data)) {
  
  fname <- "g01hist.xls"
  fpath <- paste0("./data/src/", fname)
  fsheet <- "Data"
  frange <- "A12:C415"
  
  cpi_raw <- readxl::read_xls(path = fpath,
                                    sheet = fsheet,
                                    range = frange,
                                    col_names = FALSE)
  
  names(cpi_raw) <- c("year", "cpi", "cpi_ye_change_pct")
  
  cpi <- cpi_raw |>
    filter(month(year) == 12) |>
    mutate(year = year(year),
           factor_2021  = cpi[year == 2021] / cpi)
  
  write_rds(cpi, fname_prepared_data)
  
  rm(cpi_raw)
  
} else {
  cpi <- read_rds(fname_prepared_data)
}


###### data prep ######

dta_yearly_volume_value <- dta_yearly_all_columns |>
  #select(year, contains("Australia") & !contains(c(r"($)", r"(%)")) & !contains(c("Placer", "PGE", "Platinum Group", "Rare Earths"))) |>
  select(year, contains("Australia") & !contains(r"(%)") & !contains(c("Placer", "PGE", "Platinum Group", "Rare Earths", "Bismuth",
                                                                       "Gallium", "Vanadium"))) |>
  mutate(across(everything(), as.double)) # convert errant strings to NA (creates warning message: NAs introduced by coercion)
# TODO figure out why some column names aren't following the main pattern; for now those ores will be removed in the next step)
# TODO add back in contains(c("Placer", "PGE", "Platinum Group", "Rare Earths")

xx_colnames <- tibble(xx = colnames(dta_yearly_volume_value))

my_colnames <- str_replace_all(colnames(dta_yearly_volume_value), 
                               c("Black Coal" = "Black-Coal", 
                                 "raw coal" = "raw-coal",
                                 "Iron Ore" = "Iron-ore",
                                 "(?i)phosphate (?i)rock" = "Phosphate-rock",
                                 #"Platinum Group Elements by Individual Element" = "Platinum_Group_Elements",
                                 #"Rare Earths" = "Rare_Earths",
                                 "_Australia" = "", 
                                 " .*" = "") 
)
# TODO figure out what some of the units are
#. for example "Managanese t Mn conc"

names(dta_yearly_volume_value) <- my_colnames

dta_yearly_volume_value_long <- dta_yearly_volume_value |>
  pivot_longer(cols = contains("_"),
               names_to = "product",
               values_to = "amount") |>
  filter(!product %in% c("Niobium_t", "Bismuth_t")) |> # too little mined to report it
  mutate(#units = str_extract(product, "([[:alpha:]][-]?)+$"),
    my_units = str_extract(product, "(?<=_).+$"),
    type = if_else(str_detect(my_units, "[$]"), "price", "volume"),
    product_name = str_extract(product, "^([[:alpha:]][-]?)+"),
  ) |>
  mutate(amount_max = max(amount, na.rm = TRUE),
         .by = c(product, my_units)) |>
  mutate(group = cut_number(amount_max, n_groups,
                            #include.lowest = TRUE,
                            ordered_result = TRUE,
                            labels = FALSE,
                            na.rm = TRUE),
         .by = type
  )

# summary_dta_yearly_volume_long <- summary_dta_yearly_volume_value_long |>
#   filter(type == "volume") |>
#   rename(volume = amount,
#          volume_max = amount_max,
#          group_volume = group) #|>
#   # mutate(group_volume = cut_number(volume_max, n_groups,
#   #                           #include.lowest = TRUE,
#   #                           ordered_result = TRUE,
#   #                           labels = FALSE),
#   #        .by = type
#   # )

dta_yearly_long <- dta_yearly_volume_value_long |>
  #mutate(product_name = str_extract(product, str_extract(product, "^([[:alpha:]][-]?)+"))) |>
  group_by(year, product_name) |>
  mutate(product_volume = lag(product, n = 1, default = NA),
         volume = lag(amount, n = 1, default = NA),
         units_volume = lag(my_units, n = 1, default = NA),
         group_volume = lag(group, n = 1, default = NA)
         ) |>
  ungroup() |>
  filter(type == "price") |>
  rename(price = amount,
         price_max = amount_max,
         units_price = my_units,
         group_price = group,
         product_price = product) |>
  mutate(value = price * volume) |>
  mutate(volume_max = max(volume, na.rm = TRUE),
         volume_pct_of_max = volume / volume_max,
         #price_max = max(price, na.rm = TRUE),
         value_max = max(value, na.rm = TRUE),
         .by = c(product_name)) |>
  #summary_dta_yearly_value_2021_dollars_long <- summary_dta_yearly_value_long |>
  #rename(price = amount, price_max = amount_max) |>
  inner_join(cpi |> select(year, factor_2021),
             by = "year") |>
  mutate(price_2021 = price * factor_2021) |>
  mutate(price_max_2021 = max(price_2021, na.rm = TRUE),
         .by = c(product_name, units_price)) |> #TODO: do I need units_price?
  mutate(price_2021_pct_of_max = price_2021 / price_max_2021,
         value_2021 = value * factor_2021,
         value_max_2021 = value_max * factor_2021,
         value_2021_pct_of_max = value_2021 / value_max_2021) |>
  mutate(group_value = cut_number(value_max, n_groups,
                            #include.lowest = TRUE,
                            ordered_result = TRUE,
                            labels = FALSE)) |>
  mutate(group_price_2021 = cut_number(price_max_2021, n_groups,
                                  #include.lowest = TRUE,
                                  ordered_result = TRUE,
                                  labels = FALSE)) |>
  mutate(group_value_2021 = cut_number(value_max_2021, n_groups,
                                       #include.lowest = TRUE,
                                       ordered_result = TRUE,
                                       labels = FALSE)) |>
  select(year, product_name, contains("price"), contains("volume"), contains("value"))

# summary_dta_yearly_value_2021_dollars_long <- summary_dta_yearly_value_long |>
#   rename(price = amount, price_max = amount_max) |>
#   inner_join(cpi |> select(year, factor_2021),
#              by = "year") |>
#   mutate(price_2021 = price * factor_2021) |>
#   mutate(price_max_2021 = max(price_2021, na.rm = TRUE),
#          .by = c(product, units)) |>
#   mutate(price_2021_pct_of_max = price_2021 / price_max_2021,
#          value_2021 = value * factor_2021,
#          value_max_2021 = value_max * factor_2021,
#          value_2021_pct_of_max = value_2021 / value_max_2021)

###### plotting functions ######

plot_group <- function(grp) {
  
  # test
  # grp = 1
  
  # assume dta_yearly_long exists in parent environment
  
  dta_for_plot <- dta_yearly_long |>
    filter(group_volume == grp,
           year >= min(dta_yearly_long$year, na.rm = TRUE)) |>
    mutate(group = glue("Group {grp}"))
  
  plot_year_min <- min(dta_for_plot$year) #, na.rm = TRUE
  plot_year_max <- max(dta_for_plot$year) #, na.rm = TRUE
  
  labels_for_plot <- dta_for_plot |>
    filter(!is.na(volume)) |>
    mutate(final_year = max(year), #, na.rm = FALSE
           .by = product_volume) |>
    filter(year == max(year),
           .by = product_volume)
  
  p <- dta_for_plot |>
    ggplot(aes(year, volume, color = product_volume, group = product_volume)) +
    geom_line(na.rm = TRUE, show.legend = FALSE) +
    # geom_text(data = labels_for_plot,
    #           aes(max(year) + 50, amount, label = product),
    #           hjust = 1, show.legend = FALSE, check_overlap = TRUE) +
    geom_text_repel(data = labels_for_plot,
                    aes(max(year) + 30, volume, label = product_volume),
                    hjust = 1, vjust = 0.5, show.legend = FALSE, 
                    direction = "y", force = 0.4) +
    scale_x_continuous(breaks = c(1850, 1900, 1950, 2000)) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()),
    ) +
    facet_wrap(~ group, scales = "free_y") +
    labs(
      title = glue("Australian mining output: group {grp}"),
      subtitle = glue("{plot_year_min}-{plot_year_max}"),
      x = NULL,
      y = "Amount",
      caption = my_caption
    )
  
  print(p)
  
}

# test
# plot_group(2)

plot_group_log <- function(grp) {
  
  # test
  # grp = 1
  
  # assume data_for_plot_log (not dta_yearly_long) exists in parent environment
  
  dta_for_plot <- data_for_plot_log |>
    filter(group_volume == grp,
           year >= min(data_for_plot_log$year, na.rm = TRUE)) |>
    mutate(group = glue("Group {grp}"))
  
  plot_year_min <- min(dta_for_plot$year) #, na.rm = TRUE
  plot_year_max <- max(dta_for_plot$year) #, na.rm = TRUE
  
  labels_for_plot <- dta_for_plot |>
    filter(!is.na(volume)) |>
    mutate(final_year = max(year), #, na.rm = FALSE
           .by = product_volume) |>
    filter(year == max(year),
           .by = product_volume)
  
  p <- dta_for_plot |>
    ggplot(aes(year, volume, color = product_volume, group = product_volume)) +
    geom_line(na.rm = TRUE, show.legend = FALSE) +
    geom_text_repel(data = labels_for_plot,
                    aes(max(year) + 30, volume, label = product_volume),
                    hjust = 1, vjust = 0.5, show.legend = FALSE, 
                    direction = "y", force = 0.4) +
    scale_x_continuous(breaks = c(1850, 1900, 1950, 2000)) +
    scale_y_log10(labels = label_number(scale_cut = cut_short_scale()),
    ) +
    facet_wrap(~ group, scales = "free_y") +
    labs(
      title = glue("Australian mining output: group {grp}"),
      subtitle = glue("{plot_year_min}-{plot_year_max}"),
      x = NULL,
      y = "Amount (log10 scale)",
      caption = my_caption
    )
  
  print(p)
  
}

plot_group_value <- function(grp) {
  
  # test
  # grp = 1
  
  # assume dta_yearly_long exists in parent environment
  
  dta_for_plot <- dta_yearly_long |>
    filter(group_value == grp,
           year >= min(dta_yearly_long$year, na.rm = TRUE)) |>
    mutate(group = glue("Group {grp}"))
  
  plot_year_min <- min(dta_for_plot$year) #, na.rm = TRUE
  plot_year_max <- max(dta_for_plot$year) #, na.rm = TRUE
  
  labels_for_plot <- dta_for_plot |>
    filter(!is.na(value)) |>
    mutate(final_year = max(year), #, na.rm = FALSE
           .by = product_price) |>
    filter(year == max(year),
           .by = product_name)
  
  p <- dta_for_plot |>
    ggplot(aes(year, value, color = product_name, group = product_name)) +
    geom_line(na.rm = TRUE, show.legend = FALSE) +
    # geom_text(data = labels_for_plot,
    #           aes(max(year) + 50, amount, label = product),
    #           hjust = 1, show.legend = FALSE, check_overlap = TRUE) +
    geom_text_repel(data = labels_for_plot,
                    aes(max(year) + 30, value, label = product_name),
                    hjust = 1, vjust = 0.5, show.legend = FALSE, 
                    direction = "y", force = 0.4) +
    scale_x_continuous(breaks = c(1850, 1900, 1950, 2000)) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale(),
                                             #scale_cut = cut_si("M"), # TODO: confirm this
                                             prefix = "$"),
    ) +
    facet_wrap(~ group_value, scales = "free_y") +
    labs(
      title = glue("Australian mining value extracted: group {grp}"),
      subtitle = glue("{plot_year_min}-{plot_year_max}"),
      x = NULL,
      y = "AUD (nominal)",
      caption = my_caption
    )
  
  print(p)
  
}

# test
# plot_group_value(2)


plot_group_value_2021 <- function(grp) {
  
  # test
  # grp = 1
  
  # assume dta_yearly_long exists in parent environment
  
  dta_for_plot <- dta_yearly_long |>
    filter(group_value_2021 == grp,
           year >= min(dta_yearly_long$year, na.rm = TRUE)) |>
    mutate(group = glue("Group {grp}"))
  
  plot_year_min <- min(dta_for_plot$year) #, na.rm = TRUE
  plot_year_max <- max(dta_for_plot$year) #, na.rm = TRUE
  
  labels_for_plot <- dta_for_plot |>
    filter(!is.na(value_2021)) |>
    mutate(final_year = max(year), #, na.rm = FALSE
           .by = product_price) |>
    filter(year == max(year),
           .by = product_name)
  
  p <- dta_for_plot |>
    ggplot(aes(year, value_2021, color = product_name, group = product_name)) +
    geom_line(na.rm = TRUE, show.legend = FALSE) +
    geom_text_repel(data = labels_for_plot,
                    aes(max(year) + 30, value_2021, label = product_name),
                    hjust = 1, vjust = 0.5, show.legend = FALSE, 
                    direction = "y", force = 0.4) +
    scale_x_continuous(breaks = c(1850, 1900, 1950, 2000)) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale(),
                                             prefix = "$"),
    ) +
    facet_wrap(~ group_value_2021, scales = "free_y") +
    labs(
      title = glue("Australian mining value extracted: group {grp}"),
      subtitle = glue("2021 dollars. {plot_year_min}-{plot_year_max}"),
      x = NULL,
      y = "AUD (2021 dollars)",
      caption = my_caption
    )
  
  print(p)
  
}

# test
# plot_group_value_2021(2)

