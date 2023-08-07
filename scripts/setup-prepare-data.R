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
my_caption <- "Data: Gavin Mudd; Analysis: Daniel Moul"
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

dta_yearly_mass_value <- dta_yearly_all_columns |>
  select(year, (contains("Australia") | contains("Synthetic rutile")) & 
           !contains(r"(%)") & 
           !contains(c("Placer", "PGE", "Bismuth", #"Rare Earths", "Platinum Group", 
                       "Gallium", "Vanadium", "Niobium"))) |>
  mutate(across(everything(), as.double)) # convert errant strings to NA (creates warning message: NAs introduced by coercion)
# TODO figure out why some column names aren't following the main pattern; for now those ores will be removed in the next step)
# TODO add back in contains(c("Placer", "PGE", "Platinum Group", "Rare Earths")

xx_colnames <- tibble(xx = colnames(dta_yearly_mass_value))

my_colnames <- str_replace_all(colnames(dta_yearly_mass_value), 
                               c("Black Coal" = "Black-Coal", 
                                 "raw coal" = "raw-coal",
                                 "Iron Ore" = "Iron-ore",
                                 "(?i)phosphate (?i)rock" = "Phosphate-rock",
                                 "Platinum Group Elements by Individual Element" = "PGE-element",
                                 "Rare Earths" = "Rare-earths",
                                 "Synthetic Rutile" = "Synthetic-rutile",
                                 "_Australia" = "",
                                 "_WA" = "",
                                 # assume missing mass units are kg; reorder text pieces
                                 "PGE-element_kg Pt" = "PGE-Pt_kg",
                                 "PGE-element_kg Pd" = "PGE-Pd_kg",
                                 "PGE-element_Rh" = "PGE-Rh_kg",
                                 "PGE-element_kg Ru" = "PGE-Ru_kg",
                                 "PGE-element_Os" = "PGE-Os_kg",
                                 "PGE-element_Ir" = "PGE-Ir_kg",
                                 " .*" = ""
                                 ) 
)

names(dta_yearly_mass_value) <- my_colnames

dta_yearly_mass_value_long <- dta_yearly_mass_value |>
  pivot_longer(cols = contains("_"),
               names_to = "product",
               values_to = "amount") |>
  #filter(!product %in% c("Niobium_t", "Bismuth_t")) |> # too little mined to report it
  mutate(
    my_units = str_extract(product, "(?<=_).+$"),
    type = if_else(str_detect(my_units, "[$]"), "price", "mass"),
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

dta_yearly_long <- dta_yearly_mass_value_long |>
  group_by(year, product_name) |>
  mutate(product_mass = lag(product, n = 1, default = NA),
         mass = lag(amount, n = 1, default = NA),
         units_mass = lag(my_units, n = 1, default = NA),
         group_mass = lag(group, n = 1, default = NA)
         ) |>
  ungroup() |>
  filter(type == "price") |>
  rename(price = amount,
         price_max = amount_max,
         units_price = my_units,
         group_price = group,
         product_price = product) |>
  mutate(value = price * mass) |>
  mutate(mass_max = max(mass, na.rm = TRUE),
         mass_pct_of_max = mass / mass_max,
         value_max = max(value, na.rm = TRUE),
         .by = c(product_name)) |>
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
  select(year, product_name, contains("price"), contains("mass"), contains("value"))

