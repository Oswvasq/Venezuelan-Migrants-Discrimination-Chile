#*******************************************************************************
# PROJECT:  Migración y divergencias en el mercado de trabajo: Analisis de las
#           condiciones laborales de los venezolanos en Chile frente a la    
#           población local, 2015-2022
#
# SCRIPT:   Master
#
# AUTHOR:   Oswel Yosue Martinez Vasquez
#          
#
# ADVISOR:  Jesús Marcano
#
# DATE:     See file properties
#
#*******************************************************************************

rm(list = ls())

gc()

#*******************************************************************************
# 1. Load External Packages
#*******************************************************************************

# Officially Supplied Basic External Packages

installed_pkgs <- rownames(
  installed.packages()
)

basic_pkgs <- c(
  "pacman",
  "devtools"
)

basic_pkgs_check <- basic_pkgs %in% installed_pkgs

if (
  any(
    basic_pkgs_check == FALSE
  )
) {
  install.packages(
    pkgs = basic_pkgs[!basic_pkgs_check],
    repos = "https://cloud.r-project.org"
  )
}

# Officially Supplied External Packages

pacman::p_load(
  tidyverse,
  haven,
  readxl,
  lubridate,
  ggthemes,
  archive,
  estimatr,
  fixest,
  future,
  furrr,
  broom,
  oaxaca,
  marginaleffects,
  tidymodels,
  fastDummies,
  srvyr, 
  scales, 
  openxlsx
)

# Unofficially Supplied External Packages

# need_pack <- c(
#   "need_pack"
# )
# 
# if (
#   need_pack %in% installed_pkgs != TRUE
# ) {
#   devtools::install_github(
#     "provider/pack_site"
#   )
# }

#*******************************************************************************
# 2. Define Directories
#*******************************************************************************

# This folder structure has been designed to work with R projects.

# Standard Paths

in_folder <- "Input"

out_folder <- "Output"

code_folder <- "Code"

docs_folder <- "Docs"

out_folder_data <- 
  paste0(
    out_folder,
    "/Data"
  )

out_folder_figures <- 
  paste0(
    out_folder,
    "/Figures"
  )

out_folder_tables <- 
  paste0(
    out_folder,
    "/Tables"
  )

out_folder_templates <- 
  paste0(
    out_folder,
    "/Templates"
  )

out_folder_tempfiles <- 
  paste0(
    out_folder,
    "/Tempfiles"
  )

out_folder_reports <- 
  paste0(
    out_folder,
    "/Reports"
  )

# Non-standard Paths

in_folder_casen_2015 <- 
  paste0(
    in_folder,
    "/CASEN 2015"
  )

in_folder_casen_2017 <- 
  paste0(
    in_folder,
    "/CASEN 2017"
  )

in_folder_casen_2020 <- 
  paste0(
    in_folder,
    "/CASEN 2020"
  )

in_folder_casen_2022 <- 
  paste0(
    in_folder,
    "/CASEN 2022"
  )


#*******************************************************************************
# 3. Run Code
#*******************************************************************************

source(
  file = paste0(
    code_folder,
    "/Condiciones Laborales Migrantes Venezolanos Chile Tidy CASEN 2015.R"
  )
)

source(
  file = paste0(
    code_folder,
    "/Condiciones Laborales Migrantes Venezolanos Chile Tidy CASEN 2017.R"
  )
)

source(
  file = paste0(
    code_folder,
    "/Condiciones Laborales Migrantes Venezolanos Chile Tidy CASEN 2020.R"
  )
)

source(
  file = paste0(
    code_folder,
    "/Condiciones Laborales Migrantes Venezolanos Chile Tidy CASEN 2022.R"
  )
)



#*******************************************************************************
# 4. Save Data
#*******************************************************************************

save.image(
  file = paste0(
    out_folder_data,
    "/Condiciones Laborales Migrantes Venezolanos Chile.RData"
  )
)

