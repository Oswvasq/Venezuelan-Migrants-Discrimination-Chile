#*******************************************************************************
# PROJECT:  Migración y divergencias en el mercado de trabajo: Analisis de las
#           condiciones laborales de los venezolanos en Chile frente a la    
#           población local, 2015-2022
#
# SCRIPT:   Tidy
#
# AUTHOR:   Oswel Yosue Martinez Vasquez
#          
#
# ADVISOR:  Jesús Marcano
#
# DATE:     See file properties
#
#******************************************************************************

#*******************************************************************************
# 1. Tidy Data
#*******************************************************************************

load(
  file = paste0(
    out_folder_data,
    "/Condiciones Laborales Migrantes Venezolanos Chile.RData"
  )
)


#*******************************************************************************
# 2. Tidy Data
#*******************************************************************************

data_casen <- bind_rows(
  data_casen_2015, 
  data_casen_2017, 
  data_casen_2020, 
  data_casen_2022
)


# Data con Factor de Expansión 

data_casen_expr <- data_casen %>% 
  as_survey_design(weights = expr) 


rm(
  data_casen_2015, 
  data_casen_2017, 
  data_casen_2020, 
  data_casen_2022
)

gc()

#*******************************************************************************
# 3. Save Data
#*******************************************************************************

save.image(
  file = paste0(
    out_folder_data,
    "/Condiciones Laborales Migrantes Venezolanos Chile.RData"
  )
)

