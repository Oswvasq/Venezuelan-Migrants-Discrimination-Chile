#*******************************************************************************
# PROJECT:  Migración y divergencias en el mercado de trabajo: Analisis de las
#           condiciones laborales de los venezolanos en Chile frente a la    
#           población local, 2015-2022
#
# SCRIPT:   Visualize Model 
#
# AUTHOR:   Oswel Yosue Martinez Vasquez
#          
#
# ADVISOR:  Jesús Marcano
#
# DATE:     See file properties
#
#*******************************************************************************

#*******************************************************************************
# 1. Load Data
#*******************************************************************************

load(
  file = paste0(
    out_folder_data,
    "/Condiciones Laborales Migrantes Venezolanos Chile.RData"
  )
)


#*******************************************************************************
# 2. 
#*******************************************************************************


################################## Employment ##################################


##### Model I - empleo_Ocupado ~ nacionalidad_origen

emp_model %>%
  filter(term == "nacionalidad_origenVenezolana") %>%
  ggplot(aes(estimate, model)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Estimation of the effect of the variable on employment ",
    y = NULL,
    title = "Coefficient plot with error bars"
  )

##### Model II -  empleo_Ocupado ~ nacionalidad_origen + sexo

emp_model %>% 
  filter(model == "model_2") %>%
  ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Estimation of the effect of the variable on employment ",
    y = NULL,
    title = "Coefficient plot with error bars"
  )

##### Model III -  empleo_Ocupado ~ nacionalidad_origen + grupo_etario

emp_model %>% 
  filter(model == "model_3") %>%
  ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Estimation of the effect of the variable on employment ",
    y = NULL,
    title = "Coefficient plot with error bars"
  )

##### Model IV -  empleo_Ocupado ~ nacionalidad_origen + grado_de_instruccion

emp_model %>% 
  filter(model == "model_4") %>%
  ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Estimation of the effect of the variable on employment ",
    y = NULL,
    title = "Coefficient plot with error bars"
  )

##### Model V -  empleo_Ocupado ~ nacionalidad_origen + sexo 
#####  + grupo_etario + grado_de_instruccion 

emp_model %>% 
  filter(model == "model_5") %>%
  ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Estimation of the effect of the variable on employment ",
    y = NULL,
    title = "Coefficient plot with error bars"
  )


#################################### Income ####################################




##### Model I - log(ingreso) ~ nacionalidad_origen

inc_model %>% 
  filter(term == "nacionalidad_origenVenezolana") %>% 
  ggplot(aes(estimate, model)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Estimation of the effect of the variable on income ",
    y = NULL,
    title = "Coefficient plot with error bars"
  )

##### Model II -  log(ingreso) ~ nacionalidad_origen + sexo

inc_model %>% 
  filter(model == "model_2") %>%
  ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Estimation of the effect of the variable on income ",
    y = NULL,
    title = "Coefficient plot with error bars"
  )

##### Model III -  log(ingreso) ~ nacionalidad_origen + grupo_etario

inc_model %>% 
  filter(model == "model_3") %>%
  ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Estimation of the effect of the variable on income ",
    y = NULL,
    title = "Coefficient plot with error bars"
  )

##### Model IV -  log(ingreso) ~ nacionalidad_origen + grado_de_instruccion

inc_model %>% 
  filter(model == "model_4") %>%
  ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Estimation of the effect of the variable on income ",
    y = NULL,
    title = "Coefficient plot with error bars"
  )

##### Model V -  log(ingreso) ~ nacionalidad_origen + sexo 
#####  + grupo_etario + grado_de_instruccion 

inc_model %>% 
  filter(model == "model_5") %>%
  ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Estimation of the effect of the variable on income ",
    y = NULL,
    title = "Coefficient plot with error bars"
  )


######################## Effective weekly working hours ########################


##### Model I - log(horas_efectivas) ~ nacionalidad_origen

week_hour_effective_model %>% 
  filter(term == "nacionalidad_origenVenezolana") %>%
  ggplot(aes(estimate, model)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Estimation of the effect of the variable on week hour effective ",
    y = NULL,
    title = "Coefficient plot with error bars"
  )

##### Model II -  log(horas_efectivas) ~ nacionalidad_origen + sexo

week_hour_effective_model %>% 
  filter(model == "model_2") %>%
  ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Estimation of the effect of the variable on week hour effective ",
    y = NULL,
    title = "Coefficient plot with error bars"
  )

##### Model III -  log(horas_efectivas) ~ nacionalidad_origen + grupo_etario

week_hour_effective_model %>% 
  filter(model == "model_3") %>%
  ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Estimation of the effect of the variable on week hour effective ",
    y = NULL,
    title = "Coefficient plot with error bars"
  )

##### Model IV -  log(horas_efectivas) ~ nacionalidad_origen + grado_de_instruccion

week_hour_effective_model %>% 
  filter(model == "model_4") %>%
  ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Estimation of the effect of the variable on week hour effective ",
    y = NULL,
    title = "Coefficient plot with error bars"
  )

##### Model V -  log(horas_efectivas) ~ nacionalidad_origen + sexo 
#####  + grupo_etario + grado_de_instruccion 

week_hour_effective_model %>% 
  filter(model == "model_5") %>%
  ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Estimation of the effect of the variable on week hour effective ",
    y = NULL,
    title = "Coefficient plot with error bars"
  )


########################## Agreed weekly working hours #########################


##### Model I - log(horas_pactadas) ~ nacionalidad_origen

week_hour_agreed_model %>% 
  filter(model == "model_1") %>%
  ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Estimation of the effect of the variable on week hour agreed ",
    y = NULL,
    title = "Coefficient plot with error bars"
  )

##### Model II -  log(horas_pactadas) ~ nacionalidad_origen + sexo

week_hour_agreed_model %>% 
  filter(term == "nacionalidad_origenVenezolana") %>% 
  ggplot(aes(estimate, model)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Estimation of the effect of the variable on week hour agreed ",
    y = NULL,
    title = "Coefficient plot with error bars"
  )

##### Model III -  log(horas_pactadas) ~ nacionalidad_origen + grupo_etario

week_hour_agreed_model %>% 
  filter(model == "model_3") %>%
  ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Estimation of the effect of the variable on week hour agreed ",
    y = NULL,
    title = "Coefficient plot with error bars"
  )

##### Model IV -  log(horas_pactadas) ~ nacionalidad_origen + grado_de_instruccion

week_hour_agreed_model %>% 
  filter(model == "model_4") %>%
  ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Estimation of the effect of the variable on week hour agreed ",
    y = NULL,
    title = "Coefficient plot with error bars"
  )

##### Model V -  log(horas_pactadas) ~ nacionalidad_origen + sexo 
#####  + grupo_etario + grado_de_instruccion 

week_hour_agreed_model %>% 
  filter(model == "model_5") %>%
  ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Estimation of the effect of the variable on week hour agreed ",
    y = NULL,
    title = "Coefficient plot with error bars"
  )










