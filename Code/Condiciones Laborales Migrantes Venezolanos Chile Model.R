#*******************************************************************************
# PROJECT:  Migración y divergencias en el mercado de trabajo: Analisis de las
#           condiciones laborales de los venezolanos en Chile frente a la    
#           población local, 2015-2022
#
# SCRIPT:   Model
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
# 2. Model Data
#*******************************************************************************

################################## Employment ##################################




emp_model <-
  data_casen %>%
  filter(
    edad >= 15,
    nacionalidad_origen %in% c("Chilena", "Venezolana")
  ) %>% 
  mutate(grado_de_instruccion = relevel(factor(grado_de_instruccion), ref = "Ninguno"),
         grupo_etario = relevel(factor(grupo_etario), ref = "Joven")) %>%
  nest(
    data = everything()
  ) %>%
  mutate(
    model_1 = map(
      .x = data,
      .f = ~ dummy_cols(
        .data = .x, 
        select_columns = "empleo"
      ) %>%
        lm_robust(
          formula = empleo_Ocupado ~ nacionalidad_origen,
          data = .,
          se_type = "stata",
          weights = expr,
          fixed_effects = ~ año
        ) %>% tidy(.)
    ), 
    model_2 = map(
      .x = data,
      .f = ~ dummy_cols(
        .data = .x, 
        select_columns = "empleo"
      ) %>%
        lm_robust(
          formula = empleo_Ocupado ~ nacionalidad_origen + sexo,
          data = .,
          se_type = "stata",
          weights = expr,
          fixed_effects = ~ año
        ) %>% tidy(.)
    ),
    model_3 = map(
      .x = data,
      .f = ~ dummy_cols(
        .data = .x, 
        select_columns = "empleo"
      ) %>%
        lm_robust(
          formula = empleo_Ocupado ~ nacionalidad_origen + grupo_etario,
          data = .,
          se_type = "stata",
          weights = expr,
          fixed_effects = ~ año
        ) %>% tidy(.)
    ),
    model_4 = map(
      .x = data,
      .f = ~ dummy_cols(
        .data = .x, 
        select_columns = "empleo"
      ) %>%
        lm_robust(
          formula = empleo_Ocupado ~ nacionalidad_origen + grado_de_instruccion,
          data = .,
          se_type = "stata",
          weights = expr,
          fixed_effects = ~ año
        ) %>% tidy(.)
    ),
    model_5 = map(
      .x = data,
      .f = ~ dummy_cols(
        .data = .x, 
        select_columns = "empleo"
      ) %>%
        lm_robust(
          formula = empleo_Ocupado ~ nacionalidad_origen + sexo + grupo_etario + grado_de_instruccion,
          data = .,
          se_type = "stata",
          weights = expr,
          fixed_effects = ~ año
        ) %>% tidy(.)
    )
  ) %>%
  select(
    -data
  ) %>%
  pivot_longer(
    cols = model_1:model_5,
    names_to = "model",
    values_to = "value"
  ) %>% 
  unnest(value)


emp_model_year <-
  data_casen %>%
  filter(
    edad >= 15, 
    nacionalidad_origen %in% c("Chilena", "Venezolana")
  ) %>%
  mutate(grado_de_instruccion = relevel(factor(grado_de_instruccion), ref = "Ninguno"),
         grupo_etario = relevel(factor(grupo_etario), ref = "Joven")) %>%
  group_by(
    año
  ) %>% 
  nest() %>%
  mutate(
    model_1 = map(
      .x = data,
      .f = ~ dummy_cols(
        .data = .x, 
        select_columns = "empleo"
      ) %>%
        lm_robust(
          formula = empleo_Ocupado ~ nacionalidad_origen,
          data = .,
          se_type = "stata",
          weights = expr
        ) %>% tidy(.)
    ), 
    model_2 = map(
      .x = data,
      .f = ~ dummy_cols(
        .data = .x, 
        select_columns = "empleo"
      ) %>%
        lm_robust(
          formula = empleo_Ocupado ~ nacionalidad_origen + sexo,
          data = .,
          se_type = "stata",
          weights = expr
        ) %>% tidy(.)
    ),
    model_3 = map(
      .x = data,
      .f = ~ dummy_cols(
        .data = .x, 
        select_columns = "empleo"
      ) %>%
        lm_robust(
          formula = empleo_Ocupado ~ nacionalidad_origen + grupo_etario,
          data = .,
          se_type = "stata",
          weights = expr
        ) %>% tidy(.)
    ),
    model_4 = map(
      .x = data,
      .f = ~ dummy_cols(
        .data = .x, 
        select_columns = "empleo"
      ) %>%
        lm_robust(
          formula = empleo_Ocupado ~ nacionalidad_origen + grado_de_instruccion,
          data = .,
          se_type = "stata",
          weights = expr
        ) %>% tidy(.)
    ),
    model_5 = map(
      .x = data,
      .f = ~ dummy_cols(
        .data = .x, 
        select_columns = "empleo"
      ) %>%
        lm_robust(
          formula = empleo_Ocupado ~ nacionalidad_origen + sexo + grupo_etario + grado_de_instruccion, 
          data = .,
          se_type = "stata",
          weights = expr
        ) %>% tidy(.)
    )
  ) %>%
  select(
    -data
  ) %>%
  pivot_longer(
    cols = model_1:model_5,
    names_to = "model",
    values_to = "value"
  ) %>% 
  unnest(value)




#################################### Income ####################################

inc_model <- 
  data_casen %>%
  filter(
    edad >= 15, 
    nacionalidad_origen %in% c("Chilena", "Venezolana"), 
    empleo == "Ocupado", 
    ingreso > 0 
  ) %>%
  mutate(grado_de_instruccion = relevel(factor(grado_de_instruccion), ref = "Ninguno"),
         grupo_etario = relevel(factor(grupo_etario), ref = "Joven"),
         ocupacion = relevel(factor(ocupacion), ref = "Empleado u obrero del sector privado")) %>%
  nest(
    data = everything()
  ) %>% 
  mutate(
    model_1 = map(
      .x= data, 
      .f =  ~ lm_robust(
        formula = log(ingreso)  ~ nacionalidad_origen, 
        data = ., 
        se_type = "stata", 
        weights = expr, 
        fixed_effects = año
      ) %>% tidy(.) 
    ), 
    model_2 = map(
      .x= data, 
      .f =  ~ lm_robust(
        formula = log(ingreso)  ~ nacionalidad_origen + sexo, 
        data = ., 
        se_type = "stata", 
        weights = expr, 
        fixed_effects = año
      ) %>% tidy(.) 
    ),
    model_3 = map(
      .x= data, 
      .f =  ~ lm_robust(
        formula = log(ingreso)  ~ nacionalidad_origen + grupo_etario, 
        data = ., 
        se_type = "stata", 
        weights = expr, 
        fixed_effects = año
      ) %>% tidy(.) 
    ),
    model_4 = map(
      .x= data, 
      .f =  ~ lm_robust(
        formula = log(ingreso)  ~ nacionalidad_origen + grado_de_instruccion, 
        data = ., 
        se_type = "stata", 
        weights = expr, 
        fixed_effects = año
      ) %>% tidy(.) 
    ),
    model_5 = map(
      .x= data, 
      .f =  ~ lm_robust(
        formula = log(ingreso)  ~ nacionalidad_origen + ocupacion, 
        data = ., 
        se_type = "stata", 
        weights = expr, 
        fixed_effects = año
      ) %>% tidy(.) 
    ),
    model_6 = map(
      .x= data, 
      .f =  ~ lm_robust(
        formula = log(ingreso)  ~ nacionalidad_origen + sexo + grupo_etario + grado_de_instruccion + ocupacion + 1, 
        data = ., 
        se_type = "stata", 
        weights = expr, 
        fixed_effects = año
      ) %>% tidy(.) 
    ) 
  ) %>% 
  select(
    -data
  ) %>% 
  pivot_longer(
    cols = model_1:model_6, 
    names_to = "model", 
    values_to = "value"
  ) %>%
  unnest(value)



inc_model_year <- 
  data_casen %>%
  filter(
    edad >= 15, 
    nacionalidad_origen %in% c("Chilena", "Venezolana"), 
    empleo == "Ocupado",
    ingreso > 0 
  ) %>%
  mutate(grado_de_instruccion = relevel(factor(grado_de_instruccion), ref = "Ninguno"),
         grupo_etario = relevel(factor(grupo_etario), ref = "Joven"),
         ocupacion = relevel(factor(ocupacion), ref = "Empleado u obrero del sector privado")) %>%
  group_by(
    año
  ) %>%
  nest() %>% 
  mutate(
    model_1 = map(
      .x= data, 
      .f =  ~ lm_robust(
        formula = log(ingreso)  ~ nacionalidad_origen, 
        data = ., 
        se_type = "stata", 
        weights = expr
      ) %>% tidy(.) 
    ), 
    model_2 = map(
      .x= data, 
      .f =  ~ lm_robust(
        formula = log(ingreso)  ~ nacionalidad_origen + sexo, 
        data = ., 
        se_type = "stata", 
        weights = expr
      ) %>% tidy(.) 
    ),
    model_3 = map(
      .x= data, 
      .f =  ~ lm_robust(
        formula = log(ingreso)  ~ nacionalidad_origen + grupo_etario, 
        data = ., 
        se_type = "stata", 
        weights = expr
      ) %>% tidy(.) 
    ),
    model_4 = map(
      .x= data, 
      .f =  ~ lm_robust(
        formula = log(ingreso)  ~ nacionalidad_origen + grado_de_instruccion, 
        data = ., 
        se_type = "stata", 
        weights = expr
      ) %>% tidy(.) 
    ),
    model_5 = map(
      .x= data, 
      .f =  ~ lm_robust(
        formula = log(ingreso)  ~ nacionalidad_origen + ocupacion, 
        data = ., 
        se_type = "stata", 
        weights = expr
      ) %>% tidy(.) 
    ),
    model_6 = map(
      .x= data, 
      .f =  ~ lm_robust(
        formula = log(ingreso)  ~ nacionalidad_origen + sexo + grupo_etario + grado_de_instruccion + ocupacion, 
        data = ., 
        se_type = "stata", 
        weights = expr
      ) %>% tidy(.) 
    ) 
  ) %>% 
  select(
    -data
  ) %>% 
  pivot_longer(
    cols = model_1:model_6, 
    names_to = "model", 
    values_to = "value"
  ) %>%
  unnest(value)




######################## Effective weekly working hours ########################


week_hour_effective_model <- 
  data_casen %>% 
  filter( 
    edad >= 15,
    nacionalidad_origen %in% c("Chilena", "Venezolana"), 
    empleo == "Ocupado",
    año != 2020, # La pregunta de horas trabajadas no fue planteada para 2020
    horas_efectivas > 0 
  ) %>% 
  mutate(grado_de_instruccion = relevel(factor(grado_de_instruccion), ref = "Ninguno"),
         grupo_etario = relevel(factor(grupo_etario), ref = "Joven"),
         ocupacion = relevel(factor(ocupacion), ref = "Empleado u obrero del sector privado")) %>%
  nest(
    data = everything()
  ) %>% 
  mutate( 
    model_1 = map( 
      .x = data, 
      .f =  ~  lm_robust(
        formula = log(horas_efectivas)  ~ nacionalidad_origen, 
        data = ., 
        se_type = "stata", 
        weights = expr, 
        fixed_effects = año
      ) %>% tidy(.)
    ),
    model_2 = map( 
      .x = data, 
      .f =  ~  lm_robust(
        formula = log(horas_efectivas)  ~ nacionalidad_origen + sexo, 
        data = ., 
        se_type = "stata", 
        weights = expr, 
        fixed_effects = año
      ) %>% tidy(.)
    ),
    model_3 = map( 
      .x = data, 
      .f =  ~  lm_robust(
        formula = log(horas_efectivas)  ~ nacionalidad_origen + grupo_etario, 
        data = ., 
        se_type = "stata", 
        weights = expr, 
        fixed_effects = año
      ) %>% tidy(.)
    ),
    model_4 = map( 
      .x = data, 
      .f =  ~  lm_robust(
        formula = log(horas_efectivas)  ~ nacionalidad_origen + grado_de_instruccion, 
        data = ., 
        se_type = "stata", 
        weights = expr, 
        fixed_effects = año
      ) %>% tidy(.)
    ), 
    model_5 = map( 
      .x = data, 
      .f =  ~  lm_robust(
        formula = log(horas_efectivas)  ~ nacionalidad_origen + ocupacion, 
        data = ., 
        se_type = "stata", 
        weights = expr, 
        fixed_effects = año
      ) %>% tidy(.)
    ),
    model_6 = map( 
      .x = data, 
      .f =  ~  lm_robust(
        formula = log(horas_efectivas)  ~ nacionalidad_origen + sexo + grupo_etario + grado_de_instruccion + ocupacion, 
        data = ., 
        se_type = "stata", 
        weights = expr, 
        fixed_effects = año
      ) %>% tidy(.)
    ),
  )  %>%
  select(
    -data
  ) %>%
  pivot_longer(
    cols = model_1:model_6,
    names_to = "model",
    values_to = "value"
  ) %>%
  unnest(value)



week_hour_effective_model_year <- 
  data_casen %>% 
  filter( 
    edad >= 15,
    nacionalidad_origen %in% c("Chilena", "Venezolana"), 
    empleo == "Ocupado",
    año != 2020, # La pregunta de horas trabajadas no fue planteada para 2020
    horas_efectivas > 0
  ) %>% 
  mutate(grado_de_instruccion = relevel(factor(grado_de_instruccion), ref = "Ninguno"),
         grupo_etario = relevel(factor(grupo_etario), ref = "Joven"),
         ocupacion = relevel(factor(ocupacion), ref = "Empleado u obrero del sector privado")) %>%
  group_by(
    año
  ) %>%
  nest() %>% 
  mutate( 
    model_1 = map( 
      .x = data, 
      .f =  ~  lm_robust(
        formula = log(horas_efectivas)  ~ nacionalidad_origen, 
        data = ., 
        se_type = "stata", 
        weights = expr
      ) %>% tidy(.)
    ),
    model_2 = map( 
      .x = data, 
      .f =  ~  lm_robust(
        formula = log(horas_efectivas)  ~ nacionalidad_origen + sexo, 
        data = ., 
        se_type = "stata", 
        weights = expr
      ) %>% tidy(.)
    ),
    model_3 = map( 
      .x = data, 
      .f =  ~  lm_robust(
        formula = log(horas_efectivas)  ~ nacionalidad_origen + grupo_etario, 
        data = ., 
        se_type = "stata", 
        weights = expr
      ) %>% tidy(.)
    ),
    model_4 = map( 
      .x = data, 
      .f =  ~  lm_robust(
        formula = log(horas_efectivas)  ~ nacionalidad_origen + grado_de_instruccion, 
        data = ., 
        se_type = "stata", 
        weights = expr
      ) %>% tidy(.)
    ),
    model_5 = map( 
      .x = data, 
      .f =  ~  lm_robust(
        formula = log(horas_efectivas)  ~ nacionalidad_origen + ocupacion, 
        data = ., 
        se_type = "stata", 
        weights = expr
      ) %>% tidy(.)
    ),
    model_6 = map( 
      .x = data, 
      .f =  ~  lm_robust(
        formula = log(horas_efectivas)  ~ nacionalidad_origen + sexo + grupo_etario + grado_de_instruccion + ocupacion, 
        data = ., 
        se_type = "stata", 
        weights = expr
      ) %>% tidy(.)
    ),
  ) %>%
  select(
    -data
  ) %>%
  pivot_longer(
    cols = model_1:model_6,
    names_to = "model",
    values_to = "value"
  ) %>%
  unnest(value)


########################## Agreed weekly working hours #########################


week_hour_agreed_model <- 
  data_casen %>% 
  filter( 
    edad >= 15,
    nacionalidad_origen %in% c("Chilena", "Venezolana"), 
    empleo == "Ocupado",
    horas_pactadas > 0 
  ) %>%
  mutate(grado_de_instruccion = relevel(factor(grado_de_instruccion), ref = "Ninguno"),
         grupo_etario = relevel(factor(grupo_etario), ref = "Joven"),
         ocupacion = relevel(factor(ocupacion), ref = "Empleado u obrero del sector privado")) %>%
  nest(
    data = everything()
  ) %>% 
  mutate( 
    model_1 = map( 
      .x = data, 
      .f =  ~  lm_robust(
        formula = log(horas_pactadas)  ~ nacionalidad_origen, 
        data = ., 
        se_type = "stata", 
        weights = expr, 
        fixed_effects = año
      ) %>% tidy(.)
    ),
    model_2 = map( 
      .x = data, 
      .f =  ~  lm_robust(
        formula = log(horas_pactadas)  ~ nacionalidad_origen + sexo, 
        data = ., 
        se_type = "stata", 
        weights = expr, 
        fixed_effects = año
      ) %>% tidy(.)
    ),
    model_3 = map( 
      .x = data, 
      .f =  ~  lm_robust(
        formula = log(horas_pactadas)  ~ nacionalidad_origen + grupo_etario, 
        data = ., 
        se_type = "stata", 
        weights = expr, 
        fixed_effects = año
      ) %>% tidy(.)
    ),
    model_4 = map( 
      .x = data, 
      .f =  ~  lm_robust(
        formula = log(horas_pactadas)  ~ nacionalidad_origen + grado_de_instruccion, 
        data = ., 
        se_type = "stata", 
        weights = expr, 
        fixed_effects = año
      ) %>% tidy(.)
    ),
    model_5 = map( 
      .x = data, 
      .f =  ~  lm_robust(
        formula = log(horas_pactadas)  ~ nacionalidad_origen + ocupacion, 
        data = ., 
        se_type = "stata", 
        weights = expr, 
        fixed_effects = año
      ) %>% tidy(.)
    ),
    model_6 = map( 
      .x = data, 
      .f =  ~  lm_robust(
        formula = log(horas_pactadas)  ~ nacionalidad_origen + sexo + grupo_etario + grado_de_instruccion + ocupacion, 
        data = ., 
        se_type = "stata", 
        weights = expr, 
        fixed_effects = año
      ) %>% tidy(.)
    ),
  )  %>%
  select(
    -data
  ) %>%
  pivot_longer(
    cols = model_1:model_6,
    names_to = "model",
    values_to = "value"
  ) %>%
  unnest(value)



week_hour_agreed_model_year <- 
  data_casen %>% 
  filter( 
    edad >= 15,
    nacionalidad_origen %in% c("Chilena", "Venezolana"), 
    empleo == "Ocupado",
    horas_pactadas > 0
  ) %>% 
  mutate(grado_de_instruccion = relevel(factor(grado_de_instruccion), ref = "Ninguno"),
         grupo_etario = relevel(factor(grupo_etario), ref = "Joven"),
         ocupacion = relevel(factor(ocupacion), ref = "Empleado u obrero del sector privado")) %>%
  group_by(
    año
  ) %>%
  nest() %>% 
  mutate( 
    model_1 = map( 
      .x = data, 
      .f =  ~  lm_robust(
        formula = log(horas_pactadas)  ~ nacionalidad_origen, 
        data = ., 
        se_type = "stata", 
        weights = expr
      ) %>% tidy(.)
    ),
    model_2 = map( 
      .x = data, 
      .f =  ~  lm_robust(
        formula = log(horas_pactadas)  ~ nacionalidad_origen + sexo, 
        data = ., 
        se_type = "stata", 
        weights = expr
      ) %>% tidy(.)
    ),
    model_3 = map( 
      .x = data, 
      .f =  ~  lm_robust(
        formula = log(horas_pactadas)  ~ nacionalidad_origen + grupo_etario, 
        data = ., 
        se_type = "stata", 
        weights = expr
      ) %>% tidy(.)
    ),
    model_4 = map( 
      .x = data, 
      .f =  ~  lm_robust(
        formula = log(horas_pactadas)  ~ nacionalidad_origen + grado_de_instruccion, 
        data = ., 
        se_type = "stata", 
        weights = expr
      ) %>% tidy(.)
    ), 
    model_5 = map( 
      .x = data, 
      .f =  ~  lm_robust(
        formula = log(horas_pactadas)  ~ nacionalidad_origen + ocupacion, 
        data = ., 
        se_type = "stata", 
        weights = expr
      ) %>% tidy(.)
    ),
    model_6 = map( 
      .x = data, 
      .f =  ~  lm_robust(
        formula = log(horas_pactadas)  ~ nacionalidad_origen + sexo + grupo_etario + grado_de_instruccion + ocupacion, 
        data = ., 
        se_type = "stata", 
        weights = expr
      ) %>% tidy(.)
    ),
  ) %>%
  select(
    -data
  ) %>%
  pivot_longer(
    cols = model_1:model_6,
    names_to = "model",
    values_to = "value"
  ) %>%
  unnest(value)




#*******************************************************************************
# 3. Save Data 1
#*******************************************************************************


write.xlsx(list(
  emp_model = emp_model, 
  emp_model_year = emp_model_year, 
  inc_model = inc_model, 
  inc_model_year = inc_model_year, 
  week_hour_effective_model = week_hour_effective_model, 
  week_hour_effective_model_year = week_hour_effective_model_year, 
  week_hour_agreed_model = week_hour_agreed_model, 
  week_hour_agreed_model_year = week_hour_agreed_model_year
), 
paste0(
  out_folder_tables,
  "/modelos.xlsx"
)
)



#*******************************************************************************
# 4. Save Data 2 
#*******************************************************************************

save.image(
  file = paste0(
    out_folder_data,
    "/Condiciones Laborales Migrantes Venezolanos Chile.RData"
  )
)














