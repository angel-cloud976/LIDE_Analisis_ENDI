setwd("C:/Users/angel/OneDrive/Documentos/Escritorio/graficos-el-quantificador")
source("C:/Users/angel/OneDrive/Documentos/Escritorio/graficos-el-quantificador/scripts/utils.R")
source("C:/Users/angel/OneDrive/Documentos/Escritorio/graficos-el-quantificador/scripts/packages.R")

ensure_packages(c("dplyr", "ggplot2", "stringr", "ggtext", "ragg", "readxl", "scales", "tidyr", "magick", "cowplot", "lubridate","readODS", "survey", "broom", "haven" , "purrr"))

datos <- read_dta("C:/Users/angel/OneDrive/Documentos/Escritorio/Pre-profesional/Base de datos ENDI 2023/BDD_ENDI_R2_f1_personas.dta")
datos2 <- read_dta("C:/Users/angel/OneDrive/Documentos/Escritorio/Pre-profesional/Base de datos ENDI 2023/BDD_ENDI_R2_f1_hogar.dta")
datos3 <- read_dta("C:/Users/angel/OneDrive/Documentos/Escritorio/Pre-profesional/Base de datos ENDI 2023/BDD_ENDI_R2_f2_mef.dta")
datos4 <- read_dta("C:/Users/angel/OneDrive/Documentos/Escritorio/Pre-profesional/Base de datos ENDI 2023/BDD_ENDI_R2_f2_salud_ninez.dta")
datos5 <- read_dta("C:/Users/angel/OneDrive/Documentos/Escritorio/Pre-profesional/Base de datos ENDI 2023/BDD_ENDI_R2_f2_lactancia.dta")

###Union de bases

datos <- datos %>%
  select(
    id_upm,id_viv,id_hogar,id_per,id_mef,fecha_anio,fecha_mes,fecha_dia,
    fexp,estrato,area,region,prov,parr_pri,etnia,persona,altitud,edaddias,
    grupo_edad_nin,nivins_mef,f1_s1_2,f1_s6_3,quintil)

datos_c <- datos %>%
  left_join(
    datos2 %>%
      select(
        id_hogar,
        f_agua = f1_s3_10,
        saneamiento = f1_s3_11,
        material_pared = f1_s3_5,
        disp_alimentos = f1_s4_1_6
      ),
    by = "id_hogar"
  ) %>%
  left_join(
    datos3 %>%
      select(
        id_hogar,
        hijos_d = f2_s2_233_1,
        hijos_f = f2_s2_233_2,
        edad_madre = f2_s1_101
      ),
    by = "id_hogar"
  ) %>%
  left_join(
    datos4 %>%
      select(
        id_hogar,
        lugar_parto = f2_s4c_429_a,
        c_pren_natal = f2_s4b_404,
        consumo_he_madre = f2_s4b_410_a,
        consumo_he_hijo = f2_s4i_495
      ),
    by = "id_hogar"
  ) %>%
  left_join(
    datos5 %>%
      select(
        id_per,
        amamanto = f2_s3_302
      ),
    by = "id_per"
  )
unique(datos_c$nivins_mef)

###Construcción de las variables para analisis
datos_c <- datos_c %>%
  mutate(grupo_edad_nin = na_if(as.character(grupo_edad_nin), "."))

datos_c <- datos_c %>%
  filter(!is.na(datos_c$f1_s6_3))

datos_c <- datos_c %>%
  mutate(
    A = altitud / 1000,
    ajuste = -0.032*A + 0.022*A^2,
    hb_ajustada = f1_s6_3 - ajuste
  )

datos_c <- datos_c %>%
  mutate(
    anemia = case_when(
      grupo_edad_nin %in% c("2", "3") & hb_ajustada <= 10.5 ~ TRUE,
      
      grupo_edad_nin %in% c("4", "5", "6") & hb_ajustada <= 11.0 ~ TRUE,
      
      grupo_edad_nin %in% c("2", "3") & hb_ajustada > 10.5 ~ FALSE,
      
      grupo_edad_nin %in% c("4", "5", "6") & hb_ajustada > 11.0 ~ FALSE,
      TRUE ~ NA_real_
    )
  )

datos_c <- datos_c %>%
  filter(!is.na(datos_c$anemia))

datos_c$total_hijos <- datos_c$hijos_d + datos_c$hijos_f
datos_c$consumo_he_hijo1 <- ifelse(datos_c$consumo_he_hijo > 0, TRUE, FALSE)
datos_c$consumo_he_madre <- ifelse(datos_c$consumo_he_madre == 1, TRUE, FALSE)
datos_c$c_pren_natal <- ifelse(datos_c$c_pren_natal == 1, TRUE, FALSE)
datos_c$amamanto <- ifelse(datos_c$amamanto == 1, TRUE, FALSE)


datos_c <- datos_c %>%
  mutate(
    sexo = factor(
      f1_s1_2, 
      labels = c("Hombre", "Mujer")
    ),
    area = factor(
      area, 
      labels = c("Urbano", "Rural")
    ),
    etnia = factor(
      etnia, 
      labels = c("Indígena", "Afroecuatoriana/o", "Montubia/o", "Mestiza/o", "Blanca/o u Otra")
    ),
    f_agua = factor(
      f_agua, 
      labels = c(
        "Empresa pública/Municipio",
        "Juntas de Agua/Organizaciones comunitarias/GAD parroquial",
        "Pozo",
        "Carro o tanquero repartidor",
        "Otras fuentes (río, vertiente, acequia, canal, grieta o agua lluvia)"
      )
    ),
    saneamiento = factor(
      saneamiento, 
      labels = c(
        "conectado a red pública de alcantarillado?",
        "conectado a pozo séptico?",
        "conectado a biodigestor?",
        "conectado a pozo ciego?",
        "con descarga directa al mar, río, lago o quebrada?",
        "Letrina?",
        "No tiene"
      )
    ),
    material_pared = factor(
      material_pared, 
      labels = c(
        "Hormigón", "Ladrillo o bloque", "Panel prefabricado (yeso, fibrocemento, etc.)",
        "Adobe o tapia", "Madera", "Caña revestida o bahareque", "Caña no revestida", "Otro, cuál"
      )
    ),
    disp_alimentos = factor(
      disp_alimentos, 
      labels = c("Si", "No", "N_s")
    ),
    lugar_parto = factor(
      lugar_parto, 
      labels = c(
        "Establecimientos de Salud del MSP",
        "Hospital/Clínica/Dispensario del IESS",
        "Seguro Social Campesino",
        "Hospital FF.AA/ Policía",
        "Junta de Beneficencia*",
        "Consejo Provincial/Unidad Municipalde Salud",
        "Fundación/ ONG**",
        "Clínica/Consultorio privado",
        "En casa",
        "Otro, Cuál?"
      )
    )
  )

#Separacion en grupos

# Niños de 6 a 23 meses
datos_6_23 <- datos_c %>%
  filter(grupo_edad_nin %in% c("2", "3"))

# Niños de 24 a 59 meses
datos_24_59 <- datos_c %>%
  filter(grupo_edad_nin %in% c("4", "5", "6"))

###Analisis descriptivo

datos_totales <- bind_rows(
  datos_6_23  %>% mutate(grupo_edad = "6-23 meses"),
  datos_24_59 %>% mutate(grupo_edad = "24-59 meses")
)

vars_interes <- c(
  "anemia", "sexo", "etnia", "area", "quintil", "f_agua", 
  "saneamiento", "material_pared", "disp_alimentos", "edad_madre", 
  "total_hijos", "lugar_parto", "c_pren_natal", "consumo_he_madre", 
  "consumo_he_hijo1", "amamanto"
)

resumen_tablas <- map_dfr(vars_interes, function(var) {
  datos_totales %>%
    filter(!is.na(.data[[var]])) %>%
    count(grupo_edad, Categoria = as.character(.data[[var]])) %>%
    group_by(grupo_edad) %>%
    mutate(Porcentaje = round((n / sum(n)) * 100, 1)) %>%
    pivot_wider(
      names_from = grupo_edad, 
      values_from = c(n, Porcentaje),
      values_fill = 0
    ) %>%
    mutate(Variable = var, .before = 1)
})


print(resumen_tablas)


##Analisis bivariado

resultado_poisson <- function(modelo){
  
  broom::tidy(modelo,
              exponentiate = TRUE,
              conf.int = TRUE)
  
}

# Sexo
modelo_sexo1 <- glm(
  anemia ~ sexo,
  family = poisson(link="log"),
  data = datos_6_23
)
modelo_sexo2 <- glm(
  anemia ~ sexo,
  family = poisson(link="log"),
  data = datos_24_59
)
resultado_poisson(modelo_sexo1)
resultado_poisson(modelo_sexo2)

# Área
modelo_area1 <- glm(
  anemia ~ area,
  family = poisson(link="log"),
  data = datos_6_23
)
modelo_area2 <- glm(
  anemia ~ area,
  family = poisson(link="log"),
  data = datos_24_59
)

resultado_poisson(modelo_area1)
resultado_poisson(modelo_area2)

# Etnia
modelo_etnia1 <- glm(
  anemia ~ etnia,
  family = poisson(link="log"),
  data = datos_6_23
)
modelo_etnia2 <- glm(
  anemia ~ etnia,
  family = poisson(link="log"),
  data = datos_24_59
)

resultado_poisson(modelo_etnia1)
resultado_poisson(modelo_etnia2)



# Quintil
modelo_quintil1 <- glm(
  anemia ~ quintil,
  family = poisson(link="log"),
  data = datos_6_23
)
modelo_quintil2 <- glm(
  anemia ~ quintil,
  family = poisson(link="log"),
  data = datos_24_59
)

resultado_poisson(modelo_quintil1)
resultado_poisson(modelo_quintil2)

#fuente de agua 
modelo_f_agua1 <- glm(
  anemia ~ f_agua,
  family = poisson(link="log"),
  data = datos_6_23
)
modelo_f_agua2 <- glm(
  anemia ~ f_agua,
  family = poisson(link="log"),
  data = datos_24_59
)

resultado_poisson(modelo_f_agua1)
resultado_poisson(modelo_f_agua2)


#Saneamiento 
modelo_saneamiento1 <- glm(
  anemia ~ saneamiento,
  family = poisson(link="log"),
  data = datos_6_23
)
modelo_saneamiento2 <- glm(
  anemia ~ saneamiento,
  family = poisson(link="log"),
  data = datos_24_59
)

resultado_poisson(modelo_saneamiento1)
resultado_poisson(modelo_saneamiento2)

# Material de paredes
modelo_paredes1 <- glm(
  anemia ~ material_pared,
  family = poisson(link="log"),
  data = datos_6_23
)
modelo_paredes2 <- glm(
  anemia ~ material_pared,
  family = poisson(link="log"),
  data = datos_24_59
)

resultado_poisson(modelo_paredes1)
resultado_poisson(modelo_paredes2)

# Disponibilidad de alimentos
modelo_alimentos1 <- glm(
  anemia ~ disp_alimentos,
  family = poisson(link="log"),
  data = datos_6_23
)
modelo_alimentos2 <- glm(
  anemia ~ disp_alimentos,
  family = poisson(link="log"),
  data = datos_24_59
)

resultado_poisson(modelo_alimentos1)
resultado_poisson(modelo_alimentos2)

# Edad materna
modelo_edadm1 <- glm(
  anemia ~ edad_madre,
  family = poisson(link="log"),
  data = datos_6_23
)
modelo_edadm2 <- glm(
  anemia ~ edad_madre,
  family = poisson(link="log"),
  data = datos_24_59
)

resultado_poisson(modelo_edadm1)
resultado_poisson(modelo_edadm2)

# Número de hijos
modelo_hijos1 <- glm(
  anemia ~ total_hijos,
  family = poisson(link="log"),
  data = datos_6_23
)
modelo_hijos2 <- glm(
  anemia ~ total_hijos,
  family = poisson(link="log"),
  data = datos_24_59
)

resultado_poisson(modelo_hijos1)
resultado_poisson(modelo_hijos2)

# Lugar del parto
modelo_parto1 <- glm(
  anemia ~ lugar_parto,
  family = poisson(link="log"),
  data = datos_6_23
)
modelo_parto2 <- glm(
  anemia ~ lugar_parto,
  family = poisson(link="log"),
  data = datos_24_59
)

resultado_poisson(modelo_parto1)
resultado_poisson(modelo_parto2)

# Controles prenatales
modelo_prenatal1 <- glm(
  anemia ~ c_pren_natal,
  family = poisson(link="log"),
  data = datos_6_23
)
modelo_prenatal2 <- glm(
  anemia ~ c_pren_natal,
  family = poisson(link="log"),
  data = datos_24_59
)

resultado_poisson(modelo_prenatal1)
resultado_poisson(modelo_prenatal2)

# Hierro durante el embarazo
modelo_hierro_madre1 <- glm(
  anemia ~ consumo_he_madre,
  family = poisson(link="log"),
  data = datos_6_23
)
modelo_hierro_madre2 <- glm(
  anemia ~ consumo_he_madre,
  family = poisson(link="log"),
  data = datos_24_59
)

resultado_poisson(modelo_hierro_madre1)
resultado_poisson(modelo_hierro_madre2)

# Hierro del niño
modelo_hierro_hijo1 <- glm(
  anemia ~ consumo_he_hijo1,
  family = poisson(link="log"),
  data = datos_6_23
)
modelo_hierro_hijo2 <- glm(
  anemia ~ consumo_he_hijo1,
  family = poisson(link="log"),
  data = datos_24_59
)

resultado_poisson(modelo_hierro_hijo1)
resultado_poisson(modelo_hierro_hijo2)

# Lactancia
modelo_lactancia1 <- glm(
  anemia ~ amamanto,
  family = poisson(link="log"),
  data = datos_6_23
)
modelo_lactancia2 <- glm(
  anemia ~ amamanto,
  family = poisson(link="log"),
  data = datos_24_59
)

resultado_poisson(modelo_lactancia1)
resultado_poisson(modelo_lactancia2)


##Modelo de regresion ( Poisson )

design_6_23 <- svydesign(
  ids = ~id_upm,
  strata = ~estrato,
  weights = ~fexp,
  data = datos_6_23,
  nest = TRUE
)

design_24_59 <- svydesign(
  ids = ~id_upm,
  strata = ~estrato,
  weights = ~fexp,
  data = datos_24_59,
  nest = TRUE
)


modelo_6_23 <- svyglm(
  anemia ~
    sexo +
    etnia +
    area +
    quintil +
    f_agua +
    saneamiento +
    material_pared +
    disp_alimentos +
    edad_madre +
    total_hijos +
    lugar_parto +
    consumo_he_madre + 
    amamanto,
  design = design_6_23,
  family = quasipoisson(link = "log")
)
summary(modelo_6_23)

modelo_24_59 <- svyglm(
  anemia ~
    sexo +
    etnia +
    quintil +
    f_agua +
    saneamiento +
    material_pared +
    disp_alimentos +
    edad_madre +
    total_hijos + 
    lugar_parto,
  design = design_24_59,
  family = quasipoisson(link = "log")
)
summary(modelo_24_59)


#Tabla de 6_23
tabla_pr1 <- data.frame(
  PR = exp(coef(modelo_6_23)),
  exp(confint(modelo_6_23)),
  p_value = summary(modelo_6_23)$coefficients[, "Pr(>|t|)"]
)

#Tabla de 24_59
tabla_pr2 <- data.frame(
  PR = exp(coef(modelo_24_59)),
  exp(confint(modelo_24_59)),
  p_value = summary(modelo_24_59)$coefficients[, "Pr(>|t|)"]
)

round(tabla_pr1, 3)
round(tabla_pr2, 3)
