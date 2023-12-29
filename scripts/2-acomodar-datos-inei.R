
# extraer información de cada centro poblado o distrito
# se recomienda ver el excel, para entender todas las acomodaciones 

library(here)
library(tidyverse)

dptos <- rio::import_list(Sys.glob(here("data", "data-inei", "*.xlsx")))

por_region <- list()
nom_region <- list()
for(i in 1:length(dptos)){ #i=1
  bd <- dptos[[i]]
  nom_region[[i]] <- names(bd[1])
  
  # acomodamos un poco
  names(bd) <- unlist(bd[2, ])
  bd <- bd[-c(1, 2), ]
  bd <- bd[-c(1, 2), ]
  bd <- bd[-c((nrow(bd)-2):nrow(bd)), ]
  
  nom <- c("cod", "cp", "reg_natural", "altitud", "pob", "pob_hombre", "pob_mujer", "viviendas", "viviendas_ocupadas", "viviendas_desocupadas")
  names(bd) <- nom
  
  bd <- filter(bd, if_any(everything(), ~ !is.na(.))) # quitar si todo es NA
  bd <- mutate(bd, nn = nchar(cod)) # para contar codgeo
  
  bd_dis <- filter(bd, nn == 6) # distritos
  bd_dis <- bd[bd$nn == 6, ]
  index <- as.numeric(rownames(bd_dis))
  
  bdl <- list()
  for(j in 1:length(index)){ #i=1
    
    if(j == length(index)){
      # para el ultimo distrito :
      ini <- index[j] + 1
      fin <- nrow(bd)
      s <- ini:fin
    } else{
      # para los demas
      ini <- index[j] + 1
      fin <- index[j+1] - 1
      s <- ini:fin
    }
    
    bd_j <- bd[s, ]
    bdl[[j]] <- bd_j
    
  }
  
  bdl <- set_names(bdl, bd_dis$cp)
  
  # acomodaciones para bd final
  bd_dis2 <- bind_rows(bdl, .id = "distrito")
  bd_dis3 <- left_join(bd_dis2, bd_dis[1:2], by = c("distrito" = "cp"))
  bd_dis3 <- rename(bd_dis3, codgeo_cp = cod.x, codgeo_distrito = cod.y)
  bd_dis3 <- mutate(bd_dis3, codgeo_cp2 = paste0(codgeo_distrito, codgeo_cp))
  
  por_region[[i]] <- bd_dis3
  
}

por_region <- set_names(por_region, nom_region)
bd_region <- bind_rows(por_region, .id = "region")
head(bd_region)
unique(bd_region$reg_natural)

bd <- select(bd_region,  region, distrito, codgeo_distrito, codgeo_cp2, cp, reg_natural, altitud, pob)

bd <- mutate(bd, regnat3 = case_when(
  reg_natural == "Chala" ~ "Costa",
  reg_natural %in% c("Yunga marítima", "Quechua", "Suni", "Puna", "Janca") ~ "Sierra",
  reg_natural %in% c("Yunga fluvial", "Rupa Rupa", "Omagua") ~ "Selva")
)

rio::export(bd, here("data", "region_natural_cp.xlsx"))



