

# agregamos otras variables geograficas a base de padron

library(here)
library(tidyverse)

padron <- rio::import(here("data", "padron.rds"))
regnat <- rio::import(here("data", "region_natural_cp.xlsx"))
regnat <- select(regnat, codgeo_cp2, regnat9 = reg_natural, altitud, pob2017 = pob, regnat3, -distrito)

bdf <- left_join(padron2, regnat, by = c("CODCP_INEI" = "codgeo_cp2"))

mean(is.na(bdf$regnat3)) # hay missing! 

# algunos no tienen codgeo de cp
# que le asignamos? la region del colegio mas cercano
# https://conservationecology.wordpress.com/2013/06/30/distance-between-two-points-in-r/
# ¿que tan aproximado? 

earth.dist <- function (long1, lat1, long2, lat2){
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}


# nos quedamos con los casos que tiene missing en cp
tmp <- filter(bdf, is.na(CODCP_INEI))

for(i in 1:nrow(tmp)){ #i=2706
  
  tmp_i <- tmp[i, ]
  
  # los que si tienen codgeo inei y son del mismo distrito de tmp_i
  tmp_i_dis <- filter(bdf, distrito == tmp_i$distrito, !is.na(CODCP_INEI))
  tmp_i_prov <- filter(bdf, prov == tmp_i$prov, !is.na(CODCP_INEI))
  
  if(nrow(tmp_i_dis) != 0){
    dat <- tmp_i_dis
  }else{
    dat <- tmp_i_prov
  }
  
  # calculamos la distancia entre todos los colegios de ese distrito y el colegio
  distancia <- map2(dat$NLAT_IE, dat$NLONG_IE,
                    ~earth.dist(lat1 = tmp_i$NLAT_IE, long1 = tmp_i$NLONG_IE, lat2 = .x, long2 = .y))
  
  index <- which.min(unlist(distancia)) # posicion de la menor distancia
  rn9 <- dat[index, ]$regnat9 # region natural del colegio mas cercano
  rn3 <- dat[index, ]$regnat3 # region natural del colegio mas cercano
  
  tmp[i, 21] <- rn9 # reemplazamos el valor
  tmp[i, 24] <- rn3 # reemplazamos el valor
  
}

# juntamos las bases
bdf_nomiss <- filter(bdf, !is.na(CODCP_INEI))
bdfinal <- bind_rows(bdf_nomiss, tmp)

# cambiamos nombre a las DRE
bdfinal <- mutate(bdfinal, dre = str_remove(dre, "DRE "))
bdfinal <- mutate(bdfinal, dre = tolower(dre))
bdfinal <- mutate(bdfinal, dre = str_to_sentence(dre))

bdfinal <- mutate(bdfinal, dre = 
                    case_when(dre == "Ancash" ~ "Áncash",
                              dre == "Apurimac" ~ "Apurímac",
                              dre == "Huanuco" ~ "Huánuco",
                              dre == "Junin" ~ "Junín",
                              dre == "La libertad" ~ "La Libertad",
                              dre == "Lima metropolitana" ~ "Lima Metropolitana",
                              dre == "Lima provincias" ~ "Lima Provincias",
                              dre == "Madre de dios" ~ "Madre de Dios",
                              dre == "San martin" ~ "San Martín",
                              TRUE ~ dre))

rio::export(bdfinal, here("data", "padron2.rds"))


# rio::import("https://github.com/diegohc1/padron-ie/raw/main/data/padron2.rds")
