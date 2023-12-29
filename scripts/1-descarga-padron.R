
# descargar padron web
# acomodar algunos nombres
# qudarse con algunas columnas 

library(here)
library(tidyverse)

#descargar padron web 
url <- "https://escale.minedu.gob.pe/documents/10156/22bf2f8a-108f-43ee-9c25-b6f83f6a2208"

download.file(url = url, destfile = here("data", "temp.zip"), quiet = TRUE, mode = "wb")
padron <- rio::import(unzip(here("data", "temp.zip"), files = "Padron_web.dbf", exdir = here("data")))
unlink(here("data", "temp.zip"))
unlink(here("data", "Padron_web.dbf"))


# nos quedamos con primaria y secundaria: 
padron2 <- filter(padron, D_NIV_MOD %in% c("Primaria", "Secundaria"))

# nos quedamos con activas y escolarizadas 
padron2 <- filter(padron2, D_FORMA == "Escolarizada", D_ESTADO == "Activa")

# generamos algunas variables de interes y ordenamos nombres 
padron2 <- padron2 %>%
  mutate(gestion2 = ifelse(D_GESTION == "Privada", "Privada", "Pública"),
         gestion3 = case_when(D_GESTION == "Privada" ~ "Privada",
                              D_GESTION == "P£blica de gesti¢n privada" ~ "Pública de gestión privada",
                              D_GESTION == "P£blica de gesti¢n directa" ~ "Pública de gestión directa"),
         gestion_dep = case_when(D_GES_DEP == "Asociaci¢n civil / Inst.Ben‚fica" ~ "Asociación civil/Institución benéfica",
                                 D_GES_DEP == "Comunidad o asociaci¢n religiosa" ~ "Comunidad o asociación religiosa",
                                 D_GES_DEP == "Convenio con Sector Educaci¢n" ~ "Convenio con Sector Educación",
                                 D_GES_DEP == "Sector Educaci¢n" ~ "Sector Educación",
                                 D_GES_DEP == "Otro sector p£blico (FF.AA.)" ~ "Otro sector público (FFAA)", 
                                 TRUE ~ D_GES_DEP),
         estrato = case_when(gestion2 == "Privada" ~ "Privada",
                             gestion2 == "Pública" & DAREACENSO == "Rural" ~ "Pública rural",
                             gestion2 == "Pública" & DAREACENSO == "Urbana" ~ "Pública urbana")
  )

padron2 <- padron2 %>%
  select(cod_mod7 = COD_MOD, anexo = ANEXO, CODLOCAL, 
         gestion2, gestion3, gestion_dep, area = DAREACENSO, estrato, 
         centro_poblado = CEN_POB, localidad = LOCALIDAD, dep = D_DPTO, prov = D_PROV, distrito = D_DIST, 
         dre = D_REGION, ugel = D_DREUGEL, CODCCPP, CODGEO, CODCP_INEI,
         NLAT_IE, NLONG_IE)

rio::export(padron2, here("data", "padron.rds"))


