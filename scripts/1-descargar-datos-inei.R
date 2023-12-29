

# descargar datos INEI 
# poblacion, altura, region natural de centro poblado segun censo 2017

library(here)
library(tidyverse)

# segun censo 2017

# https://www.inei.gob.pe/media/MenuRecursivo/publicaciones_digitales/Est/Lib1541/cuadros/dpto21.xlsx

deps <- 1:25
deps <- str_pad(deps, 2, pad = "0")
deps <- deps[-15]

# region lima y provincia lima -_-
# como 30 min para entenderlo...
lima <- c("15a", "15b")
deps <- c(deps, lima)

deps

for(i in 1:length(deps)){ #i=1
  x <- deps[i]
  url <- paste0("https://www.inei.gob.pe/media/MenuRecursivo/publicaciones_digitales/Est/Lib1541/cuadros/dpto", x, ".xlsx")
  download.file(url, here("data", "data-inei", paste0("dpto", x, ".xlsx")), quiet = TRUE, mode = "wb") # descargamos
}
