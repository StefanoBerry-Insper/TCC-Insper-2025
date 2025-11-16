library(tidyverse)
library(readxl)
library(sf)

shp = read_sf("C:/Users/stefa/OneDrive/Insper/TCC I/QGIS/abrainc_insper_analise_lotes_sirgas.shp")

shp


iptu = read_sf("C:/Users/stefa/OneDrive/Insper/TCC Base Dados IPTU/IPTU_2023.csv")

print(head(iptu))


iptu <- iptu %>%
  mutate(`NUMERO DO CONTRIBUINTE` = str_sub(`NUMERO DO CONTRIBUINTE`, 1, -3))

iptu <- iptu %>% 
  rename(codigo_sql = `NUMERO DO CONTRIBUINTE`)


shp <- shp %>% 
  mutate(codigo_sql= str_sub(codigo_sql, 1, -3))


view(head(shp))

shp1 <- merge(shp, iptu, by = "codigo_sql")

shp1

write_sf(shp1, "IPTUeABRAINCTratado2023.shp")
  



