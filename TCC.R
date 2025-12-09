# ====================== PACOTES ======================
library(dplyr)
library(stringr)
library(janitor)
library(readr)
library(arrow)
library(ggplot2)
library(sf)
library(tidyr)
library(sfarrow)
# ====================== CONFIG ======================
base_dir_iptu <- "C:/Users/stefa/OneDrive/Insper/TCC II"
arq_iptu_2014 <- file.path(base_dir_iptu, "IPTUeABRAINCTratado2014.shp")
arq_iptu_2023 <- file.path(base_dir_iptu, "IPTUeABRAINCTratado2023.shp")
arq_zoneamento <- file.path(base_dir_iptu, "zoneamento.parquet")
arq_zoneamento2024 <- file.path(base_dir_iptu, "zoneamento_2024.parquet")

base_dir_parq <- "C:/Users/stefa/OneDrive/Insper/TCC II"
arq_parquet   <- file.path(base_dir_parq, "geo_alvaras_emp_pde.parquet")

out_dir <- file.path(base_dir_iptu, "Distritos_Tabelas")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
plots_dir <- file.path(out_dir, "graficos_distritos")
if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)

# ====================== HELPERS ======================
pick_first <- function(df, candidates) {
  existing <- intersect(candidates, names(df))
  if (length(existing) == 0) return(rep(NA, nrow(df)))
  df[[existing[1]]]
}

to_num <- function(v) {
  if (is.numeric(v)) return(v)
  out <- readr::parse_number(v, locale = readr::locale(decimal_mark = ",", grouping_mark = "."))
  if (all(is.na(out))) out <- readr::parse_number(v)
  out
}

norm_txt <- function(x) {
  x |> stringi::stri_trans_general("Latin-ASCII") |> str_to_lower() |>
    str_replace_all("\\s+", " ") |> str_trim()
}


# ====================== 1) IPTU 2014 ======================
iptu14_raw <- sf::read_sf(arq_iptu_2014) |> clean_names()

iptu14 <- iptu14_raw |>
  mutate(
    area_ocupada    = suppressWarnings(as.numeric(pick_first(iptu14_raw, c("areaocu")))),
    area_do_terreno = to_num(pick_first(iptu14_raw, c("areadot","area_terreno","area_lote","area_do_lote"))),
    distrito_iptu   = pick_first(iptu14_raw, c("bairrdi","bairro_do_imovel","distrito","nome_do_distrito")),
    padrao          = pick_first(iptu14_raw, c("tipdpdc","padrao","padrao_da_construcao")),
    uso_descr       = pick_first(iptu14_raw, c("tipdudi","tipo_de_uso","uso"))
  ) |>
  mutate(
    distrito_norm = norm_txt(distrito_iptu),
    padrao_norm   = norm_txt(padrao),
    eh_horizontal = str_detect(padrao_norm, "\\bhorizontal\\b"),
    eh_terreno    = !is.na(area_ocupada) & area_ocupada == 0
  ) |>
  filter(!is.na(distrito_norm), (eh_terreno | eh_horizontal)) |>
  select(-padrao_norm, -uso_descr)


# ====================== 2) IPTU 2023 ======================
iptu23_raw <- sf::read_sf(arq_iptu_2023) |> clean_names()

iptu23 <- iptu23_raw |>
  mutate(
    area_ocupada    = suppressWarnings(as.numeric(pick_first(iptu23_raw, c("areaocu")))),
    area_do_terreno = to_num(pick_first(iptu23_raw, c("areadot","area_terreno","area_lote","area_do_lote"))),
    distrito_iptu   = pick_first(iptu23_raw, c("bairrdi","bairro_do_imovel","distrito","nome_do_distrito")),
    padrao          = pick_first(iptu23_raw, c("tipdpdc","padrao","padrao_da_construcao")),
    uso_descr       = pick_first(iptu23_raw, c("tipdudi","tipo_de_uso","uso"))
  ) |>
  mutate(
    distrito_norm = norm_txt(distrito_iptu),
    padrao_norm   = norm_txt(padrao),
    eh_horizontal = str_detect(padrao_norm, "\\bhorizontal\\b"),
    eh_terreno    = !is.na(area_ocupada) & area_ocupada == 0
  ) |>
  filter(!is.na(distrito_norm), (eh_terreno | eh_horizontal)) |>
  select(-padrao_norm, -uso_descr)


# ====================== 3) ZONEAMENTO + JUNÇÃO ESPACIAL ======================

zoneamento_2016 <- sfarrow::st_read_parquet(
  arq_zoneamento) |>
  dplyr::select(distrito, zoneamento, zoneamento_grupo, geometry)

iptu14 <- st_transform(iptu14, crs = 31983) # Transformar CRS para SIRGAS 
zoneamento_2016<- st_transform(zoneamento_2016, 31983) # Transformar CRS para SIRGAS 2000
iptu14_zonas <- st_intersection(
  iptu14 %>% mutate(area_total = st_area(geometry)),
  zoneamento_2016
)

# Calcula área da intersecção
iptu14_zonas <- iptu14_zonas %>%
  mutate(
    area_intersec = st_area(geometry),
    prop_intersec = as.numeric(area_intersec / area_total)
  )
iptu14_zonas <- iptu14_zonas %>%
  filter(!is.na(zoneamento))
iptu14_eetu <- iptu14_zonas %>%
  filter(zoneamento_grupo %in% c("EETU", "EETU Futuros"))

zoneamento_2024 <- sfarrow::st_read_parquet(
  arq_zoneamento2024) |>
  dplyr::select(distrito, zoneamento, zoneamento_grupo, geometry)

iptu23 <- st_transform(iptu23, crs = 31983) # Transformar CRS para SIRGAS 
zoneamento_2024<- st_transform(zoneamento_2024, 31983) # Transformar CRS para SIRGAS 2000
iptu23_zonas <- st_intersection(
  iptu23 %>% mutate(area_total = st_area(geometry)),
  zoneamento_2024
)

# Calcula área da intersecção
iptu23_zonas <- iptu23_zonas %>%
  mutate(
    area_intersec = st_area(geometry),
    prop_intersec = as.numeric(area_intersec / area_total)
  )
iptu23_zonas <- iptu23_zonas %>%
  filter(!is.na(zoneamento))
iptu23_eetu <- iptu23_zonas %>%
  filter(zoneamento_grupo %in% c("EETU", "EETU Futuros"))

# ====================== 3) ZONEAMENTO + JUNÇÃO ESPACIAL (Terreno Total) ======================

iptu14_total <- st_transform(iptu14_raw, crs = 31983) # Transformar CRS para SIRGAS 

iptu14_zonastotal <- st_intersection(
  iptu14_total %>% mutate(area_total = st_area(geometry)),
  zoneamento_2016
)

# Calcula área da intersecção
iptu14_zonastotal <- iptu14_zonastotal %>%
  mutate(
    area_intersec = st_area(geometry),
    prop_intersec = as.numeric(area_intersec / area_total)
  )
iptu14_zonastotal <- iptu14_zonastotal %>%
  filter(!is.na(zoneamento))
iptu14_zonastotal <- iptu14_zonastotal %>%
  filter(zoneamento_grupo %in% c("EETU", "EETU Futuros"))

iptu23_total <- st_transform(iptu23_raw, crs = 31983) # Transformar CRS para SIRGAS 

iptu23_zonastotal <- st_intersection(
  iptu23_total %>% mutate(area_total = st_area(geometry)),
  zoneamento_2024
)

# Calcula área da intersecção
iptu23_zonastotal <- iptu23_zonastotal %>%
  mutate(
    area_intersec = st_area(geometry),
    prop_intersec = as.numeric(area_intersec / area_total)
  )
iptu23_zonastotal <- iptu23_zonastotal %>%
  filter(!is.na(zoneamento))
iptu23_zonastotal <- iptu23_zonastotal %>%
  filter(zoneamento_grupo %in% c("EETU", "EETU Futuros"))

# ====================== 3) ALVARÁS (por distrito e ano) ======================
alvaras_raw <- read_parquet(arq_parquet) |> clean_names()

# helpers já definidos acima: pick_first(), to_num(), norm_bairro()

alvaras <- alvaras_raw |>
  dplyr::mutate(
    distrito_alv    = .data$distrito_x,
    ano             = suppressWarnings(as.integer(.data$ano_execucao)),
    area_do_terreno = to_num(
      pick_first(alvaras_raw, c("area_do_terreno","area_terreno","area_lote","area_do_lote"))
    )
  ) |>
  dplyr::filter(
    zoneamento_grupo %in% c("EETU", "EETU Futuro"),
    ano >= 2014, ano <= 2024
  ) |>
  dplyr::transmute(
    distrito_norm = norm_txt(distrito_alv),
    ano,
    area_do_terreno
  ) |>
  dplyr::filter(!is.na(distrito_norm), !is.na(ano), !is.na(area_do_terreno))


# ====================== 5) CONSUMO ANUAL POR DISTRITO ======================

consumo_anual_distrito <- alvaras %>%
  dplyr::group_by(distrito_norm, ano) %>%
  dplyr::summarise(consumo_m2 = sum(area_do_terreno, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(distrito_norm, ano)

# completar anos 2014..2024 e calcular acumulado
todos_distritos <- sort(unique(consumo_anual_distrito$distrito_norm))
anos_seq        <- 2014:2024
grid <- expand.grid(distrito_norm = todos_distritos, ano = anos_seq)

consumo_anual_distrito_full <- grid %>%
  dplyr::left_join(consumo_anual_distrito, by = c("distrito_norm","ano")) %>%
  dplyr::mutate(consumo_m2 = coalesce(consumo_m2, 0)) %>%
  dplyr::group_by(distrito_norm) %>%
  dplyr::arrange(ano, .by_group = TRUE) %>%
  dplyr::mutate(consumo_acumulado_m2 = cumsum(consumo_m2)) %>%
  dplyr::ungroup()

# ====================== 6) GRÁFICOS POR DISTRITO ======================

# ========= helpers =========
norm_txt <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("\\s+", " ") |>
    stringr::str_trim()
}
fmt <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)

# ========= consumo (2014–2024) =========
consumo_anual_distrito <- alvaras |>
  dplyr::group_by(distrito_norm, ano) |>
  dplyr::summarise(consumo_m2 = sum(area_do_terreno, na.rm = TRUE), .groups = "drop") |>
  dplyr::arrange(distrito_norm, ano)

anos_seq <- 2014:2024
consumo_anual_distrito_full <- tidyr::expand_grid(
  distrito_norm = sort(unique(consumo_anual_distrito$distrito_norm)),
  ano = anos_seq
) |>
  dplyr::left_join(consumo_anual_distrito, by = c("distrito_norm","ano")) |>
  dplyr::mutate(consumo_m2 = dplyr::coalesce(consumo_m2, 0)) |>
  dplyr::group_by(distrito_norm) |>
  dplyr::arrange(ano, .by_group = TRUE) |>
  dplyr::mutate(consumo_acumulado_m2 = cumsum(consumo_m2)) |>
  dplyr::ungroup()

# ========= estoque por distrito: 2014 e 2023 (base EETU) =========
# 2014 — garantir numérico e somar
estoque_2014_por_distrito <- iptu14_eetu |>
  dplyr::mutate(
    distrito_norm = norm_txt(distrito),
    areadot_num   = to_num(areadot)   # ou: readr::parse_number(areadot, locale = readr::locale(decimal_mark = ",", grouping_mark = "."))
  ) |>
  dplyr::filter(!is.na(distrito_norm), !is.na(areadot_num)) |>
  dplyr::group_by(distrito_norm) |>
  dplyr::summarise(estoque_2014_m2 = sum(areadot_num, na.rm = TRUE), .groups = "drop")


# 2023 — garantir numérico e somar
estoque_2023_por_distrito <- iptu23_eetu |>
  dplyr::mutate(
    distrito_norm = norm_txt(distrito),
    areadot_num   = to_num(areadot)   # mesmo tratamento
  ) |>
  dplyr::filter(!is.na(distrito_norm), !is.na(areadot_num)) |>
  dplyr::group_by(distrito_norm) |>
  dplyr::summarise(estoque_2023_m2 = sum(areadot_num, na.rm = TRUE), .groups = "drop")


estoques_distritos <- dplyr::full_join(
  estoque_2014_por_distrito |> sf::st_drop_geometry(),
  estoque_2023_por_distrito |> sf::st_drop_geometry(),
  by = "distrito_norm"
) |>
  tidyr::replace_na(list(estoque_2014_m2 = 0, estoque_2023_m2 = 0))


# ========= tabela longa (para facilitar gráficos e export) =========
tabelas_por_distrito <- consumo_anual_distrito_full |>
  dplyr::left_join(estoques_distritos, by = "distrito_norm") |>
  dplyr::relocate(dplyr::all_of(c("distrito_norm","ano","consumo_m2","consumo_acumulado_m2",
                                  "estoque_2014_m2","estoque_2023_m2")))

# ========= salvar CSV por distrito =========
unique_distritos <- unique(tabelas_por_distrito$distrito_norm)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
for (d in unique_distritos) {
  tb <- dplyr::filter(tabelas_por_distrito, distrito_norm == d)
  fname <- paste0("tabela_distrito_", gsub("[^a-z0-9]+", "_", d), ".csv")
  readr::write_csv(tb, file.path(out_dir, fname))
}

# ========= estoque por distrito: 2014 e 2023 (total) =========
# 2014 — garantir numérico e somar
estoque_2014_por_distritototal <- iptu14_zonastotal |>
  dplyr::mutate(
    distrito_norm = norm_txt(distrito),
    areadot_num   = to_num(areadot)   # ou: readr::parse_number(areadot, locale = readr::locale(decimal_mark = ",", grouping_mark = "."))
  ) |>
  dplyr::filter(!is.na(distrito_norm), !is.na(areadot_num)) |>
  dplyr::group_by(distrito_norm) |>
  dplyr::summarise(estoque_2014_m2total = sum(areadot_num, na.rm = TRUE), .groups = "drop")


# 2023 — garantir numérico e somar
estoque_2023_por_distritototal <- iptu23_zonastotal |>
  dplyr::mutate(
    distrito_norm = norm_txt(distrito),
    areadot_num   = to_num(areadot)   # mesmo tratamento
  ) |>
  dplyr::filter(!is.na(distrito_norm), !is.na(areadot_num)) |>
  dplyr::group_by(distrito_norm) |>
  dplyr::summarise(estoque_2023_m2total = sum(areadot_num, na.rm = TRUE), .groups = "drop")


estoques_distritostotal <- dplyr::full_join(
  estoque_2014_por_distritototal |> sf::st_drop_geometry(),
  estoque_2023_por_distritototal |> sf::st_drop_geometry(),
  by = "distrito_norm"
) |>
  tidyr::replace_na(list(estoque_2014_m2total = 0, estoque_2023_m2total = 0))

# ========= gráficos (estoque 2014 para 2014–2022, estoque 2023 para 2023–2034) =========
plots_dir <- file.path(out_dir, "graficos_distritos")
if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)

for (d in unique_distritos) {
  tb <- dplyr::filter(tabelas_por_distrito, distrito_norm == d)
  
  est14 <- dplyr::first(tb$estoque_2014_m2) %>% { ifelse(is.na(.), 0, .) }
  est23 <- dplyr::first(tb$estoque_2023_m2) %>% { ifelse(is.na(.), 0, .) }
  
  # séries de estoque “em degraus”
  amarelas_d <- dplyr::bind_rows(
    tibble::tibble(ano = 2014:2022, valor = est14, serie = "Estoque (base 2014)"),
    tibble::tibble(ano = 2023:2034, valor = est23, serie = "Estoque (base 2023)")
  )
  
  # consumo acumulado 2014–2024
  consumo_d <- tb |>
    dplyr::select(ano, consumo_acumulado_m2) |>
    dplyr::distinct() |>
    dplyr::arrange(ano)
  
  consumo_total_2024 <- max(consumo_d$consumo_acumulado_m2, na.rm = TRUE)
  
  # média anual (2014–2024) para projetar a partir de 2024 contra o estoque de 2023
  serie_anual_d <- consumo_anual_distrito |>
    dplyr::filter(distrito_norm == d) |>
    dplyr::right_join(tibble::tibble(ano = 2014:2024), by = "ano") |>
    dplyr::mutate(consumo_m2 = dplyr::coalesce(consumo_m2, 0)) |>
    dplyr::arrange(ano)
  media_anual_14_24 <- mean(serie_anual_d$consumo_m2, na.rm = TRUE)
  
  # projeção (só se est_2023 > consumo acumulado 2024)
  projeta <- (est23 > consumo_total_2024) && is.finite(media_anual_14_24) && (media_anual_14_24 > 0)
  proj_df <- NULL
  frase_esgotamento <- "Sem projeção (estoque ≤ consumo ou média anual = 0)."
  if (projeta) {
    anos_ate_esgotar <- (est23 - consumo_total_2024) / media_anual_14_24
    ano_esgotamento  <- 2024 + anos_ate_esgotar
    limite_ano       <- max(2024, ceiling(ano_esgotamento))
    proj_df <- tibble::tibble(
      ano = 2024:limite_ano,
      consumo_proj_acum = consumo_total_2024 + media_anual_14_24 * (ano - 2024)
    )
    frase_esgotamento <- paste0("Esgotaria por volta de ", limite_ano, ".")
  }
  
  g <- ggplot2::ggplot() +
  
  # Estoques (linhas douradas)
  ggplot2::geom_line(
    data = amarelas_d,
    ggplot2::aes(x = ano, y = valor, group = serie, color = "Estoque disponível"),
    linewidth = 1
  ) +
  ggplot2::geom_point(
    data = amarelas_d,
    ggplot2::aes(x = ano, y = valor, color = "Estoque disponível"),
    size = 2
  ) +
  
  # Consumo acumulado (linha verde)
  ggplot2::geom_line(
    data = consumo_d,
    ggplot2::aes(x = ano, y = consumo_acumulado_m2, color = "Consumo acumulado"),
    linewidth = 1.2
  ) +
  ggplot2::geom_point(
    data = consumo_d,
    ggplot2::aes(x = ano, y = consumo_acumulado_m2, color = "Consumo acumulado"),
    size = 2
  ) +
  
  # Projeção (linha vermelha) se aplicável
  { if (projeta) ggplot2::geom_line(
      data = proj_df,
      ggplot2::aes(x = ano, y = consumo_proj_acum, color = "Projeção de consumo"),
      linewidth = 1.1
    )
  } +
  
  ggplot2::geom_vline(xintercept = 2023, linetype = "dashed", linewidth = 0.6) +
  
  ggplot2::scale_x_continuous(limits = c(2014, 2034), breaks = seq(2014, 2034, 2)) +
  ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  
  # LEGENDA
  ggplot2::scale_color_manual(
    name = "Legenda",
    values = c(
      "Estoque disponível" = "goldenrod",
      "Consumo acumulado" = "forestgreen",
      "Projeção de consumo" = "red"
    )
  ) +
  
  ggplot2::labs(
    title = paste0("Estoque (2014/2023) e Consumo Acumulado — Distrito: ",
                   stringr::str_to_title(d)),
    subtitle = paste0(
      "Estoque 2014: ", fmt(est14), " m²  |  ",
      "Estoque 2023: ", fmt(est23), " m²  |  ",
      "Consumo total (até 2024): ", fmt(consumo_total_2024), " m²"
    ),
    x = "Ano", y = "m²"
  ) +
  
  ggplot2::theme_minimal(base_size = 12)
  
  fname_plot <- paste0("grafico_distrito_", gsub("[^a-z0-9]+", "_", d), ".png")
  ggplot2::ggsave(file.path(plots_dir, fname_plot), g, width = 11, height = 6, dpi = 300)
}

cat("Gráficos por distrito salvos em:", plots_dir, "\n")


# ====================== 1) Parâmetros ======================
distritos_alvo_rotulo <- c(
  "Santo Amaro","Mooca","Vila Mariana","Pinheiros","Moema",
  "Lapa","Perdizes","Campo Belo","Jabaquara","Butantã",
  "Consolação"
)
distritos_alvo_norm <- norm_txt(distritos_alvo_rotulo)

# ====================== 2) Consumo total 2014–2024 ======================
# Filtra apenas distritos selecionados e soma o consumo total no período
consumo_total_distritos <- alvaras |>
  dplyr::mutate(distrito_norm = norm_txt(distrito_norm)) |>
  dplyr::filter(
    distrito_norm %in% distritos_alvo_norm,
    dplyr::between(ano, 2014, 2024)
  ) |>
  dplyr::group_by(distrito_norm) |>
  dplyr::summarise(consumo_total_m2 = sum(area_do_terreno, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(Distrito = stringr::str_to_title(distrito_norm)) |>
  dplyr::arrange(desc(consumo_total_m2))


# ====================== 3) Gráfico estilo horizontal ======================
library(ggplot2)

g_consumo_total <- ggplot(consumo_total_distritos, aes(
  x = consumo_total_m2,
  y = reorder(Distrito, consumo_total_m2),
  fill = Distrito
)) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(label = scales::label_number(big.mark = ".", decimal.mark = ",")(round(consumo_total_m2, 0))),
    hjust = -0.05, size = 3.5, color = "black"
  ) +
  scale_x_continuous(
    labels = scales::label_number(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "Total consumido no período (2014–2024) por distrito",
    x = "Consumo de solo (m²)",
    y = "Distrito"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.title.y = element_text(margin = margin(r = 10))
  ) +
  scale_fill_brewer(palette = "Paired")

# ====================== 4) Salvar o gráfico ======================
plots_dir <- file.path(out_dir, "graficos_sintese")
if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)

ggsave(
  filename = file.path(plots_dir, "consumo_total_2014_2024_por_distrito.png"),
  plot = g_consumo_total,
  width = 9,
  height = 6,
  dpi = 300
)

cat("✅ Gráfico salvo em:", file.path(plots_dir, "consumo_total_2014_2024_por_distrito.png"), "\n")

# ====================== CONSUMO X ESTOQUE TOTAL (distritos) ======================
dados_combo <- consumo_total_distritos |>
  # garantir mesma normalização de distrito
  mutate(distrito_norm = norm_txt(distrito_norm)) |>
  left_join(
    estoques_distritostotal |>
      mutate(distrito_norm = norm_txt(distrito_norm)) |>
      select(distrito_norm, estoque_2023_m2total),
    by = "distrito_norm"
  ) |>
  # passar para formato longo: Consumo x Estoque
  tidyr::pivot_longer(
    cols = c(consumo_total_m2, estoque_2023_m2total),
    names_to = "tipo",
    values_to = "valor"
  ) |>
  mutate(
    tipo = dplyr::recode(
      tipo,
      "consumo_total_m2"      = "Consumo total 2014–2024",
      "estoque_2023_m2total" = "Estoque total 2023"
    )
  )

# ====================== 2) Gráfico horizontal com duas barras por distrito ======================

g_conest_total <- ggplot(dados_combo, aes(
  y = reorder(Distrito, valor)
)) +
  
  # Estoque total – eixo inferior
  geom_col(
    data = dados_combo |> filter(tipo == "Estoque total 2023"),
    aes(x = valor, fill = tipo),
    position = "identity",
    alpha = 0.7
  ) +
  
  # Consumo total – eixo superior (transformado)
  geom_col(
    data = dados_combo |> filter(tipo == "Consumo total 2014–2024"),
    aes(x = valor * 1, fill = tipo),
    position = "identity",
    alpha = 0.7
  ) +
  
  scale_x_continuous(
    name = "Estoque total 2023 (m²)",
    labels = scales::label_number(big.mark = ".", decimal.mark = ","),
    sec.axis = sec_axis(
      trans = ~ .,  # mesma escala (pode mudar se quiser)
      name = "Consumo total 2014–2024 (m²)",
      labels = scales::label_number(big.mark = ".", decimal.mark = ",")
    ),
    expand = expansion(mult = c(0, 0.15))
  ) +
  
  scale_fill_brewer(palette = "Paired") +
  labs(
    title = "Consumo total (2014–2024) e Terreno total (2023) por distrito",
    y = "Distrito", fill = "Indicador"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x.top = element_text(margin = margin(b = 10)),
    axis.title.x = element_text(margin = margin(t = 10))
  )


# ====================== 4) Salvar o gráfico ======================
plots_dir <- file.path(out_dir, "graficos_sintese")
if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)

ggsave(
  filename = file.path(plots_dir, "consumo_e_estoque_total_2014_2024_por_distrito.png"),
  plot = g_conest_total,
  width = 9,
  height = 6,
  dpi = 300
)

cat("✅ Gráfico salvo em:", file.path(plots_dir, "consumo_e_estoque_total_2014_2024_por_distrito.png"), "\n")


# ====================== ESTOQUE TOTAL (soma de todos os distritos) ======================

# Remove geometrias e soma os estoques totais
estoque_2014_total <- iptu14_eetu |>
  sf::st_drop_geometry() |>
  dplyr::filter(!is.na(areadot)) |>
  dplyr::mutate(areadot = readr::parse_number(as.character(areadot))) |>
  dplyr::summarise(estoque_2014_m2 = sum(areadot, na.rm = TRUE)) |>
  dplyr::pull(estoque_2014_m2)

estoque_2023_total <- iptu23_eetu |>
  sf::st_drop_geometry() |>
  dplyr::filter(!is.na(areadot)) |>
  dplyr::mutate(areadot = readr::parse_number(as.character(areadot))) |>
  dplyr::summarise(estoque_2023_m2 = sum(areadot, na.rm = TRUE)) |>
  dplyr::pull(estoque_2023_m2)

# ====================== CRIAR SÉRIES TEMPORAIS ======================
estoque_series <- dplyr::bind_rows(
  tibble::tibble(ano = 2014:2022, valor = estoque_2014_total, serie = "Estoque (base 2014)"),
  tibble::tibble(ano = 2023:2034, valor = estoque_2023_total, serie = "Estoque (base 2023)")
)

# consumo total (acumulado) — precisa existir a base consumo_anual_distrito_full
consumo_series <- consumo_anual_distrito_full |>
  dplyr::group_by(ano) |>
  dplyr::summarise(consumo_acumulado_m2 = sum(consumo_acumulado_m2, na.rm = TRUE), .groups = "drop")

# ====================== GRÁFICO ======================
g_total <- ggplot2::ggplot() +
  
  # linhas douradas de estoque
  ggplot2::geom_line(
    data = estoque_series,
    ggplot2::aes(x = ano, y = valor, group = serie, color = "Estoque disponível"),
    linewidth = 1
  ) +
  ggplot2::geom_point(
    data = estoque_series,
    ggplot2::aes(x = ano, y = valor, color = "Estoque disponível"),
    size = 2
  ) +
  
  # linha verde (consumo acumulado)
  ggplot2::geom_line(
    data = consumo_series,
    ggplot2::aes(x = ano, y = consumo_acumulado_m2, color = "Consumo acumulado"),
    linewidth = 1.3
  ) +
  ggplot2::geom_point(
    data = consumo_series,
    ggplot2::aes(x = ano, y = consumo_acumulado_m2, color = "Consumo acumulado"),
    size = 2
  ) +
  
  ggplot2::geom_vline(xintercept = 2023, linetype = "dashed", linewidth = 0.6) +
  
  ggplot2::scale_x_continuous(
    limits = c(2014, 2034),
    breaks = seq(2014, 2034, 2)
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, NA),
    labels = scales::label_number(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  # LEGENDA (apenas duas linhas)
  ggplot2::scale_color_manual(
    name = "Legenda",
    values = c(
      "Estoque disponível" = "goldenrod",
      "Consumo acumulado" = "forestgreen"
    )
  ) +
  
  ggplot2::labs(
    title = "Município de São Paulo — Estoque total (2014/2023) e Consumo acumulado (2014–2024)",
    subtitle = paste0(
      "Estoque 2014: ", fmt(estoque_2014_total), " m²  |  ",
      "Estoque 2023: ", fmt(estoque_2023_total), " m²  |  ",
      "Consumo acumulado até 2024: ", fmt(max(consumo_series$consumo_acumulado_m2, na.rm = TRUE)), " m²"
    ),
    x = "Ano", y = "m²"
  ) +
  
  ggplot2::theme_minimal(base_size = 13)


# salvar o gráfico
ggplot2::ggsave(
  filename = file.path(out_dir, "grafico_estoque_total_sp.png"),
  plot = g_total,
  width = 11,
  height = 6,
  dpi = 300
)

cat("Gráfico total do município salvo em:", file.path(out_dir, "grafico_estoque_total_sp.png"), "\n")


# ---- Consumo total por ano (município) ----
consumo_municipio <- alvaras |>
  dplyr::filter(!is.na(ano), !is.na(area_do_terreno)) |>
  dplyr::group_by(ano) |>
  dplyr::summarise(consumo_m2 = sum(area_do_terreno, na.rm = TRUE), .groups = "drop") |>
  dplyr::arrange(ano)

# ---- Gráfico: barras + linha de tendência ----
library(ggplot2)
library(scales)

ggplot(consumo_municipio, aes(x = ano, y = consumo_m2, fill = ano)) +
  geom_col(width = 0.8) +
  # linha de tendência (regressão linear sobre todo o período)
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed", linewidth = 1) +
  scale_x_continuous(breaks = 2014:2024) +
  scale_y_continuous(
    limits = c(0, NA),
    labels = label_number(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.05))
  ) +
  # gradiente de cores ao longo do tempo (ajuste se quiser outra paleta)
  scale_fill_gradientn(
    colours = c("#0b0b6d","#4336a7","#7a2ca5","#b12a90","#d75573","#e98f53","#f1c453","#f9f871")
  ) +
  labs(
    title = "Consumo de Solo no Município de São Paulo (2014–2024)",
    x = "Ano",
    y = "Consumo de solo (m²)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ====================== ESTOQUE DISPONÍVEL POR DISTRITO (ex.: 2023) ======================

# normalizador simples (use o seu norm_txt se já estiver no script)
norm_txt_local <- function(x){
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("\\s+"," ") |>
    stringr::str_trim()
}

# rotulador (acerta Butantã, etc.)
fix_rotulo <- function(v){
  base <- stringr::str_to_title(v)
  base[v == "butanta"] <- "Butantã"
  base[v == "consolacao"] <- "Consolação"
  base
}

distritos_alvo_norm <- norm_txt_local(distritos_alvo_rotulo)

# ==== Tabela para o gráfico ====
tab_estoque23 <- estoque_2023_por_distrito |>
  dplyr::mutate(distrito_norm = norm_txt_local(distrito_norm)) |>
  dplyr::filter(distrito_norm %in% distritos_alvo_norm) |>
  dplyr::select(distrito_norm, estoque_2023_m2) |>
  dplyr::mutate(Distrito = fix_rotulo(distrito_norm)) |>
  dplyr::arrange(desc(estoque_2023_m2))

total_sel <- sum(tab_estoque23$estoque_2023_m2, na.rm = TRUE)


# ====================== GRÁFICO HORIZONTAL ======================

g_estoque_2023 <- ggplot(
  tab_estoque23,
  aes(x = estoque_2023_m2, y = reorder(Distrito, estoque_2023_m2), fill = Distrito)
) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(label = scales::label_number(big.mark=".", decimal.mark=",")(round(estoque_2023_m2,0))),
    hjust = -0.05, size = 3.5, color = "black"
  ) +
  scale_x_continuous(
    labels = scales::label_number(big.mark=".", decimal.mark=","),
    expand = expansion(mult = c(0, 0.10))
  ) +
  labs(
    title = "Estoque de solo disponível em 2023",
    subtitle = paste0(
      "Total selecionado: ",
      scales::label_number(big.mark=".", decimal.mark=",")(round(total_sel,0)),
      " m²"
    ),
    x = "Estoque de solo (m²)",
    y = "Distrito",
    caption = "Fonte: IPTU (estoque EETU) — elaboração própria."
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.y = element_blank()) +
  scale_fill_brewer(palette = "Paired")




plots_dir <- file.path(out_dir, "graficos_sintese")
if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)

ggsave(file.path(plots_dir, "estoque_2023_por_distrito_selecionados.png"),
       g_estoque_2023, width = 10, height = 7, dpi = 300)

cat("✅ Gráfico salvo em:", file.path(plots_dir, "estoque_2023_por_distrito_selecionados.png"), "\n")

# ====================== Estoque total vs Terreno total ======================


# ====================== 1) Base de Estoque (2023) ======================

tab_estoque23tot <- estoques_distritostotal |>
  mutate(distrito_norm = norm_txt_local(distrito_norm)) |>
  filter(distrito_norm %in% distritos_alvo_norm) |>
  select(distrito_norm, estoque_2023_m2total) |>
  mutate(Distrito = fix_rotulo(distrito_norm))


# ====================== 3) Juntar Estoque + Consumo ======================

tab_combo <- tab_estoque23tot |>
  left_join(tab_estoque23, by = c("distrito_norm", "Distrito")) |>
  tidyr::pivot_longer(
    cols = c(estoque_2023_m2, estoque_2023_m2total),
    names_to = "tipo",
    values_to = "valor"
  ) |>
  mutate(
    tipo = dplyr::recode(
      tipo,
      "estoque_2023_m2"  = "Estoque total 2023",
      "estoque_2023_m2total" = "Terreno Total"
    )
  )


# Soma total do estoque (como no seu gráfico original)
total_sel <- sum(tab_estoque23$estoque_2023_m2total, na.rm = TRUE)

# ====================== 4) GRÁFICO FINAL (duas barras por distrito) ======================

g_estoque_2023 <- ggplot(
  tab_combo,
  aes(x = valor, y = reorder(Distrito, valor), fill = tipo)
) +
  geom_col(position = position_dodge(width = 0.8)) +
  
  geom_text(
    aes(label = scales::label_number(big.mark=".", decimal.mark=",")(round(valor,0))),
    position = position_dodge(width = 0.8),
    hjust = -0.05, size = 3.5, color = "black"
  ) +
  
  scale_x_continuous(
    labels = scales::label_number(big.mark=".", decimal.mark=","),
    expand = expansion(mult = c(0, 0.10))
  ) +
  
  labs(
    title = "Estoque de solo (2023) e Terreno total por distrito",
    x = "m²",
    y = "Distrito",
    caption = "Fonte: IPTU (estoque EETU) — elaboração própria."
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.title.y = element_text(margin = margin(r = 10))
  ) +
  scale_fill_brewer(
    palette = "Paired",
    name = "Legenda",
    labels = c(
      "Estoque total 2023",
      "Terreno total"
    )
  ) +
  scale_fill_brewer(palette = "Paired")

plots_dir <- file.path(out_dir, "graficos_sintese")
if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)

ggsave(file.path(plots_dir, "estoque_2023_por_distrito_selecionadostotal.png"),
       g_estoque_2023, width = 10, height = 7, dpi = 300)

cat("✅ Gráfico salvo em:", file.path(plots_dir, "estoque_2023_por_distrito_selecionadostotal.png"), "\n")

# ====================== Consumo Futuro ======================

# --------- 0) Helper para normalizar nome de distrito ---------
norm_distrito <- function(x) {
  x |>
    tolower() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_squish()
}

anos_seq <- 2014:2024   # janela de anos usada no estudo (11 anos)

# --------- 1) Consumo anual 2014–2024 por distrito (completa anos faltantes com 0) ---------
# Espera: alvaras tem colunas: distrito_norm, ano, area_do_terreno
consumo_anual_distrito <- alvaras %>%
  dplyr::mutate(distrito_norm = norm_distrito(distrito_norm)) %>%
  dplyr::filter(ano >= 2014, ano <= 2024) %>%
  dplyr::group_by(distrito_norm, ano) %>%
  dplyr::summarise(
    consumo_m2 = sum(area_do_terreno, na.rm = TRUE),
    .groups = "drop"
  )

consumo_anual_full <- consumo_anual_distrito %>%
  dplyr::group_by(distrito_norm) %>%
  tidyr::complete(ano = anos_seq, fill = list(consumo_m2 = 0)) %>%
  dplyr::ungroup()

# --------- 2) Consumo acumulado 2014–2024 (11 anos) ---------
consumo_acum_2024 <- consumo_anual_full %>%
  dplyr::group_by(distrito_norm) %>%
  dplyr::summarise(
    consumo_acum_2024_m2 = sum(consumo_m2, na.rm = TRUE),
    .groups = "drop"
  )

# --------- 3) Estoque 2023 e saldo após o consumo 2014–2024 ---------
# Espera: estoque_2023_por_distrito tem colunas: distrito_norm, estoque_2023_m2
estoque_2023_clean <- estoque_2023_por_distrito %>%
  dplyr::mutate(distrito_norm = norm_distrito(distrito_norm)) %>%
  dplyr::select(distrito_norm, estoque_2023_m2)

saldo_pos_2024 <- estoque_2023_clean %>%
  dplyr::left_join(consumo_acum_2024, by = "distrito_norm") %>%
  dplyr::mutate(
    consumo_acum_2024_m2 = dplyr::coalesce(consumo_acum_2024_m2, 0),
    estoque_2023_m2      = dplyr::coalesce(estoque_2023_m2, 0),
    # saldo = estoque 2023 – tudo que foi consumido entre 2014 e 2024
    saldo_m2             = estoque_2023_m2 - consumo_acum_2024_m2
  )

# --------- 4) Taxa média anual (consumo acumulado / 11 anos) ---------
taxa_media_por_distrito <- consumo_acum_2024 %>%
  dplyr::mutate(
    taxa_media_anual_m2 = ifelse(
      consumo_acum_2024_m2 > 0,
      consumo_acum_2024_m2 / 11,
      NA_real_
    )
  )

# --------- 5) Projeção do ano de esgotamento (usando consumo médio) ---------
projecao_esgotamento_media <- saldo_pos_2024 %>%
  dplyr::left_join(taxa_media_por_distrito, by = "distrito_norm") %>%
  dplyr::mutate(
    anos_restantes = dplyr::case_when(
      saldo_m2 <= 0 ~ 0,  # já teria esgotado
      is.na(taxa_media_anual_m2) | taxa_media_anual_m2 <= 0 ~ Inf,
      TRUE ~ saldo_m2 / taxa_media_anual_m2
      # isso é exatamente: saldo / (consumo_acum/11)
    ),
    ano_esgotamento = dplyr::case_when(
      is.infinite(anos_restantes) ~ NA_real_,
      TRUE ~ 2024 + anos_restantes
    )
  ) %>%
  dplyr::select(
    distrito_norm,
    estoque_2023_m2,
    saldo_m2,
    taxa_media_anual_m2,
    anos_restantes,
    ano_esgotamento
  ) %>%
  dplyr::arrange(ano_esgotamento)

# --------- 6) Tabela amigável para relatório ---------
tabela_esgotamento_media <- projecao_esgotamento_media %>%
  dplyr::mutate(Distrito = stringr::str_to_title(distrito_norm)) %>%
  dplyr::transmute(
    Distrito,
    `Saldo após 2014–2024 (m²)`   = round(saldo_m2, 0),
    `Consumo médio anual (m²/ano)`= round(taxa_media_anual_m2, 0),
    `Anos restantes (aprox.)`     = ifelse(is.infinite(anos_restantes), NA, round(anos_restantes, 1)),
    `Ano estimado de esgotamento` = dplyr::if_else(
      is.na(ano_esgotamento),
      "Indeterminado",
      as.character(round(ano_esgotamento, 0))
    )
  )

print(tabela_esgotamento_media, n = Inf)


