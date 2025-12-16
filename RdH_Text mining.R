#MINERÍA DE TEXTO A LA COMUNIDAD RELATOS DE HORROR#


# Limpiar entorno (opcional)
rm(list = ls())

# Cargar todas las bibliotecas necesarias
library(tidyverse)
library(tidytext)
library(tm)
library(wordcloud)
library(ggraph)
library(igraph)
library(scico)
library(viridis)
library(syuzhet)
library(RColorBrewer)
library(stopwords)
library(stringi)
library(stringr)
library(scales)
library(tidyr)
library(widyr)
library(quanteda)
library(topicmodels)
library(treemapify)
library(rcartocolor)
library(xml2)
library(readxl)

# Configuración de tema para ggplot2
theme_set(theme_minimal())

# Carga de archivos

# Configurar ruta a los archivos relatos de horror.zip#
ruta_txt <- "C:/Users/TU RUTA DE UBICACION DE RELATOS ZIP"

# Verificar que la carpeta existe
if (!dir.exists(ruta_txt)) {
  stop("No existe: ", ruta_txt)
}

# Listar archivos
relatos_de_horror <- list.files(ruta_txt, pattern = "\\.txt$", full.names = FALSE)
cat("Se encontraron", length(relatos_de_horror), "archivos TXT\n")

# Función para leer archivos
leer_txt_seguro <- function(ruta) {
  intentos <- c("UTF-8", "Latin-1", "Windows-1252")
  for (enc in intentos) {
    texto <- tryCatch(
      readLines(ruta, encoding = enc, warn = FALSE),
      error = function(e) NULL
    )
    if (!is.null(texto)) {
      return(paste(texto, collapse = " "))
    }
  }
  warning("No se pudo leer el archivo: ", basename(ruta))
  return("")
}

# Cargar todos los textos
rdh_txt <- lapply(file.path(ruta_txt, relatos_de_horror), leer_txt_seguro)

# Verificar carga
cat("Primeros 3 archivos cargados:\n")
for (i in 1:min(3, length(relatos_de_horror))) {
  cat(i, "-", relatos_de_horror[i], ":", substr(rdh_txt[[i]], 1, 50), "...\n")
}

#doble enfoque de limpieza

# a) limpieza conservadora (para frases y sentimientos)
limpiar_conservador <- function(texto) {
  if (is.null(texto) || texto == "") return("")
  
  texto_limpio <- texto %>%
    # normalizar espacios primero
    str_squish() %>%
    
    # normalizar espacios alrededor de puntos
    str_replace_all("\\s+\\.\\s+", " . ") %>%
    
    # convertir a minúsculas
    tolower() %>%
    
    # mantener puntuación básica
    str_remove_all("[¡!¿?;]") %>%
    
    # manejar puntos
    str_replace_all("\\.\\.+", " . ") %>%  # Puntos suspensivos como punto simple
    str_replace_all("(?<=\\w)\\.(?=\\s|$)", " . ") %>%  # Punto final
    
    # quitar números
    str_remove_all("\\b\\d+\\b") %>%
    
    # espacios finales
    str_squish()
  
  return(texto_limpio)
}

# limpieza agresiva (para palabras y bigramas) 
limpiar_para_palabras <- function(texto) {
  if (is.null(texto) || texto == "") return("")
  
  texto_limpio <- texto %>%
    # Aplicar limpieza conservadora primero
    limpiar_conservador() %>%
    
    # Ahora quitar puntuación restante para palabras individuales
    str_remove_all("[,\\-]") %>%  # Quitar comas y guiones
    
    # Reemplazar puntos por espacio (para separar oraciones)
    str_replace_all("\\.", " ") %>%
    
    # Quitar stopwords en español
    {
      texto_temp <- .
      palabras <- unlist(strsplit(texto_temp, "\\s+"))
      palabras_sin_stop <- palabras[!palabras %in% stopwords("spanish")]
      paste(palabras_sin_stop, collapse = " ")
    } %>%
    
    str_squish()
  
  return(texto_limpio)
}

# Tokenización en frases

frases_rdh <- tibble(
  archivo = relatos_de_horror,
  texto_original = map_chr(rdh_txt, limpiar_conservador)
) %>%
  # Usar regex para dividir por puntos, signos de exclamación e interrogación
  unnest_tokens(
    frase,
    texto_original,
    token = "regex",
    pattern = "(?<=[.!?])\\s+",
    to_lower = FALSE
  ) %>%
  filter(nchar(frase) > 3) %>%
  # Agregar ID
  mutate(
    frase_id = paste0(archivo, "_", sprintf("%05d", 1:n()))
  ) %>%
  select(archivo, frase_id, frase)

# Tokenización por palabras.

palabras_rdh_base <- tibble(
  archivo = relatos_de_horror,      
  texto_original = rdh_txt          
) %>%
    mutate(
    texto_limpio = map_chr(texto_original, limpiar_para_palabras)
  ) %>%
    unnest_tokens(
    palabra,
    texto_limpio,
    token = "words",
    to_lower = TRUE,
    strip_punct = TRUE
  ) %>%
  # Separar palabras unidas por negaciones
  mutate(
    tiene_negacion = str_detect(palabra, "^(no|nunca|ni|sin)_"),
    palabra_base = ifelse(tiene_negacion, 
                          str_replace(palabra, "^(no|nunca|ni|sin)_", ""), 
                          palabra),
    prefijo = ifelse(tiene_negacion, 
                     str_extract(palabra, "^(no|nunca|ni|sin)"), 
                     NA_character_)
  ) %>%
  # Contar frecuencias
  count(archivo, palabra, palabra_base, prefijo, name = "n") %>%
  # Agregar columna de emocion para análisis posterior
  mutate(
    emocion = NA_character_,
    # Normalizar para SENTICON (sin acentos)
    palabra_normalizada = stringi::stri_trans_general(palabra, "Latin-ASCII")
  ) %>%
  select(archivo, palabra, palabra_normalizada, emocion, n, palabra_base, prefijo, everything())

# Análisis de frecuencias 
#crear corpus
rdh_db <- VCorpus(VectorSource(rdh_txt))

clean_text_agresiva <- function(corpus) {
  corpus %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation, preserve_intra_word_dashes = TRUE) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords("spanish")) %>%
    tm_map(stripWhitespace)
}

rdh_db <- clean_text_agresiva(rdh_db)

#Crear Term Document Matrix
rdh_TDM <- TermDocumentMatrix(rdh_db, control = list(bounds = list(global = c(1, Inf))))

frecuencias <- rowSums(as.matrix(rdh_TDM))

# Convertir a dataframe
frecuencias_reales <- data.frame(
  term = names(frecuencias),
  freq = frecuencias
) %>%
  arrange(desc(freq))

cat("Top 20 palabras más frecuentes:\n")
print(head(frecuencias_reales, 20))

#Gráfico de barras de frecuencias 
top_100 <- frecuencias_reales %>%
  slice(1:100)
  #filter(!term %in% c("ahí", "así")) descomentar filtro si es necesario eliminar palabras

ggplot(top_100, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "#4A2545", width = 0.85) +
  geom_text(aes(label = freq), hjust = -0.1, size = 3.5, color = "black", fontface = "plain") +
  coord_flip() +
  labs(
    title = "100 palabras más frecuentes",
    x = "Palabra",
    y = "Frecuencia"
  ) +
  theme(
    axis.text.y = element_text(size = 12, margin = margin(r = 15)),
    axis.text.x = element_text(size = 11),
    axis.title = element_text(size = 13, face = "plain"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "plain", margin = margin(b = 20)),
    plot.margin = margin(1.5, 2.5, 1.5, 1.5, "cm"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# Nube de palabras 
# Preparar datos 
rdh_d <- frecuencias_reales %>%
  filter(!term %in% c("ahí", "así")) %>%
  rename(word = term, freq = freq)

colores_rocket <- rocket(6)

set.seed(1234)
wordcloud(
  words = rdh_d$word,
  freq = rdh_d$freq,
  max.words = 175,
  random.order = FALSE,
  colors = colores_rocket,
  scale = c(3.5, 0.75)
)

# Frecuencias de temas y aparición en relatos. Barras dobles.

#Definir temas
temas_rdh <- list(
  aparición = c("aparición", "apariciones", "apareció", "aparece", "aparecer", "aparecerse", "aparecía",
                "aparecían", "aparecido", "aparecidos", "aparecen", "aparecieron", "apareciendo",
                "aparezca", "aparecidos"),
  presencias = c("presencia", "presencias"),
  brujas = c("bruja", "brujas", "brujo"),
  brujería = c("brujería", "embrujada", "embrujado", "embrujaron", "embrujos", "embrujarlo", "amarre"),
  nahuales = c("nahual", "nahuales", "nahuala", "nagual", "naguales"),
  llorona = c("llorona"),
  duendes = c("duende", "duendes", "duendecillo", "duendecillos", "duendecilla", "duendecito", "duenda"),
  sombras = c("sombra", "sombras"),
  exorcismo = c("exorcismo", "exorcismos", "exorcista", "exorcisada", "exorcizar", "exorcizamos"),
  posesión = c("poseída", "posesión", "poseído", "poseídas", "posesiones", "posesor"),
  demonios = c("demonio", "demonios", "demoniaca", "demoniaco", "demoniacos", "demoniacas", "endemoniado", "demonia", "diablo", "satanás"),
  ovni = c("ovni", "ovnis", "platillo", "extraterrestre", "extraterrestres", "aliens")
)

# Función para agrupar frecuencias
agrupar_frecuencias <- function(grupos, frecuencias_df) {
  resultados <- data.frame(grupo = character(), frecuencia = numeric(), stringsAsFactors = FALSE)
  
  for (nombre_grupo in names(grupos)) {
    palabras <- grupos[[nombre_grupo]]
    frecuencia_total <- sum(frecuencias_df$freq[frecuencias_df$term %in% palabras], na.rm = TRUE)
    resultados <- rbind(resultados, data.frame(grupo = nombre_grupo, frecuencia = frecuencia_total))
  }
  
  return(resultados)
}

#calcular frecuencias por tema
frecuencias_agrupadas <- agrupar_frecuencias(temas_rdh, frecuencias_reales)

#calcular archivos
docs_por_grupo <- sapply(names(temas_rdh), function(grupo) {
  palabras <- temas_rdh[[grupo]]
  
  conteo <- sum(sapply(rdh_txt, function(texto) {
    texto_min <- tolower(texto)
    any(sapply(palabras, function(palabra) {
      grepl(paste0("\\b", palabra, "\\b"), texto_min)
    }))
  }))
  
  return(conteo)
})

# Convertir a dataframe
docs_por_grupo_df <- data.frame(
  grupo = names(temas_rdh),
  documentos = docs_por_grupo,
  stringsAsFactors = FALSE
)

#graficar
datos_grafico <- frecuencias_agrupadas %>%
  left_join(docs_por_grupo_df, by = "grupo") %>%
  pivot_longer(cols = c(frecuencia, documentos), names_to = "tipo", values_to = "valor") %>%  # ← documentos, no pdfs
  mutate(
    tipo = factor(tipo, levels = c("frecuencia", "documentos")),  # ← documentos
    grupo = factor(grupo, levels = frecuencias_agrupadas %>% 
                     arrange(frecuencia) %>% 
                     pull(grupo))
  )

ggplot(datos_grafico, aes(x = grupo, y = valor, fill = tipo)) +
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.6),
    width = 0.6
  ) +
  geom_text(
    aes(label = valor, 
        hjust = ifelse(tipo == "frecuencia", 1.5, -0.5)),
    position = position_dodge(width = 0.6),
    size = 5,
    color = "black",
    vjust = -0.5
  ) +
  labs(
    title = "Frecuencia de temas y número de documentos",
    x = NULL,
    y = NULL
  ) +
  theme_void() +
  theme(
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold")
  ) +
  scale_fill_manual(
    values = c("frecuencia" = "#4A2545", "documentos" = "#3CB371"),  # ← documentos
    labels = c("Frecuencia total", "N° de documentos")  # ← documentos
  ) +
  scale_y_continuous(breaks = NULL) +
  coord_flip()

#Bigramas y redes semánticas

rdh_bigramas <- rdh_db %>%
  tidy() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stopwords("es") & !word2 %in% stopwords("es")) %>%
  count(word1, word2, sort = TRUE)

rdh_graph <- rdh_bigramas %>%
  filter(n > 100) %>%
  graph_from_data_frame()

# Calcular frecuencia de nodos
frecuencia_palabras_bg <- rdh_bigramas %>%
  pivot_longer(cols = c(word1, word2), names_to = "tipo", values_to = "word") %>%
  count(word, name = "freq")

V(rdh_graph)$freq <- frecuencia_palabras_bg$freq[match(V(rdh_graph)$name, frecuencia_palabras_bg$word)]
rdh_graph <- delete_vertices(rdh_graph, "si")

# gráfico de red
ggraph(rdh_graph, layout = "fr") +
  geom_edge_link(
    aes(edge_width = n, edge_alpha = n),
    color = "gray50"
  ) +
  geom_node_point(
    aes(size = freq, color = name %in% c("brujas", "nahuales", "demonios", "brujería", 
                                         "llorona", "duendes", "sombras", "exorcismo", 
                                         "posesión", "ovni", "aparición", "presencias")),
    alpha = 0.8
  ) +
  geom_node_text(
    aes(label = name),
    vjust = 1, hjust = 1,
    size = 5,
    color = "black"
  ) +
  scale_edge_width(range = c(0.5, 3)) +
  scale_edge_alpha(range = c(0.2, 1), guide = "none") +
  scale_size_continuous(range = c(3, 10)) +
  scale_color_manual(
    values = c("TRUE" = "#2F4F2F", "FALSE" = "#6B3FA0"),
    guide = "none"
  ) +
  theme_void()

# Filtrar componente mas conectado
componentes <- components(rdh_graph)
giant_component <- which.max(componentes$csize)
nodos_giant <- V(rdh_graph)[componentes$membership == giant_component]
rdh_giant <- induced_subgraph(rdh_graph, nodos_giant)

# Calcular grado de cada nodo
V(rdh_giant)$degree <- degree(rdh_giant)

# umbral: sólo bigramas que aparecen 100+ veces
threshold <- 100  

#graficar
ggraph(rdh_giant, layout = "kk") +
  geom_edge_link(
    aes(edge_width = n),
    color = "gray80",
    alpha = 0.6,
    show.legend = TRUE
  ) +
  geom_node_point(
    aes(size = degree, fill = degree),
    shape = 21,
    color = "white",
    alpha = 0.8,
    stroke = 0.5
  ) +
  geom_node_text(
    aes(label = name),
    size = 4,
    color = "black",
    repel = TRUE
  ) +
  scale_size_continuous(
    range = c(3, 15),
    name = "Grado",
    guide = guide_legend()
  ) +
  scale_fill_distiller(
    palette = "Purples",
    direction = -1,
    name = "Grado",
    guide = guide_legend()
  ) +
  scale_edge_width(
    range = c(0.5, 3),
    name = "Frecuencia (n)"
  ) +
  labs(
    title = "Red de Bigramas (Componente Más Grande)",
    subtitle = paste("Umbral de peso mínimo:", threshold)
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10)
  )

#Bigramas de TEMAS

#normalización:
normalizar_palabras <- function(bigramas, temas_rdh) {
  bigramas %>%
    mutate(
      word1 = case_when(
        word1 %in% temas_rdh$aparición ~ "aparición",
        word1 %in% temas_rdh$presencias ~ "presencias",
        word1 %in% temas_rdh$brujas ~ "brujas",
        word1 %in% temas_rdh$brujería ~ "brujería",
        word1 %in% temas_rdh$nahuales ~ "nahuales",
        word1 %in% temas_rdh$llorona ~ "llorona",
        word1 %in% temas_rdh$duendes ~ "duendes",
        word1 %in% temas_rdh$sombras ~ "sombras",
        word1 %in% temas_rdh$exorcismo ~ "exorcismo",
        word1 %in% temas_rdh$posesión ~ "posesión",
        word1 %in% temas_rdh$demonios ~ "demonios",
        word1 %in% temas_rdh$ovni ~ "ovni",
        TRUE ~ word1
      ),
      word2 = case_when(
        word2 %in% temas_rdh$aparición ~ "aparición",
        word2 %in% temas_rdh$presencias ~ "presencias",
        word2 %in% temas_rdh$brujas ~ "brujas",
        word2 %in% temas_rdh$brujería ~ "brujería",
        word2 %in% temas_rdh$nahuales ~ "nahuales",
        word2 %in% temas_rdh$llorona ~ "llorona",
        word2 %in% temas_rdh$duendes ~ "duendes",
        word2 %in% temas_rdh$sombras ~ "sombras",
        word2 %in% temas_rdh$exorcismo ~ "exorcismo",
        word2 %in% temas_rdh$posesión ~ "posesión",
        word2 %in% temas_rdh$demonios ~ "demonios",
        word2 %in% temas_rdh$ovni ~ "ovni",
        TRUE ~ word2
      )
    )
}

bigramas_normalizados <- normalizar_palabras(rdh_bigramas, temas_rdh)

# umbrales específicos
bigramas_filtrados <- bigramas_normalizados %>%
  filter(
    (word1 %in% unlist(temas_rdh) | word2 %in% unlist(temas_rdh)) &
      n > 5  # Umbral general
  ) %>%
  filter(
    !(word1 %in% temas_rdh$sombras & word2 %in% temas_rdh$sombras & n <= 25)
  ) %>%
  filter(
    !(word1 %in% temas_rdh$aparición & word2 %in% temas_rdh$aparición & n <= 25)
  )

# crear grafo filtrado
rdh_graph_filtrado <- bigramas_filtrados %>%
  graph_from_data_frame(directed = FALSE)

# calcular frecuencias
frecuencia_palabras_filt <- bigramas_filtrados %>%
  pivot_longer(cols = c(word1, word2), names_to = "tipo", values_to = "word") %>%
  count(word, name = "freq")

V(rdh_graph_filtrado)$freq <- frecuencia_palabras_filt$freq[
  match(V(rdh_graph_filtrado)$name, frecuencia_palabras_filt$word)
]


# Eliminar aristas de "sombras" con n <= 10 
aristas_a_eliminar <- E(rdh_graph_filtrado)[
  (
    (ends(rdh_graph_filtrado, E(rdh_graph_filtrado))[, 1] == "sombras" |
       ends(rdh_graph_filtrado, E(rdh_graph_filtrado))[, 2] == "sombras"
    ) & E(rdh_graph_filtrado)$n <= 10
  ) 
]

rdh_graph_filtrado <- delete_edges(rdh_graph_filtrado, aristas_a_eliminar)
rdh_graph_filtrado <- delete_vertices(rdh_graph_filtrado, degree(rdh_graph_filtrado) == 0)

# Gráfico de red de temas
ggraph(rdh_graph_filtrado, layout = "fr") +
  geom_edge_link(
    aes(edge_width = n, edge_alpha = n),
    color = "gray50"
  ) +
  geom_node_point(
    aes(size = freq, color = freq),
    alpha = 0.8
  ) +
  geom_node_text(
    aes(label = name),
    vjust = -1,
    hjust = 1,
    size = 4,
    color = "black",
    alpha = 0.8
  ) +
  scale_edge_width(range = c(0.5, 3)) +
  scale_edge_alpha(range = c(0.2, 1)) +
  scale_size_continuous(range = c(3, 10)) +
  scale_color_gradientn(
    colours = rev(brewer.pal(9, "Purples")[2:9]),
    name = "Frecuencia",
    trans = "sqrt"
  ) +
  theme_void() +
  theme(
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 14, face = "bold")
  ) +
  guides(
    color = guide_colorbar(
      override.aes = list(size = 5),
      title.position = "top",
      title.hjust = 0.5
    ),
    size = "none"
  )

# Bigramas de sentidos 

sentidos_rdh <- list(
  ver = c("ver", "vío", "veía", "viendo", "vimos", "vieron", "vista", "visión", "observo", 
          "observa", "observaba", "observar", "observé", "observando", "observó", "observamos"),
  sentir = c("sentí", "sentía", "sentir", "sentimos", "sentían", "sintió", "sintiendo",
             "sintieron", "sintiera"),
  escuchar = c("escuchar", "escuche", "escuchaba", "escuchó", "escuchado", "escuchamos",
               "escuchaban", "escuchando", "escucharon", "escucho", "escucha", "escuchan",
               "escucharse", "escucharlo", "oído", "oír", "oía", "oían", "oímos"),
  oler = c("olía", "olí", "olían", "olor", "olores", "olfato", "olfatear"),
  sabor = c("sabor", "saborean", "saboreando")
)

# normalización
normalizar_sentidos <- function(bigramas, sentidos_rdh) {
  bigramas %>%
    mutate(
      word1 = case_when(
        word1 %in% sentidos_rdh$ver ~ "ver",
        word1 %in% sentidos_rdh$sentir ~ "sentir",
        word1 %in% sentidos_rdh$escuchar ~ "escuchar",
        word1 %in% sentidos_rdh$oler ~ "oler",
        word1 %in% sentidos_rdh$sabor ~ "sabor",
        TRUE ~ word1
      ),
      word2 = case_when(
        word2 %in% sentidos_rdh$ver ~ "ver",
        word2 %in% sentidos_rdh$sentir ~ "sentir",
        word2 %in% sentidos_rdh$escuchar ~ "escuchar",
        word2 %in% sentidos_rdh$oler ~ "oler",
        word2 %in% sentidos_rdh$sabor ~ "sabor",
        TRUE ~ word2
      )
    )
}

bigramas_sentidos_normalizados <- normalizar_sentidos(rdh_bigramas, sentidos_rdh)

#umbrales
bigramas_filtrados_sentidos <- bigramas_sentidos_normalizados %>%
  filter(
    (word1 %in% c("ver", "sentir", "escuchar", "oler", "sabor") | 
       word2 %in% c("ver", "sentir", "escuchar", "oler", "sabor")) &
      ifelse(word1 == "ver" | word2 == "ver", n > 60,
             ifelse(word1 == "escuchar" | word2 == "escuchar", n > 30,
                    ifelse(word1 == "sentir" | word2 == "sentir", n > 30,
                           ifelse(word1 == "oler" | word2 == "oler", n > 5,
                                  ifelse(word1 == "sabor" | word2 == "sabor", n >= 1, TRUE)))))
  )

# Crear grafo de sentidos
rdh_graph_filtrado_sentidos <- bigramas_filtrados_sentidos %>%
  graph_from_data_frame(directed = FALSE)

# Calcular frecuencias
frecuencia_palabras_sentidos <- bigramas_filtrados_sentidos %>%
  pivot_longer(cols = c(word1, word2), names_to = "tipo", values_to = "word") %>%
  count(word, name = "freq")

V(rdh_graph_filtrado_sentidos)$freq <- frecuencia_palabras_sentidos$freq[
  match(V(rdh_graph_filtrado_sentidos)$name, frecuencia_palabras_sentidos$word)
]

# Eliminar palabras no deseadas (como en tu ajuste manual)
rdh_graph_filtrado_sentidos <- delete_vertices(
  rdh_graph_filtrado_sentidos, 
  c("si", "repente", "dijo", "nariz", "querés", "tan")
)

# Gráfico de sentidos
ggraph(rdh_graph_filtrado_sentidos, layout = "kk") +
  geom_edge_link(
    aes(edge_width = n, edge_alpha = n),
    color = "gray50"
  ) +
  geom_node_point(
    aes(size = freq, 
        color = name %in% c("ver", "sentir", "escuchar", "oler", "sabor")),
    alpha = 0.8
  ) +
  geom_node_text(
    aes(label = name),
    vjust = -1, hjust = 1, 
    size = 6, 
    color = "black", 
    alpha = 0.8
  ) +
  scale_edge_width(range = c(0.5, 3)) +
  scale_edge_alpha(range = c(0.2, 1)) +
  scale_size_continuous(range = c(3, 10)) +
  scale_color_manual(
    values = c(
      "TRUE" = "#2F4F2F",   # VERDE para sentidos
      "FALSE" = "#6B3FA0"   # MORADO para el resto
    ),
    guide = "none"
  ) +
  theme_void()

#Análisis de sentiminetos

# cargar SENTICON (adjuto en repositorio)
senticon_path <- "C:/Users/CEPYGICT/Documents/lexico_senticon.es.xml"

# Función para cargar SENTICON
cargar_senticon <- function(path) {
  tryCatch({
    xml <- read_xml(path)
    
    # Extraer todos los lemmas
    lemmas <- xml_find_all(xml, "//layer/positive/lemma | //layer/negative/lemma")
    
    senticon_df <- map_df(lemmas, ~{
      palabra <- xml_text(.x) %>% 
        str_trim() %>% 
        str_to_lower()
      
      polaridad <- xml_attr(.x, "pol") %>% 
        as.numeric()
      
      es_positivo <- xml_path(.x) %>% str_detect("/positive/")
      
      data.frame(
        palabra,
        value = ifelse(es_positivo, polaridad, -polaridad),
        stringsAsFactors = FALSE
      )
    }) %>%
      filter(!is.na(value)) %>%
      mutate(
        palabra_normalizada = stringi::stri_trans_general(palabra, "Latin-ASCII"),
        sentiment = case_when(
          value > 0 ~ "positive",
          value < 0 ~ "negative",
          TRUE ~ "neutral"
        )
      )
    
    return(senticon_df)
    
  }, error = function(e) {
    message("Error al cargar SENTICON: ", e$message)
    return(data.frame(palabra=character(), value=numeric(), 
                      palabra_normalizada=character(), sentiment=character()))
  })
}

# Cargar léxico SENTICON
senticon_lexicon <- cargar_senticon(senticon_path)

# Cargar léxicos estándar
afinn_lex <- get_sentiment_dictionary("afinn", language = "spanish")
bing_lex <- get_sentiment_dictionary("bing", language = "spanish") 
nrc_lex <- get_sentiment_dictionary("nrc", language = "spanish")


# Verificar carga
cat("Léxicos cargados:\n")
cat("- AFINN: ", nrow(afinn_lex), " filas\n", sep = "")
cat("- Bing:  ", nrow(bing_lex), " filas\n", sep = "")
cat("- NRC:   ", nrow(nrc_lex), " filas\n", sep = "")
cat("- SENTICON: ", nrow(senticon_lexicon), " filas\n", sep = "")

# Definir qué léxicos probar
lexicos_a_probar <- c("afinn", "bing", "nrc", "senticon")


# Función para análisis por léxicos

analizar_por_lexico_corregido <- function(lexico) {
  
  if (lexico == "afinn") {
    resultado <- palabras_rdh_base %>%
      inner_join(afinn_lex, by = c("palabra" = "word")) %>%  # ← palabra = word
      group_by(archivo) %>%
      summarise(sentimiento = sum(value, na.rm = TRUE)) %>%
      mutate(metodo = "AFINN")
    return(resultado)
  }
  
  if (lexico == "bing") {
    resultado <- palabras_rdh_base %>%
      inner_join(bing_lex, by = c("palabra" = "word")) %>%  # ← palabra = word
      group_by(archivo) %>%
      summarise(
        positive = sum(value > 0, na.rm = TRUE),
        negative = sum(value < 0, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(
        sentimiento = positive - negative,
        metodo = "Bing"
      ) %>%
      select(archivo, sentimiento, metodo)
    return(resultado)
  }
  
  if (lexico == "nrc") {
    resultado <- palabras_rdh_base %>%
      inner_join(nrc_lex, by = c("palabra" = "word")) %>%  # ← palabra = word
      filter(sentiment %in% c("positive", "negative")) %>%
      group_by(archivo, sentiment) %>%
      summarise(n = n(), .groups = 'drop') %>%
      pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
      mutate(
        sentimiento = positive - negative,
        metodo = "NRC"
      ) %>%
      select(archivo, sentimiento, metodo)
    return(resultado)
  }
  
  if (lexico == "senticon") {
    if (exists("senticon_lexicon") && nrow(senticon_lexicon) > 0) {
      resultado <- palabras_rdh_base %>%
        inner_join(senticon_lexicon, by = "palabra_normalizada") %>%  # ← ¡OJO! Se une por palabra_normalizada
        group_by(archivo) %>%
        summarise(sentimiento = sum(value, na.rm = TRUE)) %>%
        mutate(metodo = "SENTICON")
      return(resultado)
    } else {
      return(data.frame(archivo = character(), sentimiento = numeric(), metodo = character()))
    }
  }
}


comparacion_lexicos <- map_df(lexicos_a_probar, analizar_por_lexico_corregido)

comparacion_lexicos <- map_df(c("afinn", "bing", "nrc", "senticon"), 
                              analizar_por_lexico_corregido)

# Gráfico de comparación de los 4 léxicos
ggplot(comparacion_lexicos, aes(x = archivo, y = sentimiento, fill = metodo)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  scale_fill_manual(values = c(
    "AFINN" = "#4DAF4A",    # VERDE
    "Bing" = "#377EB8",     # AZUL
    "NRC" = "#E41A1C",      # ROJO
    "SENTICON" = "#984EA3"  # MORADO
  )) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6)
  ) +
  labs(
    title = "Comparación de 4 Léxicos de Sentimientos",
    subtitle = "Análisis por documento",
    x = "Documento",
    y = "Puntaje de Sentimiento",
    fill = "Método"
  )



# Resumen global comparativo
resumen_global <- comparacion_lexicos %>%
  group_by(metodo) %>%
  summarise(
    sentimiento_promedio = mean(sentimiento, na.rm = TRUE),
    documentos_positivos = sum(sentimiento > 0) / n() * 100,
    documentos_negativos = sum(sentimiento < 0) / n() * 100
  )

print(resumen_global)

# Gráfico de resumen global
ggplot(resumen_global, aes(x = metodo, y = sentimiento_promedio, fill = metodo)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(sentimiento_promedio, 2)), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c(
    "AFINN" = "#4DAF4A", 
    "Bing" = "#377EB8", 
    "NRC" = "#E41A1C", 
    "SENTICON" = "#984EA3"
  )) +
  theme_minimal() +
  labs(
    title = "Comparación Global de 4 Léxicos de Sentimientos",
    subtitle = "Puntaje promedio de sentimiento por documento",
    x = "Método",
    y = "Sentimiento Promedio"
  )


# Histograma de distribución de sentimientos (NRC por frases)
sentimientos_por_frase <- get_sentiment(frases_rdh$frase, method = "nrc", language = "spanish")

# Crear dataframe 
frases_rdh_df <- frases_rdh %>%
  mutate(sentimiento = sentimientos_por_frase)

ggplot(frases_rdh_df, aes(x = sentimiento, 
                          fill = cut(sentimiento, 
                                     breaks = c(-Inf, 0, Inf),
                                     labels = c("Negativo", "Positivo")))) +
  geom_histogram(binwidth = 0.5, color = "black", alpha = 0.8) +
  scale_fill_manual(values = c(
    "Negativo" = "tomato3",    # ROJO oscuro
    "Positivo" = "darkgreen"   # VERDE oscuro
  )) +
  labs(
    title = "Distribución de Sentimientos por Frases (NRC)",
    subtitle = "Excluyendo frases neutrales (sentimiento = 0)",
    x = "Valor de Sentimiento",
    y = "Número de Frases",
    fill = "Polaridad"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40")
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", size = 0.8)

#Gráfico de emociones dicretas
emociones_palabras <- palabras_rdh_base %>%
  inner_join(nrc_lex, by = c("palabra" = "word"))

# (excluir positive/negative)
emociones_discretas <- emociones_palabras %>%
  filter(!sentiment %in% c("positive", "negative")) %>%
  count(sentiment, name = "frecuencia") %>%
  mutate(
    emocion_es = recode(sentiment,
                        "anger" = "Ira",
                        "anticipation" = "Anticipación", 
                        "disgust" = "Asco",
                        "fear" = "Miedo",
                        "joy" = "Alegría",
                        "sadness" = "Tristeza", 
                        "surprise" = "Sorpresa",
                        "trust" = "Confianza"
    ),
    tipo = case_when(
      sentiment %in% c("joy", "trust", "surprise", "anticipation") ~ "positivo",
      sentiment %in% c("anger", "fear", "sadness", "disgust") ~ "negativo"
    )
  ) %>%
  arrange(desc(frecuencia))

# Gráfico de emociones discretas por PALABRAS
ggplot(emociones_discretas, 
       aes(x = reorder(emocion_es, -frecuencia),
           y = frecuencia,
           fill = tipo)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = frecuencia), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c(
    "positivo" = "aquamarine3", 
    "negativo" = "darkred"
  )) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  ) +
  labs(
    title = "Frecuencia de emociones discretas por palabras",
    subtitle = "Análisis NRC (excluyendo positivo/negativo genérico)",
    x = "Emoción",
    y = "Número de palabras",
    fill = "Tipo de emoción"
  )


# Treemaps

#función

asignar_palabras_unicas <- function(datos_treemap_base) {
  # Para cada palabra, encontrar la emoción donde es más frecuente
  palabras_dominantes <- datos_treemap_base %>%
    group_by(palabra) %>%
    mutate(
      max_frecuencia = max(n),
      es_dominante = (n == max_frecuencia)
    ) %>%
    # Si hay empates, quedarse con la primera alfabéticamente
    arrange(emocion_es) %>%
    filter(es_dominante) %>%
    slice(1) %>%
    ungroup() %>%
    select(-max_frecuencia, -es_dominante)
  
  return(palabras_dominantes)
}


if (!exists("emociones_por_palabra")) {
  emociones_por_palabra <- palabras_rdh_base %>%
    # eliminar cualquier columna 'emocion' o 'sentiment' que ya exista
    select(-any_of(c("emocion", "sentiment"))) %>%
    # hacer el join
    inner_join(nrc_lex, 
               by = c("palabra" = "word"),
               relationship = "many-to-many") %>%
    # filtrar primero (más eficiente)
    filter(sentiment %in% c("anger", "anticipation", "disgust", "fear", 
                            "joy", "sadness", "surprise", "trust")) %>%
    # renombrar (ahora no hay conflicto)
    rename(emocion = sentiment)
} else {
  # Si ya existe, forzar recreación limpia
  cat("\nLa variable 'emociones_por_palabra' ya existe. Recreando...\n")
  rm(emociones_por_palabra)
  
  emociones_por_palabra <- palabras_rdh_base %>%
    select(-any_of(c("emocion", "sentiment"))) %>%
    inner_join(nrc_lex, 
               by = c("palabra" = "word"),
               relationship = "many-to-many") %>%
    filter(sentiment %in% c("anger", "anticipation", "disgust", "fear", 
                            "joy", "sadness", "surprise", "trust")) %>%
    rename(emocion = sentiment)
}


datos_treemap_base <- emociones_por_palabra %>%
  count(palabra, emocion, name = "n") %>%
  mutate(
    emocion_es = recode(emocion,
                        "anger" = "ira", "anticipation" = "anticipación", "disgust" = "asco",
                        "fear" = "miedo", "joy" = "alegría", "sadness" = "tristeza",
                        "surprise" = "sorpresa", "trust" = "confianza"
    )
  )


# Asignar palabras únicas
palabras_unicas_t1 <- asignar_palabras_unicas(datos_treemap_base)

# Lista de palabras a excluir
palabras_excluir <- c("cuenta", "ver", "miedo", "mamá", "madre", "padre", 
                      "abuelo", "grande", "señor", "tiempo", "tía", "hermano", 
                      "puerta", "hidalgo", "muy", "nada", "ay", "así", "ahí",
                      "puesto", "tan", "si", "dijo")  

# Aplicar ajustes manuales específicos
palabras_ajustadas_t1 <- palabras_unicas_t1 %>%
  # Ajustes manuales (como en tu código)
  mutate(
    emocion = case_when(
      palabra == "sangre" ~ "disgust",
      palabra == "demonio" ~ "fear", 
      palabra == "diablo" ~ "fear",
      TRUE ~ emocion
    ),
    emocion_es = recode(emocion,
                        "anger" = "ira", "anticipation" = "anticipación", "disgust" = "asco",
                        "fear" = "miedo", "joy" = "alegría", "sadness" = "tristeza",
                        "surprise" = "sorpresa", "trust" = "confianza"
    )
  ) %>%
  # Filtrar palabras excluidas
  filter(!palabra %in% palabras_excluir)

# Tomar top 10 por emoción (ya sin duplicados)
datos_treemap1 <- palabras_ajustadas_t1 %>%
  group_by(emocion_es) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  # Preparar etiquetas y totales
  mutate(
    etiqueta = paste0(palabra, "\n", n)
  ) %>%
  group_by(emocion_es) %>%
  mutate(total_emocion = sum(n)) %>%
  ungroup()

# Verificar que no hay duplicados
cat("=== VERIFICACIÓN TREEMAP 1 (Sin duplicados) ===\n")
conteo_t1 <- datos_treemap1 %>%
  group_by(emocion_es) %>%
  summarise(n_palabras = n(), total = sum(n))

print(conteo_t1)

# Verificar duplicados
duplicados_t1 <- datos_treemap1 %>%
  group_by(palabra) %>%
  filter(n() > 1) %>%
  select(palabra, emocion_es)

if (nrow(duplicados_t1) > 0) {
  cat("\n¡DUPLICADOS ENCONTRADOS!:\n")
  print(duplicados_t1)
} else {
  cat("No hay palabras duplicadas entre emociones\n")
}

# Gráfico Treemap 1 
ggplot(datos_treemap1, aes(
  area = n, 
  fill = emocion_es,
  label = etiqueta,
  subgroup = emocion_es
)) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "white", size = 3) +
  geom_treemap_subgroup_text(
    aes(label = paste0(emocion_es, "\nTotal: ", total_emocion)),
    place = "bottomleft",
    colour = "white",
    fontface = "bold",
    size = 13,
    alpha = 0.8
  ) +
  geom_treemap_text(
    aes(colour = ifelse(emocion_es %in% c("tristeza", "miedo", "asco"), "white", "black")),
    size = 12,
    place = "topleft",
    reflow = TRUE,
    padding.x = unit(2, "mm"),
    padding.y = unit(2, "mm"),
    min.size = 6,
    show.legend = FALSE
  ) +
  scale_colour_identity() +
  scale_fill_manual(values = c(
    "tristeza" = viridis(8)[1],
    "miedo" = viridis(8)[2],  
    "asco" = viridis(8)[3],
    "ira" = "#e76f51",         # ROJO especial para ira (NO verde/morado)
    "anticipación" = viridis(8)[5],
    "confianza" = viridis(8)[6],
    "sorpresa" = viridis(8)[7],
    "alegría" = viridis(8)[8]
  )) +
  labs(
    title = "Top 10 Palabras por Emoción",
    subtitle = "Palabras asignadas a su emoción dominante (sin duplicados)",
    caption = "Todas las palabras son únicas por emoción"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray40"),
    plot.caption = element_text(hjust = 0.9, size = 9, color = "gray50"),
    legend.position = "none"
  )

# treemap frecuencias medias

# calcular percentiles
percentiles <- quantile(datos_treemap_base$n, probs = seq(0, 1, 0.1))
umbral_bajo <- quantile(datos_treemap_base$n, 0.20)
umbral_alto <- quantile(datos_treemap_base$n, 0.80)

# filtrar por percentiles primero
datos_filtrados_percentil <- datos_treemap_base %>%
  filter(n >= umbral_bajo & n <= umbral_alto)

# asignar palabras únicas al conjunto filtrado
palabras_unicas_t2 <- asignar_palabras_unicas(datos_filtrados_percentil)

# aplicar ajustes manuales y exclusiones
datos_treemap2 <- palabras_unicas_t2 %>%
  mutate(
    emocion = case_when(
      palabra == "sangre" ~ "disgust",
      palabra == "demonio" ~ "fear", 
      palabra == "diablo" ~ "fear",
      TRUE ~ emocion
    ),
    emocion_es = recode(emocion,
                        "anger" = "ira", "anticipation" = "anticipación", "disgust" = "asco",
                        "fear" = "miedo", "joy" = "alegría", "sadness" = "tristeza",
                        "surprise" = "sorpresa", "trust" = "confianza"
    )
  ) %>%
  # Excluir palabras (ajustes manuales)
  filter(!palabra %in% c("miedo", "grande", "señor", "ver", "madre", "mamá", 
                         "padre", "abuelo", "tiempo", "hidalgo", "puerta")) %>%
  # Top 10 por emoción
  group_by(emocion_es) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  # Preparar etiquetas
  mutate(
    etiqueta = paste0(palabra, "\n", n),
    total_emocion = sum(n)
  ) %>%
  group_by(emocion_es) %>%
  mutate(total_emocion = sum(n)) %>%
  ungroup()

# verificar treemap 2
cat("\n=== VERIFICACIÓN TREEMAP 2 (Sin duplicados) ===\n")
conteo_t2 <- datos_treemap2 %>%
  group_by(emocion_es) %>%
  summarise(
    n_palabras = n(),
    total = sum(n),
    min_freq = min(n),
    max_freq = max(n)
  )

print(conteo_t2)

# Verificar duplicados
duplicados_t2 <- datos_treemap2 %>%
  group_by(palabra) %>%
  filter(n() > 1)

if (nrow(duplicados_t2) > 0) {
  cat("\n DUPLICADOS EN TREEMAP 2:\n")
  print(duplicados_t2 %>% select(palabra, emocion_es, n))
} else {
  cat(" No hay palabras duplicadas en Treemap 2\n")
}

# gráfico Treemap 2
ggplot(datos_treemap2, aes(
  area = n, 
  fill = emocion_es,
  label = etiqueta,
  subgroup = emocion_es
)) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "white", size = 2) +
  geom_treemap_subgroup_text(
    aes(label = paste0(emocion_es, "\nTotal: ", total_emocion)),
    place = "bottomleft",
    colour = "white",
    size = 14,
    alpha = 0.8
  ) +
  geom_treemap_text(
    aes(colour = ifelse(emocion_es %in% c("tristeza", "miedo", "asco"), "white", "black")),
    size = 12,
    place = "topleft",
    reflow = TRUE,
    padding.x = unit(2, "mm"),
    padding.y = unit(2, "mm"),
    min.size = 6,
    show.legend = FALSE
  ) +
  scale_colour_identity() + 
  scale_fill_manual(values = c(
    "tristeza" = viridis(8)[1],
    "miedo" = viridis(8)[2],  
    "asco" = viridis(8)[3],
    "ira" = "#e76f51",         # ROJO para ira (viridis para las demás)
    "anticipación" = viridis(8)[5],
    "confianza" = viridis(8)[6],
    "sorpresa" = viridis(8)[7],
    "alegría" = viridis(8)[8]
  )) +
  labs(
    title = "Top Palabras por Emoción. Frecuencias medias",
    subtitle = paste("Percentiles 20-80 (", round(umbral_bajo, 1), "-", round(umbral_alto, 1), "apariciones)"),
    caption = "Palabras únicas por emoción - Sin duplicados"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(hjust = 0.5, size = 11, margin = margin(b = 10), color = "gray40"),
    plot.caption = element_text(hjust = 0.9, size = 9, margin = margin(t = 5), color = "gray50"),
    legend.position = "none",
    plot.margin = margin(1, 1, 1, 1, "cm")
  )

# Análsiis de emociones por figuras familiares

categorias_familiares <- list(
  madre = c("mamá", "mami", "mamita", "mamá", "madre", "madres"),
  padre = c("papá", "padre"),
  abuela = c("abuela", "abuelita", "abuelas"),
  abuelo = c("abuelo", "abuelito"),
  bisabuela = c("bisabuela", "tatarabuela"),
  bisabuelo = c("bisabuelo", "tatarabuelo"),
  hermana = c("hermanas", "hermana", "hermanita", "hermanastra"),
  hermano = c("hermano", "hermanito"),
  tío = c("tío", "tíos"),
  tía = c("tías", "tía")
)

# identificar frases con familiares
frases_con_familiares <- frases_rdh %>%
  mutate(
    categoria = case_when(
      # Verificar que la categoría exista antes de usar paste()
      "madre" %in% names(categorias_familiares) & 
        str_detect(tolower(frase), paste(categorias_familiares$madre, collapse = "|")) ~ "madre",
      
      "padre" %in% names(categorias_familiares) & 
        str_detect(tolower(frase), paste(categorias_familiares$padre, collapse = "|")) ~ "padre",
      
      "abuela" %in% names(categorias_familiares) & 
        str_detect(tolower(frase), paste(categorias_familiares$abuela, collapse = "|")) ~ "abuela",
      
      "abuelo" %in% names(categorias_familiares) & 
        str_detect(tolower(frase), paste(categorias_familiares$abuelo, collapse = "|")) ~ "abuelo",
      
      "bisabuela" %in% names(categorias_familiares) & 
        str_detect(tolower(frase), paste(categorias_familiares$bisabuela, collapse = "|")) ~ "bisabuela",
      
      "bisabuelo" %in% names(categorias_familiares) & 
        str_detect(tolower(frase), paste(categorias_familiares$bisabuelo, collapse = "|")) ~ "bisabuelo",
      
      "hermana" %in% names(categorias_familiares) & 
        str_detect(tolower(frase), paste(categorias_familiares$hermana, collapse = "|")) ~ "hermana",
      
      "hermano" %in% names(categorias_familiares) & 
        str_detect(tolower(frase), paste(categorias_familiares$hermano, collapse = "|")) ~ "hermano",
      
      "tío" %in% names(categorias_familiares) & 
        str_detect(tolower(frase), paste(categorias_familiares$tío, collapse = "|")) ~ "tío",
      
      "tía" %in% names(categorias_familiares) & 
        str_detect(tolower(frase), paste(categorias_familiares$tía, collapse = "|")) ~ "tía",
      
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(categoria))

# verificar  
cat("=== FRASES CON FAMILIARES ENCONTRADAS ===\n")
cat("Total de frases:", nrow(frases_con_familiares), "\n")
cat("Distribución por categoría:\n")
print(frases_con_familiares %>% count(categoria, sort = TRUE))

palabras_familiares <- frases_con_familiares %>%
  unnest_tokens(palabra, frase) %>%
  # Unir con NRC ANTES de filtrar, para evitar duplicados
  inner_join(
    nrc_lex %>% 
      filter(sentiment %in% c("anger", "anticipation", "disgust", "fear", 
                              "joy", "sadness", "surprise", "trust")) %>%
      distinct(word, sentiment),  
    by = c("palabra" = "word")
  ) %>%
  rename(emocion = sentiment) %>%
  group_by(categoria, emocion, palabra) %>%
  summarise(
    n_apariciones = n(),
    .groups = 'drop'
  )

#  resumen_final 
resumen_final <- palabras_familiares %>%
  group_by(categoria, emocion) %>%
  summarise(
    n_palabras = n_distinct(palabra),  # Palabras EMOCIONALES únicas
    total_apariciones = sum(n_apariciones),  # Total de apariciones de esas palabras
    .groups = 'drop'
  ) %>%
  mutate(
    emocion_es = recode(emocion,
                        "anger" = "ira", "anticipation" = "anticipación", "disgust" = "asco",
                        "fear" = "miedo", "joy" = "alegría", "sadness" = "tristeza",
                        "surprise" = "sorpresa", "trust" = "confianza"
    )
  )


# Densidad emocional

frecuencias_totales <- frases_con_familiares %>%
  group_by(categoria) %>%
  summarise(total_frases = n())

diversidad_relativa <- resumen_final %>%
  left_join(frecuencias_totales, by = "categoria") %>%
  mutate(
    densidad_emocional = n_palabras / total_frases  # n_palabras EMOCIONALES únicas / total_frases
  )

# Verificar
cat("\n=== DENSIDAD EMOCIONAL ===\n")
print(head(diversidad_relativa))

tabla_final <- palabras_familiares %>%
  group_by(categoria) %>%
  summarise(
    `Palabras Únicas` = n_distinct(palabra),
    .groups = 'drop'
  ) %>%
  left_join(
    frases_con_familiares %>%
      group_by(categoria) %>%
      summarise(`Apariciones Totales` = n()),
    by = "categoria"
  ) %>%
  mutate(
    `Densidad Emocional` = round(`Palabras Únicas` / `Apariciones Totales`, 3),
    `Frecuencia Promedio` = round(`Apariciones Totales` / `Palabras Únicas`, 2)
  ) %>%
  select(
    `Figura Familiar` = categoria,
    `Palabras Únicas`,
    `Apariciones Totales`,
    `Densidad Emocional`,
    `Frecuencia Promedio`
  ) %>%
  arrange(desc(`Densidad Emocional`))

print(tabla_final)

#graficar

ggplot(diversidad_relativa, aes(x = reorder(categoria, densidad_emocional), 
                                y = densidad_emocional)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Densidad Emocional por Figura Familiar",
    subtitle = "Palabras emocionales únicas por cada frase con el familiar",
    x = "Figura Familiar", 
    y = "Densidad Emocional",
    caption = "Valores más altos = mayor diversidad léxica emocional relativa"
  ) +
  theme_minimal()

# calcular frecuencias de palabras
palabras_emocionales_freq <- palabras_familiares %>%
  rename(n = n_apariciones)

# calcular percentiles por categoría
percentiles_familia <- palabras_emocionales_freq %>%
  group_by(categoria) %>%
  summarise(
    p20 = quantile(n, 0.20, na.rm = TRUE),
    p50 = quantile(n, 0.50, na.rm = TRUE), 
    p80 = quantile(n, 0.80, na.rm = TRUE),
    p85 = quantile(n, 0.85, na.rm = TRUE),
    .groups = 'drop'
  )

# filtrar palabras en rango percentil 20-85
palabras_balanceadas_familia <- palabras_emocionales_freq %>%
  left_join(percentiles_familia, by = "categoria") %>%
  filter(n >= p20 & n <= p85) %>%
  select(-p20, -p50, -p80, -p85)

# definir traducción de emociones
english_spanish <- c(
  "anger" = "ira",
  "anticipation" = "anticipación",
  "disgust" = "asco",
  "fear" = "miedo",
  "joy" = "alegría",
  "sadness" = "tristeza",
  "surprise" = "sorpresa",
  "trust" = "confianza"
)

# top palabras balanceadas
cat("\nProcesando top palabras SIN duplicados...\n")
top_balanceado_familia <- palabras_balanceadas_familia %>%
  filter(palabra != "ay") %>% 
  group_by(categoria, palabra) %>%
  arrange(desc(n)) %>%
  slice(1) %>%  # Solo la emoción con mayor frecuencia
  ungroup() %>%
  group_by(categoria, emocion) %>%
  slice_max(n, n = 8, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(emocion_es = recode(emocion, !!!english_spanish))

# Verificar que no haya duplicados
cat("\n=== VERIFICACIÓN DE DUPLICADOS ===\n")
duplicados_check <- top_balanceado_familia %>%
  group_by(categoria, palabra) %>%
  summarise(n_emociones = n(), .groups = 'drop') %>%
  filter(n_emociones > 1)

if (nrow(duplicados_check) > 0) {
  cat("¡ADVERTENCIA! Hay", nrow(duplicados_check), "palabras en múltiples emociones:\n")
  print(duplicados_check)
} else {
  cat("VERIFICADO: Cada palabra aparece en una sola emoción por categoría.\n")
}


print(top_balanceado_familia %>% count(emocion_es, sort = TRUE))


# Graficos por categoria
grafico_percentil_familiar <- function(categoria_seleccionada) {
  datos <- top_balanceado_familia %>%
    filter(categoria == categoria_seleccionada) %>%
    mutate(palabra = str_to_title(palabra))
  
  if (nrow(datos) == 0) return(NULL)
  
  stats_cat <- percentiles_familia %>%
    filter(categoria == categoria_seleccionada)
  
  ggplot(datos, aes(x = reorder_within(palabra, n, emocion_es), y = n, fill = emocion_es)) +
    geom_col(alpha = 0.9, show.legend = FALSE) +
    scale_x_reordered() +
    coord_flip() +
    facet_wrap(~emocion_es, scales = "free_y", ncol = 3) +
    scale_fill_manual(values = c(
      "tristeza" = viridis(8)[1],
      "miedo" = viridis(8)[2],
      "asco" = viridis(8)[3],
      "ira" = "#e76f51",
      "anticipación" = viridis(8)[5],
      "confianza" = viridis(8)[6],
      "sorpresa" = viridis(8)[7],
      "alegría" = viridis(8)[8]
    )) +
    labs(
      title = paste("Perfil Emocional de:", str_to_title(categoria_seleccionada)),
      subtitle = paste("(percentiles 20-85):",
                       round(stats_cat$p20, 1), "-", round(stats_cat$p85, 1), "apariciones"),
      x = "Palabra",
      y = "Frecuencia",
    ) +
    theme_minimal(base_size = 12) +
    theme(
      strip.text = element_text(face = "bold", size = 12),
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(size = 9, color = "gray50"),
      axis.text.y = element_text(size = 12),
      panel.grid.major.y = element_blank()
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
}
categorias_balanceadas <- unique(top_balanceado_familia$categoria)

cat("\nGenerando gráficos por percentil para cada figura familiar...\n")
for (i in seq_along(categorias_balanceadas)) {
  cat <- categorias_balanceadas[i]
  cat(sprintf("%d/%d: Generando gráfico para: %s\n", 
              i, length(categorias_balanceadas), cat))
  
  g <- grafico_percentil_familiar(cat) 
  if (!is.null(g)) {
    print(g)  
  }
}


