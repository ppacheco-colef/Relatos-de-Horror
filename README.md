# Relatos-de-Horror
Contiene mas de 4000 archivos con historias de terror escritas por escuchas y suscriptores de la comunidad youtubera Relatos de Horror. El material se presenta junto con un código de rstudio para la replicación, corrección y ampliación de análisis cultural mediante técnicas de minería de texto. 

INSTRUCCIONES:
1. Descarga este repositorio.

2. Descomprime relatos de horror.zip en una carpeta llamada datos/ (junto a los scripts).

3. Ejecuta en R (en orden)
3.1 RdH_Text mining.R: Análisis de frecuencias, redes de bigramas y sentimiento.
3.2 RdH_mapeo de lugares.R: Extrae topónimos y genera un CSV para mapas.

ADVERTENCIA IMPORTANTE PARA EL MAPEO
El archivo CSV que genera RdH_mapeo de lugares.R NO lo abras con Excel antes de usarlo en QGIS. Excel corrompe caracteres especiales
Para QGIS correctamente:
Abre el CSV con editor de texto plano (Bloc de notas, VS Code, RStudio).
Verifica "Ñ" y otros especiales
Guárdalo con codificación UTF-8.
