rm(list = ls())
cat("\014")


library(tidyverse)
library(future)
library(furrr)
library(stringr)
library(stringi)

# Configuración para procesamiento paralelo
options(future.rng.onMisuse = "ignore")
plan(multisession, workers = availableCores() - 1)

# normalización de texto
normalizar_texto <- function(texto) {
  texto %>%
    str_to_lower() %>%
    stri_trans_general("Latin-ASCII") %>%  # Eliminar acentos y caracteres especiales
    str_replace_all("[^[:alnum:][:space:]]", " ") %>%  # Mantener solo alfanuméricos y espacios
    str_replace_all("\\s+", " ") %>%  # Reducir múltiples espacios a uno solo
    str_trim()  # Eliminar espacios al inicio y final
}

#definición de términos geográficos

keywords_paises <- c("mexico", "estados unidos", "canada", "belice", "costa rica", 
                     "el salvador", "guatemala", "honduras", "nicaragua", "panama", 
                     "argentina", "bolivia", "brasil", "chile", "colombia", "ecuador", 
                     "paraguay", "peru", "uruguay", "venezuela") %>% 
  normalizar_texto()

keywords_paises <- c("mexico", "estados unidos", "canada", "belice", "costa rica", 
                     "el salvador", "guatemala", "honduras", "nicaragua", "panama", 
                     "argentina", "bolivia", "brasil", "chile", "colombia", "ecuador", 
                     "paraguay", "peru", "uruguay", "venezuela")

keywords_subdivisiones <- c(
  # México
  "aguascalientes", "baja california", "baja california sur", "campeche", "chiapas", 
  "chihuahua", "coahuila", "colima", "durango", "guanajuato", "guerrero", "hidalgo", 
  "jalisco", "estado de mexico", "michoacan", "morelos", "nayarit", "nuevo leon", 
  "oaxaca", "puebla", "queretaro", "quintana roo", "san luis potosi", "sinaloa", 
  "sonora", "tabasco", "tamaulipas", "tlaxcala", "veracruz", "yucatan", "zacatecas", 
  "cdmx", "ciudad de mexico",
  
  # Estados Unidos
  "alabama", "alaska", "arizona", "arkansas", "california", "carolina del norte", 
  "carolina del sur", "colorado", "connecticut", "dakota del norte", "dakota del sur", 
  "delaware", "florida", "georgia", "hawai", "idaho", "illinois", "indiana", "iowa", 
  "kansas", "kentucky", "louisiana", "maine", "maryland", "massachusetts", "michigan", 
  "minnesota", "mississippi", "missouri", "montana", "nebraska", "nevada", "new jersey", 
  "new york", "new hampshire", "new mexico", "ohio", "oklahoma", "oregon", 
  "pennsylvania", "rhode island", "tennessee", "texas", "utah", "vermont", "virginia", 
  "west virginia", "washington", "wisconsin", "wyoming", "distrito de columbia",
  
  # Canadá
  "alberta", "british columbia", "prince edward island", "manitoba", 
  "nova scotia", "new brunswick", "ontario", "quebec", "saskatchewan", 
  "newfoundland and labrador", "nunavut", "northwest territories", "yukon",
  
  # Belice
  "belize", "cayo", "corozal", "orange walk", "stann creek", "toledo",
  
  # Costa Rica
  "san jose", "alajuela", "cartago", "heredia", "guanacaste", "puntarenas", "limon",
  
  # El Salvador
  "ahuachapan", "cabanas", "chalatenango", "cuscatlan", "la libertad", "la paz", 
  "la union", "morazan", "san miguel", "san salvador", "san vicente", "santa ana", 
  "sonsonate", "usulutan",
  
  # Guatemala
  "alta verapaz", "baja verapaz", "chimaltenango", "chiquimula", "el progreso", 
  "escuintla", "guatemala", "huehuetenango", "izabal", "jalapa", "jutiapa", 
  "peten", "quetzaltenango", "quiche", "retalhuleu", "sacatepequez", 
  "san marcos", "santa rosa", "solola", "suchitepequez", "totonicapan", "zacapa",
  
  # Honduras
  "atlantida", "choluteca", "colon", "comayagua", "copan", "cortes", 
  "el paraiso", "francisco morazan", "gracias a dios", "intibuca", 
  "islas de la bahia", "la paz", "lempira", "ocotepeque", "olancho", 
  "santa barbara", "valle", "yoro",
  
  # Nicaragua
  "boaco", "carazo", "chinandega", "chontales", "esteli", "granada", 
  "jinotega", "leon", "madriz", "managua", "masaya", "matagalpa", 
  "nueva segovia", "rivas", "rio san juan", "costa caribe norte", 
  "costa caribe sur",
  
  # Panamá
  "bocas del toro", "chiriqui", "cocle", "colon", "darien", "herrera", 
  "los santos", "panama", "veraguas", "embera-wounaan", 
  "guna yala", "ngabe-bugle", "madugandi", "wargandi",
  
  # Argentina
  "buenos aires", "catamarca", "chaco", "chubut", "cordoba", "corrientes", 
  "entre rios", "formosa", "jujuy", "la pampa", "la rioja", "mendoza", 
  "misiones", "neuquen", "rio negro", "salta", "san juan", "san luis", 
  "santa cruz", "santa fe", "santiago del estero", "tierra del fuego", 
  "tucuman", "ciudad autonoma de buenos aires",
  
  # Bolivia
  "chuquisaca", "cochabamba", "beni", "la paz", "oruro", "pando", "potosi", 
  "santa cruz", "tarija",
  
  # Brasil
  "acre", "alagoas", "amapa", "amazonas", "bahia", "ceara", "espirito santo", 
  "goias", "wara", "maranhao", "mato grosso", "mato grosso do sul", "minas gerais", 
  "paraiba", "parana", "pernambuco", "piaui", "rio de janeiro", 
  "rio grande do norte", "rio grande do sul", "rondonia", "roraima", 
  "santa catarina", "sao paulo", "sergipe", "tocantins", "distrito federal",
  
  # Chile
  "arica y parinacota", "tarapaca", "antofagasta", "atacama", "coquimbo", 
  "valparaiso", "metropolitana de santiago", "libertador general bernardo o'higgins", 
  "maule", "nuble", "biobio", "la araucania", "los rios", "los lagos", 
  "aysen del general carlos ibanez del campo", "magallanes y de la antartica chilena",
  
  # Colombia
  "amazonas", "antioquia", "arauca", "atlantico", "bolivar", "boyaca", 
  "caldas", "caqueta", "casanare", "cauca", "cesar", "choco", "cordoba", 
  "cundinamarca", "guainia", "guaviare", "huila", "la guajira", "magdalena", 
  "meta", "nariño", "norte de santander", "putumayo", "quindio", "risaralda", 
  "san andres y providencia", "santander", "sucre", "tolima", "valle del cauca", 
  "vaupes", "vichada", "bogota",
  
  # Ecuador
  "azuay", "bolivar", "canar", "carchi", "chimborazo", "cotopaxi", 
  "el oro", "esmeraldas", "galapagos", "guayas", "imbabura", "loja", 
  "los rios", "manabi", "morona santiago", "napo", "orellana", 
  "pastaza", "pichincha", "santa elena", "santo domingo de los tsachilas", 
  "sucumbios", "tungurahua", "zamora chinchipe",
  
  # Paraguay
  "alto paraguay", "alto parana", "amambay", "boqueron", "caaguazu", 
  "caazapa", "canindeyu", "central", "concepcion", "cordillera", 
  "guaira", "itapua", "misiones", "neembucu", "paraguari", 
  "presidente hayes", "san pedro", "asuncion",
  
  # Perú
  "amazonas", "ancash", "apurimac", "arequipa", "ayacucho", "cajamarca", 
  "callao", "cusco", "huancavelica", "huanuco", "ica", "junin", "la libertad", 
  "lambayeque", "lima", "loreto", "madre de dios", "moquegua", "pasco", 
  "piura", "puno", "san martin", "tacna", "tumbes", "ucayali",
  
  # Uruguay
  "artigas", "canelones", "cerro largo", "colonia", "durazno", "flores", 
  "florida", "lavalleja", "maldonado", "montevideo", "paysandu", "rio negro", 
  "rivera", "rocha", "salto", "san jose", "soriano", "tacuarembo", "treinta y tres",
  
  # Venezuela
  "amazonas", "anzoategui", "apure", "aragua", "barinas", "bolivar", 
  "carabobo", "cojedes", "delta amacuro", "falcon", "guarico", "lara", 
  "merida", "miranda", "monagas", "nueva esparta", "portuguesa", "sucre", 
  "tachira", "trujillo", "vargas", "yaracuy", "zulia", "distrito capital", 
  "dependencias federales"
) %>% normalizar_texto()

# combinar país - subdivisión
keywords_completas <- c(
  paste0("pais_", keywords_paises),  # Países con prefijo
  keywords_subdivisiones             # Subdivisiones
)

#desambiguación 

terminos_ambiguos <- list(
  "cayo" = "belice",
  "bahia" = "brasil",
  "colonia" = "uruguay",
  "salto" = "uruguay",
  "flores" = "uruguay",
  "san pedro" = "paraguay",
  "gracias a dios" = "honduras",
  "herrera" = "panama",
  "nevada" = "estados unidos",
  "florida" = "estados unidos",
  "magdalena" = "colombia",
  "corrientes" = "argentina",
  "meta" = "colombia",
  "bolivar" = "ecuador",
  "cabanas" = "panama",
  "santa ana" = "el salvador",
  "la libertad" = "el salvador",
  "cesar" = "colombia",
  "lima" = "peru",
  "ica" = "peru",
  "puno" = "peru",
  "madre de dios" = "peru",
  "los lagos" = "chile",
  "los rios" = "chile",
  "vargas" = "venezuela",
  "miranda" = "venezuela",
  "el oro" = "ecuador",
  "distrito federal" = "venezuela",
  "salta" = "argentina",
  "apure" = "venezuela",
  "lara" = "venezuela",
  "mendoza" = "argentina",
  "cordoba" = "argentina",
  "central" = "paraguay",
  "san luis" = "argentina",
  "colorado" = "estados unidos",
  "montana" = "estados unidos",
  "san juan" = "argentina",
  "valle" = "honduras",
  "rivera" = "uruguay",
  "choco" = "colombia",
  "cortes" = "honduras",
  "santa cruz" = c("argentina", "bolivia"),
  "la paz" = c("el salvador", "honduras", "bolivia")
) %>%
  map(normalizar_texto)

names(terminos_ambiguos) <- names(terminos_ambiguos) %>% normalizar_texto()

#mapeo de subdivisiones

mapeo_subdivision_pais <- list(
  # México
  "aguascalientes" = "mexico",
  "baja california" = "mexico",
  "baja california sur" = "mexico",
  "campeche" = "mexico",
  "chiapas" = "mexico",
  "chihuahua" = "mexico",
  "coahuila" = "mexico",
  "colima" = "mexico",
  "durango" = "mexico",
  "guanajuato" = "mexico",
  "guerrero" = "mexico",
  "hidalgo" = "mexico",
  "jalisco" = "mexico",
  "estado de mexico" = "mexico",
  "michoacan" = "mexico",
  "morelos" = "mexico",
  "nayarit" = "mexico",
  "nuevo leon" = "mexico",
  "oaxaca" = "mexico",
  "puebla" = "mexico",
  "queretaro" = "mexico",
  "quintana roo" = "mexico",
  "san luis potosi" = "mexico",
  "sinaloa" = "mexico",
  "sonora" = "mexico",
  "tabasco" = "mexico",
  "tamaulipas" = "mexico",
  "tlaxcala" = "mexico",
  "veracruz" = "mexico",
  "yucatan" = "mexico",
  "zacatecas" = "mexico",
  "cdmx" = "mexico",
  "ciudad de mexico" = "mexico",
  
  # Estados Unidos
  "alabama" = "estados unidos",
  "alaska" = "estados unidos",
  "arizona" = "estados unidos",
  "arkansas" = "estados unidos",
  "california" = "estados unidos",
  "carolina del norte" = "estados unidos",
  "carolina del sur" = "estados unidos",
  "colorado" = "estados unidos",
  "connecticut" = "estados unidos",
  "dakota del norte" = "estados unidos",
  "dakota del sur" = "estados unidos",
  "delaware" = "estados unidos",
  "florida" = "estados unidos",
  "georgia" = "estados unidos",
  "hawai" = "estados unidos",
  "idaho" = "estados unidos",
  "illinois" = "estados unidos",
  "indiana" = "estados unidos",
  "iowa" = "estados unidos",
  "kansas" = "estados unidos",
  "kentucky" = "estados unidos",
  "louisiana" = "estados unidos",
  "maine" = "estados unidos",
  "maryland" = "estados unidos",
  "massachusetts" = "estados unidos",
  "michigan" = "estados unidos",
  "minnesota" = "estados unidos",
  "mississippi" = "estados unidos",
  "missouri" = "estados unidos",
  "montana" = "estados unidos",
  "nebraska" = "estados unidos",
  "nevada" = "estados unidos",
  "new jersey" = "estados unidos",
  "new york" = "estados unidos",
  "new hampshire" = "estados unidos",
  "new mexico" = "estados unidos",
  "ohio" = "estados unidos",
  "oklahoma" = "estados unidos",
  "oregon" = "estados unidos",
  "pennsylvania" = "estados unidos",
  "rhode island" = "estados unidos",
  "tennessee" = "estados unidos",
  "texas" = "estados unidos",
  "utah" = "estados unidos",
  "vermont" = "estados unidos",
  "virginia" = "estados unidos",
  "west virginia" = "estados unidos",
  "washington" = "estados unidos",
  "wisconsin" = "estados unidos",
  "wyoming" = "estados unidos",
  "distrito de columbia" = "estados unidos",
  
  # Canadá
  "alberta" = "canada",
  "british columbia" = "canada",
  "prince edward island" = "canada",
  "manitoba" = "canada",
  "nova scotia" = "canada",
  "new brunswick" = "canada",
  "ontario" = "canada",
  "quebec" = "canada",
  "saskatchewan" = "canada",
  "newfoundland and labrador" = "canada",
  "nunavut" = "canada",
  "northwest territories" = "canada",
  "yukon" = "canada",
  
  # Belice
  "belize" = "belice",
  "cayo" = "belice",
  "corozal" = "belice",
  "orange walk" = "belice",
  "stann creek" = "belice",
  "toledo" = "belice",
  
  # Costa Rica
  "san jose" = "costa rica",
  "alajuela" = "costa rica",
  "cartago" = "costa rica",
  "heredia" = "costa rica",
  "guanacaste" = "costa rica",
  "puntarenas" = "costa rica",
  "limon" = "costa rica",
  
  # El Salvador
  "ahuachapan" = "el salvador",
  "cabanas" = "el salvador",
  "chalatenango" = "el salvador",
  "cuscatlan" = "el salvador",
  "la libertad" = "el salvador",
  "la paz" = "el salvador",
  "la union" = "el salvador",
  "morazan" = "el salvador",
  "san miguel" = "el salvador",
  "san salvador" = "el salvador",
  "san vicente" = "el salvador",
  "santa ana" = "el salvador",
  "sonsonate" = "el salvador",
  "usulutan" = "el salvador",
  
  # Guatemala
  "alta verapaz" = "guatemala",
  "baja verapaz" = "guatemala",
  "chimaltenango" = "guatemala",
  "chiquimula" = "guatemala",
  "el progreso" = "guatemala",
  "escuintla" = "guatemala",
  "guatemala" = "guatemala",
  "huehuetenango" = "guatemala",
  "izabal" = "guatemala",
  "jalapa" = "guatemala",
  "jutiapa" = "guatemala",
  "peten" = "guatemala",
  "quetzaltenango" = "guatemala",
  "quiche" = "guatemala",
  "retalhuleu" = "guatemala",
  "sacatepequez" = "guatemala",
  "san marcos" = "guatemala",
  "santa rosa" = "guatemala",
  "solola" = "guatemala",
  "suchitepequez" = "guatemala",
  "totonicapan" = "guatemala",
  "zacapa" = "guatemala",
  
  # Honduras
  "atlantida" = "honduras",
  "choluteca" = "honduras",
  "colon" = "honduras",
  "comayagua" = "honduras",
  "copan" = "honduras",
  "cortes" = "honduras",
  "el paraiso" = "honduras",
  "francisco morazan" = "honduras",
  "gracias a dios" = "honduras",
  "intibuca" = "honduras",
  "islas de la bahia" = "honduras",
  "la paz" = "honduras",
  "lempira" = "honduras",
  "ocotepeque" = "honduras",
  "olancho" = "honduras",
  "santa barbara" = "honduras",
  "valle" = "honduras",
  "yoro" = "honduras",
  
  # Nicaragua
  "boaco" = "nicaragua",
  "carazo" = "nicaragua",
  "chinandega" = "nicaragua",
  "chontales" = "nicaragua",
  "esteli" = "nicaragua",
  "granada" = "nicaragua",
  "jinotega" = "nicaragua",
  "leon" = "nicaragua",
  "madriz" = "nicaragua",
  "managua" = "nicaragua",
  "masaya" = "nicaragua",
  "matagalpa" = "nicaragua",
  "nueva segovia" = "nicaragua",
  "rivas" = "nicaragua",
  "rio san juan" = "nicaragua",
  "costa caribe norte" = "nicaragua",
  "costa caribe sur" = "nicaragua",
  
  # Panamá
  "bocas del toro" = "panama",
  "chiriqui" = "panama",
  "cocle" = "panama",
  "colon" = "panama",
  "darien" = "panama",
  "herrera" = "panama",
  "los santos" = "panama",
  "panama" = "panama",
  "veraguas" = "panama",
  "embera-wounaan" = "panama",
  "guna yala" = "panama",
  "ngabe-bugle" = "panama",
  "madugandi" = "panama",
  "wargandi" = "panama",
  
  # Argentina
  "buenos aires" = "argentina",
  "catamarca" = "argentina",
  "chaco" = "argentina",
  "chubut" = "argentina",
  "cordoba" = "argentina",
  "corrientes" = "argentina",
  "entre rios" = "argentina",
  "formosa" = "argentina",
  "jujuy" = "argentina",
  "la pampa" = "argentina",
  "la rioja" = "argentina",
  "mendoza" = "argentina",
  "misiones" = "argentina",
  "neuquen" = "argentina",
  "rio negro" = "argentina",
  "salta" = "argentina",
  "san juan" = "argentina",
  "san luis" = "argentina",
  "santa cruz" = "argentina",
  "santa fe" = "argentina",
  "santiago del estero" = "argentina",
  "tierra del fuego" = "argentina",
  "tucuman" = "argentina",
  "ciudad autonoma de buenos aires" = "argentina",
  
  # Bolivia
  "chuquisaca" = "bolivia",
  "cochabamba" = "bolivia",
  "beni" = "bolivia",
  "la paz" = "bolivia",
  "oruro" = "bolivia",
  "pando" = "bolivia",
  "potosi" = "bolivia",
  "santa cruz" = "bolivia",
  "tarija" = "bolivia",
  
  # Brasil
  "acre" = "brasil",
  "alagoas" = "brasil",
  "amapa" = "brasil",
  "amazonas" = "brasil",
  "bahia" = "brasil",
  "ceara" = "brasil",
  "espirito santo" = "brasil",
  "goias" = "brasil",
  "maranhao" = "brasil",
  "mato grosso" = "brasil",
  "mato grosso do sul" = "brasil",
  "minas gerais" = "brasil",
  "para" = "brasil",
  "paraiba" = "brasil",
  "parana" = "brasil",
  "pernambuco" = "brasil",
  "piaui" = "brasil",
  "rio de janeiro" = "brasil",
  "rio grande do norte" = "brasil",
  "rio grandeo do sul" = "brasil",
  "rondonia" = "brasil",
  "roraima" = "brasil",
  "santa catarina" = "brasil",
  "sao paulo" = "brasil",
  "sergipe" = "brasil",
  "tocantins" = "brasil",
  "distrito federal" = "brasil",
  
  # Chile
  "arica y parinacota" = "chile",
  "tarapaca" = "chile",
  "antofagasta" = "chile",
  "atacama" = "chile",
  "coquimbo" = "chile",
  "valparaiso" = "chile",
  "metropolitana de santiago" = "chile",
  "libertador general bernardo o'higgins" = "chile",
  "maule" = "chile",
  "nuble" = "chile",
  "biobio" = "chile",
  "la araucania" = "chile",
  "los rios" = "chile",
  "los lagos" = "chile",
  "aysen del general carlos ibanez del campo" = "chile",
  "magallanes y de la antartica chilena" = "chile",
  
  # Colombia
  "amazonas" = "colombia",
  "antioquia" = "colombia",
  "arauca" = "colombia",
  "atlantico" = "colombia",
  "bolivar" = "colombia",
  "boyaca" = "colombia",
  "caldas" = "colombia",
  "caqueta" = "colombia",
  "casanare" = "colombia",
  "cauca" = "colombia",
  "cesar" = "colombia",
  "choco" = "colombia",
  "cordoba" = "colombia",
  "cundinamarca" = "colombia",
  "guainia" = "colombia",
  "guaviare" = "colombia",
  "huila" = "colombia",
  "la guajira" = "colombia",
  "magdalena" = "colombia",
  "meta" = "colombia",
  "nariño" = "colombia",
  "norte de santander" = "colombia",
  "putumayo" = "colombia",
  "quindio" = "colombia",
  "risaralda" = "colombia",
  "san andres y providencia" = "colombia",
  "santander" = "colombia",
  "sucre" = "colombia",
  "tolima" = "colombia",
  "valle del cauca" = "colombia",
  "vaupes" = "colombia",
  "vichada" = "colombia",
  "bogota" = "colombia",
  
  # Ecuador
  "azuay" = "ecuador",
  "bolivar" = "ecuador",
  "canar" = "ecuador",
  "carchi" = "ecuador",
  "chimborazo" = "ecuador",
  "cotopaxi" = "ecuador",
  "el oro" = "ecuador",
  "esmeraldas" = "ecuador",
  "galapagos" = "ecuador",
  "guayas" = "ecuador",
  "imbabura" = "ecuador",
  "loja" = "ecuador",
  "los rios" = "ecuador",
  "manabi" = "ecuador",
  "morona santiago" = "ecuador",
  "napo" = "ecuador",
  "orellana" = "ecuador",
  "pastaza" = "ecuador",
  "pichincha" = "ecuador",
  "santa elena" = "ecuador",
  "santo domingo de los tsachilas" = "ecuador",
  "sucumbios" = "ecuador",
  "tungurahua" = "ecuador",
  "zamora chinchipe" = "ecuador",
  
  # Paraguay
  "alto paraguay" = "paraguay",
  "alto parana" = "paraguay",
  "amambay" = "paraguay",
  "boqueron" = "paraguay",
  "caaguazu" = "paraguay",
  "caazapa" = "paraguay",
  "canindeyu" = "paraguay",
  "central" = "paraguay",
  "concepcion" = "paraguay",
  "cordillera" = "paraguay",
  "guaira" = "paraguay",
  "itapua" = "paraguay",
  "misiones" = "paraguay",
  "neembucu" = "paraguay",
  "paraguari" = "paraguay",
  "presidente hayes" = "paraguay",
  "san pedro" = "paraguay",
  "asuncion" = "paraguay",
  
  # Perú
  "amazonas" = "peru",
  "ancash" = "peru",
  "apurimac" = "peru",
  "arequipa" = "peru",
  "ayacucho" = "peru",
  "cajamarca" = "peru",
  "callao" = "peru",
  "cusco" = "peru",
  "huancavelica" = "peru",
  "huanuco" = "peru",
  "ica" = "peru",
  "junin" = "peru",
  "la libertad" = "peru",
  "lambayeque" = "peru",
  "lima" = "peru",
  "loreto" = "peru",
  "madre de dios" = "peru",
  "moquegua" = "peru",
  "pasco" = "peru",
  "piura" = "peru",
  "puno" = "peru",
  "san martin" = "peru",
  "tacna" = "peru",
  "tumbes" = "peru",
  "ucayali" = "peru",
  
  # Uruguay
  "artigas" = "uruguay",
  "canelones" = "uruguay",
  "cerro largo" = "uruguay",
  "colonia" = "uruguay",
  "durazno" = "uruguay",
  "flores" = "uruguay",
  "florida" = "uruguay",
  "lavalleja" = "uruguay",
  "maldonado" = "uruguay",
  "montevideo" = "uruguay",
  "paysandu" = "uruguay",
  "rio negro" = "uruguay",
  "rivera" = "uruguay",
  "rocha" = "uruguay",
  "salto" = "uruguay",
  "san jose" = "uruguay",
  "soriano" = "uruguay",
  "tacuarembo" = "uruguay",
  "treinta y tres" = "uruguay",
  
  # Venezuela
  "amazonas" = "venezuela",
  "anzoategui" = "venezuela",
  "apure" = "venezuela",
  "aragua" = "venezuela",
  "barinas" = "venezuela",
  "bolivar" = "venezuela",
  "carabobo" = "venezuela",
  "cojedes" = "venezuela",
  "delta amacuro" = "venezuela",
  "falcon" = "venezuela",
  "guarico" = "venezuela",
  "lara" = "venezuela",
  "merida" = "venezuela",
  "miranda" = "venezuela",
  "monagas" = "venezuela",
  "nueva esparta" = "venezuela",
  "portuguesa" = "venezuela",
  "sucre" = "venezuela",
  "tachira" = "venezuela",
  "trujillo" = "venezuela",
  "vargas" = "venezuela",
  "yaracuy" = "venezuela",
  "zulia" = "venezuela",
  "distrito capital" = "venezuela",
  "dependencias federales" = "venezuela"
) %>%
  map(normalizar_texto)

names(mapeo_subdivision_pais) <- names(mapeo_subdivision_pais) %>% normalizar_texto()

#función de conteo geográfico

contar_geografia <- function(texto) {
  texto_norm <- normalizar_texto(texto)
  conteos <- setNames(integer(length(keywords_completas)), keywords_completas)
  paises_presentes <- keywords_paises[str_detect(texto_norm, paste0("\\b", keywords_paises, "\\b"))]
  for (pais in keywords_paises) {
    if (str_detect(texto_norm, paste0("\\b", pais, "\\b"))) {
      conteos[paste0("pais_", pais)] <- 1L
    }
  }
  
  for (sub in keywords_subdivisiones) {
    patron_sub <- sub %>% 
      str_replace_all(" ", "\\\\s+") %>% 
      paste0("\\b", ., "\\b")
    
    if (str_detect(texto_norm, patron_sub)) {
      if (sub %in% names(terminos_ambiguos)) {
        paises_requeridos <- terminos_ambiguos[[sub]]
        
        if (any(paises_requeridos %in% paises_presentes)) {
          conteos[sub] <- 1L
        }
      } else {
        conteos[sub] <- 1L
      }
    }
  }
  
  # Regla para California
  if ("california" %in% names(conteos) && conteos["california"] == 1) {
    if (str_detect(texto_norm, "\\bbaja\\s+california\\b")) {
      conteos["california"] <- 0L
    }
  }
  
  return(conteos)
}

# Procesamiento de archivos
procesar_txt <- function(archivo) {
  tryCatch({
    texto <- readLines(archivo, warn = FALSE, encoding = "UTF-8")
    texto_unido <- paste(texto, collapse = " ")
    texto_normalizado <- normalizar_texto(texto_unido)
    
    return(texto_normalizado)
  }, error = function(e) {
    warning("Error procesando: ", archivo, " - ", e$message)
    return("")
  })
}

#procesamiento principal 

ruta_txt <- "C:/Users/CEPYGICT/Documents/ProyectoTextMining/02_textos_anonimos"

archivos_txt <- dir(ruta_txt, pattern = "\\.txt$", full.names = TRUE, ignore.case = TRUE)

cat("Encontrados", length(archivos_txt), "archivos TXT\n")

# procesar txts y conteo de términos

datos <- tibble(
  archivo = archivos_txt,
  contenido = future_map_chr(archivos_txt, procesar_txt, .progress = TRUE)
) %>%
  mutate(
    conteos = future_map(contenido, contar_geografia, .progress = TRUE)
  )

#crear dataframe

df_resultados <- datos %>%
  select(archivo, conteos) %>%
  unnest_longer(conteos, values_to = "presente", indices_to = "termino") %>%
  group_by(termino) %>%
  summarise(num_pdfs = sum(presente, na.rm = TRUE)) %>%
  ungroup()

#preparar y exporar datos para qgis

mapeo_completo <- bind_rows(
  tibble(
    termino = paste0("pais_", keywords_paises),
    pais = keywords_paises,
    tipo = "país"
  ),
  enframe(mapeo_subdivision_pais, name = "termino", value = "pais") %>%
    unnest(pais) %>%
    mutate(tipo = "subdivisión")
)

df_treemap <- df_resultados %>%
  left_join(mapeo_completo, by = "termino") %>%
  mutate(
    termino_base = ifelse(str_detect(termino, "^pais_"), 
                          str_remove(termino, "^pais_"), 
                          termino)
  ) %>%
  group_by(pais) %>%
  mutate(total_pais = sum(num_pdfs)) %>%
  ungroup() %>%
  arrange(desc(total_pais), pais, desc(num_pdfs)) %>%
  filter(num_pdfs > 0, !is.na(pais), !is.na(termino_base))


#exportación

df_qgis <- df_treemap %>%
  mutate(
    subdivision = if_else(tipo == "subdivisión", termino_base, NA_character_)
  ) %>%
  group_by(pais, subdivision) %>%
  summarise(
    total_menciones = sum(num_pdfs),
    num_terminos = n_distinct(termino_base)
  ) %>%
  ungroup()

ruta_exportacion <- "C:/Users/CEPYGICT/Documents/menciones_geograficas_txt.csv"

write.csv(df_qgis, ruta_exportacion, 
          row.names = FALSE, 
          fileEncoding = "UTF-8")



#visualización rápida
library(treemapify)

ggplot(df_treemap, aes(
  area = num_pdfs,
  fill = pais,
  label = paste0(termino_base, "\n", num_pdfs),
  subgroup = pais
)) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "white", size = 2) +
  geom_treemap_subgroup_text(
    place = "bottomleft",
    colour = "white",
    fontface = "bold",
    size = 10
  ) +
  geom_treemap_text(
    colour = "black",
    size = 8,
    place = "center",
    reflow = TRUE,
    padding.x = unit(2, "mm"),
    padding.y = unit(2, "mm"),
    min.size = 3
  ) +
  labs(
    title = "Menciones ",
    subtitle = paste("Análisis de", length(archivos_txt), "documentos")
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "none"
  )

