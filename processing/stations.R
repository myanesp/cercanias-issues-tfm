library(readr)
library(dplyr)
library(tidytext)
library(lubridate)
library(stringr)
library(tidyr)
library(stringdist)

stations_df <- read_csv("../data/madrid-cercanias-stations.csv") # https://data.renfe.com/dataset/estaciones-cercanias-madrid/resource/daa68d9b-77cf-4024-890f-285d31184c5a

stations <- tolower(as.vector(stations_df$DESCRIPCION)) %>% 
  str_replace_all("-", " ") %>% 
  str_split("\\s+")

stations <- lapply(stations, function(x) str_remove_all(x, "madrid"))

stations <- sapply(stations, paste, collapse = " ") %>% 
  str_trim()

## This works:
stations_lower <- c("guadalajara", "azuqueca", "coslada", "recoletos", "piramides", "principe pio", "embajadores", "sol", "asamblea de entrevias", "villaverde alto", "villaverde bajo", "san cristobal industrial", "san cristobal de los angeles", "aravaca", "cuatro vientos", "doce de octubre", "vallecas", "el pozo", "santa eugenia", "fuencarral", "mirasierra", "ramon y cajal", "pitis", "chamartin", "orcasitas", "puente alcocer", "aeropuerto t 4", "fanjul", "las aguilas", "atocha cercanias", "mendez alvaro", "delicias", "mendez alvaro", "nuevos ministerios", "aluche", "laguna", "el goloso", "universidad cantoblanco", "universidad pontificia de comillas", "fuente de la mora", "vicalvaro", "valdebebas", "valdelasfuentes", "alcobendas san sebastian de los reyes", "majadahonda", "pozuelo", "el barrial centro comercial pozuelo", "las rozas", "pinar de las rozas", "torrelodones", "el escorial", "las matas", "san yago", "las zorreras navalquejigo", "aranjuez", "pinto", "valdemoro", "ciempozuelos", "villalba de guadarrama", "los negrales", "galapagar la navata", "alpedrete", "collado mediano", "los molinos guadarrama", "cercedilla", "puerto de navacerrada", "tres cantos", "colmenar viejo", "la garena", "alcala de henares", "alcala de henares universidad", "san fernando de henares", "torrejon de ardoz", "soto del henares", "meco", "getafe sector 3", "getafe centro", "las margaritas", "getafe industrial", "el casar", "leganes", "parque polvoranca", "zarzaquemada", "alcorcon", "las retamas", "san jose de valderas", "mostoles", "mostoles el soto", "fuenlabrada", "la serna fuenlabrada", "humanes", "parla", "los cotos")

stations_upper <- c("Guadalajara", "Azuqueca", "Coslada", "Recoletos", "Piramides", "Principe Pio", "Embajadores", "Sol", "Asamblea De Entrevias", "Villaverde Alto", "Villaverde Bajo", "San Cristobal Industrial", "San Cristobal De Los Angeles", "Aravaca", "Cuatro Vientos", "Doce De Octubre", "Vallecas", "El Pozo", "Santa Eugenia", "Fuencarral", "Mirasierra", "Ramon Y Cajal", "Pitis", "Chamartin", "Orcasitas", "Puente Alcocer", "Aeropuerto T 4", "Fanjul", "Las Aguilas", "Atocha Cercanias", "Mendez Alvaro", "Delicias", "Mendez Alvaro", "Nuevos Ministerios", "Aluche", "Laguna", "El Goloso", "Universidad Cantoblanco", "Universidad Pontificia De Comillas", "Fuente De La Mora", "Vicalvaro", "Valdebebas", "Valdelasfuentes", "Alcobendas San Sebastian De Los Reyes", "Majadahonda", "Pozuelo", "El Barrial Centro Comercial Pozuelo", "Las Rozas", "Pinar De Las Rozas", "Torrelodones", "El Escorial", "Las Matas", "San Yago", "Las Zorreras Navalquejigo", "Aranjuez", "Pinto", "Valdemoro", "Ciempozuelos", "Villalba De Guadarrama", "Los Negrales", "Galapagar La Navata", "Alpedrete", "Collado Mediano", "Los Molinos Guadarrama", "Cercedilla", "Puerto De Navacerrada", "Tres Cantos", "Colmenar Viejo", "La Garena", "Alcala De Henares", "Alcala De Henares Universidad", "San Fernando De Henares", "Torrejon De Ardoz", "Soto Del Henares", "Meco", "Getafe Sector 3", "Getafe Centro", "Las Margaritas", "Getafe Industrial", "El Casar", "Leganes", "Parque Polvoranca", "Zarzaquemada", "Alcorcon", "Las Retamas", "San Jose De Valderas", "Mostoles", "Mostoles El Soto", "Fuenlabrada", "La Serna Fuenlabrada", "Humanes", "Parla", "Los Cotos")

combined <- c(stations_lower, stations_upper)
pattern <- paste0("\\b", paste(combined, collapse = "|"), "\\b")

## Revisar cuando hay más de una
capitalized_stations <- sapply(stations, function(station) {
  words <- strsplit(station, " ")[[1]]
  capitalized_words <- toupper(substring(words, 1, 1))  # Capitalize first letter
  capitalized_words <- paste0(capitalized_words, substring(words, 2))  # Combine with remaining letters
  paste(capitalized_words, collapse = " ")  # Combine words with space
})


# df <- df %>% 
#   mutate(stations = str_extract_all(Text, pattern))  %>% 
#   unnest(stations, keep_empty = TRUE)

##
destination_pattern <- "(?i)(destino|dirección|direccion)\\s+(\\w+)"

text <- "tren con direccion Aranjuez"

str_match(text, destination_pattern)[, 3]

final_stations <- c("Aeropuerto T4", "Principe Pio", "Guadalajara", "Chamartin", "Aranjuez",
                    "El Escorial", "Parla", "Colmenar Viejo", "Alcobendas San Sebastian de los Reyes",
                    "Mostoles El Soto", "Humanes", "Principe Pio", "Alcala de Henares", "Guadalajara",
                    "Cercedilla", "Cotos", "Cercedilla", "Villalba", "Aeropuerto T4")


# Define the variable to check
variable_to_check <- c("cotos", "san sebastian", "principe pío", "villalba")

stringdist("alcobendas san", "humanes", method = "lv")

# Create a new variable "Line" to store the closest vector
data <- data.frame(variable_to_check)
data$Line <- ""
data$issue <- ""

for (i in seq_along(df$original)) {
  observation <- df$original[i]
  distances <- stringdist::stringdist(final_stations, observation, method = "lv")
  closest_index <- which.min(distances)
  closest_vector <- final_stations[closest_index]
  closest_value <- sub(".*?(\\d+).*", "\\1", closest_vector)  # Extract the numeric value from the closest vector
  
  if (length(closest_value) > 0) {
    df$Station[i] <- closest_value
    
    if (closest_value == "Aeropuerto T4") {
      df$Lines[i] <- "C1/C10"
    } else if (closest_value == "Principe Pio") {
      df$Lines[i] <- "C1/C7"
    } else if (closest_value == "Guadalajara") {
      df$Lines[i] <- "C2/C8"
    } else if (closest_value == "Chamartin") {
      df$Lines[i] <- "C2"
    } else if (closest_value == "Aranjuez") {
      df$Lines[i] <- "C3"
    } else if (closest_value == "El Escorial") {
      df$Lines[i] <- "C3"
    } else if (closest_value == "Parla") {
      df$Lines[i] <- "C4/C4a/C4b"
    } else if (closest_value == "Colmenar Viejo") {
      df$Lines[i] <- "C4a"
    } else if (closest_value == "Alcobendas San Sebastian de los Reyes") {
      df$Lines[i] <- "C4b"
    } else if (closest_value == "Mostoles El Soto") {
      df$Lines[i] <- "C5"
    } else if (closest_value == "Humanes") {
      df$Lines[i] <- "C5"
    } else if (closest_value == "Alcala de Henares") {
      df$Lines[i] <- "C7"
    } else if (closest_value == "Cercedilla") {
      df$Lines[i] <- "C8/C9"
    } else if (closest_value == "Cotos") {
      df$Lines[i] <- "C9"
    } else if (closest_value == "Villalba") {
      df$Lines[i] <- "C10"
    } else {
      df$Lines[i] <- NA  
    }
  } else {
    df$Station[i] <- NA
    df$Lines[i] <- NA
  }
}
