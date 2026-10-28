# ADL_SURNAME_LABELS_V1
# Display labels only. Canonical filter values remain unchanged.

adl_surname_label <- function(name) {
  name <- trimws(as.character(name))
  if (!nzchar(name)) return(name)

  # ADL_EDITORIAL_EXCEPTIONS_V2
  # Valor original -> etiqueta visible (sense alterar el filtre).
  editorial <- c(
    "Eugenio José Méndez de Haro" =
      "Méndez de Haro, Eugenio José",
    "Raquel Mateos de la Paz" =
      "Mateos de la Paz, Raquel",
    "Joan de Grau (personatge llegendari)" =
      "Grau, Joan de (personatge llegendari)",
    "Amada López de Meneses" =
      "López de Meneses, Amada",
    "Antoni Bonet i Isart" =
      "Bonet i Isart, Antoni",
    "Esteve Albert i Corp" =
      "Albert i Corp, Esteve",
    "José Luis Medina Monzón" =
      "Medina Monzón, José Luis",
    "Miquel Mas i Miró" =
      "Mas i Miró, Miquel",
    "Ramon M. Rodon" =
      "Rodon, Ramón M.",
    "Cèsar August Torras" =
      "Torras i Ferreri, Cèsar August",
    "Xipahuatzin (personatge llegendari)" =
      "Xipahuatzin (personatge llegendari)",
    "Projecte Desmuntant-Princessa" =
      "Projecte Desmuntant-Princessa",
    "Organització local" =
      "Organització local",
    "Autor no identificat" =
      "Autor no identificat",
    "Picaresca Club" =
      "Picaresca Club",
    "Família Grau i Grau" =
      "Família Grau i Grau",
    "Casa de Guillermo de Grau-Moctezuma" =
      "Casa de Guillermo de Grau-Moctezuma",
    "Autor material no identificat" =
      "Autor material no identificat",
    "Toloriu" =
      "Toloriu"
  )
  if (name %in% names(editorial))
    return(unname(editorial[[name]]))

  # Explicit, editorially reviewed cases.
  overrides <- c(
    "Joan Amades" = "Amades, Joan",
    "Joaquín del Álamo" = "Álamo, Joaquín del",
    "Guillermo de Grau Rifé" = "Grau Rifé, Guillermo",
    "Carles Grandó" = "Grandó, Carles",
    "Enrique Rubio" = "Rubio, Enrique",
    "Josep Xicota" = "Xicota, Josep",
    "Moctezuma II" = "Moctezuma II"
  )
  if (name %in% names(overrides))
    return(unname(overrides[[name]]))

  # Already inverted: keep original.
  if (grepl(",", name, fixed=TRUE)) return(name)

  parts <- strsplit(name, "\\s+")[[1]]
  n <- length(parts)
  if (n < 2L) return(name)

  # Historical regnal names, e.g. Moctezuma II.
  if (grepl("^[IVXLCDM]+$", parts[n]))
    return(name)

  # Institutions: never invert automatically.
  institutions <- c(
    "arxiu", "archivo", "biblioteca", "universitat",
    "universidad", "museu", "museo", "ministerio",
    "generalitat", "ajuntament", "ayuntamiento",
    "institut", "instituto", "fundació", "fundacion",
    "revista", "diari", "diario", "centre", "centro"
  )
  if (tolower(parts[1]) %in% institutions)
    return(name)

  # Given name + particle + family name.
  if (n == 3L && tolower(parts[2]) %in% c("de","del","d'")) {
    return(paste0(parts[3], ", ", parts[1], " ", parts[2]))
  }

  # Given name + particle + two surnames.
  if (n == 4L && tolower(parts[2]) %in% c("de","del","d'")) {
    return(paste0(
      paste(parts[3:4], collapse=" "),
      ", ", parts[1], " ", parts[2]
    ))
  }

  # Simple given name + surname.
  if (n == 2L)
    return(paste0(parts[2], ", ", parts[1]))

  # Three tokens: assume two surnames.
  # Complex 4+ token names require editorial review.
  if (n == 3L)
    return(paste0(parts[2], " ", parts[3], ", ", parts[1]))

  name
}

adl_surname_choices <- function(values) {
  values <- as.character(values)
  if (!length(values)) return(setNames(character(), character()))

  labels <- vapply(values, adl_surname_label, character(1))
  keys <- iconv(tolower(labels), from="", to="ASCII//TRANSLIT")
  keys[is.na(keys)] <- tolower(labels[is.na(keys)])
  idx <- order(keys, labels, method="radix")

  setNames(values[idx], labels[idx])
}
