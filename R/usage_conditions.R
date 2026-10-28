
usage_conditions_ui <- function(lang = "ca") {

  if (identical(lang, "es")) {
    heading <- "Condiciones de uso de los materiales"
    p1 <- paste(
      "El Archivo digital de una leyenda es un proyecto",
      "de investigación y divulgación histórica, sin",
      "finalidad comercial, que facilita la consulta de",
      "fuentes documentales relacionadas con la leyenda",
      "de Xipahuatzin y Toloriu."
    )
    p2 <- paste(
      "Los textos, documentos e imágenes reproducidos",
      "se ofrecen exclusivamente con fines de consulta,",
      "estudio y difusión cultural, de acuerdo con el",
      "régimen jurídico de cada material y, cuando",
      "corresponde, con las autorizaciones concedidas",
      "por sus titulares."
    )
    p3 <- paste(
      "La consulta de los materiales no implica cesión",
      "alguna de derechos de reproducción, distribución",
      "o reutilización. Cualquier uso posterior debe",
      "respetar los derechos aplicables, las condiciones",
      "de las autorizaciones y los créditos indicados",
      "en cada ficha, sin perjuicio de los usos",
      "legalmente permitidos y del régimen de dominio público."
    )
    p4 <- paste(
      "Las descripciones e interpretaciones elaboradas",
      "por el proyecto pueden citarse indicando su",
      "procedencia. Las reproducciones documentales",
      "de terceros mantienen sus propias condiciones",
      "de uso. La ausencia de una opción de descarga",
      "no impide técnicamente la captura de los",
      "contenidos ni autoriza su reutilización."
    )
  } else {
    heading <- "Condicions d’ús dels materials"
    p1 <- paste(
      "L’Arxiu digital d’una llegenda és un projecte",
      "de recerca i divulgació històrica, sense",
      "finalitat comercial, que facilita la consulta",
      "de fonts documentals relacionades amb la",
      "llegenda de Xipahuatzin i Toloriu."
    )
    p2 <- paste(
      "Els textos, documents i imatges reproduïts",
      "s’ofereixen exclusivament amb finalitats de",
      "consulta, estudi i difusió cultural, d’acord",
      "amb el règim jurídic de cada material i,",
      "quan correspon, amb les autoritzacions",
      "concedides pels seus titulars."
    )
    p3 <- paste(
      "La consulta dels materials no comporta cap",
      "cessió de drets de reproducció, distribució",
      "o reutilització. Qualsevol ús posterior ha",
      "de respectar els drets aplicables, les",
      "condicions de les autoritzacions i els crèdits",
      "indicats a cada fitxa, sens perjudici dels",
      "usos legalment permesos i del règim de domini públic."
    )
    p4 <- paste(
      "Les descripcions i interpretacions elaborades",
      "pel projecte es poden citar amb indicació de",
      "la procedència. Les reproduccions documentals",
      "de tercers mantenen les seves pròpies",
      "condicions d’ús. L’absència d’una opció de",
      "descàrrega no impedeix tècnicament la captura",
      "dels continguts ni n’autoritza la reutilització."
    )
  }

  tags$section(
    class = "usage-conditions",
    tags$h3(heading),
    tags$p(p1),
    tags$p(p2),
    tags$p(tags$strong(p3)),
    tags$p(p4)
  )
}
