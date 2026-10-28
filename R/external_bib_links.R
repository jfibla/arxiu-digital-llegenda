# ADL_EXTERNAL_BIB_LINKS_V1
# Enllaços documentals de consulta externa, sense reproducció.
adl_external_bib_links <- local({
  bib <- read.csv(
    "data/bibliography_v1.csv",
    stringsAsFactors=FALSE,
    fileEncoding="UTF-8-BOM"
  )
  links <- read.csv(
    "data/bibliography_internal_links.csv",
    stringsAsFactors=FALSE,
    fileEncoding="UTF-8-BOM"
  )

  function(item_id, lang="ca") {
    ids <- unique(links$bib_id[links$item_id == item_id])
    if (!length(ids)) return(NULL)

    rows <- bib[bib$id %in% ids, , drop=FALSE]
    if (!nrow(rows)) return(NULL)

    valid <- vapply(rows$url, function(u) {
      if (is.na(u) || !nzchar(trimws(u))) return(FALSE)
      grepl("^https://", u) &&
        !grepl("[[:space:]<>\"']", u) &&
        !grepl(
          "^https://(?:localhost|127[.]0[.]0[.]1)(?:[:/]|$)",
          u,
          ignore.case=TRUE
        )
    }, logical(1))

    rows <- rows[valid, , drop=FALSE]
    if (!nrow(rows)) return(NULL)

    # Only mapped public source links; do not imply
    # that a general portal is the precise source record.
    es <- identical(lang, "es")

    tags$div(
      class="adl-external-document-links",
      tags$p(
        tags$strong(
          if (es)
            "Fuentes documentales externas"
          else
            "Fonts documentals externes"
        )
      ),
      tags$p(
        class="text-muted",
        if (es)
          paste(
            "Estos recursos se consultan en sitios externos.",
            "No están reproducidos en este archivo."
          )
        else
          paste(
            "Aquests recursos es consulten en llocs externs.",
            "No estan reproduïts en aquest arxiu."
          )
      ),
      tags$ul(
        lapply(seq_len(nrow(rows)), function(i) {
          r <- rows[i, , drop=FALSE]
          is_portal <- identical(
            trimws(as.character(r$tipus_enllac)),
            "portal_cerca"
          )

          tags$li(
            tags$a(
              href=as.character(r$url),
              target="_blank",
              rel="noopener noreferrer",
              if (is_portal) {
                if (es)
                  "Abrir el portal de búsqueda"
                else
                  "Obrir el portal de cerca"
              } else {
                if (es)
                  "Consultar la fuente"
                else
                  "Consultar la font"
              }
            ),
            tags$div(
              class="text-muted",
              style="font-size:.86rem;margin-top:.25rem;",
              as.character(r$referencia)
            )
          )
        })
      )
    )
  }
})
