
# Bibliographic display decomposition. Never alters the canonical citation.
bib_card_parts <- function(reference, author, year){
  ref <- trimws(as.character(reference))
  au <- trimws(as.character(author))
  yr <- trimws(as.character(year))
  title <- ref
  quoted <- regmatches(ref,regexpr("«[^»]+»",ref,perl=TRUE))
  if(length(quoted) && nzchar(quoted)){
    title <- substring(quoted,2,nchar(quoted)-1)
  } else {
    prefix <- "^.*?\\([^)]*\\)\\.\\s*"
    remaining <- sub(prefix,"",ref,perl=TRUE)
    if(!identical(remaining,ref))title <- sub("\\.\\s+.*$","",remaining,perl=TRUE)
  }
  if(!nzchar(title))title <- ref
  # Show original bibliographic remainder rather than claiming publisher extraction is exact.
  meta <- paste(c(au,yr),collapse=" · ")
  list(title=title,meta=meta)
}

# ADL_BIBLIO_V1_1 — catalog with compact cards, reviewed links and curated miniature assets
`%||%` <- function(x,y) if(is.null(x)) y else x
adl_bib_ui <- function(){
  tags$div(class="container-fluid",style="max-width:1060px;padding:20px 24px;",
  # ADL_BIB_EDITORIAL_V2 — editorial library catalog, airy but compact
  tags$head(tags$style(HTML("
    .adl-bib-entry{display:flex;align-items:flex-start;gap:23px;padding:22px 0;border-bottom:1px solid #e8e7e3;background:#fff;max-width:100%;}
    .adl-bib-entry:first-of-type{padding-top:16px;}
    .adl-bib-thumb{flex:0 0 112px;width:112px;height:150px;overflow:hidden;border-radius:4px;border:1px solid #ddd9d1;background:#f7f4ec;display:flex;align-items:center;justify-content:center;box-shadow:0 2px 7px rgba(40,31,18,.09);}
    .adl-bib-thumb img{display:block;width:100%;height:100%;object-fit:contain;}
    .adl-bib-cover{box-sizing:border-box;width:100%;height:100%;position:relative;display:flex;flex-direction:column;justify-content:flex-start;padding:14px 10px 11px 14px;background:linear-gradient(100deg,#e2dac8 0,#f8f4eb 12%,#f2ecdf 100%);border-left:5px solid #8d7862;overflow:hidden;}
    .adl-bib-cover:before{content:'';position:absolute;left:11px;right:9px;top:10px;height:1px;background:#a99c83;}
    .adl-bib-cover-label{font:600 8px/1.4 system-ui,-apple-system,sans-serif;letter-spacing:.08em;text-transform:uppercase;color:#786e60;margin:7px 0 10px;}
    .adl-bib-cover-title{font:600 12px/1.25 Georgia,'Times New Roman',serif;color:#554837;display:-webkit-box;-webkit-box-orient:vertical;-webkit-line-clamp:5;overflow:hidden;overflow-wrap:anywhere;}
    .adl-bib-cover-year{margin-top:auto;font:500 10px/1.2 Georgia,serif;color:#857865;}
    .adl-bib-info{flex:1;min-width:0;max-width:850px;overflow-wrap:anywhere;}
    .adl-bib-eyebrow{font:600 10px/1.4 system-ui,-apple-system,sans-serif;text-transform:uppercase;letter-spacing:.11em;color:#7a786e;margin:3px 0 7px;}
    .adl-bib-title{font:600 20px/1.3 Georgia,'Times New Roman',serif;color:#252c31;margin:0 0 10px;}
    .adl-bib-reference{font:400 14px/1.65 system-ui,-apple-system,'Segoe UI',sans-serif;color:#46505a;margin:0 0 9px;}
    .adl-bib-note{font:400 13px/1.55 system-ui,-apple-system,sans-serif;color:#697078;margin:0 0 10px;}
    .adl-bib-actions{display:flex;flex-wrap:wrap;gap:10px 19px;margin-top:11px;}
    .adl-bib-actions a{font:600 12.5px/1.5 system-ui,-apple-system,sans-serif;text-decoration:none;color:#277366;border-bottom:1px solid transparent;}
    .adl-bib-actions a:hover,.adl-bib-actions a:focus{border-bottom-color:currentColor;text-decoration:none;}
    /* ADL_BIB_EDITORIAL_V3 — miniature first */
    .adl-bib-thumb{flex:0 0 114px;width:114px;height:151px;background:#eeeae1;box-shadow:0 2px 8px rgba(20,20,20,.1)}
    .adl-bib-thumb img{width:100%;height:100%;object-fit:contain;display:block}
    .adl-bib-title{font-family:Georgia,'Times New Roman',serif;font-size:20px;line-height:1.3;font-weight:600}
    .adl-bib-actions a{display:inline-flex;gap:5px;font-weight:600}
    @media(max-width:650px){.adl-bib-thumb{flex:0 0 85px;width:85px;height:113px}.adl-bib-title{font-size:17px}}
    @media(max-width:650px){.adl-bib-entry{gap:14px;padding:17px 0}.adl-bib-thumb{flex-basis:82px;width:82px;height:111px}.adl-bib-cover{padding:9px 6px 7px 9px}.adl-bib-cover-label{font-size:7px;margin-bottom:6px}.adl-bib-cover-title{font-size:10px}.adl-bib-cover-year{font-size:9px}.adl-bib-title{font-size:17px}.adl-bib-reference{font-size:13px}.adl-bib-eyebrow{font-size:9px}}
  "))),
    uiOutput("adl_bib_heading"),
    fluidRow(column(8,textInput("adl_bib_q","Cerca / Buscar",placeholder="Autor, títol, any...")),
      column(4,checkboxInput("adl_bib_links","Només amb enllaç / Solo con enlace",FALSE))),
    uiOutput("adl_bib_sections"))
}
adl_bib_server <- function(input,output,session,language){
  bib <- tryCatch(read.csv("data/bibliography_v1.csv",stringsAsFactors=FALSE,fileEncoding="UTF-8",check.names=FALSE),error=function(e)data.frame())
  kinds<-c("Estudis i bibliografia històrica","Arxius i fonts primàries","Premsa i hemerografia","Literatura i memòria","Audiovisuals i activitats","Recursos digitals i patrimoni")
  kind_es<-c("Estudios y bibliografía histórica","Archivos y fuentes primarias","Prensa y hemerografía","Literatura y memoria","Audiovisuales y actividades","Recursos digitales y patrimonio")
  output$adl_bib_heading<-renderUI({
    es<-identical(language(),"es")
    tagList(tags$h2(if(es)"Bibliografía" else "Bibliografia"),
      tags$p(if(es)"Bibliografía del manuscrito. Los enlaces a fichas documentales se publican solo tras validar la correspondencia y el acceso." else "Bibliografia del manuscrit. Els enllaços a fitxes documentals només es publiquen un cop validats la correspondència i l’accés."))
  })
  output$adl_bib_sections<-renderUI({
    es<-identical(language(),"es")
    if(!nrow(bib))return(tags$p("Catàleg no disponible / Catálogo no disponible"))
    labs<-if(es)kind_es else kinds
    do.call(navset_card_tab,c(list(id="adl_bib_type_tabs",nav_panel(if(es)"Totes" else "Totes",value="all",uiOutput("adl_bib_cards_all"))),
      lapply(seq_along(kinds),function(i)nav_panel(labs[i],value=paste0("type",i),uiOutput(paste0("adl_bib_cards_",i))))))
  })
  # ADL_BIB_EDITORIAL_V1 — preserve full references and verified links only.
  # ADL_BIB_INTERNAL_V1 — curated IDs with live public-record validation.
  internal_map <- tryCatch(read.csv("data/bibliography_internal_links.csv",stringsAsFactors=FALSE),error=function(e)data.frame())
  public_ids <- tryCatch({
    rr <- read_items()
    as.character(rr$item_id[rr$record_status %in% c("reviewed","publishable","published")])
  },error=function(e)character())
  render_cards<-function(type){
    es <- identical(language(),"es")
    d <- bib
    clean <- function(x) { if(length(x)==0 || is.na(x[[1]])) "" else trimws(as.character(x[[1]])) }
    # ADL_BIB_SEARCH_FLEXIBLE_V2
    # Cerca insensitive a caixa i accents, amb múltiples termes.
    normalize_search <- function(x) {
      x <- as.character(x)
      x[is.na(x)] <- ""
      # ADL_BIB_SEARCH_UNICODE_V3
      x <- stringi::stri_trans_general(x, "Latin-ASCII")
      x <- tolower(x)
      gsub("[^a-z0-9]+", " ", x)
    }

    q <- trimws(input$adl_bib_q %||% "")

    if(nzchar(q) && nrow(d)) {
      terms <- unlist(strsplit(
        trimws(normalize_search(q)), "\\s+"
      ))
      terms <- unique(terms[nzchar(terms)])

      if(length(terms)) {
        searchable <- vapply(seq_len(nrow(d)), function(i) {
          normalize_search(paste(
            d$autor[i],
            d$referencia[i],
            d$any[i],
            d$categoria[i],
            d$tipologia[i],
            d$nota_ca[i],
            d$nota_es[i],
            sep=" "
          ))
        }, character(1))

        matches <- vapply(searchable, function(s) {
          all(vapply(
            terms,
            function(t) grepl(t, s, fixed=TRUE),
            logical(1)
          ))
        }, logical(1))

        d <- d[matches, , drop=FALSE]
      }
    }
    if(nzchar(type)) d <- d[d$tipologia==type,,drop=FALSE]
    valid_external <- function(x) nzchar(x) && grepl("^https://[^[:space:]<>\"']+$",x) && !grepl("^(?i:https://(?:localhost|127\\.0\\.0\\.1))(?:[:/]|$)",x,perl=TRUE)
    valid_internal <- function(x,v) identical(tolower(v),"si") && nzchar(x) && grepl("^/[A-Za-z0-9_/?=&%-]+$",x) && !grepl("//|%2[fF]|%5[cC]",x)
    if(isTRUE(input$adl_bib_links)) {
      keep <- vapply(seq_len(nrow(d)),function(i){
        r <- d[i,,drop=FALSE]
        valid_external(clean(r$url)) || valid_internal(clean(r$fitxa_public_url),clean(r$fitxa_validada))
      },logical(1))
      d <- d[keep,,drop=FALSE]
    }
    if(!nrow(d)) return(tags$p(if(es) "Sin resultados" else "Sense resultats"))
    entries <- lapply(seq_len(nrow(d)),function(i){
      r <- d[i,,drop=FALSE]
      ref <- clean(r$referencia); author <- clean(r$autor); yr <- clean(r$any)
      title <- bib_card_parts(ref,author,yr)$title
      if(!nzchar(title)) title <- ref
      img <- clean(r$imatge_local)
      img_ok <- nzchar(img) && grepl("^bibliography/[A-Za-z0-9_.-]+\\.(jpg|jpeg|png|webp)$",img,ignore.case=TRUE) && file.exists(file.path("www",img))
      thumb <- if(img_ok) tags$img(src=img,alt=if(es) "Cubierta o reproducción bibliográfica" else "Coberta o reproducció bibliogràfica",loading="lazy") else {
        generated <- paste0("bibliography/generated/",clean(r$id),".svg")
        if(file.exists(file.path("www",generated))) {
          tags$img(src=generated,alt=if(es) "Miniatura bibliográfica ilustrativa" else "Miniatura bibliogràfica il·lustrativa",loading="lazy")
        } else tags$div(class="adl-bib-cover",tags$span(class="adl-bib-cover-title",title))
      }
      external <- clean(r$url); internal <- clean(r$fitxa_public_url)
      internal_ok <- valid_internal(internal,clean(r$fitxa_validada))
      mapped <- ""
      if(nrow(internal_map) && all(c("bib_id","item_id") %in% names(internal_map))) {
        hit <- internal_map$item_id[internal_map$bib_id==clean(r$id)]
        hit <- unique(as.character(hit))
        hit <- hit[!is.na(hit) & nzchar(hit) & hit %in% public_ids]
        if(length(hit)) {
          # Prefer an editorial/historical EV record over an asset ITM.
          preferred <- hit[grepl("^EV", hit)]
          mapped <- if(length(preferred)) preferred[[1L]] else hit[[1L]]
        }
      }
      external_ok <- valid_external(external)
      note <- if(es) clean(r$nota_es) else clean(r$nota_ca)
      kind <- clean(r$tipologia); kind_index <- match(kind,kinds)
      kind_label <- if(es && !is.na(kind_index)) kind_es[[kind_index]] else kind
      eyebrow <- paste(c(kind_label,yr)[nzchar(c(kind_label,yr))],collapse=" · ")
      tags$article(class="adl-bib-entry",
        tags$div(class="adl-bib-thumb",thumb),
        tags$div(class="adl-bib-info",
          if(nzchar(eyebrow))tags$div(class="adl-bib-eyebrow",eyebrow),
          tags$div(class="adl-bib-title",title),
          tags$p(class="adl-bib-reference",ref),
          if(nzchar(note))tags$p(class="adl-bib-note",note),
          if(internal_ok || external_ok || nzchar(mapped))tags$div(class="adl-bib-actions",
            if(external_ok)tags$a(href=external,target="_blank",rel="noopener noreferrer",if(es) "Fuente externa ↗" else "Font externa ↗"),
            if(nzchar(mapped))tags$a(href="#",onclick=paste0("Shiny.setInputValue('adl_bib_open_item',",jsonlite::toJSON(mapped,auto_unbox=TRUE),",{priority:'event'});return false;"),if(es) "Ficha documental →" else "Fitxa documental →")
            else if(internal_ok)tags$a(href=internal,if(es) "Ficha documental →" else "Fitxa documental →")
          )
        )
      )
    })
    tagList(tags$p(paste(nrow(d),if(es) "referencias" else "referències")),entries)
  }
  output$adl_bib_cards_all<-renderUI(render_cards(""))
  for(i in seq_along(kinds))local({k<-kinds[[i]];id<-paste0("adl_bib_cards_",i);output[[id]]<-renderUI(render_cards(k))})
}
