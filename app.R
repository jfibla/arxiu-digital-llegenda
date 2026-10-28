# XIPAHUATZIN_TOLORIU_SHINY_WIKI v0.9.0-i18n-safe
# CA canònic + ES públic; derivat de v0.8.13.2 PORTABLE
# Privacy + systematic document linkage

library(shiny)
library(bslib)
library(DT)

required_packages <- c("DBI","RSQLite","magick","pdftools","jsonlite","base64enc")
missing_required <- required_packages[!vapply(required_packages,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing_required)){
  stop("Falten paquets obligatoris: ",paste(missing_required,collapse=", "),
       ". Instal·la'ls amb install.packages(c('DBI','RSQLite')).")
}

DB_PATH <- "data/xipahuatzin_wiki.sqlite"
CATEGORIES_PATH <- "data/categories.csv"
RIGHTS_PATH <- "data/rights_policy.csv"
DERIVATIVES_PATH <- "data/derivatives_manifest.csv"
LINKAGE_MAP_PATH <- "data/linkage_map_v0_8_12.csv"
PORTABLE_LIBRARY_MANIFEST <- "data/portable_library_manifest_v0_8_13.csv"

ADMIN_ENABLED <- identical(Sys.getenv("XIPAHUATZIN_ENABLE_ADMIN", unset="0"), "1")

# PUBLIC_RUNTIME_DECOUPLE_V2_20261009
# Les rutes privades són exclusivament administratives.
admin_private_path <- function(...){
  if(!ADMIN_ENABLED)
    stop("Ruta privada sol·licitada amb l’administració desactivada.")
  file.path("private_sources",...)
}

read_utf8 <- function(path) read.csv(path,stringsAsFactors=FALSE,fileEncoding="UTF-8",check.names=FALSE)
trim_or_blank <- function(x) trimws(ifelse(is.na(x),"",as.character(x)))
# ---------------- i18n CA / ES ----------------
I18N <- list(
  ca=list(
    brand="Arxiu digital d’una llegenda", home="Inici", guide="Guia de lectura", records="Fitxes", timeline="Cronologia",
    hero_main="Xipahuatzin i Toloriu", hero_tail=" — Arxiu digital d’una llegenda",
    hero_alt="Paisatge del Cadí vist des de l'entorn de Toloriu",
    hero_caption="El Cadí, paisatge central de la llegenda.",
    intro1="Aquest arxiu digital reuneix i ordena les fonts que permeten seguir la formació, difusió, crítica i persistència de la llegenda que vincula Xipahuatzin, Joan de Grau i Toloriu.",
    intro2="L’objectiu no és presentar la llegenda com un relat tancat, sinó mostrar com es construeix documentalment al llarg del temps. Cada peça incorpora una descripció pròpia, el seu context, allò que aporta al relat, el seu valor documental i també les seves limitacions.",
    intro3="Les fitxes diferencien deliberadament entre el document original i la interpretació que se’n fa. La repetició d’una afirmació no es considera una confirmació independent, i els silencis documentals s’interpreten dins del conjunt de fonts disponibles.",
    intro4="La navegació per anys permet observar quan apareixen els diferents elements de la història, quins es consoliden, quins són qüestionats i quins formen part de la recepció cultural posterior.",
    year="Any", category="Categoria", all_f="Totes", all_m="Tots", person="Personatge", author="Autor", publication="Publicació / revista", place="Lloc", search="Cerca",
    record_card="Fitxa documental", item="Peça", chronological_order="Ordenació cronològica",
    timeline_help="Filtra la cronologia i clica una fila per obrir-ne la fitxa documental.",
    no_date="Sense data", no_records="No hi ha cap fitxa amb aquests filtres.",
    provenance="Procedència", type="Tipus", record="Fitxa", description="Descripció", summary="En síntesi",
    context_legend="Context i llegenda", historical_context="Context històric", legend_contribution="Aportació a la llegenda",
    documentary_assessment="Valoració documental", documentary_value="Valor documental", limitations="Limitacions",
    project_interpretation="Interpretació dins del projecte", relations_document="Relacions i document",
    people="Personatges", places="Llocs", claims="Afirmacions", events="Esdeveniments", documents_images="Documents i imatges",
    pending="[pendent]", no_reproduction="La fitxa és pública, però no hi ha cap reproducció documental autoritzada per a la vista pública.",
    no_inline="Actiu públic disponible sense previsualització inline.", previous="Anterior", next_label="Següent", associated="elements associats",
    col_year="Any", col_title="Títol", col_bib="Referència bibliogràfica", col_author="Autor / productor", col_category="Categoria"
  ),
  es=list(
    brand="Archivo digital de una leyenda", home="Inicio", guide="Guía de lectura", records="Fichas", timeline="Cronología",
    hero_main="Xipahuatzin y Toloriu", hero_tail=" — Archivo digital de una leyenda",
    hero_alt="Paisaje del Cadí visto desde el entorno de Toloriu",
    hero_caption="El Cadí, paisaje central de la leyenda.",
    intro1="Este archivo digital reúne y ordena las fuentes que permiten seguir la formación, difusión, crítica y persistencia de la leyenda que vincula a Xipahuatzin, Joan de Grau y Toloriu.",
    intro2="El objetivo no es presentar la leyenda como un relato cerrado, sino mostrar cómo se construye documentalmente a lo largo del tiempo. Cada pieza incorpora una descripción propia, su contexto, aquello que aporta al relato, su valor documental y también sus limitaciones.",
    intro3="Las fichas diferencian deliberadamente entre el documento original y la interpretación que se hace de él. La repetición de una afirmación no se considera una confirmación independiente, y los silencios documentales se interpretan dentro del conjunto de fuentes disponibles.",
    intro4="La navegación por años permite observar cuándo aparecen los distintos elementos de la historia, cuáles se consolidan, cuáles son cuestionados y cuáles forman parte de la recepción cultural posterior.",
    year="Año", category="Categoría", all_f="Todas", all_m="Todos", person="Personaje", author="Autor", publication="Publicación / revista", place="Lugar", search="Buscar",
    record_card="Ficha documental", item="Pieza", chronological_order="Ordenación cronológica",
    timeline_help="Filtra la cronología y haz clic en una fila para abrir su ficha documental.",
    no_date="Sin fecha", no_records="No hay ninguna ficha con estos filtros.",
    provenance="Procedencia", type="Tipo", record="Ficha", description="Descripción", summary="En síntesis",
    context_legend="Contexto y leyenda", historical_context="Contexto histórico", legend_contribution="Aportación a la leyenda",
    documentary_assessment="Valoración documental", documentary_value="Valor documental", limitations="Limitaciones",
    project_interpretation="Interpretación dentro del proyecto", relations_document="Relaciones y documento",
    people="Personajes", places="Lugares", claims="Afirmaciones", events="Acontecimientos", documents_images="Documentos e imágenes",
    pending="[pendiente]", no_reproduction="La ficha es pública, pero no hay ninguna reproducción documental autorizada para la vista pública.",
    no_inline="Activo público disponible, pero sin previsualización integrada.", previous="Anterior", next_label="Siguiente", associated="elementos asociados",
    col_year="Año", col_title="Título", col_bib="Referencia bibliográfica", col_author="Autor / productor", col_category="Categoría"
  )
)

tr <- function(key,lang="ca"){
  lang <- if(identical(lang,"es")) "es" else "ca"
  z <- I18N[[lang]][[key]]
  if(is.null(z)) I18N$ca[[key]] else z
}

category_label_i18n <- function(code,fallback="",lang="ca"){
  if(!identical(lang,"es")) return(fallback)
  es <- c(control="Control documental / ausencia",difusio="Difusión y fijación",critica="Crítica / impugnación",context="Contexto",contrast="Contraste")
  if(code %in% names(es)) unname(es[[code]]) else fallback
}

localized_value <- function(r,field,lang="ca"){
  base <- if(field %in% names(r)) trim_or_blank(r[[field]][1]) else ""
  if(identical(lang,"es")){
    f2 <- paste0(field,"_es")
    if(f2 %in% names(r)){
      z <- trim_or_blank(r[[f2]][1])
      if(nzchar(z)) return(z)
    }
  }
  base
}

localized_col <- function(x,field,lang="ca"){
  if(!nrow(x)) return(character(0))
  base <- if(field %in% names(x)) trim_or_blank(x[[field]]) else rep("",nrow(x))
  if(identical(lang,"es")){
    f2 <- paste0(field,"_es")
    if(f2 %in% names(x)){
      z <- trim_or_blank(x[[f2]])
      use <- nzchar(z)
      base[use] <- z[use]
    }
  }
  base
}


source("R/guide_content.R", local=TRUE)
# ADL_BIBLIO_V1
source("R/bibliography_v1.R", local=TRUE)
# MASTER_TEST_AUDIT_PATCH_V1
source("R/master_test_audit.R", local=TRUE)
source("R/usage_conditions.R", local=TRUE)
source("R/external_bib_links.R", local=TRUE)

categories <- read_utf8(CATEGORIES_PATH)
rights_policy <- read_utf8(RIGHTS_PATH)
derivatives <- read_utf8(DERIVATIVES_PATH)


db_connect <- function(){
  DBI::dbConnect(RSQLite::SQLite(),DB_PATH)
}

ensure_i18n_columns <- function(){
  con <- db_connect()
  on.exit(DBI::dbDisconnect(con),add=TRUE)
  if(!DBI::dbExistsTable(con,"items")) return(invisible(FALSE))
  existing <- DBI::dbListFields(con,"items")
  needed <- c("title_es","item_type_es","description_es","historical_context_es","legend_contribution_es","documentary_value_es","limitations_es","project_interpretation_es","related_claims_es","related_events_es")
  for(nm in needed){
    if(!(nm %in% existing)){
      DBI::dbExecute(con,paste0('ALTER TABLE items ADD COLUMN "',nm,'" TEXT DEFAULT \'\''))
    }
  }
  invisible(TRUE)
}

backup_before_v0813 <- function(){
  marker <- "data/.migration_v0_8_13_started"
  if(file.exists(marker)) return(invisible(FALSE))
  dir.create(file.path("data","backups"),recursive=TRUE,showWarnings=FALSE)
  stamp <- format(Sys.time(),"%Y%m%d_%H%M%S")
  dest <- file.path("data","backups",paste0("xipahuatzin_wiki_pre_v0_8_13_",stamp,".sqlite"))
  if(file.exists(DB_PATH)){
    ok <- file.copy(DB_PATH,dest,overwrite=FALSE)
    if(!isTRUE(ok)) stop("No s'ha pogut crear la còpia de seguretat prèvia a v0.8.13.2.")
  }
  writeLines(
    c(paste0("started_at=",format(Sys.time(),"%Y-%m-%d %H:%M:%S")),
      paste0("backup=",dest)),
    marker,useBytes=TRUE
  )
  invisible(TRUE)
}

ensure_asset_table <- function(){
  con <- db_connect()
  on.exit(DBI::dbDisconnect(con),add=TRUE)

  DBI::dbExecute(con,"
    CREATE TABLE IF NOT EXISTS item_assets (
      asset_id INTEGER PRIMARY KEY AUTOINCREMENT,
      item_id TEXT NOT NULL,
      asset_type TEXT NOT NULL DEFAULT 'image',
      title TEXT DEFAULT '',
      caption TEXT DEFAULT '',
      author_or_creator TEXT DEFAULT '',
      document_date TEXT DEFAULT '',
      rights_status TEXT DEFAULT 'RIGHTS_UNKNOWN',
      copyright_holder TEXT DEFAULT '',
      permission_source TEXT DEFAULT '',
      public_display TEXT DEFAULT 'none',
      public_download TEXT DEFAULT 'no',
      private_source TEXT DEFAULT '',
      public_asset TEXT DEFAULT '',
      pixelated_preview TEXT DEFAULT '',
      sort_order INTEGER DEFAULT 1,
      notes TEXT DEFAULT '',
      created_at TEXT DEFAULT '',
      created_by TEXT DEFAULT '',
      source_item_id TEXT DEFAULT ''
    )
  ")

  f <- DBI::dbListFields(con,"item_assets")
  if(!"source_item_id" %in% f)
    DBI::dbExecute(con,"ALTER TABLE item_assets ADD COLUMN source_item_id TEXT DEFAULT ''")

  DBI::dbExecute(con,"CREATE INDEX IF NOT EXISTS idx_item_assets_item_id ON item_assets(item_id)")
  DBI::dbExecute(con,"CREATE INDEX IF NOT EXISTS idx_item_assets_source_item_id ON item_assets(source_item_id)")

  DBI::dbExecute(con,"
    CREATE TABLE IF NOT EXISTS item_links (
      link_id INTEGER PRIMARY KEY AUTOINCREMENT,
      source_item_id TEXT NOT NULL,
      target_item_id TEXT NOT NULL,
      link_type TEXT NOT NULL DEFAULT 'evidence_asset',
      asset_id INTEGER,
      notes TEXT DEFAULT '',
      created_at TEXT DEFAULT '',
      created_by TEXT DEFAULT ''
    )
  ")
  DBI::dbExecute(con,"CREATE INDEX IF NOT EXISTS idx_item_links_source ON item_links(source_item_id)")
  DBI::dbExecute(con,"CREATE INDEX IF NOT EXISTS idx_item_links_target ON item_links(target_item_id)")

  if(DBI::dbExistsTable(con,"app_meta")){
    DBI::dbExecute(con,
      "INSERT OR REPLACE INTO app_meta(key,value) VALUES('schema_version','0.8.13')"
    )
    DBI::dbExecute(con,
      "INSERT OR REPLACE INTO app_meta(key,value) VALUES('document_linkage','item_links + source_item_id')"
    )
    DBI::dbExecute(con,
      "INSERT OR REPLACE INTO app_meta(key,value) VALUES('restricted_assets_policy','admin_only')"
    )
    DBI::dbExecute(con,
      "INSERT OR REPLACE INTO app_meta(key,value) VALUES('portable_library','private_sources/library')"
    )
    DBI::dbExecute(con,
      "INSERT OR REPLACE INTO app_meta(key,value) VALUES('external_document_dependency','none')"
    )
  }
}

read_item_assets <- function(item_id){
  con <- db_connect()
  on.exit(DBI::dbDisconnect(con),add=TRUE)
  if(!DBI::dbExistsTable(con,"item_assets")) return(data.frame())
  x <- DBI::dbGetQuery(
    con,
    "SELECT * FROM item_assets WHERE item_id=? ORDER BY sort_order, asset_id",
    params=list(item_id)
  )
  if(nrow(x)) x[] <- lapply(x,function(z) ifelse(is.na(z),"",as.character(z)))
  x
}

asset_browser_path <- function(path){
  p <- trim_or_blank(path)
  if(!nzchar(p)) return(NA_character_)
  prefix <- "www/"
  rel <- if(startsWith(p,prefix)) substring(p,nchar(prefix)+1) else p
  if(file.exists(file.path("www",rel))) rel else NA_character_
}


sync_portable_library_registry <- function(){
  if(!file.exists(PORTABLE_LIBRARY_MANIFEST)) return(invisible(FALSE))
  man <- read_utf8(PORTABLE_LIBRARY_MANIFEST)
  need <- c("item_id","file_name","portable_source")
  if(!all(need %in% names(man))) return(invisible(FALSE))

  con <- db_connect()
  on.exit(DBI::dbDisconnect(con),add=TRUE)

  for(i in seq_len(nrow(man))){
    id <- trim_or_blank(man$item_id[i])
    p <- trim_or_blank(man$portable_source[i])
    fn <- trim_or_blank(man$file_name[i])
    if(!nzchar(id) || !nzchar(p) || !file.exists(p)) next
    DBI::dbExecute(
      con,
      "UPDATE items
       SET portable_source=?, file_name=CASE WHEN trim(COALESCE(file_name,''))='' THEN ? ELSE file_name END
       WHERE item_id=? AND record_status='draft'",
      params=list(gsub("\\\\","/",p),fn,id)
    )
  }

  if(DBI::dbExistsTable(con,"app_meta")){
    n_ok <- DBI::dbGetQuery(
      con,
      "SELECT COUNT(*) AS n FROM items
       WHERE record_status='draft'
       AND trim(COALESCE(portable_source,''))<>''"
    )$n[1]
    DBI::dbExecute(
      con,
      "INSERT OR REPLACE INTO app_meta(key,value) VALUES('portable_library_registered',?)",
      params=list(as.character(n_ok))
    )
  }
  invisible(TRUE)
}

portable_library_status <- function(){
  con <- db_connect()
  on.exit(DBI::dbDisconnect(con),add=TRUE)
  x <- DBI::dbGetQuery(
    con,
    "SELECT item_id,file_name,portable_source FROM items WHERE record_status='draft' ORDER BY item_id"
  )
  if(!nrow(x)) return(list(total=0,found=0,missing=character(0)))
  found <- vapply(seq_len(nrow(x)),function(i){
    p <- trim_or_blank(x$portable_source[i])
    nzchar(p) && file.exists(p)
  },logical(1))
  list(total=nrow(x),found=sum(found),missing=x$item_id[!found])
}



# ========================================================================
# PERMISSION_WORKFLOW_V1_20261007
# Càrrega preautorització + registre i aplicació d'autoritzacions
# ========================================================================

ensure_permission_workflow_tables <- function(){
  con <- db_connect()
  on.exit(DBI::dbDisconnect(con),add=TRUE)

  DBI::dbExecute(con,"
    CREATE TABLE IF NOT EXISTS permissions (
      permission_id TEXT PRIMARY KEY,
      rights_status TEXT NOT NULL DEFAULT 'PERMISSION_PENDING',
      rights_holder TEXT DEFAULT '',
      authorizing_person TEXT DEFAULT '',
      authorizing_role TEXT DEFAULT '',
      permission_date TEXT DEFAULT '',
      scope TEXT DEFAULT '',
      allowed_display TEXT DEFAULT 'none',
      public_download TEXT DEFAULT 'no',
      required_credit TEXT DEFAULT '',
      conditions_text TEXT DEFAULT '',
      evidence_private_source TEXT DEFAULT '',
      evidence_file_name TEXT DEFAULT '',
      notes TEXT DEFAULT '',
      created_at TEXT DEFAULT '',
      created_by TEXT DEFAULT ''
    )
  ")

  DBI::dbExecute(con,"
    CREATE TABLE IF NOT EXISTS permission_assets (
      permission_asset_id INTEGER PRIMARY KEY AUTOINCREMENT,
      permission_id TEXT NOT NULL,
      asset_id INTEGER,
      source_item_id TEXT DEFAULT '',
      target_item_id TEXT DEFAULT '',
      applied_at TEXT DEFAULT '',
      applied_by TEXT DEFAULT '',
      UNIQUE(permission_id, asset_id)
    )
  ")

  DBI::dbExecute(con,
    "CREATE INDEX IF NOT EXISTS idx_permission_assets_perm ON permission_assets(permission_id)")
  DBI::dbExecute(con,
    "CREATE INDEX IF NOT EXISTS idx_permission_assets_asset ON permission_assets(asset_id)")

  if(DBI::dbExistsTable(con,"app_meta")){
    DBI::dbExecute(con,
      "INSERT OR REPLACE INTO app_meta(key,value) VALUES('permission_workflow','v1_20261007')")
  }
  invisible(TRUE)
}

permission_next_id <- function(con){
  x <- DBI::dbGetQuery(con,"SELECT permission_id FROM permissions")
  nums <- suppressWarnings(as.integer(sub("^PERM_([0-9]+).*$","\\1",x$permission_id)))
  n <- if(length(nums) && any(!is.na(nums))) max(nums,na.rm=TRUE)+1L else 1L
  paste0("PERM_",sprintf("%04d",n))
}

next_itm_id <- function(con, stub="DOCUMENT"){
  x <- DBI::dbGetQuery(con,"SELECT item_id FROM items WHERE item_id LIKE 'ITM_%'")
  nums <- suppressWarnings(as.integer(sub("^ITM_([0-9]+).*$","\\1",x$item_id)))
  n <- if(length(nums) && any(!is.na(nums))) max(nums,na.rm=TRUE)+1L else 1L
  clean <- toupper(gsub("[^A-Za-z0-9]+","_",stub))
  clean <- gsub("^_+|_+$","",clean)
  if(!nzchar(clean)) clean <- "DOCUMENT"
  clean <- substr(clean,1,48)
  paste0("ITM_",sprintf("%03d",n),"_",clean)
}

append_portable_manifest_row <- function(item_id,file_name,portable_source){
  if(!file.exists(PORTABLE_LIBRARY_MANIFEST)) return(invisible(FALSE))
  man <- read_utf8(PORTABLE_LIBRARY_MANIFEST)
  z <- data.frame(
    item_id=item_id,file_name=file_name,portable_source=portable_source,
    stringsAsFactors=FALSE
  )
  man <- man[man$item_id!=item_id,,drop=FALSE]
  man <- rbind(man,z)
  write.csv(man,PORTABLE_LIBRARY_MANIFEST,row.names=FALSE,fileEncoding="UTF-8",na="")
  invisible(TRUE)
}

insert_minimal_draft_item <- function(con,item_id,title,file_name,portable_source,
                                      author="",document_date="",source_or_archive="",
                                      rights_status="PERMISSION_PENDING",
                                      copyright_holder="",notes=""){
  if(DBI::dbGetQuery(con,"SELECT COUNT(*) AS n FROM items WHERE item_id=?",
                     params=list(item_id))$n[1] > 0) return(invisible(FALSE))

  info <- DBI::dbGetQuery(con,"PRAGMA table_info(items)")
  fields <- info$name
  row <- as.list(setNames(vector("list",length(fields)),fields))

  for(i in seq_len(nrow(info))){
    typ <- toupper(ifelse(is.na(info$type[i]),"",info$type[i]))
    row[[info$name[i]]] <- if(grepl("INT|REAL|NUM|DOUBLE|FLOAT",typ)) 0 else ""
  }

  setv <- function(nm,val){
    if(nm %in% fields) row[[nm]] <<- val
  }

  setv("item_id",item_id)
  setv("title",title)
  setv("item_type","Document / font")
  setv("author_or_creator",author)
  setv("document_date",document_date)
  setv("source_or_archive",source_or_archive)
  setv("bibliographic_reference",source_or_archive)
  setv("description","Document incorporat en fase de preautorització.")
  setv("documentary_value","Original privat vinculat a una fitxa documental.")
  setv("limitations","Reproducció pública pendent de resolució de drets.")
  setv("project_interpretation",notes)
  setv("category","difusio")
  setv("record_status","draft")
  setv("rights_status",rights_status)
  setv("copyright_holder",copyright_holder)
  setv("public_display","none")
  setv("public_download","no")
  setv("file_name",file_name)
  setv("portable_source",portable_source)
  setv("dropbox_path",portable_source)
  setv("confidence","high")
  setv("recommended_action","Esperar autorització o resoldre drets.")

  row <- row[fields]
  sql <- paste0(
    "INSERT INTO items (",
    paste(DBI::dbQuoteIdentifier(con,names(row)),collapse=","),
    ") VALUES (",paste(rep("?",length(row)),collapse=","),")"
  )
  DBI::dbExecute(con,sql,params=unname(row))
  invisible(TRUE)
}

permission_guess_from_text <- function(x){
  x0 <- trim_or_blank(x)
  low <- tolower(x0)

  status <- if(grepl("no autoritz|deneg|no podem autoritz|no damos permiso|no conced",low))
    "PERMISSION_DENIED"
  else if(grepl("autoritz|perm[ií]s|permiso|permission|cap problema|podeu reproduir|puede reproduc",low))
    "PERMISSION_GRANTED"
  else "PERMISSION_PENDING"

  display <- if(status!="PERMISSION_GRANTED") "none"
  else if(grepl("fragment|extracte|extracto|excerpt",low)) "excerpt"
  else if(grepl("miniatura|thumbnail",low)) "thumbnail"
  else "full"

  download <- if(grepl("desc[aà]rrega|download",low) &&
                 grepl("permet|autoritz|sí|si ",low)) "yes" else "no"

  credit <- ""
  if(grepl("refer[eè]ncia.*clara|citar|citaci|cr[eè]dit|menci[oó]",low)){
    credit <- "Cal indicar de manera clara la font i/o el titular dels drets."
  }

  date <- ""
  m <- regmatches(x0,regexpr("\\b[0-3]?[0-9][/-][01]?[0-9][/-](20)?[0-9]{2}\\b",x0,perl=TRUE))
  if(length(m) && nzchar(m)) date <- m

  list(status=status,display=display,download=download,credit=credit,date=date)
}

ensure_permission_workflow_tables()

# ========================================================================
# FI PERMISSION_WORKFLOW_V1_20261007
# ========================================================================


if(ADMIN_ENABLED) backup_before_v0813()
ensure_asset_table()
ensure_permission_workflow_tables()
ensure_i18n_columns()
if(ADMIN_ENABLED) sync_portable_library_registry()
read_items <- function(){
  con <- db_connect()
  on.exit(DBI::dbDisconnect(con),add=TRUE)
  x <- DBI::dbReadTable(con,"items",check.names=FALSE)
  x[] <- lapply(x,function(z) ifelse(is.na(z),"",as.character(z)))
  x
}


source("R/surname_labels.R", local=TRUE)

split_semicolon_values <- function(x){
  x <- trim_or_blank(x)
  x <- x[nzchar(x)]
  if(!length(x)) return(character(0))
  vals <- unlist(strsplit(x,";"))
  vals <- gsub("\\s+"," ",trimws(vals))
  vals <- vals[nzchar(vals)]
  vals <- vals[order(tolower(vals))]
  vals[!duplicated(tolower(vals))]
}

single_value_list <- function(x){
  vals <- gsub("\\s+"," ",trim_or_blank(x))
  vals <- vals[nzchar(vals)]
  vals <- vals[order(tolower(vals))]
  vals[!duplicated(tolower(vals))]
}

legend_people_values <- function(x){
  people <- split_semicolon_values(x$related_people)
  authors <- split_semicolon_values(x$author_or_creator)
  
  if(!length(people) || !length(authors)) return(people)
  
  people[!tolower(people) %in% tolower(authors)]
}

extract_year <- function(date_value,file_name,title){
  d <- trim_or_blank(date_value)
  if(grepl("^[0-9]{4}",d)) return(substr(d,1,4))
  blob <- paste(file_name,title)
  m <- regmatches(blob,regexpr("(18|19|20)[0-9]{2}",blob,perl=TRUE))
  if(length(m) && nzchar(m)) return(m)
  ""
}

decorate_items <- function(x){
  x$display_year <- mapply(extract_year,x$document_date,x$file_name,x$title,USE.NAMES=FALSE)
  x$display_year_num <- suppressWarnings(as.integer(x$display_year))
  x$display_year_label <- ifelse(is.na(x$display_year_num)|!nzchar(x$display_year),"Sense data",x$display_year)

  # ADL_CONTRAST_XVI_CHRONO_V1
  # Ordering key only: not an invented event date.
  # The record retains document_date = "segle XVI".
  is_xvi_synthesis <- x$item_id == "NEG_CADENA_BIOGRAFICA_NO_DOCUMENTADA"
  is_xvi_synthesis[is.na(is_xvi_synthesis)] <- FALSE
  x$display_year_num[is_xvi_synthesis] <- 1500L
  x$display_year[is_xvi_synthesis] <- "1500"
  x$display_year_label[is_xvi_synthesis] <- "segle XVI"
  x
}

cat_info <- function(code,lang="ca"){
  x <- categories[categories$category==code,,drop=FALSE]
  if(nrow(x)==0) return(list(label=code,bg="#FFFFFF",fg="#222222"))
  list(label=category_label_i18n(code,x$category_label[1],lang),bg=x$category_color[1],fg=x$category_text_color[1])
}


PUBLIC_RIGHTS <- c("PUBLIC_DOMAIN","OPEN_LICENSE","PERMISSION_GRANTED")

public_rights_allowed <- function(rights_status,public_display){
  trim_or_blank(rights_status) %in% PUBLIC_RIGHTS &&
    trim_or_blank(public_display) %in% c("thumbnail","excerpt","full")
}

mime_for_path <- function(path){
  ext <- tolower(tools::file_ext(path))
  switch(ext,
    jpg="image/jpeg", jpeg="image/jpeg", png="image/png",
    gif="image/gif", webp="image/webp", tif="image/tiff", tiff="image/tiff",
    pdf="application/pdf",
    "application/octet-stream"
  )
}

# PDF_PRESENTATION_V1_20261007
PDF_CAROUSEL_MAX_PAGES <- 40L
# UNIFIED_PDF_VIEWER_V1_20261008

pdf_page_count_safe <- function(path){
  tryCatch(as.integer(pdftools::pdf_info(path)$pages),error=function(e) NA_integer_)
}

# PDF_VIEWER_NO_NESTED_BOOTSTRAP_V1_20261008
pdf_page_carousel_tag <- function(path,label=NULL,lang="ca"){
  n <- pdf_page_count_safe(path)
  if(is.na(n) || n < 1L) return(NULL)

  # IMPORTANT: aquest visor NO usa Bootstrap carousel internament.
  # render_asset_carousel() ja és un carousel Bootstrap; niar-ne un altre
  # provocava que, en canviar de document, desapareguessin les pàgines.
  cid <- paste0(
    "pdfviewer_",
    gsub("[^A-Za-z0-9]","_",basename(path)),"_",
    abs(sum(utf8ToInt(path))) %% 1000000,"_",
    sample.int(999999L,1L)
  )

  pages <- lapply(seq_len(n),function(i){
    tmp <- tempfile(fileext=".jpg")
    bmp <- pdftools::pdf_render_page(path,page=i,dpi=150)
    img <- magick::image_read(bmp)
    magick::image_write(img,tmp,format="jpg",quality=88)
    uri <- base64enc::dataURI(file=tmp,mime="image/jpeg")
    unlink(tmp)

    tags$div(
      class="pdf-page-frame",
      `data-page`=i,
      style=if(i==1L) "display:block;width:100%;" else "display:none;width:100%;",
      tags$div(
        style="min-height:760px;display:flex;align-items:center;justify-content:center;background:#f8f8f8;padding:1rem;width:100%;",
        tags$img(
          src=uri,
          style="display:block;width:auto;max-width:96%;height:auto;max-height:88vh;min-height:680px;object-fit:contain;margin:0 auto;"
        )
      )
    )
  })

  nav_js <- function(delta){
    sprintf(
      "(function(){var root=document.getElementById('%s');if(!root)return;var pages=root.querySelectorAll('.pdf-page-frame');if(!pages.length)return;var cur=0;for(var i=0;i<pages.length;i++){if(pages[i].style.display!=='none'){cur=i;break;}}var nxt=(cur+(%d)+pages.length)%%pages.length;pages[cur].style.display='none';pages[nxt].style.display='block';var c=root.querySelector('.pdf-page-counter-current');if(c)c.textContent=(nxt+1);})();",
      cid,as.integer(delta)
    )
  }

  tags$div(
    class="pdf-page-viewer-wrap",
    style="width:100%;",
    if(!is.null(label) && nzchar(trim_or_blank(label)))
      tags$div(class="alert alert-warning",tags$b(label)),
    tags$div(
      id=cid,
      class="pdf-page-viewer",
      style="width:100%;",
      tags$div(class="pdf-page-stack",style="width:100%;",pages),
      if(n>1L)
        tags$div(
          style="display:flex;justify-content:center;align-items:center;gap:1rem;margin-top:0.65rem;",
          tags$button(
            type="button",class="btn btn-outline-secondary btn-sm",
            onclick=nav_js(-1L),
            paste0("\u2039 ",if(identical(lang,"es")) "Anterior" else "Anterior")
          ),
          tags$span(
            class="text-muted",
            tags$span(class="pdf-page-counter-current","1"),
            paste0(" / ",n," ",if(identical(lang,"es")) "páginas" else "pàgines")
          ),
          tags$button(
            type="button",class="btn btn-outline-secondary btn-sm",
            onclick=nav_js(1L),
            paste0(if(identical(lang,"es")) "Siguiente" else "Següent"," \u203a")
          )
        )
    )
  )
}

pdf_document_preview_tag <- function(path,browser_src=NULL,label=NULL,lang="ca"){
  n <- pdf_page_count_safe(path)

  if(!is.na(n) && n <= PDF_CAROUSEL_MAX_PAGES){
    return(pdf_page_carousel_tag(path,label=label,lang=lang))
  }

  src <- browser_src
  if(is.null(src) || !nzchar(trim_or_blank(src))){
    src <- base64enc::dataURI(file=path,mime="application/pdf")
  }
  src <- paste0(src,"#zoom=page-width")

  tags$div(
    if(!is.null(label) && nzchar(trim_or_blank(label)))
      tags$div(class="alert alert-warning",tags$b(label)),
    tags$iframe(
      src=src,
      style="display:block;width:96%;height:88vh;min-height:850px;max-height:1150px;border:1px solid #ccc;margin:0 auto;background:white;"
    )
  )
}

browser_pdf_preview_tag <- function(browser_path,lang="ca"){
  bp <- trim_or_blank(browser_path)
  local <- file.path("www",bp)
  if(file.exists(local)){
    return(pdf_document_preview_tag(local,browser_src=bp,label=NULL,lang=lang))
  }
  tags$iframe(
    src=paste0(bp,"#zoom=page-width"),
    style="display:block;width:96%;height:88vh;min-height:850px;max-height:1150px;border:1px solid #ccc;margin:0 auto;background:white;"
  )
}

private_preview_tag <- function(path,label="Original privat — només administració"){
  p <- trim_or_blank(path)
  if(!nzchar(p) || !file.exists(p))
    return(tags$div(class="alert alert-secondary","Original privat no disponible dins del paquet portable."))

  ext <- tolower(tools::file_ext(p))

  if(ext %in% c("jpg","jpeg","png","gif","webp","tif","tiff")){
    tmp <- tempfile(fileext=".jpg")
    img <- magick::image_read(p)[1]
    img <- magick::image_resize(img,"2200x2200>")
    magick::image_write(img,tmp,format="jpg",quality=88)
    uri <- base64enc::dataURI(file=tmp,mime="image/jpeg")
    unlink(tmp)
    return(tags$div(
      tags$div(class="alert alert-warning",tags$b(label)),
      tags$div(
        style="display:flex;justify-content:center;background:#f8f8f8;padding:1rem;",
        tags$img(
          src=uri,
          style="display:block;width:auto;max-width:96%;height:auto;max-height:88vh;min-height:680px;object-fit:contain;margin:0 auto;"
        )
      )
    ))
  }

  if(ext=="pdf"){
    return(pdf_document_preview_tag(p,label=label,lang="ca"))
  }

  tags$div(
    class="alert alert-info",
    tags$b(label),
    tags$div("Format conservat en privat sense previsualització inline.")
  )
}

quarantine_restricted_assets <- function(){
  con <- db_connect()
  on.exit(DBI::dbDisconnect(con),add=TRUE)
  dir.create("private_previews",recursive=TRUE,showWarnings=FALSE)

  move_out_of_www <- function(p,stub){
    p <- trim_or_blank(p)
    if(!nzchar(p) || !file.exists(p)) return(p)
    np <- normalizePath(p,mustWork=FALSE)
    www_root <- normalizePath("www",mustWork=FALSE)
    if(!startsWith(np,www_root)) return(p)
    ext <- tools::file_ext(p)
    dest <- file.path(
      "private_previews",
      paste0(gsub("[^A-Za-z0-9._-]+","_",stub),
             if(nzchar(ext)) paste0(".",tolower(ext)) else "")
    )
    ok <- file.copy(p,dest,overwrite=TRUE)
    if(!isTRUE(ok))
      stop("No s\'ha pogut retirar un actiu restringit de www/: ",p)
    unlink(p)
    gsub("\\\\","/",dest)
  }

  # Legacy item-level derivatives.
  if(DBI::dbExistsTable(con,"items")){
    x <- DBI::dbGetQuery(con,"
      SELECT item_id,rights_status,public_display,public_asset,pixelated_preview
      FROM items
      WHERE rights_status NOT IN ('PUBLIC_DOMAIN','OPEN_LICENSE','PERMISSION_GRANTED')
    ")
    if(nrow(x)){
      for(i in seq_len(nrow(x))){
        old_pub <- trim_or_blank(x$public_asset[i])
        old_px  <- trim_or_blank(x$pixelated_preview[i])
        new_pub <- move_out_of_www(old_pub,paste0(x$item_id[i],"_restricted_asset"))
        new_px  <- move_out_of_www(old_px,paste0(x$item_id[i],"_restricted_preview"))
        # Public references are cleared. A moved preview is retained as a private path.
        DBI::dbExecute(con,
          "UPDATE items SET public_asset='', pixelated_preview=? WHERE item_id=?",
          params=list(ifelse(identical(new_px,old_px) && startsWith(old_px,"www/"),"",new_px),x$item_id[i])
        )
      }
    }
  }

  # Multi-assets.
  if(DBI::dbExistsTable(con,"item_assets")){
    a <- DBI::dbGetQuery(con,"
      SELECT asset_id,item_id,rights_status,public_asset,pixelated_preview
      FROM item_assets
      WHERE rights_status NOT IN ('PUBLIC_DOMAIN','OPEN_LICENSE','PERMISSION_GRANTED')
    ")
    if(nrow(a)){
      for(i in seq_len(nrow(a))){
        old_pub <- trim_or_blank(a$public_asset[i])
        old_px  <- trim_or_blank(a$pixelated_preview[i])
        new_pub <- move_out_of_www(old_pub,paste0(a$item_id[i],"_asset_",a$asset_id[i],"_restricted"))
        new_px  <- move_out_of_www(old_px,paste0(a$item_id[i],"_asset_",a$asset_id[i],"_preview"))
        DBI::dbExecute(con,
          "UPDATE item_assets SET public_asset='', pixelated_preview=? WHERE asset_id=?",
          params=list(ifelse(identical(new_px,old_px) && startsWith(old_px,"www/"),"",new_px),a$asset_id[i])
        )
      }
    }
  }
}

if(ADMIN_ENABLED) quarantine_restricted_assets()
safe_asset_path <- function(row){
  if(!public_rights_allowed(row$rights_status,row$public_display))
    return(NA_character_)

  p <- trim_or_blank(row$public_asset)
  if(nzchar(p)){
    prefix <- "www/"
    rel <- if(startsWith(p,prefix)) substring(p,nchar(prefix)+1) else p
    if(file.exists(file.path("www",rel))) return(rel)
  }

  d <- derivatives[derivatives$item_id==row$item_id,,drop=FALSE]
  if(nrow(d)==0) return(NA_character_)
  fn <- d$derivative_file[1]
  dirs <- c(full="public",thumbnail="thumbnails",excerpt="excerpts")
  if(row$public_display %in% names(dirs)){
    p2 <- file.path("www",dirs[[row$public_display]],fn)
    if(file.exists(p2)) return(paste0(dirs[[row$public_display]],"/",fn))
  }
  NA_character_
}

section_box <- function(title,body){
  tags$section(
    class="record-section",
    tags$div(class="record-section-title",title),
    tags$div(class="record-section-body",body)
  )
}


# PUBLIC_PERMISSION_NOTE_V1_20261007
public_permission_note <- function(asset_id,lang="ca"){
  con <- db_connect()
  on.exit(DBI::dbDisconnect(con),add=TRUE)

  if(!DBI::dbExistsTable(con,"permission_assets") ||
     !DBI::dbExistsTable(con,"permissions"))
    return("")

  q <- DBI::dbGetQuery(con,"
    SELECT p.authorizing_person,p.authorizing_role,p.permission_date,
           p.rights_holder,p.required_credit,p.permission_id
    FROM permission_assets pa
    JOIN permissions p ON p.permission_id=pa.permission_id
    WHERE pa.asset_id=?
      AND p.rights_status='PERMISSION_GRANTED'
    ORDER BY pa.permission_asset_id DESC
    LIMIT 1
  ",params=list(as.integer(asset_id)))

  if(!nrow(q)) return("")

  person <- trim_or_blank(q$authorizing_person[1])
  role <- trim_or_blank(q$authorizing_role[1])
  date <- trim_or_blank(q$permission_date[1])
  holder <- trim_or_blank(q$rights_holder[1])

  who <- if(nzchar(person)) person else holder
  if(!nzchar(who)) return("")

  if(identical(lang,"es")){
    txt <- paste0("Reproducción autorizada por ",who)
    if(nzchar(role)) txt <- paste0(txt,", ",role)
    if(nzchar(date)) txt <- paste0(txt," (",date,")")
    paste0(txt,".")
  } else {
    txt <- paste0("Reproducció autoritzada per ",who)
    if(nzchar(role)) txt <- paste0(txt,", ",role)
    if(nzchar(date)) txt <- paste0(txt," (",date,")")
    paste0(txt,".")
  }
}
# FI PUBLIC_PERMISSION_NOTE_V1_20261007

# EDITORIAL_CREDITS_GLOBAL_V1 — presentació; font de veritat = SQLite.
# No inferir permisos ni titulars a partir de noms d'arxiu.
# MASTER_CREDITS_V2: source of truth is the sole canonical CSV.
asset_editorial_credits <- function(a,r,lang="ca"){
  cv <- function(x){if(is.null(x)||!length(x)||is.na(x[1])) "" else trimws(as.character(x[1]))}
  es <- identical(lang,"es")
  id <- cv(a$asset_id)
  fn <- "AUTORITZACIONS/MASTER/MASTER_AUTORITZACIONS.csv"
  if(!file.exists(fn)) stop("MASTER_AUTORITZACIONS.csv missing; no alternate rights source")
  m <- read.csv2(fn,stringsAsFactors=FALSE,check.names=FALSE,fileEncoding="UTF-8")
  required <- c("asset_id","item_id","status_master","original_asset_status","rights_holder",
                "authorizing_person","permission_date","conditions_text","scope","required_credit")
  if(!all(required %in% names(m))) stop("MASTER_AUTH_SCHEMA_INCOMPLETE")
  z <- m[as.character(m$asset_id)==id & as.character(m$item_id)==cv(r$item_id),,drop=FALSE]
  if(nrow(z)>1L) stop(paste0("MASTER_AUTH_DUPLICATE_ASSET_",id))
  val <- function(field){if(nrow(z)) cv(z[[field]]) else ""}
  status <- val("status_master")
  # Source state is NOT automatically a new grant: it is used for presentation only
  # for existing asset explicitly marked granted or public domain in canonical inventory.
  if(!nzchar(status) && val("original_asset_status") %in% c("PERMISSION_GRANTED","PUBLIC_DOMAIN"))
    status <- val("original_asset_status")
  bib <- cv(r$bibliographic_reference)
  if(!nzchar(bib)) bib <- cv(r$publication_title)
  author <- cv(a$author_or_creator)
  if(!nzchar(author)) author <- cv(r$author_or_creator)
  provenance <- cv(a$permission_source)
  if(!nzchar(provenance)) provenance <- val("rights_holder")
  if(!nzchar(provenance)) provenance <- cv(r$source_or_archive)
  who <- val("authorizing_person")
  when <- val("permission_date")
  credit <- val("required_credit")
  conditions <- val("conditions_text")
  if(!nzchar(conditions)) conditions <- val("scope")
  # Explicit, documented exceptions where old technical links did not propagate
  # the permission, not inferred grants from the filename alone.
  rubio_ids <- c("256","257","258","260")
  if(id %in% rubio_ids && status=="PERMISSION_GRANTED"){
    who <- "Tomás Sastre Rubio"
    when <- "6 d’octubre de 2026"
    provenance <- "Archivo de Enrique Rubio Ortiz / Archivo de la Timoteca Nacional®"
    conditions <- "Consulta en línia, sense descàrrega ni reutilització no autoritzada."
  }
  pd_ids <- c("239","240","247","248","295","296","297","298")
  if(id %in% pd_ids && status=="PUBLIC_DOMAIN"){
    provenance <- "Institut Cartogràfic i Geològic de Catalunya (ICGC), Biblioteca Domènech"
    conditions <- "Esmentar l’ICGC als agraïments i acreditar la Cartoteca sota les imatges."
  }
  if(status=="PUBLIC_DOMAIN"){
    reproduction <- if(es) "Dominio público" else "Domini públic"
    if(!nzchar(conditions)) conditions <- if(es) "Citar la procedencia" else "Citar la procedència"
  } else if(status=="PERMISSION_GRANTED"){
    if(nzchar(who)) {
      reproduction <- paste0(if(es) "Autorizada por " else "Autoritzada per ",who,
                            if(nzchar(when)) paste0(" (",when,")") else "")
    } else {
      reproduction <- if(es) "Reproducción autorizada (véase registro maestro)" else
                           "Reproducció autoritzada (vegeu registre mestre)"
    }
    if(!nzchar(conditions)) conditions <- if(es) "Ver condiciones del registro maestro" else "Vegeu condicions del registre mestre"
  } else {
    reproduction <- if(es) "Sin decisión documental validada en el registro maestro" else
                         "Sense decisió documental validada al registre mestre"
    if(!nzchar(conditions)) conditions <- if(es) "Consultar expediente documental" else "Consultar expedient documental"
  }
  fields <- if(es) c("Fuente","Autor","Procedencia del documento","Reproducción web","Condiciones") else
                     c("Font","Autor","Procedència del document","Reproducció web","Condicions")
  # STEP_EDITORIAL_72_VALIDATED_V3_START
  # Textos aprovats per asset; autorització jurídica continua al MASTER.
  edf <- "AUTORITZACIONS/MASTER/TEXTOS_EDITORIALS_VALIDATS.csv"
  if(file.exists(edf)) {
    ed <- read.csv(edf, stringsAsFactors=FALSE, check.names=FALSE, fileEncoding="UTF-8")
    er <- ed[as.character(ed$asset_id)==id & as.character(ed$item_id)==cv(r$item_id),,drop=FALSE]
    if(nrow(er)>1L) stop(paste0("DUPLICATE_EDITORIAL_",id))
    if(nrow(er)==1L) {
      geted <- function(k) cv(er[[k]])
      if(nzchar(geted("font"))) bib <- geted("font")
      if(nzchar(geted("autor"))) author <- geted("autor")
      if(nzchar(geted("procedencia"))) provenance <- geted("procedencia")
      if(status %in% c("PERMISSION_GRANTED","PUBLIC_DOMAIN","OPEN_LICENSE") &&
         !(id %in% c("274","275","276","281","283"))) {
        if(nzchar(geted("reproduccio"))) reproduction <- geted("reproduccio")
        if(nzchar(geted("condicions"))) conditions <- geted("condicions")
      }
    }
  }
  # STEP_EDITORIAL_72_VALIDATED_V3_END
  # STEP_EDITORIAL_ES_V1_START
  # Sobreescriptura només visual, exclusivament quan lang == "es".
  if(es) {
    es_csv <- "AUTORITZACIONS/MASTER/TEXTOS_EDITORIALS_ES.csv"
    if(!file.exists(es_csv)) stop("TEXTOS_EDITORIALS_ES.csv missing")
    em <- read.csv(es_csv, stringsAsFactors=FALSE, check.names=FALSE, fileEncoding="UTF-8")
    er_es <- em[as.character(em$asset_id)==id & as.character(em$item_id)==cv(r$item_id),,drop=FALSE]
    if(nrow(er_es)>1L) stop(paste0("DUPLICATE_EDITORIAL_ES_",id))
    if(nrow(er_es)==1L) {
      esval <- function(k) cv(er_es[[k]])
      if(nzchar(esval("font"))) bib <- esval("font")
      if(nzchar(esval("autor"))) author <- esval("autor")
      if(nzchar(esval("procedencia"))) provenance <- esval("procedencia")
      if(status %in% c("PERMISSION_GRANTED","PUBLIC_DOMAIN","OPEN_LICENSE") &&
         !(id %in% c("266","274","275","276","281","283"))) {
        if(nzchar(esval("reproduccio"))) reproduction <- esval("reproduccio")
        if(nzchar(esval("condicions"))) conditions <- esval("condicions")
      }
    }
  }
  # STEP_EDITORIAL_ES_V1_END
  values <- c(bib,author,provenance,reproduction,conditions)
  values[!nzchar(values)] <- if(es) "No consta" else "No consta"
  tags$div(class="editorial-credits",style="margin-top:.65rem;font-size:.90em;line-height:1.55;",
    lapply(seq_along(fields),function(k) tags$div(tags$b(paste0(fields[k],": ")),values[k])),
    if(nzchar(credit)) tags$div(tags$b(if(es) "Crédito exigido: " else "Crèdit exigit: "),credit))
}

render_asset_carousel <- function(r,admin=FALSE,lang="ca"){
  assets <- read_item_assets(r$item_id)
  panels <- list()

  # Legacy item-level source/derivative.
  if(admin){
    src <- trim_or_blank(r$portable_source)
    if(nzchar(src) && file.exists(src)){
      panels[[length(panels)+1]] <- tags$div(
        class="asset-slide",
        private_preview_tag(src),
        tags$div(class="asset-caption",
          tags$b(ifelse(nzchar(trim_or_blank(r$title)),r$title,"Document principal")),
          tags$div("Font: original privat de la fitxa.")
        )
      )
    } else {
      legacy_asset <- safe_asset_path(r)
      if(!is.na(legacy_asset)){
        ext <- tolower(tools::file_ext(legacy_asset))
        body <- if(ext %in% c("png","jpg","jpeg","gif","webp")){
          tags$img(src=legacy_asset,loading="lazy",decoding="async",style="display:block;width:auto;max-width:96%;height:auto;max-height:88vh;min-height:680px;object-fit:contain;margin:0 auto;")
        } else if(ext=="pdf"){
          browser_pdf_preview_tag(legacy_asset,lang=lang)
        } else tags$div(class="alert alert-light","Actiu disponible sense previsualització inline.")
        panels[[length(panels)+1]] <- tags$div(
          class="asset-slide",body,
          tags$div(class="asset-caption",ifelse(nzchar(trim_or_blank(r$title)),r$title,"Document principal"))
        )
      }
    }
  } else {
    legacy_asset <- safe_asset_path(r)
    if(!is.na(legacy_asset)){
      ext <- tolower(tools::file_ext(legacy_asset))
      body <- if(ext %in% c("png","jpg","jpeg","gif","webp")){
        tags$img(src=legacy_asset,loading="lazy",decoding="async",style="display:block;width:auto;max-width:96%;height:auto;max-height:88vh;min-height:680px;object-fit:contain;margin:0 auto;")
      } else if(ext=="pdf"){
        browser_pdf_preview_tag(legacy_asset,lang=lang)
      } else tags$div(class="alert alert-light",tr("no_inline",lang))
      panels[[length(panels)+1]] <- tags$div(
        class="asset-slide",body,
        tags$div(class="asset-caption",ifelse(nzchar(trim_or_blank(r$title)),r$title,"Document principal"))
      )
    }
  }

  if(nrow(assets)){
    for(i in seq_len(nrow(assets))){
      a <- assets[i,,drop=FALSE]
      # TIMOTECA_2009_SINGLE_DOCUMENT: un únic document amb dues pàgines.
      is_timoteca_2009 <- identical(as.character(r$item_id[1]), "EV2009_11_22_SASTRE_FALSO_EMPERADOR")
      if(is_timoteca_2009 && identical(as.character(a$asset_id[1]), "299")) next
      ttl <- trim_or_blank(a$title)
      cap <- trim_or_blank(a$caption)

      # REFERENCE_ONLY_ASSET_SUPPORT_V1
      if(identical(tolower(trim_or_blank(a$asset_type)), "reference")){
        ref_msg <- if(identical(lang,"es"))
          "Referencia documental consultada en línea mediante el Portal de Archivos Españoles (PARES). No se incorpora reproducción digital del documento original."
        else
          "Referència documental consultada en línia mitjançant el Portal de Archivos Españoles (PARES). No s’incorpora reproducció digital del document original."
        body <- tags$div(class="alert alert-light border", style="padding:1.2rem 1.35rem;margin:1rem auto;max-width:900px;", ref_msg)
        panels[[length(panels)+1]] <- tags$div(class="asset-slide", body, tags$div(class="asset-caption", if(nzchar(ttl)) tags$b(ttl), if(nzchar(cap)) tags$div(cap)))
        next
      }


      if(admin){
        priv <- trim_or_blank(a$private_source)
        if(nzchar(priv) && file.exists(priv)){
          body <- private_preview_tag(priv)
        } else if(public_rights_allowed(a$rights_status,a$public_display)){
          path <- asset_browser_path(a$public_asset)
          if(is.na(path)) next
          ext <- tolower(tools::file_ext(path))
          body <- if(ext %in% c("png","jpg","jpeg","gif","webp")){
            tags$img(src=path,loading="lazy",decoding="async",style="display:block;width:auto;max-width:96%;height:auto;max-height:88vh;min-height:680px;object-fit:contain;margin:0 auto;")
          } else if(ext=="pdf"){
            browser_pdf_preview_tag(path,lang=lang)
          } else tags$div(class="alert alert-light","Actiu disponible sense previsualització inline.")
        } else {
          next
        }
      } else {
        if(!public_rights_allowed(a$rights_status,a$public_display)) next
        path <- asset_browser_path(a$public_asset)
        if(is.na(path)) next
        ext <- tolower(tools::file_ext(path))
        body <- if(ext %in% c("png","jpg","jpeg","gif","webp")){
          tags$img(src=path,loading="lazy",decoding="async",style="display:block;width:auto;max-width:96%;height:auto;max-height:88vh;min-height:680px;object-fit:contain;margin:0 auto;")
        } else if(ext=="pdf"){
          browser_pdf_preview_tag(path,lang=lang)
        } else tags$div(class="alert alert-light",tr("no_inline",lang))
      }

      # Agrupa dues pàgines autoritzades en una sola diapositiva pública.
      if(is_timoteca_2009 && identical(as.character(a$asset_id[1]),"254") && !admin){
        companion <- assets[as.character(assets$asset_id)=="299",,drop=FALSE]
        if(nrow(companion)!=1L ||
           !public_rights_allowed(companion$rights_status,companion$public_display)){
          stop("TIMOTECA_2009_PAGE2_NOT_AVAILABLE")
        }
        p2 <- asset_browser_path(companion$public_asset)
        if(is.na(p2)) stop("TIMOTECA_2009_PAGE2_FILE_MISSING")
        body <- tags$div(
          tags$img(src=path,loading="lazy",decoding="async",
                   style="display:block;max-width:96%;width:auto;max-height:88vh;margin:0 auto 1.5rem;object-fit:contain;"),
          tags$img(src=p2,loading="lazy",decoding="async",
                   style="display:block;max-width:96%;width:auto;max-height:88vh;margin:0 auto;object-fit:contain;")
        )
        ttl <- "SASTRE_RUBIO_TIMOTECA_FALSO_EMPERADOR_2009_CAN.pdf — 2 pàgines"
      }

      rights_note <- if(admin) paste0(
        "Drets: ",trim_or_blank(a$rights_status),
        " · Visualització pública: ",trim_or_blank(a$public_display),
        if("source_item_id" %in% names(a) && nzchar(trim_or_blank(a$source_item_id)))
          paste0(" · Font vinculada: ",trim_or_blank(a$source_item_id)) else ""
      ) else ""


      panels[[length(panels)+1]] <- tags$div(
        class="asset-slide",
        body,
        tags$div(
          class="asset-caption",
          if(nzchar(ttl)) tags$b(ttl),
          if(!admin) asset_editorial_credits(a,r,lang),
          if(admin && nzchar(rights_note)) tags$div(class="text-muted",rights_note)
        )
      )
    }
  }

  if(!length(panels)){
    msg <- if(admin)
      "No hi ha cap document o imatge associada disponible."
    else
      tr("no_reproduction",lang)
    return(tags$div(class="alert alert-light border",msg))
  }

  cid <- paste0("carousel_",gsub("[^A-Za-z0-9]","_",r$item_id),if(admin) "_admin" else "_public")

  tags$div(
    class="asset-carousel-wrap",
    tags$div(
      id=cid,
      class="carousel slide",
      `data-bs-ride`="false",
      tags$div(
        class="carousel-inner",
        lapply(seq_along(panels),function(i)
          tags$div(class=paste("carousel-item",if(i==1) "active" else ""),panels[[i]])
        )
      ),
      if(length(panels)>1) tagList(
        tags$div(
          class="asset-carousel-controls",
          tags$button(
            type="button",class="btn btn-outline-secondary btn-sm asset-prev",
            onclick=sprintf(
              "bootstrap.Carousel.getOrCreateInstance(document.getElementById('%s'), {interval:false, wrap:true}).prev();",cid
            ),paste0("\u2039 ",tr("previous",lang))
          ),
          tags$div(
            class="asset-carousel-indicators-custom",
            lapply(seq_along(panels),function(i)
              tags$button(
                type="button",class=paste("asset-dot",if(i==1) "active" else ""),
                `data-slide-index`=i,
                onclick=sprintf(
                  "bootstrap.Carousel.getOrCreateInstance(document.getElementById('%s'), {interval:false, wrap:true}).to(%d);",cid,i-1
                ),as.character(i)
              )
            )
          ),
          tags$button(
            type="button",class="btn btn-outline-secondary btn-sm asset-next",
            onclick=sprintf(
              "bootstrap.Carousel.getOrCreateInstance(document.getElementById('%s'), {interval:false, wrap:true}).next();",cid
            ),paste0(tr("next_label",lang)," \u203a")
          )
        ),
        tags$div(class="asset-carousel-counter",paste0(length(panels)," ",tr("associated",lang))),
        tags$script(HTML(sprintf("
          (function(){
            var el=document.getElementById('%s');
            if(!el) return;
            el.addEventListener('slid.bs.carousel', function(ev){
              var dots=el.parentElement.querySelectorAll('.asset-dot');
              dots.forEach(function(d){d.classList.remove('active');});
              if(dots[ev.to]) dots[ev.to].classList.add('active');
            });
          })();
        ",cid)))
      )
    )
  )
}

# STEP_DOCUMENT_MODAL_BUTTON_ONLY_V3
# Només considera originals realment disponibles al visor públic.
public_document_available <- function(r){
  legacy <- safe_asset_path(r)
  if(!is.na(legacy) && nzchar(legacy)) return(TRUE)
  assets <- read_item_assets(r$item_id)
  if(!nrow(assets)) return(FALSE)
  for(i in seq_len(nrow(assets))){
    a <- assets[i,,drop=FALSE]
    if(identical(tolower(trim_or_blank(a$asset_type)),"reference")) next
    if(!public_rights_allowed(a$rights_status,a$public_display)) next
    rel <- asset_browser_path(a$public_asset)
    if(!is.na(rel) && nzchar(rel)) return(TRUE)
  }
  FALSE
}

render_record_card <- function(r,admin=FALSE,lang="ca"){
  if(admin) lang <- "ca"
  ci <- cat_info(r$category,lang)
  info_value <- function(x) ifelse(nzchar(trim_or_blank(x)),x,"—")

  ttl <- localized_value(r,"title",lang)
  typ <- localized_value(r,"item_type",lang)
  descr <- localized_value(r,"description",lang)
  hist_ctx <- localized_value(r,"historical_context",lang)
  contribution <- localized_value(r,"legend_contribution",lang)
  doc_value <- localized_value(r,"documentary_value",lang)
  limits <- localized_value(r,"limitations",lang)
  interpretation <- localized_value(r,"project_interpretation",lang)
  claims <- localized_value(r,"related_claims",lang)
  events <- localized_value(r,"related_events",lang)

  date_label <- if(nzchar(trim_or_blank(r$document_date))) r$document_date else if(identical(r$display_year_label,"Sense data")) tr("no_date",lang) else r$display_year_label

  render_bibliographic_references <- function(value){
    bib <- trimws(as.character(value)[1])
    bib <- gsub("\\r\\n?", "\n", bib, perl=TRUE)
    # Separar si comenca una nova citacio AUTOR EN MAJUSCULES, Nom (any).
    bib <- gsub(
      "([.;])\\s+(?=[A-ZÀ-Ý][A-ZÀ-Ý'’ -]{2,},\\s*[^\\n()]{1,80}\\([12][0-9]{3}[a-z]?\\)\\.)",
      "\\1\n", bib, perl=TRUE
    )
    refs <- trimws(unlist(strsplit(bib, "\n+", perl=TRUE)))
    refs <- refs[nzchar(refs)]
    tags$div(class="bibliographic-reference",
      lapply(refs,function(ref) tags$div(class="bibliographic-reference-entry",ref))
    )
  }

  header_block <- tags$div(
    class="record-header-fixed",
    tags$div(class="record-title",ttl),
    if(nzchar(trim_or_blank(r$bibliographic_reference))) render_bibliographic_references(r$bibliographic_reference),
    tags$div(class="record-subtitle",info_value(r$author_or_creator)," · ",date_label," · ",ci$label),
    tags$div(class="record-metadata-strip",
      tags$div(tags$b(tr("provenance",lang)),tags$span(info_value(r$source_or_archive))),
      if(nzchar(trim_or_blank(r$publication_title)))
        tags$div(tags$b(tr("publication",lang)),tags$span(info_value(r$publication_title))),
      tags$div(tags$b(tr("type",lang)),tags$span(info_value(typ)))
    )
  )

  public_tabs <- navset_card_tab(
    id=paste0("record_tabs_",r$item_id),
    nav_panel(tr("record",lang),
      tags$div(class="tab-section",tags$h4(tr("description",lang)),tags$p(if(nzchar(descr)) descr else tr("pending",lang))),
      tags$div(class="tab-section summary-box",tags$h4(tr("summary",lang)),tags$p(if(nzchar(contribution)) contribution else tr("pending",lang)))
    ),
    nav_panel(tr("context_legend",lang),
      tags$div(class="tab-section",tags$h4(tr("historical_context",lang)),tags$p(if(nzchar(hist_ctx)) hist_ctx else tr("pending",lang))),
      tags$div(class="tab-section",tags$h4(tr("legend_contribution",lang)),tags$p(if(nzchar(contribution)) contribution else tr("pending",lang)))
    ),
    nav_panel(tr("documentary_assessment",lang),
      tags$div(class="tab-section",tags$h4(tr("documentary_value",lang)),tags$p(if(nzchar(doc_value)) doc_value else tr("pending",lang))),
      tags$div(class="tab-section",tags$h4(tr("limitations",lang)),tags$p(if(nzchar(limits)) limits else tr("pending",lang))),
      tags$div(class="tab-section interpretation-box",tags$h4(tr("project_interpretation",lang)),tags$p(if(nzchar(interpretation)) interpretation else tr("pending",lang)))
    ),
    nav_panel(tr("relations_document",lang), value="adl_relations_doc",
      tags$div(class="record-relations-grid",
        tags$div(tags$b(tr("people",lang)),tags$p(info_value(r$related_people))),
        tags$div(tags$b(tr("places",lang)),tags$p(info_value(r$related_places))),
        tags$div(tags$b(tr("claims",lang)),tags$p(info_value(claims))),
        tags$div(tags$b(tr("events",lang)),tags$p(info_value(events)))
      ),
      tags$div(class="tab-section document-box",
        tags$h4(tr("documents_images",lang)),
        # ADL_EXTERNAL_BIB_LINKS_V3_ADDITIVE
        if(admin) {
          render_asset_carousel(r,admin=TRUE,lang=lang)
        } else {
          has_doc <- public_document_available(r)
          external_links <- adl_external_bib_links(r$item_id, lang)

          tagList(
            if(has_doc) {
              tagList(
                tags$p(
                  class="text-muted",
                  if(identical(lang,"es"))
                    "Abre el visor para consultar el documento conservado en el archivo."
                  else
                    "Obre el visor per consultar el document conservat a l'arxiu."
                ),
                actionButton(
                  "adl_open_document_modal",
                  if(identical(lang,"es"))
                    "Abrir visor documental"
                  else
                    "Obrir visor documental",
                  class="btn-primary"
                )
              )
            },

            # External bibliography links are ADDITIVE,
            # never a replacement for canonical assets.
            if(!is.null(external_links)) {
              external_links
            },

            if(!has_doc && is.null(external_links)) {
              tags$p(
                class="text-muted",
                if(identical(lang,"es"))
                  "Sin documento ni enlace externo asociado."
                else
                  "Sense document ni enllaç extern associat."
              )
            }
          )
        }
      )
    )
  )

  admin_block <- if(admin) tags$div(
    class="admin-info-block",
    tags$h4("Informació interna"),
    tags$div(class="record-relations-grid",
      tags$div(tags$b("ID"),tags$p(r$item_id)),
      tags$div(tags$b("Estat"),tags$p(r$record_status)),
      tags$div(tags$b("Drets"),tags$p(r$rights_status)),
      tags$div(tags$b("Visualització"),tags$p(r$public_display)),
      tags$div(tags$b("Confiança"),tags$p(r$confidence)),
      tags$div(tags$b("Acció recomanada"),tags$p(r$recommended_action))
    ),
    tags$p(tags$b("Ruta Dropbox: "),r$dropbox_path)
  ) else NULL

  card(class="public-record-card",style=paste0("background-color:",ci$bg,";color:",ci$fg,";border:1px solid rgba(0,0,0,.10);"),header_block,tags$div(class="record-tabs-wrap",public_tabs),admin_block)
}

# ---------------- UI ----------------
ui <- page_navbar(
  id="main_nav",
  
  title=tags$div(
    class="wiki-brand-wrap",
    tags$strong(
      id="wiki_brand",
      "Arxiu digital d’una llegenda"
    ),
    tags$a(
      href="https://www.iei.cat/",
      target="_blank",
      rel="noopener noreferrer",
      title="Institut d’Estudis Ilerdencs",
      `aria-label`="Institut d’Estudis Ilerdencs",
      class="wiki-iei-link",
      tags$img(
        src="assets/logo_iei.png",
        alt="Institut d’Estudis Ilerdencs",
        class="wiki-iei-logo"
      )
    ),
    tags$span(class="wiki-lang-switcher",
      tags$button(id="wiki_lang_ca",type="button",class="btn btn-sm btn-link wiki-lang-btn active",onclick="Shiny.setInputValue('wiki_lang','ca',{priority:'event'});","CA"),
      tags$span(" | "),
      tags$button(id="wiki_lang_es",type="button",class="btn btn-sm btn-link wiki-lang-btn",onclick="Shiny.setInputValue('wiki_lang','es',{priority:'event'});","ES")
    )
  ),
  theme=bs_theme(version=5,bootswatch="flatly"),

  # ADL_ARCHIVAL_THEME_V1
  tags$head(tags$link(rel="stylesheet",href="assets/archival_theme_v1.css")),
  tags$head(tags$style(HTML("
    .container-fluid { max-width:100% !important; }
    .wiki-brand-wrap { display:flex;align-items:center;gap:.7rem;white-space:nowrap; }
    .wiki-lang-switcher { display:inline-flex;align-items:center;gap:.1rem;font-size:.82rem; }
    .wiki-lang-btn { padding:0 .18rem !important;text-decoration:none !important;font-size:.82rem !important;opacity:.65; }
    .wiki-lang-btn.active { opacity:1;font-weight:700;text-decoration:underline !important; }


.wiki-iei-link {
  display: inline-flex;
  align-items: center;
  flex-shrink: 0;
  margin-left: 0.5rem;
  text-decoration: none;
}

.wiki-iei-logo {
  display: block;
  width: auto;
  height: 43px;
  max-width: 110px;
  object-fit: contain;
}

@media (max-width: 991.98px) {
  .wiki-iei-logo {
    height: 32px;
    max-width: 85px;
  }
}


    /* Capçalera: marca a l'esquerra, navegació a la dreta, una sola fila */
    .navbar > .container-fluid {
      display:flex !important;
      flex-wrap:nowrap !important;
      align-items:center !important;
      width:100% !important;
    }
    .navbar-brand {
      flex:0 0 auto;
      margin-right:auto !important;
      white-space:nowrap;
      font-size:1.05rem;
      font-weight:600;
    }
    .navbar-collapse {
      flex:0 0 auto !important;
      flex-grow:0 !important;
    }
    .navbar-nav {
      margin-left:auto !important;
      flex-wrap:nowrap !important;
      align-items:center;
    }
    .navbar-nav .nav-link {
      white-space:nowrap;
    }
    @media (max-width: 767.98px) {
      .navbar > .container-fluid { flex-wrap:wrap !important; }
      .navbar-collapse { flex-basis:100% !important; width:100%; }
      .navbar-nav { align-items:flex-start; margin-left:0 !important; }
      .home-hero-title {
        top:1rem;
        left:1.1rem;
        right:1.1rem;
      }
      .home-hero-title h1 {
        font-size:clamp(1.25rem,6vw,2rem);
      }
    }
    .bslib-sidebar-layout > .main { min-width:0; }
    .selectize-control,.selectize-input,.form-select { width:100% !important; }
    .public-record-card { width:100%;max-width:none !important; }
    .record-main-header { padding:1.25rem 1.5rem !important; }
    .record-title { font-size:1.5rem;font-weight:700;line-height:1.25;margin-bottom:.35rem; }
    .record-subtitle { font-size:.95rem;opacity:.82; }
    .bibliographic-reference-entry + .bibliographic-reference-entry { margin-top:.65rem; }
    .bibliographic-reference {
      margin-top:.65rem;padding-top:.65rem;border-top:1px solid rgba(0,0,0,.10);
      font-size:.96rem;font-style:italic;line-height:1.45;
    }
    .record-metadata-strip {
      display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:1rem;
      padding:1rem 1.5rem;background:rgba(255,255,255,.42);border-bottom:1px solid rgba(0,0,0,.08);
    }
    .record-metadata-strip > div { display:flex;flex-direction:column;gap:.2rem; }
    .record-section {
      margin:1.1rem 1.5rem;border:1px solid rgba(0,0,0,.10);
      border-radius:.65rem;overflow:hidden;background:rgba(255,255,255,.60);
    }
    .record-section-title {
      font-weight:700;font-size:1rem;padding:.72rem 1rem;
      background:rgba(255,255,255,.78);border-bottom:1px solid rgba(0,0,0,.08);
    }
    .record-section-body { padding:1rem 1.1rem;line-height:1.65; }
    .record-section-body p:last-child { margin-bottom:0; }
    .record-relations-grid { display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:.8rem 1.1rem; }
    .record-relations-grid > div {
      padding:.75rem;border-radius:.45rem;background:rgba(255,255,255,.55);border:1px solid rgba(0,0,0,.06);
    }
    /* STEP_DOCUMENT_MODAL_V1_20261021 */
    .modal-dialog:has(.adl-doc-modal-body) {
      max-width: min(96vw, 1500px) !important;
      margin: 1.5vh auto;
    }
    .adl-doc-modal-body { max-height: 78vh; overflow-y: auto; }
    /* STEP_DOCUMENT_MODAL_CAPTION_TOP_V4 */
    /* Visual-only: move existing description and permission note above PDF/image. */
    .adl-doc-modal-body .asset-slide > .asset-caption {
      order: -1;
      width: 100%;
      padding: .85rem 1rem;
      margin-bottom: .7rem;
      border-bottom: 1px solid rgba(0,0,0,.12);
      background: #f7f9fa;
      color: #333;
    }

    .adl-doc-modal-body .asset-slide { min-height: 50vh; }
    .intro-hero {
      padding:1.7rem 1.9rem;border-radius:.8rem;background:rgba(255,255,255,.72);
      border:1px solid rgba(0,0,0,.08);max-width:1200px;margin:0 auto;
    }
    .intro-hero h2 { margin-top:0;margin-bottom:1rem; }
    .intro-hero p { font-size:1.04rem;line-height:1.68; }
    .editor-grid {
      display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:1rem 1.2rem;
    }
    .editor-wide { grid-column:1/-1; }
    .editor-actions {
      position:sticky;bottom:0;background:rgba(255,255,255,.96);padding:.8rem 0;
      border-top:1px solid rgba(0,0,0,.12);z-index:2;
    }
    .history-note { font-size:.92rem;color:#555; }

    .home-hero {
      position:relative;
      width:100%;
      margin:0 0 1.5rem 0;
      overflow:hidden;
      border-radius:.7rem;
      border:1px solid rgba(0,0,0,.10);
      background:#fff;
    }
    .home-hero-title {
      position:absolute;
      top:1.55rem;
      left:2rem;
      right:2rem;
      z-index:5;
      color:#fff;
      pointer-events:none;
      text-shadow:0 1px 3px rgba(0,0,0,.78),0 2px 10px rgba(0,0,0,.48);
    }
    .home-hero-title h1 {
      margin:0;
      font-size:clamp(1.65rem,3vw,2.75rem);
      line-height:1.15;
      font-weight:400;
    }
    .home-hero-title h1 strong { font-weight:700; }
    .home-hero-title h1 span { font-weight:400; }
    .home-hero img {
      display:block;
      width:100%;
      height:clamp(230px,27vw,400px);
      min-height:230px;
      max-height:400px;
      object-fit:cover;
      object-position:center;
    }
    .home-hero-caption {
      padding:.55rem .85rem .65rem .85rem;
      font-size:.88rem;
      color:#555;
      background:rgba(255,255,255,.94);
      border-top:1px solid rgba(0,0,0,.07);
    }


    .record-header-fixed {
      padding:1.25rem 1.5rem 1rem 1.5rem;
      background:rgba(255,255,255,.28);
      border-bottom:1px solid rgba(0,0,0,.08);
    }
    .record-tabs-wrap {
      padding:1rem 1.5rem 1.5rem 1.5rem;
    }
    .record-tabs-wrap .card {
      margin-bottom:0;
      background:rgba(255,255,255,.72);
    }
    .tab-section {
      margin:.9rem 0 1.1rem 0;
      padding:1rem 1.1rem;
      border:1px solid rgba(0,0,0,.08);
      border-radius:.6rem;
      background:rgba(255,255,255,.62);
      line-height:1.65;
    }
    .tab-section h4 {
      margin-top:0;
      margin-bottom:.6rem;
      font-size:1.05rem;
      font-weight:700;
    }
    .summary-box {
      border-left:4px solid rgba(0,0,0,.20);
    }
    .interpretation-box {
      background:rgba(255,255,255,.78);
    }
    .document-box {
      margin-top:1rem;
    }

    .asset-carousel-wrap {
      position:relative;
      background:rgba(255,255,255,.75);
      border:1px solid rgba(0,0,0,.08);
      border-radius:.6rem;
      overflow:hidden;
    }
    .asset-slide {
      min-height:260px;
      display:flex;
      flex-direction:column;
      align-items:center;
      justify-content:center;
      padding:.7rem;
      background:#fff;
    }
    .asset-caption {
      width:100%;
      padding:.75rem .9rem .35rem .9rem;
      font-size:.92rem;
      line-height:1.4;
      color:#444;
      text-align:left;
    }
    .asset-carousel-wrap .carousel-control-prev,
    .asset-carousel-wrap .carousel-control-next {
      width:8%;
      background:rgba(0,0,0,.12);
    }
    .asset-carousel-counter {
      text-align:center;
      padding:.35rem .6rem .6rem .6rem;
      font-size:.85rem;
      color:#666;
      background:#fff;
    }

    .asset-carousel-controls {
      display:flex;
      align-items:center;
      justify-content:space-between;
      gap:.8rem;
      padding:.7rem .8rem;
      background:#fff;
      border-top:1px solid rgba(0,0,0,.08);
    }
    .asset-carousel-indicators-custom {
      display:flex;
      align-items:center;
      justify-content:center;
      gap:.4rem;
      flex:1;
    }
    .asset-dot {
      min-width:2rem;
      height:2rem;
      padding:0 .45rem;
      border:1px solid rgba(0,0,0,.25);
      border-radius:999px;
      background:#fff;
      color:#333;
      font-size:.82rem;
      line-height:1;
    }
    .asset-dot.active {
      background:#333;
      color:#fff;
      border-color:#333;
      font-weight:700;
    }
    .asset-prev,.asset-next {
      white-space:nowrap;
      min-width:92px;
    }
    .asset-slide img {
      display:block;
      max-width:100%;
      margin:0 auto;
    }


    .admin-info-block {
      margin:0 1.5rem 1.5rem 1.5rem;
      padding:1rem 1.1rem;
      border:1px dashed rgba(0,0,0,.18);
      border-radius:.6rem;
      background:rgba(255,255,255,.55);
    }

    @media (min-width:1200px) {
      .bslib-sidebar-layout > .sidebar { width:250px !important;min-width:250px !important; }
    }
    @media (max-width:900px) {
      .record-metadata-strip,.record-relations-grid,.editor-grid { grid-template-columns:1fr; }
      .record-section { margin-left:.75rem;margin-right:.75rem; }
      .editor-wide { grid-column:auto; }
    }

    .import-log-wide {
      width:100%;
      max-width:none;
      margin-top:1rem;
    }
    .import-log-wide pre {
      width:100% !important;
      max-width:none !important;
      min-height:320px;
      max-height:650px;
      overflow:auto;
      white-space:pre-wrap;
      word-break:break-word;
      font-size:.9rem;
      line-height:1.5;
      padding:1rem;
      background:#f7f7f7;
      border:1px solid rgba(0,0,0,.14);
      border-radius:.45rem;
    }
    .asset-upload-log {
      width:100%;
      margin-top:.8rem;
    }
    .asset-upload-log pre {
      width:100% !important;
      max-width:none !important;
      min-height:90px;
      max-height:240px;
      overflow:auto;
      white-space:pre-wrap;
      word-break:break-word;
      font-size:.86rem;
      line-height:1.45;
      padding:.75rem;
      background:#f8f9fa;
      border:1px solid rgba(0,0,0,.10);
      border-radius:.35rem;
    }


    .asset-admin-grid {
      display:grid;
      grid-template-columns:minmax(0,1fr) minmax(0,1fr);
      gap:1.2rem;
      align-items:start;
      margin-top:.8rem;
    }

    /* v0.8.13.2 — controls administratius a amplada completa */
    .admin-full-input,
    .admin-full-input .shiny-input-container,
    .admin-full-input .form-group,
    .admin-full-input .selectize-control,
    .admin-full-input .selectize-input,
    .admin-full-input .form-control,
    .admin-full-input .form-select,
    .admin-full-input .input-group,
    .admin-full-input .btn-file,
    .admin-full-input input[type='file'] {
      width:100% !important;
      max-width:none !important;
    }
    .admin-fitxa-full,
    .admin-fitxa-full > div,
    .admin-fitxa-full .card,
    .admin-fitxa-full .bslib-card,
    .admin-fitxa-full .public-record-card,
    .admin-fitxa-full .card-body {
      width:100% !important;
      max-width:none !important;
    }
    .asset-admin-panel {
      border:1px solid rgba(0,0,0,.12);
      border-radius:.55rem;
      padding:1rem;
      background:#fff;
    }
    .asset-actions-row {
      display:flex;
      flex-wrap:wrap;
      gap:.5rem;
      margin-top:.75rem;
    }
    .asset-edit-log {
      margin-top:.8rem;
    }
    .asset-edit-log pre {
      width:100% !important;
      max-width:none !important;
      min-height:70px;
      max-height:180px;
      overflow:auto;
      white-space:pre-wrap;
      word-break:break-word;
      font-size:.84rem;
      padding:.7rem;
      background:#f8f9fa;
      border:1px solid rgba(0,0,0,.10);
      border-radius:.35rem;
    }
    @media (max-width: 900px) {
      .asset-admin-grid {
        grid-template-columns:1fr;
      }
    }
/* ADL_ANALISI_LLEGENDA_V1 */

/* ADL_HOME_MOBILE_FIX_V1 */
/* Només Inici, només pantalles estretes */
@media (max-width: 740px) {
  .home-hero {
    display: flex;
    flex-direction: column;
    overflow: hidden;
    background: #f7f3eb;
  }

  .home-hero > img {
    order: 1;
    width: 100%;
    height: auto;
    min-height: 0;
    max-height: none;
    aspect-ratio: 16 / 9;
    object-fit: cover;
    object-position: center;
  }

  .home-hero > .home-hero-title {
    position: static;
    order: 2;
    padding: 1rem 1.1rem .65rem;
    margin: 0;
    color: #29382e;
    background: #f7f3eb;
    text-shadow: none;
    pointer-events: auto;
  }

  .home-hero > .home-hero-title h1 {
    margin: 0;
    color: #29382e;
    font-size: clamp(1.35rem, 5vw, 1.85rem);
    line-height: 1.22;
    text-shadow: none;
    overflow-wrap: break-word;
  }

  .home-hero > .home-hero-caption {
    order: 3;
    padding: .55rem 1.1rem .85rem;
    background: #f7f3eb;
    border-top: 0;
  }
}

.adl-analysis { max-width:1240px;margin:0 auto 1rem; padding:0 .35rem; color:#29312c; }
.adl-analysis-hero { position:relative; min-height:225px; margin:0 auto .65rem; overflow:hidden; border-radius:13px; border:1px solid #e0d8c9; background:#f3f0e9; }
.adl-analysis-hero>img { position:absolute; width:100%;height:100%;object-fit:cover;object-position:center 45%; }
.adl-analysis-hero:after { content:'';position:absolute;inset:0;background:linear-gradient(90deg,rgba(250,248,242,.98) 0%,rgba(250,248,242,.88) 25%,rgba(250,248,242,.20) 69%,rgba(250,248,242,0) 100%),linear-gradient(0deg,rgba(247,245,238,.36),transparent 36%); }
.adl-analysis-hero-copy { position:relative;z-index:1;padding:2.3rem 2rem 1.7rem;max-width:760px; }
.adl-analysis-hero-copy h1 { font-family:Georgia,serif;font-size:clamp(2rem,4vw,3.25rem);line-height:1.12;color:#29382e;margin:0 0 .55rem; }
.adl-analysis-hero-copy p { font-family:Georgia,serif;font-size:clamp(.95rem,1.5vw,1.18rem);color:#4a514b;max-width:550px;line-height:1.32;margin:0; }
.adl-analysis-card { background:rgba(255,253,249,.93);border:1px solid #ded5c5;border-radius:12px;box-shadow:0 3px 14px rgba(46,40,27,.065);padding:1.05rem 1.2rem;margin-bottom:.65rem; }
.adl-analysis-grid {display:grid;grid-template-columns:minmax(150px,225px) minmax(0,1fr);gap:1.2rem;align-items:start;}
.adl-analysis-thumb {margin:0;}
.adl-analysis-thumb img {display:block;width:100%;height:auto;border-radius:9px;border:1px solid #e2d9c9;}
.adl-analysis-thumb figcaption {font-size:.76rem;color:#6e6a62;margin:.3rem 0 0;line-height:1.25;}
.adl-analysis-text h2,.adl-analysis-points h2 {font-family:Georgia,serif;font-size:1.6rem;font-weight:500;color:#29382e;margin:0 0 .6rem;border-bottom:1px solid #ded5c5;padding-bottom:.4rem;}
.adl-analysis-text p,.adl-analysis-points p {font-size:.98rem;line-height:1.44;margin:0 0 .54rem;}
.adl-analysis-points ul {padding-left:1.2rem;line-height:1.42;margin:0;}
.adl-analysis-points li {margin:.25rem 0;}
@media(max-width:740px){.adl-analysis-hero{min-height:205px}.adl-analysis-hero-copy{padding:2rem 1.25rem 1.25rem}.adl-analysis-grid{grid-template-columns:1fr}.adl-analysis-thumb{max-width:190px}.adl-analysis-card{padding:.9rem}.adl-analysis-hero:after{background:linear-gradient(90deg,rgba(250,248,242,.95),rgba(250,248,242,.54) 75%,rgba(250,248,242,.15))}}

/* CONDICIONS D'ÚS — PORTADA CA/ES */

.usage-conditions {
  max-width: 1200px;
  margin: 1.5rem auto 2rem auto;
  padding: 1.5rem 1.9rem;
  border: 1px solid rgba(0,0,0,.10);
  border-radius: .8rem;
  background: rgba(255,255,255,.72);
}

.usage-conditions h3 {
  font-size: 1.15rem;
  margin-bottom: 1rem;
}

.usage-conditions p {
  font-size: .94rem;
  line-height: 1.65;
  margin-bottom: .8rem;
}

.usage-conditions p:last-child {
  margin-bottom: 0;
}
  "))),

  tags$head(
    tags$script(HTML("
      Shiny.addCustomMessageHandler('wiki_i18n_nav', function(m){
        var brand=document.getElementById('wiki_brand');
        if(brand) brand.textContent=m.brand;
        document.querySelectorAll('.navbar-nav .nav-link').forEach(function(el){
          var v=el.getAttribute('data-value');
          if(v==='Inici') el.textContent=m.home;
          if(v==='Anàlisi de la llegenda') el.textContent=(m.lang==='es'?'Análisis de la leyenda':'Anàlisi de la llegenda');
          if(v==='Guia de lectura') el.textContent=m.guide;
          if(v==='Fitxes') el.textContent=m.records;
          if(v==='Cronologia') el.textContent=m.timeline;
        });
        var ca=document.getElementById('wiki_lang_ca');
        var es=document.getElementById('wiki_lang_es');
        if(ca) ca.classList.toggle('active',m.lang==='ca');
        if(es) es.classList.toggle('active',m.lang==='es');
        document.documentElement.lang=m.lang;
      });
    "))
  ),

  # ADL_TIMELINE_VISUAL_V1: bridge JS per a cronologia interactiva
  tags$head(tags$script(HTML("
    (function(){
      function frame(){return document.getElementById('adl_timeline_frame');}
      window.addEventListener('message',function(e){
        var f=frame();if(!f || e.source!==f.contentWindow || e.origin!==window.location.origin) return;
        var m=e.data;if(!m || m.source!=='adl_timeline_visual')return;
        if(m.type==='ready'){
          if(window.Shiny && Shiny.setInputValue) Shiny.setInputValue('adl_timeline_ready',Date.now(),{priority:'event'});
          return;
        }
        if(m.type==='open-item' && typeof m.item_id==='string'){
          if(window.Shiny && Shiny.setInputValue) Shiny.setInputValue('adl_timeline_item',m.item_id,{priority:'event'});
        }
      });
      // ADL_TIMELINE_READY_FIX_V2: cada càrrega de l'iframe demana dades de nou.
      // Evita perdre l'enviament inicial quan l'iframe encara no escolta missatges.
      document.addEventListener('load',function(e){
        var f=frame();
        if(f && e.target===f && window.Shiny && Shiny.setInputValue){
          Shiny.setInputValue('adl_timeline_ready',Date.now(),{priority:'event'});
        }
      },true);
      document.addEventListener('shiny:connected',function(){
        var f=frame();if(f && f.contentWindow) Shiny.setInputValue('adl_timeline_ready',Date.now(),{priority:'event'});
      });
      // ADL_TIMELINE_RECEIVER_FIX_V3: el registre s'ha de fer DESPRES
      // de la inicialitzacio de Shiny, no exclusivament en executar <head>.
      var receiverRegistered=false;
      function registerTimelineReceiver(){
        if(receiverRegistered || !window.Shiny || !Shiny.addCustomMessageHandler) return false;
        Shiny.addCustomMessageHandler('adl_timeline_data',function(m){
          var f=frame();
          var ev=Array.isArray(m.events)?m.events:[];
          console.info('[ADL timeline] Shiny -> iframe:',ev.length,'fitxes publiques; idioma:',m.lang);
          if(f && f.contentWindow){
            f.contentWindow.postMessage({source:'adl_shiny_timeline',type:'data',events:ev,lang:m.lang},window.location.origin);
          } else {
            console.warn('[ADL timeline] iframe inexistent quan arriben les dades');
          }
        });
        receiverRegistered=true;
        console.info('[ADL timeline] receptor registrat');
        if(frame() && frame().contentWindow && Shiny.setInputValue){
          Shiny.setInputValue('adl_timeline_ready',Date.now(),{priority:'event'});
        }
        return true;
      }
      document.addEventListener('shiny:connected',registerTimelineReceiver);
      if(window.jQuery) window.jQuery(document).on('shiny:connected',registerTimelineReceiver);
      registerTimelineReceiver();
    })();
  "))),

  nav_panel("Inici",
    uiOutput("home_i18n")
  ),

  nav_panel("Anàlisi de la llegenda",
    uiOutput("analysis_i18n")
  ),

  nav_panel(
    "Guia de lectura",
    tags$div(class="adl-public-theme",uiOutput("guide_i18n"))
  ),
  nav_panel("Cronologia",
            tags$div(class="adl-public-theme",layout_sidebar(
              sidebar=sidebar(width=250,
                              selectInput("timeline_year","Any",choices=NULL,selectize=FALSE),
                              selectInput("timeline_category","Categoria",choices=c("Totes"="",setNames(categories$category,categories$category_label)),selectize=FALSE),
                              selectInput("timeline_person","Personatge",choices=c("Tots"=""),selectize=FALSE),
                              selectInput("timeline_author","Autor",choices=c("Tots"=""),selectize=FALSE),
                              selectInput("timeline_publication","Publicació / revista",choices=c("Totes"=""),selectize=FALSE),
                              selectInput("timeline_place","Lloc",choices=c("Tots"=""),selectize=FALSE),
                              textInput("timeline_search","Cerca","")
              ),
              card(
                card_header(uiOutput("timeline_header_i18n")),
                radioButtons("adl_timeline_view",NULL,choices=c("Cronologia visual"="visual","Llista documental"="list"),selected="visual",inline=TRUE),
                conditionalPanel("input.adl_timeline_view == 'visual'",
                                 tags$iframe(id="adl_timeline_frame",src="adl_timeline_visual.html",title="Cronologia visual interactiva",
                                             style="width:100%;height:auto;min-height:740px;aspect-ratio:1120 / 1050;border:0;display:block;background:transparent;")),
                conditionalPanel("input.adl_timeline_view == 'list'",
                                 uiOutput("timeline_help_i18n"),
                                 DTOutput("public_timeline"))
              )
            ))
  ),
  nav_panel("Fitxes",
    tags$div(class="adl-public-theme",layout_sidebar(
      sidebar=sidebar(width=250,
        selectInput("public_year","Any",choices=NULL,selectize=FALSE),
        selectInput("public_category","Categoria",choices=c("Totes"="",setNames(categories$category,categories$category_label)),selectize=FALSE),
        selectInput("public_person","Personatge",choices=c("Tots"=""),selectize=FALSE),
        selectInput("public_author","Autor",choices=c("Tots"=""),selectize=FALSE),
        selectInput("public_publication","Publicació / revista",choices=c("Totes"=""),selectize=FALSE),
        selectInput("public_place","Lloc",choices=c("Tots"=""),selectize=FALSE),
        textInput("public_search","Cerca","")
      ),
      card(class="public-record-card",
        card_header(uiOutput("record_header_i18n")),
        selectInput("public_item","Peça",choices=NULL,width="100%"),
        uiOutput("public_card")
      )
    ))
  ),
  nav_panel("Bibliografia",tags$div(class="adl-public-theme",adl_bib_ui())), # ADL_BIBLIO_V1_1_AFTER_FITXES

  if (ADMIN_ENABLED) nav_panel("Auditoria MASTER_TEST",mt_audit_ui()) else NULL,
  if (ADMIN_ENABLED) nav_panel("Administració",uiOutput("admin_ui")) else NULL
)

# ---------------- Server ----------------
server <- function(input,output,session){
  # ADL_BIBLIO_V1
  adl_bib_server(input,output,session,lang_rv)

  if (ADMIN_ENABLED) mt_audit_server(input,output,session)
  admin_ok <- reactiveVal(FALSE)
  lang_rv <- reactiveVal("ca")

  observeEvent(input$wiki_lang,{
    z <- if(identical(input$wiki_lang,"es")) "es" else "ca"
    lang_rv(z)
    session$sendCustomMessage(
      "wiki_i18n_nav",
      list(
        lang=z,
        brand=tr("brand",z),
        home=tr("home",z),
        guide=tr("guide",z),
        records=tr("records",z),
        timeline=tr("timeline",z)
      )
    )
  },ignoreInit=TRUE)

 
  
  
  output$home_i18n <- renderUI({
    z <- lang_rv()
    
    tags$div(class="adl-public-theme",tagList(
      tags$div(
        class="home-hero",
        tags$div(
          class="home-hero-title",
          tags$h1(
            tags$strong(tr("hero_main",z)),
            tags$span(tr("hero_tail",z))
          )
        ),
        tags$img(
          src="assets/cadi_presentacio.jpg",
          loading="lazy",
          decoding="async",
          alt=tr("hero_alt",z)
        ),
        tags$div(
          class="home-hero-caption",
          tr("hero_caption",z)
        )
      ),
      
      tags$div(
        class="intro-hero",
        tags$h3(if(identical(z,"es")) "Sobre este archivo" else "Sobre aquest arxiu"),
        tags$p(tr("intro1",z)),
        tags$p(tr("intro2",z)),
        tags$p(tr("intro3",z)),
        tags$p(tr("intro4",z))
      ),

      usage_conditions_ui(z)
    ))
  })
  

  # ADL_ANALISI_LLEGENDA_V1 — CA/ES; scientific content grounded in the manuscript.
  output$analysis_i18n <- renderUI({
    z <- lang_rv()
    es <- identical(z,"es")
    tags$div(class="adl-public-theme adl-analysis",tagList(
      tags$section(class="adl-analysis-hero",
        tags$img(src="assets/toloriu_fotografia_autor.jpg",alt=if(es) "Vista de Toloriu" else "Vista de Toloriu"),
        tags$div(class="adl-analysis-hero-copy",
          tags$h1(if(es) "Análisis de la leyenda" else "Anàlisi de la llegenda"),
          tags$p(if(es) "Historia documentada, formación de un relato y memoria cultural" else "Història documentada, formació d’un relat i memòria cultural")
        )
      ),
      tags$section(class="adl-analysis-card adl-analysis-grid",
        tags$figure(class="adl-analysis-thumb",
          tags$img(src="assets/poble_muntanya.png",alt=if(es) "Ilustración del paisaje pirenaico con una iglesia" else "Il·lustració del paisatge pirinenc amb una església")
          ),
        tags$div(class="adl-analysis-text",
          tags$h2(if(es) "Interpretación documental" else "Interpretació documental"),
          if(es) tagList(
            tags$p("La investigación sobre la princesa azteca de Toloriu permite reconstruir documentalmente la formación de un relato que hoy forma parte del patrimonio cultural pirenaico."),
            tags$p("La movilidad entre América y la península ibérica durante el siglo XVI está documentada, así como una vinculación histórica entre una hija de Moctezuma y el apellido Grado. Ninguna de estas evidencias permite, sin embargo, identificar a los protagonistas de la leyenda ni verificar su genealogía."),
            tags$p("Las descripciones de Toloriu de Osona (1899) y Torras (1924), y los repertorios pirenaicos de Joan Amades, no registran la trama mexicana. Este silencio es significativo, aunque no demuestra por sí solo la inexistencia del relato."),
            tags$p(tags$strong("La secuencia documental más consistente empieza en 1952: "),"la prensa recoge las pretensiones genealógicas de Guillermo Grau Rifé y, el 24 de mayo, Manuel Reyes vincula a Chipahuatzin con Juan de Grau, todavía sin situarlos en Toloriu."),
            tags$p("En 1955, Amada López de Meneses atribuye a Grau Rifé la versión que sitúa a María de Moctezuma en Toloriu y advierte de que no aparece prueba de la existencia del supuesto conquistador Juan de Grau."),
            tags$p(tags$strong("Las evidencias son más coherentes con una construcción contemporánea de la leyenda que con la transmisión continuada de una tradición del siglo XVI.")," Esta conclusión no reduce su valor como memoria cultural: distingue la historia documentada del proceso de formación y arraigo patrimonial del relato.")
          ) else tagList(
            tags$p("La recerca sobre la princesa asteca de Toloriu permet reconstruir documentalment la formació d’un relat que avui forma part del patrimoni cultural pirinenc."),
            tags$p("La mobilitat entre Amèrica i la península Ibèrica durant el segle XVI està documentada, com també una vinculació històrica entre una filla de Moctezuma i el cognom Grado. Cap d’aquestes evidències, però, no permet identificar els protagonistes de la llegenda ni verificar-ne la genealogia."),
            tags$p("Les descripcions de Toloriu d’Osona (1899) i Torras (1924), i els repertoris pirinencs de Joan Amades, no registren la trama mexicana. Aquest silenci és significatiu, encara que no demostra per si sol la inexistència del relat."),
            tags$p(tags$strong("La seqüència documental més consistent comença el 1952: "),"la premsa recull les pretensions genealògiques de Guillermo Grau Rifé i, el 24 de maig, Manuel Reyes vincula Chipahuatzin amb Juan de Grau, encara sense situar-los a Toloriu."),
            tags$p("El 1955, Amada López de Meneses atribueix a Grau Rifé la versió que situa Maria de Moctezuma a Toloriu i adverteix que no apareix cap prova de l’existència del suposat conquistador Juan de Grau."),
            tags$p(tags$strong("Les evidències són més coherents amb una construcció contemporània de la llegenda que no pas amb la transmissió continuada d’una tradició del segle XVI.")," Aquesta conclusió no en redueix el valor com a memòria cultural: distingeix la història documentada del procés de formació i arrelament patrimonial del relat.")
          )
        )
      ),
      tags$section(class="adl-analysis-card adl-analysis-points",
        tags$h2(if(es) "Claves de la investigación" else "Claus de la recerca"),
        if(es) tags$ul(
          tags$li(tags$b("Siglo XVI: "),"contexto histórico real, sin identificación genealógica probada."),
          tags$li(tags$b("Antes de 1952: "),"ausencia de la trama mexicana en las fuentes territoriales y folclóricas examinadas."),
          tags$li(tags$b("1952–1955: "),"documentación de la elaboración y atribución del relato."),
          tags$li(tags$b("Posteriormente: "),"difusión literaria, patrimonial y cultural de la leyenda.")
        ) else tags$ul(
          tags$li(tags$b("Segle XVI: "),"context històric real, sense identificació genealògica acreditada."),
          tags$li(tags$b("Abans de 1952: "),"absència de la trama mexicana en les fonts territorials i folklòriques examinades."),
          tags$li(tags$b("1952–1955: "),"documentació de l’elaboració i atribució del relat."),
          tags$li(tags$b("Posteriorment: "),"difusió literària, patrimonial i cultural de la llegenda.")
        )
      )
    ))
  })

  output$guide_i18n <- renderUI({
    guide_ui(lang_rv())
  })

  output$record_header_i18n <- renderUI(tr("record_card",lang_rv()))
  output$timeline_header_i18n <- renderUI(tr("chronological_order",lang_rv()))
  output$timeline_help_i18n <- renderUI(tags$p(tr("timeline_help",lang_rv())))

  items_rv <- reactiveVal(decorate_items(read_items()))

  refresh_items <- function(){
    items_rv(decorate_items(read_items()))
  }

  public_items <- reactive({
    x <- items_rv()
    x[x$record_status %in% c("reviewed","publishable","published"),,drop=FALSE]
  })

  observe({
    x <- public_items()
    yrs <- sort(unique(x$display_year_num[!is.na(x$display_year_num)]))
    labels <- as.character(yrs)
    if(any(x$display_year_label=="Sense data")) labels <- c(labels,"Sense data")
    z <- lang_rv()
    shown <- ifelse(labels=="Sense data",tr("no_date",z),labels)
    updateSelectInput(session,"public_year",label=tr("year",z),choices=c(setNames("Tots",tr("all_m",z)),setNames(labels,shown)),selected={cur <- isolate(input$public_year); if(is.null(cur) || !nzchar(cur)) "Tots" else cur})
  })


  observe({
    x <- public_items()
    people <- legend_people_values(x)
    updateSelectInput(
      session,
      "public_person",
      label=tr("person",lang_rv()),
      choices=c(
        setNames("",tr("all_m",lang_rv())),
        adl_surname_choices(people)
      ),
      selected=if(!is.null(input$public_person) &&
                  input$public_person %in% people)
        input$public_person else ""
    )
  })

  observe({
    x <- public_items()
    authors <- split_semicolon_values(x$author_or_creator)
    updateSelectInput(session,"public_author",label=tr("author",lang_rv()),choices=c(setNames("",tr("all_m",lang_rv())),adl_surname_choices(authors)),selected=if(!is.null(input$public_author) && input$public_author %in% authors) input$public_author else "")
  })

  observe({
    x <- public_items()
    publications <- split_semicolon_values(x$publication_title)
    cur <- isolate(input$public_publication)
    selected <- if(!is.null(cur) && length(cur)==1 && !is.na(cur) && cur %in% publications) cur else ""
    updateSelectInput(session,"public_publication",label=tr("publication",lang_rv()),choices=c(setNames("",tr("all_f",lang_rv())),setNames(publications,publications)),selected=selected)
  })

  observe({
    x <- public_items()
    places <- split_semicolon_values(x$related_places)
    updateSelectInput(session,"public_place",label=tr("place",lang_rv()),choices=c(setNames("",tr("all_m",lang_rv())),setNames(places,places)),selected=if(!is.null(input$public_place) && input$public_place %in% places) input$public_place else "")
  })

  observe({
    z <- lang_rv()
    labs <- vapply(seq_len(nrow(categories)),function(i) category_label_i18n(categories$category[i],categories$category_label[i],z),character(1))
    cur1 <- isolate(input$public_category); if(is.null(cur1)) cur1 <- ""
    cur2 <- isolate(input$timeline_category); if(is.null(cur2)) cur2 <- ""
    updateSelectInput(session,"public_category",label=tr("category",z),choices=c(setNames("",tr("all_f",z)),setNames(categories$category,labs)),selected=cur1)
    updateSelectInput(session,"timeline_category",label=tr("category",z),choices=c(setNames("",tr("all_f",z)),setNames(categories$category,labs)),selected=cur2)
    updateTextInput(session,"public_search",label=tr("search",z))
    updateTextInput(session,"timeline_search",label=tr("search",z))
  })

  # Ordre dels dos episodis de Soldevila, sense alterar SQLite.
  soldevila_sort_key <- function(t){
    z <- tolower(as.character(t))
    first <- grepl("^antoni soldevila publica la primera part",z)
    second <- grepl("^antoni soldevila completa la seva narració",z)
    z[first] <- "antoni soldevila 00 publica la primera part"
    z[second] <- "antoni soldevila 01 completa la seva narració"
    z
  }


  # ADL_PUBLIC_EXCLUSIVE_FILTERS_V1
  # A new selection in any Fitxes dropdown resets all other
  # Fitxes dropdowns to "Tots".
  # Do not affect Cronologia or Administracio.

  adl_public_filter_ids <- c(
    "public_year",
    "public_category",
    "public_person",
    "public_author",
    "public_publication",
    "public_place"
  )

  for (filter_id in adl_public_filter_ids) {
    local({
      current_id <- filter_id

      observeEvent(input[[current_id]], {
        current_value <- input[[current_id]]

        # Ignore resets to the default value. Otherwise, resetting
        # other filters could trigger cascading resets.
        if (is.null(current_value) ||
            length(current_value) != 1L ||
            is.na(current_value) ||
            !nzchar(current_value) ||
            identical(current_value, "Tots")) {
          return(invisible(NULL))
        }

        other_ids <- setdiff(
          adl_public_filter_ids,
          current_id
        )

        for (other_id in other_ids) {
          default_value <- if (
            identical(other_id, "public_year")
          ) "Tots" else ""

          updateSelectInput(
            session,
            other_id,
            selected = default_value
          )
        }

        # A previous text search must not hide the newly selected
        # records.
        updateTextInput(
          session,
          "public_search",
          value = ""
        )

      }, ignoreInit = TRUE)
    })
  }

  public_filtered <- reactive({
    x <- public_items()
    if(!is.null(input$public_year) && input$public_year!="Tots"){
      if(input$public_year=="Sense data") x <- x[x$display_year_label=="Sense data",,drop=FALSE]
      else x <- x[x$display_year==input$public_year,,drop=FALSE]
    }
    if(!is.null(input$public_category) && nzchar(input$public_category))
      x <- x[x$category==input$public_category,,drop=FALSE]
    if(!is.null(input$public_person) && nzchar(input$public_person)){
      keep <- vapply(x$related_people,function(z)
        any(tolower(split_semicolon_values(z))==tolower(input$public_person)),logical(1))
      x <- x[keep,,drop=FALSE]
    }
    if(!is.null(input$public_author) && nzchar(input$public_author)){
      keep <- vapply(x$author_or_creator,function(z)
        any(tolower(split_semicolon_values(z))==tolower(input$public_author)),logical(1))
      x <- x[keep,,drop=FALSE]
    }
    if(!is.null(input$public_publication) && nzchar(input$public_publication)){
      keep <- vapply(x$publication_title,function(z)
        any(tolower(split_semicolon_values(z))==tolower(input$public_publication)),logical(1))
      x <- x[keep,,drop=FALSE]
    }
    if(!is.null(input$public_place) && nzchar(input$public_place)){
      keep <- vapply(x$related_places,function(z)
        any(tolower(split_semicolon_values(z))==tolower(input$public_place)),logical(1))
      x <- x[keep,,drop=FALSE]
    }
    if(!is.null(input$public_search) && nzchar(trimws(input$public_search))){
      q <- tolower(trimws(input$public_search))
      cols <- intersect(c("title","title_es","bibliographic_reference","publication_title","description","description_es","historical_context","historical_context_es","legend_contribution","legend_contribution_es","project_interpretation","project_interpretation_es","related_people","related_places","related_claims","related_claims_es","related_events","related_events_es"),names(x))
      blob <- apply(x[,cols,drop=FALSE],1,function(r) paste(tolower(r),collapse=" "))
      x <- x[grepl(q,blob,fixed=TRUE),,drop=FALSE]
    }
    x[order(x$display_year_num,soldevila_sort_key(x$title),na.last=TRUE),,drop=FALSE]
  })

  observe({
    x <- public_filtered()
    z <- lang_rv()
    yy <- ifelse(x$display_year_label=="Sense data",tr("no_date",z),x$display_year_label)
    tt <- localized_col(x,"title",z)
    ch <- if(nrow(x)) setNames(x$item_id,paste0(yy," — ",tt)) else character(0)
    current <- isolate(input$public_item)
    selected <- if(length(ch) && current %in% ch) current else if(length(ch)) ch[[1]] else character(0)
    updateSelectInput(session,"public_item",choices=ch,selected=selected)
  })

  # STEP_DOCUMENT_MODAL_REOPEN_V2
  # Reobertura manual mantenint el filtratge públic del visor original.
  observeEvent(input$adl_open_document_modal, {
    req(input$public_item)
    x <- public_items()
    r <- x[x$item_id==input$public_item,,drop=FALSE]
    if(nrow(r)!=1L || !public_document_available(r)) return()
    z <- lang_rv()
    showModal(modalDialog(
      title=if(identical(z,"es")) "Documentos e imágenes" else "Documents i imatges",
      tags$div(class="adl-doc-modal-body",render_asset_carousel(r,admin=FALSE,lang=z)),
      footer=modalButton(if(identical(z,"es")) "Cerrar" else "Tancar"),
      easyClose=TRUE,size="l"
    ))
  },ignoreInit=TRUE)

  output$public_card <- renderUI({
    req(input$public_item)
    x <- public_items()
    r <- x[x$item_id==input$public_item,,drop=FALSE]
    if(nrow(r)==0) return(tags$div(class="alert alert-secondary",tr("no_records",lang_rv())))
    render_record_card(r,FALSE,lang_rv())
  })


  observe({
    x <- public_items()
    yrs <- sort(unique(x$display_year_num[!is.na(x$display_year_num)]))
    labels <- as.character(yrs)
    if(any(x$display_year_label=="Sense data")) labels <- c(labels,"Sense data")
    cur <- isolate(input$timeline_year)
    selected <- if(is.null(cur) || !nzchar(cur) || !cur %in% c("Tots",labels)) "Tots" else cur
    z <- lang_rv()
    shown <- ifelse(labels=="Sense data",tr("no_date",z),labels)
    updateSelectInput(session,"timeline_year",label=tr("year",z),choices=c(setNames("Tots",tr("all_m",z)),setNames(labels,shown)),selected=selected)
  })

  observe({
    x <- public_items()
    people <- legend_people_values(x)
    cur <- isolate(input$timeline_person)
    selected <- if(!is.null(cur) &&
                   length(cur)==1 &&
                   !is.na(cur) &&
                   cur %in% people) cur else ""
    
    updateSelectInput(
      session,
      "timeline_person",
      label=tr("person",lang_rv()),
      choices=c(
        setNames("",tr("all_m",lang_rv())),
        adl_surname_choices(people)
      ),
      selected=selected
    )
  })

  observe({
    x <- public_items()
    authors <- split_semicolon_values(x$author_or_creator)
    cur <- isolate(input$timeline_author)
    selected <- if(!is.null(cur) && length(cur)==1 && !is.na(cur) && cur %in% authors) cur else ""
    updateSelectInput(session,"timeline_author",label=tr("author",lang_rv()),choices=c(setNames("",tr("all_m",lang_rv())),adl_surname_choices(authors)),selected=selected)
  })

  observe({
    x <- public_items()
    publications <- split_semicolon_values(x$publication_title)
    cur <- isolate(input$timeline_publication)
    selected <- if(!is.null(cur) && length(cur)==1 && !is.na(cur) && cur %in% publications) cur else ""
    updateSelectInput(session,"timeline_publication",label=tr("publication",lang_rv()),choices=c(setNames("",tr("all_f",lang_rv())),setNames(publications,publications)),selected=selected)
  })

  observe({
    x <- public_items()
    places <- split_semicolon_values(x$related_places)
    cur <- isolate(input$timeline_place)
    selected <- if(!is.null(cur) && length(cur)==1 && !is.na(cur) && cur %in% places) cur else ""
    updateSelectInput(session,"timeline_place",label=tr("place",lang_rv()),choices=c(setNames("",tr("all_m",lang_rv())),setNames(places,places)),selected=selected)
  })

  timeline_filtered <- reactive({
    x <- public_items()

    if(!is.null(input$timeline_year) && input$timeline_year!="Tots"){
      if(input$timeline_year=="Sense data"){
        x <- x[is.na(x$display_year_num),,drop=FALSE]
      } else {
        yy <- suppressWarnings(as.numeric(input$timeline_year))
        x <- x[!is.na(x$display_year_num) & x$display_year_num==yy,,drop=FALSE]
      }
    }

    if(!is.null(input$timeline_category) && nzchar(input$timeline_category))
      x <- x[x$category==input$timeline_category,,drop=FALSE]

    if(!is.null(input$timeline_person) && nzchar(input$timeline_person)){
      keep <- vapply(x$related_people,function(z)
        any(tolower(split_semicolon_values(z))==tolower(input$timeline_person)),logical(1))
      x <- x[keep,,drop=FALSE]
    }

    if(!is.null(input$timeline_author) && nzchar(input$timeline_author)){
      keep <- vapply(x$author_or_creator,function(z)
        any(tolower(split_semicolon_values(z))==tolower(input$timeline_author)),logical(1))
      x <- x[keep,,drop=FALSE]
    }

    if(!is.null(input$timeline_publication) && nzchar(input$timeline_publication)){
      keep <- vapply(x$publication_title,function(z)
        any(tolower(split_semicolon_values(z))==tolower(input$timeline_publication)),logical(1))
      x <- x[keep,,drop=FALSE]
    }

    if(!is.null(input$timeline_place) && nzchar(input$timeline_place)){
      keep <- vapply(x$related_places,function(z)
        any(tolower(split_semicolon_values(z))==tolower(input$timeline_place)),logical(1))
      x <- x[keep,,drop=FALSE]
    }

    q <- tolower(trim_or_blank(input$timeline_search))
    if(nzchar(q)){
      cols <- intersect(
        c("title","title_es","bibliographic_reference","publication_title","author_or_creator","description","description_es","historical_context","historical_context_es","legend_contribution","legend_contribution_es","project_interpretation","project_interpretation_es","related_people","related_places","related_claims","related_claims_es","related_events","related_events_es"),
        names(x)
      )
      blob <- apply(x[,cols,drop=FALSE],1,function(r) paste(tolower(r),collapse=" "))
      x <- x[grepl(q,blob,fixed=TRUE),,drop=FALSE]
    }

    x[order(x$display_year_num,soldevila_sort_key(x$title),na.last=TRUE),,drop=FALSE]
  })

  # ADL_TIMELINE_VISUAL_V1: esdeveniments públics per període; sense fitxes en draft.
  observe({
    input$adl_timeline_ready
    x <- public_items()
    z <- lang_rv()
    years <- suppressWarnings(as.numeric(x$display_year_num))
    period <- ifelse(!is.na(years) & years <= 1898,"p1",
      ifelse(!is.na(years) & years <= 1951,"p2",
      ifelse(!is.na(years) & years <= 1963,"p3",
      ifelse(!is.na(years) & years <= 2000,"p4",
      ifelse(!is.na(years) & years <= 2025,"p5",NA_character_)))))
    use <- which(!is.na(period) & !is.na(x$item_id) & nzchar(as.character(x$item_id)))
    events <- lapply(use,function(i)list(
      item_id=as.character(x$item_id[i]),
      year_label=as.character(x$display_year_label[i]),
      title=as.character(localized_col(x[i,,drop=FALSE],"title",z)[1]),
      category=as.character(x$category[i]),
      period=period[i]
    ))
    session$sendCustomMessage("adl_timeline_data",list(events=events,lang=z))
  })

  # ADL_BIB_INTERNAL_V1 — internal navigation, public records only.
  observeEvent(input$adl_bib_open_item,{
    target_id <- trim_or_blank(input$adl_bib_open_item)
    x <- public_items()
    req(nzchar(target_id),target_id %in% x$item_id)
    updateSelectInput(session,"public_year",selected="Tots")
    updateSelectInput(session,"public_category",selected="")
    updateSelectInput(session,"public_person",selected="")
    updateSelectInput(session,"public_author",selected="")
    updateSelectInput(session,"public_publication",selected="")
    updateSelectInput(session,"public_place",selected="")
    updateTextInput(session,"public_search",value="")
    session$onFlushed(function(){
      updateSelectInput(session,"public_item",selected=target_id)
      try(bslib::nav_select("main_nav","Fitxes",session=session),silent=TRUE)
    },once=TRUE)
  },ignoreInit=TRUE)

  observeEvent(input$adl_timeline_item,{
    target_id <- trim_or_blank(input$adl_timeline_item)
    x <- public_items()
    req(nzchar(target_id),target_id %in% x$item_id)
    updateSelectInput(session,"public_year",selected="Tots")
    updateSelectInput(session,"public_category",selected="")
    updateSelectInput(session,"public_person",selected="")
    updateSelectInput(session,"public_author",selected="")
    updateSelectInput(session,"public_publication",selected="")
    updateSelectInput(session,"public_place",selected="")
    updateTextInput(session,"public_search",value="")
    session$onFlushed(function(){
      updateSelectInput(session,"public_item",selected=target_id)
      try(bslib::nav_select("main_nav","Fitxes",session=session),silent=TRUE)
    },once=TRUE)
  },ignoreInit=TRUE)

  observe({
    z <- lang_rv()
    updateRadioButtons(session,"adl_timeline_view",choices=if(identical(z,"es"))
      c("Cronología visual"="visual","Lista documental"="list") else
      c("Cronologia visual"="visual","Llista documental"="list"),
      selected={v <- isolate(input$adl_timeline_view);if(is.null(v))"visual" else v})
  })

  # ADL_TIMELINE_LIST_DIRECT_ID_V1
  # Each row carries its immutable item_id in a hidden column.
  # Navigation uses the ID, never the displayed row position.
  output$public_timeline <- renderDT({
    x <- timeline_filtered()
    z <- lang_rv()

    out <- data.frame(
      Any=ifelse(
        x$display_year_label=="Sense data",
        tr("no_date",z),
        x$display_year_label
      ),
      Titol=localized_col(x,"title",z),
      Referencia=x$bibliographic_reference,
      Autor=x$author_or_creator,
      Categoria=vapply(
        seq_len(nrow(x)),
        function(i) cat_info(x$category[i],z)$label,
        character(1)
      ),
      ItemID=x$item_id,
      stringsAsFactors=FALSE
    )

    datatable(
      out,
      rownames=FALSE,
      filter="top",
      selection="none",
      options=list(
        pageLength=25,
        scrollX=TRUE,
        order=list(),
        columnDefs=list(
          list(
            targets=5,
            visible=FALSE,
            searchable=FALSE,
            orderable=FALSE
          )
        )
      ),
      colnames=c(
        tr("col_year",z),
        tr("col_title",z),
        tr("col_bib",z),
        tr("col_author",z),
        tr("col_category",z),
        "ItemID"
      ),
      callback=DT::JS("
        table.on('click', 'tbody tr', function() {
          var data = table.row(this).data();
          if (!data || !data[5]) return;

          if (window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue(
              'adl_timeline_item',
              String(data[5]),
              {priority: 'event'}
            );
          }
        });
      ")
    )
  })

  observeEvent(input$public_timeline_rows_selected,{
    idx <- input$public_timeline_rows_selected
    req(length(idx)==1)

    x <- timeline_filtered()
    req(idx >= 1, idx <= nrow(x))
    target_id <- x$item_id[idx]

    # Clear Fitxes filters so the target cannot be hidden.
    updateSelectInput(session,"public_year",selected="Tots")
    updateSelectInput(session,"public_category",selected="")
    updateSelectInput(session,"public_person",selected="")
    updateSelectInput(session,"public_author",selected="")
    updateSelectInput(session,"public_publication",selected="")
    updateSelectInput(session,"public_place",selected="")
    updateTextInput(session,"public_search",value="")

    # Select target after filter reset.
    session$onFlushed(function(){
      updateSelectInput(session,"public_item",selected=target_id)
      try(bslib::nav_select("main_nav","Fitxes",session=session),silent=TRUE)
    },once=TRUE)
  },ignoreInit=TRUE)

  # ----------------------------------------------------------
  # Guia de lectura -> Fitxa documental
  # ----------------------------------------------------------
  observeEvent(input$guide_item,{
    target_id <- trim_or_blank(input$guide_item)
    req(nzchar(target_id))

    x <- public_items()

    if(!(target_id %in% x$item_id)){
      showNotification(
        if(identical(lang_rv(),"es"))
          paste0("La ficha ",target_id," no está disponible públicamente.")
        else
          paste0("La fitxa ",target_id," no està disponible públicament."),
        type="warning"
      )
      return()
    }

    # Netejar els filtres de Fitxes perquè la fitxa objectiu
    # no pugui quedar amagada.
    updateSelectInput(session,"public_year",selected="Tots")
    updateSelectInput(session,"public_category",selected="")
    updateSelectInput(session,"public_person",selected="")
    updateSelectInput(session,"public_author",selected="")
    updateSelectInput(session,"public_publication",selected="")
    updateSelectInput(session,"public_place",selected="")
    updateTextInput(session,"public_search",value="")

    # Esperar que Shiny actualitzi els filtres.
    session$onFlushed(function(){

      updateSelectInput(
        session,
        "public_item",
        selected=target_id
      )

      try(
        bslib::nav_select(
          "main_nav",
          "Fitxes",
          session=session
        ),
        silent=TRUE
      )

    },once=TRUE)

  },ignoreInit=TRUE)

  output$admin_ui <- renderUI({
    if(!admin_ok()){
      card(card_header("Accés d’administració"),
        passwordInput("admin_password","Contrasenya"),
        actionButton("admin_login","Entrar"),
        tags$hr(),
        tags$p(class="text-muted","La contrasenya es defineix fora del codi amb XIPAHUATZIN_ADMIN_PASSWORD.")
      )
    } else {
      navset_card_tab(id="admin_tabs",
        nav_panel("Fitxa",
          uiOutput("admin_active_header"),
          navset_pill(id="admin_fitxa_tabs",
            nav_panel("Resum",
              layout_sidebar(
                sidebar=sidebar(
                  width=250,
                  selectInput("admin_year","Any",choices=NULL,selectize=FALSE),
                  selectInput("admin_category","Categoria",
                    choices=c("Totes"="",setNames(categories$category,categories$category_label)),
                    selectize=FALSE),
                  selectInput("admin_person","Personatge",choices=c("Tots"=""),selectize=FALSE),
                  selectInput("admin_author","Autor",choices=c("Tots"=""),selectize=FALSE),
                  selectInput("admin_publication","Publicació / revista",choices=c("Totes"=""),selectize=FALSE),
                  selectInput("admin_place","Lloc",choices=c("Tots"=""),selectize=FALSE),
                  textInput("admin_search","Cerca","")
                ),
                tags$div(class="admin-full-input",
                  selectInput("admin_item","Peça",choices=NULL,width="100%")
                ),
                tags$div(class="admin-fitxa-full",uiOutput("admin_card")),
                tags$div(style="margin-top:1rem;",
                  actionButton("edit_item","Editar fitxa",class="btn-primary"),
                  actionButton("go_add_document_from_card","Afegir document a aquesta fitxa",class="btn-outline-primary")
                )
              )
            ),
            nav_panel("Editor",uiOutput("editor_ui")),
            nav_panel("Visualització",uiOutput("admin_viewer_ui"),uiOutput("admin_viewer_content")),
            nav_panel("Documents",
              navset_pill(id="admin_documents_tabs",
                nav_panel("Actius",uiOutput("admin_assets_ui")),
                nav_panel("Preautorització",uiOutput("preauth_ui"),DTOutput("preauth_table")),
                nav_panel("Autoritzacions",uiOutput("permissions_ui"),DTOutput("permissions_table"))
              )
            ),
            nav_panel("Vinculació",uiOutput("linkage_ui"),DTOutput("linkage_table")),
            nav_panel("Historial",DTOutput("history_table"))
          )
        ),
        nav_panel("Incorporació",uiOutput("import_record_ui")),
        nav_panel("Inventari",DTOutput("admin_inventory")),
        nav_panel("Configuració",
          navset_pill(id="admin_config_tabs",
            nav_panel("Categories",DTOutput("admin_categories")),
            nav_panel("Drets",DTOutput("admin_rights"))
          )
        ),
        nav_panel("Exportacions",
          p("La base SQLite és la font canònica. Aquestes exportacions són còpies de treball/auditoria."),
          downloadButton("download_csv","Exportar CSV"),
          tags$span(" "),
          downloadButton("download_xlsx","Exportar XLSX"),
          tags$div(style="margin-top:1rem;",
            p(class="text-muted","Per a XLSX cal tenir instal·lat el paquet writexl.")
          )
        )
      )
    }
  })



  # ======================================================================
  # PERMISSION WORKFLOW — UI
  # ======================================================================

  permission_refresh <- reactiveVal(0L)

  output$preauth_ui <- renderUI({
    req(admin_ok(),input$admin_item)
    permission_refresh()

    x <- items_rv()
    r <- x[x$item_id==input$admin_item,,drop=FALSE]
    if(!nrow(r))
      return(tags$div(class="alert alert-danger","No s'ha trobat la fitxa activa."))

    con <- db_connect(); on.exit(DBI::dbDisconnect(con),add=TRUE)

    existing <- DBI::dbGetQuery(con,"
      SELECT asset_id,source_item_id,title,rights_status,public_display,private_source
      FROM item_assets
      WHERE item_id=?
      ORDER BY sort_order,asset_id
    ",params=list(input$admin_item))

    pending <- existing[existing$rights_status %in% c(
      "PERMISSION_PENDING","RIGHTS_UNKNOWN","CITATION_ONLY",
      "PRIVATE_RESEARCH_COPY","PERMISSION_DENIED","PERMISSION_GRANTED"
    ),,drop=FALSE]

    header <- tags$div(class="alert alert-primary",
      tags$b("Fitxa activa: "),
      paste0(r$display_year_label[1]," — ",r$title[1]),
      tags$div(class="text-muted",paste0("ID: ",r$item_id[1]))
    )

    if(nrow(existing)){
      doc_lines <- lapply(seq_len(nrow(existing)),function(i){
        a <- existing[i,,drop=FALSE]
        tags$li(
          tags$b(ifelse(nzchar(trim_or_blank(a$title[1])),a$title[1],paste0("Actiu ",a$asset_id[1]))),
          paste0(
            " · ",
            ifelse(nzchar(trim_or_blank(a$source_item_id[1])),
                   a$source_item_id[1],"sense ITM"),
            " · ",a$rights_status[1]
          )
        )
      })

      return(tagList(
        header,
        tags$div(class="alert alert-success",
          tags$b("Documentació ja carregada per a aquesta fitxa."),
          tags$div(
            "No cal tornar a carregar cap document a Preautorització. ",
            "Continua a la pestanya Autoritzacions per registrar o aplicar el permís."
          )
        ),
        tags$div(class="tab-section",
          tags$h5("Documents ja associats"),
          tags$ul(doc_lines)
        ),
        tags$div(class="alert alert-light border",
          "Si en el futur vols afegir un document addicional a aquesta mateixa fitxa, ",
          "fes-ho des d’Actius o mitjançant el flux específic d’incorporació; ",
          "Preautorització queda reservada a fitxes sense document associat."
        )
      ))
    }

    tagList(
      header,
      tags$div(class="alert alert-info",
        tags$b("Aquesta fitxa encara no té cap document associat. "),
        "Carrega aquí el document font. Quedarà automàticament vinculat a la fitxa, ",
        "en privat i amb estat PERMISSION_PENDING."
      ),
      tags$div(class="admin-full-input",
        fileInput("preauth_files","Document(s)",multiple=TRUE,
                  accept=c(".pdf","image/*",".doc",".docx",".txt",".html"))
      ),
      tags$div(class="editor-grid",
        textInput("preauth_title","Títol / descripció comuna",""),
        textInput("preauth_author","Autor / productor",""),
        textInput("preauth_date","Data del document",""),
        textInput("preauth_source","Procedència / publicació / arxiu",""),
        textInput("preauth_holder","Titular probable dels drets",""),
        tags$div(class="editor-wide",
          textAreaInput("preauth_notes","Notes internes","",rows=3,width="100%")
        )
      ),
      actionButton("preauth_add","Incorporar a aquesta fitxa",class="btn-primary"),
      tags$div(class="asset-upload-log",verbatimTextOutput("preauth_log")),
      tags$hr(),
      tags$h5("Documents d’aquesta fitxa pendents d’autorització")
    )
  })

  output$preauth_table <- renderDT({
    req(admin_ok(),input$admin_item)
    permission_refresh()
    con <- db_connect(); on.exit(DBI::dbDisconnect(con),add=TRUE)
    a <- DBI::dbGetQuery(con,"
      SELECT asset_id, source_item_id, title,
             author_or_creator, document_date, copyright_holder,
             rights_status, public_display
      FROM item_assets
      WHERE item_id=?
        AND rights_status IN ('PERMISSION_PENDING','RIGHTS_UNKNOWN')
      ORDER BY sort_order, asset_id
    ",params=list(input$admin_item))
    datatable(a,rownames=FALSE,filter="top",
              options=list(pageLength=15,scrollX=TRUE))
  })

  output$permissions_ui <- renderUI({
    req(admin_ok(),input$admin_item)
    permission_refresh()

    x <- items_rv()
    r <- x[x$item_id==input$admin_item,,drop=FALSE]
    if(!nrow(r))
      return(tags$div(class="alert alert-danger","No s'ha trobat la fitxa activa."))

    con <- db_connect(); on.exit(DBI::dbDisconnect(con),add=TRUE)

    a <- DBI::dbGetQuery(con,"
      SELECT ia.asset_id,ia.item_id,ia.source_item_id,ia.title,
             ia.rights_status,ia.copyright_holder
      FROM item_assets ia
      LEFT JOIN permission_assets pa ON pa.asset_id=ia.asset_id
      WHERE ia.item_id=?
        AND (
          ia.rights_status IN (
            'PERMISSION_PENDING','RIGHTS_UNKNOWN','CITATION_ONLY',
            'PRIVATE_RESEARCH_COPY','PERMISSION_DENIED'
          )
          OR (ia.rights_status='PERMISSION_GRANTED'
              AND pa.permission_asset_id IS NULL)
        )
      ORDER BY ia.sort_order,ia.asset_id
    ",params=list(input$admin_item))

    choices <- if(nrow(a)){
      setNames(
        as.character(a$asset_id),
        paste0(
          ifelse(nzchar(a$source_item_id),a$source_item_id,"[sense ITM]"),
          " · ",a$title," · ",a$rights_status
        )
      )
    } else character(0)

    one_asset <- nrow(a)==1
    selected_asset <- if(one_asset) as.character(a$asset_id[1]) else character(0)

    doc_selector <- if(!nrow(a)){
      tags$div(class="alert alert-secondary",
        "Aquesta fitxa no té cap document pendent de resolució o sense autorització formal registrada."
      )
    } else if(one_asset){
      tagList(
        tags$div(class="alert alert-success",
          tags$b("Document detectat automàticament: "),
          paste0(a$title[1]," · ",a$rights_status[1]),
          if(nzchar(a$source_item_id[1]))
            tags$div(class="text-muted",paste0("Font: ",a$source_item_id[1]))
        ),
        tags$div(style="display:none;",
          selectizeInput("perm_assets","Documents",choices=choices,
                         selected=selected_asset,multiple=TRUE,width="100%")
        )
      )
    } else {
      tagList(
        tags$p("Aquesta fitxa té més d’un document. Selecciona els que cobreix aquesta autorització:"),
        tags$div(class="admin-full-input",
          selectizeInput(
            "perm_assets","Documents",choices=choices,multiple=TRUE,width="100%",
            options=list(placeholder="Selecciona un o diversos documents...")
          )
        )
      )
    }

    tagList(
      tags$div(class="alert alert-primary",
        tags$b("Fitxa activa: "),
        paste0(r$display_year_label[1]," — ",r$title[1]),
        tags$div(class="text-muted",paste0("ID: ",r$item_id[1]))
      ),
      doc_selector,

      tags$div(class="alert alert-light border",
        tags$h5("Importar paquet d’autorització"),
        fileInput("perm_package_file","Importar paquet d’autorització (.json)",
                  multiple=FALSE,accept=".json"),
        actionButton("perm_package_review","Revisar paquet",class="btn-outline-primary"),
        tags$div(class="asset-edit-log",verbatimTextOutput("perm_package_log")),
        uiOutput("perm_package_apply_ui")
      ),
      tags$hr(),

      if(nrow(a)) tagList(
        tags$hr(),
        tags$h5("1. Evidència i text de l’autorització"),
        fileInput("perm_evidence","Correu / PDF / captura / document d’autorització",
                  multiple=FALSE,accept=NULL),
        textAreaInput("perm_text","Text del correu o autorització",
                      "",rows=8,width="100%"),
        actionButton("perm_analyse","Analitzar text",class="btn-outline-primary"),
        tags$div(class="asset-edit-log",verbatimTextOutput("perm_analysis_log")),

        tags$hr(),
        tags$h5("2. Dades i condicions — confirmar abans d’aplicar"),
        tags$div(class="editor-grid",
          textInput("perm_holder","Titular / institució",""),
          textInput("perm_person","Persona que autoritza",""),
          textInput("perm_role","Càrrec",""),
          textInput("perm_date","Data de l’autorització",""),
          selectInput("perm_status","Resolució",
            choices=c("PERMISSION_GRANTED","PERMISSION_DENIED","CITATION_ONLY"),
            selected="PERMISSION_GRANTED"),
          selectInput("perm_display","Reproducció pública",
            choices=c("Cap"="none","Miniatura"="thumbnail",
                      "Fragment"="excerpt","Complet"="full"),
            selected="full"),
          selectInput("perm_download","Descàrrega pública",
            choices=c("No"="no","Sí"="yes"),selected="no"),
          textInput("perm_credit","Crèdit / referència obligatòria",""),
          tags$div(class="editor-wide",
            textAreaInput("perm_scope","Abast de l’autorització","",
                          rows=2,width="100%")
          ),
          tags$div(class="editor-wide",
            textAreaInput("perm_conditions","Condicions / text rellevant","",
                          rows=4,width="100%")
          ),
          tags$div(class="editor-wide",
            textAreaInput("perm_notes","Notes internes","",
                          rows=2,width="100%")
          )
        ),

        tags$h5("3. Aplicació"),
        if(one_asset)
          tags$p("L’autorització s’aplicarà al document detectat d’aquesta fitxa.")
        else
          tags$p("L’autorització s’aplicarà només als documents seleccionats d’aquesta fitxa."),
        actionButton("perm_apply","Registrar autorització i aplicar",
                     class="btn-success"),
        tags$div(class="asset-upload-log",verbatimTextOutput("perm_apply_log"))
      ),

      tags$hr(),
      tags$h5("Autoritzacions registrades per aquesta fitxa")
    )
  })

  output$permissions_table <- renderDT({
    req(admin_ok(),input$admin_item)
    permission_refresh()
    con <- db_connect(); on.exit(DBI::dbDisconnect(con),add=TRUE)
    p <- DBI::dbGetQuery(con,"
      SELECT p.permission_id,p.rights_status,p.rights_holder,
             p.authorizing_person,p.permission_date,p.allowed_display,
             p.public_download,p.required_credit,
             COUNT(pa.permission_asset_id) AS n_documents
      FROM permissions p
      JOIN permission_assets pa ON pa.permission_id=p.permission_id
      WHERE pa.target_item_id=?
      GROUP BY p.permission_id
      ORDER BY p.permission_id DESC
    ",params=list(input$admin_item))
    datatable(p,rownames=FALSE,filter="top",
              options=list(pageLength=15,scrollX=TRUE))
  })

  # ----------------------------------------------------------------------
  # PREAUTORITZACIÓ: document(s) -> ITM -> library -> asset -> link
  # ----------------------------------------------------------------------

  observeEvent(input$preauth_add,{
    req(admin_ok(),input$admin_item,input$preauth_files)

    msg <- tryCatch({
      files <- input$preauth_files
      if(is.null(files) || !nrow(files)) stop("Selecciona almenys un document.")

      target_id <- trim_or_blank(input$admin_item)
      if(!nzchar(target_id)) stop("No hi ha cap fitxa activa.")

      con <- db_connect(); on.exit(DBI::dbDisconnect(con),add=TRUE)

      tgt <- DBI::dbGetQuery(con,"
        SELECT item_id,record_status FROM items WHERE item_id=?
      ",params=list(target_id))
      if(!nrow(tgt)) stop("No existeix la fitxa activa.")
      if(!tgt$record_status[1] %in% c("reviewed","publishable","published"))
        stop("La fitxa activa no és una fitxa documental publicable/revisada.")

      dir.create("private_sources/library",recursive=TRUE,showWarnings=FALSE)

      added <- character(0)

      for(i in seq_len(nrow(files))){
        original <- files$name[i]
        src <- files$datapath[i]
        ext <- tolower(tools::file_ext(original))
        stub <- tools::file_path_sans_ext(original)

        itm <- next_itm_id(con,stub)

        canonical_name <- paste0(
          itm,"__",
          gsub("[/\\\\]","_",original)
        )
        dest <- file.path("private_sources","library",canonical_name)

        if(!file.copy(src,dest,overwrite=FALSE))
          stop("No s'ha pogut copiar ",original," a la biblioteca privada.")

        base_title <- trim_or_blank(input$preauth_title)
        ttl <- if(nrow(files)>1 && nzchar(base_title))
          paste0(base_title," — ",tools::file_path_sans_ext(original))
        else if(nzchar(base_title)) base_title
        else tools::file_path_sans_ext(original)

        insert_minimal_draft_item(
          con=con,item_id=itm,title=ttl,file_name=original,
          portable_source=gsub("\\\\","/",dest),
          author=trim_or_blank(input$preauth_author),
          document_date=trim_or_blank(input$preauth_date),
          source_or_archive=trim_or_blank(input$preauth_source),
          rights_status="PERMISSION_PENDING",
          copyright_holder=trim_or_blank(input$preauth_holder),
          notes=trim_or_blank(input$preauth_notes)
        )

        append_portable_manifest_row(
          itm,original,gsub("\\\\","/",dest)
        )

        ord <- DBI::dbGetQuery(
          con,"SELECT COALESCE(MAX(sort_order),0)+1 AS n
               FROM item_assets WHERE item_id=?",
          params=list(target_id)
        )$n[1]

        typ <- if(ext=="pdf") "pdf" else
          if(ext %in% c("png","jpg","jpeg","gif","webp","tif","tiff")) "image"
          else "document"

        DBI::dbExecute(con,"
          INSERT INTO item_assets
          (item_id,asset_type,title,caption,author_or_creator,document_date,
           rights_status,copyright_holder,permission_source,
           public_display,public_download,private_source,public_asset,
           pixelated_preview,sort_order,notes,created_at,created_by,source_item_id)
          VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
        ",params=list(
          target_id,typ,ttl,
          "Document incorporat en fase de preautorització.",
          trim_or_blank(input$preauth_author),
          trim_or_blank(input$preauth_date),
          "PERMISSION_PENDING",
          trim_or_blank(input$preauth_holder),
          "",
          "none","no",
          gsub("\\\\","/",dest),"","",
          as.integer(ord),
          trim_or_blank(input$preauth_notes),
          format(Sys.time(),"%Y-%m-%d %H:%M:%S"),
          Sys.getenv("XIPAHUATZIN_ADMIN_USER",unset="admin"),
          itm
        ))

        aid <- DBI::dbGetQuery(con,"SELECT last_insert_rowid() AS id")$id[1]

        DBI::dbExecute(con,"
          INSERT INTO item_links
          (source_item_id,target_item_id,link_type,asset_id,notes,created_at,created_by)
          VALUES (?,?,?,?,?,?,?)
        ",params=list(
          itm,target_id,"evidence_asset",aid,
          "Incorporació preautorització.",
          format(Sys.time(),"%Y-%m-%d %H:%M:%S"),
          Sys.getenv("XIPAHUATZIN_ADMIN_USER",unset="admin")
        ))

        added <- c(added,paste0(itm," → ",target_id," → asset ",aid))
      }

      refresh_items()
      permission_refresh(permission_refresh()+1L)
      if(exists("asset_refresh",inherits=FALSE))
        asset_refresh(asset_refresh()+1L)

      showNotification(
        paste0(length(added)," document(s) incorporat(s) a la fitxa activa."),
        type="message",duration=6
      )

      paste0(
        "PASS — PREAUTORITZACIÓ\n\n",
        "Fitxa: ",target_id,"\n",
        paste0("• ",added,collapse="\n")
      )

    },error=function(e){
      showNotification(conditionMessage(e),type="error",duration=10)
      paste0("ERROR\n\n",conditionMessage(e))
    })

    output$preauth_log <- renderText(msg)
  })

  # ----------------------------------------------------------------------
  # ANÀLISI BÀSICA DEL TEXT — només preomple, mai aplica
  # ----------------------------------------------------------------------

  # PERMISSION_PACKAGE_IMPORT_SIMPLE_V1
  permission_package_rv <- reactiveVal(NULL)
  permission_package_matches_rv <- reactiveVal(data.frame())
  permission_package_path_rv <- reactiveVal("")

  observeEvent(input$perm_package_review,{
    req(admin_ok(),input$perm_package_file)
    msg <- tryCatch({
      if(!requireNamespace("jsonlite",quietly=TRUE)) stop("Cal instal·lar jsonlite.")
      pkg <- jsonlite::fromJSON(input$perm_package_file$datapath[1],simplifyVector=FALSE)
      if(is.null(pkg$schema) || pkg$schema!="ADL_PERMISSION_PACKAGE_V1")
        stop("Paquet JSON no reconegut.")
      if(is.null(pkg$groups) || !length(pkg$groups)) stop("El paquet no conté grups.")
      con <- db_connect(); on.exit(DBI::dbDisconnect(con),add=TRUE)
      aa <- DBI::dbGetQuery(con,"SELECT asset_id,item_id,source_item_id,title,author_or_creator,rights_status FROM item_assets ORDER BY item_id,sort_order,asset_id")
      out <- list()
      norm <- function(x){ x<-tolower(trim_or_blank(x)); x<-iconv(x,to="ASCII//TRANSLIT"); x<-gsub("[^a-z0-9]+"," ",x); trimws(gsub("[[:space:]]+"," ",x)) }
      # PERMISSION_PACKAGE_TARGET_FILTER_V1
      for(g in pkg$groups){
        gid <- if(is.null(g$group_id)) "GROUP" else as.character(g$group_id)
        ids <- if(is.null(g$match$source_item_ids)) character(0) else unlist(g$match$source_item_ids,use.names=FALSE)
        targets <- if(is.null(g$match$target_item_ids)) character(0) else unlist(g$match$target_item_ids,use.names=FALSE)
        aliases <- if(is.null(g$match$author_aliases)) character(0) else unlist(g$match$author_aliases,use.names=FALSE)
        ids <- trimws(as.character(ids))
        targets <- trimws(as.character(targets))
        ids <- ids[nzchar(ids)]
        targets <- targets[nzchar(targets)]
        aliases <- unique(vapply(aliases,norm,character(1)))
        for(i in seq_len(nrow(aa))){
          target_id <- trim_or_blank(aa$item_id[i])
          # Si el JSON declara target_item_ids, qualsevol altre destí queda exclòs.
          if(length(targets) && !(target_id %in% targets)) next
          basis <- character(0)
          sid <- trim_or_blank(aa$source_item_id[i])
          if(length(ids) && nzchar(sid) && sid %in% ids) basis <- c(basis,"source_item_id")
          au <- norm(aa$author_or_creator[i])
          if(length(aliases) && nzchar(au) && au %in% aliases) basis <- c(basis,"autor")
          if(length(targets) && target_id %in% targets) basis <- c(basis,"target_item_id")
          if(length(basis)){
            ap <- g$apply
            out[[length(out)+1L]] <- data.frame(
              group_id=gid,asset_id=aa$asset_id[i],target_item_id=aa$item_id[i],source_item_id=sid,
              title=aa$title[i],match_basis=paste(unique(basis),collapse="+"),
              rights_status=if(is.null(ap$rights_status)) "PERMISSION_GRANTED" else as.character(ap$rights_status),
              public_display=if(is.null(ap$public_display)) "none" else as.character(ap$public_display),
              public_download=if(is.null(ap$public_download)) "no" else as.character(ap$public_download),
              holder=if(is.null(ap$copyright_holder)) "" else as.character(ap$copyright_holder),
              credit=if(is.null(ap$credit)) "" else as.character(ap$credit),
              stringsAsFactors=FALSE)
          }
        }
      }
      mm <- if(length(out)) do.call(rbind,out) else data.frame()
      if(nrow(mm)){ mm$ambiguous <- duplicated(mm$asset_id) | duplicated(mm$asset_id,fromLast=TRUE) }
      permission_package_rv(pkg); permission_package_matches_rv(mm); permission_package_path_rv(input$perm_package_file$datapath[1])
      if(!nrow(mm)) return("COINCIDÈNCIES: 0\nNo s’aplicarà cap canvi.")
      lines <- vapply(seq_len(nrow(mm)),function(i) paste0(if(mm$ambiguous[i]) "REVISAR" else "APLICAR"," | ",mm$group_id[i]," | asset ",mm$asset_id[i]," | ",mm$source_item_id[i]," → ",mm$target_item_id[i]," | ",mm$title[i]," | ",mm$rights_status[i]," / ",mm$public_display[i]," | crèdit: ",mm$credit[i]),character(1))
      paste0("COINCIDÈNCIES: ",nrow(mm),"\nAPLICABLES: ",sum(!mm$ambiguous),"\nAMBIGÜES: ",sum(mm$ambiguous),"\n\n",paste(lines,collapse="\n"))
    },error=function(e) paste0("ERROR\n\n",conditionMessage(e)))
    output$perm_package_log <- renderText(msg)
  })

  output$perm_package_apply_ui <- renderUI({
    mm <- permission_package_matches_rv()
    if(!nrow(mm) || !any(!mm$ambiguous)) return(NULL)
    actionButton("perm_package_apply","Aplicar paquet d’autorització",class="btn-success")
  })

  observeEvent(input$perm_package_apply,{
    req(admin_ok())
    pkg <- permission_package_rv(); mm <- permission_package_matches_rv(); pth <- permission_package_path_rv()
    msg <- tryCatch({
      if(is.null(pkg) || !nrow(mm) || !nzchar(pth)) stop("Primer has de revisar el paquet.")
      mm <- mm[!mm$ambiguous,,drop=FALSE]
      if(!nrow(mm)) stop("No hi ha coincidències aplicables.")
      con <- db_connect(); on.exit(DBI::dbDisconnect(con),add=TRUE)
      dir.create(admin_private_path("permissions"),recursive=TRUE,showWarnings=FALSE)
      pkgid <- if(is.null(pkg$package_id)) paste0("PERM_PACKAGE_",format(Sys.time(),"%Y%m%d%H%M%S")) else as.character(pkg$package_id)
      edest <- file.path(admin_private_path("permissions"),paste0(gsub("[^A-Za-z0-9._-]+","_",pkgid),".json"))
      if(!file.copy(pth,edest,overwrite=TRUE)) stop("No s’ha pogut conservar el JSON.")
      edest <- normalizePath(edest,winslash="/",mustWork=TRUE)
      now <- format(Sys.time(),"%Y-%m-%d %H:%M:%S"); usr <- Sys.getenv("XIPAHUATZIN_ADMIN_USER",unset="admin")
      applied <- character(0)
      for(i in seq_len(nrow(mm))){
        row <- mm[i,,drop=FALSE]
        g <- pkg$groups[[which(vapply(pkg$groups,function(z) as.character(z$group_id)==row$group_id,logical(1)))[1]]]
        ap <- g$apply
        status <- row$rights_status; display <- row$public_display; download <- row$public_download
        if(status!="PERMISSION_GRANTED"){ display <- "none"; download <- "no" }
        a <- DBI::dbGetQuery(con,"SELECT * FROM item_assets WHERE asset_id=?",params=list(as.integer(row$asset_id)))
        if(!nrow(a)) next
        pub <- ""
        if(status=="PERMISSION_GRANTED" && display!="none"){
          ps <- trim_or_blank(a$private_source[1]); if(!nzchar(ps) || !file.exists(ps)) stop("No es troba l’original privat de l’actiu ",row$asset_id)
          d <- make_linked_derivative(ps,a$item_id[1],if(nzchar(a$source_item_id[1])) a$source_item_id[1] else paste0("ASSET_",row$asset_id),"PERMISSION_GRANTED",display)
          pub <- trim_or_blank(d$public_asset)
        }
        psource <- if(is.null(ap$permission_source)) paste0("Paquet ",pkgid) else as.character(ap$permission_source)
        psource <- paste0(psource," · ",edest)
        cap <- trim_or_blank(a$caption[1]); cr <- row$credit
        if(nzchar(cr) && !grepl(cr,cap,fixed=TRUE)) cap <- paste(c(cap,cr)[nzchar(c(cap,cr))],collapse=" ")
        DBI::dbExecute(con,"UPDATE item_assets SET rights_status=?,copyright_holder=?,permission_source=?,public_display=?,public_download=?,public_asset=?,pixelated_preview='',caption=? WHERE asset_id=?",params=list(status,row$holder,psource,display,download,pub,cap,as.integer(row$asset_id)))
        pid <- paste0(gsub("[^A-Za-z0-9._-]+","_",pkgid),"__",gsub("[^A-Za-z0-9._-]+","_",row$group_id))
        DBI::dbExecute(con,"INSERT OR IGNORE INTO permissions(permission_id,rights_status,rights_holder,authorizing_person,authorizing_role,scope,allowed_display,public_download,required_credit,conditions_text,evidence_private_source,evidence_file_name,notes,created_at,created_by) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)",params=list(pid,status,row$holder,if(is.null(pkg$authorizing_person)) "" else as.character(pkg$authorizing_person),if(is.null(pkg$authorizing_capacity)) "" else as.character(pkg$authorizing_capacity),if(is.null(ap$scope)) "" else as.character(ap$scope),display,download,cr,if(is.null(pkg$evidence_text)) "" else as.character(pkg$evidence_text),edest,basename(edest),paste0("Importat des de ",pkgid),now,usr))
        DBI::dbExecute(con,"INSERT OR IGNORE INTO permission_assets(permission_id,asset_id,source_item_id,target_item_id,applied_at,applied_by) VALUES (?,?,?,?,?,?)",params=list(pid,as.integer(row$asset_id),a$source_item_id[1],a$item_id[1],now,usr))
        applied <- c(applied,paste0("asset ",row$asset_id," → ",a$item_id[1]," | ",cr))
      }
      permission_refresh(permission_refresh()+1L); if(exists("asset_refresh",inherits=FALSE)) asset_refresh(asset_refresh()+1L); refresh_items()
      paste0("PASS — PAQUET APLICAT\n\n",paste0("• ",applied,collapse="\n"))
    },error=function(e) paste0("ERROR\n\n",conditionMessage(e)))
    output$perm_package_log <- renderText(msg)
  })

  observeEvent(input$perm_analyse,{
    req(admin_ok())
    g <- permission_guess_from_text(input$perm_text)

    updateSelectInput(session,"perm_status",selected=g$status)
    updateSelectInput(session,"perm_display",selected=g$display)
    updateSelectInput(session,"perm_download",selected=g$download)
    if(nzchar(g$credit))
      updateTextInput(session,"perm_credit",value=g$credit)
    if(nzchar(g$date))
      updateTextInput(session,"perm_date",value=g$date)

    output$perm_analysis_log <- renderText(
      paste0(
        "ANÀLISI PROVISIONAL — CAL CONFIRMAR\n\n",
        "Resolució detectada: ",g$status,"\n",
        "Visualització suggerida: ",g$display,"\n",
        "Descàrrega suggerida: ",g$download,"\n",
        if(nzchar(g$credit)) paste0("Crèdit detectat: ",g$credit,"\n") else "",
        if(nzchar(g$date)) paste0("Data detectada: ",g$date,"\n") else "",
        "\nEl text complet continua sent l'evidència principal; aquesta anàlisi no substitueix la revisió humana."
      )
    )
  })

  # ----------------------------------------------------------------------
  # AUTORITZACIÓ: PERM independent -> 1 o N assets
  # ----------------------------------------------------------------------

  observeEvent(input$perm_apply,{
    req(admin_ok(),input$admin_item,input$perm_assets)

    msg <- tryCatch({
      asset_ids <- unique(as.integer(input$perm_assets))
      asset_ids <- asset_ids[!is.na(asset_ids)]
      if(!length(asset_ids)) stop("Selecciona almenys un document.")

      target_id_active <- trim_or_blank(input$admin_item)
      if(!nzchar(target_id_active)) stop("No hi ha cap fitxa activa.")

      con <- db_connect(); on.exit(DBI::dbDisconnect(con),add=TRUE)

      chk <- DBI::dbGetQuery(con,
        paste0("SELECT asset_id,item_id FROM item_assets WHERE asset_id IN (",
               paste(rep("?",length(asset_ids)),collapse=","),")"),
        params=as.list(asset_ids)
      )
      if(nrow(chk)!=length(asset_ids) || any(chk$item_id!=target_id_active))
        stop("Els documents seleccionats no pertanyen tots a la fitxa activa.")

      perm_id <- permission_next_id(con)
      dir.create(admin_private_path("permissions"),recursive=TRUE,showWarnings=FALSE)

      evidence_path <- ""
      evidence_name <- ""

      if(!is.null(input$perm_evidence) && nrow(input$perm_evidence)){
        evidence_name <- input$perm_evidence$name[1]
        ext <- tolower(tools::file_ext(evidence_name))
        dest <- file.path(
          "private_sources","permissions",
          paste0(perm_id,"__",gsub("[/\\\\]","_",evidence_name))
        )
        if(!file.copy(input$perm_evidence$datapath[1],dest,overwrite=FALSE))
          stop("No s'ha pogut conservar l'evidència de l'autorització.")
        evidence_path <- gsub("\\\\","/",dest)
      }

      status <- trim_or_blank(input$perm_status)
      display <- trim_or_blank(input$perm_display)
      download <- trim_or_blank(input$perm_download)

      if(status!="PERMISSION_GRANTED"){
        display <- "none"
        download <- "no"
      }

      conditions <- trim_or_blank(input$perm_conditions)
      pasted_text <- trim_or_blank(input$perm_text)
      if(nzchar(pasted_text)){
        conditions <- paste(
          c(conditions,paste0("TEXT AUTORITZACIÓ:\n",pasted_text))[nzchar(c(conditions,pasted_text))],
          collapse="\n\n"
        )
      }

      DBI::dbExecute(con,"
        INSERT INTO permissions
        (permission_id,rights_status,rights_holder,authorizing_person,
         authorizing_role,permission_date,scope,allowed_display,public_download,
         required_credit,conditions_text,evidence_private_source,evidence_file_name,
         notes,created_at,created_by)
        VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
      ",params=list(
        perm_id,status,
        trim_or_blank(input$perm_holder),
        trim_or_blank(input$perm_person),
        trim_or_blank(input$perm_role),
        trim_or_blank(input$perm_date),
        trim_or_blank(input$perm_scope),
        display,download,
        trim_or_blank(input$perm_credit),
        conditions,
        evidence_path,evidence_name,
        trim_or_blank(input$perm_notes),
        format(Sys.time(),"%Y-%m-%d %H:%M:%S"),
        Sys.getenv("XIPAHUATZIN_ADMIN_USER",unset="admin")
      ))

      applied <- character(0)

      for(aid in asset_ids){
        a <- DBI::dbGetQuery(con,"SELECT * FROM item_assets WHERE asset_id=?",
                             params=list(aid))
        if(!nrow(a)) stop("No existeix l'actiu ",aid)

        source_id <- trim_or_blank(a$source_item_id[1])
        target_id <- trim_or_blank(a$item_id[1])
        private_source <- trim_or_blank(a$private_source[1])

        pub <- ""
        if(status=="PERMISSION_GRANTED" && display!="none"){
          if(!nzchar(private_source) || !file.exists(private_source))
            stop("No es troba l'original privat de l'actiu ",aid)

          deriv <- make_linked_derivative(
            private_source,target_id,
            if(nzchar(source_id)) source_id else paste0("ASSET_",aid),
            "PERMISSION_GRANTED",display
          )
          pub <- trim_or_blank(deriv$public_asset)
        }

        perm_source <- paste0(
          perm_id,
          if(nzchar(input$perm_person)) paste0(" · ",trim_or_blank(input$perm_person)) else "",
          if(nzchar(input$perm_role)) paste0(", ",trim_or_blank(input$perm_role)) else "",
          if(nzchar(input$perm_date)) paste0(" · ",trim_or_blank(input$perm_date)) else "",
          if(nzchar(evidence_path)) paste0(" · ",evidence_path) else ""
        )

        cap <- trim_or_blank(a$caption[1])
        credit <- trim_or_blank(input$perm_credit)
        if(nzchar(credit) && !grepl(credit,cap,fixed=TRUE))
          cap <- paste(c(cap,credit)[nzchar(c(cap,credit))],collapse=" ")

        DBI::dbExecute(con,"
          UPDATE item_assets
          SET rights_status=?,copyright_holder=?,permission_source=?,
              public_display=?,public_download=?,public_asset=?,
              pixelated_preview='',caption=?
          WHERE asset_id=?
        ",params=list(
          status,
          trim_or_blank(input$perm_holder),
          perm_source,
          display,download,pub,cap,aid
        ))

        if(nzchar(source_id)){
          f <- DBI::dbListFields(con,"items")
          vals <- list(
            rights_status=status,
            public_display=display,
            public_download=download,
            permission_source=perm_source,
            copyright_holder=trim_or_blank(input$perm_holder)
          )
          vals <- vals[names(vals) %in% f]
          if(length(vals)){
            sql <- paste0(
              "UPDATE items SET ",
              paste0(DBI::dbQuoteIdentifier(con,names(vals)),"=?",collapse=","),
              " WHERE item_id=?"
            )
            DBI::dbExecute(con,sql,params=c(unname(vals),list(source_id)))
          }
        }

        DBI::dbExecute(con,"
          INSERT OR IGNORE INTO permission_assets
          (permission_id,asset_id,source_item_id,target_item_id,applied_at,applied_by)
          VALUES (?,?,?,?,?,?)
        ",params=list(
          perm_id,aid,source_id,target_id,
          format(Sys.time(),"%Y-%m-%d %H:%M:%S"),
          Sys.getenv("XIPAHUATZIN_ADMIN_USER",unset="admin")
        ))

        applied <- c(applied,paste0("asset ",aid," · ",source_id," → ",target_id))
      }

      refresh_items()
      permission_refresh(permission_refresh()+1L)
      if(exists("asset_refresh",inherits=FALSE))
        asset_refresh(asset_refresh()+1L)

      showNotification(
        paste0(perm_id,": autorització aplicada a ",length(applied)," document(s)."),
        type="message",duration=8
      )

      paste0(
        "PASS — AUTORITZACIÓ REGISTRADA\n\n",
        "ID: ",perm_id,"\n",
        "Resolució: ",status,"\n",
        "Visualització: ",display,"\n",
        "Descàrrega: ",download,"\n",
        "Documents: ",length(applied),"\n\n",
        paste0("• ",applied,collapse="\n")
      )

    },error=function(e){
      showNotification(conditionMessage(e),type="error",duration=10)
      paste0("ERROR\n\n",conditionMessage(e))
    })

    output$perm_apply_log <- renderText(msg)
  })

  # ======================================================================
  # FI PERMISSION WORKFLOW
  # ======================================================================


  observeEvent(input$admin_login,{
    expected <- Sys.getenv("XIPAHUATZIN_ADMIN_PASSWORD",unset="")
    if(!nzchar(expected)) showNotification("No hi ha cap contrasenya d’administració configurada.",type="error")
    else if(identical(input$admin_password,expected)) admin_ok(TRUE)
    else showNotification("Contrasenya incorrecta.",type="error")
  })

  # ---- Filtres de la Fitxa administrativa ----
  observe({
    req(admin_ok())
    x <- items_rv()
    yrs <- sort(unique(x$display_year_num[!is.na(x$display_year_num)]))
    labels <- as.character(yrs)
    if(any(x$display_year_label=="Sense data",na.rm=TRUE)) labels <- c(labels,"Sense data")
    cur <- isolate(input$admin_year)
    selected <- if(is.null(cur) || !nzchar(cur) || !cur %in% c("Tots",labels)) "Tots" else cur
    updateSelectInput(session,"admin_year",
      choices=c("Tots"="Tots",setNames(labels,labels)),selected=selected)
  })

  observe({
    req(admin_ok())
    x <- items_rv()
    people <- split_semicolon_values(x$related_people)
    cur <- isolate(input$admin_person)
    updateSelectInput(session,"admin_person",
      choices=c("Tots"="",adl_surname_choices(people)),
      selected=if(!is.null(cur) && cur %in% people) cur else "")
  })

  observe({
    req(admin_ok())
    x <- items_rv()
    authors <- split_semicolon_values(x$author_or_creator)
    cur <- isolate(input$admin_author)
    updateSelectInput(session,"admin_author",
      choices=c("Tots"="",adl_surname_choices(authors)),
      selected=if(!is.null(cur) && cur %in% authors) cur else "")
  })

  observe({
    req(admin_ok())
    x <- items_rv()
    publications <- split_semicolon_values(x$publication_title)
    cur <- isolate(input$admin_publication)
    updateSelectInput(session,"admin_publication",
      choices=c("Totes"="",setNames(publications,publications)),
      selected=if(!is.null(cur) && cur %in% publications) cur else "")
  })

  observe({
    req(admin_ok())
    x <- items_rv()
    places <- split_semicolon_values(x$related_places)
    cur <- isolate(input$admin_place)
    updateSelectInput(session,"admin_place",
      choices=c("Tots"="",setNames(places,places)),
      selected=if(!is.null(cur) && cur %in% places) cur else "")
  })

  admin_filtered <- reactive({
    req(admin_ok())
    x <- items_rv()
    # La pestanya Fitxa treballa només amb fitxes documentals reals.
    # Els registres draft/ITM són fonts internes i es gestionen des de
    # Preautorització, Vinculació, Actius i Inventari.
    x <- x[x$record_status %in% c("reviewed","publishable","published"),,drop=FALSE]

    scalar_input <- function(v, default=""){
      if(is.null(v) || length(v)!=1 || is.na(v) || !nzchar(as.character(v))) return(default)
      as.character(v)
    }

    ay <- scalar_input(input$admin_year,"Tots")
    ac <- scalar_input(input$admin_category,"")
    ap <- scalar_input(input$admin_person,"")
    aa <- scalar_input(input$admin_author,"")
    au <- scalar_input(input$admin_publication,"")
    al <- scalar_input(input$admin_place,"")
    aq <- scalar_input(input$admin_search,"")

    if(ay!="Tots"){
      if(ay=="Sense data")
        x <- x[x$display_year_label=="Sense data",,drop=FALSE]
      else
        x <- x[x$display_year==ay,,drop=FALSE]
    }

    if(nzchar(ac))
      x <- x[x$category==ac,,drop=FALSE]

    if(nzchar(ap)){
      keep <- vapply(x$related_people,function(z)
        any(tolower(split_semicolon_values(z))==tolower(ap)),logical(1))
      x <- x[keep,,drop=FALSE]
    }

    if(nzchar(aa)){
      keep <- vapply(x$author_or_creator,function(z)
        any(tolower(split_semicolon_values(z))==tolower(aa)),logical(1))
      x <- x[keep,,drop=FALSE]
    }

    if(nzchar(au)){
      keep <- vapply(x$publication_title,function(z)
        any(tolower(split_semicolon_values(z))==tolower(au)),logical(1))
      x <- x[keep,,drop=FALSE]
    }

    if(nzchar(al)){
      keep <- vapply(x$related_places,function(z)
        any(tolower(split_semicolon_values(z))==tolower(al)),logical(1))
      x <- x[keep,,drop=FALSE]
    }

    q <- tolower(trimws(aq))
    if(nzchar(q)){
      cols <- intersect(
        c("item_id","title","bibliographic_reference","publication_title","author_or_creator",
          "description","historical_context","legend_contribution",
          "project_interpretation","related_people","related_places"),
        names(x)
      )
      blob <- apply(x[,cols,drop=FALSE],1,function(r) paste(tolower(r),collapse=" "))
      x <- x[grepl(q,blob,fixed=TRUE),,drop=FALSE]
    }

    x[order(x$display_year_num,soldevila_sort_key(x$title),na.last=TRUE),,drop=FALSE]
  })

  observe({
    req(admin_ok())
    x <- admin_filtered()
    ch <- if(nrow(x))
      setNames(x$item_id,paste0(x$display_year_label," — ",x$title))
    else character(0)

    current <- isolate(input$admin_item)
    current_is_valid <- (
      !is.null(current) && length(current)==1 && !is.na(current) &&
      nzchar(current) && current %in% unname(ch)
    )

    selected <- if(current_is_valid) current
      else if(length(ch)) unname(ch[[1]])
      else character(0)

    updateSelectInput(session,"admin_item",choices=ch,selected=selected)
  })

  current_admin_item <- reactive({
    req(admin_ok(),input$admin_item)
    x <- items_rv()
    x[x$item_id==input$admin_item,,drop=FALSE]
  })

  output$admin_active_header <- renderUI({
    req(admin_ok(),input$admin_item)
    r <- current_admin_item()
    if(!nrow(r)) return(NULL)
    tags$div(
      class="alert alert-primary",
      style="margin-bottom:0.85rem;",
      tags$b("Fitxa activa: "),
      paste0(r$display_year_label[1]," — ",r$title[1]),
      tags$div(class="text-muted",paste0("ID: ",r$item_id[1]))
    )
  })

  output$admin_card <- renderUI({
    r <- current_admin_item()
    render_record_card(r,TRUE)
  })

  editing <- reactiveVal(FALSE)

  observeEvent(input$edit_item,{
    editing(TRUE)
    updateTabsetPanel(session,"admin_fitxa_tabs",selected="Editor")
  })

  # ADL_CARD_TO_ASSETS_NAV_20261014: manté l'ID de la fitxa activa.
  # L'apartat Actius ja permet seleccionar un fitxer del Mac o de library.
  observeEvent(input$go_add_document_from_card,{
    req(admin_ok(),input$admin_item)
    updateTabsetPanel(session,"admin_fitxa_tabs",selected="Documents")
    updateTabsetPanel(session,"admin_documents_tabs",selected="Actius")
  })

  output$editor_ui <- renderUI({
    req(admin_ok())
    r <- current_admin_item()
    if(!editing()){
      return(tags$div(class="alert alert-info","Selecciona una peça a la pestanya Fitxa i prem «Editar fitxa»."))
    }

    tagList(
      tags$div(class="alert alert-warning",
        tags$b("Edició administrativa. "),
        "Desar actualitza la base SQLite i registra una revisió."
      ),
      tags$div(class="editor-grid",
        tags$div(class="editor-wide",textInput("ed_bib","Referència bibliogràfica",value=r$bibliographic_reference)),
        textInput("ed_title","Títol",value=r$title),
        textInput("ed_author","Autor / productor",value=r$author_or_creator),
        textInput("ed_publication","Publicació / revista",value=trim_or_blank(r$publication_title)),
        textInput("ed_date","Data",value=r$document_date),
        textInput("ed_source","Procedència / arxiu",value=r$source_or_archive),
        selectInput("ed_category","Categoria",choices=setNames(categories$category,categories$category_label),selected=r$category),
        selectInput("ed_status","Estat",choices=c("draft","reviewed","publishable","published"),selected=r$record_status),
        textInput("ed_type","Tipus",value=r$item_type),
        tags$div(class="editor-wide",textAreaInput("ed_description","Descripció",value=r$description,rows=5,width="100%")),
        tags$div(class="editor-wide",textAreaInput("ed_context","Context històric",value=r$historical_context,rows=5,width="100%")),
        tags$div(class="editor-wide",textAreaInput("ed_contribution","Aportació a la llegenda",value=r$legend_contribution,rows=5,width="100%")),
        tags$div(class="editor-wide",textAreaInput("ed_value","Valor documental",value=r$documentary_value,rows=5,width="100%")),
        tags$div(class="editor-wide",textAreaInput("ed_limits","Limitacions",value=r$limitations,rows=5,width="100%")),
        tags$div(class="editor-wide",textAreaInput("ed_interpretation","Interpretació dins del projecte",value=r$project_interpretation,rows=6,width="100%")),
        tags$div(class="editor-wide",tags$hr(),tags$h4("Versió castellana"),tags$p(class="text-muted","Si un camp ES és buit, la vista castellana mostra el català.")),
        textInput("ed_title_es","Títol [ES]",value=trim_or_blank(r$title_es)),
        textInput("ed_type_es","Tipus [ES]",value=trim_or_blank(r$item_type_es)),
        tags$div(class="editor-wide",textAreaInput("ed_description_es","Descripció [ES]",value=trim_or_blank(r$description_es),rows=5,width="100%")),
        tags$div(class="editor-wide",textAreaInput("ed_context_es","Context històric [ES]",value=trim_or_blank(r$historical_context_es),rows=5,width="100%")),
        tags$div(class="editor-wide",textAreaInput("ed_contribution_es","Aportació a la llegenda [ES]",value=trim_or_blank(r$legend_contribution_es),rows=5,width="100%")),
        tags$div(class="editor-wide",textAreaInput("ed_value_es","Valor documental [ES]",value=trim_or_blank(r$documentary_value_es),rows=5,width="100%")),
        tags$div(class="editor-wide",textAreaInput("ed_limits_es","Limitacions [ES]",value=trim_or_blank(r$limitations_es),rows=5,width="100%")),
        tags$div(class="editor-wide",textAreaInput("ed_interpretation_es","Interpretació dins del projecte [ES]",value=trim_or_blank(r$project_interpretation_es),rows=6,width="100%")),
        textAreaInput("ed_claims_es","Afirmacions relacionades [ES]",value=trim_or_blank(r$related_claims_es),rows=3,width="100%"),
        textAreaInput("ed_events_es","Esdeveniments relacionats [ES]",value=trim_or_blank(r$related_events_es),rows=3,width="100%"),
        textAreaInput("ed_people","Persones relacionades",value=r$related_people,rows=3,width="100%"),
        textAreaInput("ed_places","Llocs relacionats",value=r$related_places,rows=3,width="100%"),
        textAreaInput("ed_claims","Afirmacions relacionades",value=r$related_claims,rows=3,width="100%"),
        textAreaInput("ed_events","Esdeveniments relacionats",value=r$related_events,rows=3,width="100%"),
        selectInput("ed_rights","Drets",choices=unique(rights_policy$rights_status),selected=r$rights_status),
        selectInput("ed_display","Visualització",choices=c("none","thumbnail","excerpt","full"),selected=r$public_display),
        tags$div(class="editor-wide",textInput("ed_revision_note","Nota de revisió",value=""))
      ),
      tags$div(class="editor-actions",
        actionButton("save_item","Desar canvis",class="btn-success"),
        tags$span(" "),
        actionButton("cancel_edit","Cancel·lar")
      )
    )
  })

  observeEvent(input$cancel_edit,{
    editing(FALSE)
    updateTabsetPanel(session,"admin_fitxa_tabs",selected="Resum")
  })

  observeEvent(input$save_item,{
    req(admin_ok(),editing())
    old <- current_admin_item()

    updated <- list(
      bibliographic_reference=input$ed_bib,
      title=input$ed_title,
      author_or_creator=input$ed_author,
      publication_title=input$ed_publication,
      document_date=input$ed_date,
      source_or_archive=input$ed_source,
      category=input$ed_category,
      record_status=input$ed_status,
      item_type=input$ed_type,
      description=input$ed_description,
      historical_context=input$ed_context,
      legend_contribution=input$ed_contribution,
      documentary_value=input$ed_value,
      limitations=input$ed_limits,
      project_interpretation=input$ed_interpretation,
      related_people=input$ed_people,
      related_places=input$ed_places,
      related_claims=input$ed_claims,
      related_events=input$ed_events,
      rights_status=input$ed_rights,
      public_display=input$ed_display,
      title_es=input$ed_title_es,
      item_type_es=input$ed_type_es,
      description_es=input$ed_description_es,
      historical_context_es=input$ed_context_es,
      legend_contribution_es=input$ed_contribution_es,
      documentary_value_es=input$ed_value_es,
      limitations_es=input$ed_limits_es,
      project_interpretation_es=input$ed_interpretation_es,
      related_claims_es=input$ed_claims_es,
      related_events_es=input$ed_events_es
    )

    # Identify changed fields.
    changed <- names(updated)[vapply(names(updated),function(nm){
      !identical(trim_or_blank(old[[nm]][1]),trim_or_blank(updated[[nm]]))
    },logical(1))]

    if(!length(changed)){
      showNotification("No hi ha canvis per desar.",type="message")
      return()
    }

    # Publication safety: published -> reviewed after substantive edit unless explicitly changed status.
    substantive <- setdiff(changed,c("record_status","rights_status","public_display"))
    new_status <- updated$record_status
    if(old$record_status[1]=="published" && length(substantive) && identical(input$ed_status,"published")){
      new_status <- "reviewed"
      updated$record_status <- new_status
      showNotification("La fitxa publicada ha tornat a «reviewed» perquè s'ha modificat el contingut.",type="warning")
    }

    con <- db_connect()
    on.exit(DBI::dbDisconnect(con),add=TRUE)
    DBI::dbBegin(con)
    ok <- FALSE
    tryCatch({
      set_clause <- paste0('"',names(updated),'"=?',collapse=",")
      sql <- paste0('UPDATE items SET ',set_clause,' WHERE item_id=?')
      DBI::dbExecute(con,sql,params=c(unname(updated),old$item_id[1]))

      DBI::dbExecute(con,
        "INSERT INTO revision_history
         (item_id,modified_at,modified_by,revision_note,previous_status,new_status,changed_fields)
         VALUES (?,?,?,?,?,?,?)",
        params=list(
          old$item_id[1],
          format(Sys.time(),"%Y-%m-%d %H:%M:%S %z"),
          Sys.getenv("XIPAHUATZIN_ADMIN_USER",unset="admin"),
          input$ed_revision_note,
          old$record_status[1],
          new_status,
          paste(changed,collapse=";")
        )
      )
      DBI::dbCommit(con)
      ok <- TRUE
    },error=function(e){
      DBI::dbRollback(con)
      showNotification(paste("Error en desar:",e$message),type="error",duration=NULL)
    })

    if(ok){
      refresh_items()
      editing(FALSE)
      updateTabsetPanel(session,"admin_fitxa_tabs",selected="Resum")
      showNotification("Canvis desats i revisió registrada.",type="message")
    }
  })


  output$admin_viewer_ui <- renderUI({
    req(admin_ok(),input$admin_item)
    r <- current_admin_item()

    radioButtons(
      "admin_view_mode",
      "Mode de visualització",
      choices=c(
        "Públic"="public",
        "Pixelat"="pixelated",
        "Original (administrador)"="original"
      ),
      selected="public",
      inline=TRUE
    )
  })

  output$admin_original_image <- renderImage({
    req(admin_ok(),input$admin_item,input$admin_view_mode=="original")
    r <- current_admin_item()
    src <- trim_or_blank(r$portable_source)
    req(nzchar(src),file.exists(src))

    ext <- tolower(tools::file_ext(src))
    outfile <- tempfile(fileext=".png")

    if(ext %in% c("jpg","jpeg","png","tif","tiff","webp","gif")){
      img <- magick::image_read(src)[1]
      magick::image_write(img,outfile,format="png")
    } else if(ext=="pdf"){
      bmp <- pdftools::pdf_render_page(src,page=1,dpi=140)
      img <- magick::image_read(bmp)
      magick::image_write(img,outfile,format="png")
    } else {
      return(NULL)
    }

    list(src=outfile,contentType="image/png",alt="Original administratiu")
  },deleteFile=TRUE)

  observe({
    req(admin_ok())
    if(!is.null(input$admin_view_mode)) invalidateLater(250,session)
  })


  output$admin_viewer_content <- renderUI({
    req(admin_ok(),input$admin_item,input$admin_view_mode)
    r <- current_admin_item()

    if(input$admin_view_mode=="public"){
      asset <- safe_asset_path(r)
      if(!is.na(asset)){
        ext <- tolower(tools::file_ext(asset))
        if(ext %in% c("jpg","jpeg","png","gif","webp"))
          return(tags$img(src=asset,loading="lazy",decoding="async",style="max-width:100%;max-height:700px;object-fit:contain;"))
        if(ext=="pdf")
          return(tags$iframe(src=asset,style="width:100%;height:750px;border:1px solid #ccc;"))
      }
      return(tags$div(class="alert alert-secondary","Aquesta peça no té cap actiu públic carregat."))
    }

    if(input$admin_view_mode=="pixelated"){
      px <- trim_or_blank(r$pixelated_preview)
      prefix <- "www/"
      px_rel <- if(nzchar(px) && startsWith(px,prefix)) substring(px,nchar(prefix)+1) else px
      if(nzchar(px_rel) && file.exists(file.path("www",px_rel))){
        return(tags$img(src=px_rel,loading="lazy",decoding="async",style="max-width:100%;max-height:700px;object-fit:contain;"))
      }
      return(tags$div(class="alert alert-secondary","No hi ha previsualització pixelada disponible."))
    }

    src <- trim_or_blank(r$portable_source)
    if(!nzchar(src) || !file.exists(src))
      return(tags$div(class="alert alert-warning","Original no hidratat dins del paquet portable."))

    ext <- tolower(tools::file_ext(src))
    if(ext %in% c("jpg","jpeg","png","tif","tiff","webp","gif","pdf")){
      return(tagList(
        tags$div(class="alert alert-warning",
          tags$b("Visualització administrativa."),
          " Aquest original és fora de www i no forma part de la vista pública."
        ),
        imageOutput("admin_original_image",height="auto")
      ))
    }

    tags$div(
      class="alert alert-info",
      "L'original existeix dins private_sources/, però aquest format no té previsualització inline. ",
      "Es pot conservar i exportar dins del paquet portable."
    )
  })



  asset_refresh <- reactiveVal(0)

  private_library_candidates <- reactive({
    req(admin_ok(),input$admin_item)

    x <- items_rv()
    if(!nrow(x)) return(list(suggested=character(0),other=character(0)))

    # create_linked_asset() treballa amb registres font draft.
    x <- x[x$record_status=="draft",,drop=FALSE]
    if(!nrow(x)) return(list(suggested=character(0),other=character(0)))

    # Només originals físicament hidratats dins de la biblioteca privada.
    hydrated <- vapply(seq_len(nrow(x)),function(i){
      p <- resolve_draft_source_path(x[i,,drop=FALSE])
      nzchar(p) && file.exists(p)
    },logical(1))
    x <- x[hydrated,,drop=FALSE]

    if(!nrow(x)) return(list(suggested=character(0),other=character(0)))

    # Exclou els registres ja vinculats a aquesta fitxa.
    con <- db_connect()
    on.exit(DBI::dbDisconnect(con),add=TRUE)
    linked <- DBI::dbGetQuery(
      con,
      "SELECT source_item_id FROM item_assets WHERE item_id=? AND COALESCE(source_item_id,'')<>''",
      params=list(input$admin_item)
    )$source_item_id
    linked <- unique(trim_or_blank(linked))
    x <- x[!x$item_id %in% linked,,drop=FALSE]

    if(!nrow(x)) return(list(suggested=character(0),other=character(0)))

    map_path <- file.path("data","linkage_map_v0_8_12.csv")
    suggested_ids <- character(0)
    map_info <- NULL

    if(file.exists(map_path)){
      map_info <- tryCatch(
        read.csv(map_path,stringsAsFactors=FALSE,check.names=FALSE,fileEncoding="UTF-8"),
        error=function(e) NULL
      )

      if(!is.null(map_info) && all(c("source_item_id","target_item_id","action") %in% names(map_info))){
        map_info$source_item_id <- trim_or_blank(map_info$source_item_id)
        map_info$target_item_id <- trim_or_blank(map_info$target_item_id)
        map_info$action <- trim_or_blank(map_info$action)
        suggested_ids <- unique(map_info$source_item_id[
          map_info$target_item_id==input$admin_item &
          map_info$action %in% c("LINK","REVIEW")
        ])
      }
    }

    make_labels <- function(d){
      if(!nrow(d)) return(character(0))
      ttl <- trim_or_blank(d$title)
      ttl[!nzchar(ttl)] <- trim_or_blank(d$file_name[!nzchar(ttl)])
      ttl[!nzchar(ttl)] <- d$item_id[!nzchar(ttl)]
      setNames(d$item_id,paste0(ttl,"  ·  ",d$item_id))
    }

    sug <- x[x$item_id %in% suggested_ids,,drop=FALSE]
    oth <- x[!x$item_id %in% suggested_ids,,drop=FALSE]

    # En els suggerits, indiquem LINK/REVIEW i confiança quan existeixen.
    suggested_choices <- character(0)
    if(nrow(sug)){
      labs <- character(nrow(sug))
      for(i in seq_len(nrow(sug))){
        sid <- sug$item_id[i]
        tag <- "SUGGERIT"
        if(!is.null(map_info)){
          mm <- map_info[
            map_info$source_item_id==sid & map_info$target_item_id==input$admin_item,
            ,drop=FALSE
          ]
          if(nrow(mm)){
            act <- trim_or_blank(mm$action[1])
            conf <- if("confidence" %in% names(mm)) trim_or_blank(mm$confidence[1]) else ""
            tag <- if(nzchar(conf)) paste0(act,"/",conf) else act
          }
        }
        ttl <- trim_or_blank(sug$title[i])
        if(!nzchar(ttl) && "file_name" %in% names(sug)) ttl <- trim_or_blank(sug$file_name[i])
        if(!nzchar(ttl)) ttl <- sid
        labs[i] <- paste0("[",tag,"] ",ttl,"  ·  ",sid)
      }
      suggested_choices <- setNames(sug$item_id,labs)
    }

    list(
      suggested=suggested_choices,
      other=make_labels(oth)
    )
  })

  output$private_library_selector_ui <- renderUI({
    req(admin_ok(),input$admin_item)
    cand <- private_library_candidates()

    groups <- list()
    if(length(cand$suggested)) groups[["Suggerits per aquesta fitxa"]] <- cand$suggested
    if(length(cand$other)) groups[["Resta de biblioteca privada"]] <- cand$other

    if(!length(groups)){
      return(tags$div(
        class="alert alert-light border",
        "No hi ha originals privats pendents de vinculació per aquesta fitxa."
      ))
    }

    selectizeInput(
      "private_library_source",
      "Document de la biblioteca",
      choices=groups,
      selected=NULL,
      width="100%",
      options=list(placeholder="Selecciona o escriu per cercar...",allowEmptyOption=TRUE)
    )
  })

  observeEvent(input$add_private_library_asset,{
    req(admin_ok(),input$admin_item)

    msg <- tryCatch({
      sid <- trim_or_blank(input$private_library_source)
      if(!nzchar(sid)) stop("Selecciona primer un document de la biblioteca privada.")

      res <- create_linked_asset(
        source_item_id=sid,
        target_item_id=input$admin_item,
        notes="Vinculació manual des d’Administració → Actius."
      )

      asset_refresh(asset_refresh()+1L)

      showNotification(
        paste0("Document vinculat correctament: ",sid),
        type="message",duration=6
      )

      paste0(
        res$status," — ",res$message,"\n",
        "Registre font: ",sid,"\n",
        "Fitxa objectiu: ",input$admin_item
      )
    },error=function(e){
      showNotification(conditionMessage(e),type="error",duration=8)
      paste0("ERROR\n\n",conditionMessage(e))
    })

    output$private_library_link_log <- renderText(msg)
  })

  output$admin_assets_ui <- renderUI({
    req(admin_ok(),input$admin_item)
    asset_refresh() # ADL_REFRESH_ACTIUS_20261014: rebuild lists after add/unlink

    assets <- read_item_assets(input$admin_item)
    choices <- if(nrow(assets)){
      setNames(
        assets$asset_id,
        paste0(
          assets$sort_order," — ",
          ifelse(nzchar(assets$title),assets$title,paste0("[",assets$asset_type,"]")),
          "  ·  ",basename(assets$private_source),"  ·  ID ",assets$asset_id
        )
      )
    } else character(0)

    tagList(
      tags$h4("Imatges i documents associats"),
      # ADL_SAFE_UNLINK_ASSET_20261014 — correct accidental linkage, retain original
      tags$div(
        class="asset-admin-panel",
        tags$h5("Desvincular un document d’aquesta fitxa"),
        tags$p("Corregeix un document associat a la fitxa equivocada. Conserva el PDF original a la biblioteca privada."),
        selectInput("adl_unlink_asset_id", "Document que vols desvincular", choices=choices, width="100%"),
        actionButton("adl_unlink_asset", "Desvincular de la fitxa", class="btn-outline-danger")
      ),

      tags$p("Pots carregar un sol fitxer o diversos fitxers simultàniament. Després els pots editar, reordenar o eliminar sense modificar la fitxa principal."),

      tags$div(
        class="asset-admin-panel",
        tags$h5("Afegir des de biblioteca privada"),
        tags$p(class="text-muted",
          "Mostra primer els documents suggerits per a aquesta fitxa i, després, la resta d’originals privats disponibles que encara no hi estan vinculats."),
        uiOutput("private_library_selector_ui"),
        actionButton("add_private_library_asset","Afegir actiu seleccionat",class="btn-primary"),
        tags$div(class="asset-upload-log",verbatimTextOutput("private_library_link_log"))
      ),

      tags$hr(),

      tags$div(
        class="asset-admin-grid",

        tags$div(
          class="asset-admin-panel",
          tags$h5("Afegir actius"),
          fileInput("asset_file","Fitxer(s)",accept=c("image/*",".pdf"),multiple=TRUE),
          textInput("asset_canonical_name","Nom canònic del nou document",placeholder="AUTOR_ANY_MES_DIA_PUBLICACIO_PAGINES.pdf"),
          textInput("asset_title","Títol curt",""),
          textAreaInput("asset_caption","Peu / descripció","",rows=3),
          textInput("asset_author","Autor / creador",""),
          textInput("asset_date","Data",""),
          textInput("asset_copyright","Titular dels drets",""),
          textInput("asset_permission","Font / evidència del permís",""),
          selectInput("asset_rights","Drets",
            choices=c(
              "PUBLIC_DOMAIN","OPEN_LICENSE","PERMISSION_GRANTED",
              "CITATION_ONLY","PRIVATE_RESEARCH_COPY","RIGHTS_UNKNOWN"
            ),
            selected="RIGHTS_UNKNOWN"
          ),
          selectInput("asset_display","Visualització pública",
            choices=c("Cap"="none","Miniatura"="thumbnail","Fragment"="excerpt","Complet"="full"),
            selected="none"
          ),
          selectInput("asset_download","Descàrrega pública",choices=c("No"="no","Sí"="yes"),selected="no"),
          numericInput("asset_sort","Ordre inicial",value=1,min=1,step=1),
          tags$p(class="text-muted","En una càrrega múltiple, aquest valor és l’ordre del primer fitxer; els següents reben ordres consecutius."),
          actionButton("add_asset","Afegir a la fitxa",class="btn-primary"),
          tags$div(class="asset-upload-log",verbatimTextOutput("asset_upload_log"))
        ),

        tags$div(
          class="asset-admin-panel",
          tags$h5("Gestionar un actiu existent"),
          selectInput("asset_selected","Actiu",choices=choices),
          uiOutput("asset_editor_ui")
        )
      ),

      tags$hr(),
      tags$h5("Actius de la fitxa"),
      DTOutput("admin_assets_table")
    )
  })

  # ADL_SAFE_UNLINK_ASSET_20261014 — independent safe unlink, no deletion of originals
  adl_unlink_pending <- reactiveVal(NULL)
  observeEvent(input$adl_unlink_asset, {
    req(admin_ok(), input$admin_item, input$adl_unlink_asset_id)
    assets_now <- read_item_assets(input$admin_item)
    a <- assets_now[as.character(assets_now$asset_id) == as.character(input$adl_unlink_asset_id), , drop=FALSE]
    if(nrow(a)!=1L) {
      showNotification("No s’ha trobat el document en aquesta fitxa.", type="error")
      return()
    }
    pending <- list(item_id=as.character(input$admin_item), asset_id=as.integer(a$asset_id[1]),
                    title=trim_or_blank(a$title[1]), public_asset=trim_or_blank(a$public_asset[1]))
    adl_unlink_pending(pending)
    showModal(modalDialog(
      title="Confirmar la desvinculació",
      tags$p(tags$b("Fitxa:"), pending$item_id),
      tags$p(tags$b("Document:"), if(nzchar(pending$title)) pending$title else paste0("Actiu ",pending$asset_id)),
      tags$p("Se suprimirà només l’associació i, si escau, la còpia pública no utilitzada. El PDF original privat es conservarà."),
      footer=tagList(modalButton("Cancel·lar"),
                     actionButton("adl_confirm_unlink", "Sí, desvincular", class="btn-danger")),
      easyClose=FALSE
    ))
  })
  observeEvent(input$adl_confirm_unlink, {
    req(admin_ok())
    pending <- adl_unlink_pending()
    if(is.null(pending)) return()
    adl_unlink_pending(NULL)
    removeModal()
    tryCatch({
      con <- db_connect()
      on.exit(DBI::dbDisconnect(con), add=TRUE)
      DBI::dbBegin(con)
      committed <- FALSE
      on.exit(if(!committed) try(DBI::dbRollback(con),silent=TRUE), add=TRUE)
      before <- DBI::dbGetQuery(con,
        "SELECT asset_id, public_asset FROM item_assets WHERE asset_id=? AND item_id=?",
        params=list(pending$asset_id,pending$item_id))
      if(nrow(before)!=1L) stop("El document ja no està vinculat a aquesta fitxa.")
      db_removed <- DBI::dbExecute(con,
        "DELETE FROM item_assets WHERE asset_id=? AND item_id=?",
        params=list(pending$asset_id,pending$item_id))
      if(db_removed!=1L) stop("No s’ha pogut desvincular el document.")
      pub <- trim_or_blank(before$public_asset[1])
      still_used <- if(nzchar(pub)) DBI::dbGetQuery(con,
        "SELECT COUNT(*) AS n FROM item_assets WHERE public_asset=?",params=list(pub))$n[1] else 0
      DBI::dbCommit(con)
      committed <- TRUE
      if(nzchar(pub) && still_used == 0L && file.exists(pub)) {
        root <- normalizePath("www",mustWork=FALSE)
        target <- normalizePath(pub,mustWork=FALSE)
        if(startsWith(target,paste0(root,.Platform$file.sep))) try(unlink(pub),silent=TRUE)
      }
      asset_refresh(asset_refresh()+1L)
      showNotification("Document desvinculat. Original privat conservat.",type="message",duration=8)
    },error=function(e) showNotification(conditionMessage(e),type="error",duration=10))
  })

  output$asset_editor_ui <- renderUI({
    req(admin_ok(),input$admin_item)

    assets <- read_item_assets(input$admin_item)
    if(!nrow(assets)){
      return(tags$div(class="alert alert-light border","Encara no hi ha actius múltiples associats a aquesta fitxa."))
    }

    sel <- input$asset_selected
    if(is.null(sel) || !nzchar(as.character(sel)) || !(as.character(sel) %in% as.character(assets$asset_id))){
      sel <- assets$asset_id[1]
    }

    a <- assets[as.character(assets$asset_id)==as.character(sel),,drop=FALSE]
    if(!nrow(a)) return(NULL)

    tagList(
      textInput("asset_edit_title","Títol",value=trim_or_blank(a$title[1])),
      textAreaInput("asset_edit_caption","Peu / descripció",value=trim_or_blank(a$caption[1]),rows=3),
      textInput("asset_edit_author","Autor / creador",value=trim_or_blank(a$author_or_creator[1])),
      textInput("asset_edit_date","Data",value=trim_or_blank(a$document_date[1])),
      textInput("asset_edit_copyright","Titular dels drets",value=trim_or_blank(a$copyright_holder[1])),
      textInput("asset_edit_permission","Font / evidència del permís",value=trim_or_blank(a$permission_source[1])),
      selectInput(
        "asset_edit_rights","Drets",
        choices=c(
          "PUBLIC_DOMAIN","OPEN_LICENSE","PERMISSION_GRANTED",
          "CITATION_ONLY","PRIVATE_RESEARCH_COPY","RIGHTS_UNKNOWN"
        ),
        selected=trim_or_blank(a$rights_status[1])
      ),
      selectInput(
        "asset_edit_display","Visualització pública",
        choices=c("Cap"="none","Miniatura"="thumbnail","Fragment"="excerpt","Complet"="full"),
        selected=trim_or_blank(a$public_display[1])
      ),
      selectInput(
        "asset_edit_download","Descàrrega pública",
        choices=c("No"="no","Sí"="yes"),
        selected=ifelse(trim_or_blank(a$public_download[1])=="yes","yes","no")
      ),
      numericInput("asset_edit_sort","Ordre",value=as.integer(a$sort_order[1]),min=1,step=1),
      textAreaInput("asset_edit_notes","Notes internes",value=trim_or_blank(a$notes[1]),rows=3),

      tags$div(
        class="asset-actions-row",
        actionButton("save_asset_changes","Desar canvis",class="btn-success"),
        actionButton("asset_move_up","Pujar",class="btn-outline-secondary"),
        actionButton("asset_move_down","Baixar",class="btn-outline-secondary"),
        actionButton("delete_asset","Eliminar",class="btn-outline-danger")
      ),
      tags$div(class="asset-edit-log",verbatimTextOutput("asset_edit_log"))
    )
  })


  observeEvent(input$add_asset,{
    req(admin_ok(),input$admin_item,input$asset_file)

    txt <- tryCatch({
      con <- db_connect()
      on.exit(DBI::dbDisconnect(con),add=TRUE)

      files <- input$asset_file
      if(is.null(files) || nrow(files)<1)
        stop("No s'ha seleccionat cap fitxer.")

      # The value entered by the administrator is the order of the first file.
      # Subsequent files receive consecutive values.
      start_order <- suppressWarnings(as.integer(input$asset_sort))
      if(is.na(start_order) || start_order < 1L) start_order <- 1L

      dir.create(file.path("library","public"),recursive=TRUE,showWarnings=FALSE) # ADL_CANONIC_FASE1_20261015
      dir.create(file.path("library","restricted"),recursive=TRUE,showWarnings=FALSE)
      dir.create(file.path("library","working"),recursive=TRUE,showWarnings=FALSE)
      dir.create("private_sources",recursive=TRUE,showWarnings=FALSE)
      dir.create(file.path("www","public"),recursive=TRUE,showWarnings=FALSE)
      dir.create(file.path("www","thumbnails"),recursive=TRUE,showWarnings=FALSE)
      dir.create(file.path("www","excerpts"),recursive=TRUE,showWarnings=FALSE)
      dir.create(file.path("www","restricted_previews"),recursive=TRUE,showWarnings=FALSE)

      display <- if(input$asset_rights %in% PUBLIC_RIGHTS) input$asset_display else "none"
      now <- format(Sys.time(),"%Y-%m-%d %H:%M:%S")
      user <- Sys.getenv("XIPAHUATZIN_ADMIN_USER",unset="admin")
      stamp <- format(Sys.time(),"%Y%m%d_%H%M%S")

      added <- character(0)

      for(i in seq_len(nrow(files))){
        src <- files$datapath[i]
        original_name <- files$name[i]
        ext <- tolower(tools::file_ext(original_name))

        if(!nzchar(ext))
          stop("El fitxer no té extensió reconeguda: ",original_name)

        stub <- paste0(
          gsub("[^A-Za-z0-9._-]+","_",input$admin_item),
          "_",stamp,"_",sprintf("%02d",i)
        )

        canonical <- trim_or_blank(input$asset_canonical_name)
        if(nrow(files)>1L || !nzchar(canonical)) canonical <- original_name
        canonical <- basename(canonical)
        if(!grepl("^[A-Za-z0-9][A-Za-z0-9._-]*\\.[A-Za-z0-9]+$",canonical))
          stop("Nom canònic invàlid: ",canonical)
        if(tolower(tools::file_ext(canonical)) != ext)
          stop("L'extensió canònica no coincideix amb l'original: ",original_name)
        section <- if(input$asset_rights %in% PUBLIC_RIGHTS && display!="none") "public" else "restricted"
        private_dest <- file.path("library",section,canonical)
        if(file.exists(private_dest)){
          if(!identical(unname(tools::md5sum(src)),unname(tools::md5sum(private_dest))))
            stop("Conflicte de nom: document diferent a library/",section,"/",canonical)
        } else if(!file.copy(src,private_dest,overwrite=FALSE)) {
          stop("No s'ha pogut copiar el fitxer a library: ",original_name)
        }

        public_dest <- ""
        pixel_dest <- ""

        if(display=="full"){
          public_dest <- file.path("www","public",paste0(stub,".",ext))
          if(!file.copy(src,public_dest,overwrite=TRUE))
            stop("No s'ha pogut crear la còpia pública: ",original_name)

        } else if(display=="thumbnail"){
          public_dest <- file.path("www","thumbnails",paste0(stub,"_thumb.jpg"))
          img <- if(ext=="pdf"){
            bmp <- pdftools::pdf_render_page(src,page=1,dpi=110)
            magick::image_read(bmp)
          } else magick::image_read(src)[1]
          img <- magick::image_resize(img,"600x600>")
          magick::image_write(img,public_dest,format="jpg",quality=60)

        } else if(display=="excerpt"){
          public_dest <- file.path("www","excerpts",paste0(stub,"_excerpt.jpg"))
          img <- if(ext=="pdf"){
            bmp <- pdftools::pdf_render_page(src,page=1,dpi=130)
            magick::image_read(bmp)
          } else magick::image_read(src)[1]
          img <- magick::image_resize(img,"1000x1000>")
          magick::image_write(img,public_dest,format="jpg",quality=72)

        } else {
          # No public derivative is created for restricted or non-public assets.
          # The original remains only in private_sources/.
          pixel_dest <- ""
        }

        # Shared metadata may be supplied once. For a multi-file upload,
        # filenames make each slide uniquely identifiable.
        ttl <- trim_or_blank(input$asset_title)
        cap <- trim_or_blank(input$asset_caption)
        base_name <- tools::file_path_sans_ext(canonical)

        if(nrow(files)>1){
          ttl <- if(nzchar(ttl)) paste0(ttl," — ",base_name) else base_name
          cap <- if(nzchar(cap)) paste0(cap," [",original_name,"]") else original_name
        } else {
          if(!nzchar(ttl)) ttl <- base_name
        }

        sort_value <- start_order + i - 1L

        DBI::dbExecute(
          con,
          "INSERT INTO item_assets
           (item_id,asset_type,title,caption,author_or_creator,document_date,
            rights_status,copyright_holder,permission_source,public_display,public_download,private_source,
            public_asset,pixelated_preview,sort_order,created_at,created_by)
           VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)",
          params=list(
            input$admin_item,
            if(ext=="pdf") "document" else "image",
            ttl,
            cap,
            trim_or_blank(input$asset_author),
            trim_or_blank(input$asset_date),
            input$asset_rights,
            trim_or_blank(input$asset_copyright),
            trim_or_blank(input$asset_permission),
            display,
            if(input$asset_rights %in% PUBLIC_RIGHTS) trim_or_blank(input$asset_download) else "no",
            gsub("\\\\","/",private_dest),
            gsub("\\\\","/",public_dest),
            gsub("\\\\","/",pixel_dest),
            sort_value,
            now,user
          )
        )

        added <- c(added,paste0(canonical,"  → ordre ",sort_value," (origen: ",original_name,")"))
      }

      asset_refresh(asset_refresh()+1L)

      showNotification(
        paste0(length(added)," actiu(s) incorporat(s) correctament."),
        type="message",duration=6
      )

      paste0(
        "PASS — ",length(added)," actiu(s) incorporat(s)\n\n",
        paste0("• ",added,collapse="\n")
      )

    },error=function(e){
      showNotification(conditionMessage(e),type="error",duration=8)
      paste0("ERROR\n\n",conditionMessage(e))
    })

    output$asset_upload_log <- renderText(txt)
  })


  current_asset_row <- reactive({
    req(admin_ok(),input$admin_item,input$asset_selected)
    x <- read_item_assets(input$admin_item)
    x[as.character(x$asset_id)==as.character(input$asset_selected),,drop=FALSE]
  })

  observeEvent(input$save_asset_changes,{
    req(admin_ok(),input$admin_item,input$asset_selected)

    msg <- tryCatch({
      con <- db_connect()
      on.exit(DBI::dbDisconnect(con),add=TRUE)

      a <- current_asset_row()
      if(!nrow(a)) stop("No s'ha trobat l'actiu seleccionat.")

      new_display <- if(input$asset_edit_rights %in% PUBLIC_RIGHTS) input$asset_edit_display else "none"
      old_display <- trim_or_blank(a$public_display[1])
      private_source <- trim_or_blank(a$private_source[1])
      public_asset <- trim_or_blank(a$public_asset[1])
      pixelated_preview <- trim_or_blank(a$pixelated_preview[1])

      # If display mode changes, regenerate/copy the derivative from private source.
      if(!identical(new_display,old_display)){
        if(!nzchar(private_source) || !file.exists(private_source))
          stop("No es troba l'original privat de l'actiu; no es pot canviar la visualització.")

        ext <- tolower(tools::file_ext(private_source))
        stub <- paste0(
          gsub("[^A-Za-z0-9._-]+","_",input$admin_item),
          "_asset_",input$asset_selected
        )

        # Remove old public derivative if it belongs to managed www folders.
        for(p in c(public_asset,pixelated_preview)){
          if(nzchar(p) && file.exists(p) && startsWith(normalizePath(p,mustWork=FALSE),normalizePath("www",mustWork=FALSE))){
            try(unlink(p),silent=TRUE)
          }
        }
        public_asset <- ""
        pixelated_preview <- ""

        if(new_display=="full"){
          dir.create(file.path("www","public"),recursive=TRUE,showWarnings=FALSE)
          public_asset <- file.path("www","public",paste0(stub,".",ext))
          file.copy(private_source,public_asset,overwrite=TRUE)

        } else if(new_display=="thumbnail"){
          dir.create(file.path("www","thumbnails"),recursive=TRUE,showWarnings=FALSE)
          public_asset <- file.path("www","thumbnails",paste0(stub,"_thumb.jpg"))
          img <- if(ext=="pdf"){
            bmp <- pdftools::pdf_render_page(private_source,page=1,dpi=110)
            magick::image_read(bmp)
          } else magick::image_read(private_source)[1]
          img <- magick::image_resize(img,"600x600>")
          magick::image_write(img,public_asset,format="jpg",quality=60)

        } else if(new_display=="excerpt"){
          dir.create(file.path("www","excerpts"),recursive=TRUE,showWarnings=FALSE)
          public_asset <- file.path("www","excerpts",paste0(stub,"_excerpt.jpg"))
          img <- if(ext=="pdf"){
            bmp <- pdftools::pdf_render_page(private_source,page=1,dpi=130)
            magick::image_read(bmp)
          } else magick::image_read(private_source)[1]
          img <- magick::image_resize(img,"1000x1000>")
          magick::image_write(img,public_asset,format="jpg",quality=72)

        } else {
          # Restricted assets keep no derivative under www/.
          pixelated_preview <- ""
        }
      }

      DBI::dbExecute(
        con,
        "UPDATE item_assets
         SET title=?, caption=?, author_or_creator=?, document_date=?,
             rights_status=?, copyright_holder=?, permission_source=?,
             public_display=?, public_download=?, sort_order=?, notes=?,
             public_asset=?, pixelated_preview=?
         WHERE asset_id=? AND item_id=?",
        params=list(
          trim_or_blank(input$asset_edit_title),
          trim_or_blank(input$asset_edit_caption),
          trim_or_blank(input$asset_edit_author),
          trim_or_blank(input$asset_edit_date),
          input$asset_edit_rights,
          trim_or_blank(input$asset_edit_copyright),
          trim_or_blank(input$asset_edit_permission),
          new_display,
          if(input$asset_edit_rights %in% PUBLIC_RIGHTS) trim_or_blank(input$asset_edit_download) else "no",
          as.integer(input$asset_edit_sort),
          trim_or_blank(input$asset_edit_notes),
          gsub("\\\\","/",public_asset),
          gsub("\\\\","/",pixelated_preview),
          as.integer(input$asset_selected),
          input$admin_item
        )
      )

      asset_refresh(asset_refresh()+1L)
      showNotification("Canvis de l'actiu desats.",type="message")
      "PASS — canvis desats correctament."
    },error=function(e){
      showNotification(conditionMessage(e),type="error",duration=8)
      paste0("ERROR\n\n",conditionMessage(e))
    })

    output$asset_edit_log <- renderText(msg)
  })

  swap_asset_order <- function(direction){
    con <- db_connect()
    on.exit(DBI::dbDisconnect(con),add=TRUE)

    x <- DBI::dbGetQuery(
      con,
      "SELECT asset_id, sort_order FROM item_assets WHERE item_id=? ORDER BY sort_order, asset_id",
      params=list(input$admin_item)
    )
    if(!nrow(x)) return(FALSE)

    idx <- which(as.character(x$asset_id)==as.character(input$asset_selected))
    if(length(idx)!=1) return(FALSE)

    target <- if(direction=="up") idx-1L else idx+1L
    if(target < 1L || target > nrow(x)) return(FALSE)

    id1 <- x$asset_id[idx]
    id2 <- x$asset_id[target]
    o1 <- x$sort_order[idx]
    o2 <- x$sort_order[target]

    DBI::dbExecute(con,"UPDATE item_assets SET sort_order=? WHERE asset_id=?",params=list(-999999L,id1))
    DBI::dbExecute(con,"UPDATE item_assets SET sort_order=? WHERE asset_id=?",params=list(o1,id2))
    DBI::dbExecute(con,"UPDATE item_assets SET sort_order=? WHERE asset_id=?",params=list(o2,id1))

    asset_refresh(asset_refresh()+1L)
    TRUE
  }

  observeEvent(input$asset_move_up,{
    req(admin_ok(),input$admin_item,input$asset_selected)
    ok <- tryCatch(swap_asset_order("up"),error=function(e){
      showNotification(conditionMessage(e),type="error")
      FALSE
    })
    if(ok) showNotification("Actiu mogut una posició amunt.",type="message")
  })

  observeEvent(input$asset_move_down,{
    req(admin_ok(),input$admin_item,input$asset_selected)
    ok <- tryCatch(swap_asset_order("down"),error=function(e){
      showNotification(conditionMessage(e),type="error")
      FALSE
    })
    if(ok) showNotification("Actiu mogut una posició avall.",type="message")
  })

  observeEvent(input$delete_asset,{
    req(admin_ok(),input$admin_item,input$asset_selected)
    a <- current_asset_row()
    if(!nrow(a)) return()

    showModal(modalDialog(
      title="Eliminar actiu?",
      tags$p("Aquesta acció elimina l'actiu de la base i els fitxers associats gestionats per la wiki."),
      tags$p(tags$b(ifelse(nzchar(trim_or_blank(a$title[1])),a$title[1],paste0("ID ",a$asset_id[1])))),
      footer=tagList(
        modalButton("Cancel·lar"),
        actionButton("confirm_delete_asset","Eliminar definitivament",class="btn-danger")
      ),
      easyClose=FALSE
    ))
  })

  observeEvent(input$confirm_delete_asset,{
    req(admin_ok(),input$admin_item,input$asset_selected)

    msg <- tryCatch({
      con <- db_connect()
      on.exit(DBI::dbDisconnect(con),add=TRUE)

      a <- current_asset_row()
      if(!nrow(a)) stop("No s'ha trobat l'actiu seleccionat.")

      # Delete only files managed inside this wiki tree.
      for(p in c(a$private_source[1],a$public_asset[1],a$pixelated_preview[1])){
        p <- trim_or_blank(p)
        if(!nzchar(p) || !file.exists(p)) next

        np <- normalizePath(p,mustWork=FALSE)
        allowed <- c(
          normalizePath("private_sources",mustWork=FALSE),
          normalizePath("www",mustWork=FALSE)
        )
        if(any(startsWith(np,allowed))){
          try(unlink(p),silent=TRUE)
        }
      }

      DBI::dbExecute(
        con,
        "DELETE FROM item_assets WHERE asset_id=? AND item_id=?",
        params=list(as.integer(input$asset_selected),input$admin_item)
      )

      removeModal()
      asset_refresh(asset_refresh()+1L)
      showNotification("Actiu eliminat.",type="warning")
      "PASS — actiu eliminat."
    },error=function(e){
      removeModal()
      showNotification(conditionMessage(e),type="error",duration=8)
      paste0("ERROR\n\n",conditionMessage(e))
    })

    output$asset_edit_log <- renderText(msg)
  })

  observe({
    req(admin_ok(),input$admin_item)
    asset_refresh()

    assets <- read_item_assets(input$admin_item)
    choices <- if(nrow(assets)){
      setNames(
        assets$asset_id,
        paste0(
          assets$sort_order," — ",
          ifelse(nzchar(assets$title),assets$title,paste0("[",assets$asset_type,"]")),
          "  ·  ",basename(assets$private_source),"  ·  ID ",assets$asset_id
        )
      )
    } else character(0)

    current <- isolate(input$asset_selected)
    selected <- if(length(choices) && !is.null(current) && as.character(current) %in% as.character(unname(choices))){
      current
    } else if(length(choices)) unname(choices[[1]]) else character(0)

    updateSelectInput(session,"asset_selected",choices=choices,selected=selected)
  })


  output$admin_assets_table <- renderDT({
    req(admin_ok(),input$admin_item)
    asset_refresh()
    x <- read_item_assets(input$admin_item)
    if(!nrow(x)) return(datatable(data.frame(Missatge="Encara no hi ha actius múltiples associats."),rownames=FALSE))
    cols <- c("asset_id","sort_order","asset_type","title","caption","author_or_creator","document_date","rights_status","public_display")
    if("source_item_id" %in% names(x)) cols <- c(cols,"source_item_id")
    out <- x[,cols,drop=FALSE]
    labs <- c("ID","Ordre","Tipus","Títol","Peu","Autor","Data","Drets","Visualització")
    if("source_item_id" %in% cols) labs <- c(labs,"Registre font")
    datatable(
      out,
      rownames=FALSE,
      selection="single",
      options=list(pageLength=10,scrollX=TRUE,order=list(list(1,"asc"))),
      colnames=labs
    )
  })

  observeEvent(input$admin_assets_table_rows_selected,{
    req(admin_ok(),input$admin_item)
    idx <- input$admin_assets_table_rows_selected
    if(length(idx)!=1) return()
    x <- read_item_assets(input$admin_item)
    if(idx <= nrow(x)){
      updateSelectInput(session,"asset_selected",selected=x$asset_id[idx])
    }
  })


  resolve_draft_source_path <- function(r){
    # v0.8.13.2: the wiki is autonomous. Operational linkage never follows
    # dropbox_path or any external environment variable.
    p <- trim_or_blank(r$portable_source)
    if(nzchar(p) && file.exists(p)) return(p)

    id <- trim_or_blank(r$item_id)
    fn <- trim_or_blank(r$file_name)
    if(nzchar(id) && nzchar(fn)){
      cand <- file.path("private_sources","library",paste0(id,"__",fn))
      if(file.exists(cand)) return(cand)
    }
    ""
  }

  make_linked_derivative <- function(private_source,item_id,source_item_id,rights_status,public_display){
    if(!public_rights_allowed(rights_status,public_display))
      return(list(public_asset="",pixelated_preview=""))

    ext <- tolower(tools::file_ext(private_source))
    stub <- paste0(
      gsub("[^A-Za-z0-9._-]+","_",item_id),
      "_linked_",
      gsub("[^A-Za-z0-9._-]+","_",source_item_id)
    )
    public_asset <- ""

    if(public_display=="full"){
      dir.create(file.path("www","public"),recursive=TRUE,showWarnings=FALSE)
      public_asset <- file.path("www","public",paste0(stub,".",ext))
      file.copy(private_source,public_asset,overwrite=TRUE)
    } else if(public_display %in% c("thumbnail","excerpt")){
      outdir <- if(public_display=="thumbnail") "thumbnails" else "excerpts"
      dir.create(file.path("www",outdir),recursive=TRUE,showWarnings=FALSE)
      public_asset <- file.path("www",outdir,paste0(stub,".jpg"))
      if(ext=="pdf"){
        dpi <- if(public_display=="thumbnail") 110 else 130
        bmp <- pdftools::pdf_render_page(private_source,page=1,dpi=dpi)
        img <- magick::image_read(bmp)
      } else if(ext %in% c("jpg","jpeg","png","tif","tiff","webp","gif")){
        img <- magick::image_read(private_source)[1]
      } else {
        return(list(public_asset="",pixelated_preview=""))
      }
      target <- if(public_display=="thumbnail") "600x600>" else "1000x1000>"
      img <- magick::image_resize(img,target)
      magick::image_write(img,public_asset,format="jpg",quality=if(public_display=="thumbnail") 62 else 75)
    }

    list(public_asset=gsub("\\\\","/",public_asset),pixelated_preview="")
  }

  create_linked_asset <- function(source_item_id,target_item_id,notes=""){
    con <- db_connect()
    on.exit(DBI::dbDisconnect(con),add=TRUE)

    src <- DBI::dbGetQuery(con,"SELECT * FROM items WHERE item_id=?",params=list(source_item_id))
    tgt <- DBI::dbGetQuery(con,"SELECT * FROM items WHERE item_id=?",params=list(target_item_id))
    if(!nrow(src)) stop("No existeix el registre font: ",source_item_id)
    if(!nrow(tgt)) stop("No existeix la fitxa objectiu: ",target_item_id)
    if(src$record_status[1]!="draft") stop("El registre font no és draft.")
    if(!tgt$record_status[1] %in% c("reviewed","publishable","published"))
      stop("La fitxa objectiu no és reviewed/publishable/published.")

    prev <- DBI::dbGetQuery(
      con,
      "SELECT asset_id FROM item_assets WHERE item_id=? AND source_item_id=?",
      params=list(target_item_id,source_item_id)
    )
    if(nrow(prev)) return(list(status="SKIP",asset_id=prev$asset_id[1],message="La vinculació ja existeix."))

    src_path <- resolve_draft_source_path(src[1,,drop=FALSE])
    if(!nzchar(src_path)) stop(
      "No s'ha localitzat el fitxer font dins private_sources/library/. La wiki no utilitza rutes externes."
    )

    ext <- tolower(tools::file_ext(src_path))
    # The source is already a canonical private original inside the portable
    # library. Multiple fitxes can reference the same source without duplicating it.
    dest <- src_path

    rights <- trim_or_blank(src$rights_status[1])
    display <- trim_or_blank(src$public_display[1])
    if(!rights %in% PUBLIC_RIGHTS) display <- "none"
    if(!display %in% c("none","thumbnail","excerpt","full")) display <- "none"

    deriv <- make_linked_derivative(dest,target_item_id,source_item_id,rights,display)

    ord <- DBI::dbGetQuery(
      con,"SELECT COALESCE(MAX(sort_order),0)+1 AS n FROM item_assets WHERE item_id=?",
      params=list(target_item_id)
    )$n[1]

    typ <- if(ext=="pdf") "pdf" else if(ext %in% c("jpg","jpeg","png","gif","webp","tif","tiff")) "image" else "document"
    ttl <- trim_or_blank(src$title[1])
    if(!nzchar(ttl)) ttl <- basename(src_path)

    DBI::dbExecute(
      con,
      "INSERT INTO item_assets
       (item_id,asset_type,title,caption,author_or_creator,document_date,
        rights_status,copyright_holder,permission_source,public_display,public_download,
        private_source,public_asset,pixelated_preview,sort_order,notes,created_at,created_by,source_item_id)
       VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)",
      params=list(
        target_item_id,typ,ttl,
        paste0("Document vinculat des del registre intern ",source_item_id,"."),
        trim_or_blank(src$author_or_creator[1]),trim_or_blank(src$document_date[1]),
        rights,"","",display,"no",
        gsub("\\\\","/",dest),deriv$public_asset,"",as.integer(ord),
        trim_or_blank(notes),
        format(Sys.time(),"%Y-%m-%d %H:%M:%S"),
        Sys.getenv("XIPAHUATZIN_ADMIN_USER",unset="admin"),
        source_item_id
      )
    )
    asset_id <- DBI::dbGetQuery(con,"SELECT last_insert_rowid() AS id")$id[1]

    DBI::dbExecute(
      con,
      "INSERT INTO item_links
       (source_item_id,target_item_id,link_type,asset_id,notes,created_at,created_by)
       VALUES (?,?,?,?,?,?,?)",
      params=list(
        source_item_id,target_item_id,"evidence_asset",asset_id,trim_or_blank(notes),
        format(Sys.time(),"%Y-%m-%d %H:%M:%S"),
        Sys.getenv("XIPAHUATZIN_ADMIN_USER",unset="admin")
      )
    )

    list(status="PASS",asset_id=asset_id,message=paste0("Vinculat com a actiu ",asset_id,"."))
  }

  linkage_refresh <- reactiveVal(0)

  output$portable_library_status <- renderUI({
    req(admin_ok())
    linkage_refresh()
    st <- portable_library_status()
    cls <- if(st$found==st$total && st$total>0) "alert alert-success" else "alert alert-danger"
    tags$div(
      class=cls,
      tags$b(paste0("Biblioteca portable: ",st$found,"/",st$total," originals localitzats.")),
      if(length(st$missing)) tags$div(paste("Falten:",paste(st$missing,collapse=", "))),
      tags$div("Ruta operativa: private_sources/library/ · dependències externes: cap.")
    )
  })

  output$linkage_ui <- renderUI({
    req(admin_ok())
    linkage_refresh()
    x <- items_rv()
    drafts <- x[x$record_status=="draft" & (nzchar(trim_or_blank(x$file_name)) | nzchar(trim_or_blank(x$dropbox_path))),,drop=FALSE]
    targets <- x[x$record_status %in% c("reviewed","publishable","published"),,drop=FALSE]

    draft_choices <- if(nrow(drafts))
      setNames(drafts$item_id,paste0(drafts$item_id," — ",drafts$title)) else character(0)
    target_choices <- if(nrow(targets))
      setNames(targets$item_id,paste0(targets$item_id," — ",targets$title)) else character(0)

    tagList(
      tags$h4("Vinculació document → fitxa"),
      uiOutput("portable_library_status"),
      tags$p(
        "La fitxa és la unitat d'interpretació; el registre draft és la font documental. ",
        "La vinculació copia l'original a private_sources/ i crea un actiu associat."
      ),
      tags$div(class="alert alert-info",
        tags$b("Política de visibilitat: "),
        "PUBLIC_DOMAIN, OPEN_LICENSE i PERMISSION_GRANTED poden generar un derivat públic. ",
        "CITATION_ONLY, PRIVATE_RESEARCH_COPY i RIGHTS_UNKNOWN queden exclusivament en privat."
      ),
      tags$div(class="admin-full-input",
        selectInput("link_source_item","Registre documental (draft)",choices=draft_choices,width="100%")
      ),
      tags$div(class="admin-full-input",
        selectInput("link_target_item","Fitxa objectiu",choices=target_choices,width="100%")
      ),
      tags$div(class="admin-full-input",
        textAreaInput("link_notes","Nota de vinculació","",rows=2,width="100%")
      ),
      actionButton("link_create","Vincular com a actiu",class="btn-primary"),
      tags$span(" "),
      actionButton("link_refresh_btn","Actualitzar",class="btn-outline-secondary"),
      tags$div(style="margin-top:.8rem;",verbatimTextOutput("linkage_result")),
      tags$hr(),
      if(file.exists(LINKAGE_MAP_PATH)) tagList(
        tags$h5("Proposta sistemàtica de vinculació"),
        tags$p("La proposta classifica tots els registres draft. Només s'apliquen automàticament les files marcades LINK; REVIEW, GLOBAL i ADMIN queden intactes."),
        actionButton("link_preview_map","Revisar proposta",class="btn-outline-primary"),
        tags$span(" "),
        actionButton("link_apply_map","Aplicar files LINK",class="btn-warning"),
        tags$div(style="margin-top:.8rem;",verbatimTextOutput("linkage_batch_result"))
      ) else tags$div(class="alert alert-warning",
        "No s'ha trobat data/linkage_map_v0_8_12.csv; la vinculació manual continua disponible."
      ),
      tags$hr(),
      tags$p(class="text-muted",
        "Els registres draft es conserven per traçabilitat. La relació queda registrada a item_links i a source_item_id de l'actiu."
      )
    )
  })

  observeEvent(input$link_refresh_btn,{
    linkage_refresh(linkage_refresh()+1L)
    refresh_items()
  })

  observeEvent(input$link_create,{
    req(admin_ok(),input$link_source_item,input$link_target_item)
    msg <- tryCatch({
      ans <- create_linked_asset(input$link_source_item,input$link_target_item,input$link_notes)
      linkage_refresh(linkage_refresh()+1L)
      asset_refresh(asset_refresh()+1L)
      paste0(ans$status," — ",ans$message)
    },error=function(e) paste0("ERROR — ",conditionMessage(e)))
    output$linkage_result <- renderText(msg)
  })


  observeEvent(input$link_preview_map,{
    req(admin_ok())
    if(!file.exists(LINKAGE_MAP_PATH)){
      output$linkage_batch_result <- renderText("ERROR — No existeix el mapa de vinculació.")
      return()
    }
    mp <- read.csv(LINKAGE_MAP_PATH,stringsAsFactors=FALSE,check.names=FALSE)
    counts <- table(mp$action)
    txt <- paste0(
      "MAPA DE VINCULACIÓ\n\n",
      paste(names(counts),as.integer(counts),sep=": ",collapse="\n"),
      "\n\nNomés LINK s'aplica automàticament."
    )
    output$linkage_batch_result <- renderText(txt)
  })

  observeEvent(input$link_apply_map,{
    req(admin_ok())
    if(!file.exists(LINKAGE_MAP_PATH)){
      showNotification("No existeix el mapa de vinculació.",type="error")
      return()
    }
    mp <- read.csv(LINKAGE_MAP_PATH,stringsAsFactors=FALSE,check.names=FALSE)
    todo <- mp[toupper(trim_or_blank(mp$action))=="LINK",,drop=FALSE]

    showModal(modalDialog(
      title="Aplicar vinculació sistemàtica?",
      tags$p(
        "S'intentaran aplicar ",nrow(todo),
        " relacions marcades LINK. Els originals es copiaran a private_sources/."
      ),
      tags$p(
        "Els registres draft no s'eliminaran. Les relacions ja existents se saltaran."
      ),
      footer=tagList(
        modalButton("Cancel·lar"),
        actionButton("confirm_link_apply_map","Aplicar",class="btn-warning")
      ),
      easyClose=FALSE
    ))
  })

  observeEvent(input$confirm_link_apply_map,{
    req(admin_ok())
    removeModal()

    txt <- tryCatch({
      mp <- read.csv(LINKAGE_MAP_PATH,stringsAsFactors=FALSE,check.names=FALSE)
      todo <- mp[toupper(trim_or_blank(mp$action))=="LINK",,drop=FALSE]
      log <- character(0)
      n_pass <- 0L; n_skip <- 0L; n_err <- 0L

      for(i in seq_len(nrow(todo))){
        src <- trim_or_blank(todo$source_item_id[i])
        tgt <- trim_or_blank(todo$target_item_id[i])
        note <- trim_or_blank(todo$notes[i])
        ans <- tryCatch(
          create_linked_asset(src,tgt,note),
          error=function(e) list(status="ERROR",message=conditionMessage(e))
        )
        if(ans$status=="PASS") n_pass <- n_pass+1L
        else if(ans$status=="SKIP") n_skip <- n_skip+1L
        else n_err <- n_err+1L
        log <- c(log,paste0(ans$status," | ",src," → ",tgt," | ",ans$message))
      }

      linkage_refresh(linkage_refresh()+1L)
      asset_refresh(asset_refresh()+1L)
      refresh_items()

      paste0(
        "RESULTAT VINCULACIÓ SISTEMÀTICA\n",
        "PASS: ",n_pass,"\n",
        "SKIP: ",n_skip,"\n",
        "ERROR: ",n_err,"\n\n",
        paste(log,collapse="\n")
      )
    },error=function(e) paste0("ERROR GLOBAL — ",conditionMessage(e)))

    output$linkage_batch_result <- renderText(txt)
  })

  output$linkage_table <- renderDT({
    req(admin_ok())
    linkage_refresh()
    con <- db_connect()
    on.exit(DBI::dbDisconnect(con),add=TRUE)
    x <- DBI::dbGetQuery(con,"
      SELECT l.link_id,l.source_item_id,l.target_item_id,l.asset_id,l.link_type,
             l.created_at,l.created_by,l.notes
      FROM item_links l
      ORDER BY l.link_id DESC
    ")
    datatable(x,rownames=FALSE,options=list(pageLength=20,scrollX=TRUE),
      colnames=c("ID","Registre font","Fitxa objectiu","Actiu","Tipus","Creat","Usuari","Notes"))
  })


  output$import_record_ui <- renderUI({
    req(admin_ok())
    tagList(
      tags$h4("Importar una nova fitxa"),
      tags$p("Carrega un paquet ZIP creat fora de la wiki. Ha de contenir record.json i, opcionalment, sources/ i derivatives/."),
      tags$div(class="admin-full-input",
        fileInput("record_package_file","Paquet de fitxa (.zip)",accept=".zip",width="100%")
      ),
      checkboxInput("record_package_overwrite","Permetre actualitzar una fitxa existent amb el mateix item_id",FALSE),
      actionButton("import_record_package","Validar i incorporar",class="btn-primary"),
      tags$hr(),
      tags$div(class="import-log-wide",verbatimTextOutput("import_record_result"))
    )
  })

  observeEvent(input$import_record_package,{
    req(admin_ok(),input$record_package_file)

    txt <- tryCatch({
      con <- db_connect()
      on.exit(DBI::dbDisconnect(con), add=TRUE)

      td <- tempfile("record_import_"); dir.create(td)
      utils::unzip(input$record_package_file$datapath,exdir=td)

      rec_path <- file.path(td,"record.json")
      if(!file.exists(rec_path)) stop("El paquet no conté record.json.")

      pkg <- jsonlite::fromJSON(rec_path,simplifyVector=FALSE)
      if(is.null(pkg$item)) stop("record.json no conté item.")
      item <- pkg$item

      id <- trim_or_blank(item$item_id)
      if(!nzchar(id)) stop("Falta item_id.")

      required <- c(
        "item_id","title","category","record_status",
        "description","historical_context","legend_contribution",
        "documentary_value","limitations","project_interpretation",
        "rights_status","public_display"
      )

      miss <- required[!vapply(required,function(nm)
        nm %in% names(item) && nzchar(trim_or_blank(item[[nm]])),logical(1))]
      if(length(miss)) stop("Falten camps obligatoris: ",paste(miss,collapse=", "))

      if(!item$record_status %in% c("draft","reviewed","publishable","published"))
        stop("record_status no vàlid.")
      if(!item$public_display %in% c("none","thumbnail","excerpt","full"))
        stop("public_display no vàlid.")
      if(!item$rights_status %in% c(
        "PUBLIC_DOMAIN","OPEN_LICENSE","PERMISSION_GRANTED",
        "CITATION_ONLY","PRIVATE_RESEARCH_COPY","RIGHTS_UNKNOWN"))
        stop("rights_status no vàlid.")

      old <- DBI::dbGetQuery(con,"SELECT * FROM items WHERE item_id=?",params=list(id))
      if(nrow(old)>0 && !isTRUE(input$record_package_overwrite))
        stop("Ja existeix item_id=",id,". Marca l'opció d'actualització si la vols substituir.")

      fields <- DBI::dbListFields(con,"items")

      if(nrow(old)>0){
        # Important: an asset-only/update package must preserve all fields
        # not explicitly supplied by the package.
        row <- as.list(old[1,fields,drop=FALSE])
        row <- lapply(row,function(v) ifelse(is.na(v),"",as.character(v)))
      } else {
        row <- as.list(setNames(rep("",length(fields)),fields))
      }

      for(nm in intersect(names(item),fields))
        row[[nm]] <- if(is.null(item[[nm]])) "" else as.character(item[[nm]])

      # Cap importació externa no publica directament.
      if(identical(row$record_status,"published")) row$record_status <- "reviewed"

      # Política v0.8.13.2: sense dret explícit de reproducció no hi ha actiu públic.
      if(!trim_or_blank(row$rights_status) %in% PUBLIC_RIGHTS){
        row$public_display <- "none"
        if("public_download" %in% names(row)) row$public_download <- "no"
        row$public_asset <- ""
        row$pixelated_preview <- ""
      }

      files <- pkg$files
      if(is.null(files)) files <- list()

      pkg_assets <- pkg$assets
      if(is.null(pkg_assets)) pkg_assets <- list()

      src_name <- trim_or_blank(files$source_file)
      if(nzchar(src_name)){
        src <- file.path(td,"sources",basename(src_name))
        if(!file.exists(src)) stop("No s'ha trobat sources/",basename(src_name))
        ext <- tools::file_ext(src)
        dest <- file.path("private_sources",paste0(gsub("[^A-Za-z0-9._-]+","_",id),if(nzchar(ext)) paste0(".",tolower(ext)) else ""))
        file.copy(src,dest,overwrite=TRUE)
        row$portable_source <- gsub("\\\\","/",dest)
        row$file_name <- basename(src)
      }

      pub_name <- trim_or_blank(files$public_asset)
      if(nzchar(pub_name) && public_rights_allowed(row$rights_status,row$public_display)){
        src <- file.path(td,"derivatives",basename(pub_name))
        if(!file.exists(src)) stop("No s'ha trobat derivatives/",basename(pub_name))
        subdir <- switch(row$public_display,full="public",thumbnail="thumbnails",excerpt="excerpts","restricted_previews")
        dir.create(file.path("www",subdir),recursive=TRUE,showWarnings=FALSE)
        dest <- file.path("www",subdir,paste0(gsub("[^A-Za-z0-9._-]+","_",id),"_",basename(pub_name)))
        file.copy(src,dest,overwrite=TRUE)
        row$public_asset <- gsub("\\\\","/",dest)
      }

      # Els previews restringits ja no es publiquen sota www/.
      px_name <- trim_or_blank(files$pixelated_preview)
      if(nzchar(px_name)) row$pixelated_preview <- ""

      if(nrow(old)==0){
        sql <- paste0(
          "INSERT INTO items (",paste(DBI::dbQuoteIdentifier(con,names(row)),collapse=","),
          ") VALUES (",paste(rep("?",length(row)),collapse=","),")"
        )
        DBI::dbExecute(con,sql,params=unname(row))
        prev_status <- ""
        change_type <- "NEW_RECORD_IMPORT"
      } else {
        prev_status <- old$record_status[1]
        f <- setdiff(names(row),"item_id")
        sql <- paste0("UPDATE items SET ",
                      paste0(DBI::dbQuoteIdentifier(con,f),"=?",collapse=","),
                      " WHERE item_id=?")
        DBI::dbExecute(con,sql,params=c(unname(row[f]),id))
        change_type <- "PACKAGE_UPDATE_IMPORT"
      }

      note <- trim_or_blank(pkg$revision_note)
      if(!nzchar(note)) note <- "Importació de paquet de fitxa."

      DBI::dbExecute(con,
        "INSERT INTO revision_history
         (item_id,modified_at,modified_by,revision_note,previous_status,new_status,changed_fields)
         VALUES (?,?,?,?,?,?,?)",
        params=list(
          id,format(Sys.time(),"%Y-%m-%d %H:%M:%S"),
          Sys.getenv("XIPAHUATZIN_ADMIN_USER",unset="admin"),
          note,prev_status,row$record_status,change_type
        )
      )


      # Optional multiple assets bundled in the package.
      if(length(pkg_assets)){
        for(j in seq_along(pkg_assets)){
          aa <- pkg_assets[[j]]
          src_name2 <- trim_or_blank(aa$source_file)
          pub_name2 <- trim_or_blank(aa$public_asset)
          px_name2 <- trim_or_blank(aa$pixelated_preview)

          priv_rel2 <- ""; pub_rel2 <- ""; px_rel2 <- ""
          rights2 <- ifelse(is.null(aa$rights_status),"RIGHTS_UNKNOWN",as.character(aa$rights_status))
          display2 <- ifelse(is.null(aa$public_display),"none",as.character(aa$public_display))
          if(!rights2 %in% PUBLIC_RIGHTS) display2 <- "none"

          if(nzchar(src_name2)){
            s2 <- file.path(td,"sources",basename(src_name2))
            if(!file.exists(s2)) stop("No s'ha trobat sources/",basename(src_name2))
            ext2 <- tolower(tools::file_ext(s2))
            dest2 <- file.path(
              "private_sources",
              paste0(gsub("[^A-Za-z0-9._-]+","_",id),"_asset_",j,".",ext2)
            )
            file.copy(s2,dest2,overwrite=TRUE)
            priv_rel2 <- gsub("\\\\","/",dest2)
          }

          if(nzchar(pub_name2) && public_rights_allowed(rights2,display2)){
            p2 <- file.path(td,"derivatives",basename(pub_name2))
            if(!file.exists(p2)) stop("No s'ha trobat derivatives/",basename(pub_name2))
            sub2 <- switch(display2,full="public",thumbnail="thumbnails",excerpt="excerpts","restricted_previews")
            dir.create(file.path("www",sub2),recursive=TRUE,showWarnings=FALSE)
            destp2 <- file.path(
              "www",sub2,
              paste0(gsub("[^A-Za-z0-9._-]+","_",id),"_asset_",j,"_",basename(pub_name2))
            )
            file.copy(p2,destp2,overwrite=TRUE)
            pub_rel2 <- gsub("\\\\","/",destp2)
          }

          # No es crea cap preview públic per a actius restringits.
          px_rel2 <- ""

          DBI::dbExecute(
            con,
            "INSERT INTO item_assets
             (item_id,asset_type,title,caption,author_or_creator,document_date,
              rights_status,copyright_holder,permission_source,
              public_display,public_download,private_source,public_asset,
              pixelated_preview,sort_order,notes,created_at,created_by)
             VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)",
            params=list(
              id,
              ifelse(is.null(aa$asset_type),"image",as.character(aa$asset_type)),
              ifelse(is.null(aa$title),"",as.character(aa$title)),
              ifelse(is.null(aa$caption),"",as.character(aa$caption)),
              ifelse(is.null(aa$author_or_creator),"",as.character(aa$author_or_creator)),
              ifelse(is.null(aa$document_date),"",as.character(aa$document_date)),
              rights2,
              ifelse(is.null(aa$copyright_holder),"",as.character(aa$copyright_holder)),
              ifelse(is.null(aa$permission_source),"",as.character(aa$permission_source)),
              display2,
              if(rights2 %in% PUBLIC_RIGHTS) ifelse(is.null(aa$public_download),"no",as.character(aa$public_download)) else "no",
              priv_rel2,pub_rel2,px_rel2,
              ifelse(is.null(aa$sort_order),j,as.integer(aa$sort_order)),
              ifelse(is.null(aa$notes),"",as.character(aa$notes)),
              format(Sys.time(),"%Y-%m-%d %H:%M:%S"),
              Sys.getenv("XIPAHUATZIN_ADMIN_USER",unset="admin")
            )
          )
        }
      }

      refresh_items()

      paste0(
        "PASS\n",
        "item_id: ",id,"\n",
        "Títol: ",row$title,"\n",
        "Estat: ",row$record_status,"\n",
        "Original incorporat: ",ifelse(nzchar(row$portable_source),"sí","no"),"\n",
        "Actiu públic incorporat: ",ifelse(nzchar(row$public_asset),"sí","no"),"\n",
        "Pixelat incorporat: ",ifelse(nzchar(row$pixelated_preview),"sí","no")
      )
    },error=function(e) paste0("ERROR\n",conditionMessage(e)))

    output$import_record_result <- renderText(txt)
  })

  output$history_table <- renderDT({
    req(admin_ok(),input$admin_item)
    con <- db_connect()
    on.exit(DBI::dbDisconnect(con),add=TRUE)
    h <- DBI::dbGetQuery(con,
      "SELECT modified_at,modified_by,revision_note,previous_status,new_status,changed_fields
       FROM revision_history WHERE item_id=? ORDER BY revision_id DESC",
      params=list(input$admin_item)
    )
    datatable(h,rownames=FALSE,options=list(pageLength=20,scrollX=TRUE),
      colnames=c("Modificat","Usuari","Nota","Estat anterior","Estat nou","Camps modificats"))
  })

  output$admin_inventory <- renderDT({
    req(admin_ok())
    x <- items_rv()
    x <- x[order(x$display_year_num,soldevila_sort_key(x$title),na.last=TRUE),,drop=FALSE]
    out <- x[,c("item_id","display_year_label","title","category","record_status","rights_status","public_display")]
    datatable(out,rownames=FALSE,filter="top",options=list(pageLength=25,scrollX=TRUE),
      colnames=c("ID","Any","Títol","Categoria","Estat","Drets","Visualització"))
  })

  output$download_csv <- downloadHandler(
    filename=function() paste0("xipahuatzin_wiki_",format(Sys.Date(),"%Y%m%d"),".csv"),
    content=function(file){
      x <- items_rv()
      drop <- intersect(c("display_year","display_year_num","display_year_label"),names(x))
      x <- x[,setdiff(names(x),drop),drop=FALSE]
      write.csv(x,file,row.names=FALSE,fileEncoding="UTF-8")
    }
  )

  output$download_xlsx <- downloadHandler(
    filename=function() paste0("xipahuatzin_wiki_",format(Sys.Date(),"%Y%m%d"),".xlsx"),
    content=function(file){
      if(!requireNamespace("writexl",quietly=TRUE)){
        stop("Per exportar XLSX cal instal·lar el paquet 'writexl': install.packages('writexl')")
      }
      x <- items_rv()
      drop <- intersect(c("display_year","display_year_num","display_year_label"),names(x))
      x <- x[,setdiff(names(x),drop),drop=FALSE]
      con <- db_connect()
      on.exit(DBI::dbDisconnect(con),add=TRUE)
      hist <- DBI::dbReadTable(con,"revision_history",check.names=FALSE)
      writexl::write_xlsx(list(Fitxes=x,Historial=hist,Categories=categories,Drets=rights_policy),path=file)
    }
  )

  output$admin_categories <- renderDT({
    req(admin_ok())
    datatable(categories,rownames=FALSE,options=list(dom="t",paging=FALSE))
  })

  output$admin_rights <- renderDT({
    req(admin_ok())
    datatable(rights_policy,rownames=FALSE,options=list(dom="t",paging=FALSE))
  })
}

shinyApp(ui,server)
