# MASTER_TEST_AUDIT_20261020 — ONLY for isolated MASTER_TEST. READ-ONLY.
mt_audit_db <- function() file.path('data','ADL_MASTER_TEST_VINCULACIONS_20261020.sqlite')
mt_read <- function(sql, params=list()) {
  if (!file.exists(mt_audit_db())) stop('No existeix la SQLite de prova: ', mt_audit_db())
  con <- DBI::dbConnect(RSQLite::SQLite(),dbname=mt_audit_db(),flags=RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con),add=TRUE)
  if (length(params)) {
    DBI::dbGetQuery(con, sql, params=params)
  } else {
    DBI::dbGetQuery(con, sql)
  }
}
mt_audit_ui <- function() {
  tagList(
    h3('MASTER_TEST · contrast de fitxes i CANÒNICS'),
    p('Vista administrativa de només lectura. Les correspondències noves NO substitueixen els actius de la wiki.'),
    DT::DTOutput('mt_audit_grid'),
    uiOutput('mt_audit_detail')
  )
}
mt_audit_server <- function(input, output, session) {
  mapping <- shiny::reactive({
    mt_read(paste(
      'SELECT m.item_id, COALESCE(i.title,\'\') AS titol,',
      'COALESCE(i.bibliographic_reference,m.bibliographic_reference,\'\') AS referencia,',
      'm.rds_group AS grup, m.canonical_name AS canonical,',
      'm.canonical_proposed_file AS fitxer_proposat, m.review_status AS estat,',
      'm.sqlite_existing_assets AS actius_existents',
      'FROM adl_test_canonical_map m LEFT JOIN items i ON i.item_id=m.item_id',
      'ORDER BY m.rds_group,m.item_id'
    ))
  })
  output$mt_audit_grid <- DT::renderDT({
    x <- mapping()
    DT::datatable(x,rownames=FALSE,selection='single',filter='top',
      options=list(pageLength=15,scrollX=TRUE,autoWidth=TRUE,dom='ftip'),
      escape=TRUE)
  })
  output$mt_audit_detail <- shiny::renderUI({
    k <- input$mt_audit_grid_rows_selected
    x <- mapping()
    if (length(k)!=1L || k<1L || k>nrow(x))
      return(p('Selecciona una fitxa per contrastar la referència i els actius actuals.'))
    r <- x[k,,drop=FALSE]
    a <- mt_read('SELECT asset_id,title,private_source,public_asset,rights_status,public_display FROM item_assets WHERE item_id=? ORDER BY sort_order,asset_id',list(r$item_id[[1]]))
    tagList(
      hr(),h4(as.character(r$titol[[1]])),
      tags$p(tags$b('ITEM_ID: '),as.character(r$item_id[[1]])),
      tags$p(tags$b('Bibliografia SQLite: '),as.character(r$referencia[[1]])),
      tags$p(tags$b('Grup RDS: '),as.character(r$grup[[1]])),
      tags$p(tags$b('Base CANÒNIC: '),as.character(r$canonical[[1]])),
      tags$p(tags$b('Nom proposat: '),as.character(r$fitxer_proposat[[1]])),
      if(identical(as.character(r$estat[[1]]),'BLOCKED'))
        tags$p(style='color:#9a3d18;font-weight:bold','BLOQUEJAT: no aplicar aquesta assignació.')
      else tags$p(style='color:#845c14','PENDENT: confirmar ruta i contingut físic abans d’activar.'),
      h5('Documents actualment vinculats a la fitxa a SQLite'),
      if(nrow(a)) DT::datatable(a,rownames=FALSE,escape=TRUE,options=list(dom='t',scrollX=TRUE))
      else p('Aquesta fitxa encara no té entrades a item_assets.'),
      p('Per comprovar visualment el document antic, obre la fitxa al visor habitual. El CANÒNIC nou no es pot visualitzar fins que la ruta física estigui verificada.'),
      actionButton('mt_view_original','Obrir fitxa al visor de la wiki')
    )
  })
  observeEvent(input$mt_view_original, {
    k <- input$mt_audit_grid_rows_selected
    x <- mapping()
    if(length(k)!=1L || k<1L || k>nrow(x)) return()
    id <- as.character(x$item_id[k])
    shiny::updateSelectInput(session,'public_item',selected=id)
    try(bslib::nav_select(id='main_nav',selected='Fitxes',session=session),silent=TRUE)
  })
}
