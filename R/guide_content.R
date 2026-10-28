# ADL_GUIDE_MINIATURES_V1_20261028
# ============================================================
# GUIA DE LECTURA — CA / ES
# Contingut editorial públic
# ============================================================

guide_link <- function(label,item_id){
  tags$a(
    href="#",
    class="guide-record-link",
    onclick=sprintf(
      "Shiny.setInputValue('guide_item','%s',{priority:'event'}); return false;",
      item_id
    ),
    label
  )
}

# ADL_GUIDE_LAYOUT_V3 — 2 COLUMNES, CA/ES
guide_section <- function(...){

  parts <- list(...)

  if(length(parts) < 2L ||
     !inherits(parts[[1L]], "shiny.tag") ||
     !identical(parts[[1L]]$name, "figure")){
    stop("GUIDE_LAYOUT_V3: estructura inesperada")
  }

  miniature <- parts[[1L]]
  paragraphs <- parts[-1L]

  tags$div(
    class="adl-guide-layout",

    tags$style(HTML("
      .adl-guide-layout {
        display: grid;
        grid-template-columns: 235px minmax(0,1fr);
        gap: 14px;
        align-items: start;
        padding: 14px;
        margin: 0;
        background: #f5efe4;
        border: 1px solid #e0d5c2;
        border-radius: 14px;
        box-sizing: border-box;
      }

      .adl-guide-text {
        min-width: 0;
        padding: 17px 20px;
        background: #fffdf9;
        border: 1px solid #ded5c5;
        border-radius: 11px;
        box-shadow: 0 2px 8px rgba(46,40,27,.04);
        line-height: 1.49;
      }

      .adl-guide-text p {
        margin: 0 0 .70rem;
        line-height: 1.49;
      }

      .adl-guide-text p:last-child {
        margin-bottom: 0;
      }

      .adl-guide-image {
        order: -1;
        min-width: 0;
        padding: 12px;
        background: #fffdf9;
        border: 1px solid #ded5c5;
        border-radius: 11px;
        box-shadow: 0 2px 8px rgba(46,40,27,.04);
      }

      .adl-guide-image figure {
        float: none !important;
        width: 100% !important;
        max-width: 100% !important;
        margin: 0 !important;
      }

      .adl-guide-image img {
        width: 100% !important;
        height: auto !important;
        max-height: none !important;
        display: block;
        object-fit: contain !important;
        border-radius: 8px;
      }

      .adl-guide-image figcaption {
        margin-top: .45rem;
        font-size: .8rem;
      }

      @media (max-width: 760px) {
        .adl-guide-layout {
          grid-template-columns: minmax(0,1fr);
          gap: 10px;
          padding: 10px;
        }

        .adl-guide-text {
          padding: 13px;
        }

        .adl-guide-image {
          width: min(100%, 245px);
          justify-self: center;
        }
      }
    ")),

    tags$div(
      class="adl-guide-text",
      do.call(tagList, paragraphs)
    ),

    tags$div(
      class="adl-guide-image",
      miniature
    )
  )
}

guide_figure <- function(src,alt="",caption=NULL,mode="full"){
  if(identical(mode,"side")){
    tags$figure(
      style="width:205px;max-width:32%;float:right;margin:0 0 .55rem 1rem;",
      tags$img(
        src=src,
        alt=alt,
        style="width:100%;max-height:220px;object-fit:cover;object-position:center center;display:block;border-radius:.75rem;"
      ),
      if(!is.null(caption))
        tags$figcaption(
          style="font-size:.88rem;color:#666;margin-top:.45rem;line-height:1.35;",
          caption
        )
    )
  } else {
    tags$figure(
      style="max-width:980px;margin:0 auto 1.25rem auto;",
      tags$img(
        src=src,
        alt=alt,
        style="width:100%;max-height:360px;object-fit:cover;object-position:center center;display:block;border-radius:.75rem;"
      ),
      if(!is.null(caption))
        tags$figcaption(
          style="font-size:.92rem;color:#666;margin-top:.5rem;line-height:1.4;",
          caption
        )
    )
  }
}

guide_ui <- function(lang="ca"){

  if(identical(lang,"es")){

    navset_card_tab(

      nav_panel(
        "La leyenda",
        guide_section(
          guide_figure("assets/placa_toloriu.png", alt="Miniatura decorativa: La leyenda", mode="side"),
          
          tags$p(
            "Una tradición contemporánea vincula Toloriu, al pie del Cadí, con una supuesta hija de Moctezuma II conocida habitualmente como ",
            tags$strong("Xipahuatzin"),
            ", Xipaguazín o María de Moctezuma. Según la versión que acabaría consolidándose, la princesa habría llegado a la Península después de la conquista de México, se habría unido o casado con un caballero llamado Joan o Juan de Grau, habría vivido en Toloriu y habría dejado allí descendencia. Algunas versiones sitúan también su muerte y sepultura en el pueblo."
          ),
          tags$p(
            "Sin embargo, esta narración no aparece como un relato único y estable desde el siglo XVI. Sus componentes se documentan progresivamente durante el siglo XX. El año 1952 constituye un momento especialmente importante: ",
            guide_link("Enrique Rubio recoge en marzo la figura de una «princesa María de Moctezuma»","EV1952_03_16_RUBIO"),
            ", y pocas semanas después ",
            guide_link("Manuel Reyes publica una genealogía que une a María/Chipahuatzin con Juan de Grau","EV1952_05_24_REYES"),
            " y les atribuye descendencia."
          ),
          tags$p(
            "En este proceso, ",
            guide_link("los textos vinculados a Joaquín del Álamo","EV1952_DEL_ALAMO"),
            " contribuyen a dar forma genealógica y territorial al relato y a relacionarlo expresamente con Toloriu. La materialización pública de esta asociación se hace especialmente visible con ",
            guide_link("la placa colocada en la iglesia de Toloriu en 1963","EV1963_PLACA_TOLORIU"),
            ", que presenta a María Xipaguazín Moctezuma como una figura histórica vinculada al pueblo."
          ),
          tags$p(
            "A partir de entonces, libros, artículos, testimonios y actividades culturales amplían el relato. Los motivos de la princesa, el caballero, la descendencia, el sepulcro y el tesoro se integran retrospectivamente en una historia aparentemente continua. La difusión de esa historia permite estudiar su arraigo cultural, pero no convierte sus episodios del siglo XVI en hechos documentados."
          ),
          tags$p(
            tags$strong("La pregunta documental que guía este archivo es, por tanto: "),
            "¿cuándo aparece cada elemento, qué fuente lo documenta, de qué fuentes anteriores depende y en qué momento los distintos componentes quedan unidos en la leyenda que conocemos hoy?"
          )
        )
      ),

      nav_panel(
        "Contexto",
        guide_section(
          guide_figure("assets/indi.png", alt="Miniatura decorativa: Contexto", mode="side"),
          
          tags$p(
            "El contexto histórico obliga a distinguir entre posibilidad general y trayectoria individual demostrada. La presencia y movilidad de personas indígenas entre América y la Península durante el siglo XVI está documentada y forma parte del marco atlántico de la época. ",
            guide_link("Las noticias sobre indígenas trasladados por Hernán Cortés a la Península","EV1528_CORTES_INDIGENES_PENINSULA"),
            " muestran que estos desplazamientos existieron. Esta realidad, sin embargo, no demuestra por sí misma el viaje de Xipahuatzin ni su presencia en Toloriu."
          ),
          tags$p(
            "También existen elementos históricos locales que pudieron favorecer posteriormente el anclaje territorial de la leyenda. ",
            guide_link("El copón medieval relacionado con Ramon Grau","EV1390_COPO_RAMON_GRAU"),
            " documenta un Grau real en un contexto histórico muy anterior, mientras que ",
            guide_link("el dossier sobre el apellido Grau en Toloriu","DS_GRAU_TOLORIU"),
            " reúne evidencias de la presencia efectiva del apellido en el pueblo."
          ),
          tags$p(
            "Entre estas evidencias modernas se encuentran ",
            guide_link("Marie Grau, nacida en Toloriu en 1930","EV1930_MARIE_GRAU_TOLORIU"),
            ", ",
            guide_link("la familia Grau i Grau de Casa Miret","EV2026_GRAU_CASA_MIRET"),
            " y ",
            guide_link("Maria Grau Blanch","EV2026_MARIA_GRAU_BLANCH"),
            ". Son datos relevantes para reconstruir la historia local del apellido, pero no establecen una filiación con el Joan de Grau de la leyenda ni con Guillem Grau Rifé."
          ),
          tags$p(
            "El contexto contemporáneo es igualmente importante. ",
            guide_link("La documentación de 1939 sobre Guillem Grau Rifé","EV1939_GRAU_MATRIMONI"),
            " identifica a Guillermo de Grau Rifé como natural de Barcelona, hijo de Guillermo de Grau Iscla y Mercedes Rifé Brosa. Esta filiación documentada no acredita una ascendencia de Moctezuma ni una relación genealógica con Toloriu. Posteriormente, ",
            guide_link("la actividad del círculo de Charles Grando entre 1956 y 1961","EV1956_1961_GRANDO"),
            " y ",
            guide_link("la documentación publicada en el BOE en 1960","EV1960_BOE"),
            " muestran que la reivindicación Grau-Moctezuma adquirió también formas institucionales o pseudo-institucionales."
          ),
          tags$p(
            "Este apartado reúne, por tanto, hechos y personas que ayudan a entender el terreno histórico sobre el que pudo construirse la narración, sin convertir automáticamente esos elementos reales en pruebas de la genealogía legendaria."
          )
        )
      ),

      nav_panel(
        "Reivindicación",
        guide_section(
          guide_figure("assets/escut.png", alt="Miniatura decorativa: Reivindicación Grau-Moctezuma", mode="side"),
          tags$p(
            "La leyenda de Xipahuatzin en Toloriu no puede entenderse aisladamente de una reivindicación más amplia articulada alrededor de Guillem o Guillermo Grau Rifé. La documentación contemporánea permite observar, antes de la fijación territorial de la historia, una pretensión nobiliaria y sucesoria que vinculaba el apellido Grau con Moctezuma. La existencia y la circulación de esa pretensión no son pruebas de la genealogía alegada."
          ),
          tags$p(
            guide_link("La documentación matrimonial de 1939","EV1939_GRAU_MATRIMONI"),
            " constituye un antecedente biográfico importante. En febrero de 1952, ",
            guide_link("«Yo, el príncipe, en el calabozo» (26-02-1952)","EV1952_02_20_SOLIDARIDAD"),
            " muestra que la pretensión relacionada con la llamada «Corona de Méjico» ya había adquirido presencia pública. Poco después, ",
            guide_link("la prensa recoge también el carácter polémico de esta reivindicación","EV1952_02_24_DIARIO_BURGOS"),
            "."
          ),
          tags$p(
            "Durante la primavera de 1952 la reivindicación se transforma rápidamente en una construcción genealógica más definida. ",
            guide_link("Enrique Rubio","EV1952_03_16_RUBIO"),
            " incorpora la figura de una princesa María de Moctezuma; ",
            guide_link("el 5 de abril publica una supuesta confesión de Grau Rifé y cuestiona la figura de Juan de Grau","EV1952_04_05_RUBIO_CONFESSA"),
            "; y ",
            guide_link("Manuel Reyes, el 24 de mayo","EV1952_05_24_REYES"),
            ", presenta ya una genealogía que enlaza explícitamente a María/Chipahuatzin con Juan de Grau y una descendencia posterior."
          ),
          tags$p(
            guide_link("Los materiales asociados a Joaquín del Álamo","EV1952_DEL_ALAMO"),
            " profundizan esta construcción y contribuyen a vincularla con Toloriu. En los años siguientes, ",
            guide_link("el entorno de Charles Grando","EV1956_1961_GRANDO"),
            " le proporciona una dimensión pseudo-institucional, mientras que ",
            guide_link("la documentación del BOE de 1960","EV1960_BOE"),
            " permite seguir la presencia pública de denominaciones y pretensiones relacionadas con Grau-Moctezuma."
          ),
          tags$p(
            "Décadas después, ",
            guide_link("la prensa de 1983","EV1983_LA_VANGUARDIA"),
            " muestra la persistencia o reaparición pública de la «Casa de Grau-Moctezuma». Vista en conjunto, esta secuencia permite distinguir una primera fase de ",
            tags$strong("reivindicación dinástica y genealógica"),
            " de la posterior conversión de algunos de sus elementos en una leyenda histórica y territorial centrada en Toloriu."
          )
        )
      ),

      nav_panel(
        "Control documental / ausencia",
        guide_section(
          guide_figure("assets/llibres.png", alt="Miniatura decorativa: Control documental / ausencia", mode="side"),
          tags$p(
            "El silencio documental exige valorar qué investigaba cada fuente, qué territorio describía y qué elementos acostumbraba a registrar. La ausencia de una noticia en una obra aislada no demuestra que la tradición no existiera; la coincidencia de varios testimonios pertinentes permite, en cambio, delimitar qué relato no hemos podido documentar antes de mediados del siglo XX."
          ),
          tags$p(
            guide_link("Artur Osona describe Toloriu en 1899","EV1899_OSONA"),
            " sin recoger una trama mexicana asociada al pueblo. ",
            guide_link("Cèsar August Torras, en 1924","EV1924_TORRAS"),
            ", trata Toloriu y el Querforadat sin mencionar Xipahuatzin, Joan de Grau ni una conexión con Moctezuma. ",
            guide_link("Bonet i Isart, en 1935","EV1935_BONET_ISART_CONTROL"),
            ", aporta un control territorial complementario."
          ),
          tags$p(
            "Los repertorios de Joan Amades ofrecen otra perspectiva. ",
            guide_link("El Pirineu","EV1949_AMADES"),
            " y ",
            guide_link("el Costumari català","EV1950_1956_AMADES_COSTUMARI"),
            " no proporcionan, en las búsquedas realizadas, una formulación individualizada de la historia Xipahuatzin–Grau–Toloriu anterior a la irrupción pública de los años cincuenta."
          ),
          tags$p(
            "Como control posterior a la difusión pública de la leyenda, la obra de Xavier Fàbregas ",
            tags$em("Tradicions, mites i creences dels catalans"),
            " (1979) ofrece un punto de comparación complementario. Aunque presta atención a las tradiciones y leyendas catalanas, no recoge la historia de Xipahuatzin y Toloriu. Esta ausencia, posterior a las publicaciones de 1952 y a la placa de 1963, no permite determinar hasta qué punto el relato era conocido fuera del ámbito local."
          ),
          tags$p(
            tags$strong("La conclusión debe formularse con precisión: "),
            "con las fuentes examinadas hasta ahora, no se ha localizado una formulación documental anterior a 1952 de la cadena completa Xipahuatzin–Grau–Toloriu. Esto no equivale a demostrar una inexistencia absoluta, sino a establecer el límite que permite la evidencia disponible."
          )
        )
      ),

      nav_panel(
        "Difusión y fijación",
        guide_section(
          guide_figure("assets/paisatge_muntanya.png", alt="Miniatura decorativa: Difusión y fijación", mode="side"),
          
          tags$p(
            "A partir de las publicaciones de 1952, la narración se extiende más allá del contexto de la reclamación Grau-Moctezuma. Toloriu adquiere una posición central y el relato circula mediante placas, publicaciones, celebraciones, literatura y medios audiovisuales. Esta circulación debe estudiarse como historia de la recepción, no como corroboración retrospectiva de los hechos narrados."
          ),
          tags$p(
            "Un hito especialmente visible es ",
            guide_link("la placa instalada en Toloriu en 1963","EV1963_PLACA_TOLORIU"),
            ", que fija materialmente en el paisaje local la asociación entre María Xipaguazín Moctezuma y el pueblo. Posteriormente, la historia circula en textos y reconstrucciones de ámbito pirenaico y catalán, entre ellos ",
            guide_link("Esteve Albert","EV1974_ESTEVE_ALBERT"),
            ", ",
            guide_link("nuevas elaboraciones de Esteve Albert","EV1982_ESTEVE_ALBERT"),
            " y ",
            guide_link("las publicaciones de mossèn Soldevila","EV1976_SOLDEVILA_1"),
            " ",
            guide_link("sobre la tradición","EV1976_SOLDEVILA_2"),
            "."
          ),
          tags$p(
            "La difusión continúa por canales cada vez más diversos. ",
            guide_link("La prensa de 1983","EV1983_LA_VANGUARDIA"),
            " reactiva la reivindicación Grau-Moctezuma; ",
            guide_link("las referencias de los años noventa","EV1996_1997_PICARESCA"),
            " la sitúan también en registros de picaresca y memoria contemporánea; y, ya en el siglo XXI, la historia entra plenamente en la literatura, la divulgación, las fiestas y los medios."
          ),
          tags$p(
            "Entre estas reelaboraciones se encuentran ",
            guide_link("La serpent de plomes, de Núria Pradas (2002)","EV2003_PRADAS"),
            ", ",
            guide_link("el pregón local de 2003","EV2003_MAS_MIRO"),
            ", ",
            guide_link("Medina Monzón","EV2006_MEDINA_MONZON"),
            ", ",
            guide_link("el Moctezuma Fest","EV2007_2010_MOCTEZUMA_FEST"),
            ", ",
            guide_link("«El secreto catalán de Moctezuma»","EV2008_SOLER"),
            ", ",
            guide_link("el reportaje de Sàpiens","EV2010_REGO"),
            ", ",
            guide_link("el recuerdo de Ventura Pons","EV2011_PONS"),
            " y ",
            guide_link("la difusión digital de Renom","EV2012_RENOM"),
            "."
          ),
          tags$p(
            "En años recientes, ",
            guide_link("A cavall del vent","EV2019_TUBELLA"),
            ", ",
            guide_link("el documental Toloriu","EV2019_ORDAZ"),
            " y ",
            guide_link("nuevas recreaciones literarias","EV2023_CONTRERAS"),
            " muestran la continuidad de una narración que ya funciona simultáneamente como leyenda local, recurso cultural y relato identitario."
          ),
          tags$p("La reiteración del relato a lo largo del tiempo, tanto en publicaciones que lo presentan como un hecho histórico como en otras que cuestionan su veracidad, puede haber contribuido a consolidar su presencia en la memoria colectiva. Esta persistencia cultural no constituye, sin embargo, una prueba de la autenticidad histórica de los episodios narrados."),
          tags$p(
            "La secuencia permite observar un proceso de transformación: ",
            tags$strong("reivindicación genealógica → territorialización → memoria local → difusión mediática → patrimonialización y recreación cultural"),
            "."
          )
        )
      ),

      nav_panel(
        "Crítica / impugnación",
        guide_section(
          guide_figure("assets/esglesia_toloriu.png", alt="Miniatura decorativa: Crítica / impugnación", mode="side"),
          tags$p(
            "La crítica no es una reacción tardía: ya en 1952 convive con las primeras formulaciones públicas de la genealogía. Conviene distinguir la impugnación periodística de aquel año de las comprobaciones documentales posteriores, especialmente las de Amada López de Meneses en 1955 y 1960. Las fuentes críticas también participan en la circulación del relato, sin que ello invalide sus objeciones ni demuestre los episodios legendarios."
          ),
          tags$p(
            guide_link("El 5 de abril de 1952 Enrique Rubio publica «El falso príncipe de Moctezuma, confiesa»","EV1952_04_05_RUBIO_CONFESSA"),
            ". El artículo atribuye a Guillermo Grau Rifé una confesión sobre la falsedad de la pretensión principesca y cuestiona también la figura de Juan de Grau. Su valor es especialmente relevante porque muestra que algunos elementos de la genealogía ya estaban siendo discutidos mientras todavía se estaban fijando públicamente."
          ),
          tags$p(
            "La crítica adquiere después una dimensión genealógica académica con Amada López de Meneses. ",
            guide_link("Su estudio de 1955","EV1955_LOPEZ_MENESES"),
            " señala que no aparece prueba documental del «Juan de Grau» invocado por la genealogía. ",
            guide_link("En 1960","EV1960_LOPEZ_MENESES"),
            ", su reconstrucción de María de Moctezuma la sitúa en Nueva España, soltera y sin hijos, una trayectoria incompatible con el matrimonio y la descendencia pirenaica atribuidos por la leyenda."
          ),
          tags$p(
            "La revisión crítica continúa posteriormente desde perspectivas diferentes. ",
            guide_link("Josep Xicota","EV1992_XICOTA"),
            ", ",
            guide_link("Rosalia Pantebre","EV2001_PANTEBRE"),
            " y ",
            guide_link("Albert Villaró","EV2009_VILLARO"),
            " permiten observar cómo la tradición es examinada, matizada o confrontada con la documentación y con la biografía conocida de sus promotores."
          ),
          tags$p(
            "Estas fuentes no tienen todas el mismo carácter ni el mismo peso probatorio. Su interés conjunto reside en demostrar que la recepción de la leyenda nunca fue completamente acrítica: ",
            tags$strong("la afirmación y la impugnación han acompañado al relato desde su etapa de cristalización.")
          )
        )
      ),

      nav_panel(
        "Contraste",
        guide_section(
          guide_figure("assets/archival_books_capture_v3.png", alt="Miniatura decorativa: Contraste", mode="side"),
          tags$p(
            "El contraste documental diferencia tres situaciones: hechos históricos acreditados, afirmaciones de la leyenda que carecen de respaldo independiente y afirmaciones que entran en conflicto con fuentes concretas. Sabemos que hubo desplazamientos indígenas a la Península durante el siglo XVI; ese contexto no identifica a Xipahuatzin ni documenta su matrimonio, descendencia o residencia en Toloriu."
          ),
          tags$p(
            guide_link("Isabel Tecuichpotzin Moctezuma y Alonso de Grado","EV1526_ISABEL_MOCTEZUMA_ALONSO_GRADO"),
            " constituyen un ejemplo particularmente útil porque combinan personajes históricos reales y una relación documentalmente rastreable. También pueden seguirse las trayectorias de ",
            guide_link("Martín Cortés Moctezuma","EV1525_MARTIN_CORTES_MOCTEZUMA_ESPANYA"),
            ", ",
            guide_link("Pedro de Moctezuma","EV1540_PEDRO_MOCTEZUMA_ESPANYA"),
            " y ",
            guide_link("Pedro de Toledo Moctezuma","EV1589_PEDRO_TOLEDO_MOCTEZUMA_TALAVERA"),
            ", cuyas vidas generan referencias nominales, administrativas, familiares o residenciales."
          ),
          tags$p(
            "Frente a estas trayectorias, ",
            guide_link("María de Moctezuma aparece documentada en Nueva España","EV1537_MARIA_MOCTEZUMA_NUEVA_ESPANA"),
            ", sin que la documentación examinada permita reconstruir el desplazamiento a Toloriu, el matrimonio con Joan de Grau y la descendencia que le atribuye la leyenda."
          ),
          tags$p(
            "La síntesis de este contraste se recoge en cuatro fichas negativas o de ausencia documental: ",
            guide_link("Xipahuatzin no identificada documentalmente","NEG_XIPAHUATZIN_NO_DOCUMENTADA"),
            ", ",
            guide_link("Joan de Grau no documentado","NEG_JOAN_GRAU_NO_DOCUMENTAT"),
            ", ",
            guide_link("ausencia de una vinculación Toloriu–Moctezuma en el siglo XVI","NEG_TOLORIU_SEGLEXVI_NO_VINCULACIO"),
            " y ",
            guide_link("ausencia de la cadena biográfica completa","NEG_CADENA_BIOGRAFICA_NO_DOCUMENTADA"),
            "."
          ),
          tags$p(
            "Las revisiones contemporáneas, como ",
            guide_link("la aportación de Carles Gascón","EV2024_GASCON"),
            " o ",
            guide_link("la lectura de Corominas i Planes","EV2025_COROMINAS_PLANES"),
            ", muestran que esta confrontación entre relato y documentación sigue abierta. El objetivo del contraste no es sustituir una leyenda por otra narración cerrada, sino hacer visible ",
            tags$strong("qué elementos cuentan con evidencia independiente, cuáles dependen de fuentes anteriores y cuáles continúan sin demostración documental.")
          )
        )
      )
    )

  } else {

    navset_card_tab(

      nav_panel(
        "La llegenda",
        guide_section(
          guide_figure("assets/placa_toloriu.png", alt="Miniatura decorativa: La llegenda", mode="side"),
          
          tags$p(
            "Una tradició contemporània vincula Toloriu, al peu del Cadí, amb una suposada filla de Moctezuma II coneguda habitualment com ",
            tags$strong("Xipahuatzin"),
            ", Xipaguazín o María de Moctezuma. Segons la versió que acabà consolidant-se, la princesa hauria arribat a la Península després de la conquesta de Mèxic, s’hauria unit o casat amb un cavaller anomenat Joan o Juan de Grau, hauria viscut a Toloriu i hi hauria deixat descendència. Algunes versions situen també la seva mort i sepultura al poble."
          ),
          tags$p(
            "Aquesta narració, però, no apareix com un relat únic i estable des del segle XVI. Els seus components es documenten progressivament durant el segle XX. L’any 1952 constitueix un moment especialment important: ",
            guide_link("Enrique Rubio recull al març la figura d’una «princesa María de Moctezuma»","EV1952_03_16_RUBIO"),
            ", i poques setmanes després ",
            guide_link("Manuel Reyes publica una genealogia que uneix María/Chipahuatzin amb Juan de Grau","EV1952_05_24_REYES"),
            " i els atribueix descendència."
          ),
          tags$p(
            "En aquest procés, ",
            guide_link("els textos vinculats a Joaquín del Álamo","EV1952_DEL_ALAMO"),
            " contribueixen a donar forma genealògica i territorial al relat i a relacionar-lo explícitament amb Toloriu. La materialització pública d’aquesta associació es fa especialment visible amb ",
            guide_link("la placa col·locada a l’església de Toloriu l’any 1963","EV1963_PLACA_TOLORIU"),
            ", que presenta María Xipaguazín Moctezuma com una figura històrica vinculada al poble."
          ),
          tags$p(
            "A partir d’aleshores, llibres, articles, testimonis i activitats culturals amplien el relat. Els motius de la princesa, el cavaller, la descendència, la sepultura i el tresor s’integren retrospectivament en una història aparentment contínua. La difusió d’aquesta història permet estudiar-ne l’arrelament cultural, però no converteix els episodis atribuïts al segle XVI en fets documentats."
          ),
          tags$p(
            tags$strong("La pregunta documental que guia aquest arxiu és, per tant: "),
            "quan apareix cada element, quina font el documenta, de quines fonts anteriors depèn i en quin moment els diferents components queden units en la llegenda que coneixem avui?"
          )
        )
      ),

      nav_panel(
        "Context",
        guide_section(
          guide_figure("assets/indi.png", alt="Miniatura decorativa: Context", mode="side"),
          
          tags$p(
            "El context històric obliga a distingir entre possibilitat general i trajectòria individual demostrada. La presència i mobilitat de persones indígenes entre Amèrica i la Península durant el segle XVI està documentada i forma part del marc atlàntic de l’època. ",
            guide_link("Les notícies sobre indígenes traslladats per Hernán Cortés a la Península","EV1528_CORTES_INDIGENES_PENINSULA"),
            " mostren que aquests desplaçaments existiren. Aquesta realitat, però, no demostra per si mateixa el viatge de Xipahuatzin ni la seva presència a Toloriu."
          ),
          tags$p(
            "També existeixen elements històrics locals que pogueren afavorir posteriorment l’ancoratge territorial de la llegenda. ",
            guide_link("El copó medieval relacionat amb Ramon Grau","EV1390_COPO_RAMON_GRAU"),
            " documenta un Grau real en un context històric molt anterior, mentre que ",
            guide_link("el dossier sobre el cognom Grau a Toloriu","DS_GRAU_TOLORIU"),
            " reuneix evidències de la presència efectiva del cognom al poble."
          ),
          tags$p(
            "Entre aquestes evidències modernes hi ha ",
            guide_link("Marie Grau, nascuda a Toloriu el 1930","EV1930_MARIE_GRAU_TOLORIU"),
            ", ",
            guide_link("la família Grau i Grau de Casa Miret","EV2026_GRAU_CASA_MIRET"),
            " i ",
            guide_link("Maria Grau Blanch","EV2026_MARIA_GRAU_BLANCH"),
            ". Són dades rellevants per reconstruir la història local del cognom, però no estableixen una filiació amb el Joan de Grau de la llegenda ni amb Guillem Grau Rifé."
          ),
          tags$p(
            "El context contemporani és igualment important. ",
            guide_link("La documentació de 1939 sobre Guillem Grau Rifé","EV1939_GRAU_MATRIMONI"),
            " identifica Guillermo de Grau Rifé com a natural de Barcelona, fill de Guillermo de Grau Iscla i Mercedes Rifé Brosa. Aquesta filiació documentada no acredita cap ascendència de Moctezuma ni cap relació genealògica amb Toloriu. Posteriorment, ",
            guide_link("l’activitat del cercle de Charles Grando entre 1956 i 1961","EV1956_1961_GRANDO"),
            " i ",
            guide_link("la documentació publicada al BOE l’any 1960","EV1960_BOE"),
            " mostren que la reivindicació Grau-Moctezuma adquirí també formes institucionals o pseudo-institucionals."
          ),
          tags$p(
            "Aquest apartat reuneix, per tant, fets i persones que ajuden a entendre el terreny històric sobre el qual pogué construir-se la narració, sense convertir automàticament aquests elements reals en proves de la genealogia llegendària."
          )
        )
      ),

      nav_panel(
        "Reivindicació",
        guide_section(
          guide_figure("assets/escut.png", alt="Miniatura decorativa: Reivindicació Grau-Moctezuma", mode="side"),
          tags$p(
            "La llegenda de Xipahuatzin a Toloriu no es pot entendre aïlladament d’una reivindicació més àmplia articulada al voltant de Guillem o Guillermo Grau Rifé. La documentació contemporània permet observar, abans de la fixació territorial de la història, una pretensió nobiliària i successòria que vinculava el cognom Grau amb Moctezuma. L’existència i la circulació d’aquesta pretensió no són proves de la genealogia al·legada."
          ),
          tags$p(
            guide_link("La documentació matrimonial de 1939","EV1939_GRAU_MATRIMONI"),
            " constitueix un antecedent biogràfic important. El febrer de 1952, ",
            guide_link("«Yo, el príncipe, en el calabozo» (26-02-1952)","EV1952_02_20_SOLIDARIDAD"),
            " mostra que la pretensió relacionada amb l’anomenada «Corona de Méjico» ja havia adquirit presència pública. Poc després, ",
            guide_link("la premsa recull també el caràcter polèmic d’aquesta reivindicació","EV1952_02_24_DIARIO_BURGOS"),
            "."
          ),
          tags$p(
            "Durant la primavera de 1952 la reivindicació es transforma ràpidament en una construcció genealògica més definida. ",
            guide_link("Enrique Rubio","EV1952_03_16_RUBIO"),
            " incorpora la figura d’una princesa María de Moctezuma; ",
            guide_link("el 5 d’abril publica una suposada confessió de Grau Rifé i qüestiona la figura de Juan de Grau","EV1952_04_05_RUBIO_CONFESSA"),
            "; i ",
            guide_link("Manuel Reyes, el 24 de maig","EV1952_05_24_REYES"),
            ", presenta ja una genealogia que enllaça explícitament María/Chipahuatzin amb Juan de Grau i una descendència posterior."
          ),
          tags$p(
            guide_link("Els materials associats a Joaquín del Álamo","EV1952_DEL_ALAMO"),
            " aprofundeixen aquesta construcció i contribueixen a vincular-la amb Toloriu. Els anys següents, ",
            guide_link("l’entorn de Charles Grando","EV1956_1961_GRANDO"),
            " li proporciona una dimensió pseudo-institucional, mentre que ",
            guide_link("la documentació del BOE de 1960","EV1960_BOE"),
            " permet seguir la presència pública de denominacions i pretensions relacionades amb Grau-Moctezuma."
          ),
          tags$p(
            "Dècades després, ",
            guide_link("la premsa de 1983","EV1983_LA_VANGUARDIA"),
            " mostra la persistència o reaparició pública de la «Casa de Grau-Moctezuma». Vista en conjunt, aquesta seqüència permet distingir una primera fase de ",
            tags$strong("reivindicació dinàstica i genealògica"),
            " de la posterior conversió d’alguns dels seus elements en una llegenda històrica i territorial centrada en Toloriu."
          )
        )
      ),

      nav_panel(
        "Control documental / absència",
        guide_section(
          guide_figure("assets/llibres.png", alt="Miniatura decorativa: Control documental / absència", mode="side"),
          tags$p(
            "El silenci documental exigeix valorar què investigava cada font, quin territori descrivia i quins elements acostumava a registrar. L’absència d’una notícia en una obra aïllada no demostra que la tradició no existís; la coincidència de diversos testimonis pertinents permet, en canvi, delimitar quin relat no hem pogut documentar abans de mitjan segle XX."
          ),
          tags$p(
            guide_link("Artur Osona descriu Toloriu l’any 1899","EV1899_OSONA"),
            " sense recollir-hi una trama mexicana associada al poble. ",
            guide_link("Cèsar August Torras, el 1924","EV1924_TORRAS"),
            ", tracta Toloriu i el Querforadat sense mencionar Xipahuatzin, Joan de Grau ni una connexió amb Moctezuma. ",
            guide_link("Bonet i Isart, el 1935","EV1935_BONET_ISART_CONTROL"),
            ", aporta un control territorial complementari."
          ),
          tags$p(
            "Els repertoris de Joan Amades ofereixen una altra perspectiva. ",
            guide_link("El Pirineu","EV1949_AMADES"),
            " i ",
            guide_link("el Costumari català","EV1950_1956_AMADES_COSTUMARI"),
            " no proporcionen, en les cerques realitzades, una formulació individualitzada de la història Xipahuatzin–Grau–Toloriu anterior a la irrupció pública dels anys cinquanta."
          ),
          tags$p(
            "Com a control posterior a la difusió pública de la llegenda, l’obra de Xavier Fàbregas ",
            tags$em("Tradicions, mites i creences dels catalans"),
            " (1979) ofereix un punt de comparació complementari. Malgrat la seva atenció a les tradicions i llegendes catalanes, no recull la història de Xipahuatzin i Toloriu. Aquesta absència, posterior a les publicacions de 1952 i a la placa de 1963, no permet determinar fins a quin punt el relat era conegut fora de l’àmbit local."
          ),
          tags$p(
            tags$strong("La conclusió s’ha de formular amb precisió: "),
            "amb les fonts examinades fins ara, no s’ha localitzat una formulació documental anterior a 1952 de la cadena completa Xipahuatzin–Grau–Toloriu. Això no equival a demostrar-ne una inexistència absoluta, sinó a establir el límit que permet l’evidència disponible."
          )
        )
      ),

      nav_panel(
        "Difusió i fixació",
        guide_section(
          guide_figure("assets/paisatge_muntanya.png", alt="Miniatura decorativa: Difusió i fixació", mode="side"),
          
          tags$p(
            "A partir de les publicacions de 1952, la narració s’estén més enllà del context de la reclamació Grau-Moctezuma. Toloriu adquireix una posició central i el relat circula mitjançant plaques, publicacions, celebracions, literatura i mitjans audiovisuals. Aquesta circulació s’ha d’estudiar com a història de la recepció, no com a corroboració retrospectiva dels fets narrats."
          ),
          tags$p(
            "Una fita especialment visible és ",
            guide_link("la placa instal·lada a Toloriu l’any 1963","EV1963_PLACA_TOLORIU"),
            ", que fixa materialment en el paisatge local l’associació entre María Xipaguazín Moctezuma i el poble. Posteriorment, la història circula en textos i reconstruccions d’àmbit pirinenc i català, entre els quals ",
            guide_link("Esteve Albert","EV1974_ESTEVE_ALBERT"),
            ", ",
            guide_link("noves elaboracions d’Esteve Albert","EV1982_ESTEVE_ALBERT"),
            " i ",
            guide_link("les publicacions de mossèn Soldevila","EV1976_SOLDEVILA_1"),
            " ",
            guide_link("sobre la tradició","EV1976_SOLDEVILA_2"),
            "."
          ),
          tags$p(
            "La difusió continua per canals cada vegada més diversos. ",
            guide_link("La premsa de 1983","EV1983_LA_VANGUARDIA"),
            " reactiva la reivindicació Grau-Moctezuma; ",
            guide_link("les referències dels anys noranta","EV1996_1997_PICARESCA"),
            " la situen també en registres de picaresca i memòria contemporània; i, ja al segle XXI, la història entra plenament en la literatura, la divulgació, les festes i els mitjans."
          ),
          tags$p(
            "Entre aquestes reelaboracions hi ha ",
            guide_link("La serpent de plomes, de Núria Pradas (2002)","EV2003_PRADAS"),
            ", ",
            guide_link("el pregó local de 2003","EV2003_MAS_MIRO"),
            ", ",
            guide_link("Medina Monzón","EV2006_MEDINA_MONZON"),
            ", ",
            guide_link("el Moctezuma Fest","EV2007_2010_MOCTEZUMA_FEST"),
            ", ",
            guide_link("«El secreto catalán de Moctezuma»","EV2008_SOLER"),
            ", ",
            guide_link("el reportatge de Sàpiens","EV2010_REGO"),
            ", ",
            guide_link("el record de Ventura Pons","EV2011_PONS"),
            " i ",
            guide_link("la difusió digital de Renom","EV2012_RENOM"),
            "."
          ),
          tags$p(
            "En anys recents, ",
            guide_link("A cavall del vent","EV2019_TUBELLA"),
            ", ",
            guide_link("el documental Toloriu","EV2019_ORDAZ"),
            " i ",
            guide_link("noves recreacions literàries","EV2023_CONTRERAS"),
            " mostren la continuïtat d’una narració que ja funciona simultàniament com a llegenda local, recurs cultural i relat identitari."
          ),
          tags$p("La reiteració del relat al llarg del temps, tant en publicacions que el presenten com un fet històric com en altres que en qüestionen la veracitat, pot haver contribuït a consolidar-ne la presència en la memòria col·lectiva. Aquesta persistència cultural no constitueix, però, una prova de l’autenticitat històrica dels episodis narrats."),
          tags$p(
            "La seqüència permet observar un procés de transformació: ",
            tags$strong("reivindicació genealògica → territorialització → memòria local → difusió mediàtica → patrimonialització i recreació cultural"),
            "."
          )
        )
      ),

      nav_panel(
        "Crítica / impugnació",
        guide_section(
          guide_figure("assets/esglesia_toloriu.png", alt="Miniatura decorativa: Crítica / impugnació", mode="side"),
          tags$p(
            "La crítica no és una reacció tardana: ja el 1952 conviu amb les primeres formulacions públiques de la genealogia. Cal distingir la impugnació periodística d’aquell any de les comprovacions documentals posteriors, especialment les d’Amada López de Meneses el 1955 i el 1960. Les fonts crítiques també participen en la circulació del relat, sense que això invalidi les seves objeccions ni demostri els episodis llegendaris."
          ),
          tags$p(
            guide_link("El 5 d’abril de 1952 Enrique Rubio publica «El falso príncipe de Moctezuma, confiesa»","EV1952_04_05_RUBIO_CONFESSA"),
            ". L’article atribueix a Guillermo Grau Rifé una confessió sobre la falsedat de la pretensió principesca i qüestiona també la figura de Juan de Grau. El seu valor és especialment rellevant perquè mostra que alguns elements de la genealogia ja estaven sent discutits mentre encara s’estaven fixant públicament."
          ),
          tags$p(
            "La crítica adquireix després una dimensió genealògica acadèmica amb Amada López de Meneses. ",
            guide_link("El seu estudi de 1955","EV1955_LOPEZ_MENESES"),
            " assenyala que no apareix prova documental del «Juan de Grau» invocat per la genealogia. ",
            guide_link("L’any 1960","EV1960_LOPEZ_MENESES"),
            ", la seva reconstrucció de María de Moctezuma la situa a Nova Espanya, soltera i sense fills, una trajectòria incompatible amb el matrimoni i la descendència pirinenca atribuïts per la llegenda."
          ),
          tags$p(
            "La revisió crítica continua posteriorment des de perspectives diferents. ",
            guide_link("Josep Xicota","EV1992_XICOTA"),
            ", ",
            guide_link("Rosalia Pantebre","EV2001_PANTEBRE"),
            " i ",
            guide_link("Albert Villaró","EV2009_VILLARO"),
            " permeten observar com la tradició és examinada, matisada o confrontada amb la documentació i amb la biografia coneguda dels seus promotors."
          ),
          tags$p(
            "Aquestes fonts no tenen totes el mateix caràcter ni el mateix pes probatori. El seu interès conjunt és mostrar que la recepció de la llegenda mai no fou completament acrítica: ",
            tags$strong("l’afirmació i la impugnació han acompanyat el relat des de la seva etapa de cristal·lització.")
          )
        )
      ),

      nav_panel(
        "Contrast",
        guide_section(
          guide_figure("assets/archival_books_capture_v3.png", alt="Miniatura decorativa: Contrast", mode="side"),
          tags$p(
            "El contrast documental diferencia tres situacions: fets històrics acreditats, afirmacions de la llegenda mancades de suport independent i afirmacions que entren en conflicte amb fonts concretes. Sabem que hi hagué desplaçaments indígenes a la Península durant el segle XVI; aquest context no identifica Xipahuatzin ni documenta el seu matrimoni, descendència o residència a Toloriu."
          ),
          tags$p(
            guide_link("Isabel Tecuichpotzin Moctezuma i Alonso de Grado","EV1526_ISABEL_MOCTEZUMA_ALONSO_GRADO"),
            " constitueixen un exemple particularment útil perquè combinen personatges històrics reals i una relació documentalment rastrejable. També es poden seguir les trajectòries de ",
            guide_link("Martín Cortés Moctezuma","EV1525_MARTIN_CORTES_MOCTEZUMA_ESPANYA"),
            ", ",
            guide_link("Pedro de Moctezuma","EV1540_PEDRO_MOCTEZUMA_ESPANYA"),
            " i ",
            guide_link("Pedro de Toledo Moctezuma","EV1589_PEDRO_TOLEDO_MOCTEZUMA_TALAVERA"),
            ", les vides dels quals generen referències nominals, administratives, familiars o residencials."
          ),
          tags$p(
            "Davant d’aquestes trajectòries, ",
            guide_link("María de Moctezuma apareix documentada a Nova Espanya","EV1537_MARIA_MOCTEZUMA_NUEVA_ESPANA"),
            ", sense que la documentació examinada permeti reconstruir el desplaçament a Toloriu, el matrimoni amb Joan de Grau i la descendència que li atribueix la llegenda."
          ),
          tags$p(
            "La síntesi d’aquest contrast es recull en quatre fitxes negatives o d’absència documental: ",
            guide_link("Xipahuatzin no identificada documentalment","NEG_XIPAHUATZIN_NO_DOCUMENTADA"),
            ", ",
            guide_link("Joan de Grau no documentat","NEG_JOAN_GRAU_NO_DOCUMENTAT"),
            ", ",
            guide_link("absència d’una vinculació Toloriu–Moctezuma al segle XVI","NEG_TOLORIU_SEGLEXVI_NO_VINCULACIO"),
            " i ",
            guide_link("absència de la cadena biogràfica completa","NEG_CADENA_BIOGRAFICA_NO_DOCUMENTADA"),
            "."
          ),
          tags$p(
            "Les revisions contemporànies, com ",
            guide_link("l’aportació de Carles Gascón","EV2024_GASCON"),
            " o ",
            guide_link("la lectura de Corominas i Planes","EV2025_COROMINAS_PLANES"),
            ", mostren que aquesta confrontació entre relat i documentació continua oberta. L’objectiu del contrast no és substituir una llegenda per una altra narració tancada, sinó fer visible ",
            tags$strong("quins elements disposen d’evidència independent, quins depenen de fonts anteriors i quins continuen sense demostració documental.")
          )
        )
      )
    )
  }
}
