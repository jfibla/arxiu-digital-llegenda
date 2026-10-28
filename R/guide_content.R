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

guide_section <- function(...){
  tags$div(
    class="guide-section",
    ...
  )
}

guide_figure <- function(src,alt="",caption=NULL,mode="full"){
  if(identical(mode,"side")){
    tags$figure(
      style="width:320px;max-width:42%;float:right;margin:0 0 1rem 1.25rem;",
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
          guide_figure(
            "assets/guia_placa_toloriu.jpg",
            alt="Placa de Toloriu",
            caption="Placa instalada en la iglesia de Toloriu (1963), uno de los hitos principales de la fijación pública de la leyenda.",
            mode="side"
          ),
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
            "A partir de entonces, libros, artículos, testimonios, literatura e iniciativas culturales amplían y transforman la narración. La leyenda acaba funcionando como una historia coherente porque elementos de orígenes distintos —Moctezuma, una princesa mexicana, un Grau, Toloriu, una descendencia y una tumba— quedan unidos retrospectivamente en un mismo relato."
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
          guide_figure(
            "assets/cadi_presentacio.jpg",
            alt="Toloriu y el Cadí",
            caption="Toloriu y el Cadí. Fotografía de portada del archivo digital."
          ),
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
            " permite situar al personaje antes de la cristalización pública de 1952. Posteriormente, ",
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
        "Reivindicación Grau-Moctezuma",
        guide_section(
          tags$p(
            "La leyenda de Xipahuatzin en Toloriu no puede entenderse aisladamente de una reivindicación más amplia articulada alrededor de Guillem o Guillermo Grau Rifé. Antes de que la historia quedase plenamente territorializada en Toloriu, aparece una pretensión nobiliaria, dinástica y genealógica que vinculaba el apellido Grau con la descendencia de Moctezuma."
          ),
          tags$p(
            guide_link("La documentación matrimonial de 1939","EV1939_GRAU_MATRIMONI"),
            " constituye un antecedente biográfico importante. En febrero de 1952, ",
            guide_link("«Yo, el príncipe, en el calabozo»","EV1952_02_20_SOLIDARIDAD"),
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
          tags$p(
            "Las ausencias documentales deben utilizarse con prudencia. El silencio de una fuente concreta no demuestra que una tradición no existiera, pero una serie de fuentes independientes, próximas al territorio y adecuadas para registrar lugares, acontecimientos o tradiciones permite establecer límites documentales razonables."
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
            "El control documental no se limita a fuentes antiguas. ",
            guide_link("La revisión completa del número 6 de Nuestro Tiempo de 1952","EV1952_07_NUESTRO_TIEMPO_CONTROL"),
            " permite descartar una atribución retrospectiva concreta: el número examinado no contiene el relato que posteriormente se le había asociado."
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
          guide_figure(
            "assets/guia_esglesia_toloriu.jpg",
            alt="Iglesia de Toloriu",
            caption="Iglesia de Toloriu, lugar de emplazamiento de la placa conmemorativa vinculada a la leyenda.",
            mode="side"
          ),
          tags$p(
            "Una vez formulados sus componentes principales durante el ciclo de 1952, la historia comienza a adquirir autonomía respecto de la reivindicación dinástica que la había impulsado. Toloriu se convierte progresivamente en el escenario central y el relato pasa de la polémica genealógica a la memoria local, la divulgación y la cultura popular."
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
            guide_link("La serp de plomes, de Núria Pradas","EV2003_PRADAS"),
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
          tags$p(
            "La crítica de la genealogía no aparece al final del proceso, sino prácticamente al mismo tiempo que su formulación pública. La construcción y la impugnación del relato conviven desde 1952."
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
          tags$p(
            "El contraste documental consiste en comparar la cadena biográfica de la leyenda con trayectorias del siglo XVI que sí dejan rastros identificables. No pretende demostrar que todo desplazamiento indígena a la Península fuera imposible —sabemos que ocurrió—, sino preguntar qué documentación cabría esperar cuando una persona o un linaje dejan una huella histórica verificable."
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
          guide_figure(
            "assets/guia_placa_toloriu.jpg",
            alt="Placa de Toloriu",
            caption="Placa instal·lada a l’església de Toloriu (1963), una de les fites principals de la fixació pública de la llegenda.",
            mode="side"
          ),
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
            "A partir d’aquí, llibres, articles, testimonis, literatura i iniciatives culturals amplien i transformen la narració. La llegenda acaba funcionant com una història coherent perquè elements d’orígens diferents —Moctezuma, una princesa mexicana, un Grau, Toloriu, una descendència i una tomba— queden units retrospectivament en un mateix relat."
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
          guide_figure(
            "assets/cadi_presentacio.jpg",
            alt="Toloriu i el Cadí",
            caption="Toloriu i el Cadí. Fotografia de portada de l’arxiu digital."
          ),
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
            " permet situar el personatge abans de la cristal·lització pública de 1952. Posteriorment, ",
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
        "Reivindicació Grau-Moctezuma",
        guide_section(
          tags$p(
            "La llegenda de Xipahuatzin a Toloriu no es pot entendre aïlladament d’una reivindicació més àmplia articulada al voltant de Guillem o Guillermo Grau Rifé. Abans que la història quedés plenament territorialitzada a Toloriu, apareix una pretensió nobiliària, dinàstica i genealògica que vinculava el cognom Grau amb la descendència de Moctezuma."
          ),
          tags$p(
            guide_link("La documentació matrimonial de 1939","EV1939_GRAU_MATRIMONI"),
            " constitueix un antecedent biogràfic important. El febrer de 1952, ",
            guide_link("«Yo, el príncipe, en el calabozo»","EV1952_02_20_SOLIDARIDAD"),
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
          tags$p(
            "Les absències documentals s’han d’utilitzar amb prudència. El silenci d’una font concreta no demostra que una tradició no existís, però una sèrie de fonts independents, pròximes al territori i adequades per registrar llocs, esdeveniments o tradicions permet establir límits documentals raonables."
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
            "El control documental no es limita a fonts antigues. ",
            guide_link("La revisió completa del número 6 de Nuestro Tiempo de 1952","EV1952_07_NUESTRO_TIEMPO_CONTROL"),
            " permet descartar una atribució retrospectiva concreta: el número examinat no conté el relat que posteriorment se li havia associat."
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
          guide_figure(
            "assets/guia_esglesia_toloriu.jpg",
            alt="Església de Toloriu",
            caption="Església de Toloriu, lloc on es troba la placa commemorativa vinculada a la llegenda.",
            mode="side"
          ),
          tags$p(
            "Un cop formulats els seus components principals durant el cicle de 1952, la història comença a adquirir autonomia respecte de la reivindicació dinàstica que l’havia impulsada. Toloriu es converteix progressivament en l’escenari central i el relat passa de la polèmica genealògica a la memòria local, la divulgació i la cultura popular."
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
            guide_link("La serp de plomes, de Núria Pradas","EV2003_PRADAS"),
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
          tags$p(
            "La crítica de la genealogia no apareix al final del procés, sinó pràcticament al mateix temps que la seva formulació pública. La construcció i la impugnació del relat conviuen des de 1952."
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
          tags$p(
            "El contrast documental consisteix a comparar la cadena biogràfica de la llegenda amb trajectòries del segle XVI que sí que deixen rastres identificables. No pretén demostrar que tot desplaçament indígena a la Península fos impossible —sabem que n’hi hagué—, sinó preguntar quina documentació és esperable quan una persona o un llinatge deixen una empremta històrica verificable."
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
