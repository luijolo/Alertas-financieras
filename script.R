# Lectura limpia e independiente del JSON de estado
  old_state <- list()
  if (file.exists(state_file) && file.info(state_file)$size > 2) {
    tryCatch({ 
      old_state <- fromJSON(state_file, simplifyVector = TRUE)
    }, error = function(e) {
      cat("Aviso: No se pudo parsear estado_hashes.json:", e$message, "\n")
    })
  }
  new_state <- old_state

  fecha_hoy <- format(Sys.Date(), "%d/%m/%Y")
  new_state[["ultima_ejecucion"]] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S AST")

  # ==========================================
  # 1. FIDUCIARIA RESERVAS (Vía Headless Chrome)
  # ==========================================
  cat("\n==========================================\n")
  cat("1. PROCESANDO FIDUCIARIA RESERVAS (Headless)\n")
  cat("==========================================\n")
  
  Sys.setenv(CHROMOTE_EXTRA_ARGS = "--no-sandbox --disable-dev-shm-usage")
  
  url_fid <- paste0("https://www.fiduciariareservas.com/proyectos-oferta-publica/fideicomiso-de-oferta-publica-de-valores-multiplaza-fr-n02/?nocache=", as.numeric(Sys.time()))
  
  val_fid <- NA
  val_raw <- NA
  
  tryCatch({
    cat("Iniciando navegador Chrome headless...\n")
    b <- chromote::ChromoteSession$new()
    
    cat("Navegando a Fiduciaria Reservas...\n")
    b$Page$navigate(url_fid)
    
    Sys.sleep(8)
    
    doc <- b$Runtime$evaluate("document.documentElement.outerHTML")
    html_raw_fid <- doc$result$value
    b$close()
    
    html_obj <- read_html(html_raw_fid)
    
    nodo_valor <- html_node(
      html_obj, 
      xpath = "//tr[td[1][contains(normalize-space(), 'Valor patrimonial de los valores')]]/td[2]"
    )
    
    if (!is.null(nodo_valor) && !is.na(nodo_valor)) {
      val_raw <- html_text(nodo_valor, trim = TRUE)
    }
    
    if (is.na(val_raw) || nchar(val_raw) == 0) {
      texto_pagina <- html_text2(html_obj)
      patron_estricto <- "(?i)Valor patrimonial de los valores[^0-9]*([0-9]{1,3}(?:,[0-9]{3})*\\.[0-9]{2,6})"
      coincidencia <- str_match(texto_pagina, patron_estricto)
      
      if (!is.na(coincidencia[1, 2])) {
        val_raw <- coincidencia[1, 2]
      }
    }
    
    if (!is.na(val_raw)) {
      val_fid <- parse_number_dr(val_raw)
    }
    
    fop_multi_env <- Sys.getenv("FOP_GR")
    fop_multi <- suppressWarnings(as.numeric(fop_multi_env))
    if (is.na(fop_multi)) fop_multi <- 20
    
    if (!is.na(val_fid)) {
      val_fid <- round(val_fid, 6)
      
      # Conversión limpia y segura desde el JSON previo
      prev_raw <- old_state[["multiplaza_valor"]]
      prev_num <- suppressWarnings(as.numeric(as.character(prev_raw[1])))
      
      cat("\n--- DIAGNÓSTICO FIDUCIARIA ---\n")
      cat("Valor leído en JSON previo:", prev_num, "\n")
      cat("Valor nuevo scrapeado:", val_fid, "\n")
      
      hubo_cambio <- FALSE
      if (is.na(prev_num)) {
        cat("Resultado: Primera ejecución o JSON previo no encontrado. Se enviará alerta.\n")
        hubo_cambio <- TRUE
      } else {
        diferencia <- abs(val_fid - prev_num)
        cat("Diferencia calculada:", diferencia, "\n")
        if (diferencia > 1e-4) {
          hubo_cambio <- TRUE
        } else {
          cat("Resultado: El valor no ha cambiado. Se omite envío de Telegram.\n")
        }
      }
      cat("------------------------------\n\n")
      
      if (hubo_cambio) {
        inv_fid <- val_fid * fop_multi
        msg_fid <- paste0(
          fecha_hoy, "\n",
          "Valor cuota FOP Multiplaza RD$ ", format(val_fid, nsmall = 6), "\n",
          "Valor inversión RD$ ", format(inv_fid, big.mark = ",", nsmall = 2)
        )
        send_telegram(msg_fid)
        cat("Notificación enviada a Telegram para Fiduciaria Reservas.\n")
      }
      
      new_state[["multiplaza_valor"]] <- val_fid 
      save_state(new_state, state_file)
      
    } else {
      cat("ADVERTENCIA: No se pudo extraer un valor válido de Fiduciaria Reservas.\n")
    }
    
  }, error = function(e) {
    cat("Error al ejecutar Chrome Headless:", e$message, "\n")
  })
