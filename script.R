library(httr)
library(rvest)
library(jsonlite)
library(stringr)

start_time <- Sys.time()
token <- Sys.getenv("TELEGRAM_TOKEN")
chat_id <- Sys.getenv("TELEGRAM_CHAT_ID")

send_telegram <- function(msg) {
  url <- paste0("https://api.telegram.org/bot", token, "/sendMessage")
  res <- POST(url, body = list(chat_id = chat_id, text = msg), encode = "form")
  if (status_code(res) != 200) {
    stop(paste("Telegram rechazó el mensaje con código", status_code(res)))
  }
}

parse_number_dr <- function(text) {
  if (is.na(text) || is.null(text) || nchar(text) == 0) return(NA)
  clean_text <- gsub("[^0-9.,]", "", text)
  clean_text <- gsub(",", "", clean_text)
  as.numeric(clean_text)
}
send_telegram("Prueba de conexión exitosa desde script.R 🚀")
ua <- user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")

tryCatch({
  state_file <- "estado_hashes.json"
  
  old_state <- list()
  if (file.exists(state_file) && file.info(state_file)$size > 2) {
    tryCatch({ old_state <- fromJSON(state_file) }, error = function(e) list())
  }
  new_state <- old_state

  fecha_hoy <- format(Sys.Date(), "%d/%m/%Y")

  # ==========================================
  # 1. FIDUCIARIA RESERVAS
  # ==========================================
  cat("\n--- 1. PROCESANDO FIDUCIARIA RESERVAS ---\n")
  url_fid <- "https://www.fiduciariareservas.com/proyectos-oferta-publica/fideicomiso-de-oferta-publica-de-valores-multiplaza-fr-n02/"
  res_fid <- GET(url_fid, ua)
  cat("Status Code:", status_code(res_fid), "\n")
  
  if (status_code(res_fid) == 200) {
    web_fid <- read_html(res_fid)
    
    # Selector CSS del elemento proporcionado
    css_fid <- "#quienes-somos table tr:nth-child(2) td:nth-child(2) em"
    node_fid <- html_element(web_fid, css_fid)
    
    # Fallback sin tag 'em' por si acaso
    if (is.na(html_text(node_fid))) {
      css_fid <- "#quienes-somos table tr:nth-child(2) td:nth-child(2)"
      node_fid <- html_element(web_fid, css_fid)
    }
    
    val_text <- html_text(node_fid, trim = TRUE)
    cat("Texto Fiduciaria:", val_text, "\n")
    
    val_fid <- parse_number_dr(val_text)
    cat("Valor numérico Fiduciaria:", val_fid, "\n")
    
    if (!is.na(val_fid)) {
      prev_fid <- old_state[["multiplaza_valor"]]
      
      if (!is.null(prev_fid) && val_fid != prev_fid) {
        inv_fid <- val_fid * 20
        msg_fid <- paste0(
          fecha_hoy, "\n",
          "Valor cuota FOP Multiplaza RD$", format(val_fid, nsmall = 6), "\n",
          "Valor inversión RD$", format(inv_fid, big.mark = ",", nsmall = 2)
        )
        send_telegram(msg_fid)
      }
      new_state[["multiplaza_valor"]] <- val_fid
    }
  }

  # ==========================================
  # 2. AFI UNIVERSAL
  # ==========================================
  cat("\n--- 2. PROCESANDO AFI UNIVERSAL ---\n")
  url_afi <- "https://www.afiuniversal.com.do/universal-liquidez/"
  res_afi <- GET(url_afi, ua)
  cat("Status Code:", status_code(res_afi), "\n")
  
  if (status_code(res_afi) == 200) {
    web_afi <- read_html(res_afi)
    
    fondos_afi <- list(
      list(key = "uni_liq", name = "Cuota Universal Liquidez", mult = 57, row = 11),
      list(key = "dep_flex", name = "Cuota Dep. Financiero Flexible", mult = 1, row = 8),
      list(key = "plazo_dol", name = "Cuota Plazo mensual dólar", mult = 1, row = 2)
    )

    for (f in fondos_afi) {
      # Probar selector con y sin tbody
      css_1 <- paste0("#cifras tr:nth-child(", f$row, ") td:nth-child(6)")
      css_2 <- paste0("#cifras tbody tr:nth-child(", f$row, ") td:nth-child(6)")
      
      node_val <- html_element(web_afi, css_1)
      if (is.na(html_text(node_val))) {
        node_val <- html_element(web_afi, css_2)
      }
      
      txt_val <- html_text(node_val, trim = TRUE)
      cat(f$name, "- Texto extraído:", txt_val, "\n")
      
      val_actual <- parse_number_dr(txt_val)
      cat(f$name, "- Número parseado:", val_actual, "\n")
      
      if (!is.na(val_actual)) {
        prev_val <- old_state[[f$key]]
        
        if (!is.null(prev_val) && val_actual != prev_val) {
          val_inv <- val_actual * f$mult
          msg_afi <- paste0(
            fecha_hoy, "\n",
            f$name, " ", format(val_actual, nsmall = 6), "\n",
            "Valor inversion ", format(val_inv, big.mark = ",", nsmall = 2)
          )
          send_telegram(msg_afi)
        }
        new_state[[f$key]] <- val_actual
      }
    }
  }

  # ==========================================
  # 3. CEVALDOM (OTC)
  # ==========================================
  cat("\n--- 3. PROCESANDO CEVALDOM ---\n")
  url_cev <- "https://www.cevaldom.com/mercado/otc/"
  res_cev <- GET(url_cev, ua)
  cat("Status Code:", status_code(res_cev), "\n")
  
  if (status_code(res_cev) == 200) {
    web_cev <- read_html(res_cev)
    filas_isin <- html_elements(web_cev, xpath = "//tr[contains(., 'DO9035100120')]")
    cat("Filas con ISIN encontradas:", length(filas_isin), "\n")
    
    if (length(filas_isin) > 0) {
      for (fila in filas_isin) {
        columnas <- html_elements(fila, "td") %>% html_text(trim = TRUE)
        cat("Columnas de la fila:", paste(columnas, collapse = " | "), "\n")
        
        if (length(columnas) >= 3) {
          hora_pacto <- columnas[1]
          precio_limpio <- columnas[length(columnas)]
          
          trade_id <- paste0("DO9035100120_", hora_pacto, "_", precio_limpio)
          trades_vistos <- old_state[["otc_trades_vistos"]]
          if (is.null(trades_vistos)) trades_vistos <- c()

          if (!(trade_id %in% trades_vistos)) {
            msg_otc <- paste0(
              "FOP Multiplaza OTC\n",
              "Hora pacto ", hora_pacto, "\n",
              "Precio limpio ", precio_limpio
            )
            send_telegram(msg_otc)
            trades_vistos <- c(trades_vistos, trade_id)
            new_state[["otc_trades_vistos"]] <- tail(trades_vistos, 50)
          }
        }
      }
    }
  }

  # Guardar estado actualizado
  write_json(new_state, state_file, auto_unbox = TRUE, pretty = TRUE)
  cat("\n--- PROCESO FINALIZADO EXITOSAMENTE ---\n")

}, error = function(e) {
  send_telegram(paste0("Error en script.R: ", e$message))
  stop(e)
})
