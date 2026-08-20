library(httr)
library(readxl)
library(digest)
library(readxl)
library(jsonlite)

start_time <- Sys.time()

token <- Sys.getenv("TELEGRAM_TOKEN")
chat_id <- Sys.getenv("TELEGRAM_CHAT_ID")

send_telegram <- function(msg) {
  url <- paste0("https://api.telegram.org/bot", token, "/sendMessage")
  res <- POST(url, body = list(chat_id = chat_id, text = msg), encode = "form")
  
  if (status_code(res) != 200) {
    cat("Error al enviar mensaje a Telegram. Código HTTP:", status_code(res), "\n")
  }
}

# Función para filtrar marcas de tiempo y metadatos dinámicos antes del hash
clean_excel_data <- function(file_path) {
  tryCatch({
    hojas <- excel_sheets(file_path)
    hojas_limpias <- lapply(hojas, function(h) {
      # Leer matriz completa sin asumir encabezados
      df_raw <- read_excel(file_path, sheet = h, col_names = FALSE)
      if (nrow(df_raw) == 0) return(df_raw)
      
      # Identificar y descartar filas con timestamps o metadatos de exportación
      is_metadata <- apply(df_raw, 1, function(row) {
        txt <- paste(as.character(row[!is.na(row)]), collapse = " ")
        grepl("fecha de consulta|generado|hora de impresi[oó]n|timestamp|descargado|reporte generado", txt, ignore.case = TRUE)
      })
      
      df_clean <- df_raw[!is_metadata, , drop = FALSE]
      df_clean <- df_clean[rowSums(!is.na(df_clean) & df_clean != "") > 0, , drop = FALSE]
      return(df_clean)
    })
    return(hojas_limpias)
  }, error = function(e) NULL)
}

tryCatch({
  state_file <- "estado_hashes.json"
  
  old_hashes <- list()
  if (file.exists(state_file) && file.info(state_file)$size > 2) {
    tryCatch({
      old_hashes <- fromJSON(state_file)
    }, error = function(err) {
      old_hashes <- list()
    })
  }
  new_hashes <- old_hashes
  
  df <- read_excel("Links_alertas.xlsx")
  archivos_actualizados <- c()
  
  if (!dir.exists("temp_downloads")) dir.create("temp_downloads")
  
  for (i in seq_len(nrow(df))) {
    file_name <- df$nombre_archivo[i]
    file_url  <- trimws(df$link[i])
    dest_path <- file.path("temp_downloads", paste0("temp_", i))
    
    # 1. Limpieza de prefijos blob:
    file_url <- gsub("^blob:", "", file_url)
    
    # 2. Validar formato de URL
    if (!grepl("^https?://", file_url, ignore.case = TRUE)) {
      cat("URL omitida o no válida para:", file_name, "(", file_url, ")\n")
      next
    }
    
    # 3. Lógica Anti-caché (se excluyen INE Uruguay y Banrep Colombia para no alterar reportes dinámicos)
    if (grepl("ine\\.gub\\.uy|banrep\\.gov\\.co", file_url, ignore.case = TRUE)) {
      url_final <- file_url
    } else {
      sep <- if (grepl("\\?", file_url)) "&" else "?"
      url_final <- paste0(file_url, sep, "nocache=", as.numeric(Sys.time()))
    }
    
    cat("Procesando:", file_name, "...\n")
    
    # 4. Descarga vía HTTR
    res <- tryCatch({
      GET(
        url_final, 
        add_headers(
          "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
          "Cache-Control" = "no-cache", 
          "Pragma" = "no-cache"
        ),
        config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE),
        write_disk(dest_path, overwrite = TRUE),
        timeout(25)
      )
    }, error = function(e) {
      cat("Error httr conectando con", file_name, ":", e$message, "\n")
      return(NULL)
    })
    
    # 5. Respaldo con cURL nativo
    if (is.null(res) || status_code(res) != 200) {
      cat("Reintentando descarga con cURL nativo para:", file_name, "\n")
      cmd_curl <- sprintf(
        'curl -s -k -L -A "Mozilla/5.0 (Windows NT 10.0; Win64; x64)" "%s" -o "%s"',
        file_url, dest_path
      )
      system(cmd_curl)
    }
    
    # 6. Extracción de datos y cálculo del Hash MD5 limpio
    if (file.exists(dest_path) && file.info(dest_path)$size > 100) {
      current_hash <- NULL
      fmt <- tryCatch(excel_format(dest_path), error = function(e) NULL)
      
      if (!is.null(fmt)) {
        # Filtra el contenido del Excel eliminando estampas de tiempo dinámicas
        datos_excel_clean <- clean_excel_data(dest_path)
        
        if (!is.null(datos_excel_clean)) {
          current_hash <- digest(datos_excel_clean, algo = "md5")
        }
      } else {
        # Hash directo para PDF y archivos binarios
        current_hash <- digest(dest_path, algo = "md5", file = TRUE)
      }
      
      # Comparación con el estado guardado
      if (!is.null(current_hash)) {
        previous_hash <- old_hashes[[file_name]]
        
        if (!is.null(previous_hash) && current_hash != previous_hash) {
          archivos_actualizados <- c(archivos_actualizados, file_name)
        }
        
        new_hashes[[file_name]] <- current_hash
      }
    } else {
      cat("ADVERTENCIA: No se pudo descargar un contenido válido para:", file_name, "\n")
    }
  }
  
  # Guardar estado en JSON
  write_json(new_hashes, state_file, auto_unbox = TRUE, pretty = TRUE)
  
  # Notificar en Telegram
  if (length(archivos_actualizados) > 0) {
    end_time <- Sys.time()
    duration <- round(as.numeric(difftime(end_time, start_time, units = "secs")))
    hora_str <- format(Sys.time(), "%I:%M%p", tz = "America/Santo_Domingo")
    
    for (arch in archivos_actualizados) {
      msg <- paste0(
        "Hora revisión ", hora_str, "\n",
        "Actualización \"", arch, "\"\n",
        "Tiempo de ejecución: ", duration, " segundos"
      )
      send_telegram(msg)
    }
  } else {
    cat("Revisión completada exitosamente. Sin cambios detectados.\n")
  }
  
}, error = function(e) {
  msg_err <- paste0("Error revisando actualizaciones: ", e$message)
  cat(msg_err, "\n")
  send_telegram(msg_err)
  stop(e)
})
