library(httr)
library(readxl)
library(digest)
library(jsonlite)

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

tryCatch({
  state_file <- "estado_hashes.json"
  
  # Lectura segura del JSON existente
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
    file_url  <- df$link[i]
    dest_path <- file.path("temp_downloads", paste0("temp_", i, ".xlsx"))
    
    # Descargar el archivo
    res <- GET(file_url, write_disk(dest_path, overwrite = TRUE))
    
    if (status_code(res) == 200) {
      # 1. Detectar el formato real ("xlsx", "xls" o NULL si está corrupto/HTML)
      fmt <- excel_format(dest_path)
      
      datos_excel <- NULL
      if (!is.null(fmt)) {
        datos_excel <- tryCatch({
          read_excel(dest_path, format = fmt)
        }, error = function(err) {
          NULL
        })
      }
      
      # 2. Si el archivo se leyó correctamente, calcular hash de los datos
      if (!is.null(datos_excel)) {
        current_hash <- digest(datos_excel, algo = "md5")
        previous_hash <- old_hashes[[file_name]]
        
        if (!is.null(previous_hash) && current_hash != previous_hash) {
          archivos_actualizados <- c(archivos_actualizados, file_name)
        }
        
        new_hashes[[file_name]] <- current_hash
      }
    }
  }
  
  # Guardar estado en JSON
  write_json(new_hashes, state_file, auto_unbox = TRUE, pretty = TRUE)
  
  # Notificar actualizaciones si las hay
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
  }
  
}, error = function(e) {
  send_telegram(paste0("Error revisando actualizaciones: ", e$message))
  stop(e)
})