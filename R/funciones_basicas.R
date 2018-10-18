Provincias <- function(){
  provicias <- read.csv(file = "R/provincias_code.txt",header = TRUE,sep = ";")
  return(provicias)
}

DescargarDatosProvincia <- function(provincia){
  if(is.numeric(provincia)){
    unzip(zipfile = sprintf("https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/ifn3p%01d_tcm30-293907.zip",provincia))
  } else {
    if (!is.character(provincia)){
      stop("La provincia debe ser un número o una palabra")
    }
    if (!(provincia %in% Provincias()$nombre)){
      stop("Ese nombre no lo reconozco, si quiere conocer los nombre y códigos teclea Provincias()")
    }
    provincia <- Provincias() %>% dplyr::filter(nombre==provincia) %>% dplyr::select(Codigo) %>% as.numeric()
    unzip(zipfile = sprintf("https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/ifn3p%01d_tcm30-293907.zip",provincia))
  }
  mdf.file <- list.files(pattern = ".mdb")
  datos <- Hmisc::mdb.get(file = mdf.file)
  unlink(mdf.file)
  return(datos)
}

GetArchivos <- function(){
  base_page <- "https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/"
  h <- httr::handle(base_page)
  archivos1 <- httr::GET("https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/ifn3_base_datos_1_25.aspx",handle = h)
}
