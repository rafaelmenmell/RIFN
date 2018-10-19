Provincias <- function(){
  provicias <- read.csv(file = "R/provincias_code.txt",header = TRUE,sep = ";")
  return(provicias)
}

DescargarDatosProvincia <- function(provincia){
  archivos <- GetArchivos()
  if(is.numeric(provincia)){
    file <- archivos[provincia]
    download.file(url = file,destfile = "temporal.zip",quiet = TRUE,method = "wget",extra = "--no-check-certificate")
    unzip(zipfile = "temporal.zip",exdir = ".")
    unlink("temporal.zip")
  } else {
    if (!is.character(provincia)){
      stop("La provincia debe ser un número o una palabra")
    }
    if (!(provincia %in% Provincias()$nombre)){
      stop("Ese nombre no lo reconozco, si quiere conocer los nombre y códigos teclea Provincias()")
    }
    provincia <- Provincias() %>% dplyr::filter(nombre==provincia) %>% dplyr::select(Codigo) %>% as.numeric()
    file <- archivos[provincia]
    download.file(url = file,destfile = "temporal.zip",quiet = TRUE,method = "wget",extra = "--no-check-certificate")
    unzip(zipfile = "temporal.zip",exdir = ".")
    unlink("temporal.zip")
  }
  mdf.file <- list.files(pattern = ".mdb")
  datos <- Hmisc::mdb.get(file = mdf.file)
  unlink(mdf.file)
  return(datos)
}

GetArchivos <- function(){
  page1 <- readLines("R/pagina1.html")
  page1 <- page1[grepl("zip",page1)]
  page1 <- strsplit(page1,split = "\"")
  page1 <- page1[grepl("ifn",page1)]
  page1 <- lapply(page1,function(x) x[grepl("zip",x)])
  page1 <- lapply(page1,function(x) x[grepl("https",x)])
  page1 <- unlist(page1)
  #cutre
  page2 <- readLines("R/pagina2.html")
  page2 <- page2[grepl("zip",page2)]
  page2 <- strsplit(page2,split = "\"")
  page2 <- page2[grepl("ifn",page2)]
  page2 <- lapply(page2,function(x) x[grepl("zip",x)])
  page2 <- lapply(page2,function(x) x[grepl("https",x)])
  page2 <- unlist(page2)
  pages <- c(page1,page2)
  return(pages)
}
