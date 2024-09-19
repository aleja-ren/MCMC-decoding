#PRACTICA ENTREGABLE PROCESOS ESTOCASTICOS

#Alejandra Gavino-Dias

mensaje_cifrado<-'HTSVBPX QTIVBHRXIXBSX XLNTBVBGVBGREX HVSBSXBPXIQVYRXIHTBSXBLTILRXILRVBCBSXB XGRJRTIBXQHXBSX XLNTBRILGACXBGVBGREX HVSBSXBLVYERV BSXB XGRJRTIBTBSXBL XXILRVBVQRBLTYTBGVBGREX HVSBSXBYVIRWXQHV BQAB XGRJRTIBTBQABL XXILRVBRISRZRSAVGBCBLTGXLHRZVYXIHXBHVIHTBXIBPAEGRLTBLTYTBXIBP RZVSTBPT BGVBXIQXMVIDVBGVBP VLHRLVBXGBLAGHTBCBGVBTEQX ZVILRVBHTSTBRISRZRSATBHRXIXBSX XLNTBVBGVBGREX HVSBSXBTPRIRTIBCBSXBXKP XQRTIBXQHXBSX XLNTBRILGACXBXGBSXBITBQX BYTGXQHVSTBVBLVAQVBSXBQAQBTPRIRTIXQBXGBSXBRIZXQHRJV BCB XLRER BRIWT YVLRTIXQBCBTPRIRTIXQBCBXGBSXBSRWAISR GVQBQRIBGRYRHVLRTIBSXBW TIHX VQBPT BLAVGUARX BYXSRTBSXBXKP XQRTI'

long_palabras_espanol <- read.csv("C:/Users/iglez/OneDrive - UVa/4-CURSO/PEST/DecodificacionMCMC/long_palabras_espanol.txt", sep="")
bigramas_espanol <- read.csv("C:/Users/iglez/OneDrive - UVa/4-CURSO/PEST/DecodificacionMCMC/bigramas_espanol.txt", sep="", encoding="UTF-8")
trigramas_espanol <- read.csv("C:/Users/iglez/OneDrive - UVa/4-CURSO/PEST/DecodificacionMCMC/trigramas_espanol.txt", sep="", encoding="UTF-8")

#correcciones
bigramas_espanol$bigrama[28]<-"NA"



clave<-c(toupper(letters)[1:14],"Ñ",toupper(letters)[15:26]," ")
generate_cipher <- function() sample(clave,
                                     replace = FALSE)

swap <- function(x){
  # Select two distinct indices
  rand_indices <- sample(1:length(x), size = 2, replace=FALSE)
  element_1 <- x[rand_indices[1]]
  element_2 <- x[rand_indices[2]]
  
  x[rand_indices[1]] <- element_2
  x[rand_indices[2]] <- element_1
  
  return(x)
}

#funcion que decodifica dado un texto cifrado y un cifrado
decode_text <- function(ciphered_text, cipher) {
  chartr(
    x = ciphered_text,
    old = paste(clave, collapse = ""),
    new = paste(cipher, collapse = "")
  )
}

encode_text <- function(text, cipher) {
  chartr(
    x = text,
    old = paste(clave, collapse = ""),
    new = paste(cipher, collapse = "")
  )
}

#funcion que divide una palabra en sus bigramas
bigramas_palabra <- function(palabra) {
  aaa<-unlist(strsplit(palabra,''))
  nnn<-length(aaa)
  if(nnn>=2){
    bigramas<-paste0(aaa[1:(nnn-1)],aaa[2:nnn])
    return(bigramas)
  }else{
    return("*")
  }
}

#funcion que divide una palabra en sus trigramas
trigramas_palabra <- function(palabra) {
  aaa<-unlist(strsplit(palabra,''))
  nnn<-length(aaa)
  if(nnn>=3){
    trigramas<-paste0(aaa[1:(nnn-2)],aaa[2:(nnn-1)],aaa[3:nnn])
    return(trigramas)
  }else{
    return("*")
  }
}

#score 1: se calcula como la frecuencia de la longitud de las palabras en español
#dada por el archivo long_palabras_espanol, tenemos que ir calculando el score para
#cada palabra del mensaje
score1<-function(palabras){
  freq<-1
  f<-0
  for(i in palabras){
    long_palabra<-nchar(i)
    if(long_palabra <= nrow(long_palabras_espanol)){
      f<-long_palabras_espanol$frecuencia[long_palabra]
    }else{
      f<-10^-8
    }
    freq<-freq*f
  }
  freq<-log(freq)
  return(freq)
}

#score 2: se calcula dividiendo cada palabra del mensaje en bigramas y obteniendo la
#frecuencia del correspondiente bigrama dado por el archivo bigramas_espanol
score2<-function(palabras){
  s2<-0
  bigramas<-c()

  for(palabra in palabras){
    bigramas<-c(bigramas, bigramas_palabra(palabra))
  }
  for(i in bigramas){
    if(i %in% bigramas_espanol$bigrama){
      f<-log(bigramas_espanol$frecuencia[bigramas_espanol$bigrama==i])
    }else{
      f<-log(10^-8)
    }
    s2<-s2+f
  }
  
  return(s2)
}

#score 3: se calcula dividiendo cada palabra del mensaje en trigramas y obteniendo la
#frecuencia del correspondiente trigrama dado por el archivo trigramas_espanol
score3<-function(palabras){
  s3<-0
  trigramas<-c()
  
  for(palabra in palabras){
    trigramas<-c(trigramas, trigramas_palabra(palabra))
  }
  for(i in trigramas){
    if(i %in% trigramas_espanol$V1){
      f<-log(trigramas_espanol$V2[trigramas_espanol$V1==i])
    }else{
      f<-log(10^-8)
    }
    s3<-s3+f
  }
  return(s3)
}

#el score total del mensaje es score1^3*score2*score3^(1/2), pero como los scores
#individuales les hemos dado de forma logaritmica por las propiedades de los logaritmos
#esta sera la formula 3*score1+score2+(0.5)*score3
score<-function(mensaje){
  palabras<-unlist(strsplit(mensaje, split = " "))
  score_valor<-3*score1(palabras)+score2(palabras)+0.5*score3(palabras)
  return(score_valor)
}


#########################################################################################


#en el cryptography.r pone que inicializamos con la identidad
scoresMap <- new.env(hash=T, parent=emptyenv())
current<-clave

#introducimos el score que obtenemos con la identidad en el mapa
score_inicial<-score(mensaje_cifrado)
scoresMap[[paste(current, collapse=' ')]] <- score_inicial


for (iteration in 5000) {
  old<-current
  current<-swap(old)
  
  # Calculo del score de la funcion. Si hemos caido en la misma funcion,
  # cogemos el score de scoresMap
  if (exists(paste(current, collapse =' '), scoresMap)){
    newScore <- scoresMap[[paste(current, collapse =' ')]]
  } else {
    newScore <- score (decrypt(encryptedMessage,currentFunction))
    scoresMap[[paste(currentFunction, collapse = ' ')]] <- newScore
  }
  
  # Aceptacion: En funcion de U nos quedamos con la anterior funcion o la nueva
  if (runif(1) > exp(newScore-oldScore)){
    currentFunction <- oldFunction
  } else {
    oldScore <- newScore
  }
  
  if ((iteration %%  100) == 0)
  {
    cat(paste0("\n\nIteration = ",iteration,"\n",
               decrypt(encryptedMessage,currentFunction)))
  }
}


