#### Script para decodificar c?digos con Metropolis-Hastings ####

#### 1. Carga de la matriz generadora de candidatos ####
M <- read.table("/home/tasio/AustenCount.txt",header=F)
logM <- log(M + 1) # Transformaci?n monotona

#### 2. Funciones necesarias en el algoritmo ####

# Funci?n que transforma un caracter en lo que seria su
# indice en el abecedario.
#                 a=1, b=2, ..., z=26, space=27
charIndex <- function(char)
{
  rawAsciiValue <- strtoi(charToRaw(char),16L)
  return(ifelse(rawAsciiValue == 32,
                27, # Espacio
                rawAsciiValue-96)) # a en ASCII es 97, b es 98,...
}

# Calculo del score de una propuesta de texto descifrado
# empleando matriz generadora de cambios cargada
score <- function(decryptedMessage)
{  
	totalScore <- 0
	for (i in 1:(nchar(decryptedMessage)-1)){
	  totalScore <- totalScore +  #Usar logM nos permite sumar en vez de multiplicar
	    logM[charIndex(substr(decryptedMessage, i, i)),
	         charIndex(substr(decryptedMessage, i+1, i+1))]
	}

	return(totalScore)
}

# Dado un mensaje, lo descifra acorde a la funcion candidateFunction
decrypt <- function(originalMessage,candidateFunction)
{  	
	decryptedMessage <- originalMessage
	
	#Sustuimos cada letra del mensaje de acuerdo a la funcion
	for (i in 1:nchar(originalMessage)){
		charInd <- charIndex(substr(originalMessage,i,i))
		if (charInd < 27){ # El caracter espacio no se altera (charIndex==27)
		  substr(decryptedMessage,i,i) <- rawToChar(as.raw(candidateFunction[charInd] + 96))}
	}
	return(decryptedMessage) 
}

#### 3. Ejecucion del algoritmo ####

# Ciframos un mensaje acorde a una funcion aleatoria
message <- "coincidences in general are great stumbling blocks in the way of that class of thinkers who have been educated to know nothing of the theory of probabilities that theory to which the most glorious objects of human research are indebted for the most glorious of illustrations edgar allen poe the murders in the rue morgue morpheus this is your last chance after this there is no turning back you take the blue pill the story ends you wake up in your bed and believe whatever you want to believe you take the red pill you stay in wonderland and i show you how deep the rabbit hole goes"
encryptedMessage <- decrypt(message,sample(1:26))

# Inicializaci?n de variables usadas
scoresMap <- new.env(hash=T, parent=emptyenv()) #Contendra los scores de las funciones
currentFunction <- 1:27 #Funcion de descifrado; inicializamos en la identidad

# Introducimos el primer score, el de la identidad, en el Map
oldScore <- score(decrypt(encryptedMessage,currentFunction))
scoresMap[[paste(currentFunction, collapse=' ')]] <- oldScore

# Iteramos el algoritmo de Metropolis-Hastings
nIterations=29000
for (iteration in 1:nIterations) {
	
  # En la nueva funcion candidata se intercambia el valor asociado a dos letras
	swaps <- sample(1:26,2)
	oldFunction <- currentFunction
	currentFunction[swaps[1]] <- oldFunction[swaps[2]]
	currentFunction[swaps[2]] <- oldFunction[swaps[1]]
	#aqui el current es el propuesto
	#y old function es nuestro current
	
	# Calculo del score de la funcion. Si hemos caido en la misma funcion,
	# cogemos el score de scoresMap
	if (exists(paste(currentFunction, collapse =' '), scoresMap)){
		newScore <- scoresMap[[paste(currentFunction, collapse =' ')]]
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
	
	if ((iteration %%  100) == 0){
		cat(paste0("\n\nIteration = ",iteration,"\n",
		             decrypt(encryptedMessage,currentFunction)))
	}
}

