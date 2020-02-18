# ----------------------------------------------------------------------------------------------------
# Basic functions for R
# ----------------------------------------------------------------------------------------------------
# written
# Gabriel LEMYRE
# ----------------------------------------------------------------------------------------------------
# Under the supervision of :
# Maciej AUGUSTYNIAK
# ----------------------------------------------------------------------------------------------------
# Last version : November 14th, 2019
# ----------------------------------------------------------------------------------------------------

# ——————————————————————————————————————————————————————————————————————————
# SET OF BASIC FUNCTIONS TO MANIPULATE DATA AND STRINGS
# ——————————————————————————————————————————————————————————————————————————

# ——————————————————————————————————————————————————————————————————————————
# ROUNDING with n zeros function <- Returns a string and not a numeric type
# ——————————————————————————————————————————————————————————————————————————
s <- function(x,n){
	sprintf(paste("%.",n,"f",sep=""), round(x,n))
}

# ——————————————————————————————————————————————————————————————————————————
# Getting COLUMNWISE MAXIMUM
# ——————————————————————————————————————————————————————————————————————————
colMax <- function(data){
	apply(data,2, which.max)
}

# ——————————————————————————————————————————————————————————————————————————
# Getting Dates in correct format from a matlab dataset
# ——————————————————————————————————————————————————————————————————————————
Matlab2Rdate <- function(val) as.POSIXct((val - 719529)*86400, origin = "1970-01-01", tz = "UTC")

# ——————————————————————————————————————————————————————————————————————————
# Transforme un nombre en son caractère associé
# ——————————————————————————————————————————————————————————————————————————
chr <- function(n) { rawToChar(as.raw(n)) }

# ——————————————————————————————————————————————————————————————————————————
# Copie le contenu d'une variable de type 'string' au presse-papier de l'usager
# ——————————————————————————————————————————————————————————————————————————
Copie.Presse.Papier <- function(string) {
  os <- Sys.info()[['sysname']]
  if (os == "Windows") { # Si systeme dexploitation windows
    return(utils::writeClipboard(string))
  } else if (os == "Darwin") { # Si systeme dexploitation iOS
    Mac.Copie.Presse.Papier <- function(string){
      presse.papier <- pipe("pbcopy", "w")
      cat(string, file = presse.papier, sep = "\n")
      close(presse.papier)	# Fermer lobjet presse-papier
    }
    return(Mac.Copie.Presse.Papier(string))
  }
}

# ——————————————————————————————————————————————————————————————————————————
# Adds underscores to a string of characters in place of spaces - for bibliographical purposes
# ——————————————————————————————————————————————————————————————————————————
p <- function(A){
  str <- gsub(" ", "_", A, fixed = TRUE) # Ajoute des '_' à la place des espace
  str <- gsub("-", "_", str, fixed = TRUE) # Remplace '-' par des '_'
  str <- gsub(":", "_", str, fixed = TRUE) # Remplace ':' par des '_'
  Copie.Presse.Papier(str) # Copie dans le presse papier
  return(str)
}