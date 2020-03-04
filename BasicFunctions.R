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
# Sourcing code to check for errors
# ——————————————————————————————————————————————————————————————————————————
sources_correctly <- function(file)
{
  fn <- try(source(file), silent = TRUE)
  if (inherits(fn, "try-error")){
    stop(paste("\n-------------------------\nFile :",file,"\nError :",fn))
  }
}

# ——————————————————————————————————————————————————————————————————————————
# Obtention des derniers n charactères d'une string x
# ——————————————————————————————————————————————————————————————————————————
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# ——————————————————————————————————————————————————————————————————————————
# Listing all files in a directory and keeping a list of .R files with full paths
# ——————————————————————————————————————————————————————————————————————————
Full.Source <- function(path, first.pass=TRUE, ignored.files.vector=NULL, print.inside.message=FALSE){
  R.Files <- c() # Initialisation de la liste de fonction .R
  
  # longueur de la liste des dossiers et fichiers à ignorer
  n.files.ignored <- length(ignored.files.vector)
  
  # Information sur le document analysé
  if (print.inside.message){ cat("\n# ------------------------------\n","Inside",path,"\n# ------------------------------\n")}
  
  # Obtention de la liste de tous les documents dans le fichier en path
  liste.document <- list.files(path, pattern=NULL, 
                               all.files=FALSE,
                               full.names=TRUE)
  
  # Impression de la liste des documents si c'est le premier appels de la foncion
  if (first.pass==TRUE){
    if (print.inside.message) print(liste.document);
  }
  
  for (i in 1:length(liste.document)){
    
    # Si c'est le document qui appel la fonction ou un fichier qui l'appel,
    #   on le saute pour éviter les boucles infinies. 
    #   --> La liste des documents à sauter doit être donné par l'utilisateur <--
    for (j in 1:n.files.ignored){
      if (substrRight(liste.document[i],nchar(ignored.files.vector[j]))==ignored.files.vector[j]){next.bool=TRUE; break}else{next.bool=FALSE}
    }
    if (next.bool){next}
    
    # Sinon on test pour les extensions ou lance la fonction dans les documents
    #   internes de l'appel actuel
    if (grepl("\\.R", liste.document[i])){ 
      # Si document R, ajout à la liste
      if (print.inside.message) cat("+",liste.document[i],"\n");
      R.Files <- append(R.Files,liste.document[i])
    } else if (grepl("\\.csv", liste.document[i])){ 
      # Si document csv, rien pour l'instant
      if (print.inside.message) cat("dataset",liste.document[i],"\n");
    } else if (!grepl("\\.", liste.document[i])){ 
      # Si aucune extension, on entre dans le document et roule la présente fonction
      if (print.inside.message) print("--> Going Deeper <--");
      R.Files <- append(R.Files,Full.Source(liste.document[i],first.pass=FALSE, ignored.files.vector, print.inside.message))
    } else { 
      # Si ce n'est pas un format recherché, on rejette et averti l'utilisateur
      if (print.inside.message) cat("REJECTING",liste.document[i],":",grep("\\.", liste.document[i]),"\n");
    }
    
  }
  
  return(R.Files) # On retourne la liste à la fonction précédente
}

# ——————————————————————————————————————————————————————————————————————————
# Unpacking the ellipsis in function calls and saving the variables under the right name
# ——————————————————————————————————————————————————————————————————————————
unpack.Ellipsis <- function(x,...){
  opt.args <- list(...)
  if (length(opt.args)>0){
    for(i in 1:length(opt.args)) {
      assign(x <- names(opt.args)[i], value <- opt.args[[i]])
    }
  }
}

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
bib.prep <- function(A){
  str <- gsub(" ", "_", A, fixed = TRUE) # Ajoute des '_' à la place des espace
  str <- gsub("-", "_", str, fixed = TRUE) # Remplace '-' par des '_'
  str <- gsub(":", "_", str, fixed = TRUE) # Remplace ':' par des '_'
  Copie.Presse.Papier(str) # Copie dans le presse papier
  return(str)
}