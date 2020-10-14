# ----------------------------------------------------------------------------------------------------
# Basic functions for R
# ----------------------------------------------------------------------------------------------------
# written
# Gabriel LEMYRE
# ----------------------------------------------------------------------------------------------------
# Under the supervision of :
# Maciej AUGUSTYNIAK
# ----------------------------------------------------------------------------------------------------
# First version : November 14th, 2019
# Last version : 4 mars 2020
# ----------------------------------------------------------------------------------------------------

# ——————————————————————————————————————————————————————————————————————————
# SET OF BASIC FUNCTIONS TO MANIPULATE DATA AND STRINGS
# ——————————————————————————————————————————————————————————————————————————



# ——————————————————————————————————————————————————————————————————————————
# Testing if FILE exists, and creating it if not
# ——————————————————————————————————————————————————————————————————————————
file.test <- function(file){
  if(!file.exists(file)){
    file.create(file) 
    print(paste("File :",file,"created successfully."))
  }
}


# ——————————————————————————————————————————————————————————————————————————
# Testing if DOCUMENT exists, and creating it if not
# ——————————————————————————————————————————————————————————————————————————
document.test <- function(file){
  if(!dir.exists(file)){
    dir.create(file) 
    print(paste("File :",file,"created successfully."))
  }
}


# ——————————————————————————————————————————————————————————————————————————
# Testing if DOCUMENT exists, and creating it if not
# ——————————————————————————————————————————————————————————————————————————
hline <- function() {
  cat("# --------------------------------------------------------\n")
}

# ——————————————————————————————————————————————————————————————————————————
# Sourcing code to check for errors
# ——————————————————————————————————————————————————————————————————————————
sources_correctly <- function(file)
{
  fn <- try(source(file), silent = TRUE)
  if (inherits(fn, "try-error")){
    stop(paste("\n-------------------------\nFile :",file,"\nError :",fn))
  } else {
    cat("Sourcing :",basename(file),"\n")
  }
}

# ——————————————————————————————————————————————————————————————————————————
# Obtention des derniers n charactères d'une string x
# ——————————————————————————————————————————————————————————————————————————
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}



# ——————————————————————————————————————————————————————————————————————————
# file path remaker
# ——————————————————————————————————————————————————————————————————————————
make.new.path <- function(file.path,new.path.str,full.path=FALSE){
  if (full.path){
    str.split <- strsplit(file.path, "/", fixed = TRUE)[[1]]
    file.name <- str.split[length(str.split)]
  } else {
    file.name <- file.path
  }
  new.file.path <- paste(new.path.str,file.name,sep="",collapse="")
  return(new.file.path)
}

# ——————————————————————————————————————————————————————————————————————————
# Listing all files in a directory and keeping a list of .R files with full paths
# ——————————————————————————————————————————————————————————————————————————
file.list <- function(path, first.pass=TRUE, ignored.files.vector=NULL, print.inside.message=FALSE,type="all",
                      from.git=FALSE,
                      file.names=NULL){
  
  
  # If sourcing files from github repository
  if (from.git){
    if(is.null(file.names)){stop("No files provided for the file.names argument.")}
    git.file.paths <- c()
    cat(hline(),"\n")
    for (i in file.names){
      file.path <- make.new.path(i,new.path.str=path,full.path = TRUE)
      git.file.paths <- append(git.file.paths,file.path)
      cat("sourcing :",file.path,"\n")
      script <- getURL(file.path, ssl.verifypeer = FALSE)
      cat("evaluating :",file.path,"\n")
      eval(parse(text = script))
    }
    cat(hline(),"\n")
    return()
  }
  
  R.Files <- c() # Initialisation de la liste de fonction .R
  
  # longueur de la liste des dossiers et fichiers à ignorer
  n.files.ignored <- length(ignored.files.vector)
  
  # Information sur le document analysé
  if (print.inside.message){ cat("\n# ------------------------------\n","Inside",path,"\n# ------------------------------\n")}
  
  
  # Obtention de la liste de tous les documents dans le fichier en path
  liste.document <- list.files(path, pattern=NULL, 
                               all.files=FALSE,
                               full.names=TRUE)
  
  if (!length(liste.document)){stop(paste("No files in specified path : ",path,"\n",sep="",collapse=""))}
  
  
  # Impression de la liste des documents si c'est le premier appels de la foncion
  if (first.pass==TRUE){
    if (print.inside.message) print(liste.document);
  }
  
  for (i in 1:length(liste.document)){
    
    # Si c'est le document qui appel la fonction ou un fichier qui l'appel,
    #   on le saute pour éviter les boucles infinies. 
    #   --> La liste des documents à sauter doit être donné par l'utilisateur <--
    if (n.files.ignored>0){
      for (j in 1:n.files.ignored){
        if (substrRight(liste.document[i],nchar(ignored.files.vector[j]))==ignored.files.vector[j]){next.bool=TRUE; break}else{next.bool=FALSE}
      }
      if (next.bool){next}
    }
    
    
    if (type=="all"){
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
        R.Files <- append(R.Files,file.list(liste.document[i],first.pass=FALSE, ignored.files.vector, print.inside.message))
      } else { 
        # Si ce n'est pas un format recherché, on rejette et averti l'utilisateur
        if (print.inside.message) cat("REJECTING",liste.document[i],":",grep("\\.", liste.document[i]),"\n");
      }
    } else if (type=="R") {
      if (grepl("\\.R", liste.document[i])){ 
        # Si document R, ajout à la liste
        if (print.inside.message) cat("+",liste.document[i],"\n");
        R.Files <- append(R.Files,liste.document[i])
      } else {
        # Si ce n'est pas un format recherché, on rejette et averti l'utilisateur
        if (print.inside.message) cat("REJECTING",liste.document[i],":",grep("\\.", liste.document[i]),"\n");
      }
    } else if (type=="pdf") {
      if (grepl("\\.pdf", liste.document[i])){ 
        # Si document R, ajout à la liste
        if (print.inside.message) cat("+",liste.document[i],"\n");
        R.Files <- append(R.Files,liste.document[i])
      } else {
        # Si ce n'est pas un format recherché, on rejette et averti l'utilisateur
        if (print.inside.message) cat("REJECTING",liste.document[i],":",grep("\\.", liste.document[i]),"\n");
      }
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

Matlab2Rdate <- function(val){ as.POSIXct((val - 719529)*86400, origin = "1970-01-01", tz = "UTC") }

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


# ——————————————————————————————————————————————————————————————————————————
# Test égalité de deux listes
# ——————————————————————————————————————————————————————————————————————————
diff.liste <- function(list.A, list.B){
  n <- length(list.A)
  liste.err <- list.A # Utilisation de la première liste comme support
  new.nom.var <- rep(NA,n)
  
  # Impression des erreurs un paramètre à la fois
  for (i in 1:length(list.A)){
    nom.var <- names(list.A[i])
    new.nom.var[i] <- paste("diff",nom.var,collapse="",sep=".")
    
    err <- list.A[[i]]-list.B[[i]]
    
    liste.err[[i]] <- err
    
    abs.err <- abs(err)
    max.index <- which.max(abs.err)
    err.perc <- 100*round((abs.err[max.index]/list.A[[i]][max.index]),4)
    
    # Impression message informations
    cat("Max diff(",nom.var,") = ",max(abs.err)," (",err.perc,"%)\n",sep="")
  }
  
  # Changement des noms - ajout préfix "diff."
  names(liste.err) <- new.nom.var
  
  return(liste.err)
}



# ——————————————————————————————————————————————————————————————————————————
# Arrangement des objets en fonction de l'ordre d'un des objets
# ——————————————————————————————————————————————————————————————————————————
order.sigma <- function(liste,order.var="sigma",Transition.Type){
  nbRegime <- length(liste[[order.var]])
  
  # Getting sigma order
  augmented.order.var <- cbind(liste[[order.var]],c(1:nbRegime))
  order.matrix <- augmented.order.var[order(augmented.order.var[,1], decreasing = TRUE),]
  order.vector <- order.matrix[,2]
  
  # Ordering the parameters in decreasing order of volatility
  mu.F <- liste$mu[order.vector]
  sigma.F <- liste[[order.var]][order.vector]
  if (Transition.Type=="Homogeneous"){
    Gamma.F <- liste$Gamma[order.vector,order.vector]
  } else if (Transition.Type=="Diebold"){
    Gamma.F <- liste$Gamma[order.vector,]
  } else if (Transition.Type=="Diebold.w.filter"){
    Gamma.F <- liste$Gamma[order.vector,]
  }
  
  # Reassignment of the parameters for the returned parameters in
  # HMM.Train to match the ordered values passed in the other functions
  liste$mu <- mu.F
  liste$sigma <- sigma.F
  liste$Gamma <- Gamma.F
  
  return(liste)
}