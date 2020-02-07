# ----------------------------------------------------------------------------------------------------
# Making LaTeX Table envir. compatible code from R dataframes
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
# FONCTION PERMETTANT D'IMPRIMER LE RÉSULTATS EN FORMAT LATEX
# ——————————————————————————————————————————————————————————————————————————
Make.LaTeX.Table = function(R.Matrix.Object,
							caption=TRUE,
							title = "",
							Col.Titles = NULL,
							Row.Titles = NULL,
							n.dec = 3,
							type = "Table",
							Row.Pos="c",
							Cross.Lines=FALSE,
							print.Cons=TRUE,
							copy.CB=TRUE,...){ # Ecrit le code LaTeX dun environnement Tabular a partir dune matrice R
	
	n.col = max(ncol(R.Matrix.Object),1)
	# n.Row = max(nrow(R.Matrix.Object),length(R.Matrix.Object))
	n.Row = nrow(R.Matrix.Object)
	
	Row.Titles.Ind = FALSE
	Col.Titles.Ind = FALSE
	if(!is.null(Row.Titles)){Row.Titles.Ind = TRUE}
	if(!is.null(Col.Titles)){Col.Titles.Ind = TRUE}
	if((Row.Titles.Ind | Cross.Lines)  & !(n.Row==1)){
		pos.col = "r |"
		nb.col.pos = n.col
		if(Cross.Lines & !Row.Titles.Ind){
			nb.col.pos = nb.col.pos - 1
		}
	}else{
		pos.col=""
		nb.col.pos = n.col
	}
	
	pos.col = paste(pos.col," ",chr(42),'{',nb.col.pos,"}","{",Row.Pos,"} ",sep="")
	
	str = paste("% ------------------------ \n",
				chr(92),"begin{minipage}{",
				chr(92),"linewidth} \n",
				chr(92),"centering",
				if(caption){paste(chr(92),"captionof{table}{",title,"} \n",sep="")},
				chr(92),"label{tab : ",title,"} \n",
				chr(92),"begin{tabular}[t]{",pos.col,"} \n",sep="")
	
	Print.Nth.Element = function(Element,String,Col,last=FALSE){
		if (is.numeric(Element)){
			String=paste(String," $",round(Element,n.dec),"$ ",if(!last){chr(38)}else{""},sep="")
		}else{
			String=paste(String," ",Element," ",if(!last){chr(38)}else{""},sep="")
		}
		Print.Nth.Element = String # Returns the input string modified
	}
	
	if (Col.Titles.Ind){
		lenoff = length(Col.Titles)
		if (Row.Titles.Ind){
			if (length(Col.Titles) == n.col){
				Col.Titles = append(Col.Titles,"",after=0)
			}
		}
		for (i in 1:(lenoff-1)){
			str = paste(str,Col.Titles[i],chr(38))
		}
		str = paste(str," ",Col.Titles[lenoff]," ", chr(92),chr(92),"\n ", chr(92),"hline \n", sep="")
	}
	
	for (i in 1:n.Row){
		if (!is.null(Row.Titles[i])){ # Adds the Row titles if there are any
			str = paste(str,Row.Titles[i],chr(38)) # Adds the ith one
		}
		
		if (n.col!=1){ # If there is more than one Column
			for (k in 1:n.col){ # Does all the Columns and doesnt add & on the last one
				str = Print.Nth.Element(R.Matrix.Object[i,k],str,k,last=isTRUE(k==n.col))
			}
		} else {
			str = Print.Nth.Element(R.Matrix.Object[i],str,n.col,last=TRUE) # If one Column only, does that one only
		}
		str=paste(str," ",chr(92),chr(92),sep="") # Adds the double backslash at the end of the line
		if(!(i==n.Row)){str=paste(str,"\n",sep="")} # If it is the last line, it wont add line jump
		if (i==1){ # If the first line was already printed
			if (Cross.Lines & !Col.Titles.Ind & !(n.Row==1)){ # If the line is to be printed but no col.titles giving expressively
				str = paste(str, chr(92),"hline \n", sep="")
			}
		}
	}
	
	
	
	# Fin de lexecution et retour du resultat
	if(copy.CB){
		Copie.Presse.Papier(paste(str,"\n",
								  chr(92),"end{tabular} \n",
								  chr(92),"end{minipage}","\n ~",chr(92),chr(92),
								  "\n % ------------------------ \n",sep="")) # Copie le string concatene au presse-papier
	}
	
	if(print.Cons){
		str=cat(paste(str,"\n",
					  chr(92),"end{tabular} \n",
					  chr(92),"end{minipage}","\n ~",chr(92),chr(92),
					  "\n % ------------------------ \n",sep="")) # Limprime aussi dans la console R
	}
}

chr <- function(n) { rawToChar(as.raw(n)) }

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