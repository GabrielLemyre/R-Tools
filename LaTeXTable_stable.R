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
                            add.label=FALSE,
                            Col.Titles = NULL,
                            Row.Titles = NULL,
                            bold.col.title=FALSE,
                            bold.row.title=FALSE,
                            top.triangle.color=NULL,
                            bottom.triangle.color=NULL,
                            cond.color.triangle=TRUE,
                            color.vector=NULL,
                            color.strenght=25,
                            rotate.col.titles=FALSE,
                            last.col.total=FALSE,
                            last.row.total=FALSE,
                            angle=90,
                            n.dec = 3,
                            remove.zeros=FALSE,
                            type = "table",
                            Row.Pos="c",
                            Row.Titles.pos="r",
                            Cross.Lines=FALSE,
                            Top.Line=FALSE,
                            highlight.diag=FALSE,
                            highlight.color="yellow",
                            left.line=FALSE,
                            print.Cons=TRUE,
                            hlines=FALSE,
                            copy.CB=TRUE,...){ # Ecrit le code LaTeX dun environnement Tabular a partir dune matrice R
	
          # Initialisation de la variable gardant le nombre de table à imprimer
          nb.tables <- 1
          if (is.list(R.Matrix.Object)){
                    nb.tables <- length(R.Matrix.Object)
          } else {
                    R.Matrix.Object <- list(R.Matrix.Object)
          }
          # print(R.Matrix.Object)
          # print(typeof(R.Matrix.Object))
          
          # Si l'utilisateur ne veut pas afficher les valeurs nulles, elles sont remplacées par des chaines vides
          if (remove.zeros){
                    for (t in 1:nb.tables){
                              R.Matrix.Object[[t]][R.Matrix.Object[[t]]==0] <- ""  
                    }
          }
          
          for (p in 1:nb.tables){
                    if (is.null(dim(R.Matrix.Object[[p]]))){ # Si c'est un vecteur seulement, on le transforme en matrice à 1 colonne
                              R.Matrix.Object[[p]] <- matrix(R.Matrix.Object[[p]],ncol=1)
                    }
          }
          
          n.col = max(ncol(R.Matrix.Object[[1]]),1)
          # n.row = max(nrow(R.Matrix.Object),length(R.Matrix.Object))
          n.row = nrow(R.Matrix.Object[[1]])
	
	Row.Titles.Ind = FALSE
	Col.Titles.Ind = FALSE
	if(!is.null(Row.Titles)){
	          Row.Titles.Ind = TRUE
	          }
	if(!is.null(Col.Titles)){
	          Col.Titles.Ind = TRUE
	}
	
	# print(Row.Titles.Ind)
	# print(Cross.Lines)
	# print(n.row)
	# print(Col.Titles.Ind)
	if((Row.Titles.Ind | Cross.Lines)  & (!(n.row==1) | Col.Titles.Ind)){
	          # Position des titres de lignes
	          pos.col = paste0(Row.Titles.pos," |")
	                    
		nb.col.pos = n.col
		if(Cross.Lines & !Row.Titles.Ind){
			nb.col.pos = nb.col.pos - 1
		}
	}else{
		pos.col=""
		nb.col.pos = n.col
	}
	
	if (last.col.total){
	          pos.col = paste(pos.col," ",chr(42),'{',(nb.col.pos-1),"}","{",Row.Pos,"} ","|",Row.Pos,sep="")
	} else {
	          pos.col = paste(pos.col," ",chr(42),'{',nb.col.pos,"}","{",Row.Pos,"} ",sep="")
	}
	
	# —————————————————————————————————————————————————————————————————————————————————————————————————————————
	Print.Nth.Element = function(Element,String,last=FALSE, highlight.color=NULL){
	          color <- ""
	          if (!is.null(highlight.color)){
	                    color <- paste0("{\\cellcolor{",highlight.color,"!",color.strenght,"}}")
	          }
		if (is.numeric(Element)){
			paste0(String," ",color,"$",round(Element,n.dec),"$ ",if(!last){chr(38)}else{""})
		}else{
			paste0(String," ",color,Element," ",if(!last){chr(38)}else{""})
		}
	}
	# —————————————————————————————————————————————————————————————————————————————————————————————————————————
	
	
	if (nb.tables>1){
	          str = paste0("% ------------------------ \n",
	                       chr(92),"noindent \n",
	                       chr(92),"begin{table}[h]\n",
	                       chr(92),"centering")
	          
	} else if (nb.tables==1){
	          str = paste0("% ------------------------ \n",
	                       chr(92),"noindent \n",
	                       chr(92),"begin{minipage}{",
	                       chr(92),"linewidth} \n",
	                       chr(92),"centering")
	}
	
	for (i.table in 1:nb.tables){
	          if (nb.tables>1){
	                    str <- paste0(str,
	                                  chr(92),"begin{subtable}[t]{",(1/nb.tables-.01),"\\linewidth}\n",
	                                  if(caption){paste0(chr(92),"captionof{table}{",title[i.table],"} \n")},
	                                  if (add.label) {paste0(chr(92),"label{tab : ",title[i.table],"} \n")},
	                                  chr(92),"begin{tabular}[t]{",pos.col,"} \n")
	          } else if (nb.tables==1){
	                    str = paste0(str,
	                                 if(caption){paste(chr(92),"captionof{table}{",title,"} \n",sep="")},
	                                 if (add.label) {paste0(chr(92),"label{tab : ",title,"} \n")},
	                                 chr(92),"begin{tabular}[t]{",pos.col,"} \n")
	          }

	# Ajout ligne sur le top de la table
	if (Top.Line){
	          if (Row.Titles.Ind){
	                    str <- paste0(str,"\\cline{2-",n.col+1,"}\n")
	          } else {
	                    str <- paste0(str,"\\hline\n")
	          }
	}
	
	if (Col.Titles.Ind){
	          if (Row.Titles.Ind){
	                    if (length(Col.Titles) <= n.col){
	                              Col.Titles = append(Col.Titles,"",after=0)
	                    }
	          }
	          
		lenoff = length(Col.Titles)
		for (i in 1:(lenoff-1)){
			str = paste(str,
			            if (rotate.col.titles){paste0("\\rotatebox{",angle,"}{")},
			            if (bold.col.title){paste0("\\textbf{",as.character(Col.Titles[i]),"}")}else{as.character(Col.Titles[i])},
			            if (rotate.col.titles){"}"},
			            chr(38))
		}
		str = paste(str," ",
		            if (rotate.col.titles){paste0("\\rotatebox{",angle,"}{")},
		            if (bold.col.title){paste0("\\textbf{",as.character(Col.Titles[lenoff]),"}")}else{as.character(Col.Titles[lenoff])},
		            if (rotate.col.titles){"}"},
		            " ", chr(92),chr(92),"\n ", chr(92),"hline \n", sep="")
	}
	
	for (i in 1:n.row){
		if (!is.null(Row.Titles[i])){ # Adds the Row titles if there are any
			str = paste0(str,
			            if (left.line){paste0("\\multicolumn{1}{|",Row.Titles.pos,"|}{")},
			            if (bold.row.title){"\\textbf{"},
			            Row.Titles[i],
			            if (left.line){"}"},
			            if (bold.row.title){"}"},
			            " ",chr(38)) # Adds the ith row name
		}
		
		if (n.col!=1){ # If there is more than one Column
			for (k in 1:n.col){ # Does all the Columns and doesnt add & on the last one
			          
			          if (highlight.diag & k==i){ 
			                    # Si la valeur est sur la diagonale, et qu'on veut la colorer
			                    str = Print.Nth.Element(R.Matrix.Object[[i.table]][i,k],str,last=isTRUE(k==n.col), 
			                                            highlight.color=highlight.color)
			                    
			          } else if (!is.null(top.triangle.color) && k>i && eval(parse(text=cond.color.triangle))){
			                    # Si la valeur est dans le triangle supérieur de la matrice et qu'on veut le colorer avec conditions dans "cond.color.triangle"
			                    str = Print.Nth.Element(R.Matrix.Object[[i.table]][i,k],str,last=isTRUE(k==n.col),
			                                            highlight.color=top.triangle.color)
			                    
			          } else if (!is.null(bottom.triangle.color) && k<i && eval(parse(text=cond.color.triangle))){
			                    # Si la valeur est dans le triangle inférieur de la matrice et qu'on veut le colorer avec conditions dans "cond.color.triangle"
			                    str = Print.Nth.Element(R.Matrix.Object[[i.table]][i,k],str,last=isTRUE(k==n.col),
			                                            highlight.color=bottom.triangle.color)
			                    
			          } else if (!is.null(color.vector)){
			                    # Si les couleurs par lignes sont spécifié, on utilise ces couleurs
			                    str = Print.Nth.Element(R.Matrix.Object[[i.table]][i,k],str,last=isTRUE(k==n.col),
			                                            highlight.color=color.vector[i])
			          } else {
			                    # Sinon, on ne colore pas
			                    str = Print.Nth.Element(R.Matrix.Object[[i.table]][i,k],str,last=isTRUE(k==n.col))
			          }
			}
		} else {
			str = Print.Nth.Element(R.Matrix.Object[[i.table]][i],str,last=TRUE) # If one Column only, does that one only
		}
	          
		str=paste(str," ",chr(92),chr(92),if(hlines){"\\hline"},
		          if(last.row.total & i==(n.row-1)){"\\hline"},sep="") # Adds the double backslash at the end of the line
		
		# Adding line jump
		str <- paste(str,"\n",sep="") 
		
		if (i==1){ # If the first line was already printed
			if (Cross.Lines & !Col.Titles.Ind & !(n.row==1)){ # If the line is to be printed but no col.titles giving expressively
				str = paste(str, chr(92),"hline \n", sep="")
			}
		}
	}
	          
	          if (nb.tables>1){
	                    str <- paste0(str,
	                                  chr(92),"end{tabular} \n",
	                                  chr(92),"end{subtable}%\n")
	          } else if (nb.tables==1){
	                    str = paste0(str,
	                                 chr(92),"end{tabular}\n")
	          }
}
	
	Final.str <- paste(str,
	                   chr(92),"end{minipage}","\n ~",chr(92),chr(92),
	                   "\n % ------------------------ \n",sep="")
	
	# Fin de lexecution et retour du resultat
	if(copy.CB){
		Copie.Presse.Papier(Final.str) # Copie le string concatene au presse-papier
	}
	
	if(print.Cons){
		str=cat(Final.str) # Limprime aussi dans la console R
	}
	
	return(Final.str)
}