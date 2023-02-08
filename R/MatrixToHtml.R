# Transforme une matrice en chaine html
# Style=matrice(x,y,bgcolor,color,fontweight)
# Ajout du border
#' @export
MatrixToHtml = function(matrix, decimals=3, flip=FALSE, style=NULL,horizontalVector=FALSE,keepMatrix=FALSE,border=FALSE)
{
	# if(!keepMatrix)
	# {
		# matrix=as.data.frame(matrix)
	# }
	#if(horizontalVector){matrix=t(matrix);rownames(matrix)=NULL}
	addTD = function(x)
	{
		x = paste("<td align='center'>",x,"</td>",sep="")
	}

	applyStyle = function(rowStyle)
	{

		x=rowStyle[1]
		y=rowStyle[2]
		textStyle="<td align='center' style='"
		if (rowStyle[3]!="")
		{
			textStyle=paste(textStyle," background-color:",rowStyle[3],";",sep="")
		}
		if (rowStyle[4]!="")
		{
			textStyle=paste(textStyle," color:",rowStyle[4],";",sep="")
		}
		if (rowStyle[5]!="")
		{
			textStyle=paste(textStyle," font-weight:",rowStyle[5],sep="")
		}
		textStyle=paste(textStyle,"'>",sep="")
		tmp=substring(dataframe[x,y],20)
		newText=paste(textStyle,tmp,sep="")
		dataframe[x,y]<<-newText
	}

	# Arrondis
	if(!keepMatrix)
	{
		matrix=as.data.frame(matrix)
		numericCols=sapply(matrix, is.numeric)
		matrix[,numericCols]=round(matrix[,numericCols],decimals)
	}
	if (flip==TRUE)
	{
		matrix=t(matrix)
	}
	if(!keepMatrix)
	{
		dataframe = as.data.frame(matrix)
	}
	else
	{
		dataframe=matrix
	}
	style=style[style$Y %in% colnames(dataframe),]

	if (nrow(dataframe)>1)
	{
		# Transformation en caract?res
		dataframe = apply(dataframe, 2, as.character)
		# Ajout des balises <td> des cellules et du style des cellules
		dataframe = apply(dataframe,2,addTD)
		rownames(dataframe)=rownames(matrix)

	} else
	{
		if(!horizontalVector)
		{
			dataframe[1,]=as.character(dataframe[1,])
			dataframe[1,]=addTD(dataframe)
		}
		# changement pour la fonction questionTable
		if(horizontalVector)
		{
			dataframe=apply(dataframe, 2, as.character)
			dataframe=addTD(dataframe)
		}
		#

	}

	# Style des cellules
	if (!is.null(style))
	{
		apply(style,1,applyStyle)
	}


	# Ajout de la premi?re ligne
	firstLine = paste("<th align='center'>",colnames(matrix),"</th>",sep="")
	dataframe = rbind(firstLine, dataframe)

	# Ajout de la 1ere colonne
	firstCol = c("",rownames(matrix))
	firstCol = paste("<td><strong>",firstCol,"</strong></td>",sep="")
	firstCol[1] = "<th></th>"
	dataframe = cbind(firstCol, dataframe)

	# Ajout des balises <tr> aux lignes
	dataframe=cbind("<tr>",dataframe)
	dataframe=cbind(dataframe,"</tr>")

	# Transformation en vecteur
	vector = as.vector(t(dataframe))

	# Transformation en chaine de caract?res
	string = paste(vector,collapse="")
	# Ajout de la condition border
	if (border==FALSE){
	  string = paste("<table>",string,"</table><br/>",sep="", collapse="")
	}else{
	  string = paste("<TABLE BORDER CELLSPACING=0>",string,"</table><br/>",sep="", collapse="")
	}

	return (string)
}
