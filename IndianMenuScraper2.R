library(XML)
library(rvest)
#setwd("C:\\Users\\MZ\\Documents\\for Daniel")
setwd("C:\\Users\\bermads1\\Documents\\Daniel\\GAN\\spices")
GetHerbsAndSpices=function(url){
  #' scrapes a list of herbs and spices and adds a few
  listofherbsandspices=read_html(url)
  
  listofspices=listofherbsandspices%>%
    html_nodes("table")%>%
    html_nodes("tbody")%>%
    html_nodes("tr")%>%
    html_text()
  
  listofherbsandspices=listofherbsandspices%>%
    html_nodes("table")%>%
    html_nodes("thead")%>%
    html_nodes("tr")%>%
    html_text()
  listofherbsandspices=c(listofspices,listofherbsandspices)
  listofherbsandspices=listofherbsandspices[-grep("^\r\n",listofherbsandspices)]
  listofherbsandspices=gsub("[0-9]{6}\r\n            ","",listofherbsandspices)
  listofbreaks=gregexpr('\\r\n|,|\\(|\\"',listofherbsandspices)
  for(i in 1:length(listofbreaks)){
    listofherbsandspices[i]=substr(listofherbsandspices[i],1,listofbreaks[[i]][1]-1)
  }
  listofherbsandspices=gsub(" $","",listofherbsandspices)
  listofherbsandspices=listofherbsandspices[!duplicated(listofherbsandspices)]
  listofherbsandspices=c(listofherbsandspices,"Chilli","Chilli powder","Salt","Asafoetida")
  return(listofherbsandspices)
}

Getlistofrecipes=function(rootwebpage,endpage=296){
	#Function that scrapes the rootwebpage for the recipes on it. It requires last page of the recipes
	recipetable=data.frame("url_extension"=NULL,"recipe_name"=NULL)
	for(i in 1:endpage){
		#pastes the page number onto the root webpage and then scrapes it
		webpage=paste(rootwebpage,i,sep='')
		indianrecipelist=read_html(webpage)
	
		#This is for scraping the webpage to the the links to the recipe
		listoflinks=indianrecipelist%>%
			html_nodes('div')%>%
			html_nodes('span')%>%
			html_nodes('a')%>%
			html_attr(name="href")
		
		#removes any lines containing http or myreviews and any duplicated lines (starting from the last). 
		listoflinks=listoflinks[!grepl("http|myreviews",listoflinks)]
		listoflinks=listoflinks[!duplicated(listoflinks,fromLast=TRUE)]
		
		#This is for scraping the webpage for the name of the recipe. It removes any lines with specific special characters and key words
		listofrecipes=indianrecipelist%>%
			html_nodes('div')%>%
			html_nodes('span')%>%
			html_nodes('a')%>%
  			html_text()
		listofrecipes=listofrecipes[!grepl("^ |Tarla",listofrecipes)]
		listofrecipes=listofrecipes[!grepl("\\#|\\&|:|,",listofrecipes)]

		#'The rest of the code is used for comparing the list of recipes in listofrecipes with the list of links in listoflinks 
		#' to remove the links that do not correspond to recipes.
		
		#replaces some characters so that they can be used in regular expression matching.
		matchrecipes=gsub(" |, ","-",listofrecipes)
		matchrecipes=gsub("\\(","\\\\(",matchrecipes)
		matchrecipes=gsub("\\)","\\\\)",matchrecipes)
		matchrecipes=gsub("\\_","\\\\_",matchrecipes)
		matchrecipes=gsub("\\{","\\\\{",matchrecipes)
		matchrecipes=gsub("\\}","\\\\}",matchrecipes)
		matchrecipes=gsub("/|\\.|'","",matchrecipes)

		#keeps all of the links that are relevant and removes the rest.
		listoflinks=listoflinks[grep(paste(matchrecipes,collapse="|"),listoflinks)]
		temptable=data.frame("url_extension"=listoflinks,"recipe_name"=listofrecipes)
		recipetable=rbind(recipetable,temptable)
	}
	write.csv(recipetable,"recipetables.csv",row.names=FALSE)
	return(recipetable)
}

GetStarsAndReviews=function(webpage){
	#' A function that takes in a list of links and collects information on what the number and rating of the recipe is. Has not been used yet.
	
	#reads the webpage
	indianrecipe=read_html(webpage)
	
	#grabs the number of stars the recipe has received (not used so far)
	parseofstars=indianrecipe%>%
			html_nodes('div')%>%
			html_nodes('span')%>%
  			html_text()
	num_stars=parseofstars[grep("/5 stars",parseofstars)]
	num_stars=as.integer(gsub("\r\n                |/5 stars ",'',num_stars))

	#grabs the number of reviews the recipe has recieved.
	parseofreview=indianrecipe%>%
			html_nodes('div')%>%
			html_nodes('span')%>%
			html_nodes('a')%>%
  			html_text()

	num_reviews=parseofreview[grep("Review",parseofreview,ignore.case=TRUE)]
	num_reviews=as.integer(gsub(" Reviews| Review",'',num_reviews,ignore.case=TRUE))

	reciperating=data.frame("num_reviews"=num_reviews,"stars"=num_stars)
	return(reciperating)
}

GetIngredients=function(webpage,listofherbsandspices){
	#' A function that takes in a list of links and a list of herbs and spices (listofherbsandspices) and scrapes each link for all of the
	#' ingredients, label the ones that are spices and herbs, and computes the percent of the total spice or herb that a single spice occupies. 
	#' Fresh vs dried are not distinguished so common sense must be used when judging whether it's dried or fresh. Additionally, onions are treated 
	#' as a spice/herb because of onion powder and because of how essential an aromatic it is. The same with garlic.
	
	#reads the webpage
	indianrecipe=read_html(webpage)
	
	#scrapes the html of the recipe page and list of ingredients begins and where the methods begins and takes all the lines in between.
	parseofingredients=indianrecipe%>%
			html_nodes('div')%>%
			html_nodes('div')%>%
			html_nodes('div')%>%
			html_nodes('div')%>%
			html_nodes('span')%>%
			html_text()
	ingredientsLine=grep("ingredients$",parseofingredients,ignore.case=TRUE)
	methodsLine=grep("Method$",parseofingredients,ignore.case=TRUE)
	listofingredients=parseofingredients[(ingredientsLine[1]+1):(methodsLine[1]-1)]
	
	#'Cleaning the text
	#takes all lines that start with a number, contain the words pinch or taste and removes all others. Also removes all lines that do not have a 
	#removes all lines that end in a measurement or contain the word food color or food colour. This is because when scraped, it is often, not not always the case 
	# that a single ingredient is split up into three lines. Example: "1 tsp of cumin" will become c("1 tsp cumin", "1 tsp", "cumin"). However, sometimes it is only 
	# split in two. Therefore, we just remove any lines that do not contain a measurement and an ingredient. Also removes blank lines.
	listofingredients=listofingredients[grep("^[0-9/]+|pinch|taste",listofingredients)]
	listofingredients=listofingredients[!grepl("tsp$|cup$|cups$|tbsp$|[0-9]$|food colour|food color",listofingredients)]
	#listofingredients=listofingredients[!grepl("",listofingredients)]
	listofingredients=listofingredients[!grepl("for serving",listofingredients,ignore.case=TRUE)]
	listofingredients=listofingredients[!grepl("for the|^garlic |^For |to be|ingredients|^slit",listofingredients,ignore.case=TRUE)]	
	listofingredients=listofingredients[!grepl("mustard oil",listofingredients,ignore.case=TRUE)]	
	
	#cleaning the text removes anything in parenthesis, anything after the word soaked, the word whole, few, the phrase "tsp pinch" (this was due to a specific instance
	# and had to be removed to not cause errors, it also makes no sense), any length measurement lines should be removed, the phrase "[0-9] ginger" was replaced with 2 tsp ginger because the original makes no sense, "  asafoetida  a pinch" is replaced with "a pinch asafoetida", and the word strips was removed because it does not contain any relevant information but can cause confusion.
	listofingredients=gsub("\\([a-z /-]{1,}\\)","",listofingredients,ignore.case=TRUE)
	listofingredients=gsub("[, ]*soaked in [a-z0-9 ]+|[, ]*dissolved in [a-z0-9 ]+","",listofingredients,ignore.case=TRUE)
	listofingredients=gsub("tsp pinch","tsp",listofingredients)
	listofingredients=gsub(" few| whole","",listofingredients)
	listofingredients=listofingredients[!grepl("[0-9.]+ mm$",listofingredients)]
	listofingredients=gsub(' piece | strips ',' ',listofingredients)
	listofingredients=gsub("  asafoetida  a pinch","a pinch asafoetida",listofingredients)
	listofingredients=gsub("big|small","",listofingredients)
	listofingredients=gsub(" rsp | tso "," tsp ",listofingredients)
	listofingredients=gsub(" pods [a-z ]*garlic| garlic pieces| garlic| fresh green garlic "," garlic cloves",listofingredients)
	listofingredients=gsub("^to taste ([a-z]+)","\\1 to taste ",listofingredients)
	listofingredients=gsub(" for taste| per taste| per your taste| to your taste| at taste"," to taste",listofingredients)
	listofingredients=gsub("- according to your taste","to taste ",listofingredients)
	listofingredients=gsub("saltto taste|salt taste","salt to taste",listofingredients)
	listofingredients=gsub("2 garam masala","2 tsp garam masala",listofingredients)
	listofingredients=gsub("piches","pinches",listofingredients)
	listofingredients=gsub("pepperto","pepper to",listofingredients)
	listofingredients=gsub("1/21/4","3/4",listofingredients)
	listofingredients=gsub("1 1/0.5/4 tsp tsp","1",listofingredients)
	listofingredients=gsub("1/0/2","1.5",listofingredients)
	listofingredients=gsub("tsp tsp","tsp",listofingredients)
	
	#Checks if there are any lines left in the recipe. if there are not, it returns a null data.frame
	if(length(listofingredients)==0){
	  tableofingredients=data.frame("all"=NULL,"measurement"=NULL,"ingredient"=NULL,"spice"=NULL,"ratio"=NULL)
	  return(tableofingredients) 
	}
	
	#This batch of code looks for any instance in the number of inches are used to describe something like cinnamon. If there is, it replaces any mm measurement, and takes the 
	#inches measurement and replaces it with the number of inches. This relates to 1 inch of cinnamon = 1tsp of cinnamon.
	inches=grepl('[\\(]*[0-9.]+[ ]*(\"|inch)+[\\)]*',listofingredients)
	if(any(inches)){
	  listofingredients[inches]=gsub("[0-9./]+ mm[.]*","",listofingredients[inches])
	  listofingredients[inches]=gsub("\\(|\\)","",listofingredients[inches])
	  listofingredients[inches]=gsub('[\\(]*([0-9.]+)[ ]*(\"|inch)+[\\)]*','\\1',listofingredients[inches])
	}
	
	kg=grepl('[\\(]*[0-9.]+[ ]*\ kg [\\)]*',listofingredients)
	if(any(kg)){
	  listofingredients[cm]=gsub('([0-9.])+ kg','1000*\\1',listofingredients[cm])
	  locations=regexpr("[0-9./*]+",listofingredients[kg])
	  newval=eval(parse(text=substr(listofingredients[kg],locations,attr(locations,"match.length")))) 
	  listofingredients[kg]=paste(newval,substr(listofingredients[cm],attr(locations,"match.length")+1,nchar(listofingredients[kg])),'gms',sep='')
	}
	
	cm=grepl('[\\(]*[0-9.]+[ ]*\ cm [\\)]*',listofingredients)
	if(any(inches)){
	  listofingredients[cm]=gsub('([0-9.])+ cm','2.5*\\1',listofingredients[cm])
	  locations=regexpr("[0-9./*]+",listofingredients[cm])
	  newval=eval(parse(text=substr(listofingredients[cm],locations,attr(locations,"match.length")))) 
	  listofingredients[cm]=paste(newval,substr(listofingredients[cm],attr(locations,"match.length")+1,nchar(listofingredients[cm])),sep='')
	}
	
	#This for for cleaning any potential changes, removing multiple successive spaces and lines that end and begin with spaces.	
	listofingredients=gsub("^ | $","",listofingredients)
	listofingredients=gsub(" {2,}"," ",listofingredients)
	
	#Identifies any lines that contain the word pinch and replaces it with the number and 1/4 tsp. It also replaces instances of "a pinch" with "1 pinch"
	pinches=grep("pinch",listofingredients,ignore.case=TRUE)
	sizeofpinch=gregexpr("[0-9]+|a pinch|^pinch",listofingredients[grep("pinch",listofingredients,ignore.case=TRUE)])
	listofingredients=gsub(" of "," ",listofingredients)
	
	if(length(sizeofpinch)>0){
	  for(j in 1:length(sizeofpinch)){
	    if(grepl(" to ",listofingredients[pinches[j]])){
	      pinchNumbers=unlist(strsplit(listofingredients[pinches[j]]," to "))
	      pinchNumbers=mean(as.numeric(gsub("[a-z ]",'',pinchNumbers)))
	      listofingredients[pinches[j]]=gsub('[0-9] to [0-9]',pinchNumbers,listofingredients[pinches[j]])				
	      sizeofpinch=gregexpr("[0-9.]+|a pinch",listofingredients[grep("pinch",listofingredients,ignore.case=TRUE)])
	    }
	    size=substr(listofingredients[pinches[j]],sizeofpinch[[j]][1],sizeofpinch[[j]][1]+attr(sizeofpinch[[j]],"match.length"))
	    size=gsub("^pinch","a pinch",size)
	    size=gsub("pinch","",size)
	    size=gsub(" ","",size)
	    size=gsub("a","1",size)
	    listofingredients[pinches[j]]=gsub("[0-9]+ pinch|a pinch|^pinch",paste(as.integer(size),'/4 tsp',sep=''),listofingredients[pinches[j]])
	  }
	}
	
	#Identifies any lines that contain "to taste" with the number and 1/4 tsp.	
	istotaste=grepl("to taste", listofingredients,ignore.case=TRUE)
	if(any(istotaste)){
	  listofingredients[which(istotaste)]=gsub("([a-z -]{1,} to taste)","1/4 tsp \\1",listofingredients[which(istotaste)])
	  listofingredients=gsub(" to taste","",listofingredients)
	}
	
	
	#Replaces certain plural or certain spacings of words with alternative forms to make it more standardized.
	listofingredients=gsub("leaf","leaves",listofingredients)
	listofingredients=gsub("cups","cup",listofingredients)
	listofingredients=gsub("tbsp","tbp",listofingredients)
	listofingredients=gsub("tspes","tsp",listofingredients)
	listofingredients=gsub("baylea","bay lea",listofingredients)
	listofingredients=gsub("clove ","cloves ",listofingredients)
	listofingredients=gsub("stick cinnamon|sticks cinnamon|stick of cinnamon|sticks of cinnamon","cinnamon sticks",listofingredients)
	listofingredients=gsub("(^[0-9]+[ black]* cardamom[s]*$)","\\1 pods",listofingredients)
	listofingredients=gsub("broken to","broken",listofingredients)
	listofingredients=gsub("flakes garlic","garlic cloves",listofingredients)
	listofingredients=gsub("1 (lemon)* size ","1/4 cup",listofingredients)
	listofingredients=gsub(" bunch | handful | tin | bowl | bowls "," cup ",listofingredients)
	listofingredients=gsub("1 recipe coconut-coriander masala","1.5 cup masala blend",listofingredients)
	listofingredients=gsub("1/2 turmeric","1/2 tsp turmeric",listofingredients)
	
	#creates a dataframe that contains all the information necessary
	tableofingredients=data.frame('all'=listofingredients)
	tableofingredients$all=as.character(tableofingredients$all)
	
	#splits ingredient sentence into two parts. The first is the measurement and the second is the ingredient. 
	splitpoint=regexpr("([0-9]/.[0-9]+)+|[0-9]/*[0-9]* to [0-9]/*[0-9]*|cup|tsp|tbp|stick|pod|piece|whole|clove|onion",listofingredients)
	splitpoint=splitpoint+attr(splitpoint,"match.length")
	tableofingredients$measurement=substr(listofingredients,1,splitpoint)
	tableofingredients$measurement=gsub("whole","",tableofingredients$measurement)
	tableofingredients$ingredient=substr(listofingredients,splitpoint,nchar(listofingredients))
	
	tableofingredients$spice=''
	
	#Handles instances where the recipe calls for "x to y" number of ingredients and it takes the average of them and just leaves it as such. Units are kept
	integermeasurement=grep(" to ",tableofingredients$measurement)
	if(length(integermeasurement)>0){
	  #Finds instances where " to " exist and removes extra spaces at the end of the text
	  splitnumbers=strsplit(tableofingredients$measurement[integermeasurement]," to ")
	  splitnumbers[[1]]=gsub(" $","",splitnumbers[[1]])
	  
	  #uses the function eval(parse(text=gsub(" ","+",x))) to remove any instance of " to " and inserts a "+", parses and evaluates it and then takes the mean and inserts it.
	  splitnumbers[[1]]=unlist(sapply(splitnumbers[[1]],function(x) eval(parse(text=gsub(" ","+",x)))))
	  newnumbers=unlist(lapply(splitnumbers,function(x) mean(as.integer(x))))
	  tableofingredients$measurement[integermeasurement]=unlist(lapply(splitnumbers,function(x) mean(as.integer(x))))
	  for(m in 1:length(integermeasurement)){
	    tableofingredients$all[integermeasurement[m]]= gsub("[0-9]+ to [0-9]+", newnumbers[m],tableofingredients$all[integermeasurement[m]])
	  }
	}
	
	
	
	#Takes specific key ingredients that might have specific units, such as leaves or sticks, and replaces those spice specific measurements with a volumetric equivalent.
	spice=c('tamarind',"rose",'nutmeg','coriander','basil', "star anise",'ginger','fenugreek seeds',"mace",'black pepper|white pepper|green pepper','saffron','onion',"cinnamon",'cardamom','clove',"bay leaves","curry leaves","chilli","dr[iedy]+[a-z ]+chilli","garlic clove",'gms',' ml ')
	unit=c('',"",'','','',"", '','','','', 'strands','','stick',"",'','leaves',"leaves","","","",'','')
	volume=c('1 tbp',"1/8 tsp",'2 tsp','1/2 tbp','1/4 tsp',"1/2 tsp",'2 tsp',"1/64 tsp",'1/2 tsp','1/64 tsp',"1/6 tsp","1 cup",'1/2 tsp',"1/20 tsp",'1/12 tsp','1/5 tsp',"1/8 tsp","1 tbp", "1 tsp",'1/2 tsp',"1/5 tsp",'1/5 tsp')
	tableofingredients$all=as.character(tableofingredients$all)
	tableofingredients$ingredient=as.character(tableofingredients$ingredient)
	for(k in 1:length(unit)){
	  
	  #identifies all instances of the unit and the spice appearing without a volumetric unit already in place.
	  containsunit=grepl(unit[k],tableofingredients$all,ignore.case=TRUE)
	  hasAUnitAlready=!grepl("tsp|tbp|cup",tableofingredients$all)
	  containsunit=containsunit&hasAUnitAlready
	  if(any(containsunit)){
	    #checks of that spice is there
	    containsspice=grepl(spice[k],tableofingredients$all)
	    if(any(containsunit&containsspice)){
	      
	      #in the instance it's looking for dried chillies, it replaces tbsp with tsp to avoid tripling the amount of dried chillies.
	      if(spice[k]=="dr[iedy]+[a-z ]+chilli"){
	        tableofingredients$measurement[containsunit&containsspice]=gsub("tbp","",tableofingredients$measurement[containsunit&containsspice])
	      }
	      #finds all numbers in the spice ingredient and splits them so that you get the specific number. Then it replaces them in ingredient and removes extra spaces, and numbers in the ingredients column.
	      numSpice=gregexpr("[0-9./]+",tableofingredients$all[containsunit&containsspice])
	      allNumSpices=lapply(numSpice,function(x) substr(tableofingredients$all[containsunit&containsspice],x,(attr(x,"match.length")+x)))
	      properNumSpices=c()
	      for(n in 1:length(allNumSpices)){
	        properNumSpices=c(properNumSpices,allNumSpices[[n]][n])
	      }
	      tableofingredients$measurement[containsunit&containsspice]=properNumSpices
	      tableofingredients$measurement[containsunit&containsspice]=gsub(" ",'',tableofingredients$measurement[containsunit&containsspice])
	      tableofingredients$measurement[containsunit&containsspice]=
	        paste(tableofingredients$measurement[containsunit&containsspice],
	              substr(volume[k],2,nchar(volume[k])),sep='')
	      tableofingredients$ingredient[containsunit&containsspice]=gsub("[0-9]|^ |","",as.character(tableofingredients$all[containsunit&containsspice]))
	    }
	  }
	}
	
	#goes through the spices in the listofherbsandspices in the spices column, puts the specific herb or spice
	for(i in 1:length(listofherbsandspices)){
	  checkspice=grepl(listofherbsandspices[i],tableofingredients$all,ignore.case=TRUE)
	  if(any(checkspice)){
	    tableofingredients$spice[checkspice]=listofherbsandspices[i]
	  }	
	}
	
	#removes all rows that do not contain a valid spice or herb
	tableofspices=tableofingredients[!grepl("^$",tableofingredients$spice),]
	
	#Computes the volumnetric ratio of the spices and herbs
	
	#finds a split point for the measurements based on the three units: cup, tablespoon, teaspoon" and subtracts 2 (so as to split before the volume unit.
	splits=unlist(gregexpr("tsp|tbp|cup",as.character(tableofspices$measurement)))-2
	#on each measurement, split the measurement so we have the number and unit. Then, replace the unit with a multiplication factor. 
	sizeofIngredient=mapply(as.character(tableofspices$measurement),FUN=substr,rep(0,length(splits)),splits)
	unit=mapply(as.character(tableofspices$measurement),FUN=substr,splits+2,splits+4)
	unit[unit=="cup"]=3*16
	unit[unit=="tbp"]=3
	unit[unit=="tsp"]=1
	unit=as.integer(unit)
	unit[is.na(unit)]=0
	sizeofIngredient=gsub("^$", "0",sizeofIngredient)
	measurementasNumber=unlist(sapply(sizeofIngredient,function(x) eval(parse(text=gsub(" ","+",x)))))
	tableofingredients$ratio=0
	tableofingredients$ratio[!grepl("^$",tableofingredients$spice)]=unit*measurementasNumber/sum(unit*measurementasNumber)
	
	
	#if the recipe calls for ginger-garlic paste, it splits the volume in half and uses 1/2 for ginger and 1/2 for garlic
	pairs=c("ginger-garlic","ginger-[ a-z]* chilli","garlic-[ a-z]* chilli","coriander-cumin","salt and [ a-z]* pepper")
	replace=c("Garlic","Ginger","Garlic","Coriander","Pepper")
	for(i in 1:length(pairs)){
	  if(sum(grepl(pairs[i],tableofingredients$all))%%2==1){
	    if(any(grepl(pairs[i],tableofingredients$all))){
	      tableofingredients=rbind(tableofingredients,tableofingredients[grepl(pairs[i],tableofingredients$all),])
	      tableofingredients$ratio[grepl(pairs[i],tableofingredients$all)]=tableofingredients$ratio[grepl(pairs[i],tableofingredients$all)]/2
	      tableofingredients$spice[grep(pairs[i],tableofingredients$all)[2]]=replace[i]
	    }
	  }
	}
	tableofingredients=tableofingredients[tableofingredients$spice!="",]
	options(warn=2)	
	return(tableofingredients)
}

url="https://www.spicesinc.com/t-list-of-spices.aspx"
#listofherbsandspices=GetHerbsAndSpices(url)
listofherbsandspices=as.character(read.table("listofherbsandspices.txt")$V1)

rootwebpage="https://www.tarladalal.com/recipes-for-indian-2?sort-7&pageindex="
#recipetable=Getlistofrecipes(rootwebpage)
load("recipebook.RData")

recipetable=read.csv("recipetables.csv")
# recipebook=list()
# recipereviews=list()
# recipenumbers=list()
basewebpage="https://www.tarladalal.com/"
pb <- txtProgressBar(title = "progress bar", min = 0,
                    max = 1, style = 3)

#1632,1943,2385,2435,2490,2517,2611,2975,2991,3021,3026,3027,3028, was a bad recipe without any measurements.
for(pg in 2365:dim(recipetable)[1]){
	webpage=paste(basewebpage,recipetable$url_extension[pg],sep='')
	if(any(pg==c(1632,1943,2385,3435,2490,2517,2611,2975,2991,3021,3026,3027,3028))){
		recipebook[[pg]]=NULL
		recipereviews[[pg]]=NULL
		recipenumbers[[pg]]=NULL
		next
	}
	recipeanalysis=tryCatch(GetIngredients(webpage,listofherbsandspices),error=function(e) e)
	if (all(class(recipeanalysis)=="data.frame")){
	  recipequality=GetStarsAndReviews(webpage)
		
	  recipebook[[pg]]=recipeanalysis
		recipereviews[[pg]]=recipequality$stars
		recipenumbers[[pg]]=recipequality$num_reviews
		
	}else if (recipeanalysis$message=="HTTP error 500."){
		recipebook[[pg]]=NULL
		recipereviews[[pg]]=NULL
		recipenumbers[[pg]]=NULL
	}		
	setTxtProgressBar(pb, pg/dim(recipetable)[1])

}
save(recipebook, recipereviews, recipenumbers,file="recipebook_blanksremoved 20180109 155000.RData")

numSpices=unlist(lapply(recipebook,function(x) length(row.names(x))))
emptyrecipes=numSpices>0
recipebook=recipebook[emptyrecipes]
recipereviews=recipereviews[emptyrecipes]
recipenumbers=recipenumbers[emptyrecipes]

#recipebookerrors=recipebook[which(unlist(lapply(recipebook,function(x) any(is.na(x)))))]
#recipebook[which(unlist(lapply(recipebook,function(x) any(is.na(x)))))]=recipebookerrors
#recipebook[[3278]]$ratio=1
spiceratiobook=list()
for( i in 1:length(recipebook)){
  x=recipebook[[i]]
  if(any(grepl('chilli powder',x$all))){
    x[grepl('chilli powder',x$all),]$spice="Chilli powder"
  }
  recipebook[[i]]=x
  x$spice=tolower(x$spice)
  spiceratiobook[[i]]=aggregate(formula=ratio~spice,data=x,FUN=sum)
}
save(spiceratiobook,recipebook, recipereviews, recipenumbers,file="spiceratiobook_blanksremoved 20180109 155200.RData")

#
#
#
#
listofspices=unique(unlist(lapply(spiceratiobook,function(x) unique(x$spice))))
alldata=data.frame(matrix(0,ncol=length(listofspices)))
names(alldata)=listofspices
alldata=alldata[-1,]

for(i in 1:length(spiceratiobook)){
  tempframe=data.frame(matrix(0,ncol=length(listofspices)))
  names(tempframe)=listofspices
  for(j in 1:dim(spiceratiobook[[i]])[1]){
    whichcolumn=grep(paste("^",spiceratiobook[[i]]$spice[j],'$',sep=""),listofspices, ignore.case=TRUE)
    tempframe[whichcolumn]=spiceratiobook[[i]]$ratio[j]
  }
  alldata=rbind(alldata,tempframe)
}
alldata$cinnamon=alldata$`cinnamon powder`+alldata$`cinnamon sticks`
cinnamoncolumns=grep(paste(c('cinnamon powder', 'cinnamon sticks'),collapse="|"),names(alldata))
alldata=alldata[,-cinnamoncolumns]

for(i in 1:length(recipereviews)){
  if(length(recipereviews[[i]])==0){
    recipereviews[[i]]=-1
  }
  if(length(recipenumbers[[i]])==0){
    recipenumbers[[i]]=-1
  }
}
write.csv(alldata,"spiceratio.csv",row.names=FALSE)

# recipe_Numstars=unlist(recipereviews)
# recipe_Numreviewers=unlist(recipenumbers)
# alldata$Star_rating=recipe_Numstars
# alldata$Num_reviews=recipe_Numreviewers
# 
# sum(alldata$Num_reviews<1)
