#
# prepare for BTM on GBM only

library(RPostgreSQL)
library(jsonlite)
library(urltools)
library(curl)

# presumes no quotes in rawtext itself
#
JeffNCIt <- function(rawtext="Temozolomide and glioblastoma and bears, oh my") {
  mystring=sprintf("http://trex.xcures.com:4242/ncitbindings?s=%s",rawtext)
  mystring=URLencode(mystring)
  suppressWarnings(  {
     con = curl( mystring,"r")
    fu=readLines(con)
    close(con)
  }  )

  fromJSON(fu)
}

# added case 100,000 etc that has comma
nufixNumericInequality <- function(criteria) {
  nmbr = "\\b[-+]?([0-9]*[.,])?[0-9]+([eE^][-+]?\\d+)?" 
  neql = 
   ">=|> or =|greater than or equal|<=|< or =|less than or equal|≥|≤|>|<|=|not more than|not less than|more than|less than|not greater than|greater than|at least|at most|equal to|not equal"

  for ( i in nmbr)
    criteria=gsub( i, " !!the_number ", criteria, ignore.case=TRUE)
  for ( i in strsplit(neql,"|",fixed=TRUE)[[1]])
    criteria=gsub( i, " !!math_rel ", criteria, ignore.case=TRUE)
  criteria
}
fixNumericInequality <- function(criteria,justmathrel=TRUE,donumber=TRUE,bangs=TRUE) {
  nmbr = "\\b[-+]?([0-9]*[.,])?[0-9]+([eE^][-+]?\\d+)?" 
  neql = c(  ">=","greater_equal",  "> or =","greater_equal",  "greater than or equal","greater_equal",
	     "<=","lesser_equal",   "< or =","lesser_equal",   "less than or equal"   ,"lesser_equal" ,
             "≥","greater_equal", "at least","greater_equal",  "not less than","greater_equal",
	     "≤","lesser_equal",  "at most", "lesser_equal",   "not more than","lesser_equal",
	     "not below","greater_equal"   ,"not above","lesser_equal",   "not greater than","lesser_equal",
	     "<","less_than",  ">","more_than",  "more than","more_than",  "less than","less_than",
             "greater than","more_than",  "=","equal_to",  "equal to","equal_to",
	     "!=","not_equal","not equal","not_equal"
	     )
  neql = matrix(neql,length(neql)/2,2,byrow=TRUE)


  if (donumber) {
    if (bangs) the_number=" !!the_number " else the_number=" the_number "

    criteria=gsub( nmbr, the_number, criteria, ignore.case=TRUE)
  }
  rpl = "math_rel"
  for ( i in 1:dim(neql)[1] ) {
    if ( !justmathrel ) rpl=neql[i,2]
    if (bangs) RPL=sprintf(" !!%s ",rpl) else  RPL=sprintf(" %s ",rpl)
    criteria=gsub(neql[i,1], RPL, criteria, ignore.case=TRUE)
  }
  criteria
}


# clean up things that cause unexpected failures in trex Norman API
#  Better splitting 7/17/20 for CTG which has \n\n as actual sentence ending.
#
preprossest <- function(dirty_text, grab_numbers_first=TRUE,doublelinefeeds=TRUE) {
  if (grab_numbers_first) 
	  dirty_text = nufixNumericInequality(dirty_text)
  clean_text = gsub("&"," and ",dirty_text,fixed=TRUE)
  clean_text = gsub(",",", ",clean_text,fixed=TRUE)
  clean_text = gsub(".",". ",clean_text,fixed=TRUE)
  clean_text = fixhyphens(clean_text)
  if (doublelinefeeds) gg = "\n\n|;|\\."
  else                 gg = "\n|;|\\."
  multi_text = strsplit(clean_text, gg )[[1]]
  multi_text[nchar(multitext) > 2)]
}

fixhyphens <- function(dirty_text) {
  clean_text = gsub("-"," ",dirty_text,fixed=TRUE)
  # -- may want to also try deleting the hyphen, keeping multiple verions of the term (?????)
  clean_text
}

letsdoit <- function(thecorpus,priorder=priority,ignore=drop_terms,getmore=add_terms,
		     keepitall=TRUE,hdlf=TRUE) {
  one=preprossest(thecorpus,TRUE,hdlf)
  for (i in 1:length(one)) 
	  one[i] = nufixCriterOrig(one[i],priorder,ignore=ignore,getmore,TRUE)
  one
}

fixMore <- function( atext, aflag) {
  if ( ! grepl(aflag, atext, ignore.case=TRUE) ) return(atext)
  gsub(sprintf("\\b%s\\b",aflag),sprintf("!!%s",aflag),atext)
  atext
}

nufixCriterOrig <- function(criteria=fromthis,priorder=priority,ignore=list(),
			    getmore=list(),keepitall=TRUE) {
 dothis=unique( JeffNCIt(criteria) )
 if (length(dothis) < 1) return(criteria)
 for ( i in 1:length(dothis)) {
   if (dothis[[i]][1] %in% ignore) next
   if (keepitall) what="take anything"
   else           what = pull_type(dothis[[i]],priorder)
   if (nchar(what)>0) criteria= gsub( dothis[[i]][1], sprintf(" !!%s ",dothis[[i]][1]), criteria, ignore.case=TRUE)
 }
 if ( length(getmore) > 0)
    for (i in 1:length(getmore)) {
      criteria= fixMore(criteria,getmore[i])
    }
 criteria
}

# # # # #
if (FALSE) {
drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname="aact",host="aact-db.ctti-clinicaltrials.org", 
                      port=5432, user="pattripos", password="newxCures#2019")
ctia = dbGetQuery(con,
		"select nct_id,criteria from eligibilities
				where nct_id in (select distinct nct_id from browse_conditions 
				  where downcase_mesh_term like '%glioblastoma%') ;")

fo = file("get_ctg_gbm_btm.txt","w")
writeLines("NCT_id\tInEx\tPara",fo)
for (i in 1:dim(ctia)[1]) {
  hold=gsub('"',"",ctia$criteria[i])
  foo= regexpr("Exclusion",hold,ignore.case=TRUE)
  if (foo[1] < 0)
        writeLines(sprintf('%s\tIn\t"%s"',ctia[i,1],hold),fo)
  else 
       {writeLines(sprintf('%s\tIn\t"%s"',ctia[i,1],substr(hold,1,foo[1]-1)),fo) 
        writeLines(sprintf('%s\tEx\t"%s"',ctia[i,1],substr(hold,foo[1],nchar(hold))),fo) }
}
close(fo)

hotcha=read.table("get_ctg_gbm_btm.txt",header=TRUE,as.is=TRUE,quote='"',comment.char="")

} # protected

corpusBTM <- function( bigarr=hotcha,priorder=priority, ignore=drop_terms,getmore=add_terms,
		      keepitall=TRUE,hasdoublelinefeeds=TRUE,start=1) {
  fo= file("cBTM.txt","w")
  for (i in start:dim(bigarr)[1]) {
    if (nchar(bigarr[i,3])==0) next
    ga=letsdoit(bigarr[i,3],priorder,ignore,getmore,keepitall,hasdoublelinefeeds)
    foo = gregexpr("!![A-Z,0-9]* ",ga)
    if (length(foo) < 2) next
    for(j in 1:length(foo)) {
       if (foo[[j]][1] == -1) next
       for (k in 1:length(foo[[j]])) {
          aterm = substr(ga[j],foo[[j]][k]+2,foo[[j]][k]+attr(foo[[j]],"match.length")[k]-2)
          if (nchar(aterm)==0) next
          writeLines(sprintf("%s_%s_%06d\t%s",bigarr[i,1],bigarr[i,2],j,aterm),fo)
       }
    }  
  }
  close(fo)
}

if (FALSE) {

  set.seed(314159); 
   allGBM25bkg=BTM(as.data.frame(allGBM), k = 25, alpha = 1, beta = 0.01, iter = 100, trace = TRUE, background=TRUE)
set.seed(314159); 
   allGBM25=BTM(as.data.frame(allGBM), k = 25, alpha = 1, beta = 0.01, iter = 100, trace = TRUE, background=FALSE)
terms(allGBM25)
terms(allGBM25bkg)
}


