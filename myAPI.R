# this is translated as functioning API via
#   pr <- plumber::plumb("DRug2NCIt_CTG_DM.R")  ; pr$run()
# invoke as http://host:port/drug?s=name
#  where name needs %20 in place of spaces, at least initially
#
#  take a drug name and generate a table row of info from NCIt, CTG and DailyMed
#
library(httr)
library(rjson)
#    library(XML)
library(RPostgreSQL)

query_daily_med <- function(query, by="drugname", ...) {
  res <- list()
  if(by == "drugname") {
    r <- GET(paste0("https://dailymed.nlm.nih.gov/dailymed/services/v2/spls.json?drug_name=", query))
    if (status_code(r) == 200) {
      res <- rjson::fromJSON(content(r, "text"))[["data"]]
    }
    else {
      message(" did not find any drugs ")
    }
  }
  else if(by == "drugclass") {
    message("not yet implemented")
  }

  res
}

# not used in current version - batch mode only
getNCTids <- function( fi = "CT_for_DP_20200305.txt",maxi=10) {
	x=unlist(read.table(fi,header=FALSE))
	if (maxi >0 ) x=x[1:maxi]
	x
}

forSERO <- function(nctid="NCT02563548") {
  studies           =dbGetQuery(con, sprintf("select * from studies           where nct_id='%s';",nctid))
      if (dim(studies)[1] < 1) return(list(failure=sprintf("%s not found",nctid)))
  conditions        =dbGetQuery(con, sprintf("select * from conditions        where nct_id='%s';",nctid))
  interventions     =dbGetQuery(con, sprintf("select * from interventions     where nct_id='%s';",nctid))
  eligibilities     =dbGetQuery(con, sprintf("select * from eligibilities     where nct_id='%s';",nctid))
  sponsors          =dbGetQuery(con, sprintf("select * from sponsors          where nct_id='%s';",nctid))
  facilities        =dbGetQuery(con, sprintf("select * from facilities        where nct_id='%s';",nctid))
  overall_officials =dbGetQuery(con, sprintf("select * from overall_officials where nct_id='%s';",nctid))
  outcomes          =dbGetQuery(con, sprintf("select * from outcomes          where nct_id='%s';",nctid))
  outdesigns        =dbGetQuery(con, sprintf("select * from design_outcomes   where nct_id='%s';",nctid))
  #  assume multiples possible except for studies and eligibilities

  inclexcl = split_eligibilities(eligibilities[1,]$criteria)
  spnsrcol = split_sponsors(sponsors)
  primsecd = split_outcomes(outdesigns)

# placeholder
  return(list(studies=studies,conditions=conditions,interventions=interventions,eligibilities=eligibilities,
	      sponsors=sponsors,facilities=facilities,overall_officials=overall_officials,outcomes=outcomes,
	      inclexcl=inclexcl, spnsrcol=spnsrcol,primsecd=primsecd,outdesigns=outdesigns))
}

split_eligibilities  <- function(els) {
  goo = regexpr("Inclusion Criter",els,ignore.case=TRUE) ; if (goo<1) goo=1
  foo = regexpr("Exclusion Criter",els,ignore.case=TRUE)
  if (foo < 1) return( list(incl=substr(els,goo,nchar(els)),excl=""))
  foo=foo[[1]]
  return(list( incl=substr(els,goo,foo-1), excl=substr(els,foo,nchar(els))  ) )
}

split_sponsors  <- function(sps) {
  #  grps = c("lead","collaborator")
  spns = sps[sps$lead_or_collaborator=="lead",]
  colb = sps[sps$lead_or_collaborator=="collaborator",]
  return(list(  spns=spns,colb=colb)  )
}

split_outcomes  <-  function(out) {
  #  grps = c("Post-Hoc","Primary","Secondary","Other Pre-specified")
  prim = out[out$outcome_type == "primary",]
  secd = out[out$outcome_type == "secondary",]
  othr = out[out$outcome_type == "other Pre-specified",]
  phoc = out[out$outcome_type == "post-hoc",]  # actually this is only in outcomes table
  
  return(list(  prim=prim,secd=secd,othr=othr,phoc=phoc  )  )
}

banger <- function(mtrx, bar) {
	# remove embedded carriage returns in the string
  mtrx = gsub("\n","~~",mtrx)
  mtrx = gsub("\t","",mtrx)
  paste(mtrx,collapse=bar)
}

loc_banger  <- function(fac,bar="") {
  x=sprintf("%s\n%s\n%s\n%s",fac$name,fac$city,fac$state,fac$zip)
  banger(x,bar)
}

pso_banger  <-  function(pso,bar) {
  x=sprintf("%s\n%s\n%s",pso$measure,pso$description,pso$population)
  banger(x,bar)
}

pi_banger  <-  function(pi,bar) {
  x=sprintf("%s\n%s\n%s",pi$role,pi$name,pi$affiliation)
  banger(x,bar)
}

outSERO  <-  function(nct,one,bar) {
  sprintf(
    '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s',
    nct,
    banger(one$conditions$name,bar),
    banger(one$interventions$name,bar),
    banger(one$studies$phase,""),
    banger(one$studies$official_title,""),
    banger(one$studies$study_type,""),
    banger(one$studies$overall_status,""),
    banger(one$inclexcl$incl,""),
    banger(one$inclexcl$excl,""),
    banger(one$studies$start_month_year,""),
    banger(one$studies$primary_completion_month_year,""),
    banger(one$studies$completion_month_year,""),
    loc_banger(one$facilities,bar),
    banger(one$spnsrcol$spns$name,bar),
    banger(one$spncrol$lcolb$name,bar),
    banger(one$studies$enrollment,""),
    pso_banger(one$primsecd$prim,bar),
    pso_banger(one$primsecd$secd,bar),
    pso_banger(one$primsecd$othr,bar),
    sprintf("http://www.clinicaltrials.gov/show/%s",nct),
    pi_banger(one$overall_officials,bar)
	   )  # end sprintf)
}

read_downloadof_NCIT  <- function(filename="Thesaurus.txt") {
  ths=read.table(filename,header=FALSE,sep="\t",comment.char="",quote="",as.is=TRUE)
  colnames(ths) = c("code","concept_name","parents","synonyms","definition","display_name","concept_status","semantic_type")
  return(ths)
}

get_drug_gene  <-  function(ths=ths) {
  gene=ths[grepl("Gene or Genome",ths$semantic_type),]
  drug=ths[grepl("Pharmacologic Substance",ths$semantic_type) | grepl("Immunologic Factor",ths$semantic_type),]
  return(list(gene=gene,drug=drug))
}

lotsofextracts  <- function(names,ths=ths) {
	extr = list()
	for (d in names) {
		qz= list(name=d       ,matchname="",ncit_code="",descr="",syn="",genlist=list())
		tryCatch( {qz=extractfromNCIT(d,ths) },
		       	error=function(e) {
				print(sprintf("names with backslash fail: %s",d)) } )
		
		extr = append(extr,list(qz))
	}
	gaga=data.frame(Reduce(rbind, extr))
# write.table(as.matrix(gaga),file="drug2NCIT.txt",quote=TRUE,sep="\t",row.names=FALSE)
	return(gaga)
}

extractfromNCIT <- function(drugname, alldrug=drug, seekgenes=FALSE,allgene=gene) {
  thevec = alldrug[tolower(drugname)==tolower(alldrug$display_name),]
  if (dim(thevec)[1] != 1) {
   hold= findinsynonyms(drugname,alldrug)
   if (hold>0) thevec=alldrug[hold,]
   else {
   print(sprintf("FAIL with %d matches instead of 1 match to %s",dim(thevec)[1],drugname))
   return(list(name=drugname,matchname="",ncit_code="",descr="",syn="",genlist=list())) }
  }
  code=thevec[1,'code']
  matchname=thevec[1,'display_name']
  descr=thevec$definition
  syn = thevec$synonyms
  if ( !seekgenes ) 
    return(list(name=drugname,matchname=matchname,ncit_code=code,descr=descr,syn=syn,genlist=list()))

  genlist = spotgenes(thevec[1,'definition'], allgene)
  return(list(name=drugname,matchname=matchname,ncit_code=code,descr=descr,syn=syn,genlist=genlist))
}

findinsynonyms  <- function(name,fama) {
  abit= which(grepl(sprintf("\\|%s\\|",name),sprintf("|%s|",fama$synonyms),ignore.case=TRUE))
  if (length(abit)>0) return(abit[[1]])
  return(0)
}



# "DRug2NCIt_CTG_DM.R`

#' Echo the parameter that was sent in
#' @param msg The message to echo back.
#' @get /echo
function(msg=""){
  list(msg = paste0("The message is: '", msg, "'"))
}

#' Plot out data from the iris dataset
#' @param spec If provided, filter the data to only this species (e.g. 'setosa')
#' @get /plot
#' @png
function(spec){
  myData <- iris
  title <- "All Species"

  # Filter if the species was specified
  if (!missing(spec)){
    title <- paste0("Only the '", spec, "' Species")
    myData <- subset(iris, Species == spec)
  }

  plot(myData$Sepal.Length, myData$Petal.Length,
       main=title, xlab="Sepal Length", ylab="Petal Length")
}

#' Generate actual results
#' @param drug 
#' @get /drug
function(s){
  if (missing(s))   return(list(msg="Need ?s=<name>"))
  
ths = read_downloadof_NCIT()

drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname="aact",host="aact-db.ctti-clinicaltrials.org", 
                      port=5432, user="pattripos", password="newxCures#2019")
druglist = s   # later may be a list

for (drug in druglist) {
  DMlink="NA"
  testuery=query_daily_med(drug)
  if (length(testuery) > 0) 
    DMlink=paste(
       "https://dailymed.nlm.nih.gov/dailymed/drugInfo.cfm?setid=", testuery[[1]]$setid,sep="",collapse="")
  one= dbGetQuery(con,sprintf("select distinct c.downcase_mesh_term from browse_conditions c 
      join interventions i 
       on c.nct_id=i.nct_id where lower(i.name)=lower('%s');",drug))
  if (dim(one)[1] >0) one=sort(one[,1])      
  else one="NA"
  conditions = banger(one,"|")
  stuff = extractfromNCIT(drug,ths)
  if (length(stuff)>0) {
    matchname=stuff$matchname
    NCITid=stuff$ncit_code
    synonyms=stuff$syn
    description = stuff$descr }
  else {
    matchname="NA"
    NCITid="NA"
    synonyms="NA"
    description = "NA" }
 }
 dbDisconnect(con)

# modify below to accumulate  multiple drug names inside loop
 list(drug=drug,matchname=matchname,NCITid=NCITid,synonyms=synonyms,
       conditions=conditions,DMlink=DMlink,description=description)

}

