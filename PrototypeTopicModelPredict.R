# prototypical code to implement BTM topic model scoring on new text
#
library(BTM)

source("JeffNCIt.R")

priority = c("neoplastic_process" ,
	     "pharmacologic_substance" ,
	     "biologically_active_substance",
	     "immunologic_factor",
	     "clinical_attribute",
	     "finding",
	     "pathologic_function",
	     "antibiotic",
	     "receptor",
	     "molecular_function",
	     "gene_or_genome",
	     "genetic_function",
	     "diagnostic_procedure",
	     "therapeutic_or_preventive_procedure",
	     "disease_or_syndrome",
	     "tissue",
	     "laboratory_or_test_result",
	     "laboratory_procedure",
	     "sign_or_symptom",
	     "body_part__organ__or_organ_component",
	     "body_location_or_region",
	     "health_care_activity",
	     "quantitative_concept",
	     "organism_attribute",
	     "virus"   
)


add_terms = c("CHILDBEARING","TNBC","HNSCC","GEJ","RECURRENCE","DIABETIC")

# # # since the model is built w/o them we do not have to actully drop them 
drop_terms = c("PATIENTS","INCLUSION","EXCLUSION","CRITERIA")

recall_btm_model <- function(gbm_rdata="nub_every_model.Rdata") { load(gbm_rdata); nub_every_model }

# clean up things that cause unexpected failures in trex Norman API
#
preprossest <- function(dirty_text, grab_numbers_first=TRUE) {
  if (grab_numbers_first) 
	  dirty_text = nufixNumericInequality(dirty_text)
  clean_text = gsub("&"," and ",dirty_text,fixed=TRUE)
  clean_text = gsub(",",", ",clean_text,fixed=TRUE)
  clean_text = gsub(".",". ",clean_text,fixed=TRUE)
  clean_text = fixhyphens(clean_text)
  multi_text = strsplit(clean_text,"[\n.;]")[[1]]
  multi_text
}

fixhyphens <- function(dirty_text) {
  clean_text = gsub("-"," ",dirty_text,fixed=TRUE)
  # -- may want to also try deleting the hyphen, keeping multiple verions of the term (?????)
  clean_text
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

#    ignore = c("PATIENTS","INCLUSION","EXCLUSION","CRITERIA")

letsdoit <- function(thecorpus,priorder=priority,ignore=drop_terms,getmore=add_terms,keepitall=TRUE) {
  one=preprossest(thecorpus,TRUE)
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

orignPara <- function(criteria,priorder=priority,igme=ignore,num1=TRUE,keepitall=TRUE) {
  if (is.na(criteria)) return("")
  one = nufixNumericInequality(criteria)
  one = nufixCriterOrig(one,priorder,igme,keepitall)
  if (is.na(one)) return("")
  h2=strsplit(one," ")[[1]]
  h3=gsub("!!","",h2[grepl("!!",h2)])
  if (num1 & (length(h3)>1) & h3[1]=="the_number")
     h3=h3[2:length(h3)]
  paste(gsub("[^_a-zA-Z0-9 ]", "", h3),collapse=";")
}

# modify below for a single trial (etc)
predsBTM <- function(prepped_vector,BTM_model=nub_every_model,min_max_score = 0.001) {
  whichtopics=numeric(BTM_model$K)
  for (para in prepped_vector) {
#	  print(para)
    hold=gregexpr("!![^ ]+\\b",para)[[1]]
    if (length(hold)<1) next
    keep=list()
    for (i in 1:length(hold)) {
#	    print(i)
      keep=append(keep,substr(para,2+hold[i],attr(hold,"match.length")[i]-1+hold[i]))
    }
    tizz=as.data.frame(cbind("himom",unlist(keep)))
#    		print(tizz)
    zzit=predict(BTM_model,tizz)
    bg = max(zzit[1,])
    if (is.na(bg)) next
    topicno = which(zzit[1,]==max(zzit[1,]))[1]
    if (zzit[1,topicno] < min_max_score) next
#    		print(sprintf("%2d  %6.3f",topicno,zzit[1,topicno]))
    whichtopics[topicno]=whichtopics[topicno]+1
  }
  whichtopics
}

recall_sample_file <- function(filename="output.Rdata") {
  load(filename)                                            # it is a table named output with known columns
  corpus= paste(output$Paragraph,sep="\n",collapse="\n")    # reassemble criteria for NCT01434602
  corpus
 }

fullOn <- function(corpus="",btmodel=NULL, modname="nub_every_model.Rdata") {
 if (nchar(corpus)==0) corpus  = recall_sample_file()
 if (is.null(btmodel)) btmodel = recall_btm_model(modname)
 step1 = letsdoit(corpus,priority,drop_terms,add_terms,TRUE)
 step2 = predsBTM(step1,btmodel,0.001)
 step2
}
