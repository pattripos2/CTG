library(RPostgreSQL)

forSERO <- function(nctid="NCT02563548",con) {
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

write_CTG4Tim <- function() {
  drv <- dbDriver('PostgreSQL')
  con <- dbConnect(drv, dbname="aact",host="aact-db.ctti-clinicaltrials.org", 
                      port=5432, user="pattripos", password="newxCures#2019")

  wanted= "select distinct s.nct_id from
        studies s join facilities f on s.nct_id=f.nct_id join  browse_conditions c on s.nct_id=c.nct_id
                  join conditions d on s.nct_id=d.nct_id
        where s.overall_status in('Active, not recruiting','Not yet recruiting','Recruiting')  and
              f.country='United States' and
            ( lower(s.official_title) like '%glioblastoma%' or
              lower(s.official_title) like '%pancreatic cancer%' or
              lower(s.official_title) like '%ovarian cancer%' or
              lower(s.official_title) like '%glioma%' or
              lower(s.official_title) like '%astrocytoma%' or
              lower(s.official_title) like '%dipg%' or
              lower(s.official_title) like '%solid tumor%'
              or
               c.downcase_mesh_term like  '%glioblastoma%' or
               c.downcase_mesh_term like  '%pancreatic%neoplasms%' or
               c.downcase_mesh_term like  '%ovarian%neoplasms%' or
               c.downcase_mesh_term like  '%glioma%' or
               c.downcase_mesh_term like  '%astrocytoma%' or
               c.downcase_mesh_term like  '%dipg%' 
	      or
               d.downcase_name like  '%glioblastoma%' or
               d.downcase_name like  '%pancreatic%cancer%' or
               d.downcase_name like  '%ovarian%cancer%' or
               d.downcase_name like  '%diffuse midline glioma%' or
               d.downcase_name like  '%diffuse intrinsic pontine glioma%' or
               d.downcase_name like  '%glioma%' or
               d.downcase_name like  '%astrocytoma%' or
               d.downcase_name like  '%dipg%' or
               d.downcase_name like  '%solid tumors%' 
            )
        ;"
  wanted=sub("\n"," ",wanted)
  foo=dbGetQuery(con,wanted)
  dbDisconnect(con)
  
  namepref=sprintf("%s.txt",Sys.Date())
  write.table(foo,row.names=FALSE,col.names=FALSE,file=namepref)

  list(nctids_new=foo, written=namepref)
}

findchanges <- function( newids=nctids_new, lastids = readlastids) {
  drop2b = setdiff(lastids,newids)
   add2b = setdiff(newids,lastids)

  list(add2b=add2b, drop2b=drop2b)
}

ctg4tim  <- function(nctlist,fname) {
  drv <- dbDriver('PostgreSQL')
  con <- dbConnect(drv, dbname="aact",host="aact-db.ctti-clinicaltrials.org", 
                   port=5432, user="pattripos", password="newxCures#2019")
  fo=file(sprintf("results_%s",fname),"w")
  write(paste(c("NCTid","Diseases","Treatments","Phase","OfficialTitle","StudyType","RecruitStatus",
	     "Inclusion","Exclusion","StartDate","PrimaryCompleteDate","StudyCompleteDate","Locations",
	     "Sponsors","Collaborators","Enrollment","PrimaryOut","SecondaryOut","OtherOut",
	     "Link","PrincipalInvestigator"),collapse="\t" ),fo)
 for (nid in nctlist) {
    one= forSERO(nid,con)
       if (length(one) == 1) {write(one$failure,fo) ; next}
    two= outSERO(nid,one,"|")
    write(two,fo)
 }
 close(fo)
 dbDisconnect(con)

}

init_ctgol <- function(filename="results_wanted.txt") {
  drv <- dbDriver('PostgreSQL')
  con <- dbConnect(drv, dbname="trials",host="dev-slsdb.cluster-cm8k3wtxhy27.us-west-2.rds.amazonaws.com", 
                   port=5432, user="trialswrite", password="g%w*d91KooUt^Xe")
# foo=dbGetQuery(con,"DROP TABLE IF EXISTS ctg.options_lib;")
  foo=dbGetQuery(con,"create table ctg.options_lib( NCTid varchar(11) PRIMARY KEY,Diseases text,Treatments text,Phase text,OfficialTitle text,StudyType text,RecruitStatus text, Inclusion text,Exclusion text,StartDate text,PrimaryCompleteDate text,StudyCompleteDate text,Locations text, Sponsors text,Collaborators text,Enrollment text,PrimaryOut text,SecondaryOut text,OtherOut text, Link text,PrincipalInvestigator text);")
# for reasons unclear, the \copy command fails. Need to log into psql and execute "live"
#  foo=dbGetQuery(con,"\\copy ctg.options_lib FROM '/home/dpatterson/auto4OL/results_wanted.txt' WITH HEADER CSV  DELIMITER '	';")
# note that the delimiter is a tab, entered as Ctrl-V<tab>
#
  print("Warning - must START without the table and must INITILIZE it manually with a file input")
  dbDisconnect(con)
}

zap_ctgol <- function(nctidList) {
  drv <- dbDriver('PostgreSQL')
  con <- dbConnect(drv, dbname="trials",host="dev-slsdb.cluster-cm8k3wtxhy27.us-west-2.rds.amazonaws.com", 
                   port=5432, user="trialswrite", password="g%w*d91KooUt^Xe")
  for (i in 1:length(nctidList))
	  foo=dbGetQuery(con,sprintf("DELETE FROM ctg.options_lib WHERE nctid='%s';",nctidList[i]))
  dbDisconnect(con)
}

fixtrouble <- function(ctgtext) {
  for (i in 1:dim(ctgtext)[1]) for (j in 1:dim(ctgtext)[2]) 
	  ctgtext[i,j]= gsub("[\"\']","_",ctgtext[i,j])
  ctgtext
  }

add_ctgcol <- function(thefile) {
  drv <- dbDriver('PostgreSQL')
  con <- dbConnect(drv, dbname="trials",host="dev-slsdb.cluster-cm8k3wtxhy27.us-west-2.rds.amazonaws.com", 
                   port=5432, user="trialswrite", password="g%w*d91KooUt^Xe")
  # actually read aline of text and split after fixing " and '
  thefile=sprintf("results_%s",thefile)
  fin = read.table(thefile,header=TRUE,sep="\t",quote="",as.is=TRUE,comment.char="")
  fin=fixtrouble(fin)
  for (i in 1:dim(fin)[1]) {
    foo=dbGetQuery(con,sprintf("INSERT INTO ctg.options_lib VALUES('%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s');",fin[i,1],fin[i,2],fin[i,3],fin[i,4],fin[i,5],fin[i,6],fin[i,7],fin[i,8],fin[i,9],fin[i,10],fin[i,11],fin[i,12],fin[i,13],fin[i,14],fin[i,15],fin[i,16],fin[i,17],fin[i,18],fin[i,19],fin[i,20],fin[i,21]) )
  }

  dbDisconnect(con)
}

# what is in ctg.options_lib when we start?
grabcurrentnctids <- function() {
  drv <- dbDriver('PostgreSQL')
  con <- dbConnect(drv, dbname="trials",host="dev-slsdb.cluster-cm8k3wtxhy27.us-west-2.rds.amazonaws.com", 
                   port=5432, user="trialswrite", password="g%w*d91KooUt^Xe")
  what=dbGetQuery(con,"select nctid from ctg.options_lib;")
  what
}
	
	
if (FALSE) {
# # #   main  # # # 
#
# A. Get current NCTids of interest from CTG and write them to file
  step1= write_CTG4Tim()
#
# B. get differences
  whatwas= grabcurrentnctids()
  step2= findchanges(step1$nctids_new[,1], whatwas[,1])
#
# C. write new records (if any)
  ctg4tim(step2$add2b,step1$written)
  add_ctgcol(step1$written)
#
# D. write records to be dropped
  write.table(step2$drop2b,row.names=FALSE,col.names=FALSE,file=sprintf("DROP_%s",step1$written))
  zap_ctgol(step2$drop2b)
#

}
