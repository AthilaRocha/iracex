# ---------------------------------------- -*- mode: r; mode: font-lock -*- #
# race.R                       Racing methods for the selection of the best #
# ------------------------------------------------------------------------- #

# ========================================================================= #
# Racing methods for the selection of the best                              #
# ------------------------------------------------------------------------- #
# Copyright (C) 2003 Mauro Birattari                                        #
# ========================================================================= #
# This program is free software; you can redistribute it and/or modify it   #
# under the terms of the GNU General Public License as published by the     #
# Free Software Foundation; either version 2 of the License, or (at your    #
# option) any later version.                                                #
#                                                                           #
# This program is distributed in the hope that it will be useful, but       #
# WITHOUT ANY WARRANTY; without even the implied warranty of                #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU         #
# General Public License for more details.                                  #
#                                                                           #
# You should have received a copy of the GNU General Public License along   #
# with this program; if not, write to the Free Software Foundation, Inc.,   #
# 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.                  #
# ========================================================================= #

# ========================================================================= #
# Mauro BIRATTARI                                                           #
# IRIDIA - ULB, CP 194/6                                                    #
# Av. F. D. Roosevelt 50                                    mbiro@ulb.ac.be #
# 1050 Brussels, Belgium                     http://iridia.ulb.ac.be/~mbiro #
# ========================================================================= #

# $Id: race.R,v 1.54 2005/03/30 12:40:42 mbiro Exp $ #

# Configuration variables
.race.warn.quiet<--1
.race.warn.level<-1
.slave.init.function<-"race.init"
.slave.wrapper.function<-"race.wrapper"
.slave.info.function<-"race.info"
.slave.describe.function<-"race.describe"

.race.warn.save<-getOption("warn")


# The master:
race<-function(maxExp=0,
               stat.test=c("friedman","t.bonferroni","t.holm","t.none"),
               conf.level=0.95,
               first.test=5,
               each.test=1,
               interactive=TRUE,
               stop.min.cand=1,
               media = media,
               effects = effects,
               effectsagg = effectsagg,
               totaltests = totaltests,
               totrejag = totrejag,
               totrejnag=totrejnag,
               it,
               ...){
  print("Race function initializing...")
 
  
  timestamp.start<-date()

  # Change warning behavior
  .race.warn.save<-getOption("warn")
  on.exit(options(warn=.race.warn.save))
  options(warn=.race.warn.level)

  # Check argument: wrapper.file
  if (!exists(.slave.wrapper.function,inherits=TRUE,mode="function")||
      !exists(.slave.info.function,inherits=TRUE,mode="function"))
    stop('Required functions are missing: "',
         .slave.wrapper.function,'"\n',
         'and/or the function "',
         .slave.info.function,'"')

  # Check argument: maxExp
  if (!missing(maxExp) &&
      (!is.numeric(maxExp) ||
       length(maxExp)!=1 ||
       !is.finite(maxExp)))
    stop("maxExp must be an single number")
  maxExp<-ifelse(maxExp>0,maxExp,0)
  maxExp<-floor(maxExp)

  # Check argument: stat.test
  stat.test<-match.arg(stat.test)

  # Check argument: conf.level
  if (!missing(conf.level) &&
      (!is.numeric(conf.level) || length(conf.level)!=1 ||
       !is.finite(conf.level) || conf.level<0 || conf.level>1))
    stop("conf.level must be a single number between 0 and 1")

  # Check argument: first.test
  if (!missing(first.test) &&
      (!is.numeric(first.test) ||
       length(first.test)!=1 ||
       !is.finite(first.test)))
    stop("first.test must be an single number")
  first.test<-ifelse(first.test>0,first.test,0)
  first.test<-floor(first.test)

  # Check argument: interactive
  if (!missing(interactive) &&
      (!is.logical(interactive) || length(interactive)!=1))
    stop("interactive must be a logical")

  # Run init function 
  if (exists(.slave.init.function,inherits=TRUE,mode="function")){
    race.data<-do.call(.slave.init.function,list(...))
    if(!is.list(race.data))
      stop(paste("Error while running",.slave.init.function))
    precis.init<-TRUE
  } else {
    race.data<-list()
    precis.init<-FALSE
  }

  
 
  ##At this point the informations about instances are in memory to be used
  
  ##debug code  
  ##print("RACE.DATA \n")
  ##print(str(race.data))
  ##print(" tamanho de RACE.DATA$INSTANCES")
  ##print(length(race.data$config$instances))
  ##print("RACE.DATA$RACE.INSTANCES")
  ##print(race.data$race.instances)
  
  
  # Collect info on race from wrapper
  race.info<-do.call(.slave.info.function,list(race.data))
  # Check race.info
  if (# race.info$race.name must be a string
      is.na(match("race.name",names(race.info)))||
      !is.character(race.info$race.name)||
      length(race.info$race.name)!=1 ||
      # race.info$no.candidates must be an integer
      is.na(match("no.candidates",names(race.info)))||
      !is.numeric(race.info$no.candidates) ||
      length(race.info$no.candidates)!=1 ||
      !is.finite(race.info$no.candidates) ||
      race.info$no.candidates!=as.integer(race.info$no.candidates) ||
      # race.info$no.tasks must be an integer
      is.na(match("no.tasks",names(race.info)))||
      !is.numeric(race.info$no.tasks) ||
      length(race.info$no.tasks)!=1 ||
      !is.finite(race.info$no.tasks) ||
      race.info$no.tasks!=as.integer(race.info$no.tasks)||
      # race.info$no.subtasks is a non-compulsory integer.
      (!is.na(match("no.subtasks",names(race.info)))&&
      (!is.numeric(race.info$no.subtasks) ||
       (length(race.info$no.subtasks)!=1 &&
        length(race.info$no.subtasks)!=race.info$no.tasks) ||
       any(!is.finite(race.info$no.subtasks)) ||
       any(race.info$no.subtasks!=as.integer(race.info$no.subtasks))))||
      # race.info$extra is a non-compulsory string or paragraph.
      (!is.na(match("extra",names(race.info)))&&
       !is.character(race.info$extra)))
    stop(paste("Function \"",.slave.info.function,
               "\" returned an invalid object",sep=""))

  # Default for no.subtasks
  if (is.na(match("no.subtasks",names(race.info))))
    race.info$no.subtasks<-1

  
  
  # copy race.info contents to workspace for convenience
  race.name<-race.info$race.name
  no.candidates<-race.info$no.candidates
  no.tasks<-race.info$no.tasks
  no.subtasks<-race.info$no.subtasks
  
  
  # Prepare a precis for documentation
  format.precis <- function(title, value) {
    title.width <- nchar(title) + nchar(": ")
    string <- sprintf("%s: %*s", title, 79 - title.width, value)
    return(paste(string, "\n"))
  }

  ids <- as.numeric(race.data$ids)
  idsagg <- as.numeric(race.data$idsagg)
  
  
#   print("non aggregated test candidates")
#   print(ids)
#   print("aggregated teste candidates ")
#   print(idsagg)
  
  ##determining the effects of candidates X instances
  
  instances <- as.numeric(ncol(effects))

  precis<-paste("\n",
                "Racing methods for the selection of the best\n",
                "Copyright (C) 2003 Mauro Birattari\n",
                "This software comes with ABSOLUTELY NO WARRANTY\n\n",
                format.precis("Race name",race.name),
                format.precis("Number of candidates",no.candidates),
                format.precis("Number of available tasks",no.tasks),
                ifelse(length(no.subtasks)>1,
                       format.precis("Subtasks per task","task-dependent"),
                       ifelse(no.subtasks>1,
                              format.precis("Subtasks per task",no.subtasks),
                              "")),
                format.precis("Max number of experiments",
                              ifelse(maxExp,maxExp,"unlimited")),
                format.precis("Statistical test",
                              switch(stat.test,
                                     friedman="Friedman test",
                                     t.bonferroni=paste("t-test with",
                                       "Bonferroni's correction",
                                       "for multiple comparisons"),
                                     t.holm=paste("t-test with",
                                       "Holm's correction",
                                       "for multiple comparisons"),
                                     t.none=paste("t-test with",
                                       "no correction",
                                       "for multiple comparisons"))),
                format.precis("Tasks seen before discarding",first.test),
                format.precis("Initialization function",
                              ifelse(precis.init,"ok","none found")),
                sep="")

  if (!is.null(race.info$extra)){
    extra<-paste(strwrap(race.info$extra,width=60,prefix="\t"),collapse="\n")
    precis<-paste(precis,"\n",extra,"\n")
  }


  # Do not print precis. All information are redundant with
  # the ones provided from irace.
  interactive <- FALSE
  # Print out precis if interactive
  if (interactive)
    cat(paste(precis,"\n\n"))
  interactive <- TRUE

  if (maxExp && no.candidates > maxExp)
    stop("Max number of experiments is smaller than number of candidates")

  if (no.candidates <= stop.min.cand) {
    stop("Not enough candidates (", no.candidates, ") for a race (stop.min.cand=", stop.min.cand, ")")
  }

  check.result<-function(result){
    expected.length<-ifelse(length(no.subtasks)==1,
                            no.subtasks,
                            no.subtasks[current.task])
    if (length(result)!=expected.length)
      stop(paste("Bad output returned by \"",
                 .slave.wrapper.function,"\"",sep=""))
  }

  # Initialize some variables...
  testtype <- 0
  Tasks<-1:no.tasks
  Results<-matrix(data=NA,
                  nrow=ifelse(length(no.subtasks)==1,
                    no.tasks*no.subtasks,sum(no.subtasks)),
                  ncol=no.candidates+1)
  
  ##structures to load data to the tests
  Resultsagg<-matrix(data=NA,
                  nrow=ifelse(length(no.subtasks)==1,
                  no.tasks*no.subtasks,sum(no.subtasks)),
                  ncol=no.candidates+1)
  
  
  Time <- matrix(data = NA,
                 nrow = ifelse(length(no.subtasks) == 1,
                   no.tasks * no.subtasks, sum(no.subtasks)),
                 ncol = no.candidates)
  
  aliveagg<-array(TRUE,no.candidates)
  alive<-array(TRUE,no.candidates)
  
  race.totalrejecagg <-0
  race.totalrejecnonagg <-0
  no.experiments.sofar<-0
  no.subexperiments.sofar<-0
  best<-0
  bestagg <-0
  race.ranks <- c()
  race.ranksagg <- c()
  no.tasks.sofar<-0
  no.subtasks.sofar<-0

  # Define some functions...
  # FIXME: Keep only what we need!
  log.list<-function(end=FALSE){
    timestamp.current<-date()
    log<-list(precis=precis,
              results=Results[1:no.subtasks.sofar,],
              resultsagg = Resultsagg[1:nrow(Resultsagg),1:(ncol(Resultsagg)-1)],
              time = Time[1:no.subtasks.sofar, ],
              no.candidates=no.candidates,
              no.tasks=no.tasks.sofar,
              no.subtasks=no.subtasks,
              no.experiments=no.experiments.sofar,
              no.alive=sum(alive),
              no.aliveagg = sum(aliveagg),
              alive=alive,
              aliveagg = aliveagg,
              best=best,
              bestagg = bestagg,
              mean.best=mean.best,
              timestamp.start=timestamp.start,
	            race.data=race.data,
              ranks = race.ranks,
              ranksagg = race.ranksagg,
              effects = effects,
              effectsagg = effectsagg,
              totaltests = totaltests,
              totrejag =  totrejag,
              totrejnag=totrejnag)
    if (end) {
      log<-c(log,list(timestamp.end=timestamp.current,
                      description.best=description.best,
                      alive.inTime=ifelse(no.subtasks.sofar>1,
		      	apply(Results[1:no.subtasks.sofar,],
                        	1,function(u){sum(!(is.na(u)))}),
			sum(!is.na(Results[1,])))),
			apply(Resultsagg[1:nrow(Resultsagg),],
			      1,function(u){sum(!(is.na(u)))}),
			sum(!is.na(Resultsagg[1,])))
    } else {
      log<-c(log,list(timestamp.current=timestamp.current))
    }
    return(log)
  }

  aux2.friedman<-function(y,I=1:ncol(y)-1,n=nrow(y),conf.level=0.95,testtype){
    k<-length(I)
  
    r<-t(apply(y[1:n,I], 1, rank))
    #r = vector with the rankings of each candidate on each instance (beginning from the number 1)
    A<-sum(as.vector(r)^2)
    R<-apply(r, 2, sum)
    J<-I[order(R)]
    alpha<-1-conf.level
    TIES<-tapply(r, row(r), table)
    cat("Candidate Numbers: \n")
    cat(k)
    cat("\n")
    cat("Instance numbers: \n")
    cat(n)
    cat("\n")
    
    ##Debug code to derive the formulation of tests
    #cat("Valor de TIES \n")
    #print(TIES)
    #print("valor de lapply(TIES, function (u) {u^3-u})\n")
    #print(lapply(TIES, function (u) {u^3 - u}))
    #print("valor de unlist(lapply(TIES, function (u) {u^3 - u}))\n")
    #print(unlist(lapply(TIES, function (u) {u^3 - u})))
    #print("somatorio (sum(unlist(lapply(TIES, function (u) {u^3 - u})))\n")
    #print(sum(unlist(lapply(TIES, function (u) {u^3 - u}))))
    
    
    STATISTIC<-((12 * sum((R - n * (k + 1) / 2)^2)) /
                (n * k * (k + 1)
                 - (sum(unlist(lapply(TIES, function (u) {u^3 - u}))) /
                    (k - 1))))
    PARAMETER<-k-1
    cat("Statistic value: \n")
    cat(STATISTIC)
    cat("\n")
    PVAL<-pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
    cat("P-value: \n",file="iraceaggresults.csv",append=T)
    cat(PVAL,file="iraceaggresults.csv",append=T)
    cat("\n",file="iraceaggresults.csv",append=T)
    
    ##Debug code
    ##cat("vetor r: \n")
    ##cat(r)
    ##cat("\n")
    ##cat("valor de A: \n")
    ##cat(A)
    ##cat("\n")
    ##cat("vetor R: \n")
    ##cat(R)
    ##cat("\n")
    ##cat("Vetor J: \n")
    ##cat(J)
    ##cat("\n")
    #vetor J contem a identificação dos candidatos, ordenados de acordo com os rankings (ordem crescente)
    
    
    race.totalrejecagg <- (race.totalrejecagg+1)
    if (!is.nan(PVAL) && (PVAL<alpha)){
      if (interactive)
        cat("|-|")
      if (testtype==1) {
            cat("Null hypothesis rejected, non-aggregated test...")
            race.totalrejecnonagg <- (race.totalrejecnonagg +  1)
        }
      else { 
          cat("Null hypothesis rejected, aggregated test...") 
          race.totalrejecagg <- (race.totalrejecagg +1)
        }
      cat("\n")
      t<-qt(1-alpha/2,(n-1)*(k-1))*(2*(n*A-sum(R^2))/((n-1)*(k-1)))^(1/2)
      cat("Test t value: ")
      cat(t)
      cat("\n")
      o<-order(R)
      #cat("vetor o:\n")
      #cat(o)
      J<-I[o[1]]
      for (j in 2:k) {
        if (abs(R[o[j]]-R[o[1]])>t) {  #testa diferença do somatorio dos rankings do best contra todos
          break
        } else {
          J<-c(J,I[o[j]])
        }
      }
    } else {
       if (testtype==1) cat("Null hypothesis no rejected, non-aggregated test...")
      else cat ("Null hypothesis no rejected, aggregated test...")
       cat("\n")
       if (interactive)
        cat("|=|") 
    }
    if (testtype==1 ) { race.ranks <<- R
                       return(J)
    }
    else {
         race.ranksagg <<- R
         return(J)
    }
  }

  aux.friedman<-function(R,testtype){
    if (no.alive==2) {
      # If only 2 candidates are left, switch to Wilcoxon
      V1<-R[1:(no.subtasks.sofar),which.alive[1]]
      V2<-R[1:(no.subtasks.sofar),which.alive[2]]
      PVAL<-wilcox.test(V1,V2,paired=TRUE,exact=FALSE)$p.value
      if (!is.nan(PVAL)&&!is.na(PVAL)&&(PVAL<1-conf.level)){
        if (interactive)
          cat("|-|")
        if (median(V1-V2)<0){
          best<<-which.alive[1]
          alive[which.alive[2]]<<-FALSE
          race.ranks <<- c(1,2)
        }else{
          best<<-which.alive[2]
          alive[which.alive[1]]<<-FALSE
          race.ranks <<- c(2,1)
        }
      }else{
        if (interactive)
          cat("|=|")
        if (median(V1-V2)<0){
          best<<-which.alive[1]
          race.ranks <<- c(1,2)
        }else{
          best<<-which.alive[2]
          race.ranks <<- c(2,1)
        }
      }
    }
    else{
      # If more then 2 candidates are left, use Friedman
      race.totalrejecnonagg<-0
      J<-aux2.friedman(R[1:min((no.subtasks.sofar),nrow(R)),],which.alive,
                       conf.level=conf.level,testtype = 1)
      
      totrejnag <- totrejnag + race.totalrejecnonagg
      
      alive[-J]<<-FALSE
      best<<-J[1]
      cat("best ")
      cat(best)
      cat("\n")
    }
  }

  aux.friedmanagg<-function(R,testtype){
    if (no.aliveagg==2) {
      # If only 2 candidates are left, switch to Wilcoxon
      V1<-R[1:nrow(R),which.aliveagg[1]]
      V2<-R[1:nrow(R),which.aliveagg[2]]
      PVAL<-wilcox.test(V1,V2,paired=TRUE,exact=FALSE)$p.value
      if (!is.nan(PVAL)&&!is.na(PVAL)&&(PVAL<1-conf.level)){
        if (interactive)
          cat("|-|")
        if (median(V1-V2)<0){
          bestagg<<-which.aliveagg[1]
          aliveagg[which.aliveagg[2]]<<-FALSE
          race.ranksagg <<- c(1,2)
        }else{
          bestagg<<-which.aliveagg[2]
          aliveagg[which.aliveagg[1]]<<-FALSE
          race.ranksagg <<- c(2,1)
        }
      }else{
        if (interactive)
          cat("|=|")
        if (median(V1-V2)<0){
          bestagg<<-which.aliveagg[1]
          race.ranksagg <<- c(1,2)
        }else{
          bestagg<<-which.aliveagg[2]
          race.ranksagg <<- c(2,1)
        }
      }
    }else{
      # If more then 2 candidates are left, use Friedman
      race.totalrejecagg<-0
      J<-aux2.friedman(R[1:min((no.subtasks.sofar),nrow(R)),],which.aliveagg,
                       conf.level=conf.level,testtype = 2)
  
      totrejag <- totrejag + race.totalrejecagg
      
      aliveagg[-J]<<-FALSE
      bestagg<<-J[1]
      cat("bestagg ")
      cat(bestagg)
      cat("\n")
    }
  }

  
  aux.ttest<-function(adjust=c("none","bonferroni","holm")){
    adjust<-match.arg(adjust)
    # FIXME why c()?
    mean.all<-array(0,c(ncol(Results)))
    for (j in 1:ncol(Results))
      # FIXME: why not just mean() ?
      mean.all[j]<-sum(Results[1:no.subtasks.sofar,j]/no.subtasks.sofar)
    # FIXME: which.min?
    best<<-match(min(mean.all[alive]),mean.all)
    race.ranks <<- mean.all[alive]

    PJ<-array(0,dim=c(2,0))
    for (j in which.alive) {
      # FIXME: This doesn't seem to change so it could be outside the for()
      Vb<-Results[1:no.subtasks.sofar,best]
      Vj<-Results[1:no.subtasks.sofar,j]
      #cat("Vb:", Vb, "\n")
      #cat("Vj:", Vj, "\n")
      # t.test may fail if the data in each group is almost
      # constant. Hence, we surround the call in a try() and we
      # initialize p with 1 if the means are equal or zero if they are
      # different.
      # FIXME: mean(Vb) doesn't seem to change either.
      p <- as.integer(isTRUE(all.equal(mean(Vb),mean(Vj))))
      try(p <- t.test(Vb,Vj,paired=TRUE)$p.value)
      if (!is.nan(p) & !is.na(p))
        PJ<-array(c(PJ,j,p),dim=dim(PJ)+c(0,1))
    }
    PJ[2,]<-p.adjust(PJ[2,],method=adjust)
    dropped.any<-FALSE
    for (j in 1:ncol(PJ))
      if (PJ[2,j]<(1-conf.level)){
        alive[PJ[1,j]]<<-FALSE
        dropped.any<-TRUE
      }
    if (interactive){
      if (dropped.any) {
    #    cat("|-|")
      } else {
    #    cat("|=|")
      }
    }
  }

  if (interactive)
#     cat("                            Markers:                           \n",
#         "                               x No test is performed.         \n",
#         "                               - The test is performed and     \n",
#         "                                 some candidates are discarded.\n",
#         "                               = The test is performed but     \n",
#         "                                 no candidate is discarded.    \n",
#         "                                                               \n",
#         "                                                               \n",
#         "+-+-----------+-----------+-----------+-----------+-----------+\n",
#         "| |       Task|      Alive|       Best|  Mean best| Exp so far|\n",
#         "+-+-----------+-----------+-----------+-----------+-----------+\n",
#         sep="")

  # Start main loop
  for (current.task in 1:no.tasks) {
    which.alive<-which(alive)
    which.aliveagg <- which(aliveagg)
    no.alive<-length(which.alive)
    no.aliveagg<-length(which.aliveagg)
    # FIXME: This should take into account each.test. It doesn't make
    # much sense to see an additional instance if there is no budget
    # to do a test. The point of each.test is to see instances in
    # groups and this defeats its purpose.
    if (maxExp && no.experiments.sofar+no.alive>maxExp)
      break
    if ((no.alive==1) && (no.aliveagg==1))
      break

    current.no.subtasks<-ifelse(length(no.subtasks)==1,
                                no.subtasks,
                                no.subtasks[current.task])

    if (length(no.subtasks)==1) {
      subtasks.range<-
        (current.task-1)*no.subtasks+1:no.subtasks
    } else {
      subtasks.range<-
        cumsum(c(0,no.subtasks))[current.task]+1:no.subtasks[current.task]
    }

    
    if (it==1){
      
      for (i in 1:instances) 
        for (j in 1:length(ids)){
          if (is.na(effects[(ids[j]),i])) {
            auxeffect<- rnorm(1,0,0.4)
            effects[(ids[j]),i] <- auxeffect
            effectsagg[(idsagg[j]),i] <- auxeffect
          }
        }
      
      for (current.candidate in which.alive){
        
        result <- do.call (.slave.wrapper.function,     
                           list(current.candidate, current.task,
                                alive=which.alive, race.data,testtype=1))
        
        check.result(result[1,1])
        
        ##mean of the candidate on an instance is equal to a fixed grand mean + mean of candidate-instance+experimental error (calculate later)
        #mediacandinst <- media + effects[ids[current.candidate]][result[1,2]]
        
        mediacandinst <- media + effects[(ids[current.candidate]),(result[1,2])] 
        

        #this code perform a real execution of an algorithm on some instance
        #Results[subtasks.range, current.candidate] <- as.numeric(result[1,1])
        
        cost <-0
       
        
        error <- mean(rnorm(30,0,0.01))
        cost <- mediacandinst + error
        
        Results[subtasks.range, current.candidate] <- cost
        Resultsagg[subtasks.range,current.candidate] <- cost
         
        #my change (14/10/15) - the last column receives the id of the instance
        Results[subtasks.range, ncol(Results)] <- as.numeric(result[1,2])
        Resultsagg[subtasks.range,ncol(Results)] <- as.numeric(result[1,2])
        
        #my change (02/10/15)
        Time[subtasks.range, current.candidate] <- NA
        ##In he prior line, was <-result[2] and I changed to NA
      }  
    }
    else {
    for (current.candidate in which.alive){
      
      result <- do.call (.slave.wrapper.function,     
                         list(current.candidate, current.task,
                             alive=which.alive, race.data,testtype=1))
      
      check.result(result[1,1])
      

      if (is.na((effects[(ids[current.candidate]),(result[1,2])]))) {
         effects[(ids[current.candidate]),(result[1,2])] <- rnorm(1,0,0.4)
      }
      mediacandinst <- media + effects[(ids[current.candidate]),(result[1,2])] 
      

      #this is the real execution of some algorithm
      #Results[subtasks.range, current.candidate] <- as.numeric(result[1,1])
      
      cost <-0
      
      error <- mean(rnorm(30,0,0.01))
      cost <- mediacandinst + error
      
      Results[subtasks.range, current.candidate] <- cost 
      
      #my change (14/10/15) - the last colum receives the id of the instance
      Results[subtasks.range, ncol(Results)] <- as.numeric(result[1,2])
      
      #my change (02/10/15)
      Time[subtasks.range, current.candidate] <- NA
      ##In the prior line, there was <-result[2] and I changed to NA
     }
    
      ##getting costs for these algorithms of the aggregated test
      for (i in 1:length(idsagg)){
        result <- do.call (.slave.wrapper.function,       
                           list(idsagg[i], current.task,
                                alive=which.aliveagg, race.data,testtype=2))  
        check.result(result[1,1]) 
        
        ##real execution of an algorithm
        ##Resultsagg[subtasks.range, i] <- as.numeric(result[1,1])  
        
        
        if (is.na(effectsagg[(idsagg[i]),(result[1,2])])) {
          effectsagg[(idsagg[i]),(result[1,2])] <- rnorm(1,0,0.4)
        }
        
        mediacandinst <- media + effectsagg[(idsagg[i]),(result[1,2])]
        cost <-0
        
        error <- mean(rnorm(30,0,0.01))
        cost <- mediacandinst + error
        
        Resultsagg[subtasks.range, i] <- cost
        Resultsagg[subtasks.range, ncol(Results)] <- as.numeric(result[1,2])   
        Time[subtasks.range, i] <- NA
        ##Na linha anterior, estava <-result[2] e eu mudei para NA
      }
    }
    no.experiments.sofar<-no.experiments.sofar+no.alive
    no.subexperiments.sofar<-no.subexperiments.sofar+
      current.no.subtasks*no.alive

    no.tasks.sofar<-no.tasks.sofar+1
    no.subtasks.sofar<-no.subtasks.sofar+current.no.subtasks

    Resultsagg <- aggregate(Resultsagg, list(Resultsagg[,ncol(Resultsagg)]),mean, na.rm=TRUE)

        
    Resultsagg <- Resultsagg[,-(1:1)]
    #Eliminating the extra-column inserted by the aggregate command
    
    ## Drop bad candidates.
    # We assume that first.test is a multiple of each.test.  In any
    # case, this will only do the first test after the first multiple
    # of each.test that is larger than first.test.
    if ( (no.tasks.sofar>=first.test) && ((no.tasks.sofar %% each.test) == 0)) {
      cat(" \n")
      
      #debug code
      #print(Resultsagg[1:no.subtasks.sofar,])
      #cat(" \n")
      #print("Estrutura não agregada \n")
      #print(Results[1:no.subtasks.sofar,])
      #cat(" \n ")
      
      totaltests <- totaltests+1
      cat("Non-Aggregated Results \n", file="iraceaggresults.csv",append=T)
      
      print(Results[1:current.task,1:(current.candidate+1)])
      r<-Results[1:current.task,1:(current.candidate+1)]
      
      c<-numeric(current.candidate+1)
      for(i in 1:current.candidate) c[i] <- ids[i]
      colnames(r) <- c
      
      write.table(r, file="iraceaggresults.csv", append=T, row.names=F, col.names=T,  sep=",")
      cat("\n", file="iraceaggresults.csv",append=T)
      
      cat("Friedman Test with non-aggregated results \n")
      switch(stat.test,
             friedman=aux.friedman(Results),
             t.none=aux.ttest("none"),
             # FIXME: These two need to be exposed as options to irace's testType parameter.
             t.holm=aux.ttest("holm"),
             t.bonferroni=aux.ttest("bonferroni"))
      
       cat("Aggregated Results \n", file="iraceaggresults.csv",append=T)
       
       print(Resultsagg[1:nrow(Resultsagg),1:ncol(Resultsagg)])
       r <- Resultsagg[1:nrow(Resultsagg),1:ncol(Resultsagg)]
       
       c<-numeric(ncol(Resultsagg))
       for(i in 1:(ncol(Resultsagg)-1)) c[i] <- idsagg[i]
       colnames(r) <- c
       write.table(r, file="iraceaggresults.csv", append=T, row.names=F, col.names=T,  sep=",")
       cat("\n", file="iraceaggresults.csv",append=T)
       
      cat("\n")
      cat("Friedman Test with aggregated results \n")
      switch(stat.test,
             friedman=aux.friedmanagg(Resultsagg),
             t.none=aux.ttest("none"),
             # FIXME: These two need to be exposed as options to irace's testType parameter.
             t.holm=aux.ttest("holm"),
             t.bonferroni=aux.ttest("bonferroni"))
    } else {
      if (interactive)
        #cat("|x|")
      if (no.subtasks.sofar==1)  {
        # FIXME: Shouldn't these be ranks when stat.test == "friedman" ?
        #race.ranks <- Results[1,]
        
        #my change - 02/10/15
        
        race.ranks <- Results[1,1:no.candidates]
        race.ranksagg <- Resultsagg[1,1:no.candidates]
        best <- order(race.ranks)[1]
        bestagg <- order(race.ranksagg)[1]
        
      } else  {
        
        tmpResultsagg<-Resultsagg[1:nrow(Resultsagg),which.aliveagg]
        tmpResults <- Results[1:no.subtasks.sofar, which.alive]
        irace.assert(!any(is.na(tmpResults)))
        
        irace.assert(!any(is.na(tmpResultsagg)))
        if (stat.test == "friedman") {
          race.ranks <- colSums(t(apply(tmpResults, 1, rank)))
          race.ranksagg <- colSums(t(apply(tmpResultsagg,1,rank)))
        } else {
          race.ranks <- colMeans(tmpResults)
          race.ranksagg <- colMeans(tmpResultsagg)
        }
        best <- which.alive[order(race.ranks)[1]]
        bestagg <-which.aliveagg[order(race.ranksagg)[1]]
      }
    }
    
    irace.assert(best == which.alive[order(race.ranks)][1])
    irace.assert(length(race.ranks) == length(which.alive))
    
    irace.assert(bestagg==which.aliveagg[order(race.ranksagg)][1])
    irace.assert(length(race.ranksagg)== length(which.aliveagg))
    
    # Remove the ranks of those that are not alive anymore
    race.ranks <- race.ranks[which.alive %in% which(alive)]
    race.ranksagg <- race.ranksagg[which.aliveagg %in% which(aliveagg)]
    irace.assert(length(race.ranks) == sum(alive))
    # FIXME: This is the mean of the best, but perhaps it should be
    # the sum of ranks in the case of test == friedman?
    ##print("Results[1:no.subtasks.sofar, best]")
    ##print(Results[1:no.subtasks.sofar, best])
    ##print("mean(Results[1:no.subtasks.sofar, best])")
    ##print(mean(Results[1:no.subtasks.sofar, best]))
    mean.best <- mean(Results[1:no.subtasks.sofar, best])
    mean.bestagg <- mean(Resultsagg[1:no.subtasks.sofar, bestagg])
    
    if (interactive)
      cat(paste(formatC(no.tasks.sofar,width=11),"|",
                formatC(sum(alive),width=11),"|",
                formatC(best,width=11),"|",
                formatC(mean.best,width=11),"|",
                formatC(no.experiments.sofar,width=11),"|\n",
                sep=""))

    # stop race if we have less or equal than the minimum number of candidates
    if (no.tasks.sofar>=first.test) {
        which.alive<-which(alive)
    	if(length(which.alive) <= stop.min.cand)
    	break
   }
  }
  
  if (exists(.slave.describe.function,inherits=TRUE,mode="function")) {
    description.best<-do.call(.slave.describe.function,list(best,race.data))
    description.bestagg<-do.call(.slave.describe.function,list(bestagg,race.data))
  } else {
    description.best<-NULL
    description.bestagg <-NULL
  }

  if (interactive) {
   cat(paste("+-+-----------+-----------+-----------+-----------+-----------+",
              "\n\n",
              "Selected candidate:",formatC(best,width=12),
              "\tmean value:",formatC(mean.best,width=11),
              "\n\n",sep=""))
    if (!is.null(description.best)){
      cat("Description of the selected candidate:\n")
      print(description.best)
      cat("\n\n")
    }

    cat(paste("+-+-----------+-----------+-----------+-----------+-----------+",
              "\n\n",
              "Selected candidate aggregated test:",formatC(bestagg,width=12),
              "\tmean value:",formatC(mean.bestagg,width=11),
              "\n\n",sep=""))
    if (!is.null(description.bestagg)){
      cat("Description of the selected candidate:\n")
      print(description.bestagg)
      cat("\n\n")
    }
  }
  
  
  # Build the return variable with everything inside:
  invisible(log.list(end=TRUE))
}
