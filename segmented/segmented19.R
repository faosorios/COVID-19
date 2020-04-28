######################################################################################
#--> These functions have been written to model covid-19 daily time series data
#--> The main goal is to assess deceleration of epidemic spreading likely due to effectiveness
#     of lockdown policy
#--> Source this file in your R workspace. The R package segmented has to be loaded. Please refer
#       to the technical report on ResearchGate for more details (see below)
#--> Author: Vito Muggeo (vito.muggeo@unipa.it)
#--> If you use these functions, please cite the following manuscript (on ResearchGate)
#--> Muggeo VMR, Sottile G, Porcu M. (2020) Modelling COVID-19 outbreak: segmented regression to assess
#     lockdown effectiveness
#######################################################################################

take.y<-function(d,country=stop("provide country"),y=c("cases","deaths"), cum=TRUE){
  #d: the ECDC dataframe
  #y: which outcome?
  #cum: take the cumulative sums
  y<-match.arg(y)
  if(!(country %in% levels(d[,"countriesAndTerritories"]))) stop("unknown 'country', see 'levels(d[,7])' ")
  d<-d[order(d[,7],d[,4],d[,3],d[,2]),]
  dd<-subset(d, countriesAndTerritories==country)
  y<-dd[,y]
  if(cum) y<-cumsum(y)
  attr(y,"first.date")<- as.character(dd$dateRep[1])
  y
}

segmented19 <- function(y, npsi=1:5, rule=">=0", oseg, fix.ppsi=FALSE, origin, rm.id, format="%Y-%m-%d"){
  #============================================================================================
  #== segmented regression for COVID-19 modelling (author: Vito Muggeo, vito.muggeo@unipa.it)
  #============================================================================================
  #y: the *cumulative* outcome counts (total cases typically)
  #npsi: scalar or vector meaning the number of breakpoints to assess.
  #      If scalar just a segmented model is fitted, otherwise the BIC is used..
  #rule: a character to consider counts larger than the specified value. Eg. ">=100"
  #oseg: an optional segmented fit. If provided its breakpoints are supplied as fixed or starting
  #      values in the new segmented fit
  #fix.p.psi: if 'oseg' is provided, fix.p.psi=TRUE means that the estimated breakpoints in 'oseg'
  #      are kept fixed in the new segmented fit, otherwise they are assumed as starting values
  #origin: optional, the date. If supplied the plots and results will expressed also in terms of
  #       dates (rather then simple numerical values 1,2,...)
  #rm.id: optional. If provided those values are dropped from the supplied vector 'y'. Eg.
  #       'rm.id=1:4' will remove the first four y values.
  require(segmented)
  date1<-attr(y, "first.date")
  if(!is.null(date1)){
    origin<-date1
    format<-"%d/%m/%Y" #y minuscolo è solo le ultime 2 cifre dell'anno..
  }
  if(!missing(rm.id)) y<-y[-rm.id]
  if(any(diff(y)<0)) stop("the response vector is not increasing")
  if(!missing(oseg)){
    x.old<-o$model[,2]
    y.old<-o$y
    psi<-o$psi[,2]
    if(length(y)<=3) y<-c(y.old, y)
    x<-1:length(y)

    if(fix.ppsi){
      X.prev<-as.matrix(oseg$model[,c(oseg$nameUV$Z, oseg$nameUV$U)])
      fo.U<-as.formula(paste("y",paste(c(oseg$nameUV$Z, oseg$nameUV$U), collapse="+"), sep="~"))
      fo.UV<-as.formula(paste("y",paste(c(oseg$nameUV$Z, oseg$nameUV$U,oseg$nameUV$V), collapse="+"), sep="~"))
      o<-glm(y~X.prev, family=poisson)
      os<-try(segmented(o, psi= length(y.old), control=seg.control(n.boot=50)), silent=TRUE)
    } else {
      o<-glm(y~x, family=poisson)
      os<-try(segmented(o, psi=c(psi.old, length(y.old)), control=seg.control(n.boot=50)), silent=TRUE)
    }
    if(!inherits(class(os),"segmented")) {
      cat("No additional breakpoint estimated.. returning 'oseg'\n")
      return(oseg)
    } else {
      class(os)<- c("segmented19", "segmented", "glm", "lm")
      return(os)
    }
    #there is no previous segmented fit.. Estimate it!
  } else {
    n<-length(y)
    x<-1:n

    id<-eval(parse(text=paste("y",rule)))
    y<-y[id]
    x<-1:length(y) #x[id]

    if(!missing(origin)){
      if((n-length(x))>0){ #change the origin..
        origin<-as.Date((n-length(x)), format=format, origin = origin)
      }
    }
    #if(!is.null(origin)) date.all<-as.Date(1:n, origin = origin)
    #if(!is.null(origin)) date.all<-date.all[id]

    o<-glm(y~x, family=poisson)
    if(length(npsi)==1){
      o<-suppressWarnings(try(segmented(o, npsi=npsi, control=seg.control(n.boot=50)), silent=TRUE))
      class(o)<- c("segmented19", "segmented", "glm", "lm")
      o$origin<-origin
      return(o)
    }
    ris.ok<-NULL
    ris<-vector("list", length(npsi))
    id.ris<-rep(FALSE, length(npsi))
    cat("Running  ... ")
    for(i in npsi){
      ris[[i]]<-suppressWarnings(try(segmented(o, npsi=i, control=seg.control(n.boot=50)), silent=TRUE))
      if(inherits(ris[[i]], "segmented")) {
        id.ris[i]<-TRUE
        ris.ok[[length(ris.ok)+1]]<-ris[[i]]
      }
    }
    n.ok<-sum(id.ris)
    r<-sapply(ris.ok, BIC)
    id.ok<-which.min(r)
#browser()
    npsi<-npsi[id.ris] #omit the errors..
    n.psi.ok<-npsi[id.ok]
    o<-ris.ok[[id.ok]]
    cat(paste(n.psi.ok, "breakpoints selected by BIC\n"))
    names(r)<-npsi
    o$bic<-r
    o$origin<-origin
    class(o)<- c("segmented19", "segmented", "glm", "lm")
    o
  }
}

`print.segmented19` <- function(x,digits = max(3, getOption("digits") - 3), rm.year.lab=TRUE,...){
  cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n")
  cat("@@@ Epidemic modelling via segmented regression @@@\n")
  cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n")
  psi<-round(x$psi[,"Est."],2)
  psi.date<-NULL
  if(!is.null(x$origin)) psi.date<-as.Date(round(psi,0), origin = x$origin)
  if(rm.year.lab) psi.date<-sapply(lapply(strsplit(as.character(psi.date),"-"),
                                          function(x)x[-1]), function(x)paste(x, collapse="-"))

  rates<- round(slope(x, APC=TRUE)[[1]][,"Est."],1)
  pend<-slope(x)[[1]][,"Est."]
  dday<- round( log(2)/pend, 2)
  cat("Number of regimes: ",length(psi)+1,"\n") #, format(signif(x$sigma.new[i],
  cat("% Growth Rates: ", paste(rates, collapse="  "), "\n" )
  cat("Doubling Times: ", paste(dday, collapse = "  "),"\n")
  cat("Breakpoints:   ", paste(psi,collapse = "  "),"\n")
  if(!is.null(psi.date)) cat("    (dates): ", paste(psi.date,collapse = "  "),"\n")

}

plot.segmented19<-function(obj, leg=TRUE, logs=FALSE, col, grey=FALSE, add=FALSE, prev.trend=TRUE, psi.int=TRUE,
                           Xlab="Days", Ylab="No. of  total  cases", overall=FALSE, main,
                           rm.year.lab=TRUE, grid=TRUE, pcol=grey(.5), pcex=1.7, Xlim=NULL,Ylim=NULL,
                           file=NULL, K=.95){
  #plotting segmented19 fits...
  #obj: the segmented19 fit
  #logs: if TRUE the plot is on the log
  #file: possible file name. If NULL, the plot is produced in R
  #leg: disegnare gli IC delle pendenze?
  #K: fattore per le dimensioni del grafico
  if(add) psi.int<-leg<-grid<-FALSE

  psi<-obj$psi[,"Est."]
  #col=grey(seq(.5,.1,l=length(psi)+1))
  if(missing(col)) {
    if(grey) {
    col=grey(seq(.5,.1,l=length(psi)+1))
    } else {
    col=2:(2+nrow(obj$psi))
    col=setdiff(col, 8)
    }
    }
  pcol=adjustcolor(pcol, alpha=.5)
  myc<-rep(col, l=length(psi)+1)
  myc<-adjustcolor(myc, alpha=.6)
  if(logs) Ylab<-paste("log(",Ylab, ")",sep="")
  x<-obj$model[[obj$nameUV$Z]]
  y<- obj$model[[1]] #log conteggi
  n<-length(y)
  if(!is.null(file)) pdf(file, width=7*K, height=5*K)
  myf<-if(logs) log else I
  myf1<-if(logs) I else exp

  if(!add){
    if(!is.null(obj$origin)) {
      date.all<-as.Date(0:(n-1), origin = obj$origin) # index correction..
      plot(1:length(y), myf(y), pch=20, col=pcol, cex=pcex, xlab="", xaxt="n", yaxt="n",
           ylab=Ylab, xlim=Xlim, ylim=Ylim)
      if(rm.year.lab) date.all<-sapply(lapply(strsplit(as.character(date.all),"-"),
                                              function(x)x[-1]), function(x)paste(x, collapse="-"))
      axis(1, at=1:n, labels=date.all, las=2, cex.axis=.5)
    } else {
      plot(1:length(y), myf(y), pch=20, col=gray(.5, alpha=.5), cex=pcex, xlab=Xlab, ylab=Ylab,
           yaxt="n",xaxt="n",xlim=Xlim, ylim=Ylim)
      axis(1, at=1:n, cex.axis=.5)
    }
    labY<-axTicks(2)
    labYnum<-labY<-seq.int(labY[1],labY[length(labY)],length.out=6)
    if(!logs) labY<-paste(c(labY[1], paste(labY[-1]/1000,"K",sep="")))
    axis(2, las=2, cex.axis=.7, labels= labY, at=labYnum)
  }
  plot.segmented(obj, col=myc, lwd=3, add=TRUE, link=logs)

  #plot.segmented(obj, col=myc, lwd=4, cex.axis=.5, link=logs, add=TRUE)
  x<-seq(min(x), max(x),l=100)
  #aggiungi le linee trattegiate:
  if(prev.trend){
      intc<- intercept(obj)[[1]]
      pend<- slope(obj)[[1]]
      lines(x, myf1(intc[1]+ pend[1]*x), col=myc[1], lty=2,lwd=1.5)
      if(length(psi)>=2) {
        for(i in 2:(length(psi)+1)) lines(x[x>psi[i]], myf1(intc[i]+pend[i]*x)[x>psi[i]], col=myc[i], lty=2, lwd=1.5)
      }
  }
  if(overall) {
    o<-glm(y~x,family=poisson)
    lines(x, myf1(coef(o)[1] + coef(o)[2]*x))
  }
  points.segmented(obj, col=myc[-1], pch=4, transf=myf1)
  tassi<-round(100*(exp(slope(obj)[[1]][,1])-1),1)
  tassi1<-round(100*(exp(slope(obj)[[1]][,4])-1),1) #CI inf
  tassi2<-round(100*(exp(slope(obj)[[1]][,5])-1),1) #CI sup
  tassiOK<-tassi
  for(i in 1:length(tassi)) tassiOK[i]<-paste(tassi[i] ," (",tassi1[i],", ",tassi2[i] ,")",sep="")

  if(leg) legend("topleft",legend=tassiOK, bty="n",col=myc,lwd=2, cex=.65)
  if(psi.int){
    axis(1, at=c(1, psi[1]), labels=FALSE, tick=TRUE, col=myc[1], lwd=2,line=.5)
    if(length(psi)>=2) {
      for(i in 2:(length(psi)+1)) axis(1, at=c(psi[(i-1)]+.1,psi[i]), labels=F, tick=T, col=myc[i], line=.5, lwd=2)
    }
    axis(1, at=c(psi[length(psi)]+.1,max(x)), labels=FALSE, tick=TRUE, col=myc[length(myc)], lwd=2,line=.5)
  }
  if(grid){
    abline(v=1:length(y), lty=3, col=grey(.8))
    abline(h=labYnum, lty=3, col=grey(.8))
  }

  if(!missing(main)) title(main=main)

  if(!is.null(file)) dev.off()
}
