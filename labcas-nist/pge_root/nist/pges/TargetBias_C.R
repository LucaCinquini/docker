### Extract Esitmated Tissue Proportions and Target Bias using Jerod's TargetPlot function

# Expectation : Call TargetPlot with the following:  [input df, site_platform]
getTargetVectors<-function(lab,indf,correction=c(1,1,1),expectation=data.frame(mix1=c(.25,.25,.5),mix2=c(.5,.25,.25)),isLog=TRUE,metrics=FALSE){
  # Inputs:
  #` @indf - Input Dataframe: (Expectation: R03_All_Tall_recast
  temp<-indf
  # Kill all NA/0s
  temp<-subset(temp,rowSums(temp[,4:8])>0)
  #indf<-normalizedata(indf,type="none") #LSN Only.
  all<-suppressMessages(reshape2::melt(indf))
  all$Tissue<-gsub(x=all$variable,pattern="Rep[0-9]",replacement="")
  all$Replicate<-gsub(x=all$variable,pattern="Liver|Brain|Placenta|Mix1|Mix2",replacement="")
  all$Replicate<-as.integer(as.character(gsub(x=all$Replicate,pattern="Rep",replacement="")))
  colnames(all)[1]<-"miR"
  all<-reshape2::dcast(all,miR+Replicate~Tissue+.,fun.aggregate=mean,na.rm=TRUE)

  if(any(is.na(correction))){
    error("not implemented: self-correction-calculation")
  #  correction<-makeconsensus(means)
  }# TODO:  test and implement automatic mrna-fraction correction with makeconsensus:

circleFun<-function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }

lmLdf<-function(qa){
  out=coef(qa)
  out$blower=0
  out$llower=0
  out$plower=0
  out$bupper=1
  out$lupper=1
  out$pupper=1
  for(I in attr(qa,"names")){
    ci=confint(qa[[I]])
    out$blower[rownames(out)==I]<-ci[1]
    out$llower[rownames(out)==I]<-ci[2]
    out$plower[rownames(out)==I]<-ci[3]
    out$bupper[rownames(out)==I]<-ci[4]
    out$lupper[rownames(out)==I]<-ci[5]
    out$pupper[rownames(out)==I]<-ci[6]
  }
  return(out)
}

targetblotse<-function(lab,interval1,interval2,tabular=FALSE){
  a<-interval1[,c(4,1,7,5,2,8,6,3,9)] #reorganize
  a2<-interval2[,c(4,1,7,5,2,8,6,3,9)] #reorganize
  #normalize to 1
  a<-a/rowSums(a[c(2,5,8)])
  a2<-a2/rowSums(a2[c(2,5,8)])
  colnames(a)<-c("lower.Brain","bleh","upper.Brain","lower.Liver","blehh","upper.Liver","lower.Placenta","blaaaa","upper.Placenta")
  colnames(a2)<-c("lower.Brain","bleh","upper.Brain","lower.Liver","blehh","upper.Liver","lower.Placenta","blaaaa","upper.Placenta")
  ellipse<-NULL
  for(I in 1:nrow(a)){
    tellipse<-data.frame(Brain=rnorm(n=1000,mean=a$bleh[I],sd=(a$bleh[I]-a$lower.Brain[I])/1.96),Liver=rnorm(n=1000,mean=a$blehh[I],sd=(a$blehh[I]-a$lower.Liver[I])/1.96),Placenta=rnorm(n=1000,mean=a$blaaaa[I],sd=(a$blaaaa[I]-a$lower.Placenta[I])/1.96),
                         Brain2=rnorm(n=1000,mean=a2$bleh[I],sd=(a2$bleh[I]-a2$lower.Brain[I])/1.96),Liver2=rnorm(n=1000,mean=a2$blehh[I],sd=(a2$blehh[I]-a2$lower.Liver[I])/1.96),Placenta2=rnorm(n=1000,mean=a2$blaaaa[I],sd=(a2$blaaaa[I]-a2$lower.Placenta[I])/1.96))
    tellipse<-suppressMessages(melt(tellipse))
    tellipse<-cbind(tellipse[1:3000,],tellipse[3001:6000,2])
    colnames(tellipse)[3]<-"value2"
    tellipse$Lab<-rownames(a)[I]
    ellipse<-rbind(ellipse,tellipse)
  }
  aa<-data.frame(x=c(a$lower.Brain,a$lower.Brain,a$upper.Brain,a$upper.Brain,a$lower.Brain,NA,a$lower.Liver,a$lower.Liver,a$upper.Liver,a$upper.Liver,a$lower.Liver,NA,a$lower.Placenta,a$lower.Placenta,a$upper.Placenta,a$upper.Placenta,a$lower.Placenta),
                 y=c(a2$lower.Brain,a2$upper.Brain,a2$upper.Brain,a2$lower.Brain,a2$lower.Brain,NA,a2$lower.Liver,a2$upper.Liver,a2$upper.Liver,a2$lower.Liver,a2$lower.Liver,NA,a2$lower.Placenta,a2$upper.Placenta,a2$upper.Placenta,a2$lower.Placenta,a2$lower.Placenta)
  )
  ellipse$Tissue<-ellipse$variable
  aa$Lab<-c(rep(rownames(a),5),rownames(a)[1],rep(rownames(a),5),rownames(a)[1],rep(rownames(a),5)) #kinda ugly but its okay.
  aa$Tissue<-c(rep("Brain",length(rownames(a))*5),"Brain",rep("Liver",length(rownames(a))*5),"Liver",rep("Placenta",length(rownames(a))*5))
  aa$RLab<-data.table::tstrsplit(aa$Lab,split="/")[[1]]
  ellipse$RLab<-data.table::tstrsplit(ellipse$Lab,split="/")[[1]]
  #dft calculation
  a$dft<-round(sqrt(((a$bleh-0.25)^2+(a$blehh-0.25)^2+(a$blaaaa-0.5)^2)),digits=3)
  a2$dfty<-round(sqrt(((a2$bleh-0.25)^2+(a2$blehh-0.25)^2+(a2$blaaaa-0.5)^2)),digits=3)
  a$Lab<-rownames(a)
  a2$Lab<-rownames(a2)
  a$RLab<-data.table::tstrsplit(a$Lab,split="/")[[1]]
  a2$RLab<-data.table::tstrsplit(a2$Lab,split="/")[[1]]
  if(tabular==TRUE){return(list(aa,ellipse))}
  #    g<-ggplot(data=aa)+geom_polygon(aes(x,y,col=Tissue,fill=Tissue),alpha=0.4,size=3)+facet_wrap(~Lab)+coord_cartesian(ylim=c(0,1),xlim=c(0,1))+geom_point(data=data.frame(x=c(0.25,0.25,0.5),y=c(0.5,0.25,0.25)),aes(x,y))+stat_ellipse(data=ellipse,aes(x=value,y=value2,col=Tissue,fill=Tissue),geom="polygon",alpha=0.4,size=2.5)
  require(ggplot2)
  setdi<-0.1
  ncirc <- 3
  theta <- seq(from=0,by=.01,to=2*pi)
  circleFun<-function(n,xx,yy,setdi,ncirc,tiss){
    r <- n*setdi/ncirc
    x<-data.frame(x=xx+r*sin(theta),y=yy+r*cos(theta),r=paste(tiss,round(r,2)))
  } # builds circles
  csf <- expectation
  csf$col<-c("Brain","Liver","Placenta")
  cdf <- do.call(rbind,
                 mapply(n=rep(seq(1,ncirc),each=length(csf$mix1)),xx=csf$mix1,yy=csf$mix2,tiss=csf$col,FUN=circleFun,MoreArgs=list(setdi,ncirc),SIMPLIFY=FALSE))
    pointdata<-rbind(subset(a,Lab==lab)[1:9],subset(a2,Lab==lab)[1:9])[c(2,5,8)] # pulls just the centerpoints of BLP fits in Mix1 and Mix2
    colnames(pointdata)<-c("Brain","Liver","Placenta")
    # extract mixture proportions
    M1_L <- pointdata["Liver"][1,]
    M1_B <- pointdata["Brain"][1,]
    M1_P <- pointdata["Placenta"][1,]
    Mix1 <- c(M1_L, M1_B, M1_P,NA,NA)
    
    M2_L <- pointdata["Liver"][2,]
    M2_B <- pointdata["Brain"][2,]
    M2_P <- pointdata["Placenta"][2,]
    Mix2 <- c(M2_L, M2_B, M2_P,NA,NA)
    
    # calculate bias vectors
    LiverVector    <- sqrt(((pointdata["Liver"][1,]-0.25)^2) + ((pointdata["Liver"][2,]-0.25)^2))
    BrainVector    <- sqrt(((pointdata["Brain"][1,]-0.25)^2) + ((pointdata["Brain"][2,]-0.5)^2))
    PlacentaVector <- sqrt(((pointdata["Placenta"][1,]-0.5)^2) + ((pointdata["Placenta"][2,]-0.25)^2))
    DFPp <- c(LiverVector,BrainVector,PlacentaVector,NA,(LiverVector+BrainVector+PlacentaVector))

    Component <- c("Liver","Brain","Placenta","","All")
    TissueVectors <- data.frame(cbind(Component,
                                      round(Mix1,3),
                                      round(Mix2,3),
                                      round(DFPp,3)
                                      ))
    return(TissueVectors)
}
form1<-paste0("Mix1~I(Brain*",correction[1],")+I(Liver*",correction[2],")+I(Placenta*",correction[3],
              ")+0|Site_Platform")
form2<-paste0("Mix2~I(Brain*",correction[1],")+I(Liver*",correction[2],")+I(Placenta*",correction[3],
              ")+0|Site_Platform")

mixmod<-targetblotse(interval1=lmLdf(nlme::lmList(data=as.data.frame(temp),as.formula(form1),na.action=na.omit)),interval2=lmLdf(nlme::lmList(data=as.data.frame(temp),as.formula(form2),na.action=na.omit))
             ,tabular=FALSE,lab=lab)
return(mixmod)
makeconsensus <- function(dataf){
    fitmirmodel <- function(indf,mifrac){
      if(grep("_SD",colnames(indf))){
        indf$variance<-indf[,grep("_SD",colnames(indf))]^2
        indf<-subset(indf,!is.na(variance)&variance>0)
        fit<-lm(data=indf,Mix1~I(Brain*mifrac[1])+I(Liver*mifrac[2])+I(Placenta*mifrac[3])+0,weights=1/variance)
        fit2<-lm(data=indf,Mix2~I(Brain*mifrac[1])+I(Liver*mifrac[2])+I(Placenta*mifrac[3])+0,weights=1/variance)
      }
      else{
        fit<-lm(data=indf,Mix1~I(Brain*mifrac[1])+I(Liver*mifrac[2])+I(Placenta*mifrac[3])+0)
        fit2<-lm(data=indf,Mix2~I(Brain*mifrac[1])+I(Liver*mifrac[2])+I(Placenta*mifrac[3])+0)
      }
      return(data.frame(coef1=coefficients(fit)/sum(coefficients(fit)),coef2=coefficients(fit2)/sum(coefficients(fit2))))
    }
    f2opt<-function(par,indata){sum(abs(fitmirmodel(mifrac=c(par,1),indata)[,2]-c(0.5,0.25,0.25)),abs(fitmirmodel(mifrac=c(par,1),indata)[,1]-c(0.25,0.25,0.5)))}
    opted<-optim(par=c(1,1),f2opt,indata=dataf)$par
    return(c(opted,1)/sum(c(opted,1)))
}


}