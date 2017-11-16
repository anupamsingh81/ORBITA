library(ggplot2)
bayestriplotci = function(hci,lci,alpha=1,a1,b1,c1,d1,title){
   p = c1/(c1+d1)
 
 ror = function (a){
   del = 1 -a/100
   k = del*(1-p)/(1-p*del)
   log(k)
 }
 
  xpr = (ror(hci)+ror(lci))/1.96
 sdpr = abs(ror(hci)-ror(lci))/3.92

xe = log((a1*d1)/(b1*c1))


sde = sqrt(1/a1 + 1/b1+1/c1+1/d1)




 espr = alpha*(4/(sdpr*sdpr))
 esxe = 4/(sde*sde)
 
 xpost = (espr*xpr+esxe*xe)/(espr+esxe)
 sdpost = 2/sqrt(espr+esxe)
 
ggplot(data.frame(x=c(-2,2)),aes(x=x))+stat_function(fun=dnorm,args=list(xpr,sdpr),aes(color="Prior"))+stat_function(fun=dnorm,args=list(xe,sde),aes(color="Data"))+stat_function(fun=dnorm,args=list(xpost,sdpost),aes(color="Posterior"))+xlab("Log Odds Ratio")+ylab("Frequency")+labs(title=title)}




bayesprop = function(xpr,sdpr,alpha,a1,b1,c1,d1,rope1,rope2){

xe = log((a1*d1)/(b1*c1))


sde = sqrt(1/a1 + 1/b1+1/c1+1/d1)




 espr = alpha*(4/(sdpr*sdpr))
 esxe = 4/(sde*sde)
 
 xpost = (espr*xpr+esxe*xe)/(espr+esxe)
 sdpost = 2/sqrt(espr+esxe)
 
 
 
 p = c1/(c1+d1)
 
 ror = function (a){
   del = 1 -a/100
   k = del*(1-p)/(1-p*del)
   log(k)
 }
 
 
 ten =ror(10)
 twenty = ror(20)
 thirty = ror(30)
 
 tenp = 100*pnorm(ten,xpost,sdpost)
 
twentyp = 100*pnorm(twenty,xpost,sdpost)
 
thirtyp = 100*pnorm(thirty,xpost,sdpost)

zerop = 100*pnorm(0,xpost,sdpost)

 


rope1x= ror(rope1)
rope2x= ror(rope2)

rope1p = 100*pnorm(rope1x,xpost,sdpost)
 
rope2p = 100*pnorm(rope2x,xpost,sdpost)

eqreg = abs(rope1p-rope2p)

lor = round(exp(xpr -1.96*sdpr),2)
hor = round(exp(xpr+1.96*sdpr),2)

conc = c(xpr,sdpr,lor,hor,alpha,espr,xe,sde,esxe,xpost,sdpost,zerop,tenp,twentyp,thirtyp,eqreg)
 
round(conc,2)

n = c(" Prior log OR","Prior SD","Lower limit prior OR ","Higher limit limit prior OR ","Power Prior","Effective Sample size Prior"," Data(log OR)","Data SD","Effective Sample size Data"," Posterior log OR"," Posterior SD","Pr(Risk Reduction>0%)","Pr(Risk reduction>10%)","Pr(Risk reduction>20%)","Pr(Risk reduction>30%)","Pr(Risk reduction in equivalence region)")
names(conc) = n
round(conc,2)}

bayesci = function(hci,lci,alpha,a1,b1,c1,d1,rope1,rope2){
   p = c1/(c1+d1)
 
 ror = function (a){
   del = 1 -a/100
   k = del*(1-p)/(1-p*del)
   log(k)
 }
 
  xpr = (ror(hci)+ror(lci))/1.96
 sdpr = abs(ror(hci)-ror(lci))/3.92

xe = log((a1*d1)/(b1*c1))


sde = sqrt(1/a1 + 1/b1+1/c1+1/d1)




 espr = alpha*(4/(sdpr*sdpr))
 esxe = 4/(sde*sde)
 
 xpost = (espr*xpr+esxe*xe)/(espr+esxe)
 sdpost = 2/sqrt(espr+esxe)
 
 
 
 p = c1/(c1+d1)
 
 ror = function (a){
   del = 1 -a/100
   k = del*(1-p)/(1-p*del)
   log(k)
 }
 
 
 ten =ror(10)
 twenty = ror(20)
 thirty = ror(30)
 
 tenp = 100*pnorm(ten,xpost,sdpost)
 
twentyp = 100*pnorm(twenty,xpost,sdpost)
 
thirtyp = 100*pnorm(thirty,xpost,sdpost)

zerop = 100*pnorm(0,xpost,sdpost)

 


rope1x= ror(rope1)
rope2x= ror(rope2)

rope1p = 100*pnorm(rope1x,xpost,sdpost)
 
rope2p = 100*pnorm(rope2x,xpost,sdpost)

eqreg = abs(rope1p-rope2p)

lor = round(exp(xpr -1.96*sdpr),2)
hor = round(exp(xpr+1.96*sdpr),2)

conc = c(xpr,sdpr,lor,hor,alpha,espr,xe,sde,esxe,xpost,sdpost,zerop,tenp,twentyp,thirtyp,eqreg)
 
round(conc,2)

n = c(" Prior log OR","Prior SD","Lower limit prior OR ","Higher limit limit prior OR ","Power Prior","Effective Sample size Prior"," Data(log OR)","Data SD","Effective Sample size Data"," Posterior log OR"," Posterior SD","Pr(Risk Reduction>0%)","Pr(Risk reduction>10%)","Pr(Risk reduction>20%)","Pr(Risk reduction>30%)","Pr(Risk reduction in equivalence region)")
names(conc) = n
round(conc,2)}


bayestotal = function(hci,lci,expes,alpha,a1,b1,c1,d1,rope1=5,rope2=-5){
  
  p = c1/(c1+d1)
 
 ror = function (a){
   del = 1 -a/100
   k = del*(1-p)/(1-p*del)
   log(k)
 }
 
 xopt= ror(expes)
 sdt = abs(xopt)/1.96
 skep = 0
  
  m = rbind(
bayesprop(xpr=0,sdpr=5,alpha=alpha,a1=a1,b1=b1,c1=c1,d1=d1,rope1=rope1,rope2=rope2),bayesprop(xpr=0,sdpr=1,alpha=alpha,a1=a1,b1=b1,c1=c1,d1=d1,rope1=rope1,rope2=rope2),bayesprop(xpr=0,sdpr=0.35,alpha=alpha,a1=a1,b1=b1,c1=c1,d1=d1,rope1=rope1,rope2=rope2),bayesci(hci=hci,lci=lci,alpha=alpha,a1=a1,b1=b1,c1=c1,d1=d1,rope1=rope1,rope2=rope2),bayesprop(xpr=xopt,sdpr=sdt,alpha=alpha,a1=a1,b1=b1,c1=c1,d1=d1,rope1=rope1,rope2=rope2),bayesprop(xpr=skep,sdpr=sdt,alpha=alpha,a1=a1,b1=b1,c1=c1,d1=d1,rope1=rope1,rope2=rope2))

  m = as.data.frame(m)

n = c(" Prior log OR","Prior SD","Lower limit prior OR ","Higher limit limit prior OR ","Power Prior","Effective Sample size Prior"," Data(log OR)","Data SD","Effective Sample size Data"," Posterior log OR"," Posterior SD","Pr(Risk Reduction>0%)","Pr(Risk reduction>10%)","Pr(Risk reduction>20%)","Pr(Risk reduction>30%)","Pr(Risk reduction in equivalence region)")

names(m) =n

t(m)


}

bayestriplot = function(xpr,sdpr,alpha=1,a1,b1,c1,d1,title){

xe = log((a1*d1)/(b1*c1))


sde = sqrt(1/a1 + 1/b1+1/c1+1/d1)




 espr = alpha*(4/(sdpr*sdpr))
 esxe = 4/(sde*sde)
 
 xpost = (espr*xpr+esxe*xe)/(espr+esxe)
 sdpost = 2/sqrt(espr+esxe)
 
 
ggplot(data.frame(x=c(-2,2)),aes(x=x))+stat_function(fun=dnorm,args=list(xpr,sdpr),aes(color="Prior"))+stat_function(fun=dnorm,args=list(xe,sde),aes(color="Data"))+stat_function(fun=dnorm,args=list(xpost,sdpost),aes(color="Posterior"))+xlab("Log Odds Ratio")+ylab("Frequency")+labs(title=title)}


bayestriplotci = function(hci,lci,alpha=1,a1,b1,c1,d1,title){
   p = c1/(c1+d1)
 
 ror = function (a){
   del = 1 -a/100
   k = del*(1-p)/(1-p*del)
   log(k)
 }
 
  xpr = (ror(hci)+ror(lci))/1.96
 sdpr = abs(ror(hci)-ror(lci))/3.92

xe = log((a1*d1)/(b1*c1))


sde = sqrt(1/a1 + 1/b1+1/c1+1/d1)




 espr = alpha*(4/(sdpr*sdpr))
 esxe = 4/(sde*sde)
 
 xpost = (espr*xpr+esxe*xe)/(espr+esxe)
 sdpost = 2/sqrt(espr+esxe)
 
ggplot(data.frame(x=c(-2,2)),aes(x=x))+stat_function(fun=dnorm,args=list(xpr,sdpr),aes(color="Prior"))+stat_function(fun=dnorm,args=list(xe,sde),aes(color="Data"))+stat_function(fun=dnorm,args=list(xpost,sdpost),aes(color="Posterior"))+xlab("Log Odds Ratio")+ylab("Frequency")+labs(title=title)}





bayestriplotes = function(expes,alpha=1,a1,b1,c1,d1){
   p = c1/(c1+d1)
 
 ror = function (a){
   del = 1 -a/100
   k = del*(1-p)/(1-p*del)
   log(k)
 }


 
 xopt= ror(expes)
 sdt = abs(xopt)/1.96
 skep = 0
 
  xpr = (ror(hci)+ror(lci))/1.96
 sdpr = abs(ror(hci)-ror(lci))/3.92

xe = log((a1*d1)/(b1*c1))


sde = sqrt(1/a1 + 1/b1+1/c1+1/d1)




 espr = alpha*(4/(sdpr*sdpr))
 esxe = 4/(sde*sde)
 
 xpost = (espr*xpr+esxe*xe)/(espr+esxe)
 sdpost = 2/sqrt(espr+esxe)
 
ggplot(data.frame(x=c(-2,2)),aes(x=x))+stat_function(fun=dnorm,args=list(xopt,sdt),aes(color="Prior"))+stat_function(fun=dnorm,args=list(xe,sde),aes(color="Data"))+stat_function(fun=dnorm,args=list(xpost,sdpost),aes(color="Posterior"))+xlab("Log Odds Ratio")+ylab("Frequency")+labs(title=" Optimistic Prior" )

ggplot(data.frame(x=c(-2,2)),aes(x=x))+stat_function(fun=dnorm,args=list(0,sdt),aes(color="Prior"))+stat_function(fun=dnorm,args=list(xe,sde),aes(color="Data"))+stat_function(fun=dnorm,args=list(xpost,sdpost),aes(color="Posterior"))+xlab("Log Odds Ratio")+ylab("Frequency")+labs(title=" Skeptical Prior" )

}


bayescont = function(eslo,eshi,alpha,hci,lci,thr,rope1,rope2){




xpr = (eslo+eshi)/1.96


sdpr = abs(eslo-eshi)/3.92
xe = (hci+lci)/1.96


sde = abs(hci-lci)/3.92




 espr = alpha*(4/(sdpr*sdpr))
 esxe = 4/(sde*sde)
 
 xpost = (espr*xpr+esxe*xe)/(espr+esxe)
 sdpost = 2/sqrt(espr+esxe)

lopost = xpost-1.96*sdpost
hipost= xpost+1.96*sdpost


 lop = 100 - 100*pnorm(eslo,xpost,sdpost)
 
midp = 100 - 100*pnorm(xpr,xpost,sdpost)
 
thrp = 100 - 100*pnorm(thr,xpost,sdpost)

zerop = 100 - 100*pnorm(0,xpost,sdpost)



rope1p = 100 - 100*pnorm(rope1,xpost,sdpost)
 
rope2p = 100 - 100*pnorm(rope2,xpost,sdpost)

eqreg = abs(rope1p-rope2p)

final = c(xpr,sdpr,alpha,esxe/espr,xe,sde,xpost,sdpost,lopost,hipost,zerop,thrp,eqreg)

names(final) = c("Prior mean","Prior SD","Power prior","Effective sample size Data-prior ratio","Data mean","Data SD","mean posterior","sd posterior","lower ci posterior","higher ci posterior","Pr(>0)","Pr(>threshold )","Probability in equivalence region")

f= round(final,2)
f}


bayescontplot = function(eslo,eshi,alpha,hci,lci,title){




xpr = (eslo+eshi)/1.96


sdpr = abs(eslo-eshi)/3.92
xe = (hci+lci)/1.96


sde = abs(hci-lci)/3.92




 espr = alpha*(4/(sdpr*sdpr))
 esxe = 4/(sde*sde)
 
 xpost = (espr*xpr+esxe*xe)/(espr+esxe)
 sdpost = 2/sqrt(espr+esxe)

lopost = xpost-1.96*sdpost
hipost= xpost+1.96*sdpost
lopr = xpr-1.96*sdpr
hipr= xpr+1.96*sdpr
hie= xe+1.96*sde
loe= xe-1.96*sde

k = 1.5*(max(abs(lopost),abs(hipost),abs(lopr),abs(hipr),abs(loe),abs(hie)))


ggplot(data.frame(x=c(-k,k)),aes(x=x))+stat_function(fun=dnorm,args=list(xpr,sdpr),aes(color="Prior"))+stat_function(fun=dnorm,args=list(xe,sde),aes(color="Data"))+stat_function(fun=dnorm,args=list(xpost,sdpost),aes(color="Posterior"))+xlab("Effect Size")+ylab("Frequency")+labs(title=title)}



 


