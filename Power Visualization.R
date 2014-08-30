library(manipulate);library(ggplot2);library(scales)

myplot <- function(sigma,mu0,mua,n,alpha) {
  z = qnorm(1-alpha)
  g = ggplot(data.frame(mu=c(27,36)),aes(x=mu))
  g = g + stat_function(fun=dnorm,geom="line",args=list(mean=mu0,sd=sigma/sqrt(n)) ,size=1,color="red")
  g = g + stat_function(fun=dnorm,geom="line",args=list(mean=mua,sd=sigma/sqrt(n)) ,size=1,color="blue",linetype=2)
  g = g + stat_function(data=NULL,fun=pnorm,geom="line",color="green",args=list(mean=mua,sd=sigma/sqrt(n),lower.tail=F) ,size=1,color="red")
  #aes(x=mu+ z * sigma/sqrt(n))
  xitc = mu0 + z * sigma/sqrt(n)
  g = g + geom_vline(xintercept = xitc, size = 1,linetype=4)
  g=g+scale_y_continuous("Probability/Power",labels=percent,breaks=seq(0,1,.05))+theme_bw()
  power=pnorm(mu0 + z * sigma/sqrt(n), mean = mua, sd = sigma/sqrt(n), lower.tail = FALSE)
  power=paste0(round(power*100,1),"%")
  g=g+geom_text(x=34,y=.9,label=paste("Power = " ,power),color="red")
  g=g+scale_x_continuous("mu0/mua")
  g
}



manipulate(myplot(sigma,mu0,mua,n,alpha),alpha=slider(0.01,.2,step=0.01,initial=0.05),sigma = slider(min = 1, max = 15,step = 1, initial = 4),
           mu0=slider(25,36,step=1,initial=30),
           mua=slider(25,36,step=1,initial=32),n=slider(10,100,step=2,initial=16)
)