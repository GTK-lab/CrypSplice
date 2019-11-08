# Beta Binomial Test #
library('ibb')
library('MASS')
args<-commandArgs(TRUE)
group <- c(rep("CTR", as.numeric(args[1])), rep("cKO", as.numeric(args[2])))
nc<-as.numeric(args[1])+as.numeric(args[2])
data<-read.table(args[3])
data<- as.matrix(data)
jc<-data[,1]
x<-data[,2:(nc+1)]
y<-data[,(nc+2):((2*nc)+1)]
Class<-data[,ncol(data)]
out<-bb.test(x, y, group,n.threads = nc)
p<-as.numeric(out$p.value)
for(j in 1:length(p))
{
   	if(is.nan(p[j]))
   	{
   		p[j]=min(p)
   	}
}

# Multiple testing correction using BH #
bp<-p.adjust(p, "bonferroni",length(p))
bhp<-p.adjust(p, "BH",length(p))
byp<-p.adjust(p, "BY",length(p))

# Writing to file #
data<-cbind(jc,x,y,p,bp,bhp,byp,Class)
write.matrix(data, file = args[4], sep = "\t")