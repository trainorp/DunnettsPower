# Title: Dunnet's t-test power
# Author: Patrick Trainor
# Date: 20160916
# Email: patrick.trainor@louisville.edu
# License: GPLv3
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

library(multcomp)
library(doParallel)

# Parameters:
alpha<-.05/20
iter<-10000
pardf<-expand.grid(n=seq(40,120,2),d=seq(.2,1.2,.1))

# Initialize cluster for parallel computing on multicore machines
cl<-makeCluster(6)
registerDoParallel(cl)
# Start the clock
ptm <- proc.time()
# Df2 holds simulation results
df2<-c()
for(j in 1:nrow(pardf))
{
  mu1<-pardf$d[j]
  mu2<--pardf$d[j]
  n<-pardf$n[j]

  df1<-foreach(i=1:iter,.combine=rbind,.packages='multcomp') %dopar%
  {
    df<-expand.grid(group=c("A","B","C"),sample=1:n)
    df$x<-NA
    df$x[df$group=="A"]<-rnorm(n=n,mean=0,sd=1)
    df$x[df$group=="B"]<-rnorm(n=n,mean=mu1,sd=1)
    df$x[df$group=="C"]<-rnorm(n=n,mean=mu2,sd=1)
    aov1<-aov(x~group,data=df)
    glht1<-glht(aov1,linfct = mcp(group="Dunnett"))
    sum1<-summary(glht1,test=adjusted(type="none"))
    all(sum1$test$pvalues<alpha)
  }

  df1<-data.frame(significant=sum(as.numeric(df1[,1])),d=mu1,n=n)
  df2<-rbind(df2,df1)
  print(j)
}
df2$power<-df2$significant/10000
# Stop clock
proc.time() - ptm
# End cluster
stopCluster(cl)