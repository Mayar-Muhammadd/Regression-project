#name:mayar muahmmad el sayed muhammad rashwan
#ID: 20191617072
y1=c(4,3,1,2,0)
x1=c(1,2,4,6,8)
plot(y1,x1)
n=length(x1)
n
kx=sum(x1)
kx
kxx=sum(x1^2)
kxx
mx=mean(x1)
mx
ky=sum(y1)
ky
kyy=sum(y1^2)
kyy
my=mean(y1)
my
sxx=((kxx))-(n*(mx*mx))
sxx
syy=(((kyy))-(n*((my)^2)))
syy
yx=sum(x1*y1)
yx
sxy=((yx)-(n*mx*my))
sxy
B1=(sxy/sxx)
B1
B0=(my-(B1*mx))
B0
#y at x=4
X0=4
X0
yobserved=(B0+(B1*X0))
yobserved
sse=(syy-((B1)^2*sxx))
sse
sst=syy
sst
ssr=(sst-sse)
ssr
mse=(sse/(n-2))
mse
msr=(ssr)
msr
f=(msr/mse)
f
level=.95
alpha=((1-level)/2)
t=2.447
#confidence interval for b0
interval1=(B0+(t*(mse*(1/n+(mx)^2/sxx))^1/2))
interval1
interval2=(B0-(t*(mse*(1/n+(mx)^2/sxx))^1/2))
interval2
#confidence interval for b1
interval11=(B1+(t*(mse/sxx)^1/2))
interval11
interval22=(B1-(t*(mse/sxx)^1/2))
interval22
#confidence interval for mean response
interval8=(yobserved+(t*((1/n)+((X0-mx)^2/sxx)^1/2)))
interval8
interval7=(yobserved-(t*((1/n)+((X0-mx)^2/sxx)^1/2)))
interval7
#confidence interval for new observation
interval8=(yobserved+(t*(1+(1/n)+((X0-mx)^2/sxx)^1/2)))
interval8
interval7=(yobserved-(t*(1+(1/n)+((X0-mx)^2/sxx)^1/2)))
interval7

#multible
level=.95
alpha=((1-level)/2)
t=2.447

y2=matrix(c(18.58,25.23,24.84,0.76,41.54,19.04,4.76,14.92),nrow=8)
y2
yt=t(y2)
yt
my2=(mean(y2))
my2
x2=c(1,2,3,4,5,6,7,8)
x3=c(4,12,16,8,32,24,20,28)
x=matrix(c(1,1,1,1,1,1,1,1,x2,x3),ncol = 3,nrow = 8)
x
ncol(x)
nrow(x)
xt=t(x)
xt
xo=xt[,2]
xo
xo1=matrix(xo)
xo1
xo1t=t(xo1)
xo1t
p=(xt%*%x)
p
s=solve(p)
s
a=(xt%*%y2)
a
b=(s%*%a)
b
bk=b[2,1]
bk
bt=t(b)
bt
yobserved=(x%*%b)
yobserved
e=(y2-yobserved)
e
plot(e)
et=t(e)
et
#confidence interval for parameters
pt2=(bt%*%xt%*%y2)
pt2

pt1=(yt%*%y2)
pt1
sse2=(pt1-pt2)
sse2
mse2=(sse2/(nrow(x)-ncol(x)))
mse2=c(mse2)
mse2

sb=(mse2*s)
sb
v=sb[2,2]
v
#intervals are

interval=bk+(t*v)
interval
intervals=bk-(t*v)
intervals
#confidence interval for mean response
uu=xo1t%*%s
uu
yo=yobserved[2,]
yo
intervalmr=yo+(t*(mse2*uu%*%xo1))
intervalmr
intervalmr2=yo-(t*(mse2*uu%*%xo1))
intervalmr2
#confidence interval for new observation
uu=xo1t%*%s
uu
yo=yobserved[2,]
yo
intervalmr=yo+(t*(mse2*(1+uu%*%xo1)))
intervalmr
intervalmr2=yo-(t*(mse2*(1+uu%*%xo1)))
intervalmr2
#variance covariance matrix
variance=(mse2*s)
variance
#anova
sse2=(et%*%e)
sse2
ssy2=(yt%*%y2)-(nrow(x)*(my2^2))
ssy2
ssr2=ssy2-sse2
ssr2
msr2=(ssr2/(ncol(x)-1))
msr2
f2=(msr2/mse2)
f2
#standardized deviation error
sigma=(mse2)^1/2
sigma