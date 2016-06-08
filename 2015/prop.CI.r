# Calculate confidence intervals for a single proportion according to one of 11
# methods (default: log likelihood ratio) for a level of alpha (default: .05)
# and with significant decimals (default: 3) given a proportion and the sample
# size (see: Newcombe, 1998; Brown et al., 2001). Except for the asymptotic and
# Agresti's methods all methods can yield asymmetric intervals around p:
#
# Methods are:
# 'likelihood'    : log likelihood ratio interval (default)
# 'asymptotic'    : simple "classical text book" or Wald method interval
# 'asymptotic.cc  : asymptotic with Yates' continuity correction
# 'score'         : score or Wilson method interval
#                   (= prop.test(...,correct=FALSE),
#                   i.e. without Yates' continuity correction)
# 'score.cc'      : score method with continuity correction (= default of
#                   prop.test())
# 'binomial'      : "exact" or Clopper-Pearson method interval (= binom.test())
# 'binomial.midp' : binomial mid-p "quasi-exact" interval
# 'Jeffreys'      : Jeffreys prior interval
# 'Agresti-Coull' : Agresti-Coull method adding z?/2 successes
# 'Agresti.2_2    : Agresti-Coull method adding 2 successes and 2 failures
# 'logit'         : logit interval
#
# References:
# Brown, L.D., Cai, T.T. & DasGupta, A. (2001). Interval estimation for a
#    binomial proportion. Statistical Science, 16, 101-133.
# Newcombe, R.G. (1998). Two-sided confidence intervals for the single propor-
#    tion: Comparison of seven methods. Statistics in Medicine, 17, 857-872.

prop.CI = function(p,n,alpha=.05,digits=3,method="likelihood")
{
# Asymptotic (or Wald) interval:
  z = qnorm(1-alpha/2)
  if (method=='asymptotic') {
    se = sqrt(p*(1-p)/n)
    CI = list(p=p,CI=c((p-z*se),(p+z*se)),n=n,level=1-alpha,method=method)
  }
  
  
# Asymptotic (or Wald-test) CIs with continuity correction:
  if (method=='asymptotic.cc') {
    se = sqrt(p*(1-p)/n)
    CI = list(p=p,CI=c((p-z*se-1/(2*n)),(p+z*se+1/(2*n))),n=n,level=1-alpha,
              method=method)
  }
  
  
# Score test (or Wilson) interval:
  if (method=='score') {
    term1 = 2*n*p + z**2
    term2 = z*sqrt(z**2 + 4*n*p*(1-p))
    term3 = 2*(n + z**2)
    CI = list(p=p,CI=c((term1-term2)/term3,(term1+term2)/term3),n=n,
              level=1-alpha,method=method)
  }
  
  
# Score test (or Wilson) interval with continuity correction:
  if (method=='score.cc') {
    term1 = 2*n*p + z**2
    if (p>0) {
    term2L = z*sqrt(z**2 - 2 - 1/n + 4*p*(n*(1-p)+1))
    }
    if (p<1) {
    term2U = z*sqrt(z**2 + 2 - 1/n + 4*p*(n*(1-p)-1))
    }
    term3 = 2*(n + z**2)
    if ((p>0) & (p<1)) {
      CI = list(p=p,CI=c((term1-1-term2L)/term3,(term1+1+term2U)/term3),n=n,
                level=1-alpha,method=method)
    }
    if (p==0) {
      CI = list(p=p,CI=c(0,CIU=(term1+1+term2U)/term3),n=n,level=1-alpha,
                method=method)
    }
    if (p==1) {
      CI = list(p=p,CI=c((term1-1-term2L)/term3,1),n=n,level=1-alpha,
                method=method)
    }
  }
  
  
# Binomial ('exact' or Clopper-Pearson) interval:
  if (method=='binomial') {
    conf.int=binom.test(round(p*n),n,conf.level=1-alpha)$conf.int
    CI = list(p=p,CI=c(conf.int[1],conf.int[2]),n=n,level=1-alpha,method=method)
  }
  
  
# Binomial mid-p quasi-exact interval:
  if (method=='binomial.midp') {
    x = round(p*n)
    uplim = 1
    lowlim = 0
    if (x == 0) uplim = 1-alpha**(1/n)
    if (x == n) lowlim = alpha**(1/n)
    if (x > 0 & x < n) {
      pp = seq(0.000001,0.999999,length=100000)
      a2 = 0.5*pbinom(x-1,n,pp) + 0.5*pbinom(x,n,pp)
      uplim = pp[max(which(a2>(alpha/2)))]
      lowlim = pp[min(which(a2<(1-alpha/2)))]
    }
    CI = list(p=p,CI=c(lowlim,uplim),n=n,level=1-alpha,method=method)
  }
  
  
# Log-likelihood-ratio interval:
  if (method=='likelihood') {
    x = round(p*n)
    k = -qchisq(1-alpha,1)/2
    pp = seq(0.000001,0.999999,length=100000)
    lik = dbinom(x,size=n,pp)
    logLR = log(lik/max(lik))
    conf.int=range(pp[logLR > k])
    CI = list(p=p,CI=c(conf.int[1],conf.int[2]),n=n,level=1-alpha,method=method)
  }
  
  
# Jeffreys prior interval:
  if (method=='Jeffreys') {
    x = round(p*n)
    conf.int=qbeta(c(alpha/2,1-alpha/2),x+.5,n-x+.5)
    CI = list(p=p,CI=c(conf.int[1],conf.int[2]),n=n,level=1-alpha,method=method)
  }
  
  
# Agresti-Coull (adding z?/2 successes) interval
# (see: http://www.stat.ufl.edu/~aa/cda/R/one_sample/R1/index.html )
  if (method=='Agresti-Coull') {
    x = round(p*n)
    tr = z**2
    suc = tr/2
    pp = (x+suc)/(n+tr)
    se = sqrt(pp*(1-pp)/(n+tr))
    CI = list(p=p,CI=c((pp-z*se),(pp+z*se)),n=n,level=1-alpha,method=method)
    if (CI$CI[1] < 0) CI$CI[1]=0
    if (CI$CI[2] > 1) CI$CI[2]=1
  }
  
  
# Agresti-Coull (adding 2 successes and 2 failures) interval:
# (see: http://www.stat.ufl.edu/~aa/cda/R/one_sample/R1/index.html )
  if (method=='Agresti.2_2') {
    x = round(p*n)
    pp = (x+2)/(n+4)
    se = sqrt(pp*(1-pp)/(n+4))
    CI = list(p=p,CI=c((pp-z*se),(pp+z*se)),n=n,level=1-alpha,method=method)
    if (CI$CI[1] < 0) CI$CI[1]=0
    if (CI$CI[2] > 1) CI$CI[2]=1
  }
  
  
# Logit interval:
  if (method=='logit') {
    lambda = log(p/(1-p))
    x = round(p*n)
    V = n/(x*(n-x))
    conf.int = (c(lambda - z*sqrt(V),lambda + z*sqrt(V)))
    conf.int = exp(conf.int)/(1+exp(conf.int))
    CI = list(p=p,CI=c(conf.int[1],conf.int[2]),n=n,level=1-alpha,method=method)
  }
  
  
  cat('p ? ',100*(1-alpha),'%-CI = ',round(p,digits),' (',
      round(CI$CI[1],digits),'; ',round(CI$CI[2],digits),')\n',sep='')
  CI
}

# The following example  reproduces the data of Table I of Newcombe (1998,
# p. 861). Other methods to calculate confidence intervals of a single propor-
# tion as discussed in Brown et al. (2001) will also be demonstrated.
#
# To run, de-comment the following line:
# source("http://www.ottersbek.de/software/ex_prop.CI.r")
