# Example using the function prop.CI() to calculate the confidence intervals of
# a single proportion according to eleven different methods:

source("http://www.ottersbek.de/software/prop.CI.r")

# The example reproduces the data of Table I of Newcombe (1998, p. 861) in
# object "Table.I". Other methods to calculate confidence intervals of a single
# proportion as discussed in Brown et al. (2001) will also be demonstrated and
# shown in "Table.I.plus".
#
# References:
# Brown, L.D., Cai, T.T. & DasGupta, A. (2001). Interval estimation for a
#    binomial proportion. Statistical Science, 16, 101-133.
# Newcombe, R.G. (1998). Two-sided confidence intervals for the single propor-
#    tion: Comparison of seven methods. Statistics in Medicine, 17, 857-872.

ci.1C1 =prop.CI(81/263,263,digits=4,method='asymptotic')$CI
ci.2C1 =prop.CI(81/263,263,digits=4,method='asymptotic.cc')$CI
ci.3C1 =prop.CI(81/263,263,digits=4,method='score')$CI
ci.4C1 =prop.CI(81/263,263,digits=4,method='score.cc')$CI
ci.5C1 =prop.CI(81/263,263,digits=4,method='binomial')$CI
ci.6C1 =prop.CI(81/263,263,digits=4,method='binomial.midp')$CI
ci.7C1 =prop.CI(81/263,263,digits=4)$CI
ci.8C1 =prop.CI(81/263,263,digits=4,method='Jeffreys')$CI
ci.9C1 =prop.CI(81/263,263,digits=4,method='Agresti-Coull')$CI
ci.10C1=prop.CI(81/263,263,digits=4,method='Agresti.2_2')$CI
ci.11C1=prop.CI(81/263,263,digits=4,method='logit')$CI

ci.1C2 =prop.CI(15/148,148,digits=4,method='asymptotic')$CI
ci.2C2 =prop.CI(15/148,148,digits=4,method='asymptotic.cc')$CI
ci.3C2 =prop.CI(15/148,148,digits=4,method='score')$CI
ci.4C2 =prop.CI(15/148,148,digits=4,method='score.cc')$CI
ci.5C2 =prop.CI(15/148,148,digits=4,method='binomial')$CI
ci.6C2 =prop.CI(15/148,148,digits=4,method='binomial.midp')$CI
ci.7C2 =prop.CI(15/148,148,digits=4)$CI
ci.8C2 =prop.CI(15/148,148,digits=4,method='Jeffreys')$CI
ci.9C2 =prop.CI(15/148,148,digits=4,method='Agresti-Coull')$CI
ci.10C2=prop.CI(15/148,148,digits=4,method='Agresti.2_2')$CI
ci.11C2=prop.CI(15/148,148,digits=4,method='logit')$CI

ci.1C3 =prop.CI(0/20,20,digits=4,method='asymptotic')$CI
ci.2C3 =prop.CI(0/20,20,digits=4,method='asymptotic.cc')$CI
ci.3C3 =prop.CI(0/20,20,digits=4,method='score')$CI
ci.4C3 =prop.CI(0/20,20,digits=4,method='score.cc')$CI
ci.5C3 =prop.CI(0/20,20,digits=4,method='binomial')$CI
ci.6C3 =prop.CI(0/20,20,digits=4,method='binomial.midp')$CI
ci.7C3 =prop.CI(0/20,20,digits=4)$CI
ci.8C3 =prop.CI(0/20,20,digits=4,method='Jeffreys')$CI
ci.9C3 =prop.CI(0/20,20,digits=4,method='Agresti-Coull')$CI
ci.10C3=prop.CI(0/20,20,digits=4,method='Agresti.2_2')$CI
ci.11C3=prop.CI(0/20,20,digits=4,method='logit')$CI

ci.1C4 =prop.CI(1/29,29,digits=4,method='asymptotic')$CI
ci.2C4 =prop.CI(1/29,29,digits=4,method='asymptotic.cc')$CI
ci.3C4 =prop.CI(1/29,29,digits=4,method='score')$CI
ci.4C4 =prop.CI(1/29,29,digits=4,method='score.cc')$CI
ci.5C4 =prop.CI(1/29,29,digits=4,method='binomial')$CI
ci.6C4 =prop.CI(1/29,29,digits=4,method='binomial.midp')$CI
ci.7C4 =prop.CI(1/29,29,digits=4)$CI
ci.8C4 =prop.CI(1/29,29,digits=4,method='Jeffreys')$CI
ci.9C4 =prop.CI(1/29,29,digits=4,method='Agresti-Coull')$CI
ci.10C4=prop.CI(1/29,29,digits=4,method='Agresti.2_2')$CI
ci.11C4=prop.CI(1/29,29,digits=4,method='logit')$CI

Table.I = cbind(rbind(ci.1C1,ci.2C1,ci.3C1,ci.4C1,ci.5C1,ci.6C1,ci.7C1),
                rbind(ci.1C2,ci.2C2,ci.3C2,ci.4C2,ci.5C2,ci.6C2,ci.7C2),
                rbind(ci.1C3,ci.2C3,ci.3C3,ci.4C3,ci.5C3,ci.6C3,ci.7C3),
                rbind(ci.1C4,ci.2C4,ci.3C4,ci.4C4,ci.5C4,ci.6C4,ci.7C4))
rownames(Table.I)=c('asymptotic','asymptotic CC','score','score CC',
                    'exact','mid-p','likelihood')
colnames(Table.I)=c('L(81/263)','U(81/263)',
                    'L(15/148)','U(15/148)',
                    'L(0/20)','U(0/20)',
                    'L(1/29)','U(1/29)')

Table.I.plus = cbind(rbind(ci.8C1,ci.9C1,ci.10C1,ci.11C1),
                     rbind(ci.8C2,ci.9C2,ci.10C2,ci.11C2),
                     rbind(ci.8C3,ci.9C3,ci.10C3,ci.11C3),
                     rbind(ci.8C4,ci.9C4,ci.10C4,ci.11C4))
rownames(Table.I.plus)=c('Jeffreys','Agresti-Coull','Agresti.2_2','logit')
colnames(Table.I.plus)=colnames(Table.I)

# Newcombe (1998, Table I):
cat('\nNewcombe (1998, Table I):\n')
print(round(Table.I,4))
# Additional CIs (see: Brown et al., 2001):
cat('\nAdditional CIs (see: Brown et al., 2001):\n')
print(round(Table.I.plus,4))
