##1)
op<-NULL
xv<-NULL
xi<-NULL
mv<-NULL
violation<-function(votes, m=TRUE,d=TRUE){
  ## Arguments m and d for m and d statistic; TRUE by default
  xv<-substr(votes,1,1) #Extracts first significant digits for each element in a vector, "votes"
  for(i in 1:9){        #Creates xi, a vector of which kth row counts frequency of k on
    xi<-c(xi,length(xv[xv==i])/length(xv)) #the first significant digits divided by length of xv.
  }
  for (i in 1:9){               #Calculates the common part of m and d statistic, which is
    mv<-c(mv,xi[i]-log10(1+1/i))  #X_i-log_10(1+1/i) for each i
  }
  
  mstat<-max(mv,na.rm=T)            #m: shows which is the maximum value among elements of mv.
  dstat<-sqrt(sum(mv^2,na.rm=T))    #d: shows square root of sum of squares of mv.
  
  
  # Returns all releavant values for calculation.
  
  op<-list(data=votes, FirstDigits=xv, Xi=xi, CommonPart=mv)
  
  # Returns results depending on if m or d is TRUE or FALSE.
  
  if(m){op<-as.list(c(op, mstat=mstat))}
  if(d){op<-as.list(c(op, dstat=dstat))}
  return(op)
}
## Testing if it is working well.
set.seed(6192)
violation(sample(c(1000:100000),30), m=T, d=T)

## 2)
## The function written above, 'violation', has 'mstat' and 'dstat' as its elements,
## which represent m statistic and d statistic, respectively.
## Using the given table, 'statcalc' function given below test if these statistics
## score higher than critical values, which are .851, .967, and 1.212 for m statistic
## for the significance level at .1, .05, and .01, and 1.212, 1.330 and 1.569 for
## d statistic for the significance level at .1, .05, and .01.

statcalc<-function(votes)
{stlist<-violation(votes)
print("Testing m-statistic")
if(stlist$mstat<.851){
  print("You cannot reject the null hypothesis at the significance level at 0.1")
}else if(stlist$mstat>=.851 & stlist$mstat<0.967){
  print("The null hypothesis rejected at the significance level at 0.1")
}else if(stlist$mstat>=0.967 & stlist$mstat<1.212){
  print("The null hypothesis rejected at the significance level at 0.05")
}else if(stlist$mstat>=1.212){
  print("The null hypothesis rejected at the significance level at 0.01")
}
print("Testing d-statistic")
if(stlist$dstat<1.212){
  print("You cannot reject the null hypothesis at the significance level at 0.1")
}else if(stlist$dstat>=1.212 & stlist$dstat<1.330){
  print("The null hypothesis rejected at the significance level at 0.1")
}else if(stlist$dstat>=1.330 & stlist$dstat<1.569){
  print("The null hypothesis rejected at the significance level at 0.05")
}else if(stlist$dstat>=1.569){
  print("The null hypothesis rejected at the significance level at 0.01")
}
}

## Now that the function is written, I test it with three sets of samples,
## one of which is sampled from the natural number in the range of (1:100000),
## another is sampled from a vector of three natural numbers,
## and the other is simply a vector of 999 rows with the same value, 999.

set.seed(21042)
## Population: c(1:100000)
statcalc(sample(c(1:100000),1000))
## Population: three natural numbers in (1:100000)
statcalc(sample((sample(c(1:100000),3)),1000,replace=T))
## Population: a natural number, 999
statcalc(rep(999,999))