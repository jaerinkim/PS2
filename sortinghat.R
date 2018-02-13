## 1. S3 object

## student generator
stgen<-function(name){
## Sampling attributes
  courage<-sample(c(1:100),1)
  ambition<-sample(c(1:100),1)
  intelligence<-sample(c(1:100),1)
  effort<-sample(c(1:100),1)
  student <- list(name, c(courage, ambition, intelligence, effort))
## Returns a list of name and attributes
  ## Assigning names for each element
  names(student)[[1]]<-"name"
  names(student)[[2]]<-"attributes"
  names(student[[2]])<-c("courage", "ambition", "intelligence", "effort")
  class(student) <- "student"
  ## This will be a student class.
  return(student)
}


## 2. sorter (S3)
## Enter proper multiplier for the hat. This one is experimental.
multiplier<-matrix(c(1:16),nrow=4)

sort.student<-function(x,multiplier){
  thehat<-t(t(multiplier)%*%x$attributes)
## Calculating X^Ta. Transposed to utilize max.col function.
## Sorting by the maximum column
  if (max.col(thehat)==1){
  print("GRYFFINDOR!")
}
if (max.col(thehat)==2){
    print("SLYTHERIN!")
  }
  if (max.col(thehat)==3){
    print("RAVENCLAW!")
  }
  if (max.col(thehat)==4){
    print("HUFFLEPUFF!")
  }
}


## Sorter(S4)
setClass(Class="student",
         representation=representation(
           name="character",
           courage="numeric",
           ambition="numeric",
           intelligence="numeric",
           effort="numeric"
         ),
         prototype=prototype(
           name=c(),
           courage=c(),
           ambition=c(),
           intelligence=c(),
           effort=c()
         ))
matr<-matrix(c(1:16),nrow=4, ncol=4)

setMethod("initialize", "student", function(.Object,...){
  value=callNextMethod()
  validObject(value)
  return(value)
})

setGeneric("sort",
           function(x,scm){
             brokenhat<-sample(c("GRYFFINDOR!","SLYTHERIN!","RAVENCLAW!","HUFFLEPUFF!"),1)
             return(brokenhat)
           }
)

setMethod("sort", "student",
          function(x,scm,decreasing=FALSE,...){
            thehat<-t(scm%*%x[[2]])
            if (max.col(thehat)==1){
              return("GRYFFINDOR!")
            }
            if (max.col(thehat)==2){
              return("SLYTHERIN!")
            }
            if (max.col(thehat)==3){
              return("RAVENCLAW!")
            }
            if (max.col(thehat)==4){
              return("HUFFLEPUFF!")
            }
          })

stgen(b)
sort(b,matr)
## 3. Adding another class with the sorting hat result.
  class(b)<-c("student",sort(b))  


## 4. Creating environments
Gryffindor_Tower<-new.env()
Black_Lake<-new.env()
Ravenclaw_Tower<-new.env()
Basement<-new.env()

##Creating generic "curfew" function
setGeneric("curfew",
             function(x){
              if(class(x)[2]=="GRYFFINDOR!"){
              Gryffindor_Tower<-x
            }
               if(class(x)[2]=="SLYTHERIN!"){
                 Black_Lake<-x
               }
               if(class(x)[2]=="RAVENCLAW!"){
                 Ravenclaw_Tower<-x
               }
               if(class(x)[2]=="HUFFLEPUFF!"){
                 Basement<-x
               }
               
                            })
curfew(b)