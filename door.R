## Door

## Defining class "door" using S3
cand<-2 
class(cand)<-"door"

## The generic function is useless
PlayGame<-function(x,...){
}
PlayGame.door <-function(x){ ## defining door method
  if(x==sample(c(1:3),1)){ ## 1/3 chance to win
    print("This car is yours!")
  } else { print("No luck this time...")}
}

PlayGame.door(cand)

## Defining class "door" using S4

## The class takes numeric element, "doorno"
setClass(Class="door",
         representation=representation(
           doorno="numeric"
         ),
         prototype=prototype(
           doorno=c()
         )
)


## Setting validity
setValidity("door", function(x){
  test1<-all(!is.numeric(x@doorno)) ##Is it numeric?
  test2<-all(x@doorno>3|x@doorno<1) ##Is it between 1 and 3?
  test3<-all(x@doorno1%%1~=0)       ##Is it a natural number?
  
  if(test1&test2&test3){return("invalid value")}
})

## Setting generic, empty function
setGeneric("PlayGame",
           function(object="door"){
             standardGeneric("PlayGame")
           })

## Initializing method for door
setMethod("initialize", "door", function(.Object,...){
  value=callNextMethod()
  validObject(value)
  return(value)
})

## Setting method "door" for playgame
setMethod("PlayGame", "door",function(x){
  if(x@doorno==sample(c(1:3),1)){         ## Success if "doorno" equals sampled number
    print("This car is yours!")
  }
  else {print("No luck this time...")}
}
)

door1<-new("door", doorno=1)
PlayGame(door1)