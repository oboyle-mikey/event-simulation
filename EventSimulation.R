#Distribution Implementation
loadingTime <- function(){
  t = runif(1,min=0, max=1)
  if(t<=0.3){
    return(5)
  }
  if(t>0.3 && t<=0.8){
    return(10)
  }
  if(t>0.8){
    return(15)
  }
}

weighingTime <- function(){
  q = runif(1,min=0, max=1)
  if(q<=0.7){
    return(12)
  }else{
    return(16)
  }
}

travelTime <- function(){
  r = runif(1,min=0, max=1)
  if(r<=0.4){
    return(40)
  }
  if(r>0.4 && r<=0.7){
    return(60)
  }
  if(r>0.7 && r<=0.9){
    return(80)
  }
  if(r>0.9){
    return(100)
  }
}

#FIFO Queue Implementation <Barry Rowlingson>
#   I have utilised throughout the simulation the following implementation of 
#   a FIFO queue. I have used this code in order to keep track of the specific
#   positions of each truck instead of an arbitrary queue size number. This in 
#   my opinion augments the simulation. This implementation is not my work and 
#   referenced as follows:
#           Barry Rowlingson, RE: Queue Implementation, Nabble R.com 
#           URL: http://r.789695.n4.nabble.com/queue-implementation-td2529272.html
#           Accessed (26/03/2017)
queue = function(){ 
  e=new.env() 
  q=list() 
  assign("q",q,envir=e) 
  class(e)=c("queue","environment") 
  e 
} 

push.queue=function(e,v){ 
  q=c(v,get("q",envir=e)) 
  assign("q",q,envir=e) 
  v 
} 

pop.queue=function(e){ 
  q=get("q",envir=e) 
  v=q[[length(q)]] 
  if(length(q)==1){ 
    assign("q",list(),e) 
  }else{ 
    assign("q",q[1:(length(q)-1)],e) 
  } 
  return(v) 
} 


#Start of Simulation
rl = 1
endSim = 1000
for(i in 1:rl){
  
  print(c("Starting sweep", i))
  
  #Initial System States
  t=0
  LQ = 3    #Trucks in loading queue
  WQ = 0    #Trucks in weighing queue
  LT = 2    #Trucks being loaded 0,1,2
  WT = 1    #Trucks being weighed 0,1
  
  #Initial Statistic Measurements
  BL = 0    #Total time on loaders
  BW = 0    #Total time on scales
  ND = 0    #Number of deliveries
  
  #Queue List
  LList = queue()
  WList = queue()
  
  push.queue(LList, "DT4")
  push.queue(LList, "DT5")
  push.queue(LList, "DT6")
  
  
  #Future event list initial state
  times = numeric()
  times = c(loadingTime(), loadingTime(), weighingTime(), endSim)
  FELS = data.frame(times = times, event = c("EL","EL", "EW", "END"), truck = c("DT1", "DT2", "DT3", "ALL"), stringsAsFactors = FALSE)
  
  #Ststistical Measurements initial state
  BL = times[1] + times[2]
  BW = times[3]

  
  event = "EL"
  while(event!="END"){
    
    #Order FEL by time
    FELS = FELS[(order(as.numeric(FELS[,1]))),]
    
    #Initial Event
    t = FELS[1,1]
    event = FELS[1,2]
    truck = FELS[1,3]
    
    #Remove immenent event
    FELS = FELS[-c(1),]
    
    #Result of End Loading Event
    if(event=="EL"){
      
      print(c("End Loading at time",t))
      
      #Decrease number at loaders
      LT = LT-1
      
      #Add truck to weigh scales or queue for weigh scales
      if(WT==0){
        #Set end weighing future event
        WT=1
        t = strtoi(t, base = 0L)        #Issue with t variable being non-numeric
        weighT = weighingTime()
        BW = BW + weighT
        tE = t + weighT
        newEL = c(tE,"EW", truck)
        FELS = rbind(FELS, newEL)
      }else{
        #Join Queue
        push.queue(WList, truck)
        WQ = WQ+1
      }
      
      #Add truck to loader from loading queue 
      if(LT<2 & LQ>0){
        LQ = LQ-1
        LT = LT+1
        truckL = pop.queue(LList)
        t = strtoi(t, base = 0L)        #Issue with t variable being non-numeric
        loadingT = loadingTime()
        BL = BL + loadingT
        tE = t + loadingT
        newEL = c(tE, "EL", truckL)
        FELS = rbind(FELS, newEL)
      }
  
    }
    
    #Result of End Weighing Event
    if(event=="EW"){
      print(c("End Weighing at time",t))
      WT=0
      
      #Set future arrival time event
      t = strtoi(t, base = 0L)          #Issue with t variable being non-numeric
      travelT = travelTime()
      tE = t + travelTime()
      newALQ = c(tE,"ALQ", truck)
      FELS = rbind(FELS, newALQ)
      
      
      #Add truck to scales from weighing queue 
      if(WQ>0){
        WT=1
        WQ = WQ-1
        truckW = pop.queue(WList)
        t = strtoi(t, base = 0L)        #Issue with t variable being non-numeric
        weighingT = weighingTime()
        tE = t + weighingT
        BW = BW + weighingT
        newEW = c( tE, "EW", truckW)
        FELS = rbind(FELS, newEW)
      }
  
    }
    
    #Result of Arrival Loading Queue Event
    if(event=="ALQ"){
      print(c("Arrive at Loading Queue at time",t))
      ND = ND + 1
      
      #Add truck to loader scales or queue for loader
      if(LT<2){
        LT = LT + 1
        #Set end loading future event
        t = strtoi(t, base = 0L)        #Issue with t variable being non-numeric
        loadingT = loadingTime()
        tE = t + loadingT
        BL = BL + loadingT
        newEL = c(tE,"EL", truck)
        FELS = rbind(FELS, newEL)
        
      }else{
        #Join Queue
        push.queue(LList, truck)
        LQ = LQ+1
      }
      
    }
    
    #Result of End Event
    if(event=="END"){
      print(c("Simulation has ended"))
      print(c("The total time spent loading is ",BL))
      print(c("The total time spent weighing is ",BW))
      print(c("The total number of deliveries is ",ND))
    }
    
  }

}