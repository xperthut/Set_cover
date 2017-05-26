##################################################
# Name: Md. Kamruzzaman
# WSU: 11481693
# Project 2
# Math 567
##################################################

if(require(gWidgetstcltk)==F){
  install.packages("gWidgetstcltk", dep = TRUE,type = "source" )
  
  require(gWidgetstcltk)
  options("guiToolkit"="tcltk")
}else{
  options("guiToolkit"="tcltk")
}

if(require(gWidgets)==F){
  install.packages("gWidgets", dep = TRUE,type = "source" )
  
  require(gWidgets)
}

if(require(igraph)==F){
  install.packages("igraph")
}

source("script.R")

confirmDialog = function(message, handler=NULL, no=F) {
  window <- gwindow("File", width = 200, height = 100)
  group <- ggroup(container = window)
  gimage("info", dirname="stock", size="dialog", container=group)
  
  ## A group for the message and buttons
  inner.group <- ggroup(horizontal=FALSE, container = group)
  glabel(message, container=inner.group, expand=TRUE)
  
  ## A group to organize the buttons
  button.group <- ggroup(container = inner.group)
  ## Push buttons to right
  addSpring(button.group)
  
  if(is.null(handler)){
    gbutton("ok", handler = function(h,...) dispose(window),
            container=button.group)
  }else{
    gbutton("ok", handler=handler, container=button.group)
  }
  
  if(no){
    gbutton("cancel", handler = function(h,...) dispose(window),
            container=button.group)
  }
  
  
  return()
}

processAction = function(fileName, methodName, optionName) {
  window <- gwindow("Result", width = 200, height = 100)
  group <- ggroup(container = window)
  gimage("info", dirname="stock", size="dialog", container=group)
  
  ## A group for the message and buttons
  inner.group <- ggroup(horizontal=FALSE, container = group)
  glabel("Please wait, result is processing...", container=inner.group, expand=TRUE)
  
  obj = Main(fileName, methodName, optionName)
  
  dispose(window)
  
  return(obj)
}

Main=function(fileName, methodName, optionName){
  # read from file
  d = read.delim(fileName, sep = " ", header = F)
  
  # Create data frame
  d=data.frame("pole"=d[,2],"meter"=d[,1])
  
  # Build adjacency matrix, row=poles, col=meter
  d.adj.mat=as.matrix(get.adjacency(graph.edgelist(as.matrix(d), directed=T)))
  
  # Default selection modified greedy
  methodType = 2
  if(methodName=="Greedy"){
    methodType = 1
  }
  
  # Default selection preprocessing = false
  allowPreprocess = F
  if(optionName=="Preprocess"){
    allowPreprocess = T
  }
  
  # Preprocess = True
  if(allowPreprocess){
    
    # Greedy method
    if(methodType==1){
      return(Greedy(d.adj.mat, fold = 23, allowPreprocess = T))
    }# Modified greedy
    else{
      return(Modified.greedy(d.adj.mat, fold = 200, h=3, allowPreprocess = T))
    }
    
  }else{
    # Greedy method
    if(methodType==1){
      return(Greedy(d.adj.mat, fold = 300))
    }# Modified greedy
    else{
      return(Modified.greedy(d.adj.mat, fold = 200, h=1))
    }
  }
  
  return(NULL)
  
}

