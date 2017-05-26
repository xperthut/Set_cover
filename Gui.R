##################################################
# Name: Md. Kamruzzaman
# WSU: 11481693
# Project 2
# Math 567
##################################################

# Remove environmental variables
rm(list = ls())

RunGUI=function(){
  
  source("main.R")
  
  # Create a window with title
  win=gwindow(title = "Set covering", width = 500, height = 500, name = "Project 2:Set cover")
  group = ggroup(horizontal = FALSE, container=win)
  
  objText = glabel(text="", container = group, width = 50)
  objButton = gbutton(text = "Select file", handler = function(h,...){
    svalue(objText) = gfile()
  }, container = group)
  objRadio = gradio(c("Preprocess","No preprocess"), container=group, selected = 2)
  objCombo = gcombobox(c("Greedy","Modified greedy"), container = group)
  objRun = gbutton(text = "Start", handler = function(h,...){
    fileName = svalue(objText)
    methodName = svalue(objCombo)
    optionName = svalue(objRadio)
    
    if(nchar(fileName)<3){
      confirmDialog("Please select data file.")
      
    }else{
      
      obj = processAction(fileName, methodName, optionName)
      
      if(!is.null(obj)){
        t = obj$Time
        u = "min"
        if(t<1){
          t = t*60
          u = "sec"
        }
        
        text = paste("\nTime: ",t," ", u, "\nPoles: ",obj$Poles,"\n")
        svalue(objResultText) = text
        
      }else{
        svalue(objResultText) = "Error in processing. Check your data file."
      }
    }
    
  }, container = group)
  objResultText = gtext("", container=group, font.attr=list(style="bold"), height = 80, width = 300, editable=F)
}

