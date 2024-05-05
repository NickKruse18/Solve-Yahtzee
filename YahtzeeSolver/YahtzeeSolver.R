library(shiny)
library(gridExtra)
library(rsconnect)
source("Dependences.R")

rsconnect::setAccountInfo(name='big-dick-richard',
                          token='E12C267F5F8B089B5E1E9EAAAFA330CD',
                          secret='QGim/u0IWrAi5pgpD23gUOmMS7eh8FmoXjSEAAv5')

diceinput = ceiling(sides*runif(dice))
R = rerolls
FF = c()
TR = reactiveValues(FU = T, FS = T, UP = T, FR = T)

NewGame <<- OldGame <<- StandardGame()
CurrentGame(OldGame)
CA <<- F
Fields <<- data.frame("Category" = Rn,"Roll" = "","Scoring" = 0)
ct <<- 0

ui = fluidPage(
  titlePanel("Yahtzee"),
  fluidRow(
    column(4,
      wellPanel(
        h4("Options (Press Restart After Changing)"),
        actionButton("ngo", "New Game Options"),
        actionButton("af", "Add Field", style="simple", size="sm", color = "warning"),
        actionButton("ar", "Remove Field", style="simple", size="sm", color = "warning"),
        h4("Game"),
        actionButton("restart", "Restart"),
        actionButton("reroll", "Reroll"),
        h4("Best Move"),
        textOutput("solve"),
        actionButton("calc","Calculate"),
        checkboxInput("bmo", "Enable Best Move"),
        textOutput("calctime")
      )
    ),
    column(8,
      h4(textOutput("dicethrow")),
      h5(textOutput("rerolltext")),
      #h5(textOutput("dice")),
      checkboxGroupInput("D", NULL, inline = T, choiceNames = rep("",dice), choiceValues = 1:dice),
      br(),
      fluidRow(
        column(9,
          h4("Fields"),
          tableOutput("fields"),
          h4('Total'),
          textOutput("tscore"),
        ),
        column(2,
          br(),br(),br(),br(),
          checkboxGroupInput("Fi", NULL, choiceNames = rep("",sn+(sn==0)), choiceValues = SQ(sn))
        )
      )
    )
  )
)

server = function(input, output) {
  l = reactiveValues()
  NewGameF(input, output)
  AddFieldF(input)
  RemoveFieldF(input)
  
  observeEvent(input$Fi, { TR$UP = !TR$UP
    FI = as.integer(input$Fi);  w = F
    for(i in FF){ if(i %in% FI){ next };  w = T }
    if(w){ updateCheckboxGroupInput(inputId = "Fi", label = NULL, selected = FF, choiceNames = rep("",sn+(sn==0)), choiceValues = SQ(sn)) }
    else{
      for(i in FI){
        if(i %in% FF){ next }
        FF <<- c(FF,i);  Fields$Roll[i] <<- paste(diceinput,collapse = " ");  TR$FU = !TR$FU
      }
    }
  }, ignoreNULL = F)
  
  observeEvent(TR$FU,{ TR$UP = !TR$UP;  TR$FS = !TR$FS
    R <<- rerolls
    diceinput <<- ceiling(sides*runif(dice))
    updateCheckboxGroupInput(inputId = "D", label = NULL, inline = T, choiceNames = diceinput, choiceValues = 1:dice)
  })
  
  observeEvent(input$calc,{
    showModal(modalDialog(
      tags$h4('Calculating'),
      footer = tagList( modalButton('Close') )
    ))
    ct <<- Sys.time()
    Y <<- Yahtzee()
    ct <<- round(as.numeric(Sys.time() - ct),2)
    removeModal()
    CA <<- T
    TR$FS = !TR$FS
  })
  
  observeEvent(TR$FS, { TR$UP = !TR$UP
    SS = Scoring(sort(diceinput))
    if(sn>0){
      for(i in SQ(sn)){
        if(i %in% FF){ next }
        Fields$Scoring[i] <<- SS[i]
      }
    }
  })
  
  observeEvent(TR$FR, { TR$FU = !TR$FU
    updateCheckboxGroupInput(inputId = "Fi", label = NULL, choiceNames = rep("",sn+(sn==0)), choiceValues = SQ(sn))
    FF <<- c();  Fields$Roll <<- ""
  })
  
  observeEvent(input$restart, { TR$FR = !TR$FR }, ignoreNULL = F)
  
  observeEvent(input$reroll, ignoreInit = T, { TR$UP = !TR$UP;  TR$FS = !TR$FS
    if(R<=0){ return() }
    for(i in 1:dice){
      if(i %in% as.integer(input$D)){ next }
      diceinput[i] <<- ceiling(sides*runif(1))
    }
    updateCheckboxGroupInput(inputId = "D", selected = input$D, label = NULL, inline = T, choiceNames = diceinput, choiceValues = 1:dice)
    R <<- R - 1
  }, ignoreNULL = F)
  
  update = eventReactive(TR$UP, { R; diceinput; Fields; CA; ct })
  
  
  output$rerolltext = renderText({
    update()
    return(paste0("Rerolls: ",R,"\n Dice: "))
  })
  
  output$dice = renderText({
    update()
    Text = ""
    for(i in diceinput){ Text = paste0(Text," ",i) }
    return(Text)
  })
  
  output$fields = renderTable({
    update()
    Fields
  })
  
  output$dicethrow = renderText({
    update()
    return(paste0("Dice Throw, ",rerolls," Rerolls of ",dice," Dices with ", sides," Sides each",":"))
  })
  
  output$tscore = renderText({
    update()
    return(sum(Fields$Scoring*(Fields$Roll!="")))
  })
  
  output$calctime = renderText({
    update()
    return(paste0("Last Calculation took ",ct,"."))
  })
  
  output$solve = renderText({
    update()
    if(!CA){ return("Best Move not calculated.") }
    else if(input$bmo){ BestMove(sort(diceinput),FF,rerolls-R,sum(Fields$Scoring*(Fields$Roll!=""))) }
    else{ return("Click Enable Best Move.") }
  })
}

#deployApp(appPrimaryDoc='YahtzeeSolver.R')
shinyApp(ui, server)
