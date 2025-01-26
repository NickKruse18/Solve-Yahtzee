AddFieldF = function(input){
  FR = reactiveValues(FA = T)
  
  observeEvent(input$af, {
    showModal(modalDialog(
      tags$h2('What type of field should be added? (Each Field Added will Double Solution Complexity)'),
      actionButton('A1', 'Uppersection'),
      actionButton('A2', 'Pair of n'),
      actionButton('A3', 'Straight'),
      actionButton('A4', 'Chance'),
      actionButton('A5', 'Yahtzee'),
      footer = tagList(
        modalButton('Cancel')
      )
    ))
  })
  
  observeEvent(input$A1, {
    removeModal()
    showModal(modalDialog(
      tags$h2('Choose the eye value, e.g. ones, twos threes...'),
      numericInput('B1', 'Eye Value',1),
      footer = tagList(
        actionButton('nf1','Submit'),
        modalButton('Cancel')
      )
    ))
  })
  
  observeEvent(input$nf1, {
    removeModal()
    NewGame$Rules <<- SortRules(rbind(NewGame$Rules,c(1,input$B1,0,0)))
    FR$FA = !FR$FA
  })
  
  observeEvent(input$A2, {
    removeModal()
    showModal(modalDialog(
      tags$h2('Choose how many identical eyes the field should have, e.g. 3 of a kind, 4 of a kind...'),
      numericInput('C2', 'Amount in first pair',3),
      numericInput('D2', 'Amount in second pair',0),
      numericInput('B2', 'Points awarded amount (0 for sum of dice)',0),
      footer = tagList(
        actionButton('nf2','Submit'),
        modalButton('Cancel')
      )
    ))
  })
  
  observeEvent(input$nf2, {
    removeModal()
    NewGame$Rules <<- SortRules(rbind(NewGame$Rules,c(2,input$B2,input$C2,input$D2)))
    FR$FA = !FR$FA
  })
  
  observeEvent(input$A3, {
    removeModal()
    showModal(modalDialog(
      tags$h2('Choose how large the Straight should be'),
      numericInput('C3', 'Amount of consequtive numbers',4),
      numericInput('B3', 'Points awarded amount',30),
      footer = tagList(
        actionButton('nf3','Submit'),
        modalButton('Cancel')
      )
    ))
  })
  
  observeEvent(input$nf3, {
    removeModal()
    NewGame$Rules <<- SortRules(rbind(NewGame$Rules,c(3,input$B3,input$C3,0)))
    FR$FA = !FR$FA
  })
  
  observeEvent(input$A4, {
    removeModal()
    NewGame$Rules <<- SortRules(rbind(NewGame$Rules,c(4,0,0,0)))
    FR$FA = !FR$FA
  })
  
  observeEvent(input$A5, {
    removeModal()
    showModal(modalDialog(
      tags$h2('How many Points should the Yahtzee award'),
      checkboxInput('C5', 'Add the sum of Eyes.'),
      numericInput('B5', 'Flat point amount',0),
      footer = tagList(
        actionButton('nf5','Submit'),
        modalButton('Cancel')
      )
    ))
  })
  
  observeEvent(input$nf5, {
    removeModal()
    NewGame$Rules <<- SortRules(rbind(NewGame$Rules,c(5,input$B5,input$C5,0)))
    FR$FA = !FR$FA
  })
  
  observeEvent(FR$FA,ignoreInit=T,{
    NewGame$sn <<- NewGame$sn + 1
    NewGame$Rn <<- RuleNames(NewGame$Rules)
    CurrentGame(NewGame)
    Fields <<- data.frame("Category" = Rn,"Roll" = "","Scoring" = 0)
    CA <<- F
  })
}

NewGameF = function(input, output){
  observeEvent(input$ngo, {
    showModal(modalDialog(
      tags$h2('New Game Options'),
      sliderInput("ngD", "Number of Dice:", 2,10,dice,1,ticks=F),
      sliderInput("ngS", "Number of Sides:", 2,20,sides,1,ticks=F),
      sliderInput("ngR", "Number of Rerolls:", 0,10,rerolls,1,ticks=F),
      actionButton("ngC", "Clear Rules"),
      h5(textOutput("ngcom")),
      footer = tagList(
        actionButton('ngs','Submit'),
        modalButton('Close'),
        #actionButton('ngc','Reload Previous'),
        actionButton('ngg','Standard Game')
      )
    ))
  })
  
  observeEvent(input$ngs, {
    removeModal()
    NewGame$sides <<- input$ngS;  NewGame$rerolls <<- input$ngR;  NewGame$dice <<- input$ngD
    
    if(input$ngC){
      NewGame$Rules <<- matrix(0,0,4)
      NewGame$sn <<- 0
      NewGame$Rn <<- c("")
    }
    CurrentGame(NewGame)
    Fields <<- data.frame("Category" = Rn,"Roll" = "","Scoring" = 0)
    CA <<- F
  })
  
  observeEvent(input$ngc, {
    removeModal()
    NewGame <<- OldGame
    CurrentGame(NewGame)
    Fields <<- data.frame("Category" = Rn,"Roll" = "","Scoring" = 0)
  })
  
  observeEvent(input$ngg, {
    removeModal()
    NewGame <<- StandardGame()
    CurrentGame(NewGame)
    Fields <<- data.frame("Category" = Rn,"Roll" = "","Scoring" = 0)
    CA <<- F
  })
  
  output$ngcom = renderText({
    ComplexityChange(list(sides=input$ngS,rerolls=input$ngR,dice=input$ngD,Rules=NewGame$Rules,Rn=NewGame$Rn,sn=NewGame$sn))
  })
}


