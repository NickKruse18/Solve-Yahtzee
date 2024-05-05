RemoveFieldF = function(input){
  
  observeEvent(input$ar, {
    showModal(modalDialog(
      tags$h2('Remove the chosen field(s). (Each Field Removed will half Solution Complexity)'),br(),br(),
      checkboxGroupInput("Fir", NULL, choiceNames = Rn, choiceValues = SQ(sn)),
      footer = tagList(
        actionButton('rf1','Submit'),
        modalButton('Cancel')
      )
    ))
  })
  
  observeEvent(input$rf1, {
    removeModal()
    NewGame$Rules <<- NewGame$Rules[-as.integer(input$Fir),]
    print(as.integer(input$Fir))
    print(NewGame$Rules)
    NewGame$sn <<- nrow(NewGame$Rules)
    NewGame$Rn <<- RuleNames(NewGame$Rules)
    CurrentGame(NewGame)
    Fields <<- data.frame("Category" = Rn,"Roll" = "","Scoring" = 0)
    CA <<- F
  })
}













