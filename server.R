shinyServer(function(input, output, session){
  selectedGraduateDf <- reactive({ 
    selected <-graduateDf[,c("student-email","advisor-email" , "student-name","student-affiliation", "student-pronouns","student-degree-program","phd-years",                   "num-workshop","advisor-name","student-pref-roommate","student-accommodations","tutorial", "rep")]
    selected[1:input$number,"selected"] <- TRUE
    # selected <-graduateDf[1:input$number,c("student-email","advisor-email" , "student-name","student-affiliation", "student-pronouns","student-degree-program","phd-years",                   "num-workshop","advisor-name","student-pref-roommate","student-accommodations","tutorial", "rep")]
    return(selected)
  })
  
  selectedUndergraduateDf <- reactive({ 
    
    selected <-undergraduateDf[1:5,c("student-email","advisor-email" , "student-name","student-affiliation", "student-pronouns","student-degree-program","phd-years",                   "num-workshop","advisor-name","student-pref-roommate","student-accommodations","tutorial", "rep")]
    return(selected)
  })
  
  selectedGPostdocDf <- reactive({ 
    
    selected <-postdocDf[1:10,c("student-email","advisor-email" , "student-name","student-affiliation", "student-pronouns","student-degree-program","phd-years",                   "num-workshop","advisor-name","student-pref-roommate","student-accommodations","tutorial", "rep")]
    return(selected)
  })
  
  advisorDf <- reactive({ 
    advisorDf <- graduateDf %>% 
      group_by(`advisor-email`) %>%
      summarise(count = n(), name = first(`advisor-name`))
      })
    #table output:
  output$selectedGraduateDf <- DT::renderDataTable(DT::datatable(
    selectedGraduateDf(),
    # colnames=c("Name","Email", "Advisor","Advisor's email"),
    filter="top",
    caption= htmltools::tags$caption(
      style='caption-side:bottom; text-align:center;', 
      "Financial Support")
  ))
  
  #table output:
  output$selectedPostdocDf <- DT::renderDataTable(DT::datatable(
    selectedGPostdocDf(),
    # colnames=c("Name","Email", "Advisor","Advisor's email"),
    filter="top",
    caption= htmltools::tags$caption(style='caption-side:bottom; text-align:center;',  "Postdoc Financial Support")
  ))
  
  output$selectedUndergraduateDf <- DT::renderDataTable(DT::datatable(
    selectedUndergraduateDf(),
    # colnames=c("Name","Email", "Advisor","Advisor's email"),
    filter="top",
    caption= htmltools::tags$caption( style='caption-side:bottom; text-align:center;',  "Undergraduate Financial Support")
  ))
  
  output$selectedGraduateDf <- DT::renderDataTable(DT::datatable(
    selectedGraduateDf(),
    # colnames=c("Name","Email", "Advisor","Advisor's email"),
    filter="top",
    caption= htmltools::tags$caption(
      style='caption-side:bottom; text-align:center;', 
      "Graduate Financial Support")
  ))
  
  #plot ouput:
  output$Degree<-renderPlotly({
    fig <- plot_ly(graduateDf, labels = ~`student-degree-program`, type = 'pie')
    fig <- fig %>% layout(title = 'Graduate Student Degree')
    
    fig
  })
  
  output$DegreeYears<-renderPlotly({
    fig <- plot_ly(graduateDf, labels = ~`phd-years`, type = 'pie')
    fig <- fig %>% layout(title = 'Years in Degree')
    
    fig
  })
  
  # output$DegreeYearsOld<-renderPlotly({
  #   oldGraduateDf = graduateDf[graduateDf$phd-years != 1,]
  #   oldGraduateDf$yearCat = "3~4"
  #   oldGraduateDf$yearCat[oldGraduateDf$phd-years <=2 ] = "1~2"
  #   oldGraduateDf$yearCat[oldGraduateDf$phd-years >=5 ] = ">5"
  #   
  #   fig <- plot_ly(oldGraduateDf, labels = ~yearCat, type = 'pie')
  #   fig <- fig %>% layout(title = 'Degree Years of Non-first Time GEMEE')
  #   
  #   fig
  # })
  
  output$NumGEM<-renderPlotly({
    fig <- plot_ly(graduateDf, labels = ~`num-workshop`, type = 'pie')
    fig <- fig %>% layout(title = 'Number of GEM Attendances ')

    fig
  })
  output$NumAdvisor<-renderPlotly({
    fig <- plot_ly(advisorDf(), labels = ~`count`, type = 'pie')
    fig <- fig %>% layout(title = 'Number of GEM Student per Advisor')
    
    fig
  })
})
