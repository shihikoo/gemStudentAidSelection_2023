library(googlesheets4)
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(data.table)
library(plotly)
library(shinythemes)
library(tidyr)
library(stats)
library(gsubfn)

clean_student_df2022 <- function(df2022){
  df2022 <- df2022[!is.na(df2022$Degree),] 
  df2022[is.na(df2022)] = ''
  df2022[is.null(df2022)] = ''
  df2022[df2022 == 'null'] = ''
  
  
  df2022$Name = paste(df2022$F_Name, df2022$Mi_Name, df2022$Fa_Name)
  
  df2022$Gender <- tolower(df2022$Gender)
  return(df2022)
  }

clean_submission_2023 <- function(df2023_submission){
  df2023_submission <- df2023_submission[df2023_submission$status != "delete",]
  df2023_submission$duplicate <- duplicated(df2023_submission$`student-email`)
  if (sum(df2023_submission$duplicate) > 0) {
    print("Duplicated entry found.")
    print(df2023_submission[df2023_submission$duplicate, ])
  }
  df2023_submission$`student-email` <- tolower(df2023_submission$`student-email`)
  
  # df2023_submission <- df2023_submission[df2023_submission$`student-email` != "efusina@css.edu", ]
  
  # print(colnames(df2023_submission))
  graduateDf <- df2023_submission[(df2023_submission$`student-degree-program` == "PhD" | df2023_submission$`student-degree-program` == "Master's"), ] 
  
  undergraduateDf <- df2023_submission[(df2023_submission$`student-degree-program` == "Undergraduate"), ] 
  
  postdocDf <- df2023_submission[(df2023_submission$`student-degree-program` == "Post Doc or Early Career (PhD + 3 yrs)"), ] 
  
  output_columns_names <- c("student-name","student-email"  , "student-affiliation","student-pronouns","student-degree-program","phd-years","num-workshop","advisor-name", "advisor-email", "student-pref-roommate" , "student-accommodations")
  
  return(list(graduateDf[,output_columns_names], undergraduateDf[,output_columns_names], postdocDf[,output_columns_names]) )
}

clean_recommendation_2023 <- function(df2023_recommendation){
  df2023_recommendation[is.na(df2023_recommendation$status),'status'] <- "Current"
  df2023_recommendation <- df2023_recommendation[df2023_recommendation$status != "delete",]
  df2023_recommendation <- df2023_recommendation[!is.na(df2023_recommendation$`student-email`),]
  
  df2023_recommendation$duplicate <- duplicated(df2023_recommendation$`student-email`)
  if (sum(df2023_recommendation$duplicate) > 0) {
    print("Duplicated entry found.")
    print(df2023_recommendation[df2023_recommendation$duplicate, ])
    df2023_recommendation <- df2023_recommendation[!df2023_recommendation$duplicate,]
    } 
  df2023_recommendation$student_name_recommendation <- df2023_recommendation$`student-name`
  df2023_recommendation$advisor_name_recommendation <- df2023_recommendation$`advisor-name`
  df2023_recommendation$`student-email` <- tolower(df2023_recommendation$`student-email`)
  
  # df2023_recommendation <- df2023_recommendation[df2023_recommendation$`student-email` != "efusina@css.edu", ]
  
  return(df2023_recommendation[(df2023_recommendation$status != "delete" | is.na(df2023_recommendation$status)) & df2023_recommendation$recommend == 'Yes' 
                               # & df2023_recommendation$`aware-fee` == 'Yes'
                               , c('student-email','advisor-email','student_name_recommendation','advisor_name_recommendation','inNeed')])
}

clean_tutorial_2023 <- function(df2023_tutorial){
  
  df2023_tutorial[,c("student-email","tutorial")]
}

clean_rep_2023 <- function(df2023_rep){
  
  df2023_rep[,c("student-email","rep")]
}

selectionRun <- function(x) {
  return(runif(1,min=0.5*(x==0),max=1)) 
}

addRandomNumber <- function(graduateDf){
  graduateDf$randomNumberGenerated <- sapply(graduateDf$`phd-years`, selectionRun)
  
  graduateDf$randomNumberGenerated[graduateDf$tutorial == "Yes"] = 1
  graduateDf$randomNumberGenerated[graduateDf$rep == "Yes"] = 1
  graduateDf$randomNumberGenerated[graduateDf$inNeed == "Yes"] = 1
  
  graduateDf <- graduateDf[order(graduateDf$randomNumberGenerated, decreasing = TRUE),]
  return(graduateDf)
}


