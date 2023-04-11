source('configure.R')
source('functions.R')

gs4_deauth()

# df2022 <- clean_student_df2022(googlesheets4::read_sheet(googleSheetId2022, sheet = "studinfo_GEM_20220410"))
list[graduateDf, undergraduateDf, postdocDf] <- clean_submission_2023(googlesheets4::read_sheet(googleSheetId2023, sheet = "submission"))
# df2023_recommendation <- googlesheets4::read_sheet(googleSheetId2023, sheet = "recommendation")

recommendation_2023 <- clean_recommendation_2023(googlesheets4::read_sheet(googleSheetId2023, sheet = "recommendation"))
tutorial_2023 <- clean_tutorial_2023(googlesheets4::read_sheet(googleSheetId2023, sheet = "tutorial"))
rep_2023 <- clean_rep_2023(googlesheets4::read_sheet(googleSheetId2023, sheet = "student rep"))

graduateDf <- merge(merge(merge(graduateDf, recommendation_2023, by = c('student-email','advisor-email'), all.x = TRUE), tutorial_2023, by = 'student-email' , all.x = TRUE), rep_2023, by = 'student-email' , all.x = TRUE) 

undergraduateDf <- merge(merge(merge(undergraduateDf, recommendation_2023, by = c('student-email','advisor-email'), all.x = TRUE), tutorial_2023, by = 'student-email' , all.x = TRUE), rep_2023, by = 'student-email' , all.x = TRUE) 

postdocDf <- merge(merge(merge(postdocDf, recommendation_2023, by = c('student-email','advisor-email'), all.x = TRUE), tutorial_2023, by = 'student-email' , all.x = TRUE), rep_2023, by = 'student-email' , all.x = TRUE)

graduateDf <- addRandomNumber(graduateDf)
undergraduateDf <- addRandomNumber(undergraduateDf)
postdocDf <- addRandomNumber(postdocDf)

combinedDF <- rbind(graduateDf, undergraduateDf,postdocDf)

missingApplication <- merge(combinedDF, recommendation_2023, by = c('student-email','advisor-email'), all.y = TRUE)
missingApplication <- missingApplication[is.na(missingApplication$`student-name`),]

missingRecommendation <- combinedDF[is.na(combinedDF$student_name_recommendation) & combinedDF$`student-degree-program` != "Post Doc or Early Career (PhD + 3 yrs)",]

# sheet_write(combinedDF[is.na(combinedDF[combinedDF$student_name_recommendation,]),], ss = googleSheetId2023, sheet = "")


rm(recommendation_2023, tutorial_2023, rep_2023 )


