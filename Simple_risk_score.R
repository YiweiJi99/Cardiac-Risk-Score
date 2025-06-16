### this file contains the calculation of point-based Framingham Score 
### Framingham simple score is stratified by sex
### Created by Yiwei, 9th June 2024

# sex: 1 = male, 0 = female

# score age by sex
score_age <- function(Age, Sex) {
  # Sex: 1 = male, 0 = female
  
  if (Sex == 1) {  # male
    if (Age >= 30 & Age <= 34) {
      return(0)
    } else if (Age >= 35 & Age <= 39) {
      return(2)
    } else if (Age >= 40 & Age <= 44) {
      return(5)
    } else if (Age >= 45 & Age <= 49) {
      return(6)
    } else if (Age >= 50 & Age <= 54) {
      return(9)
    } else if (Age >= 55 & Age <= 59) {
      return(10)
    } else if (Age >= 60 & Age <= 64) {
      return(11)
    } else if (Age >= 65 & Age <= 69) {
      return(12)
    } else if (Age >= 70 & Age <= 74) {
      return(14)
    } else if (Age >= 75) {
      return(15)
    } else {
      return(NA)
    }
  } else if (Sex == 0) {  # female
    if (Age >= 30 & Age <= 34) {
      return(0)
    } else if (Age >= 35 & Age <= 39) {
      return(2)
    } else if (Age >= 40 & Age <= 44) {
      return(4)
    } else if (Age >= 45 & Age <= 49) {
      return(5)
    } else if (Age >= 50 & Age <= 54) {
      return(7)
    } else if (Age >= 55 & Age <= 59) {
      return(8)
    } else if (Age >= 60 & Age <= 64) {
      return(9)
    } else if (Age >= 65 & Age <= 69) {
      return(10)
    } else if (Age >= 70 & Age <= 74) {
      return(11)
    } else if (Age >= 75) {
      return(12)
    } else {
      return(NA)
    }
  } else {
    return(NA)
  }
}

new_table_noNA$score_age <- mapply(score_age, new_table_noNA$Age, new_table_noNA$Sex)


# note: HDL unit is converted here


# score HDL by sex
score_HDL <- function(HDL_cholesterol, Sex) {
  if (Sex == 1) { 
    if (HDL_cholesterol >= 60 * 0.0259) {
      return(-2)
    } else if (HDL_cholesterol >= 50 * 0.0259 & HDL_cholesterol < 60 * 0.0259) {
      return(-1)
    } else if (HDL_cholesterol >= 45 * 0.0259 & HDL_cholesterol < 50 * 0.0259) {
      return(0)
    } else if (HDL_cholesterol >= 35 * 0.0259 & HDL_cholesterol < 45 * 0.0259) {
      return(1)
    } else if (HDL_cholesterol < 35 * 0.0259) {
      return(2)
    } else {
      return(NA)
    }
  } else if (Sex == 0) {  # female
    if (HDL_cholesterol >= 60 * 0.0259) {
      return(-2)
    } else if (HDL_cholesterol >= 50 * 0.0259 & HDL_cholesterol < 60 * 0.0259) {
      return(-1)
    } else if (HDL_cholesterol >= 45 * 0.0259 & HDL_cholesterol < 50 * 0.0259) {
      return(0)
    } else if (HDL_cholesterol >= 35 * 0.0259 & HDL_cholesterol < 45 * 0.0259) {
      return(1)
    } else if (HDL_cholesterol < 35 * 0.0259) {
      return(2)
    } else {
      return(NA)
    }
  } else {
    return(NA)
  }
}

new_table_noNA$score_HDL <- mapply(score_HDL, new_table_noNA$HDL_cholesterol, new_table_noNA$Sex)


# score Total cholesterol by sex
score_cholesterol <- function(Cholesterol, Sex) {
  # unit is changed 
  if (Sex == 1) { 
    if (Cholesterol >= 280 * 0.0259) {
      return(4)
    } else if (Cholesterol >= 240 * 0.0259 & Cholesterol < 280 * 0.0259) {
      return(3)
    } else if (Cholesterol >= 200 * 0.0259 & Cholesterol < 240 * 0.0259) {
      return(2)
    } else if (Cholesterol >= 160 * 0.0259 & Cholesterol < 200 * 0.0259) {
      return(1)
    } else if (Cholesterol < 160 * 0.0259) {
      return(0)
    } else {
      return(NA)
    }
  } else if (Sex == 0) {  # female
    if (Cholesterol >= 280 * 0.0259) {
      return(5)
    } else if (Cholesterol >= 240 * 0.0259 & Cholesterol < 280 * 0.0259) {
      return(4)
    } else if (Cholesterol >= 200 * 0.0259 & Cholesterol < 240 * 0.0259) {
      return(3)
    } else if (Cholesterol >= 160 * 0.0259 & Cholesterol < 200 * 0.0259) {
      return(1)
    } else if (Cholesterol < 160 * 0.0259) {
      return(0)
    } else {
      return(NA)
    }
  }
}


new_table_noNA$score_cholesterol <- mapply(score_cholesterol, new_table_noNA$Cholesterol, new_table_noNA$Sex)



# score SBP_untreated by sex
score_sbp_untreated <- function(Sbp, Sex, Antihypertensive_treatment) {
  if (Antihypertensive_treatment != 0) {
    return(NA)  # 只对未接受治疗的打分
  }
  
  if (Sex == 1) {  # male
    if (Sbp < 120) {
      return(-2)
    } else if (Sbp >= 120 & Sbp < 130) {
      return(0)
    } else if (Sbp >= 130 & Sbp < 140) {
      return(1)
    } else if (Sbp >= 140 & Sbp < 160) {
      return(2)
    } else if (Sbp >= 160) {
      return(3)
    } else {
      return(NA)
    }
  } else if (Sex == 0) {  # female
    if (Sbp < 120) {
      return(-3)
    } else if (Sbp >= 120 & Sbp < 130) {
      return(0)
    } else if (Sbp >= 130 & Sbp < 140) {
      return(1)
    } else if (Sbp >= 140 & Sbp < 150) {
      return(2)
    } else if (Sbp >= 150 & Sbp < 160) {
      return(4)
    } else if (Sbp >= 160) {
      return(5)
    } else {
      return(NA)
    }
  } else {
    return(NA)
  }
}

new_table_noNA$score_sbp_untreated <- mapply(score_sbp_untreated, new_table_noNA$Sbp, new_table_noNA$Sex, new_table_noNA$Antihypertensive_treatment)


# score SBP_treated by sex
score_sbp_treated <- function(Sbp, Sex, Antihypertensive_treatment) {
  if (Antihypertensive_treatment != 1) {
    return(NA)  # 只对未接受治疗的打分
  }
  
  if (Sex == 1) {  # male
    if (Sbp < 120) {
      return(0)
    } else if (Sbp >= 120 & Sbp < 130) {
      return(2)
    } else if (Sbp >= 130 & Sbp < 140) {
      return(3)
    } else if (Sbp >= 140 & Sbp < 160) {
      return(4)
    } else if (Sbp >= 160) {
      return(5)
    } else {
      return(NA)
    }
  } else if (Sex == 0) {  # female
    if (Sbp < 120) {
      return(-1)
    } else if (Sbp >= 120 & Sbp < 130) {
      return(2)
    } else if (Sbp >= 130 & Sbp < 140) {
      return(3)
    } else if (Sbp >= 140 & Sbp < 150) {
      return(5)
    } else if (Sbp >= 150 & Sbp < 160) {
      return(6)
    } else if (Sbp >= 160) {
      return(7)
    } else {
      return(NA)
    }
  } else {
    return(NA)
  }
}

new_table_noNA$score_sbp_treated <- mapply(score_sbp_treated, new_table_noNA$Sbp, new_table_noNA$Sex, new_table_noNA$Antihypertensive_treatment)


# score smoker
score_smoking <- function(Smoking_status, Sex) {
  if (Sex == 1) {
    if (Smoking_status == 1) {
      return(4)
    } else {
      return(0)
    }
  } else if (Sex == 0) {
    if (Smoking_status == 1) {
      return(3)
    } else {
      return(0)
    }
  } else {
    return(NA)
  }
}

new_table_noNA$score_smoking <- mapply(score_smoking, new_table_noNA$Smoking_status, new_table_noNA$Sex)


# score diabetes
score_diabetes <- function(Diabetes_status, Sex) {
  if (Sex == 1) {
    if (Diabetes_status == 1) {
      return(3)
    } else {
      return(0)
    }
  } else if (Sex == 0) {
    if (Diabetes_status == 1) {
      return(4)
    } else {
      return(0)
    }
  } else {
    return(NA)
  }
}

new_table_noNA$score_diabetes <- mapply(score_diabetes, new_table_noNA$Diabetes_status, new_table_noNA$Sex)



### Sum up the scores and get the final score
score_columns <- c("score_age", "score_cholesterol", "score_HDL", "score_sbp_untreated", "score_sbp_treated", "score_smoking", "score_diabetes")
new_table_noNA$total_score <- rowSums(new_table_noNA[, score_columns], na.rm = TRUE)

# merge data with Heart_age_gap - "df_aging$gap_xgb1_c.1"
Framingham_Simple_Risk_Scores <- merge(new_table_noNA[, c("Participant_id", "total_score")], df_aging[, c("eid", "gap_xgb1_c.1")], by.x = "Participant_id", by.y = "eid")

# round by 1 decimal
Framingham_Simple_Risk_Scores$gap_xgb1_c.1 <- round(Framingham_Simple_Risk_Scores$gap_xgb1_c.1, 1)

# export csv file
write.csv(Framingham_Simple_Risk_Scores, "Framingham_Scores_with_HeartAge.csv", row.names = FALSE)



### exploration of relationship
cor.test(Framingham_Simple_Risk_Scores$total_score, Framingham_Simple_Risk_Scores$gap_xgb1_c.1)

