#################################
## Tabita's Practical S. 28.06 ##
#################################

library(fst)
library(dplyr)

#I. Read the files with the Fst estimates (AFR_EUR.weir.fst, 
#                                       AFR_EAS.weir.fst and EAS_EUR.weir.fst)

AFR_EAS <- read.table("C:/Users/Erica/Desktop/EMBO2025/Tabita_Hunemeier/EMBO_Practical_Course_2024-main/AFR_EAS.weir.fst", header = TRUE)
AFR_EUR <- read.table("C:/Users/Erica/Desktop/EMBO2025/Tabita_Hunemeier/EMBO_Practical_Course_2024-main/AFR_EUR.weir.fst", header = TRUE)
EAS_EUR <- read.table("C:/Users/Erica/Desktop/EMBO2025/Tabita_Hunemeier/EMBO_Practical_Course_2024-main/EAS_EUR.weir.fst", header = TRUE)

#II. Remove duplicated positions

for (file in fst_files) {
  # Read the file
  df <- read_fst(file)
  
  # Remove duplicates based on specific column (e.g., "id_column")
  df_unique <- df %>% 
    distinct(id_column, .keep_all = TRUE)
  
  # Write back to the same file (or create new file)
  write_fst(df_unique, file)
  # Or save with new name: write_fst(df_unique, paste0("unique_", basename(file)))
}






  # check if duplicates were removed by row number
nrow(AFR_EAS)
nrow(AFR_EAS2)






# PBS formula as a function (Adrian)
calculate_PBS <- function(FST_AB, FST_AC, FST_BC) {
  PBS <- ((-log(1 - FST_AB) + (-log(1 - FST_AC))) - (-log(1 - FST_BC))) / 2
  return(PBS)
}
#1. Perform PBS test, using EAS as candidate population for selection
#Build the PBS Topology and why is it important to measure the distance
# A is outgroup, B focal, C is sister group
#Focal is  EAS, B
# Sister in europe, C
# Africa is outgroup. A
combined_fst$PBS <- calculate_PBS(combined_fst$FST_AFR_EAS,
                                  combined_fst$FST_AFR_EUR,
                                  combined_fst$FST_EAS_EUR)






