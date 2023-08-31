#assingment2 
csv_files <- list.files(path = "Data",pattern = ".csv")
csv_files
length(csv_files)
length_CSV <- length(csv_files)
list.files(path = "Data",pattern = ".csv")
#wingspan_vs Mass
df <- read.csv("Data/wingspan_vs_mass.csv")
head(df,n = 5)
list.files(path = "./Data", pattern = "^b", recursive = TRUE)

#"BioLog_Plate_Data.csv"
df_biolog_plate <- read.csv("Data/BioLog_Plate_Data.csv")
head(df_biolog_plate,n=5)
