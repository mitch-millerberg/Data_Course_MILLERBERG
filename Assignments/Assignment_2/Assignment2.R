#assingment2 
# lists all of the .csv files found in the Data/ directory and stores that list in an object called “csv_files”
csv_files <- list.files(path = "./Data",pattern = ".csv")
csv_files
#how many files match that description using the length() function
length(csv_files)
#sanity check
list.files(path = "./Data",pattern = ".csv")

#wingspan_vs Mass
df <- read.csv("./Data/wingspan_vs_mass.csv")
#Inspect the first 5 lines of this data set using the head() function
head(df,n = 5)

#Find any files (recursively) in the Data/ directory that begin with the letter “b” (lowercase)
#sanity check
list.files(path = "./Data", pattern = "^b",
           recursive = TRUE ,
           full.names = TRUE)
  #saving the b files in the global environment 
bfiles<- list.files(path = "./Data", pattern = "^b",
                    recursive = TRUE ,
                    full.names = TRUE)

#forloop logic 1x1
readLines("./Data/data-shell/creatures/basilisk.dat", n=1)
readLines("./Data/data-shell/data/pdb/benzaldehyde.pdb",n=1)
readLines("./Data/Messy_Take2/b_df.csv", n=1)

#forloop
#Write a command that displays the first line of each of those “b” files
#1)variable = i (i can be any variable)
#2)sequence = bfiles
#3)Do something (print readLines for variable i , first row)

for(i in bfiles){
  print(readLines(i,n=1))
}

#Do the same thing for all files that end in “.csv”
#sanity check
list.files(path = "./Data", pattern = ".csv",
           recursive = TRUE ,
           full.names = TRUE)

#save the value as a sequence
csv <- list.files(path = "./Data", pattern = ".csv",
                    recursive = TRUE ,
                    full.names = TRUE)


#for loop for all the files that end in ".csv"
for( all_csv in csv){
  print(readLines(all_csv,n=1))
}


#other for loop examples 
x <- 1:10
for(i in x ){
    print(i*2)
}


MM <- c("clear","Few","Scattered", "Broken","Overcast","Obsured")
for(i in MM){
  print(paste0("The sky today will be  ",i))
}



