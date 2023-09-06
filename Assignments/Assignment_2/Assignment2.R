#assingment2 
csv_files <- list.files(path = "Data",pattern = ".csv")

length(csv_files)
length_CSV <- length(csv_files)

list.files(path = "Data",pattern = ".csv")
#wingspan_vs Mass
df <- read.csv("Data/wingspan_vs_mass.csv")
head(df,n = 5)
bfiles<- list.files(path = "./Data", pattern = "^b", recursive = TRUE , full.names = TRUE)
x``
readLines("./Data/data-shell/creatures/basilisk.dat", n=1)
readLines("./Data/data-shell/data/pdb/benzaldehyde.pdb",n=1)
readLines("./Data/Messy_Take2/b_df.csv", n=1)

bfiles
for(i in bfiles){
  print(readLines(i,n=1))
}

x <- 1:10
for(i in x ){
    print(i*2)
}

csv_files
for(i in csv_files){
  print(readLines(i,n=1))
}

for(i in list.files(path = "Data", full.names = TRUE, 
                    recursive = TRUE, 
                    pattern = ".csv"))
  print(readLines(i,n=1))
for (in csv_files){
  print(readLines(i,n=1))
}


  




