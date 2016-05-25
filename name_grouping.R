#The objective of this program is to find potential redundance in a large survey data set, 
#by matching people with the same job and location and with similar names and ages. 
##The key is to group the population into small groups, as small as possible, so that
##the matrix-based functions can run efficently on each submatrix.

master <-read.csv("master.csv", header=TRUE)

library(dplyr)

contact<-select(master, 
  name=name,
  age=age,
  job=job.cond,
  location=location,
  sex=sex) #sex is used to minimize the sizes of submatrices

#head(contact)
##write.csv(contact, "contact.csv")

##Define the function that removes whitespaces at the beginning or the end of names
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
##This line creates the function that splits the names in the name column and creates a list of only first names
get_firstname <-function(s) strsplit(s, "\\s+")[[1]][1]
get_secondname <-function(s) strsplit(s, "\\s+")[[1]][2]
get_firstletter<-function(s) strsplit(s, "")[[1]][1]

##This line applies the function "get_firstname" to the column "name" in dataframe "contact" and first makes it a column of characters
##The outcome is a vector of first names only.
contact<-mutate(contact,
  name=as.vector(sapply(as.character(name), trim)),
  firstname=as.vector(sapply(as.character(name), get_firstname)),
  secondname=as.vector(sapply(as.character(name), get_secondname)),
  firstletter=as.vector(sapply(as.character(firstname), get_firstletter)))

##change some first names. Add more if needed
contact$firstname[contact$firstname=='Ab']<-'Abdul'
contact$firstname[contact$firstname=='Md']<-'Mohammad'

##define the function to make ids
make_id <- function(firstname, secondname, name, age, job, location) {
  prefix <- paste(firstname, secondname, age, job, location, sep="-") ##This line sets the structure of the ID by pasting name, etc sep by "-"
  #compute the distance matrices
  dist.mat.name1<-adist(firstname)
  dist.mat.name2<-adist(secondname)
  dist.mat.age<-abs(outer(age, age, '-'))
  #derive the similarity matrices (entries are "TRUE" or "FALSE"). You can tweak the parameters
  sim1<-dist.mat.name1<2
  sim2<-dist.mat.name2<2|is.na(dist.mat.name2) #na is matched with any name
  sim3<-dist.mat.age<5
  #combine the three similarity matrices into a lower triangular matrix
  sim<-sim1&sim2&sim3&lower.tri(sim1)
  #iterate to replace ids for similar people
  for (i in 1:length(name)) {
    prefix<-ifelse(sim[i,], prefix[i], prefix)
  } # for loop
  return(prefix)
}

foo <- contact %>% 
  group_by(job, location, sex, firstletter) %>%
  mutate(
    id=make_id(firstname, secondname, name, age, job, location)
    #group_count = n(),
    #num_ids=length(unique(id))
  )

write.csv(foo, "master_matched_new.csv")