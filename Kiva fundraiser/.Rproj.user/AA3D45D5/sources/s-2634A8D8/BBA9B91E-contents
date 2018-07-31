################################################
############# Chapter 5 ########################
############ Case Study ##################


packages = c("ggplot2","stringr","babynames", "rebus")

package.check <- lapply(packages, FUN= function(pkg)  {
  if(!require(pkg, character.only = T)) {
    install.packages(pkg, dependencies = T)
    library(pkg, character.only = T)
  }
})

# catcidents has been pre-defined
head(catcidents)

# Construct pattern of DOG in boundaries
whole_dog_pattern <- whole_word("DOG")

# View matches to word "DOG"
str_view(catcidents, whole_dog_pattern, match=T)

# Transform catcidents to upper case
catcidents_upper <- str_to_upper(catcidents)

# View matches to word "DOG" again
str_view(catcidents_upper, whole_dog_pattern, match=T)

# Which strings match?
has_dog <- which(str_detect(catcidents_upper, whole_dog_pattern))


# Pull out matching strings in original 
catcidents[has_dog]

# View matches to "TRIP"
str_view(catcidents, "TRIP", match=T)



# Construct case insensitive pattern
trip_pattern <- regex("TRIP", ignore_case = T)

# View case insensitive matches to "TRIP"
str_view(catcidents, trip_pattern, match=T)

# Get subset of matches
trip <- str_subset(catcidents, trip_pattern)

# Extract matches
str_extract(trip, trip_pattern)
library(stringi)

# Get first five catcidents
cat5 <- catcidents[1:5]

# Take a look at original
writeLines(cat5)

# Transform to title case
writeLines(str_to_title(cat5))

# Transform to title case with stringi
writeLines(stri_trans_totitle(cat5))

# Transform to sentence case with stringi
writeLines(stri_trans_totitle(cat5, type = "sentence") )
