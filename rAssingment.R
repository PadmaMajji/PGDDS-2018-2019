 
#	  Import the Bollywood data set in Rstudio in a variable named bollywood

  bollywood <- read.csv("rdataset.csv")
  View(bollywood)

#	  When you import a data set, R stores character vectors as factors (by default)
# You can check the structure of the data frame by using str()
  
  str(bollywood)

# You can change the attribute 'Movie' from factor to character type using the given command
  
  bollywood$Movie <- as.character(bollywood$Movie)

#Q1.
#	  Access the last 10 movies (from the bottom of the Bollywood data frame) using column bollywood$Movie
  
  tail(bollywood$Movie, 10)

# Store the names of those movies in last_10 vector (in the same order)
     
	last_10 <- tail(bollywood$Movie,10)
	
	#Output for last_10 is "Neerja"  "Fitoor"  "Sanam Re"  "Sanam Teri Kasam" "Ghayal Once Again"  "Saala Khadoos"  "Mastizaade"  "Airlift"  "Kyaa Kool Hain Hum 3" "Wazir" 	
	
	  
#Q2.
#	Find out the total number of  missing values (NA) in the bollywood data frame.
	
	#one way of getting only count of NA in data frame
	
	sum(is.na(bollywood))
	
	#Another way of getting count of NA and count of Non-NA values
	
	table(is.na(bollywood))
	
	
	# Store the result in na_bollywood vector
	
	na_bollywood <- sum(is.na(bollywood))
	
	#Output is 3
	
	na_bollywood <- table(is.na(bollywood))
	
	#output is FALSE  TRUE 
	#	          485     3 
	
	
#Q3
#	  Write the command to find out which movie tops the list in terms of Total Collections
	
# This command is to display the movie name
	
	bollywood$Movie[which.max(bollywood$Tcollection)]
	
	#Output is [1] "Dangal"
	
# This command is to display the complete row
	
	bollywood[which.max(bollywood$Tcollection),]
	
	#Output is  Movie  Lead     Rdate Ocollection Wcollection Fwcollection Tcollection   Verdict
	#          1 Dangal Aamir 23-Dec-16       29.78      106.95       197.53      386.68 Super Hit
	
#   Store the movie name in variable named top_movie
# As per above comment storing only Movie name in top_movie instead of complete row
 
  top_movie <-bollywood$Movie[which.max(bollywood$Tcollection)]
  
  #Output is [1] "Dangal"
  
#Q4
#	  Write the command to find out which movie comes second on the list in terms of Total Collections

  bollywood$Movie[order(bollywood$Tcollection, decreasing = TRUE)][2]
  
# Store the movie name in variable named top_2_movie

  top_2_movie <- bollywood$Movie[order(bollywood$Tcollection, decreasing = TRUE)][2]
  
  #Output is [1] "Sultan" 
	  
	
#   Now let's find out the movies shot by Shahrukh, Akshay and Amitabh separately.
  
# subset() function is used for that. The code has already been written for you. 
	
	shahrukh <- subset(bollywood, Lead == "Shahrukh")
	akshay <- subset(bollywood, Lead == "Akshay")
	amitabh <- subset(bollywood, Lead  == "Amitabh")

# You can view what the above data frames look like
	
  View(shahrukh)
  View(akshay)
  View(amitabh)
		   
#Q5
#	  What is the total collection of Shahrukh, Akshay and Amitabh movies individually?
# You can use	a column named 'Tcollection' for this 
 
  shahrukh_collection <- sum(shahrukh$Tcollection)
  
  # shahrukh_collection Output is [1] 148.54
  
	akshay_collection <- sum(akshay$Tcollection)
	
	# akshay_collection output is [1] 364.27
	
	amitabh_collection <- sum(amitabh$Tcollection)
	
  #	amitabh_collection Outout is [1] 121.91
    
	
#Q6  
#   Write command/s to find out how many movies are in Flop, Average, Hit and Superhit categories in the entire Bollywood data set.
  
	Verdict_category <- c("Flop","Average","Hit","Super Hit")
  Verdict_Category_Count<-c(length(which(bollywood$Verdict == "Flop")),length(which(bollywood$Verdict=="Average")),length(which(bollywood$Verdict=="Hit")),length(which(bollywood$Verdict=="Super Hit")))
 
  Movie_verdict_Count<-data.frame(Verdict_category,Verdict_Category_Count)
  
  # Movie_verdict_Count Output 
  # Verdict_category Verdict_Category_Count
  # 1             Flop                     30
  # 2          Average                     17
  # 3              Hit                      7
  # 4        Super Hit                      7
  
#   You can use SAPPLY function if you want to apply a function specific columns in a data frame 
# You can write a command to find the maximum value of Ocollection, Wcollection, Fwcollecion and Tcollection using sapply

  sapply(data.frame(max(bollywood$Ocollection,na.rm = T),max(bollywood$Wcollection,na.rm = T),max(bollywood$Fwcollection, na.rm = T),max(bollywood$Tcollection,na.rm = T)),max, simplify = TRUE)
  
  # Output of sapply 
  #max.bollywood.Ocollection..na.rm...T.  max.bollywood.Wcollection..na.rm...T. max.bollywood.Fwcollection..na.rm...T.  max.bollywood.Tcollection..na.rm...T. 
  #       36.54                                 180.36                                 229.16                                 386.68 
  
  #Another way of using sapply by storing the maximum collection value in separate variables and perform sapply.
  
  MaxOpeningCollection<-max(bollywood$Ocollection,na.rm = T)
  MaxWeekendCollection<-max(bollywood$Wcollection,na.rm = T)
  MaxFirstWeekCollection<-max(bollywood$Fwcollection, na.rm = T)
  MaxTotalCollection<-max(bollywood$Tcollection,na.rm = T)
  
  sapply(data.frame(MaxOpeningCollection,MaxWeekendCollection,MaxFirstWeekCollection,MaxTotalCollection),max, simplify = TRUE)
  
  #Output of sapply
  # MaxOpeningCollection   MaxWeekendCollection MaxFirstWeekCollection     MaxTotalCollection 
  #       36.54                 180.36                 229.16                 386.68 
  
  
#Q7 
#   Write a command to find the names of the movies which have the maximum Ocollection, Wcollection, Fwcollecion & Tcollection

    c(bollywood$Movie[which.max(bollywood$Ocollection)],bollywood$Movie[which.max(bollywood$Wcollection)],bollywood$Movie[which.max(bollywood$Fwcollection)],bollywood$Movie[which.max(bollywood$Tcollection)])
    
# Store the names of 4 movies in same sequence in movie_result vector

  movie_result <- c(bollywood$Movie[which.max(bollywood$Ocollection)],bollywood$Movie[which.max(bollywood$Wcollection)],bollywood$Movie[which.max(bollywood$Fwcollection)],bollywood$Movie[which.max(bollywood$Tcollection)])

  # Output is [1] "Sultan" "Sultan" "Sultan" "Dangal"