# What is a pirate's favorite letter?

# Create and set the variable named favorite_letter to R
favorite_letter <- "R"

# Display the variable named favorite_letter
favorite_letter

# Create two numeric variables
pirates_polled <- 102
pirates_chose_R <- 28

# Calculate and display the percentage of pirates chose R
percentage <- pirates_chose_R / pirates_polled         
percentage <- percentage * 100 
percentage

# Calculate the percentage as a whole number
percentage <- (pirates_chose_R * 100) %/% pirates_polled 

# Display a message followed by the percentage
result_message <- "Percentage of pirates whose favorite letter is R:"
result_message
percentage

# Create new variables for pirates who prefer C
new_favorite_letter <- "C"
pirates_chose_C = 70

# Calculate the new percentage as a whole number in one statement
percentage = (pirates_chose_C * 100) %/% pirates_polled 

# Display a new message followed by the percentage
result_message = "Percentage of pirates whose favorite letter is C:"
result_message
percentage

conclusion <- "In conclusion, many pirates like R, but their overwhelming first love is the C."
conclusion

NeitherCnorR <- 100 - pirates_chose_C - pirates_chose_R
NeitherCnorR
