library('ggplot2')
library('gganimate')
library('gifski')
library('prismatic')
library('stringr')
library('scales')

options(warn=-1)

setwd('C:/Users/Haroon Atcha/Desktop/Book Club')

data <- read.csv('book_club_1_voting.csv')

# Functions ---------------------------------------------------------------

#I need to get the data into a more usable format first
rank_choice_data <- function(data) {
  #create an 'options' vector
  books <- colnames(data)
  
  #temporary data frame to hold new format
  temp <- data.frame(matrix(nrow = 0,
                            ncol = ncol(data)))
  
  #columns need to be converted to rank, not names
  colnames(temp) <- paste('rank', 1:ncol(data))
  
  #for each voter
  for(i in 1:nrow(data)) {
    #order their book preferences and omit non-voted options
    preference <- books[order(data[i,], na.last = NA)]
    
    #pad options with NA until we get N options
    preference <- c(preference, rep(NA, ncol(data) - length(preference)))
    
    #append voter's preference to dataset
    temp[i, ] <- preference
  }
  
  #return better formatted dataset
  return(temp)
}

#function to return the winner
rank_choice_results <- function(data) {
  
  #Check to see what % of the vote the most popular book got in round 1
  highest_vote_percent <- max(table(data[,1])) / nrow(data)
  
  #if that percent isn't greater than 0.5...
  while(highest_vote_percent < 0.5) {
    
    #find out which voters voted for the least popular item in round 1
    rows_to_update <- which(data[,1] == names(which.min(table(data[,1]))))
    
    #remove their first preference and move up their second preference. Pad tails with 0's
    data[rows_to_update,] <- cbind(data[rows_to_update,2:ncol(data)], NA)
    
    #recalculate what # of the vote the highest vote getter got this time
    highest_vote_percent <- max(table(data[,1])) / nrow(data)
  }
  
  #check for a tie
  if(length(which.max(table(data[,1]))) > 1) {
    
    #If a tie, report both winners
    print(paste0('Tie between:', names(which(table(data[,1]) == max(table(data[,1]))))))
  } else {
    #else print single winner
    print(paste0('Winner: ', names(which.max(table(data[,1])))))
    
  }
}

plot_rounds <- function(data) {
  highest_vote_percent <- max(table(data[,1])) /nrow(data)
  
  i <- 1
  
  complete <- data.frame(matrix(nrow = 0,
                                ncol = 4))
  
  colnames(complete) <- c('Option', 'Votes', 'Round', 'Rank')
  
  while(highest_vote_percent < 0.5) {
    
    rounds <- data.frame(table(data[,1]))
    
    rounds$Round <- i
    
    rounds <- rounds[order(rounds$Freq, decreasing = TRUE),]

    rounds$Rank <- seq(from = nrow(rounds),
                       to = 1,
                       by = -1)
    
    colnames(rounds) <- colnames(complete)
    
    complete <- rbind(complete, rounds)
    
    rows_to_update <- which(data[,1] == names(which.min(table(data[,1]))))
    
    #remove their first preference and move up their second preference. Pad tails with 0's
    data[rows_to_update,] <- cbind(data[rows_to_update,2:ncol(data)], NA)
    
    #recalculate what # of the vote the highest vote getter got this time
    highest_vote_percent <- max(table(data[,1])) / nrow(data)
    
    i <- i + 1
    
  }
  
  if(highest_vote_percent > 0.5) {
    rounds <- data.frame(table(data[,1]))
    
    rounds$Round <- i
    
    rounds <- rounds[order(rounds$Freq, decreasing = TRUE),]
    
    rounds$Rank <- seq(from = nrow(rounds),
                       to = 1,
                       by = -1)
    
    colnames(rounds) <- colnames(complete)
    
    complete <- rbind(complete, rounds)
  } else {
    next
  }
  
  complete$Option <- paste0(str_wrap(gsub('\\.', ' ', complete$Option), 15), '\n')
  
  g <- ggplot(data = complete) +
    geom_vline(xintercept = nrow(complete) / 2,
               color = 'grey70',
               linetype = 'dashed') +
    geom_rect(aes(xmin = 0, xmax = Votes,
                  ymin = Rank - 0.45, ymax = Rank + 0.45,
                  group = Option, fill = Option,
                  color = after_scale(clr_darken(fill, 0.3)))) +
    geom_text(aes(x = 0, y = Rank,
                  label = Option),
              hjust = 0,
              family = 'serif') +
    scale_x_continuous(expand = c(0,0),
                       limits = c(0, nrow(complete)),
                       breaks = seq(from = 0,
                                    to = nrow(complete),
                                    length.out = 5),
                       labels = percent(seq(from = 0,
                                            to = 1,
                                            by = 0.25))) +
    labs(title = 'Round {as.integer(frame_time)}',
         x = 'Votes',
         y = 'Rank') +
    transition_time(Round) +
    theme(legend.position = 'top',
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5,
                                    family = 'serif'),
          axis.line.y = element_line(color = 'black'),
          axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          panel.background = element_rect(fill = NA),
          panel.grid = element_blank())
  
  return(animate(g, renderer = gifski_renderer(), rewind = FALSE,
                 nframes = max(complete$Round)*30 + 45, fps = 60,
                 end_pause = 45, start_pause = 15))
}

# Applying the functions --------------------------------------------------

#apply the functions to our actual vote
data <- rank_choice_data(data)

#looks like it worked
rank_choice_results(data)

#test the plotting function
plot_rounds(data)

# Test Case ---------------------------------------------------------------

#Initialize a dataset with 100 voters and 5 options
test <- data.frame(matrix(ncol = 5,
                          nrow = 100))

#for each voter...
for(i in 1:nrow(test)) {
  
  #generate the number of preferences they have
  options <- sample(1:5, 1, replace = TRUE)
  
  #get the numbers 1:preference and pad with NA's
  options <- c((1:5)[1:options], rep(NA, 5 - options))
  
  #scramble those preferences across options
  options <- options[sample(1:5, 5, replace = FALSE)]
  
  #append to dataset
  test[i,] <- options
}

write.csv(test, 'test.csv')

#try out the data munging function again
test <- rank_choice_data(test)

#and the results part again
rank_choice_results(test)

