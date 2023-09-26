library("tidyverse")
library("RJSONIO")
library("ggforce")

##################################
# Get the data
setwd("Documents/murach_r/data")
getwd()
##################################

# read the JSON file
json_data <- fromJSON("shots.json")

# get the column names and rows
column_names <- json_data[["resultSets"]][[1]][["headers"]]
rows <- json_data[["resultSets"]][[1]][["rowSet"]]

# build the data frame
shots <- data.frame()
for(row in rows) {              # this might take a while
  shots <- rbind(shots, row)
}
names(shots) <- column_names

# convert data frame to tibble
shots <- as_tibble(shots)
shots

# save data
saveRDS(shots, file = "shots_ex_11-1.rds")

# restart from here if necessary
# shots <- readRDS("shots_ex_11-1.rds")

# drop columns
shots <- shots %>% 
  select(-GRID_TYPE, -GAME_ID, -GAME_EVENT_ID, -PLAYER_ID, -PLAYER_NAME, 
         -TEAM_ID, -TEAM_NAME, -SHOT_ATTEMPTED_FLAG)

names(shots) <- c("Period", "MinutesLeft", "SecondsLeft", 
                  "EventType", "ActionType", "ShotType",
                  "Zone", "ZoneArea", "ZoneRange", "Distance", "LocX", "LocY",
                  "ShotMade", "GameDate", "HomeTeam", "VisitingTeam")
shots



##################################
# Add a column for season
##################################

shots <- shots %>% mutate(
  GameDate = as.Date(GameDate, format = "%Y%m%d"),
  ShotMadeFlag = as.logical(ShotMade)) 

shots %>% select(GameDate, ShotMadeFlag)

# define a function that adds a column for the season
get_season <- function(row) {
  month <- as.integer(format(as.Date(row[["GameDate"]]), "%m"))
  year <- as.integer(format(as.Date(row[["GameDate"]]), "%Y"))
  season <- ""
  if(month > 6) {
    season <- str_c(year, "-", year+1)
  } 
  else {
    season <-str_c(year-1, "-", year)
  }
  return(season)
}

# apply the function to add the Season column
shots <- shots %>% 
  mutate(Season = apply(X = shots, MARGIN = 1, FUN = get_season))

shots %>% select(GameDate, Season)


##################################
# Add a column for total seconds
##################################

# combine minutes left with seconds left 
# subtract from 720 if periods 1-4, or 300 if periods 5-6
shots <- shots %>% 
  mutate(TotalSeconds = ifelse(Period < 5,
                               720 - ((MinutesLeft * 60) + SecondsLeft), 
                               300 - ((MinutesLeft * 60) + SecondsLeft)))

# add 12 minutes (720 seconds) for each elapsed regular period
# if it's the first period, don't add any time
shots <- shots %>% mutate(
  TotalSeconds = ifelse(Period > 1 & Period < 6, 
                        TotalSeconds + (720*(Period-1)), TotalSeconds))

# if it's the 6th period, add 720 seconds for periods 1-4 
# + 300 seconds for overtime period 5
shots <- shots %>% mutate(
  TotalSeconds = ifelse(Period == 6, 
                        TotalSeconds + (720 * 4) + 300, TotalSeconds))

# convert the Period column to factor data type so we can use it for plots
shots <- mutate(shots, Period = as.factor(Period))



##################################
# Add a column for getting the basic action type
##################################

# view unique action types
unique(shots$ActionType)

# get the second-to-last word for the basic action type
shots <- shots %>% 
  mutate(BasicActionType = word(ActionType, -2))

# convert BasicActionType column to factor type
shots <- shots %>% mutate(BasicActionType = as.factor(BasicActionType))

shots %>% select(ActionType, BasicActionType)



##################################
# The function for drawing the court
##################################

draw_court <- function() {
  dummy_data <- data.frame(col1 = c(1))
  court <- c(
    coord_fixed(),
    
    # court outline
    geom_rect(aes(xmin = -250, xmax = 250, ymin = -47.5, ymax = 470), 
              fill = NA, color = "black", 
              data = dummy_data, inherit.aes = FALSE),
    
    # backboard
    geom_rect(aes(xmin = -30, ymin = -8.5, xmax = 30, ymax = -7.5), 
              fill = NA, color = "black", 
              data = dummy_data, inherit.aes = FALSE),
    
    # outer and inner paint boxes
    geom_rect(aes(xmin = -80, ymin = -47.5, xmax = 80, ymax = 142.5), 
              fill = NA, color = "black", 
              data = dummy_data, inherit.aes = FALSE),
    geom_rect(aes(xmin = -60, ymin = -47.5, xmax = 60, ymax = 142.5), 
              fill = NA, color = "black", 
              data = dummy_data, inherit.aes = FALSE),
    
    # left and right 3pt legs
    geom_segment(aes(x = -220, xend = -220, y = -47.5, yend = 92.5),
                 data = dummy_data, inherit.aes = FALSE),
    geom_segment(aes(x = 220, xend = 220, y = -47.5, yend = 92.5),
                 data = dummy_data, inherit.aes = FALSE),
    
    # hoop and free throw circles
    geom_circle(aes(x0 = 0, y0 = 0, r = 7.5),
                data = dummy_data, inherit.aes = FALSE),
    geom_circle(aes(x0 = 0, y0 = 142.5, r = 60),
                data = dummy_data, inherit.aes = FALSE),
    
    # restricted zone arc
    geom_arc(aes(x0 = 0, y0 = 0, r = 40, start = -pi/2, end = pi/2),
             data = dummy_data, inherit.aes = FALSE),
    
    # inner and outer center court arcs
    geom_arc(aes(x0 = 0, y0 = 470, r = 20, start = pi/2, end = 3*pi/2),
             data = dummy_data, inherit.aes = FALSE),
    geom_arc(aes(x0 = 0, y0 = 470, r = 60, start = pi/2, end = 3*pi/2),
             data = dummy_data, inherit.aes = FALSE),
    
    # three-point arc
    geom_arc(aes(x0 = 0, y0 = 0, r = 238, start = -1.18, end = 1.18),
             data = dummy_data, inherit.aes = FALSE)
  ) 
  return(court)
}



##################################
# Add your code here
##################################

#Q7
ggplot(shots) + geom_boxplot(aes(x=Period,y=TotalSeconds))
#As the game nears halftime or the end of the game as a whole, Curry is more likely to shoot the ball

#Q8
ggplot(shots) + geom_histogram(aes(x=TotalSeconds, color=Period), bin = 60)

#Q9
ggplot(shots) + geom_histogram(aes(x=TotalSeconds, color=Period), bin = 60) + facet_wrap(facets=vars(Season))

#Q10
table(shots$ActionType)

#Q11
table(shots$BasicActionType)

#Q12
ggplot(shots) + geom_bar(aes(x=BasicActionType))

#Q13
ggplot(filter(shots, BasicActionType == "Jump")) + geom_bar(aes(x=ActionType))

#Q14
table(shots[c("Zone","BasicActionType")])

#Q15
ggplot(shots) + geom_point(aes(x=LocX, y=LocY, color=BasicActionType)) + draw_court()

#Q16
ggplot(filter(shots, BasicActionType != "Jump")) + geom_point(aes(x=LocX, y=LocY)) + draw_court()

#Q17
ggplot(filter(shots, BasicActionType == "Jump")) + geom_point(aes(x=LocX, y=LocY, color=ActionType)) + draw_court()

#Q18
ggplot(filter(shots, BasicActionType == "Jump" & ActionType != "Jump Shot")) + geom_point(aes(x=LocX, y=LocY, color=ActionType)) + draw_court()

#Q19
ggplot(filter(shots, BasicActionType == "Dunk")) + geom_point(aes(x=LocX, y=LocY, color=ActionType)) + draw_court() + facet_zoom(xlim=c(-15,15),ylim=c(-15,35))
