# This assignment will produce an analysis of the shots taken
# by Stephen Curry, a NBA player for the Golden State Warriors.
# It includes the shot locations for every made and missed shot
# in every game that Curry played from 2009 to 2019.

# This assignment will need 2 libraries which we have not
# used this year. You will first need to install the 
# packages RJSONIO and ggforce.

# Three packages will be needed to complete this assignment.

library("tidyverse")
library("RJSONIO")
library("ggforce")

# The data is a JSON (JavaScript Object Notation) file.

# Set Your Working Directory. Use the data directory that
# we have been using this semester.

setwd("..")
setwd("Documents/murach_r/data")
getwd()

# Download the JSON file

url = "https://www.murach.com/python_analysis/shots.json"
dest_file <- "shots.json"

download.file(url, dest_file)

# Read the JSON file

json_data <- fromJSON("shots.json")

# Build the tibble

# Part I: Get the Column Names

column_names <- json_data[["resultSets"]][[1]][["headers"]]

# Part II: Get the Rows

rows <- json_data[["resultSets"]][[1]][["rowSet"]]

# Part III: Build the Data Frame

shots <- data.frame()
for (row in rows) {
  shots <- rbind(shots, row)
}
names(shots) <- column_names

# Part IV: Convert the Data Frame to a Tibble

shots <- as_tibble(shots)

# Examine the Count of Unique Values for Each Column

apply(X = shots, MARGIN = 2, FUN = unique) %>%
  lapply(FUN = length) %>% str()

# Examine the Unique Values for the SHOT_MADE_FLAG Column

unique(shots$SHOT_MADE_FLAG)

# Select and Rename the Columns

shots <- shots %>%
  select(GAME_DATE, SHOT_TYPE, EVENT_TYPE, SHOT_MADE_FLAG,
         LOC_X, LOC_Y, SHOT_ZONE_BASIC)

names(shots) <- c("GameDate", "ShotType", "EventType",
                 "ShotMadeFlag", "LocX", "LocY", "Zone")

# Improve the Data Types for Two Columns

shots <- shots %>% mutate(
  GameDate = as.Date(GameDate, format = "%Y%m%d"),
  ShotMadeFlag = as.logical(ShotMadeFlag)
)

# Let's Look at Our Data So Far

shots

# Define a Function that Gets the Season

get_season <- function(row) {
  month <- as.integer(format(as.Date(row["GameDate"]), "%m"))
  year <- as.integer(format(as.Date(row["GameDate"]), "%Y"))
  season <- ""
  if(month > 6) {
    season <- str_c(year, "-", year+1)
  }
  else {
    season <- str_c(year-1, "-", year)
  }
  return(season)
}
  
# Apply the Function to Add a Season Column

shots <- shots %>%
  mutate(Season = apply(X = shots, MARGIN = 1, 
                        FUN = get_season))

# View the New Column

shots %>% select(GameDate, Season) %>% unique()

# Examine the Shot Types

unique(shots$ShotType)

# Add a Points Column

shots <- shots %>%
  mutate(Points = ifelse(ShotMadeFlag == TRUE,
                         ifelse(ShotType == "3PT Field Goal",
                                3, 2), 0))

# Create Three Summary Columns for Each Game

shots <- shots %>% group_by(GameDate) %>%
  mutate(PointsPerGame = sum(Points),
         AttemptedPerGame = n(),
         MadePerGame = sum(ShotMadeFlag))

# View the New Columns

shots %>% select(GameDate, Points, PointsPerGame,
                 AttemptedPerGame, MadePerGame)

# Plot Shots Made Per Geame by Season

ggplot(shots, aes(x = Season, y = MadePerGame,
                  fill = Season)) +
  geom_boxplot() +
  labs(title = "Shots Made Per Game", x = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Plot Shots Attempted vs. Made Per Game

ggplot(filter(shots, Season == "2018-2019"),
       aes(x = GameDate)) +
  geom_line(aes(y = MadePerGame, color = "Made"), 
            linewidth = 1) +
  geom_line(aes(y = AttemptedPerGame, color = "Attempted"), 
            linewidth = 1) +
  labs(title = "2018-2019 Season", x = "", y = "", 
       color = "Shots") +
  theme(plot.title = element_text(hjust = 0.5))

# Plot Shots Made Per Game with Facet Zoom

ggplot(shots, aes(x = GameDate, y = MadePerGame)) +
  geom_line(linewidth = 1, color = "blue") +
  labs(title = "Shots Made Per Game", x = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_zoom(x = Season == "2018-2019")

# Plot Shot Statistics Per Season

# Prepare the Data

shots_season <- shots %>%
  group_by(Season) %>%
  summarize(MeanMade = mean(MadePerGame),
            MeanAttempted = mean(AttemptedPerGame),
            MeanPoints = mean(PointsPerGame))

# View the Data

shots_season

# Plot the Data

ggplot(shots_season, aes(x = Season, group = 1)) +
  geom_line(aes(y = MeanMade, color = "Made"),
            linewidth = 1) +
  geom_line(aes(y = MeanAttempted, color = "Attempted"),
            linewidth = 1) +
  geom_line(aes(y = MeanPoints, color = "Points"),
            linewidth = 1) +
  labs(y = "", color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 30)

# Plot Shooting Percentages Per Season

# Prepare the Data

shots_pct_season <- shots %>%
  group_by(Season) %>%
  summarize(Made = sum(ShotMadeFlag),
            Attempted = n()) %>%
  mutate(SeasonPct = (Made / Attempted),
         CumulativePct = (cumsum(Made) / cumsum(Attempted)))

# Plot the Data

ggplot(shots_pct_season, aes(x = Season, group = 1)) +
  geom_line(aes(y = SeasonPct, color = "Percent"),
            linewidth = 1) +
  geom_line(aes(y = CumulativePct, color =
                  "Cumulative Percent"), linewidth = 1) +
  labs(title = "Shooting Percent by Season", x = "", y = "",
       color = "") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Plot Shot Locations for Two Games

# View the Columns

select(shots, GameDate, EventType, LocX, LocY)

# Plot the Data

game_dates <- c(as.Date("2019-01-13"), 
                as.Date("2019-02-28"))

ggplot(filter(shots, GameDate %in% game_dates),
       aes(x = LocX, y = LocY, color = EventType)) +
  geom_point() +
  labs(title = "Shot Location by Game", x="", y="", 
       color = "") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  facet_wrap(facets = vars(GameDate)) +
  coord_fixed()

# The Draw_Court() Function

draw_court <- function() {
  dummy_data <- data.frame(col1 = c(1))
  court <- c(
    coord_fixed(),
    
    # Court Outline
    
    geom_rect(aes(xmin = -250, xmax = 250,
                  ymin = -47.5, ymax = 470),
              fill = NA, color = "black",
              data = dummy_data, inherit.aes = FALSE),
    
    #Backboard
    
    geom_rect(aes(xmin = -30, ymin = -8.5,
                  xmax = 30, ymax = -7.5),
              fill = NA, color = "black",
              data = dummy_data, inherit.aes = FALSE),
    
    #Outer and Innter Paint Boxes
    
    geom_rect(aes(xmin = -80, ymin = -47.5,
                  xmax = 80, ymax = 142.5),
              fill = NA, color = "black",
              data = dummy_data, inherit.aes = FALSE),
    
    geom_rect(aes(xmin = -60, ymin = -47.5,
                  xmax = 60, ymax = 142.5),
              fill = NA, color = "black",
              data = dummy_data, inherit.aes = FALSE),
    
    #Left and Right 3 Point Legs
    
    geom_segment(aes(x = -220, xend = -220,
                     y = -47.5, yend = 92.5),
                 data = dummy_data, inherit.aes = FALSE),
    
    geom_segment(aes(x = 220, xend = 220,
                     y = -47.5, yend = 92.5),
                 data = dummy_data, inherit.aes = FALSE),
    
    #Hoop and Free Throw Circle
    
    geom_circle(aes(x0 = 0, y0 = 0, r = 7.5),
                data = dummy_data, inherit.aes = FALSE),
    
    geom_circle(aes(x0 = 0, y0 = 142.5, r = 60),
                data = dummy_data, inherit.aes = FALSE),
    
    #Restricted Zone Arc
    
    geom_arc(aes(x0 = 0, y0 = 0, r = 40, 
                 start = -pi/2, end = pi/2),
             data = dummy_data, inherit.aes = FALSE),
    
    #Inner and Outer Center Court Arcs
    
    geom_arc(aes(x0 = 0, y0 = 470, r = 20, 
                 start = pi/2, end = 3*pi/2),
             data = dummy_data, inherit.aes = FALSE),
    
    geom_arc(aes(x0 = 0, y0 = 470, r = 60, 
                 start = pi/2, end = 3*pi/2),
             data = dummy_data, inherit.aes = FALSE),
    
    #Three-Point Arc
    
    geom_arc(aes(x0 = 0, y0 = 0, r = 238, 
                 start = -1.18, end = 1.18),
             data = dummy_data, inherit.aes = FALSE),
  )
  return(court)
}

# Plot the Court

ggplot() + draw_court()

# Plot Shot Loctions for Two Games on the Court

ggplot(filter(shots, GameDate %in% game_dates),
       aes(x = LocX, y = LocY, color = EventType)) +
  geom_point() +
  labs(title = "Shot Location by Game", x = "", y = "",
       color = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(facets = vars(GameDate)) +
  draw_court()

# Plot Shots by Zone for One Season

ggplot(data = filter(shots, Season == "2009-2010"),
       aes(x = LocX, y=LocY, color = Zone)) +
  geom_point() +
  labs(title = "2009-2010 Season", x = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  draw_court()

# Same Plot Adding Facet_Zoom() Function

facet_zoom(xlim = c(-60, 60), ylim = c(-50, 150), 
           zoom.size =1)

# Plot Shot Count by Zone

ggplot(shots, aes(x = Zone, fill = Zone)) +
  geom_bar() +
  labs(x = "", y = "Count", title = "Shot Count by Zone") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")






  







