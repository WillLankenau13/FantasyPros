
# Introduction

This repository combines projected fantasy football points from FantasyPros and Yahoo Daily Fantasy data for a given week. It also optimizes a fantasy football lineup for the maximum projected fantasy points based on Yahoo's daily fantasy player prices and budget. 


# Yahoo Daily Fantasy
In Yahoo Daily Fantasy, you build a fantasy lineup (1QB, 2RB, 3WR, 1TE, 1FLEX, 1DST) from a budget of 200, with player costs starting at 10 and ranging to 40 or more, with better performing players (fantasy wise) costing more. 


# Data

### Yahoo
Csv files downloaded weekly from Yahoo Daily Fantasy Football. Includes:
- Player Name
- Player Team and Opponent for the given week
- Player Injury Designation
- Yahoo Player Cost

### FantasyProsDownloads
Csv files downloaded weekly from [FantasyPros](https://www.fantasypros.com/nfl/rankings/). Separate Csv files for each position. Includes:
- Player Name
- Player Team and Opponent for the given week
- Matchup and Start/Sit Ratings
- Projected Fantasy Points

### Tidy
Csv files joining Yahoo csv file and FantasyPros csv files by player. 
