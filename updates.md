## March 21

Scraped the standings table, goal leaders, and assist leaders tables from ESPN. I scraped them from the website, tidied them by renaming the clubs levels, and arranging the table by descending Points. I have not done anything to tidy the goals and assist tables yet.

## March 23 Beginning of Class

I created some plots using the statsfull, changed the statistic variables to numeric instead of character. Then, I tried to solve the issue with the Rank variable, and how to have a value take the last non-NA value before that, but could not figure out how to put the new data in the Rank column.

## March 23 End of Class

I fixed the rank variable, and created my Shiny app. I had to figure out how to put tabs, and then how to add output to each tab. I also had to check to make sure that the club variables in all the data sets were named the same so that the input will go between tabs correctly.

## March 28 Beginning of Class

Over the weekend I included conferences, and added the team value data set, which I have yet to put into a tab on the app. I had trouble figuring out how to have someone see the conference table only.

## March 28 End of Class

I am working on trying to have one input affect the values of the other input. I want the user to be able to select a conference, and then the next input is the clubs, but only from the conference they input. I tried adding another tab for values, but got stuck messing with the club/conference inputs.

## March 30 Beginning of Class

The majority of time outside of class was getting the updateSelectizeInput function to work, I got it to work partly, I just need to get help on only having newclub2 be the variable to choose from. I also added Points per game (PPG), and started on possibly adding smoothers for each conference with a variable, that they could choose, plotted against Points.

## March 30 End of Class

This class was spent fixing the updateSelectizeInput function. I also fixed the label ability on the plots to outline the point, and create the label beside it, not being the whole point. I am trying to possibly add value_df to the app, and I scraped attendance_df. I could add a tab for team value compared to points scored, and also attendance at their home stadium.