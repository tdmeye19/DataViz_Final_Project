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

## April 4 Beginning of Class

This past weekend I spent a lot of time trying to find data sets and add new ta the app. I found a website that has a lot of data on xGoals and xPoints for teams, so I will add a tab or two along with a visual for that. I tried working with the club value and attendance data sets, however the format after scraping would not allow for changes. I spent almost an hour tidying the xgoals_df, and creating plots. I will just need to incorporate that into my app.

## April 4 End of Class

I found out how to display the input club on the lollipop plot, which allows the user to see how the club they chose was different compared to the rest. I started to change the labels on the plots to look nicer, along with changing colors so that the plots are CVD friendly. I am currently playing around in a smaller app trying to figure out how to highlight a row based on user input, which I have never gotten to work yet.

## April 6 Beginning of Class

I worked on formatting the conference table so that the user input club will be highlighted. I am testing the code in input_test.R and have some errors that I need to fix. I also tried looking up solutions to the Conference issue since the data is scraped every week to update with the weekend games. 

## April 6 End of Class

I used a case_when statement to have the conferences stay set instead of manually coding them.I am still working on the highlighting idea, and am having troubles using reactive values.

## April 18 Beginning of Class (Fixing the committ message)

I fixed some more of the labels on the club value plots. I added a new tab with goals for compared to club value, to see if the money spent on attackers and goal scorers is paying off for clubs like Atlanta and LAFC. On the xPoints and xGF plots I fixed the legend so that it says what each color means. I am trying to add some goalkeeper stats that I found, I got the data to read in, I just need to find out which plot I want to make since it is individual keepers instead of club data. 

## April 18 End of Class

I fixed the axis text so that it was larger, along with fixing the size of the points. Viewing the app in a separate window made the texts and points much smaller, but now they are readable. I added another tab for the goalie data with a plot, I am going to try and add a line or some visual to the data that shows the average or what is ideal for that stat.

## April 20 Beginning of Class

I changed the theme of the app, along with the themes of the other plots in order to make them easier to see with the new app theme. I fixed some of the variables, such as GP, so that it would be easier for a non-soccer fan to understand that it meant "Games Played". I just need to figure out if adding lines to plots to show averages would make sense, and where those lines would go for each plot.

## April 20 End of Class

I changed the keeper stats plot so that it instead shows goals conceded with expected goals. I added a line to the plot that shows who is under-performing or over-performing their expected goals value. I added a boxplot to show which conference's keeper are facing more shots, and now I just need to figure out how to format a tab with more than one plot output.

## April 25 Beginning of Class

I added some horizontal lines for where the Goal Differential was 0, however ran into some issues when trying to add a line where the mean club value was. Also, I am trying to have two plots be side-by-side in the same tab, however, I am having an issue with commas and parentheses that I cannot figure out.