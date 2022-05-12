  Trent Meyer
  
  Data Visualization Final Project Writeup
  
  My final project is a Shiny App that visualizes data from the 2022 Major League Soccer season.
I chose this data because I am a fan of MLS, and have always been interested in the data behind the league. I wanted to create visualizations that would allow me and others to see how our favorite clubs compare to the rest of the MLS. I had a few questions: 1. Are teams who score more, winning more? 2. Are more valuable clubs winning/scoring more compared to less valuable clubs? 3. Which clubs are over/underperforming expected values? The data came from a few different sources. First, statsfull scrapes data from ESPN, which gives us a basic standings table with statistics for each club. Next, assist_leaders and goal_leaders scrapes data from ESPN which shows the top 50 players in scoring and assisting goals. The value_df data set scrapes data from TransferMarkt, a website that gives club and player value based on player form, and market demand for that player. Lastly, to get expected values for the goalie_df, player_df, and xgoals_df, I used the American Soccer Analysis app, which calculates expected values based on in-game variables such as shot-type, angle, distance, and many others. goalie_df gives expected goals for each goalkeeper, and how many goals they should be saving. player_df looks at each player's individual expected goals, to see whether they are under/over-performing. xgoals_df shows expected statistics, such as expected-points and expected goals compared to actual statistics for each club. I combined statsfull, value_df, and xgoals_df and renamed that statsfull.

  My visualizations are created from a data set that was not unethical by any means. Before creating this app, and choosing this MLS 2022 season data, I had to think about whether my data could negatively impact someone's life. I do not believe my visualizations and app will put anyone in harm. This relates to one of the twelve principles for ethical data science practices, "Consider carefully the ethical implications of choices we make when using data, and the impacts of our work on individuals and society." 
  
  When creating visualizations for expected values compared to actual values, I chose to use lollipop plots instead of bar plots. This is because I wanted to enable the user to see variability in the data. I could choose a lollipop plot because I was not showing a count. When choosing colour scales, I chose CVD-friendly colors in order to allow my app to be used by as many people as possible. I also took into consideration the perspective of the user, and how similar colors and shapes could change how they view the data.

  If I had more time, I would like to look at other MLS seasons, and possibly compare clubs and their performance between seasons. I would also like to possibly add features that would look more specifically at individual players' performances, and allow users to compare between players. If that data was available, I would like to possibly include that into the app. I would also like to possibly add some sort of prediction for whether the team will win, lose, or draw their upcoming game. I also think it would be interesting to add a tab to the app regarding the average age of the club, to see whether younger or older teams are performing better. Some limitations I had were having to download CSV's from the American Soccer Analysis app, which makes it less user friendly as they will have to download the new data from the app after each week of the season since it is currently ongoing. 