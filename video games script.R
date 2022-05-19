videogamesales <- read.csv('video game sales.csv')

View(videogamesales)

#exploratory data analysis nintedo games

#step 1: filter for nintendo games 
library(dplyr)
Nintendo <- videogamesales %>%
  filter(Platform == "3DS" | Platform == "DS" | Platform == "GB" | Platform == "GBA" | Platform == "GC" | Platform == "N64" | Platform == "NES" | Platform == "Wii" | Platform == "WiiU")

library(ggplot2)
NintendoSales <- ggplot(data = Nintendo, aes(x=EU_Sales, y=NA_Sales)) + geom_point(color = "blue")
print(NintendoSales) + stat_smooth(color = "red")

cor.test(x=Nintendo$EU_Sales, y=Nintendo$NA_Sales, method= "pearson", conf = 0.95)
#games that sell well in Europe also sell well in North America 
#three games sell well in North America but not really in Europe 
#one game sells extremely well in Europe and North America outlier 
#81% correaltion strong correaltion can make a conclusion that nintendo games that sell well in Europe generally do well in america  

NintendoMoney <- ggplot(data = Nintendo, aes(x=JP_Sales, y=NA_Sales)) + geom_point(color = "red")
print(NintendoMoney) + stat_smooth(color = "purple")

cor.test(x=Nintendo$JP_Sales, y=Nintendo$NA_Sales, method="pearson", conf=0.95)
#58% correaltion, weak corealtion not enough to conclude that nintendo games that sell well in japan also sell well in north aermica  
#four games sell a lot better in north america than japan
#there is a packet of games that sell better in north america than Japan by at least 5million dollars


NintendoGlobal <- ggplot(data = Nintendo, aes(x=JP_Sales, y=EU_Sales)) + geom_point(color = "Purple")
print(NintendoGlobal) + stat_smooth(color = "green")

cor.test(Nintendo$JP_Sales, Nintendo$EU_Sales, method="pearson", conf.level = 0.95)
#correaltion of 59.1 , not enough proof to say that games that sell well in japan also do well in europe 
#there are a slew of games that sell better in Europe than Japan by 5 million dollar 
#there is one game that is an outlier and sell better in Europe than Japan by 25 million dollars 

#which company dominates the north american market (Nintendo, Xbox, Sony)
NintendoNAvsGlobal <- ggplot(data = Nintendo, aes(x=NA_Sales, y=Global_Sales)) + geom_point(color='black')
print(NintendoNAvsGlobal) + stat_smooth(color='green')

cor.test(x=Nintendo$NA_Sales, y=Nintendo$Global_Sales, method="pearson", conf.level = 0.95)
#there is a 95% correaltion between sales numbers in NA and global sales numbers
#there is a really strong coreratlion between how games sell in north america and how they sell across the global 

MicrosoftNAVsGlobal <- ggplot(data=Microsoft, aes(x=NA_Sales, y=Global_Sales)) + geom_point(color='grey')
print(MicrosoftNAVsGlobal) + stat_smooth(color='pink')

cor.test(x=Microsoft$NA_Sales, y=Microsoft$Global_Sales, method="pearson", conf.level=0.95) 
#98.2% correlation 

SonyNAvsGlobal <- ggplot(data=Microsoft, aes(x=NA_Sales, y=Global_Sales)) + geom_point(color='red')
print(SonyNAvsGlobal) + stat_smooth(color='purple')

cor.test(x=Sony$NA_Sales, y=Sony$Global_Sales, method="pearson", conf.level=0.95)
#corelation level is 91% 

#the champion ofthe north american market is microsoft 

NintendoGlobalHistogram <- ggplot(Nintendo, aes(x =Global_Sales)) + geom_histogram(binwidth = 2, color = "red", fill = "pink", alpha = 0.5)
print(NintendoGlobalHistogram)
#out of 6032 Nintendo Games ovre 5500 of them garner global sales between 0 and 2 million dollars 
#approximately 500 Nintendo games gathered global sales between 2 and 4 million dollars


NintendoBarChart <- ggplot(data = Nintendo, aes(x=Platform)) +
  geom_bar(aes(fill=Platform)) +
  coord_flip()

print(NintendoBarChart)
#nintedo made the most games for the DS 
#nintendo made the second most games for the wii
#mintaendo made the least amount of games for the wiiu

NintendoBarChart2 <- ggplot(data = Nintendo, aes(x=Platform, y=Global_Sales)) + 
  geom_bar(stat = "identity", aes(fill = Platform))
print(NintendoBarChart2)
#will and ds were the best selling nintendo platforms 
#wiiu was the worst selling nintendo platform 
#generally handheld platforms outsell home consoles for nintendo gamers 

NintendoPlot <- ggplot(data = Nintendo, aes(x=Critic_Score, y=Global_Sales)) + 
  geom_point(color='purple') 
print(NintendoPlot) + stat_smooth(color='yellow')

cor.test(x=Nintendo$Critic_Score, y=Nintendo$Global_Sales, method = 'pearson', conf.level = 0.95)
#15.9% correaltion not a linear relationship 

NintendoPlot2 <- ggplot(data = Nintendo, aes(x=User_Score, y=Global_Sales)) + 
  geom_point(color='pink')
print(NintendoPlot2) + stat_smooth(color='black')

cor.test(x=Nintendo$User_Score, y=Nintendo$Global_Sales, method='pearson', conf.level=0.95)
#9.2% confidence level 

NintendoSalesByGenre <- ggplot(data=Nintendo, aes(x=Year_of_Release, y=Global_Sales)) + 
  geom_bar(stat='identity', aes(fill=Genre))
print(NintendoSalesByGenre)
#late 80's-early 90's dominated by platformers 
#late 90's roleplaying makes a bit of a comeback 
#mid 2000 balanced product line not one genre is really carrying the company 
#2005 along with 2007 and 2008 anomaly simulation did really well and never recaptured the magic afterwards 
#2006 and 2009 was carried by sports and platforming (wii sports and mario bailing out the rest of the games)
#misc games (music titles etc... were big from 2006 to 2010 during the peak of the wii)

NintendoGenreGamesByYear <- ggplot(data=Nintendo, aes(x=Year_of_Release)) + 
  geom_bar(aes(fill=Genre))
print(NintendoGenreGamesByYear)
#late 90's early 2000's nintendo platofrms average close to 100 games a year 
#early to mid 2000's nintedo platforms average close to 300 games a year
#2006 to 2010 nintedno platofrms had between 600 to 700 games a year 
#2012 to present averages close to 100 games a year focuses on quality on quantity 
 
#nintendo makes a ton of action games from 2006-2010
#2002-2006 very balanced game line up 
#huge spike in adventure games in 2008 
#from 2011 large drop off in misc games, probably when the music genre fad died 


NintendoSalesByPlatform <- ggplot(data=Nintendo, aes(x=Year_of_Release, y=Global_Sales)) + 
  geom_bar(stat='identity', aes(fill=Platform))
print(NintendoSalesByPlatform)

#the gameboy had an immpressive ten year run of generating money for nintendo even outselling the NES in some years
#the gameboy advance outsold the gamecube during a four to five year strech 
#the nintendo DS bailed out nintendo in 2005 as it generated approximately 75% of the sales revenue 
#the wii u was a monstrous  disaster and was routinely outsold by the 3DS 

NintendoGamesByPlatform <- ggplot(data=Nintendo, aes(x=Year_of_Release)) + 
  geom_bar(aes(fill=Platform))
print(NintendoGamesByPlatform)
#early 90's nintendo made more games for consoles than handheld
#early 200's to present nintendo makes more games for handheld and console, cashing on gaming on the go
#nintendo made the most games in 2008 which was the peak of the wii 


Top10NintendoGames <- Nintendo %>%
  group_by(Global_Sales) %>%
  top_n(n = 10, wt = Global_Sales)

View(Top10NintendoGames)

Top10NintendoGamesSales <- head(Top10NintendoGames, 10)
View(Top10NintendoGamesSales)

#1 wii Sports 82.5 million
#2 Super Mario Bros (NES) 40.2 million
#3 Mario Kart Wii 35.5 million 
#4 wii sports resort 32.8
#5 Pokemon Red/Pokemon Blue Gameboy 31.4
#6 Tetris Gameboy 30.3
#7 New Super Mario Bros DS 29.80
#8 Wii Play 28.92
#9 New Super Mario Bros Wii 28.32
#10 Duck Hunt NES 28.31

#five wii games made it on here 
#no smash bros 
#wii sports made this list twice

Bottom10NintendoGames <- Nintendo %>%
  group_by(Global_Sales) %>%
  top_n(n = 10, wt = Global_Sales)

View(Bottom10NintendoGames)

Bottom10NintendoGamesSales <- tail(Bottom10NintendoGames, 10)
View(Bottom10NintendoGamesSales)

#worst selling nintedo games 
#1 nintendogs    $100k
#2 Fast Racing Neo $100k
#3 Mario and Luigi Paper Jam and Mario Kart 7 Double Pack $100K
#4 captain rainbow $100k 
#5 art academy home studio $100k 
#6 Mario vs DOnkey Kong Tipping Stars $100k
#7 Slide Advernture mag kid $100k
#8 Donkey Kong Jungle Beat $100k
#9 Jet Impusle $200K
#10	Otona no Renai Shousetsu: Harlequin Selection $200K 

library(ggplot2)
NintendoSalesByRating <- ggplot(data = Nintendo, aes(x = Rating, y=Global_Sales)) + 
  geom_bar(stat = "identity", aes(fill=Rating))
print(NintendoSalesByRating)

NintendoRating <- ggplot(data = Nintendo, aes(x=Rating)) + 
  geom_bar(aes(fill=Rating))
print(NintendoRating)

#can make many conclusion from this due to bad data 
#the most popular genere is E for everyone 
#exclusing the bad data the most amount of games made are E for everyone 
#nintendo makes the same amount of T and E+10 games 
#it is out of character for nintnedo to make an M game 
#nintendo should keep making E for everyone games, it genrates the most amount of sales 

NintendoSalesByYear <- ggplot(data = Nintendo, aes(x = Year_of_Release, y = Global_Sales)) + 
  geom_bar(stat = "identity", aes(fill = Year_of_Release))
print(NintendoSalesByYear)
#nintendo's best year was 2009 in terms of global sales
#nintnedo has a large spike on random years which can be attributed to launching an new console 
#2007 to 2009 nintendo had increases in sales due to the honeymoon period for the wii 
#sales surged dramtically in from 2005-2006
#2005 the ds got a ton of sales 
#2006 the ds and the wii both kicked ass 


#more bar plots 
NintendoBarPlot <- ggplot(data = Nintendo, aes(x= Genre, y=Global_Sales)) + 
  geom_bar(stat = "identity", aes(fill = Genre))
print(NintendoBarPlot)
#clearly nintendo best selling games are platformers
##action sports and micellandous do fairly well as well  
#sports is skewed slightly by the success of the wii sports franchise 
#role playing does okay on nintedo consoles 
#fighting strategy and adventure are the types of games that don't do as well 
#it is puzzling ti see nintendo make puzzle games that don't drive revenue 

NintendoBarPlot2 <- ggplot(data = Nintendo, aes(x= Genre)) + geom_bar(aes(fill = Genre))
print(NintendoBarPlot2)
#they made the most action games even though it isn't the highest selling genre
#they don't make as many sports games mainly they know they can't strike lightning twice like with wii sports 
#why doesn't they make more platforming games, they sell well 
#the miscellandous genre music sold really well 
#i thought fighting would be highe because of smash bros 

CriticScore <- ggplot(data = Nintendo, aes(x=Critic_Score)) + geom_histogram(binwidth = 5, color = 'red', fill = 'red', alpha = 0.5)
print(CriticScore)
#the most amount of critic scores fall behind 67.5 and 72.5 out of 100 mediocre at best 
#thera re a high amount of nintendo games that score between the following values 
#72.5 to 77.5 
#77.5 to 82.5
#82.5 to 87.5

UserScore <- ggplot(data = Nintendo, aes(x=User_Score)) + geom_histogram(binwidth = 1, color = 'blue', fill = 'blue', alpha = 0.5)
print(UserScore)
#the most amount of nintendo games score between 7.5 and 8.5 
#a decent amount of nintendo games score between 8.5 and 9.5 

#examine other video games (SONY, Microsoft)

Microsoft <- videogamesales %>%
  filter(Platform == "XB" | Platform == "X360" | Platform == "XOne")
View(Microsoft)

#sales by year 
MicrosoftSalesByYear <- ggplot(data = Microsoft, aes(x=Year_of_Release, y=Global_Sales)) +
  geom_bar(stat = "identity", aes(fill=Year_of_Release))
print(MicrosoftSalesByYear)
#sales for microsoft gaming consoles peaked at 2010 
#sales for microsoft gaming consoles did surpisngly well in 2008 despite the global economic meltdown 
#sales for the xbox360 gradually incrased every yeaer from 2005 to 2008 
#huge spike in sales from 2000 to 2001 probably halo effect 
#xboxone provided brief spike in sales but tanked from 2014-2016 

MicroSoftSalesByGenre <- ggplot(data = Microsoft, aes(x=Genre, y=Global_Sales)) +
  geom_bar(stat = "identity", aes(fill = Genre))
print(MicroSoftSalesByGenre)
#the cash cows for microsoft are shooter, action and sporst 
#these genres are made by third party develoeprs that are willing to 

MicroSoftSales <- ggplot(data = Microsoft, aes(x=EU_Sales, y=NA_Sales)) + geom_point(color = "purple") 
print(MicroSoftSales) + geom_smooth(color = "orange")
#some correaltion between sales of 0 and 1 million dollars 
#some games sold a lot better in europe than north america
#some games sold a lot better in north america than europe 

cor.test(x=Microsoft$EU_Sales, y=Microsoft$NA_Sales, method = "pearson", conf = 0.95)
#84.4% correaltion 
#we can generally conclude that there is strong correatlion between the sales numbers for games in europe and games in north america 

MicroSoftSales2 <- ggplot(data = Microsoft, aes(x=JP_Sales, y=NA_Sales)) + geom_point(color = 'black')
print(MicroSoftSales2) + geom_smooth(color = 'red')
#correlation of 0.46 percent, no discernible pattern, 
#no discernible pattern between games sales in japan and north america 
#generally many games do better in north america than japan 
cor.test(x = Microsoft$JP_Sales, y= Microsoft$NA_Sales, method = 'pearson', conf = 0.95)

MicroSoftSales3 <- ggplot(data = Microsoft, aes(x=JP_Sales, y=EU_Sales)) + geom_point(color = 'red')
print(MicroSoftSales3) + geom_smooth(color = 'yellow')
#doesn't seem like there is a discerniable pattern
#games seem to sell better in european markets than japan 
cor.test(x=Microsoft$JP_Sales, y=Microsoft$EU_Sales, method = 'pearson', conf.level = 0.95)
#correaltion level of 0.43 weak correation 
#no proof that there is a mutual relationship between games sales in japan and eu sales 

MicrosoftSalesBar <- ggplot(data = Microsoft, aes(x=Rating, y=Global_Sales)) + 
  geom_bar(stat = "identity", aes(fill = Rating))
print(MicrosoftSalesBar)
#clearly microsoft generates the most amount of sales through m games 

MicrosoftSalesBar2 <- ggplot(data = Microsoft, aes(x=Rating)) + 
  geom_bar(aes(fill=Rating))
print(MicrosoftSalesBar2)
#microsoft makes more T and E games than M games, yet the M games generate the most sales 
#probalby a marketing problem of not being able to appeal to a broader audience 

MicrosoftPlatform <- ggplot(data = Microsoft, aes(x=Platform, y=Global_Sales)) +
  geom_bar(stat = 'identity', aes(fill=Platform))
print(MicrosoftPlatform)
#xobox360 was dope games sold like crazy on this console 
#xboxone sucked 

MicrosoftPlatform2 <- ggplot(data = Microsoft, aes(x=Platform)) +
  geom_bar(aes(fill=Platform))
print(MicrosoftPlatform2)
#why did microsoft make so many games for the xbox when only a few of them did well 
#maybe they made better games for the xbox360 whichtranslated to better sales

MicrosoftGames <- ggplot(data = Microsoft, aes(x=Year_of_Release)) +
  geom_bar(aes(fill=Year_of_Release))
print(MicrosoftGames)
#2002-2004 peak of xbox, made the most amount of games are that console 
#dip in games produced from 2006-2007, probably due to switching to xbox360
#microsoft peaked in 2011in terms of games produced 


MicrosoftGenre <- ggplot(data = Microsoft, aes(x=Genre)) +
  geom_bar(aes(fill=Genre))
print(MicrosoftGenre)
#most games that are made are action games 
#second place are sports games 
#third place are shooting games 
#adventure simulation stragegy adn puzzle aren't made as much on xobx 

MicrosoftGenreSalesByYear <- ggplot(data = Microsoft, aes(x=Year_of_Release, y=Global_Sales)) +
  geom_bar(stat="identity", aes(fill=Genre))
print(MicrosoftGenreSalesByYear)
#sports action and shooters take up the bulk of the sales 

MicrosoftHist <- ggplot(data = Microsoft, aes(x=Critic_Score)) + 
  geom_histogram(binwidth=5, color='purple', fill='purple', alpha=0.5)
print(MicrosoftHist)
#most games have a critic score between 72.5-77.5 mediorce 

UserHist <- ggplot(data = Microsoft, aes(x=User_Score)) + 
  geom_histogram(binwidth=1, color='orange', fill='orange', alpha=0.5)
print(UserHist)
#most games score between 7.5 and 8.5 with user scores 

#is there a correaltion between gmaes that get good reviews and sell well 

plot <- ggplot(data = Microsoft, aes(x=Critic_Score, y=Global_Sales)) +
  geom_point(color = "green")
print(plot) + stat_smooth(color="red")

cor.test(x=Microsoft$Critic_Score, y=Microsoft$Global_Sales, method='pearson', conf=0.95)
#34% correaltion, no linear relationship 

plot2 <- ggplot(data = Microsoft, aes(x=User_Score, y=Global_Sales)) + 
  geom_point(color='blue')
print(plot2) + stat_smooth(color='pink')

cor.test(x=Microsoft$User_Score, y=Microsoft$Global_Sales)
#5% correlation, no linear relationship 

MicrosoftGenreSalesByYear <- ggplot(data=Microsoft,aes(x=Year_of_Release, y=Global_Sales)) +
  geom_bar(stat="identity", aes(fill=Genre))
print(MicrosoftGenreSalesByYear)
#xbox is being carried by shooters sports and action 

MicrosoftGenreCountByYear <- ggplot(data=Microsoft, aes(x=Year_of_Release)) +
  geom_bar(aes(fill=Genre))
print(MicrosoftGenreCountByYear)
#microsoft for the most part sticksto what makes them money action shooters and sports 
#they had a thing for making racing games in the early 2000's but have become more discplined 

MicroSoftSalesByPlatform <- ggplot(data=Microsoft, aes(x=Year_of_Release, y=Global_Sales)) +
  geom_bar(stat='identity', aes(fill=Platform))
print(MicroSoftSalesByPlatform)
#2005-2006 represented the passing of the baton from xbox to xbox360 in terms of being the revenue generator 
#2013-2014: passingof the torch from xobx360 to xboxone 
#so far the xboxone hasn't been the smash hit that the developers have hoped for 
#2014 was underwhelming for the xboxone

MicrosoftGamesMadeForPlatform <- ggplot(data=Microsoft, aes(Year_of_Release)) + 
  geom_bar(aes(fill=Platform))
print(MicrosoftGamesMadeForPlatform)
#2014 there are still a decent amount of xbox360 games made, a 50-50 split with xboxone 

library(dplyr)
Top10MicrosoftGames <- Microsoft %>%
  group_by(Global_Sales) %>%
  top_n(n = 10, wt = Global_Sales)

Top10MicrosoftGames <- head(Top10MicrosoftGames, 10)
View(Top10MicrosoftGames)
#kinect adventures was the best selling xbox 360 game of all time 
#gta five takes second place 
#most of the games on this list are either halo or call of duty 
#all of these games were released on the xbox 360 


Bottom10MicrosoftGames <- Microsoft %>%
  group_by(Global_Sales) %>%
  top_n(n = 10, wt = Global_Sales)

Bottom10MicrosoftGames <- tail(Bottom10MicrosoftGames, 10)
View(Bottom10MicrosoftGames)
#most of the games on here are no name action games 
#there is a possibility that all data from 2016 wasn't collected as there is a metal gear solid game on the list 

#sony scripts 
Sony <- videogamesales %>%
  filter(Platform == 'PS2' | Platform == 'PS3' | Platform == 'PS4' | Platform == 'PS' | Platform == 'PSP' | Platform == 'PSV')

View(Sony)

SonyUSVsEurope <- ggplot(data=Sony, aes(x=EU_Sales, y=NA_Sales)) + geom_point(color='red')
print(SonyUSVsEurope) + geom_smooth(color='yellow')

cor.test(x=Sony$EU_Sales, y=Sony$NA_Sales, method='pearson', conf.level = 0.95)
#there is a 72% correatlion between sales for playstation games in the EU and sales for playstation games in North America not bad 

#6 games sold better in Europe than north america 
#several 14 games sold better in North america than europe 

SonyUSVsJapan <- ggplot(data=Sony, aes(x=JP_Sales, y=NA_Sales)) + geom_point(color='green')
print(SonyUSVsJapan) + geom_smooth(color='black')

cor.test(x=Sony$JP_Sales, y=Sony$NA_Sales, method='pearson', conf.level = 0.95) 
#20% correlation no linear relationship 
#most games sell a lot better in north america than japan 

SOnyJpVsEurope <- ggplot(data=Sony, aes(x=JP_Sales, y=EU_Sales)) + geom_point(color='blue')
print(SOnyJpVsEurope) + geom_smooth(color='pink')

cor.test(x=Sony$JP_Sales, y=Sony$EU_Sales, method='pearson', conf.level = 0.95)
#21% correaltion 

SonySalesByConsole <- ggplot(data=Sony, aes(x=Platform, y=Global_Sales)) + 
  geom_bar(stat="identity", aes(fill=Platform))
print(SonySalesByConsole)
#sales data for ps4 is skewed slightly as data caps at 2016 
#ps3 did fairly well 
#PS2 was clearly the highest selling platform of all time 
#PSp and psvita didn't sell terribly well 
#ps3 sold a lot better and PSP

SonyGamesByConsole <- ggplot(data=Sony, aes(x=Platform)) + 
  geom_bar(aes(fill=Platform))
print(SonyGamesByConsole)
#Sony made almost as many PSP games as PS3 games yet the PS3 sold a lot better 
#PS2 had the most games by a considerable margin as it sold the most
#why didn't they make more ps3 games is sold more than the original playstation 
#psvita was left to die limited amount of games made for it 


SonySalesByYear <- ggplot(data=Sony, aes(x=Year_of_Release, y=Global_Sales)) + 
  geom_bar(stat="identity", aes(fill=Year_of_Release))
print(SonySalesByYear)
  
#1998 peak of the ps1
#major slaes spike from 2000 to 2001 ps2 gets mainstream popularity 
#2009 to 2012 sales consistenly declined some of this could be attrbuted to the ps3 going ot the ned of the cycle 
#2011 to 2012 sonys sales are decrease by roughly 50% coudl have something to do with their lack of responsiveness to the PSN hack 
#huge sales incrase from 2012 to 2013 due to realse of ps4 

SonyGamesByYear <- ggplot(data=Sony, aes(x=Year_of_Release)) + 
  geom_bar(aes(fill=Year_of_Release)) 
print(SonyGamesByYear)
#sony made a shit ton of games for the PS3 and trying to get the last sale for the ps2
#sony alsom ade more games in 2005 
#the number of games that sony made went consistenly down from 2007 to 2012
#based on the games that are produced the average life cycle for a sony console is 5 year

SonyGamesSalesByGenre <- ggplot(data=Sony, aes(x=Genre, y=Global_Sales)) + 
  geom_bar(stat="identity", aes(fill=Genre))
print(SonyGamesSalesByGenre)
#action games have been carrying sony consoles the entire time 
#sports shooters role playing and racing games are the only other genres that sell at a decent rate 


SonyGamesMadeByGenre <- ggplot(data=Sony, aes(x=Genre)) + 
  geom_bar(aes(fill=Genre))
print(SonyGamesMadeByGenre)
#for the most part sony knows what they are good at(making action games)
#don't make as many action games please 

SonyGamesSalesByYearByConsole <- ggplot(data=Sony, aes(x=Year_of_Release, y=Global_Sales)) + 
  geom_bar(stat='identity', aes(fill=Platform))
print(SonyGamesSalesByYearByConsole)
#sony couldn't compete with nintendo in the gaming on the go catergory 
#PS2 had one of the longest life cycles in console history selling for eight years and being the star of the show for six years 
#PS3 got off to a rough start and took two years befor sales took off


SonyGamesSalesByYearByGenre <- ggplot(data=Sony, aes(x=Year_of_Release, y=Global_Sales)) + 
  geom_bar(stat='identity', aes(fill=Genre))
print(SonyGamesSalesByYearByGenre)

#create variables handheld or console (0 and 1 before trying machine learning)
#action games and sports sell consistenly well on each console iteration 
#sony stopped making racing games with PS4 as the sales for that genre declined 
#fighting games did well on the ps2 did okay on the ps2, but didn't sell well on the ps3 and ps4 
#shooters have sold better from the ps3 onward to the ps4 

SonySalesByRating <- ggplot(data=Sony, aes(x=Rating, y=Global_Sales)) + 
  geom_bar(stat='identity', aes(fill=Rating))
print(SonySalesByRating)
#sony has a balanced lineup of games sales don't differ drastically from T, M and E 

SonySalesByRatingByYear <- ggplot(data=Sony, aes(x=Year_of_Release, y=Global_Sales)) + 
  geom_bar(stat='identity', aes(fill=Rating))
print(SonySalesByRatingByYear)
#ps2 sales were carried by E and T games 
#ps3 was a shift were T games still sold a lot by M games has a increase in sales 
#e games had a dip in sales in the PS3, sony was trying to cature the magic that microsoft had with halo 
#M games were the leading selling ofr the end of the ps3 era and the start of the ps4 
#e10+ games don't sell well for playstation 

SonyGamesByRatingByYear <- ggplot(data=Sony, aes(x=Year_of_Release)) + 
  geom_bar(aes(fill=Rating))
print(SonyGamesByRatingByYear)
#after 2009 sony decrased the amount of E games they made 
#sony doesn't make a ton of M games but they sell very well, is it one games like call of duty carrying these numbers of are there  multiple franchsies that can be sustainable
#at then ed of hte ps3 era T games weren't made as much as they didn't sell 

SonyCriticScoreHist <- ggplot(data=Sony, aes(x=Critic_Score)) + 
  geom_histogram(binwidth=5, color='red', fill='pink', alpha=0.5)
print(SonyCriticScoreHist)
#most playstation games have a criticscore that ranges from 72.5 to 77.5 #mediorce 

SonyUserScoreHist <- ggplot(data=Sony, aes(x=User_Score)) + 
  geom_histogram(binwidth =0.5, color='red', fill='green', alpha=0.5)
print(SonyUserScoreHist)
#most of the user score range from 7.75 to 8.25 above average 

library(dplyr)
Top10SonyGames <- Sony %>%
  group_by(Global_Sales) %>%
  top_n(n = 10, wt = Global_Sales)

Top10SonyGames <- head(Top10SonyGames, 10)
View(Top10SonyGames)
#top selling palystation games GTA V 
#gta and call of duty sell very well on playstation consoles 


Bottom10SonyGames <- Sony %>%
  group_by(Global_Sales) %>%
  top_n(n = 10, wt = Global_Sales)

Bottom10SonyGames <- tail(Bottom10SonyGames, 10)
View(Bottom10SonyGames)

library(dplyr)
pc <- videogamesales %>%
  filter(Platform == "PC")

library(ggplot2)
pcgamesNAEU <- ggplot(data=pc, aes(x=EU_Sales, y=NA_Sales)) + geom_point(color="green")
print(pcgamesNAEU) + stat_smooth(color="yellow")

cor.test(x=pc$NA_Sales, y=pc$EU_Sales, method="pearson", conf.level = 0.95)
#40% correaltion not a linear relationship 

pcsalesbyyear <- ggplot(data=pc, aes(x=Year_of_Release, y=Global_Sales)) + 
  geom_bar(stat = "identity", aes(fill=Year_of_Release))
print(pcsalesbyyear)
#2011 pc sales were the best by a considerable margin 
#from 2007 to 2011 sales for pc games increased every year 
#from 1998 to 2004 pc game saesl increased every year 
#average sales globally are roughly 10 million 

pcsalesbyyearbygenre <- ggplot(data=pc, aes(x=Year_of_Release, y=Global_Sales)) + 
  geom_bar(stat = "identity", aes(fill=Genre))
print(pcsalesbyyearbygenre)
  
#early 1990's sale sare driven by shooters probably doom counterstrike etc.. 
#strategy dominates from 1995-1999
#2001-2003 sales are domianted by simulation like civilzation 
#2011 domianted by roleplaying and shooters

pcsalesgamesbygenre <- ggplot(data=pc, aes(x=Year_of_Release)) + 
  geom_bar(aes(fill=Genre))
print(pcsalesgamesbygenre)
#2006-2008 average of 50 games made per year
#2009-2011 average of roughly 100 games per year 
#2013-2016 roughly 50 games per year 

pcgamesperyear <- ggplot(data=pc, aes(x=Year_of_Release)) + geom_bar()
print(pcgamesperyear)

pcsalescount <- ggplot(data=pc, aes(x=Global_Sales)) + geom_histogram(binwidth=2, color="red", fill="pink", alpha=1)
print(pcsalescount)
#most games sales range from 0 to 1.25 milllion

Top10PCGames <- pc %>%
  group_by(Global_Sales) %>%
  top_n(n = 10, wt = Global_Sales)

View(Top10PCGames)

Top10PCGames <- head(Top10PCGames, 10)
View(Top10PCGames)

Bottom10PCGames <- pc %>%
  group_by(Global_Sales) %>%
  top_n(n = 10, wt = Global_Sales)

View(Bottom10PCGames)

Bottom10PCGames <- tail(Bottom10PCGames, 10)
View(Bottom10PCGames)
#how the hell did a metal gear solid game appear on this list 

#sega 

sega <- videogamesales %>%
  filter(Sega == 1)

View(sega)

segasalesbyplatform <- ggplot(data = sega, aes(x=Platform, y=Global_Sales)) +
  geom_bar(stat='identity', aes(fill=Platform))
print(segasalesbyplatform)

segasalesbyrating <- ggplot(data =sega, aes(x=Year_of_Release, y=Global_Sales)) + 
  geom_bar(stat='identity', aes(fill=Rating))
print(segasalesbyrating)
#take with a grain of salt most of the ames didn't ahve a rating 

segasalesbyGenre <- ggplot(data = sega, aes(x=Year_of_Release, y=Global_Sales)) + 
  geom_bar(stat='identity', aes(fill=Genre))
print(segasalesbyGenre)
#dominated by fighting and adventure games in early years 
#sports became popular in regular years 


segarating <- ggplot(data = sega, aes(x=Rating)) +
  geom_bar(color='grey')
print(segarating)

segagenre <- ggplot(data = sega, aes(x=Genre)) + 
  geom_bar(color='grey', fill='purple')
print(segagenre)
#made mostly action and fighting games 

segacountofgenrebyyear <- ggplot(data = sega, aes(x=Year_of_Release)) + 
  geom_bar(aes(fill=Genre))
print(segacountofgenrebyyear)
#sega peaked between 1994 and 1995 
#wasn't dominated by sonic 









  