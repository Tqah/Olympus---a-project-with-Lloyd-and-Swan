#set working dir
setwd("/Users/lloyd/Documents/lloyd_2016/Services/Olympics")

#read in csv
olymp <- read.csv("data_syog2010impact_2016-06-13_17-00.csv")

#save the first two headers in olymp
head2 <- olymp[1,]

#remove 1st row, the double header
olymp2 <- olymp[2:nrow(olymp),]

#interviewees who finished
olymp2.done <- olymp2[olymp2$FINISHED == 1,]

#check for duplicates using NRIC
duplicated(olymp2.done$PI07_01)
#Try to figure out how to check for any TRUE in the above

#need to know the friends NRIC to filter
#save friends in a vector
#loop through each fren, filter the dataframe to remove frens

#Distribution of interviewees, aware of SYOG or not
#YP01 variable: 1 --> yes, 2 --> no
olymp.know <- olymp2.done[olymp2.done$YP01 == 1,]
olymp.unaware <- olymp2.done[olymp2.done$YP01 == 2,]

#Pie chart of SYOG awareness
pie.SYOG.know <- c(nrow(olymp.know)/nrow(olymp2.done), nrow(olymp.unaware)/nrow(olymp2.done))
lbls <- c("Yes", "No")
pcnt<- round(100*pie.SYOG.know, 1)
pcnt <- paste(pcnt,"%",sep="") #ad % to pcnt
pie(pie.SYOG.know, labels = pcnt, main = "Did you know that the YOG 2010 took place in Singapore? \n [n=295]",col = topo.colors(length(pie.SYOG.know)))
legend("topright", lbls, cex = 0.8, fill = topo.colors(length(pie.SYOG.know)))
#Can be improved if not nice

#Distribution of followers/non-followers
#1 --> not checked, 2 -- checked
#discordant on follower/non-follower
#olymp.know$YP02_13 == 2 AND
#one of YP02_09, YP02_10, YP02_11, YP02_12 == 2
olymp.discord <- olymp.know[(olymp.know$YP02_09 == 2 | olymp.know$YP02_10 == 2 | 
                olymp.know$YP02_11 == 2 | olymp.know$YP02_12 == 2) 
               & (olymp.know$YP02_13 == 2),]

#retrieve the followers
#YP02_09 OR YP02_10 OR YP02_11 OR YP02_12 == 2 AND YP02_13 == 1
olymp.fol <- olymp.know[(olymp.know$YP02_09 == 2 | olymp.know$YP02_10 == 2 | 
             olymp.know$YP02_11 == 2 | olymp.know$YP02_12 == 2) 
            & (olymp.know$YP02_13 == 1),]

#retrieve non-followers
olymp.nonfol <- olymp.know[(olymp.know$YP02_09 == 1 & olymp.know$YP02_10 == 1 & 
                           olymp.know$YP02_11 == 1 & olymp.know$YP02_12 == 1) 
                        & (olymp.know$YP02_13 == 2),]

#barplot on YOG followers and non-followers
barplot(barCt.SYOG.fol, width = 2, space = NULL,
        names.arg = lbls, beside = FALSE,
        horiz = FALSE, density = NULL, angle = 45,
        col = topo.colors(2, alpha = 1), border = NA,
        main = "Did you follow the YOG 2010? \n [n = 242]",
        xlim = NULL, ylim = NULL, xpd = TRUE, log = "",
        axes = FALSE, axisnames = TRUE,
        cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
        plot = TRUE, axis.lty = 0, offset = 0)
#Need to add text on the bars for percentage and maybe a horizontal line on X-axis

#Key words associated with the YOG
#YP04_01 variable: 1st word, YP04_02 variable: 2nd word, YP04_03 variable: 3rd word, 
#olymp.word_1 <- olymp.know[olymp.know$YP04_01 == 1]
