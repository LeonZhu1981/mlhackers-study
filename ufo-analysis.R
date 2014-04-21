library(ggplot2)
library(scales)
library(plyr)

# Load ufo data from local file, you can download the ufo data from: http://www.infochimps.com/datasets/60000-documented-ufo-sightings-with-text-descriptions-and-metada
ufo<-read.delim("ufo_awesome.tsv", sep="\t", stringsAsFactors=FALSE,header=FALSE, na.strings="")
# Named the each column.
names(ufo)<-c("DateOccurred", "DateReported", "Location", "ShortDescription", "Duration", "LongDescription")
# Find the invalidate format data, and remove it.
good.rows<-ifelse(nchar(ufo$DateOccurred)!=8|nchar(ufo$DateReported)!=8,FALSE,TRUE)
length(which(!good.rows))
ufo<-ufo[good.rows,]
# Convert to YY-mm-dd format for DateOccurred and DateReport column.
ufo$DateOccurred<-as.Date(ufo$DateOccurred, format="%Y%m%d")
ufo$DateReported<-as.Date(ufo$DateReported, format="%Y%m%d")

# Define a function to split Location column, the original value is: Iowa City, IA
# split two column: State and City.
get.location<-function(l) {
	split.location<-tryCatch(
		strsplit(l, ",")[[1]], 
		error=function(e) return(c(NA, NA))
	)
	clean.location<-gsub("^ ", "", split.location)
	if (length(clean.location)>2) {
		return(c(NA,NA))
	}
	else {
		return(clean.location)
	}
}

# Apply the get.location function. foreach process the data.
city.state<-lapply(ufo$Location, get.location)

# Build a martrix which the ufo event occurred in US.
location.martrix<-do.call(rbind, city.state)
ufo<-transform(ufo, USCity=location.martrix[,1], USState=tolower(location.martrix[,2]))
us.states<-c("ak","al","ar","az","ca","co","ct","de","fl","ga","hi","ia","id","il","in",
        "ks","ky","la","ma","md","me","mi","mn","mo","ms","mt","nc","nd","ne","nh","nj","nm",
        "nv","ny","oh","ok","or","pa","ri","sc","sd","tn","tx","ut","va","vt","wa","wi","wv",
        "wy")
ufo$USState<-us.states[match(ufo$USState,us.states)] 
ufo$USCity[is.na(ufo$USState)]<-NA
ufo.us<-subset(ufo, !is.na(USState))

# Build a subset which the event occurred more than 1990-01-01
ufo.us<-subset(ufo.us, DateOccurred>=as.Date("1990-01-01"))

# Intermedia diagram
#quick.hist<-ggplot(ufo.us, aes(x=DateOccurred))+geom_histogram()+scale_x_date(breaks = "50 years", labels=date_format("%Y"))
#ggsave(plot=quick.hist, filename="quick_hist.png", height=6, width=8)

# Add new cloumn which named YearMonth
ufo.us$YearMonth<-strftime(ufo.us$DateOccurred, format="%Y-%m")
#get data frame row count.
#nrow(ufo.us)  
# Use ddply from package: plyr, map-reduce stat row count of USState, YearMonth
sightings.counts<-ddply(ufo.us,.(USState,YearMonth), nrow)
# Build missing data of each month.
# Just like the following list:
# State  YearMonth Sightings
# AK     1990-01  1
# AK     1990-02  0
# AK     1990-03  1
date.range<-seq.Date(from=as.Date(min(ufo.us$DateOccurred)), to=as.Date(max(ufo.us$DateOccurred)), by="month")
date.strings<-strftime(date.range, "%Y-%m")
states.dates<-lapply(us.states,function(s) cbind(s,date.strings))
states.dates<-data.frame(do.call(rbind,states.dates), stringsAsFactors=FALSE)
all.sightings<-merge(states.dates, sightings.counts, by.x=c("s","date.strings"), by.y=c("USState","YearMonth"),all=TRUE)
names(all.sightings)<-c("State","YearMonth","Sightings")
all.sightings$Sightings[is.na(all.sightings$Sightings)]<-0
all.sightings$YearMonth<-as.Date(rep(date.range,length(us.states)))
all.sightings$State<-as.factor(toupper(all.sightings$State))

state.plot <- ggplot(all.sightings, aes(x = YearMonth,y = Sightings)) + 
	geom_line(aes(color="darkblue")) +
	facet_wrap(~State, nrow = 10, ncol = 5) +
	theme_bw() +
	scale_color_manual(values = c("darkblue" = "darkblue"), guide="none") + 
	scale_x_date(breaks = date_breaks("5 years"), labels=date_format("%Y")) +
	xlab("Time") +
	ylab("Number of Sightings") +
	labs(title = "Number of UFO sightings by Month-Year and U.S. State (1990-2010)")

rootDir<-getwd()
imagesDir<-"images"
outputImageFileName<-"ufo_sightings.pdf"
dir.create(file.path(rootDir, imagesDir), showWarnings = FALSE)
ggsave(plot = state.plot, filename = file.path(imagesDir, outputImageFileName), width = 14, height = 8.5)