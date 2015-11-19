library(truncnorm)
library(ggplot2)
library(reshape2)

mean=5; sd=1.75
pop = read.csv("populations.csv", header=TRUE)
step = 0.01
control = seq(0, 10, by=step)
sd_array = seq(1, 3, by=0.5)
default_popm = 102949
default_popw = 91332

ratingsw <- matrix(nrow=10/step + 1, ncol=2)
sd_ratingsw <- matrix(nrow=10/step + 1, ncol=5)
ratingsm <- matrix(nrow=10/step + 1, ncol=2)
age_groups <- matrix(nrow=10/step + 1, ncol=7)
sd_groups <- matrix(nrow=10/step + 1, ncol=6)

### women ###
for(i in 0:(10/step))
{
	ratingsw[i+1,1] <- i*step
	ratingsw[i+1,2] <- 1 - ptruncnorm(i*step, a=0, b=10, mean, sd)
	for(j in 1:5)
	{
		sd_ratingsw[i+1,j] <- 1 - ptruncnorm(i*step, a=0, b=10, mean, sd_array[j])
	}
}

age_groups[, 7] = ratingsw[, 1]
sd_groups[, 6] = ratingsw[, 1]

#### men ###
#by age
for(j in 1:6)
{
	for(i in 0:(10/step))
	{
		ratingsm[i+1,2] <- ratingsw[i+1,2]
		ratingsm[i+1,1] <- qtruncnorm(1 - ratingsw[i+1,2]*(pop$Women[j]/pop$Men[j]),
																	a=0, b=10, mean, sd)
	}
	age_groups[, j] = ratingsm[ ,1]
}

#by sd
for(j in 1:5)
{
	for(i in 0:(10/step))
	{
		ratingsm[i+1,1] <- qtruncnorm(1 - sd_ratingsw[i+1,j]*(default_popw/default_popm),
																	a=0, b=10, mean, sd_array[j]) # sd goes from 1 to 3 by .5
	}
	sd_groups[, j] = ratingsm[, 1]
}

### Analysis Graphs ### 

age_melt <- melt(age_groups)

agep <- ggplot() + geom_line(data=age_melt, aes(x=(Var1-1)*step, y=value, group=Var2, color=factor(Var2))) +
	labs(title="Equivalent attractiveness in SF\n", x="Women", y="Men") +
	scale_color_manual("Age groups\n", labels=c("Total", "20-29 (+7.32% men)", "20-34 (+11.28% men)", "35-49 (+25.07% men)", "50-64 (+17.29% men)", "65+ (-115.41% men)", "Control"),
		values=c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f", "#000000")) +
	theme(legend.position=c(0.8, 0.25))
agep <- agep + xlim(0,10) + ylim(0,10) +
	scale_x_continuous(breaks=0:10) +
	scale_y_continuous(breaks=0:10)

ggsave(filename="ages.png", plot=agep)

single <- matrix(nrow=10/step + 1, ncol=2)
single[, 1] = age_groups[, 3] # choose 3rd row, age 20-34
single[, 2] = age_groups[, 7]

single_melt <- melt(single)

singlep <- ggplot() + geom_line(data=single_melt, aes(x=(Var1-1)*step, y=value, group=Var2, color=factor(Var2))) +
	labs(title="Equivalent attractiveness in SF (age 20-34)\n", x="Women", y="Men") +
	scale_color_manual("Equivalences", labels=c("SF", "control"), values=c("#63BDDB", "#DB5C91")) +
	theme(legend.position=c(0.8, 0.3))
singlep <- singlep + xlim(0,10) + ylim(0,10) +
	scale_x_continuous(breaks=0:10) +
	scale_y_continuous(breaks=0:10)

ggsave(filename="single.png", plot=singlep)		 

sd_melt <- melt(sd_groups)
sdp <- ggplot() + geom_line(data=sd_melt, aes(x=(Var1-1)*step, y=value, group=Var2, color=factor(Var2))) +
	labs(title="Different SD assumptions for the 20-34 age group\n", x="Women", y="Men") +
	scale_color_manual("Standard deviation\n", labels=c("1.0", "1.5", "2.0", "2.5", "3.0", "Control"),
		values=c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#000000")) +
	theme(legend.position=c(0.8, 0.25))
sdp <- sdp + xlim(0,10) + ylim(0,10) +
	scale_x_continuous(breaks=0:10) +
	scale_y_continuous(breaks=0:10)

ggsave(filename="sd.png", plot=sdp)	

### Reference graphs ###

# truncated normal pdfs w various sd

pdfs <- matrix(nrow=10/step + 1, ncol=3)
sd_array = seq(1, 3, 1)

for(i in 1:3)
{
	pdfs[, i] <- dtruncnorm(control, a=0, b=10, mean, sd_array[i])
}

pdf_melt <- melt(pdfs)

pdfp <- ggplot() + geom_line(data=pdf_melt, aes(x=(Var1-1)*step, y=value, group=Var2, color=factor(Var2))) +
	labs(title="Truncated normal distrubtions\n", x="Attractiveness", y="Probability") +
	scale_color_manual("Standard deviation", labels=c("1", "2", "3"), values=c("#60A1E6", "#C276DE", "#D66781")) +
	theme(legend.position=c(0.85, 0.7))
pdfp <- pdfp + xlim(0,10) + ylim(0,0.42) +
	scale_x_continuous(breaks=0:10)

ggsave(filename="pdfs.png", plot=pdfp)

# Overlapping shaded men and women pdfs #

gender.df <- data.frame(x=seq(0,10,step))
gender.df$pdf_men <- dtruncnorm(gender.df$x, a=0, b=10, mean=5, sd=1.75)*default_popm
gender.df$pdf_women <- dtruncnorm(gender.df$x, a=0, b=10, mean=5, sd=1.75)*default_popw

genderp <- ggplot(data=gender.df) +
	geom_area(position='stack', aes(x=x, y=pdf_men, fill=factor("Men"))) +
	geom_area(position='stack', aes(x=x, y=pdf_women, fill=factor("Women"))) +
	labs(title="Attractiveness distributions for the 20-34 age group\n", x="Attractiveness", y="Quantity") +
	scale_fill_manual("Gender", labels=c("Men", "Women"), values=c("#259CE6", "#E84367")) +
	theme(legend.position=c(0.8, 0.7)) +
	xlim(0,10) + ylim(0,25000) +
  scale_x_continuous(breaks=0:10)

ggsave(filename="genders.png", plot=genderp)

# Graphs that show cdf equivalences

sectionpm <- ggplot() +
	geom_area(data=subset(gender.df, x > qtruncnorm(1-(5000/default_popm), a=0, b=10, mean=5, sd=1.75)), aes(x=x, y=pdf_men, fill=factor("Men"))) +
	scale_fill_manual("Top 5000", labels="Men", values="#259CE6") +
	geom_line(data=gender.df, aes(x=x, y=pdf_men, color=factor("Men"))) +
	scale_color_manual("Top 5000", labels="Men", values="#259CE6") +
	labs(title="The 5000 most attractive men in the 20-34 age group\n", x="Attractiveness", y="Quantity") +
	theme(legend.position="none") +
	xlim(0,10) + ylim(0,25000) +
  scale_x_continuous(breaks=0:10)

ggsave(filename="cdfm.png", plot=sectionpm)

sectionpw <- ggplot() +
	geom_area(data=subset(gender.df, x > qtruncnorm(1-(5000/default_popw), a=0, b=10, mean=5, sd=1.75)), aes(x=x, y=pdf_women, fill=factor("Women"))) +
	scale_fill_manual("Top 5000", labels="Women", values="#E84367") +
	geom_line(data=gender.df, aes(x=x, y=pdf_women, color=factor("Women"))) +
	scale_color_manual("Top 5000", labels="Women", values="#E84367") +
	labs(title="The 5000 most attractive women in the 20-34 age group\n", x="Attractiveness", y="Quantity") +
	theme(legend.position="none") +
	xlim(0,10) + ylim(0,25000) +
  scale_x_continuous(breaks=0:10)

ggsave(filename="cdfw.png", plot=sectionpw)