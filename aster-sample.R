# Example invocations of aster plot
#
# Jim Regetz
# NCEAS
# Created on 13-Sept-2011

source("aster.R")

# generate some fake data
set.seed(1)
scores <- sample(1:10)
weights <- sample(1:10)
labels <- paste(LETTERS[1:10], "X", sep="")

# do some plots
png(file="aster-sample.png", height=600, width=600)
par(mfrow=c(2,2), xpd=NA)
aster(lengths=scores, widths=weights, disk=0, main="Example 1",
    plot.outline=FALSE)
aster(lengths=scores, widths=weights, labels=labels, main="Example 2",
    lty=2, fill.col="gray", plot.outline=FALSE)
aster.legend(labels=labels, widths=weights)
aster(lengths=scores, widths=weights, disk=0.5, main="Example 3",
    center="Hello world")
dev.off()

