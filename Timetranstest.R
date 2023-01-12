library("ggplot2")
library("chron")


# Data to graph.  Want to show a calendar, days on the left
# and candle lines showing the duration of each event.
work <- rbind(
  data.frame(ydate_start=15480, ydate_end=15489, event="Event One"),
  data.frame(ydate_start=15485, ydate_end=15499, event="Event Two")
)


# THIS SOLVES THE PROBLEM
ggplot(work,
       aes(x=event,
           y=ydate_start,
           ymin=ydate_start,
           ymax=ydate_end,
           color=event)
) +
  geom_linerange() +
  ylab("Date") +
  scale_y_reverse(label=function(x) strftime(chron(x), "%Y-%m-%d"))