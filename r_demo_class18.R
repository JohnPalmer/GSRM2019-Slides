library(tidyverse)

D = data_frame(person=c("Alice", "Bob"), wages=c(25000, 31000))

ggplot(D, aes(x=person, y=wages)) + geom_bar(stat = "identity") + coord_cartesian(ylim=c(0, 100000))
ggsave("alice_bob_bar_badaxes1.png", height=3, width=4)

ggplot(D, aes(x=person, y=wages)) + geom_bar(stat = "identity") + coord_cartesian(ylim=c(24000, 32000))
ggsave("alice_bob_bar_badaxes2.png", height=3, width=4)

ggplot(D, aes(x=person, y=wages)) + geom_bar(stat = "identity")
ggsave("alice_bob_bar.png", height=3, width=4)


D = data_frame(gender=c(rep("Male", 1000), rep("Female", 1000)), wages=c(rnorm(1000, mean = 30616, sd=3000), rnorm(1000, mean = 26550, sd=3000)))

ggplot(D) + geom_density(aes(x=wages,colour=gender)) + ggtitle("Hypothecal Wage Distributions" ,subtitle = "(Not real data)")
ggsave("male_female_densities.png", height=3, width=4)

ggplot(D) + geom_density(aes(x=wages,fill=gender), alpha=.4) + scale_fill_brewer(palette="Dark2", guide=FALSE) + ggtitle("Hypothecal Wage Distributions" ,subtitle = "(Not real data)") + annotate("text", x=22000,y=.0001, label="Women") + annotate("text", x=35000,y=.0001, label="Men")
ggsave("male_female_densities_better.png", height=3, width=4)

