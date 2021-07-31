binom_exact.test <- binom.test(26,30,.7, alternative = "greater")
binom_exact.test$p.value*2

binom.test(224,350,.64, alternative = "two.sided")

binom_exact.test <- binom.test(8,15,.5333333, alternative = "greater")
binom_exact.test$p.value*2

binom.test(7,15,.7, alternative = "two.sided")

prop.test(8,15,.70, alternative = "less",correct = TRUE)
binom.test(x = 8, n = 15, .7)

binom.test(23,25,.75, alternative = "greater")
binom.test(8,15,.5333, alternative = "greater")

pbinom(8, size = 15, prob = .7)
pbinom(8, size = 15, prob = .7)*2


#.1311
#.2623
