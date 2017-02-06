# adapted from Zachary Brownstein's solution
load(clintonaddresses.RData)
(matches <- regmatches(clintonaddresses, gregexpr("\\w*terroris[tm]\\w*", clintonaddresses)))
lengths(matches)