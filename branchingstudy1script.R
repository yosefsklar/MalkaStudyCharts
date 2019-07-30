#load the YC 2-wave panel survey data file and coding file

#brsurvey<-read.csv(file='c:\\Users/Ariel Malka/Dropbox/All Documents/Papers, etc/Survey context-wording/Branching - start 2016/YU combined data files and code/yubranchfull.csv', sep = ',', header=T)

#brcode<-read.csv(file='c:\\Users/Ariel Malka/Dropbox/All Documents/Papers, etc/Survey context-wording/Branching - start 2016/YU combined data files and code/yubranchcoding.csv', header=T)
rm(list = ls())
brsurvey<-read.csv(file="C:/Users/yosef/Downloads/Psyc Classes/Malka Summer Work/yubranchfull.csv", sep = ',', header=T)

brcode<-read.csv(file="C:/Users/yosef/Downloads/Psyc Classes/Malka Summer Work/yubranchcoding.csv", header=T)

library(psych)

#change variable name for participant id number

brsurvey$cumulid<-brsurvey$ï..cumulid

brcode$cumulid<-brcode$ï..cumulid


#merge brsurvey and brcode

brfull<-merge(brsurvey, brcode, by="cumulid")


#useable for wave 2 varoab;e, exclude one t2 subject who didn't do same condition survey

brfull$usew2[brfull$condt1==brfull$condt2]<-1
brfull$usew2[brfull$condt1!=brfull$condt2]<-0
brfull$usew2[is.na(brfull$condt2)]<-0


#compute non-branching condition attitude measures for waves 1 and 2

brfull$golannont1<-1-((brfull$golan_non_t1-1)/6)
brfull$chopchopnont1<-1-((brfull$chopchop_non_t1-1)/6)
brfull$psychnont1<-1-((brfull$psych_non_t1-1)/6)
brfull$followpolnont1<-1-((brfull$followpol_non_t1-1)/6)
brfull$moviesnont1<-1-((brfull$movies_non_t1-1)/6)
brfull$restnont1<-1-((brfull$rest_non_t1-1)/6)
brfull$nflnont1<-1-((brfull$nfl_non_t1-1)/6)
brfull$clothesnont1<-1-((brfull$clothes_non_t1-1)/6)
brfull$ycnont1<-1-((brfull$yc_non_t1-1)/6)
brfull$starwarsnont1<-1-((brfull$starwars_non_t1-1)/6)

brfull$golannont2<-1-((brfull$golan_non_t2-1)/6)
brfull$chopchopnont2<-1-((brfull$chopchop_non_t2-1)/6)
brfull$psychnont2<-1-((brfull$psych_non_t2-1)/6)
brfull$followpolnont2<-1-((brfull$followpol_non_t2-1)/6)
brfull$moviesnont2<-1-((brfull$movies_non_t2-1)/6)
brfull$restnont2<-1-((brfull$rest_non_t2-1)/6)
brfull$nflnont2<-1-((brfull$nfl_non_t2-1)/6)
brfull$clothesnont2<-1-((brfull$clothes_non_t2-1)/6)
brfull$ycnont2<-1-((brfull$yc_non_t2-1)/6)
brfull$starwarsnont2<-1-((brfull$starwars_non_t2-1)/6)

#compute branching condition attitude measures 7pt-no lean for waves 1 and 2

brfull$golanbr7noleant1[brfull$golan_branchlike_t1==3]<-1
brfull$golanbr7noleant1[brfull$golan_branchlike_t1==2]<-5/6
brfull$golanbr7noleant1[brfull$golan_branchlike_t1==1]<-2/3
brfull$golanbr7noleant1[brfull$golan_branchinitial_t1==3]<-.5
brfull$golanbr7noleant1[brfull$golan_branchdislike_t1==1]<-1/3
brfull$golanbr7noleant1[brfull$golan_branchdislike_t1==2]<-1/6
brfull$golanbr7noleant1[brfull$golan_branchdislike_t1==3]<-0

brfull$chopchopbr7noleant1[brfull$chopchop_branchlike_t1==3]<-1
brfull$chopchopbr7noleant1[brfull$chopchop_branchlike_t1==2]<-5/6
brfull$chopchopbr7noleant1[brfull$chopchop_branchlike_t1==1]<-2/3
brfull$chopchopbr7noleant1[brfull$chopchop_branchinitial_t1==3]<-.5
brfull$chopchopbr7noleant1[brfull$chopchop_branchdislike_t1==1]<-1/3
brfull$chopchopbr7noleant1[brfull$chopchop_branchdislike_t1==2]<-1/6
brfull$chopchopbr7noleant1[brfull$chopchop_branchdislike_t1==3]<-0

brfull$psychbr7noleant1[brfull$psych_branchlike_t1==3]<-1
brfull$psychbr7noleant1[brfull$psych_branchlike_t1==2]<-5/6
brfull$psychbr7noleant1[brfull$psych_branchlike_t1==1]<-2/3
brfull$psychbr7noleant1[brfull$psych_branchinitial_t1==3]<-.5
brfull$psychbr7noleant1[brfull$psych_branchdislike_t1==1]<-1/3
brfull$psychbr7noleant1[brfull$psych_branchdislike_t1==2]<-1/6
brfull$psychbr7noleant1[brfull$psych_branchdislike_t1==3]<-0

brfull$followpolbr7noleant1[brfull$followpol_branchlike_t1==3]<-1
brfull$followpolbr7noleant1[brfull$followpol_branchlike_t1==2]<-5/6
brfull$followpolbr7noleant1[brfull$followpol_branchlike_t1==1]<-2/3
brfull$followpolbr7noleant1[brfull$followpol_branchinitial_t1==3]<-.5
brfull$followpolbr7noleant1[brfull$followpol_branchdislike_t1==1]<-1/3
brfull$followpolbr7noleant1[brfull$followpol_branchdislike_t1==2]<-1/6
brfull$followpolbr7noleant1[brfull$followpol_branchdislike_t1==3]<-0

brfull$moviesbr7noleant1[brfull$movies_branchlike_t1==3]<-1
brfull$moviesbr7noleant1[brfull$movies_branchlike_t1==2]<-5/6
brfull$moviesbr7noleant1[brfull$movies_branchlike_t1==1]<-2/3
brfull$moviesbr7noleant1[brfull$movies_branchinitial_t1==3]<-.5
brfull$moviesbr7noleant1[brfull$movies_branchdislike_t1==1]<-1/3
brfull$moviesbr7noleant1[brfull$movies_branchdislike_t1==2]<-1/6
brfull$moviesbr7noleant1[brfull$movies_branchdislike_t1==3]<-0

brfull$restbr7noleant1[brfull$rest_branchlike_t1==3]<-1
brfull$restbr7noleant1[brfull$rest_branchlike_t1==2]<-5/6
brfull$restbr7noleant1[brfull$rest_branchlike_t1==1]<-2/3
brfull$restbr7noleant1[brfull$rest_branchinitial_t1==3]<-.5
brfull$restbr7noleant1[brfull$rest_branchdislike_t1==1]<-1/3
brfull$restbr7noleant1[brfull$rest_branchdislike_t1==2]<-1/6
brfull$restbr7noleant1[brfull$rest_branchdislike_t1==3]<-0

brfull$nflbr7noleant1[brfull$nfl_branchlike_t1==3]<-1
brfull$nflbr7noleant1[brfull$nfl_branchlike_t1==2]<-5/6
brfull$nflbr7noleant1[brfull$nfl_branchlike_t1==1]<-2/3
brfull$nflbr7noleant1[brfull$nfl_branchinitial_t1==3]<-.5
brfull$nflbr7noleant1[brfull$nfl_branchdislike_t1==1]<-1/3
brfull$nflbr7noleant1[brfull$nfl_branchdislike_t1==2]<-1/6
brfull$nflbr7noleant1[brfull$nfl_branchdislike_t1==3]<-0

brfull$clothesbr7noleant1[brfull$clothes_branchlike_t1==3]<-1
brfull$clothesbr7noleant1[brfull$clothes_branchlike_t1==2]<-5/6
brfull$clothesbr7noleant1[brfull$clothes_branchlike_t1==1]<-2/3
brfull$clothesbr7noleant1[brfull$clothes_branchinitial_t1==3]<-.5
brfull$clothesbr7noleant1[brfull$clothes_branchdislike_t1==1]<-1/3
brfull$clothesbr7noleant1[brfull$clothes_branchdislike_t1==2]<-1/6
brfull$clothesbr7noleant1[brfull$clothes_branchdislike_t1==3]<-0

brfull$ycbr7noleant1[brfull$yc_branchlike_t1==3]<-1
brfull$ycbr7noleant1[brfull$yc_branchlike_t1==2]<-5/6
brfull$ycbr7noleant1[brfull$yc_branchlike_t1==1]<-2/3
brfull$ycbr7noleant1[brfull$yc_branchinitial_t1==3]<-.5
brfull$ycbr7noleant1[brfull$yc_branchdislike_t1==1]<-1/3
brfull$ycbr7noleant1[brfull$yc_branchdislike_t1==2]<-1/6
brfull$ycbr7noleant1[brfull$yc_branchdislike_t1==3]<-0

brfull$starwarsbr7noleant1[brfull$starwars_branchlike_t1==3]<-1
brfull$starwarsbr7noleant1[brfull$starwars_branchlike_t1==2]<-5/6
brfull$starwarsbr7noleant1[brfull$starwars_branchlike_t1==1]<-2/3
brfull$starwarsbr7noleant1[brfull$starwars_branchinitial_t1==3]<-.5
brfull$starwarsbr7noleant1[brfull$starwars_branchdislike_t1==1]<-1/3
brfull$starwarsbr7noleant1[brfull$starwars_branchdislike_t1==2]<-1/6
brfull$starwarsbr7noleant1[brfull$starwars_branchdislike_t1==3]<-0

#start wave 2
brfull$golanbr7noleant2[brfull$golan_branchlike_t2==3]<-1
brfull$golanbr7noleant2[brfull$golan_branchlike_t2==2]<-5/6
brfull$golanbr7noleant2[brfull$golan_branchlike_t2==1]<-2/3
brfull$golanbr7noleant2[brfull$golan_branchinitial_t2==3]<-.5
brfull$golanbr7noleant2[brfull$golan_branchdislike_t2==1]<-1/3
brfull$golanbr7noleant2[brfull$golan_branchdislike_t2==2]<-1/6
brfull$golanbr7noleant2[brfull$golan_branchdislike_t2==3]<-0

brfull$chopchopbr7noleant2[brfull$chopchop_branchlike_t2==3]<-1
brfull$chopchopbr7noleant2[brfull$chopchop_branchlike_t2==2]<-5/6
brfull$chopchopbr7noleant2[brfull$chopchop_branchlike_t2==1]<-2/3
brfull$chopchopbr7noleant2[brfull$chopchop_branchinitial_t2==3]<-.5
brfull$chopchopbr7noleant2[brfull$chopchop_branchdislike_t2==1]<-1/3
brfull$chopchopbr7noleant2[brfull$chopchop_branchdislike_t2==2]<-1/6
brfull$chopchopbr7noleant2[brfull$chopchop_branchdislike_t2==3]<-0

brfull$psychbr7noleant2[brfull$psych_branchlike_t2==3]<-1
brfull$psychbr7noleant2[brfull$psych_branchlike_t2==2]<-5/6
brfull$psychbr7noleant2[brfull$psych_branchlike_t2==1]<-2/3
brfull$psychbr7noleant2[brfull$psych_branchinitial_t2==3]<-.5
brfull$psychbr7noleant2[brfull$psych_branchdislike_t2==1]<-1/3
brfull$psychbr7noleant2[brfull$psych_branchdislike_t2==2]<-1/6
brfull$psychbr7noleant2[brfull$psych_branchdislike_t2==3]<-0

brfull$followpolbr7noleant2[brfull$followpol_branchlike_t2==3]<-1
brfull$followpolbr7noleant2[brfull$followpol_branchlike_t2==2]<-5/6
brfull$followpolbr7noleant2[brfull$followpol_branchlike_t2==1]<-2/3
brfull$followpolbr7noleant2[brfull$followpol_branchinitial_t2==3]<-.5
brfull$followpolbr7noleant2[brfull$followpol_branchdislike_t2==1]<-1/3
brfull$followpolbr7noleant2[brfull$followpol_branchdislike_t2==2]<-1/6
brfull$followpolbr7noleant2[brfull$followpol_branchdislike_t2==3]<-0

brfull$moviesbr7noleant2[brfull$movies_branchlike_t2==3]<-1
brfull$moviesbr7noleant2[brfull$movies_branchlike_t2==2]<-5/6
brfull$moviesbr7noleant2[brfull$movies_branchlike_t2==1]<-2/3
brfull$moviesbr7noleant2[brfull$movies_branchinitial_t2==3]<-.5
brfull$moviesbr7noleant2[brfull$movies_branchdislike_t2==1]<-1/3
brfull$moviesbr7noleant2[brfull$movies_branchdislike_t2==2]<-1/6
brfull$moviesbr7noleant2[brfull$movies_branchdislike_t2==3]<-0

brfull$restbr7noleant2[brfull$rest_branchlike_t2==3]<-1
brfull$restbr7noleant2[brfull$rest_branchlike_t2==2]<-5/6
brfull$restbr7noleant2[brfull$rest_branchlike_t2==1]<-2/3
brfull$restbr7noleant2[brfull$rest_branchinitial_t2==3]<-.5
brfull$restbr7noleant2[brfull$rest_branchdislike_t2==1]<-1/3
brfull$restbr7noleant2[brfull$rest_branchdislike_t2==2]<-1/6
brfull$restbr7noleant2[brfull$rest_branchdislike_t2==3]<-0

brfull$nflbr7noleant2[brfull$nfl_branchlike_t2==3]<-1
brfull$nflbr7noleant2[brfull$nfl_branchlike_t2==2]<-5/6
brfull$nflbr7noleant2[brfull$nfl_branchlike_t2==1]<-2/3
brfull$nflbr7noleant2[brfull$nfl_branchinitial_t2==3]<-.5
brfull$nflbr7noleant2[brfull$nfl_branchdislike_t2==1]<-1/3
brfull$nflbr7noleant2[brfull$nfl_branchdislike_t2==2]<-1/6
brfull$nflbr7noleant2[brfull$nfl_branchdislike_t2==3]<-0

brfull$clothesbr7noleant2[brfull$clothes_branchlike_t2==3]<-1
brfull$clothesbr7noleant2[brfull$clothes_branchlike_t2==2]<-5/6
brfull$clothesbr7noleant2[brfull$clothes_branchlike_t2==1]<-2/3
brfull$clothesbr7noleant2[brfull$clothes_branchinitial_t2==3]<-.5
brfull$clothesbr7noleant2[brfull$clothes_branchdislike_t2==1]<-1/3
brfull$clothesbr7noleant2[brfull$clothes_branchdislike_t2==2]<-1/6
brfull$clothesbr7noleant2[brfull$clothes_branchdislike_t2==3]<-0

brfull$ycbr7noleant2[brfull$yc_branchlike_t2==3]<-1
brfull$ycbr7noleant2[brfull$yc_branchlike_t2==2]<-5/6
brfull$ycbr7noleant2[brfull$yc_branchlike_t2==1]<-2/3
brfull$ycbr7noleant2[brfull$yc_branchinitial_t2==3]<-.5
brfull$ycbr7noleant2[brfull$yc_branchdislike_t2==1]<-1/3
brfull$ycbr7noleant2[brfull$yc_branchdislike_t2==2]<-1/6
brfull$ycbr7noleant2[brfull$yc_branchdislike_t2==3]<-0

brfull$starwarsbr7noleant2[brfull$starwars_branchlike_t2==3]<-1
brfull$starwarsbr7noleant2[brfull$starwars_branchlike_t2==2]<-5/6
brfull$starwarsbr7noleant2[brfull$starwars_branchlike_t2==1]<-2/3
brfull$starwarsbr7noleant2[brfull$starwars_branchinitial_t2==3]<-.5
brfull$starwarsbr7noleant2[brfull$starwars_branchdislike_t2==1]<-1/3
brfull$starwarsbr7noleant2[brfull$starwars_branchdislike_t2==2]<-1/6
brfull$starwarsbr7noleant2[brfull$starwars_branchdislike_t2==3]<-0


#compute branching condition attitude measures 7pt- Lean for waves 1 and 2

brfull$golanbr7leant1[brfull$golan_branchlike_t1==3]<-1
brfull$golanbr7leant1[brfull$golan_branchlike_t1==2]<-5/6
brfull$golanbr7leant1[brfull$golan_branchlike_t1==1]<-2/3
brfull$golanbr7leant1[brfull$golan_branchneither_t1==1]<-2/3
brfull$golanbr7leant1[brfull$golan_branchneither_t1==3]<-.5
brfull$golanbr7leant1[brfull$golan_branchneither_t1==2]<-1/3
brfull$golanbr7leant1[brfull$golan_branchdislike_t1==1]<-1/3
brfull$golanbr7leant1[brfull$golan_branchdislike_t1==2]<-1/6
brfull$golanbr7leant1[brfull$golan_branchdislike_t1==3]<-0

brfull$chopchopbr7leant1[brfull$chopchop_branchlike_t1==3]<-1
brfull$chopchopbr7leant1[brfull$chopchop_branchlike_t1==2]<-5/6
brfull$chopchopbr7leant1[brfull$chopchop_branchlike_t1==1]<-2/3
brfull$chopchopbr7leant1[brfull$chopchop_branchneither_t1==1]<-2/3
brfull$chopchopbr7leant1[brfull$chopchop_branchneither_t1==3]<-.5
brfull$chopchopbr7leant1[brfull$chopchop_branchneither_t1==2]<-1/3
brfull$chopchopbr7leant1[brfull$chopchop_branchdislike_t1==1]<-1/3
brfull$chopchopbr7leant1[brfull$chopchop_branchdislike_t1==2]<-1/6
brfull$chopchopbr7leant1[brfull$chopchop_branchdislike_t1==3]<-0

brfull$psychbr7leant1[brfull$psych_branchlike_t1==3]<-1
brfull$psychbr7leant1[brfull$psych_branchlike_t1==2]<-5/6
brfull$psychbr7leant1[brfull$psych_branchlike_t1==1]<-2/3
brfull$psychbr7leant1[brfull$psych_branchneither_t1==1]<-2/3
brfull$psychbr7leant1[brfull$psych_branchneither_t1==3]<-.5
brfull$psychbr7leant1[brfull$psych_branchneither_t1==2]<-1/3
brfull$psychbr7leant1[brfull$psych_branchdislike_t1==1]<-1/3
brfull$psychbr7leant1[brfull$psych_branchdislike_t1==2]<-1/6
brfull$psychbr7leant1[brfull$psych_branchdislike_t1==3]<-0

brfull$followpolbr7leant1[brfull$followpol_branchlike_t1==3]<-1
brfull$followpolbr7leant1[brfull$followpol_branchlike_t1==2]<-5/6
brfull$followpolbr7leant1[brfull$followpol_branchlike_t1==1]<-2/3
brfull$followpolbr7leant1[brfull$followpol_branchneither_t1==1]<-2/3
brfull$followpolbr7leant1[brfull$followpol_branchneither_t1==3]<-.5
brfull$followpolbr7leant1[brfull$followpol_branchneither_t1==2]<-1/3
brfull$followpolbr7leant1[brfull$followpol_branchdislike_t1==1]<-1/3
brfull$followpolbr7leant1[brfull$followpol_branchdislike_t1==2]<-1/6
brfull$followpolbr7leant1[brfull$followpol_branchdislike_t1==3]<-0

brfull$moviesbr7leant1[brfull$movies_branchlike_t1==3]<-1
brfull$moviesbr7leant1[brfull$movies_branchlike_t1==2]<-5/6
brfull$moviesbr7leant1[brfull$movies_branchlike_t1==1]<-2/3
brfull$moviesbr7leant1[brfull$movies_branchneither_t1==1]<-2/3
brfull$moviesbr7leant1[brfull$movies_branchneither_t1==3]<-.5
brfull$moviesbr7leant1[brfull$movies_branchneither_t1==2]<-1/3
brfull$moviesbr7leant1[brfull$movies_branchdislike_t1==1]<-1/3
brfull$moviesbr7leant1[brfull$movies_branchdislike_t1==2]<-1/6
brfull$moviesbr7leant1[brfull$movies_branchdislike_t1==3]<-0

brfull$restbr7leant1[brfull$rest_branchlike_t1==3]<-1
brfull$restbr7leant1[brfull$rest_branchlike_t1==2]<-5/6
brfull$restbr7leant1[brfull$rest_branchlike_t1==1]<-2/3
brfull$restbr7leant1[brfull$rest_branchneither_t1==1]<-2/3
brfull$restbr7leant1[brfull$rest_branchneither_t1==3]<-.5
brfull$restbr7leant1[brfull$rest_branchneither_t1==2]<-1/3
brfull$restbr7leant1[brfull$rest_branchdislike_t1==1]<-1/3
brfull$restbr7leant1[brfull$rest_branchdislike_t1==2]<-1/6
brfull$restbr7leant1[brfull$rest_branchdislike_t1==3]<-0

brfull$nflbr7leant1[brfull$nfl_branchlike_t1==3]<-1
brfull$nflbr7leant1[brfull$nfl_branchlike_t1==2]<-5/6
brfull$nflbr7leant1[brfull$nfl_branchlike_t1==1]<-2/3
brfull$nflbr7leant1[brfull$nfl_branchneither_t1==1]<-2/3
brfull$nflbr7leant1[brfull$nfl_branchneither_t1==3]<-.5
brfull$nflbr7leant1[brfull$nfl_branchneither_t1==2]<-1/3
brfull$nflbr7leant1[brfull$nfl_branchdislike_t1==1]<-1/3
brfull$nflbr7leant1[brfull$nfl_branchdislike_t1==2]<-1/6
brfull$nflbr7leant1[brfull$nfl_branchdislike_t1==3]<-0

brfull$clothesbr7leant1[brfull$clothes_branchlike_t1==3]<-1
brfull$clothesbr7leant1[brfull$clothes_branchlike_t1==2]<-5/6
brfull$clothesbr7leant1[brfull$clothes_branchlike_t1==1]<-2/3
brfull$clothesbr7leant1[brfull$clothes_branchneither_t1==1]<-2/3
brfull$clothesbr7leant1[brfull$clothes_branchneither_t1==3]<-.5
brfull$clothesbr7leant1[brfull$clothes_branchneither_t1==2]<-1/3
brfull$clothesbr7leant1[brfull$clothes_branchdislike_t1==1]<-1/3
brfull$clothesbr7leant1[brfull$clothes_branchdislike_t1==2]<-1/6
brfull$clothesbr7leant1[brfull$clothes_branchdislike_t1==3]<-0

brfull$ycbr7leant1[brfull$yc_branchlike_t1==3]<-1
brfull$ycbr7leant1[brfull$yc_branchlike_t1==2]<-5/6
brfull$ycbr7leant1[brfull$yc_branchlike_t1==1]<-2/3
brfull$ycbr7leant1[brfull$yc_branchneither_t1==1]<-2/3
brfull$ycbr7leant1[brfull$yc_branchneither_t1==3]<-.5
brfull$ycbr7leant1[brfull$yc_branchneither_t1==2]<-1/3
brfull$ycbr7leant1[brfull$yc_branchdislike_t1==1]<-1/3
brfull$ycbr7leant1[brfull$yc_branchdislike_t1==2]<-1/6
brfull$ycbr7leant1[brfull$yc_branchdislike_t1==3]<-0

brfull$starwarsbr7leant1[brfull$starwars_branchlike_t1==3]<-1
brfull$starwarsbr7leant1[brfull$starwars_branchlike_t1==2]<-5/6
brfull$starwarsbr7leant1[brfull$starwars_branchlike_t1==1]<-2/3
brfull$starwarsbr7leant1[brfull$starwars_branchneither_t1==1]<-2/3
brfull$starwarsbr7leant1[brfull$starwars_branchneither_t1==3]<-.5
brfull$starwarsbr7leant1[brfull$starwars_branchneither_t1==2]<-1/3
brfull$starwarsbr7leant1[brfull$starwars_branchdislike_t1==1]<-1/3
brfull$starwarsbr7leant1[brfull$starwars_branchdislike_t1==2]<-1/6
brfull$starwarsbr7leant1[brfull$starwars_branchdislike_t1==3]<-0

#start wave 2


brfull$golanbr7leant2[brfull$golan_branchlike_t2==3]<-1
brfull$golanbr7leant2[brfull$golan_branchlike_t2==2]<-5/6
brfull$golanbr7leant2[brfull$golan_branchlike_t2==1]<-2/3
brfull$golanbr7leant2[brfull$golan_branchneither_t2==1]<-2/3
brfull$golanbr7leant2[brfull$golan_branchneither_t2==3]<-.5
brfull$golanbr7leant2[brfull$golan_branchneither_t2==2]<-1/3
brfull$golanbr7leant2[brfull$golan_branchdislike_t2==1]<-1/3
brfull$golanbr7leant2[brfull$golan_branchdislike_t2==2]<-1/6
brfull$golanbr7leant2[brfull$golan_branchdislike_t2==3]<-0

brfull$chopchopbr7leant2[brfull$chopchop_branchlike_t2==3]<-1
brfull$chopchopbr7leant2[brfull$chopchop_branchlike_t2==2]<-5/6
brfull$chopchopbr7leant2[brfull$chopchop_branchlike_t2==1]<-2/3
brfull$chopchopbr7leant2[brfull$chopchop_branchneither_t2==1]<-2/3
brfull$chopchopbr7leant2[brfull$chopchop_branchneither_t2==3]<-.5
brfull$chopchopbr7leant2[brfull$chopchop_branchneither_t2==2]<-1/3
brfull$chopchopbr7leant2[brfull$chopchop_branchdislike_t2==1]<-1/3
brfull$chopchopbr7leant2[brfull$chopchop_branchdislike_t2==2]<-1/6
brfull$chopchopbr7leant2[brfull$chopchop_branchdislike_t2==3]<-0

brfull$psychbr7leant2[brfull$psych_branchlike_t2==3]<-1
brfull$psychbr7leant2[brfull$psych_branchlike_t2==2]<-5/6
brfull$psychbr7leant2[brfull$psych_branchlike_t2==1]<-2/3
brfull$psychbr7leant2[brfull$psych_branchneither_t2==1]<-2/3
brfull$psychbr7leant2[brfull$psych_branchneither_t2==3]<-.5
brfull$psychbr7leant2[brfull$psych_branchneither_t2==2]<-1/3
brfull$psychbr7leant2[brfull$psych_branchdislike_t2==1]<-1/3
brfull$psychbr7leant2[brfull$psych_branchdislike_t2==2]<-1/6
brfull$psychbr7leant2[brfull$psych_branchdislike_t2==3]<-0

brfull$followpolbr7leant2[brfull$followpol_branchlike_t2==3]<-1
brfull$followpolbr7leant2[brfull$followpol_branchlike_t2==2]<-5/6
brfull$followpolbr7leant2[brfull$followpol_branchlike_t2==1]<-2/3
brfull$followpolbr7leant2[brfull$followpol_branchneither_t2==1]<-2/3
brfull$followpolbr7leant2[brfull$followpol_branchneither_t2==3]<-.5
brfull$followpolbr7leant2[brfull$followpol_branchneither_t2==2]<-1/3
brfull$followpolbr7leant2[brfull$followpol_branchdislike_t2==1]<-1/3
brfull$followpolbr7leant2[brfull$followpol_branchdislike_t2==2]<-1/6
brfull$followpolbr7leant2[brfull$followpol_branchdislike_t2==3]<-0

brfull$moviesbr7leant2[brfull$movies_branchlike_t2==3]<-1
brfull$moviesbr7leant2[brfull$movies_branchlike_t2==2]<-5/6
brfull$moviesbr7leant2[brfull$movies_branchlike_t2==1]<-2/3
brfull$moviesbr7leant2[brfull$movies_branchneither_t2==1]<-2/3
brfull$moviesbr7leant2[brfull$movies_branchneither_t2==3]<-.5
brfull$moviesbr7leant2[brfull$movies_branchneither_t2==2]<-1/3
brfull$moviesbr7leant2[brfull$movies_branchdislike_t2==1]<-1/3
brfull$moviesbr7leant2[brfull$movies_branchdislike_t2==2]<-1/6
brfull$moviesbr7leant2[brfull$movies_branchdislike_t2==3]<-0

brfull$restbr7leant2[brfull$rest_branchlike_t2==3]<-1
brfull$restbr7leant2[brfull$rest_branchlike_t2==2]<-5/6
brfull$restbr7leant2[brfull$rest_branchlike_t2==1]<-2/3
brfull$restbr7leant2[brfull$rest_branchneither_t2==1]<-2/3
brfull$restbr7leant2[brfull$rest_branchneither_t2==3]<-.5
brfull$restbr7leant2[brfull$rest_branchneither_t2==2]<-1/3
brfull$restbr7leant2[brfull$rest_branchdislike_t2==1]<-1/3
brfull$restbr7leant2[brfull$rest_branchdislike_t2==2]<-1/6
brfull$restbr7leant2[brfull$rest_branchdislike_t2==3]<-0

brfull$nflbr7leant2[brfull$nfl_branchlike_t2==3]<-1
brfull$nflbr7leant2[brfull$nfl_branchlike_t2==2]<-5/6
brfull$nflbr7leant2[brfull$nfl_branchlike_t2==1]<-2/3
brfull$nflbr7leant2[brfull$nfl_branchneither_t2==1]<-2/3
brfull$nflbr7leant2[brfull$nfl_branchneither_t2==3]<-.5
brfull$nflbr7leant2[brfull$nfl_branchneither_t2==2]<-1/3
brfull$nflbr7leant2[brfull$nfl_branchdislike_t2==1]<-1/3
brfull$nflbr7leant2[brfull$nfl_branchdislike_t2==2]<-1/6
brfull$nflbr7leant2[brfull$nfl_branchdislike_t2==3]<-0

brfull$clothesbr7leant2[brfull$clothes_branchlike_t2==3]<-1
brfull$clothesbr7leant2[brfull$clothes_branchlike_t2==2]<-5/6
brfull$clothesbr7leant2[brfull$clothes_branchlike_t2==1]<-2/3
brfull$clothesbr7leant2[brfull$clothes_branchneither_t2==1]<-2/3
brfull$clothesbr7leant2[brfull$clothes_branchneither_t2==3]<-.5
brfull$clothesbr7leant2[brfull$clothes_branchneither_t2==2]<-1/3
brfull$clothesbr7leant2[brfull$clothes_branchdislike_t2==1]<-1/3
brfull$clothesbr7leant2[brfull$clothes_branchdislike_t2==2]<-1/6
brfull$clothesbr7leant2[brfull$clothes_branchdislike_t2==3]<-0

brfull$ycbr7leant2[brfull$yc_branchlike_t2==3]<-1
brfull$ycbr7leant2[brfull$yc_branchlike_t2==2]<-5/6
brfull$ycbr7leant2[brfull$yc_branchlike_t2==1]<-2/3
brfull$ycbr7leant2[brfull$yc_branchneither_t2==1]<-2/3
brfull$ycbr7leant2[brfull$yc_branchneither_t2==3]<-.5
brfull$ycbr7leant2[brfull$yc_branchneither_t2==2]<-1/3
brfull$ycbr7leant2[brfull$yc_branchdislike_t2==1]<-1/3
brfull$ycbr7leant2[brfull$yc_branchdislike_t2==2]<-1/6
brfull$ycbr7leant2[brfull$yc_branchdislike_t2==3]<-0

brfull$starwarsbr7leant2[brfull$starwars_branchlike_t2==3]<-1
brfull$starwarsbr7leant2[brfull$starwars_branchlike_t2==2]<-5/6
brfull$starwarsbr7leant2[brfull$starwars_branchlike_t2==1]<-2/3
brfull$starwarsbr7leant2[brfull$starwars_branchneither_t2==1]<-2/3
brfull$starwarsbr7leant2[brfull$starwars_branchneither_t2==3]<-.5
brfull$starwarsbr7leant2[brfull$starwars_branchneither_t2==2]<-1/3
brfull$starwarsbr7leant2[brfull$starwars_branchdislike_t2==1]<-1/3
brfull$starwarsbr7leant2[brfull$starwars_branchdislike_t2==2]<-1/6
brfull$starwarsbr7leant2[brfull$starwars_branchdislike_t2==3]<-0


#compute branching condition attitude measures 9pt- Lean for waves 1 and 2

brfull$golanbr9leant1[brfull$golan_branchlike_t1==3]<-1
brfull$golanbr9leant1[brfull$golan_branchlike_t1==2]<-.875
brfull$golanbr9leant1[brfull$golan_branchlike_t1==1]<-.75
brfull$golanbr9leant1[brfull$golan_branchneither_t1==1]<-.625
brfull$golanbr9leant1[brfull$golan_branchneither_t1==3]<-.5
brfull$golanbr9leant1[brfull$golan_branchneither_t1==2]<-.375
brfull$golanbr9leant1[brfull$golan_branchdislike_t1==1]<-.25
brfull$golanbr9leant1[brfull$golan_branchdislike_t1==2]<-.125
brfull$golanbr9leant1[brfull$golan_branchdislike_t1==3]<-0

brfull$chopchopbr9leant1[brfull$chopchop_branchlike_t1==3]<-1
brfull$chopchopbr9leant1[brfull$chopchop_branchlike_t1==2]<-.875
brfull$chopchopbr9leant1[brfull$chopchop_branchlike_t1==1]<-.75
brfull$chopchopbr9leant1[brfull$chopchop_branchneither_t1==1]<-.625
brfull$chopchopbr9leant1[brfull$chopchop_branchneither_t1==3]<-.5
brfull$chopchopbr9leant1[brfull$chopchop_branchneither_t1==2]<-.375
brfull$chopchopbr9leant1[brfull$chopchop_branchdislike_t1==1]<-.25
brfull$chopchopbr9leant1[brfull$chopchop_branchdislike_t1==2]<-.125
brfull$chopchopbr9leant1[brfull$chopchop_branchdislike_t1==3]<-0

brfull$psychbr9leant1[brfull$psych_branchlike_t1==3]<-1
brfull$psychbr9leant1[brfull$psych_branchlike_t1==2]<-.875
brfull$psychbr9leant1[brfull$psych_branchlike_t1==1]<-.75
brfull$psychbr9leant1[brfull$psych_branchneither_t1==1]<-.625
brfull$psychbr9leant1[brfull$psych_branchneither_t1==3]<-.5
brfull$psychbr9leant1[brfull$psych_branchneither_t1==2]<-.375
brfull$psychbr9leant1[brfull$psych_branchdislike_t1==1]<-.25
brfull$psychbr9leant1[brfull$psych_branchdislike_t1==2]<-.125
brfull$psychbr9leant1[brfull$psych_branchdislike_t1==3]<-0

brfull$followpolbr9leant1[brfull$followpol_branchlike_t1==3]<-1
brfull$followpolbr9leant1[brfull$followpol_branchlike_t1==2]<-.875
brfull$followpolbr9leant1[brfull$followpol_branchlike_t1==1]<-.75
brfull$followpolbr9leant1[brfull$followpol_branchneither_t1==1]<-.625
brfull$followpolbr9leant1[brfull$followpol_branchneither_t1==3]<-.5
brfull$followpolbr9leant1[brfull$followpol_branchneither_t1==2]<-.375
brfull$followpolbr9leant1[brfull$followpol_branchdislike_t1==1]<-.25
brfull$followpolbr9leant1[brfull$followpol_branchdislike_t1==2]<-.125
brfull$followpolbr9leant1[brfull$followpol_branchdislike_t1==3]<-0

brfull$moviesbr9leant1[brfull$movies_branchlike_t1==3]<-1
brfull$moviesbr9leant1[brfull$movies_branchlike_t1==2]<-.875
brfull$moviesbr9leant1[brfull$movies_branchlike_t1==1]<-.75
brfull$moviesbr9leant1[brfull$movies_branchneither_t1==1]<-.625
brfull$moviesbr9leant1[brfull$movies_branchneither_t1==3]<-.5
brfull$moviesbr9leant1[brfull$movies_branchneither_t1==2]<-.375
brfull$moviesbr9leant1[brfull$movies_branchdislike_t1==1]<-.25
brfull$moviesbr9leant1[brfull$movies_branchdislike_t1==2]<-.125
brfull$moviesbr9leant1[brfull$movies_branchdislike_t1==3]<-0

brfull$restbr9leant1[brfull$rest_branchlike_t1==3]<-1
brfull$restbr9leant1[brfull$rest_branchlike_t1==2]<-.875
brfull$restbr9leant1[brfull$rest_branchlike_t1==1]<-.75
brfull$restbr9leant1[brfull$rest_branchneither_t1==1]<-.625
brfull$restbr9leant1[brfull$rest_branchneither_t1==3]<-.5
brfull$restbr9leant1[brfull$rest_branchneither_t1==2]<-.375
brfull$restbr9leant1[brfull$rest_branchdislike_t1==1]<-.25
brfull$restbr9leant1[brfull$rest_branchdislike_t1==2]<-.125
brfull$restbr9leant1[brfull$rest_branchdislike_t1==3]<-0

brfull$nflbr9leant1[brfull$nfl_branchlike_t1==3]<-1
brfull$nflbr9leant1[brfull$nfl_branchlike_t1==2]<-.875
brfull$nflbr9leant1[brfull$nfl_branchlike_t1==1]<-.75
brfull$nflbr9leant1[brfull$nfl_branchneither_t1==1]<-.625
brfull$nflbr9leant1[brfull$nfl_branchneither_t1==3]<-.5
brfull$nflbr9leant1[brfull$nfl_branchneither_t1==2]<-.375
brfull$nflbr9leant1[brfull$nfl_branchdislike_t1==1]<-.25
brfull$nflbr9leant1[brfull$nfl_branchdislike_t1==2]<-.125
brfull$nflbr9leant1[brfull$nfl_branchdislike_t1==3]<-0

brfull$clothesbr9leant1[brfull$clothes_branchlike_t1==3]<-1
brfull$clothesbr9leant1[brfull$clothes_branchlike_t1==2]<-.875
brfull$clothesbr9leant1[brfull$clothes_branchlike_t1==1]<-.75
brfull$clothesbr9leant1[brfull$clothes_branchneither_t1==1]<-.625
brfull$clothesbr9leant1[brfull$clothes_branchneither_t1==3]<-.5
brfull$clothesbr9leant1[brfull$clothes_branchneither_t1==2]<-.375
brfull$clothesbr9leant1[brfull$clothes_branchdislike_t1==1]<-.25
brfull$clothesbr9leant1[brfull$clothes_branchdislike_t1==2]<-.125
brfull$clothesbr9leant1[brfull$clothes_branchdislike_t1==3]<-0

brfull$ycbr9leant1[brfull$yc_branchlike_t1==3]<-1
brfull$ycbr9leant1[brfull$yc_branchlike_t1==2]<-.875
brfull$ycbr9leant1[brfull$yc_branchlike_t1==1]<-.75
brfull$ycbr9leant1[brfull$yc_branchneither_t1==1]<-.625
brfull$ycbr9leant1[brfull$yc_branchneither_t1==3]<-.5
brfull$ycbr9leant1[brfull$yc_branchneither_t1==2]<-.375
brfull$ycbr9leant1[brfull$yc_branchdislike_t1==1]<-.25
brfull$ycbr9leant1[brfull$yc_branchdislike_t1==2]<-.125
brfull$ycbr9leant1[brfull$yc_branchdislike_t1==3]<-0

brfull$starwarsbr9leant1[brfull$starwars_branchlike_t1==3]<-1
brfull$starwarsbr9leant1[brfull$starwars_branchlike_t1==2]<-.875
brfull$starwarsbr9leant1[brfull$starwars_branchlike_t1==1]<-.75
brfull$starwarsbr9leant1[brfull$starwars_branchneither_t1==1]<-.625
brfull$starwarsbr9leant1[brfull$starwars_branchneither_t1==3]<-.5
brfull$starwarsbr9leant1[brfull$starwars_branchneither_t1==2]<-.375
brfull$starwarsbr9leant1[brfull$starwars_branchdislike_t1==1]<-.25
brfull$starwarsbr9leant1[brfull$starwars_branchdislike_t1==2]<-.125
brfull$starwarsbr9leant1[brfull$starwars_branchdislike_t1==3]<-0

#start wave 2


brfull$golanbr9leant2[brfull$golan_branchlike_t2==3]<-1
brfull$golanbr9leant2[brfull$golan_branchlike_t2==2]<-.875
brfull$golanbr9leant2[brfull$golan_branchlike_t2==1]<-.75
brfull$golanbr9leant2[brfull$golan_branchneither_t2==1]<-.625
brfull$golanbr9leant2[brfull$golan_branchneither_t2==3]<-.5
brfull$golanbr9leant2[brfull$golan_branchneither_t2==2]<-.375
brfull$golanbr9leant2[brfull$golan_branchdislike_t2==1]<-.25
brfull$golanbr9leant2[brfull$golan_branchdislike_t2==2]<-.125
brfull$golanbr9leant2[brfull$golan_branchdislike_t2==3]<-0

brfull$chopchopbr9leant2[brfull$chopchop_branchlike_t2==3]<-1
brfull$chopchopbr9leant2[brfull$chopchop_branchlike_t2==2]<-.875
brfull$chopchopbr9leant2[brfull$chopchop_branchlike_t2==1]<-.75
brfull$chopchopbr9leant2[brfull$chopchop_branchneither_t2==1]<-.625
brfull$chopchopbr9leant2[brfull$chopchop_branchneither_t2==3]<-.5
brfull$chopchopbr9leant2[brfull$chopchop_branchneither_t2==2]<-.375
brfull$chopchopbr9leant2[brfull$chopchop_branchdislike_t2==1]<-.25
brfull$chopchopbr9leant2[brfull$chopchop_branchdislike_t2==2]<-.125
brfull$chopchopbr9leant2[brfull$chopchop_branchdislike_t2==3]<-0

brfull$psychbr9leant2[brfull$psych_branchlike_t2==3]<-1
brfull$psychbr9leant2[brfull$psych_branchlike_t2==2]<-.875
brfull$psychbr9leant2[brfull$psych_branchlike_t2==1]<-.75
brfull$psychbr9leant2[brfull$psych_branchneither_t2==1]<-.625
brfull$psychbr9leant2[brfull$psych_branchneither_t2==3]<-.5
brfull$psychbr9leant2[brfull$psych_branchneither_t2==2]<-.375
brfull$psychbr9leant2[brfull$psych_branchdislike_t2==1]<-.25
brfull$psychbr9leant2[brfull$psych_branchdislike_t2==2]<-.125
brfull$psychbr9leant2[brfull$psych_branchdislike_t2==3]<-0

brfull$followpolbr9leant2[brfull$followpol_branchlike_t2==3]<-1
brfull$followpolbr9leant2[brfull$followpol_branchlike_t2==2]<-.875
brfull$followpolbr9leant2[brfull$followpol_branchlike_t2==1]<-.75
brfull$followpolbr9leant2[brfull$followpol_branchneither_t2==1]<-.625
brfull$followpolbr9leant2[brfull$followpol_branchneither_t2==3]<-.5
brfull$followpolbr9leant2[brfull$followpol_branchneither_t2==2]<-.375
brfull$followpolbr9leant2[brfull$followpol_branchdislike_t2==1]<-.25
brfull$followpolbr9leant2[brfull$followpol_branchdislike_t2==2]<-.125
brfull$followpolbr9leant2[brfull$followpol_branchdislike_t2==3]<-0

brfull$moviesbr9leant2[brfull$movies_branchlike_t2==3]<-1
brfull$moviesbr9leant2[brfull$movies_branchlike_t2==2]<-.875
brfull$moviesbr9leant2[brfull$movies_branchlike_t2==1]<-.75
brfull$moviesbr9leant2[brfull$movies_branchneither_t2==1]<-.625
brfull$moviesbr9leant2[brfull$movies_branchneither_t2==3]<-.5
brfull$moviesbr9leant2[brfull$movies_branchneither_t2==2]<-.375
brfull$moviesbr9leant2[brfull$movies_branchdislike_t2==1]<-.25
brfull$moviesbr9leant2[brfull$movies_branchdislike_t2==2]<-.125
brfull$moviesbr9leant2[brfull$movies_branchdislike_t2==3]<-0

brfull$restbr9leant2[brfull$rest_branchlike_t2==3]<-1
brfull$restbr9leant2[brfull$rest_branchlike_t2==2]<-.875
brfull$restbr9leant2[brfull$rest_branchlike_t2==1]<-.75
brfull$restbr9leant2[brfull$rest_branchneither_t2==1]<-.625
brfull$restbr9leant2[brfull$rest_branchneither_t2==3]<-.5
brfull$restbr9leant2[brfull$rest_branchneither_t2==2]<-.375
brfull$restbr9leant2[brfull$rest_branchdislike_t2==1]<-.25
brfull$restbr9leant2[brfull$rest_branchdislike_t2==2]<-.125
brfull$restbr9leant2[brfull$rest_branchdislike_t2==3]<-0

brfull$nflbr9leant2[brfull$nfl_branchlike_t2==3]<-1
brfull$nflbr9leant2[brfull$nfl_branchlike_t2==2]<-.875
brfull$nflbr9leant2[brfull$nfl_branchlike_t2==1]<-.75
brfull$nflbr9leant2[brfull$nfl_branchneither_t2==1]<-.625
brfull$nflbr9leant2[brfull$nfl_branchneither_t2==3]<-.5
brfull$nflbr9leant2[brfull$nfl_branchneither_t2==2]<-.375
brfull$nflbr9leant2[brfull$nfl_branchdislike_t2==1]<-.25
brfull$nflbr9leant2[brfull$nfl_branchdislike_t2==2]<-.125
brfull$nflbr9leant2[brfull$nfl_branchdislike_t2==3]<-0

brfull$clothesbr9leant2[brfull$clothes_branchlike_t2==3]<-1
brfull$clothesbr9leant2[brfull$clothes_branchlike_t2==2]<-.875
brfull$clothesbr9leant2[brfull$clothes_branchlike_t2==1]<-.75
brfull$clothesbr9leant2[brfull$clothes_branchneither_t2==1]<-.625
brfull$clothesbr9leant2[brfull$clothes_branchneither_t2==3]<-.5
brfull$clothesbr9leant2[brfull$clothes_branchneither_t2==2]<-.375
brfull$clothesbr9leant2[brfull$clothes_branchdislike_t2==1]<-.25
brfull$clothesbr9leant2[brfull$clothes_branchdislike_t2==2]<-.125
brfull$clothesbr9leant2[brfull$clothes_branchdislike_t2==3]<-0

brfull$ycbr9leant2[brfull$yc_branchlike_t2==3]<-1
brfull$ycbr9leant2[brfull$yc_branchlike_t2==2]<-.875
brfull$ycbr9leant2[brfull$yc_branchlike_t2==1]<-.75
brfull$ycbr9leant2[brfull$yc_branchneither_t2==1]<-.625
brfull$ycbr9leant2[brfull$yc_branchneither_t2==3]<-.5
brfull$ycbr9leant2[brfull$yc_branchneither_t2==2]<-.375
brfull$ycbr9leant2[brfull$yc_branchdislike_t2==1]<-.25
brfull$ycbr9leant2[brfull$yc_branchdislike_t2==2]<-.125
brfull$ycbr9leant2[brfull$yc_branchdislike_t2==3]<-0

brfull$starwarsbr9leant2[brfull$starwars_branchlike_t2==3]<-1
brfull$starwarsbr9leant2[brfull$starwars_branchlike_t2==2]<-.875
brfull$starwarsbr9leant2[brfull$starwars_branchlike_t2==1]<-.75
brfull$starwarsbr9leant2[brfull$starwars_branchneither_t2==1]<-.625
brfull$starwarsbr9leant2[brfull$starwars_branchneither_t2==3]<-.5
brfull$starwarsbr9leant2[brfull$starwars_branchneither_t2==2]<-.375
brfull$starwarsbr9leant2[brfull$starwars_branchdislike_t2==1]<-.25
brfull$starwarsbr9leant2[brfull$starwars_branchdislike_t2==2]<-.125

brfull$starwarsbr9leant2[brfull$starwars_branchdislike_t2==3]<-0

#compute inclusive att measures with non and 7pt NO lean

brfull$golanall7noleant1<-brfull$golannont1
brfull$golanall7noleant1[is.na(brfull$golannont1)]<-brfull$golanbr7noleant1[is.na(brfull$golannont1)]

brfull$chopchopall7noleant1<-brfull$chopchopnont1
brfull$chopchopall7noleant1[is.na(brfull$chopchopnont1)]<-brfull$chopchopbr7noleant1[is.na(brfull$chopchopnont1)]

brfull$psychall7noleant1<-brfull$psychnont1
brfull$psychall7noleant1[is.na(brfull$psychnont1)]<-brfull$psychbr7noleant1[is.na(brfull$psychnont1)]

brfull$followpolall7noleant1<-brfull$followpolnont1
brfull$followpolall7noleant1[is.na(brfull$followpolnont1)]<-brfull$followpolbr7noleant1[is.na(brfull$followpolnont1)]

brfull$moviesall7noleant1<-brfull$moviesnont1
brfull$moviesall7noleant1[is.na(brfull$moviesnont1)]<-brfull$moviesbr7noleant1[is.na(brfull$moviesnont1)]

brfull$restall7noleant1<-brfull$restnont1
brfull$restall7noleant1[is.na(brfull$restnont1)]<-brfull$restbr7noleant1[is.na(brfull$restnont1)]

brfull$nflall7noleant1<-brfull$nflnont1
brfull$nflall7noleant1[is.na(brfull$nflnont1)]<-brfull$nflbr7noleant1[is.na(brfull$nflnont1)]

brfull$clothesall7noleant1<-brfull$clothesnont1
brfull$clothesall7noleant1[is.na(brfull$clothesnont1)]<-brfull$clothesbr7noleant1[is.na(brfull$clothesnont1)]

brfull$ycall7noleant1<-brfull$ycnont1
brfull$ycall7noleant1[is.na(brfull$ycnont1)]<-brfull$ycbr7noleant1[is.na(brfull$ycnont1)]

brfull$starwarsall7noleant1<-brfull$starwarsnont1
brfull$starwarsall7noleant1[is.na(brfull$starwarsnont1)]<-brfull$starwarsbr7noleant1[is.na(brfull$starwarsnont1)]

#start wave 2

brfull$golanall7noleant2<-brfull$golannont2
brfull$golanall7noleant2[is.na(brfull$golannont2)]<-brfull$golanbr7noleant2[is.na(brfull$golannont2)]

brfull$chopchopall7noleant2<-brfull$chopchopnont2
brfull$chopchopall7noleant2[is.na(brfull$chopchopnont2)]<-brfull$chopchopbr7noleant2[is.na(brfull$chopchopnont2)]

brfull$psychall7noleant2<-brfull$psychnont2
brfull$psychall7noleant2[is.na(brfull$psychnont2)]<-brfull$psychbr7noleant2[is.na(brfull$psychnont2)]

brfull$followpolall7noleant2<-brfull$followpolnont2
brfull$followpolall7noleant2[is.na(brfull$followpolnont2)]<-brfull$followpolbr7noleant2[is.na(brfull$followpolnont2)]

brfull$moviesall7noleant2<-brfull$moviesnont2
brfull$moviesall7noleant2[is.na(brfull$moviesnont2)]<-brfull$moviesbr7noleant2[is.na(brfull$moviesnont2)]

brfull$restall7noleant2<-brfull$restnont2
brfull$restall7noleant2[is.na(brfull$restnont2)]<-brfull$restbr7noleant2[is.na(brfull$restnont2)]

brfull$nflall7noleant2<-brfull$nflnont2
brfull$nflall7noleant2[is.na(brfull$nflnont2)]<-brfull$nflbr7noleant2[is.na(brfull$nflnont2)]

brfull$clothesall7noleant2<-brfull$clothesnont2
brfull$clothesall7noleant2[is.na(brfull$clothesnont2)]<-brfull$clothesbr7noleant2[is.na(brfull$clothesnont2)]

brfull$ycall7noleant2<-brfull$ycnont2
brfull$ycall7noleant2[is.na(brfull$ycnont2)]<-brfull$ycbr7noleant2[is.na(brfull$ycnont2)]

brfull$starwarsall7noleant2<-brfull$starwarsnont2
brfull$starwarsall7noleant2[is.na(brfull$starwarsnont2)]<-brfull$starwarsbr7noleant2[is.na(brfull$starwarsnont2)]



#compute inclusive att measures with non and 7pt Lean

brfull$golanall7leant1<-brfull$golannont1
brfull$golanall7leant1[is.na(brfull$golannont1)]<-brfull$golanbr7leant1[is.na(brfull$golannont1)]

brfull$chopchopall7leant1<-brfull$chopchopnont1
brfull$chopchopall7leant1[is.na(brfull$chopchopnont1)]<-brfull$chopchopbr7leant1[is.na(brfull$chopchopnont1)]

brfull$psychall7leant1<-brfull$psychnont1
brfull$psychall7leant1[is.na(brfull$psychnont1)]<-brfull$psychbr7leant1[is.na(brfull$psychnont1)]

brfull$followpolall7leant1<-brfull$followpolnont1
brfull$followpolall7leant1[is.na(brfull$followpolnont1)]<-brfull$followpolbr7leant1[is.na(brfull$followpolnont1)]

brfull$moviesall7leant1<-brfull$moviesnont1
brfull$moviesall7leant1[is.na(brfull$moviesnont1)]<-brfull$moviesbr7leant1[is.na(brfull$moviesnont1)]

brfull$restall7leant1<-brfull$restnont1
brfull$restall7leant1[is.na(brfull$restnont1)]<-brfull$restbr7leant1[is.na(brfull$restnont1)]

brfull$nflall7leant1<-brfull$nflnont1
brfull$nflall7leant1[is.na(brfull$nflnont1)]<-brfull$nflbr7leant1[is.na(brfull$nflnont1)]

brfull$clothesall7leant1<-brfull$clothesnont1
brfull$clothesall7leant1[is.na(brfull$clothesnont1)]<-brfull$clothesbr7leant1[is.na(brfull$clothesnont1)]

brfull$ycall7leant1<-brfull$ycnont1
brfull$ycall7leant1[is.na(brfull$ycnont1)]<-brfull$ycbr7leant1[is.na(brfull$ycnont1)]

brfull$starwarsall7leant1<-brfull$starwarsnont1
brfull$starwarsall7leant1[is.na(brfull$starwarsnont1)]<-brfull$starwarsbr7leant1[is.na(brfull$starwarsnont1)]

#start wave 2

brfull$golanall7leant2<-brfull$golannont2
brfull$golanall7leant2[is.na(brfull$golannont2)]<-brfull$golanbr7leant2[is.na(brfull$golannont2)]

brfull$chopchopall7leant2<-brfull$chopchopnont2
brfull$chopchopall7leant2[is.na(brfull$chopchopnont2)]<-brfull$chopchopbr7leant2[is.na(brfull$chopchopnont2)]

brfull$psychall7leant2<-brfull$psychnont2
brfull$psychall7leant2[is.na(brfull$psychnont2)]<-brfull$psychbr7leant2[is.na(brfull$psychnont2)]

brfull$followpolall7leant2<-brfull$followpolnont2
brfull$followpolall7leant2[is.na(brfull$followpolnont2)]<-brfull$followpolbr7leant2[is.na(brfull$followpolnont2)]

brfull$moviesall7leant2<-brfull$moviesnont2
brfull$moviesall7leant2[is.na(brfull$moviesnont2)]<-brfull$moviesbr7leant2[is.na(brfull$moviesnont2)]

brfull$restall7leant2<-brfull$restnont2
brfull$restall7leant2[is.na(brfull$restnont2)]<-brfull$restbr7leant2[is.na(brfull$restnont2)]

brfull$nflall7leant2<-brfull$nflnont2
brfull$nflall7leant2[is.na(brfull$nflnont2)]<-brfull$nflbr7leant2[is.na(brfull$nflnont2)]

brfull$clothesall7leant2<-brfull$clothesnont2
brfull$clothesall7leant2[is.na(brfull$clothesnont2)]<-brfull$clothesbr7leant2[is.na(brfull$clothesnont2)]

brfull$ycall7leant2<-brfull$ycnont2
brfull$ycall7leant2[is.na(brfull$ycnont2)]<-brfull$ycbr7leant2[is.na(brfull$ycnont2)]

brfull$starwarsall7leant2<-brfull$starwarsnont2
brfull$starwarsall7leant2[is.na(brfull$starwarsnont2)]<-brfull$starwarsbr7leant2[is.na(brfull$starwarsnont2)]


#compute inclusive att measures with non and 9pt Lean

brfull$golanall9leant1<-brfull$golannont1
brfull$golanall9leant1[is.na(brfull$golannont1)]<-brfull$golanbr9leant1[is.na(brfull$golannont1)]

brfull$chopchopall9leant1<-brfull$chopchopnont1
brfull$chopchopall9leant1[is.na(brfull$chopchopnont1)]<-brfull$chopchopbr9leant1[is.na(brfull$chopchopnont1)]

brfull$psychall9leant1<-brfull$psychnont1
brfull$psychall9leant1[is.na(brfull$psychnont1)]<-brfull$psychbr9leant1[is.na(brfull$psychnont1)]

brfull$followpolall9leant1<-brfull$followpolnont1
brfull$followpolall9leant1[is.na(brfull$followpolnont1)]<-brfull$followpolbr9leant1[is.na(brfull$followpolnont1)]

brfull$moviesall9leant1<-brfull$moviesnont1
brfull$moviesall9leant1[is.na(brfull$moviesnont1)]<-brfull$moviesbr9leant1[is.na(brfull$moviesnont1)]

brfull$restall9leant1<-brfull$restnont1
brfull$restall9leant1[is.na(brfull$restnont1)]<-brfull$restbr9leant1[is.na(brfull$restnont1)]

brfull$nflall9leant1<-brfull$nflnont1
brfull$nflall9leant1[is.na(brfull$nflnont1)]<-brfull$nflbr9leant1[is.na(brfull$nflnont1)]

brfull$clothesall9leant1<-brfull$clothesnont1
brfull$clothesall9leant1[is.na(brfull$clothesnont1)]<-brfull$clothesbr9leant1[is.na(brfull$clothesnont1)]

brfull$ycall9leant1<-brfull$ycnont1
brfull$ycall9leant1[is.na(brfull$ycnont1)]<-brfull$ycbr9leant1[is.na(brfull$ycnont1)]

brfull$starwarsall9leant1<-brfull$starwarsnont1
brfull$starwarsall9leant1[is.na(brfull$starwarsnont1)]<-brfull$starwarsbr9leant1[is.na(brfull$starwarsnont1)]

#start wave 2

brfull$golanall9leant2<-brfull$golannont2
brfull$golanall9leant2[is.na(brfull$golannont2)]<-brfull$golanbr9leant2[is.na(brfull$golannont2)]

brfull$chopchopall9leant2<-brfull$chopchopnont2
brfull$chopchopall9leant2[is.na(brfull$chopchopnont2)]<-brfull$chopchopbr9leant2[is.na(brfull$chopchopnont2)]

brfull$psychall9leant2<-brfull$psychnont2
brfull$psychall9leant2[is.na(brfull$psychnont2)]<-brfull$psychbr9leant2[is.na(brfull$psychnont2)]

brfull$followpolall9leant2<-brfull$followpolnont2
brfull$followpolall9leant2[is.na(brfull$followpolnont2)]<-brfull$followpolbr9leant2[is.na(brfull$followpolnont2)]

brfull$moviesall9leant2<-brfull$moviesnont2
brfull$moviesall9leant2[is.na(brfull$moviesnont2)]<-brfull$moviesbr9leant2[is.na(brfull$moviesnont2)]

brfull$restall9leant2<-brfull$restnont2
brfull$restall9leant2[is.na(brfull$restnont2)]<-brfull$restbr9leant2[is.na(brfull$restnont2)]

brfull$nflall9leant2<-brfull$nflnont2
brfull$nflall9leant2[is.na(brfull$nflnont2)]<-brfull$nflbr9leant2[is.na(brfull$nflnont2)]

brfull$clothesall9leant2<-brfull$clothesnont2
brfull$clothesall9leant2[is.na(brfull$clothesnont2)]<-brfull$clothesbr9leant2[is.na(brfull$clothesnont2)]

brfull$ycall9leant2<-brfull$ycnont2
brfull$ycall9leant2[is.na(brfull$ycnont2)]<-brfull$ycbr9leant2[is.na(brfull$ycnont2)]

brfull$starwarsall9leant2<-brfull$starwarsnont2
brfull$starwarsall9leant2[is.na(brfull$starwarsnont2)]<-brfull$starwarsbr9leant2[is.na(brfull$starwarsnont2)]


#compute reactions to attitude measures for t1 and t2

brfull$diffundt1<-1-((brfull$difficultunderstand_t1-1)/3)
brfull$diffselectt1<-1-((brfull$difficultselect_t1-1)/3)
brfull$carereadt1<-1-((brfull$carefulread_t1-1)/3)
brfull$careanswert1<-1-((brfull$carefulanswer_t1-1)/3)
brfull$enjoyt1<-1-((brfull$enjoyable_t1-1)/3)
brfull$borerevt1<-(brfull$boring_t1-1)/3

brfull$diffundt2<-1-((brfull$difficultunderstand_t2-1)/3)
brfull$diffselectt2<-1-((brfull$difficultselect_t2-1)/3)
brfull$carereadt2<-1-((brfull$carefulread_t2-1)/3)
brfull$careanswert2<-1-((brfull$carefulanswer_t2-1)/3)
brfull$enjoyt2<-1-((brfull$enjoyable_t2-1)/3)
brfull$borerevt2<-(brfull$boring_t2-1)/3

brfull$diffcompost1<-(brfull$diffundt1+brfull$diffselectt1)/2
brfull$carecompost1<-(brfull$carereadt1+brfull$careanswert1)/2
brfull$enjoycompost1<-(brfull$enjoyt1+brfull$borerevt1)/2


brfull$diffcompost2<-(brfull$diffundt2+brfull$diffselectt2)/2
brfull$carecompost2<-(brfull$carereadt2+brfull$careanswert2)/2
brfull$enjoycompost2<-(brfull$enjoyt2+brfull$borerevt2)/2

#self-reported criterion variables (if from wave 2, only for usew2=1)

psych::describe(brfull$chopchoptypicalmonth_t2[brfull$usew2==1])
brfull$logchopchopeat<-log(brfull$chopchopeat+1)
brfull$logchopchopeat1s<-brfull$logchopchopeat/max(brfull$logchopchopeat[brfull$usew2==1], na.rm=T)
psych::describe(brfull$logchopchopeat1s[brfull$usew2==1])

psych::describe(brfull$golantypicalmonth_t2[brfull$usew2==1])
brfull$loggolaneat<-log(brfull$golaneat+1)
brfull$loggolaneat1s<-brfull$loggolaneat/max(brfull$loggolaneat[brfull$usew2==1], na.rm=T)
psych::describe(brfull$loggolaneat1s[brfull$usew2==1])


brfull$forceawakens[brfull$seenforceawakens_t1==1]<-1
brfull$forceawakens[brfull$seenforceawakens_t1==2]<-0




brfull$polconsumpt1<-(as.numeric(brfull$daysdiscusspol_t1)+as.numeric(brfull$daysreadpol_t1))/2


brfull$polconsumpt2<-(as.numeric(brfull$daysdiscusspol_t2)+as.numeric(brfull$daysreadpol_t2))/2

brfull$polconsumpt11s<-brfull$polconsumpt1/7

brfull$polconsumpt21s<-brfull$polconsumpt2/7



brfull$expectvote<-as.numeric(brfull$expectgeneralvote_t1)

brfull$expectvote1s<-brfull$expectvote/100


brfull$seepsychtalk<-as.numeric(brfull$expectseespeaker_t1)

brfull$seepsychtalk1s<-brfull$seepsychtalk/100



brfull$dinedpastmonth<-as.numeric(brfull$dinedoutpastmonth_t1)

brfull$logdinedpastmonth1s<-log(brfull$dinedpastmonth+1)/max(log(brfull$dinedpastmonth+1))


brfull$nflwatcht1<-as.numeric(brfull$nfltypicalweekwatch_t1)

brfull$lognflwatcht11s<-log(1+brfull$nflwatcht1)/max(log(1+brfull$nflwatcht1))


brfull$clothesspendt1<-as.numeric(brfull$moneyclothing_t1)

brfull$logclothesspendt11s<-log(1+brfull$clothesspendt1)/max(log(1+brfull$clothesspendt1))


brfull$clothesexpectt2<-as.numeric(brfull$moneyclothingexpect_t2)

brfull$logclothesexpectt2<-log(1+brfull$clothesexpectt2)
brfull$logclothesexpectt21s<-brfull$logclothesexpectt2/max(brfull$logclothesexpectt2[brfull$usew2==1])


brfull$ycpromote1s<-(as.numeric(brfull$promoteyc_t1))/100




#compute test-based criterion measures

brfull$nflknowl<-(brfull$nflknowl_carqb2015_t1_correct+brfull$nflknowl_cardsdivis_t1_correct+brfull$nflknowl_xtraptrule_t1_correct+brfull$nflknowl_riversteam_t1_correct+brfull$nflknowl_supwinner_t1_correct)/5


brfull$politknowl<-(brfull$politknowl_housespeaker_t1_correct+brfull$politknowl_majoritysenate_t1_correct+
                      brfull$politknowl_chiefjustice_t1_correct+brfull$politknowl_vetooverride_t1_correct+brfull$politknowl_sanderssecond_t1_correct+brfull$politknowl_obamacare_t1_correct+brfull$politknowl_fillibuster_t1_correct+brfull$politknowl_senatecontrol_t1_correct+brfull$politknowl_houseterm_t1_correct)/9


#compute need for cognition measure

brfull$nfc1<-(brfull$needcog1_t2-1)/4
brfull$nfc2<-(brfull$needcog2_t2-1)/4
brfull$nfc3<-1-((brfull$needcog3rev_t2-1)/4)
brfull$nfc4<-1-((brfull$needcog4rev_t2-1)/4)
brfull$nfc5<-1-((brfull$needcog5rev_t2-1)/4)
brfull$nfc6<-(brfull$needcog6_t2-1)/4
brfull$nfc7<-1-((brfull$needcog7rev_t2-1)/4)
brfull$nfc8<-1-((brfull$needcog8rev_t2-1)/4)
brfull$nfc9<-1-((brfull$needcog9rev_t2-1)/4)
brfull$nfc10<-(brfull$needcog10_t2-1)/4
brfull$nfc11<-(brfull$needcog11_t2-1)/4
brfull$nfc12<-1-((brfull$needcog12rev_t2-1)/4)
brfull$nfc13<-(brfull$needcog13_t2-1)/4
brfull$nfc14<-(brfull$needcog14_t2-1)/4
brfull$nfc15<-(brfull$needcog15_t2-1)/4
brfull$nfc16<-1-((brfull$needcog16rev_t2-1)/4)
brfull$nfc17<-1-((brfull$needcog17rev_t2-1)/4)
brfull$nfc18<-(brfull$needcog18_t2-1)/4

brfull$nfc<-(brfull$nfc1+brfull$nfc2+brfull$nfc3+brfull$nfc4+brfull$nfc5+brfull$nfc6+brfull$nfc7+brfull$nfc8+brfull$nfc9+brfull$nfc10+brfull$nfc11+brfull$nfc12+brfull$nfc13+brfull$nfc14+brfull$nfc15+brfull$nfc16+brfull$nfc17+brfull$nfc18)/18



#attention checks

brfull$attn1wave1<-0
brfull$attn1wave1[brfull$attnslightlike_branchlike_t1==1]<-1
brfull$attn1wave1[brfull$attnslightlike_non_t1==3]<-1

brfull$attn1wave2[brfull$int2correct==1]<-0
brfull$attn1wave2[brfull$attnslightlike_branchlike_t2==1]<-1
brfull$attn1wave2[brfull$attnslightlike_non_t2==3]<-1


brfull$attn63t1num<-as.numeric(as.character(brfull$attn63_t1))
brfull$attn63t2num<-as.numeric(as.character(brfull$attn63_t2))

brfull$attn2wave1<-0
brfull$attn2wave1[brfull$attn63t1num==63]<-1


brfull$attn2wave2[brfull$int2correct==1]<-0
brfull$attn2wave2[brfull$attn63t2num==63]<-1

brfull$attnscorew1<-(brfull$attn1wave1+brfull$attn2wave1)/2
brfull$attnscorew2<-(brfull$attn1wave2+brfull$attn2wave2)/2

#create brfull with just data from 221 useable wave 2 subjects

brfullw2use<-subset(brfull, usew2==1)

#RESULTS

#assessing inter-rater agreement for nfl and political knowledge test questions

brcode$nflknowl_teamsafceast_diff<-brcode$nflknowl_teamsafceast_t1_wein_code-brcode$nflknowl_teamsafceast_t1_schrier_code
brcode$nflknowl_teamsafceast_agree[brcode$nflknowl_teamsafceast_diff==0]<-1
brcode$nflknowl_teamsafceast_agree[brcode$nflknowl_teamsafceast_diff!=0]<-0


brcode$nflknowl_fieldgoal_diff<-brcode$nflknowl_fieldgoal_t1_wein_code-brcode$nflknowl_fieldgoal_t1_schrier_code
brcode$nflknowl_fieldgoal_agree[brcode$nflknowl_fieldgoal_diff==0]<-1
brcode$nflknowl_fieldgoal_agree[brcode$nflknowl_fieldgoal_diff!=0]<-0


brcode$nflknowl_carqb2015_diff<-brcode$nflknowl_carqb2015_t1_wein_code-brcode$nflknowl_carqb2015_t1_schrier_code
brcode$nflknowl_carqb2015_agree[brcode$nflknowl_carqb2015_diff==0]<-1
brcode$nflknowl_carqb2015_agree[brcode$nflknowl_carqb2015_diff!=0]<-0


brcode$nflknowl_cardsdivis_diff<-brcode$nflknowl_cardsdivis_t1_wein_code-brcode$nflknowl_cardsdivis_t1_schrier_code
brcode$nflknowl_cardsdivis_agree[brcode$nflknowl_cardsdivis_diff==0]<-1
brcode$nflknowl_cardsdivis_agree[brcode$nflknowl_cardsdivis_diff!=0]<-0

brcode$nflknowl_xtraptrule_diff<-brcode$nflknowl_xtraptrule_t1_wein_code-brcode$nflknowl_xtraptrule_t1_schrier_code
brcode$nflknowl_xtraptrule_agree[brcode$nflknowl_xtraptrule_diff==0]<-1
brcode$nflknowl_xtraptrule_agree[brcode$nflknowl_xtraptrule_diff!=0]<-0

brcode$nflknowl_riversteam_diff<-brcode$nflknowl_riversteam_t1_wein_code-brcode$nflknowl_riversteam_t1_schrier_code
brcode$nflknowl_riversteam_agree[brcode$nflknowl_riversteam_diff==0]<-1
brcode$nflknowl_riversteam_agree[brcode$nflknowl_riversteam_diff!=0]<-0

brcode$nflknowl_supwinner_diff<-brcode$nflknowl_supwinner_t1_wein_code-brcode$nflknowl_supwinner_t1_schrier_code
brcode$nflknowl_supwinner_agree[brcode$nflknowl_supwinner_diff==0]<-1
brcode$nflknowl_supwinner_agree[brcode$nflknowl_supwinner_diff!=0]<-0

brcode$nflknowl_jaxteamname_diff<-brcode$nflknowl_jaxteamname_t1_wein_code-brcode$nflknowl_jaxteamname_t1_schrier_code
brcode$nflknowl_jaxteamname_agree[brcode$nflknowl_jaxteamname_diff==0]<-1
brcode$nflknowl_jaxteamname_agree[brcode$nflknowl_jaxteamname_diff!=0]<-0

brcode$nflknowl_wonafceast2015_diff<-brcode$nflknowl_wonafceast2015_t1_wein_code-brcode$nflknowl_wonafceast2015_t1_schrier_code
brcode$nflknowl_wonafceast2015_agree[brcode$nflknowl_wonafceast2015_diff==0]<-1
brcode$nflknowl_wonafceast2015_agree[brcode$nflknowl_wonafceast2015_diff!=0]<-0

brcode$nflknowl_rushlead2015_diff<-brcode$nflknowl_rushlead2015_t1_wein_code-brcode$nflknowl_rushlead2015_t1_schrier_code
brcode$nflknowl_rushlead2015_agree[brcode$nflknowl_rushlead2015_diff==0]<-1
brcode$nflknowl_rushlead2015_agree[brcode$nflknowl_rushlead2015_diff!=0]<-0


brcode$nflknowl_losttopittplayoffs15_diff<-brcode$nflknowl_losttopittplayoffs15_t1_wein_code-brcode$nflknowl_losttopittplayoffs15_t1_schrier_code
brcode$nflknowl_losttopittplayoffs15_agree[brcode$nflknowl_losttopittplayoffs15_diff==0]<-1
brcode$nflknowl_losttopittplayoffs15_agree[brcode$nflknowl_losttopittplayoffs15_diff!=0]<-0



brcode$politknowl_housespeaker_diff<-brcode$politknowl_housespeaker_t1_wein_code-brcode$politknowl_housespeaker_t1_schrier_code
brcode$politknowl_housespeaker_agree[brcode$politknowl_housespeaker_diff==0]<-1
brcode$politknowl_housespeaker_agree[brcode$politknowl_housespeaker_diff!=0]<-0


brcode$politknowl_majoritysenate_diff<-brcode$politknowl_majoritysenate_t1_wein_code-brcode$politknowl_majoritysenate_t1_schrier_code
brcode$politknowl_majoritysenate_agree[brcode$politknowl_majoritysenate_diff==0]<-1
brcode$politknowl_majoritysenate_agree[brcode$politknowl_majoritysenate_diff!=0]<-0

brcode$politknowl_chiefjustice_diff<-brcode$politknowl_chiefjustice_t1_wein_code-brcode$politknowl_chiefjustice_t1_schrier_code
brcode$politknowl_chiefjustice_agree[brcode$politknowl_chiefjustice_diff==0]<-1
brcode$politknowl_chiefjustice_agree[brcode$politknowl_chiefjustice_diff!=0]<-0

brcode$politknowl_vetooverride_diff<-brcode$politknowl_vetooverride_t1_wein_code-brcode$politknowl_vetooverride_t1_schrier_code
brcode$politknowl_vetooverride_agree[brcode$politknowl_vetooverride_diff==0]<-1
brcode$politknowl_vetooverride_agree[brcode$politknowl_vetooverride_diff!=0]<-0

brcode$politknowl_sanderssecond_diff<-brcode$politknowl_sanderssecond_t1_wein_code-brcode$politknowl_sanderssecond_t1_schrier_code
brcode$politknowl_sanderssecond_agree[brcode$politknowl_sanderssecond_diff==0]<-1
brcode$politknowl_sanderssecond_agree[brcode$politknowl_sanderssecond_diff!=0]<-0

brcode$politknowl_obamacare_diff<-brcode$politknowl_obamacare_t1_wein_code-brcode$politknowl_obamacare_t1_schrier_code
brcode$politknowl_obamacare_agree[brcode$politknowl_obamacare_diff==0]<-1
brcode$politknowl_obamacare_agree[brcode$politknowl_obamacare_diff!=0]<-0

brcode$politknowl_fillibuster_diff<-brcode$politknowl_fillibuster_t1_wein_code-brcode$politknowl_fillibuster_t1_schrier_code
brcode$politknowl_fillibuster_agree[brcode$politknowl_fillibuster_diff==0]<-1
brcode$politknowl_fillibuster_agree[brcode$politknowl_fillibuster_diff!=0]<-0

brcode$politknowl_senatecontrol_diff<-brcode$politknowl_senatecontrol_t1_wein_code-brcode$politknowl_senatecontrol_t1_schrier_code
brcode$politknowl_senatecontrol_agree[brcode$politknowl_senatecontrol_diff==0]<-1
brcode$politknowl_senatecontrol_agree[brcode$politknowl_senatecontrol_diff!=0]<-0

brcode$politknowl_houseterm_diff<-brcode$politknowl_houseterm_t1_wein_code-brcode$politknowl_houseterm_t1_schrier_code
brcode$politknowl_houseterm_agree[brcode$politknowl_houseterm_diff==0]<-1
brcode$politknowl_houseterm_agree[brcode$politknowl_houseterm_diff!=0]<-0

#computing agreement percentages

mean(brcode$nflknowl_teamsafceast_agree, na.rm=T)

mean(brcode$nflknowl_fieldgoal_agree, na.rm=T)

mean(brcode$nflknowl_carqb2015_agree, na.rm=T)

mean(brcode$nflknowl_cardsdivis_agree, na.rm=T)

mean(brcode$nflknowl_xtraptrule_agree, na.rm=T)

mean(brcode$nflknowl_riversteam_agree, na.rm=T)

mean(brcode$nflknowl_supwinner_agree, na.rm=T)

mean(brcode$nflknowl_jaxteamname_agree, na.rm=T)

mean(brcode$nflknowl_wonafceast2015_agree, na.rm=T)

mean(brcode$nflknowl_rushlead2015_agree, na.rm=T)

mean(brcode$nflknowl_losttopittplayoffs15_agree, na.rm=T)


mean(brcode$politknowl_housespeaker_agree, na.rm=T)

mean(brcode$politknowl_majoritysenate_agree, na.rm=T)

mean(brcode$politknowl_chiefjustice_agree, na.rm=T)

mean(brcode$politknowl_vetooverride_agree, na.rm=T)

mean(brcode$politknowl_sanderssecond_agree, na.rm=T)

mean(brcode$politknowl_obamacare_agree, na.rm=T)

mean(brcode$politknowl_fillibuster_agree, na.rm=T)

mean(brcode$politknowl_senatecontrol_agree, na.rm=T)

mean(brcode$politknowl_houseterm_agree, na.rm=T)

#computing proportion correct for each knowledge item


mean(brfull$nflknowl_carqb2015_t1_correct, na.rm=T)
mean(brfull$nflknowl_cardsdivis_t1_correct, na.rm=T)
mean(brfull$nflknowl_xtraptrule_t1_correct, na.rm=T)
mean(brfull$nflknowl_riversteam_t1_correct, na.rm=T)
mean(brfull$nflknowl_supwinner_t1_correct, na.rm=T)

mean(brfull$politknowl_housespeaker_t1_correct, na.rm=T)
mean(brfull$politknowl_majoritysenate_t1_correct, na.rm=T)
mean(brfull$politknowl_chiefjustice_t1_correct, na.rm=T)
mean(brfull$politknowl_vetooverride_t1_correct, na.rm=T)
mean(brfull$politknowl_sanderssecond_t1_correct, na.rm=T)
mean(brfull$politknowl_obamacare_t1_correct, na.rm=T)
mean(brfull$politknowl_fillibuster_t1_correct, na.rm=T)
mean(brfull$politknowl_senatecontrol_t1_correct, na.rm=T)
mean(brfull$politknowl_houseterm_t1_correct, na.rm=T)



#alpha for nflknowledge

nflknowlvars <- c("nflknowl_carqb2015_t1_correct", "brfull$nflknowl_cardsdivis_t1_correct", "brfull$nflknowl_xtraptrule_t1_correct", "brfull$nflknowl_riversteam_t1_correct", "brfull$nflknowl_supwinner_t1_correct") 

nflknowlvarsdata <- brfull[nflknowlvars]

alpha(nflknowlvarsdata, keys=NULL,cumulative=FALSE, title=NULL, max=10,na.rm = TRUE,
      check.keys=FALSE,n.iter=1,delete=TRUE,use="pairwise",warnings=TRUE,n.obs=NULL)

#alpha for politknowledge

politknowlvars <- c("politknowl_housespeaker_t1_correct", "politknowl_majoritysenate_t1_correct", "politknowl_chiefjustice_t1_correct", "politknowl_vetooverride_t1_correct", "politknowl_sanderssecond_t1_correct", "politknowl_obamacare_t1_correct", "politknowl_fillibuster_t1_correct", "politknowl_senatecontrol_t1_correct", "politknowl_houseterm_t1_correct") 

politknowlvarsdata <- brfull[politknowlvars]

alpha(politknowlvarsdata, keys=NULL,cumulative=FALSE, title=NULL, max=10,na.rm = TRUE,
      check.keys=FALSE,n.iter=1,delete=TRUE,use="pairwise",warnings=TRUE,n.obs=NULL)


#descriptives for knowledge items


describe(brfull$nflknowl, na.rm=T)

describe(brfull$politknowl, na.rm=T)


#alpha for nfc

nfcvars <- c("nfc1", "nfc2", "nfc3", "nfc4", "nfc5", "nfc6", "nfc7", "nfc8", "nfc9", 
             "nfc10", "nfc11", "nfc12", "nfc13", "nfc14", "nfc15", "nfc16", "nfc17", "nfc18") 

nfcvarsdata <- brfull[nfcvars]

alpha(nfcvarsdata, keys=NULL,cumulative=FALSE, title=NULL, max=10,na.rm = TRUE,
      check.keys=FALSE,n.iter=1,delete=TRUE,use="pairwise",warnings=TRUE,n.obs=NULL)


# descriptives for reaction questions

describe(brfull$diffcompost1)
describe(brfull$carecompost1)
describe(brfull$enjoycompost1)


describe(brfullw2use$diffcompost2)
describe(brfullw2use$carecompost2)
describe(brfullw2use$enjoycompost2)

cor.test(formula = ~ diffundt1+diffselectt1,
         data = brfull)

cor.test(formula = ~ carereadt1+careanswert1,
         data = brfull)

cor.test(formula = ~ enjoyt1+borerevt1,
         data = brfull)



cor.test(formula = ~ diffundt2+diffselectt2,
         data = brfullw2use)

cor.test(formula = ~ carereadt2+careanswert2,
         data = brfullw2use)

cor.test(formula = ~ enjoyt2+borerevt2,
         data = brfullw2use)

#descriptives for self-reported criterion variables


psych::describe(brfullw2use$chopchopeat)
psych::describe(brfullw2use$logchopchopeat1s)


psych::describe(brfullw2use$golaneat)
psych::describe(brfullw2use$loggolaneat1s)

table(brfull$forceawakens)

psych::describe(brfull$polconsumpt11s)
psych::describe(brfullw2use$polconsumpt21s)
psych::describe(brfull$expectvote1s)


cor.test(formula = ~ as.numeric(brfull$daysdiscusspol_t1)+as.numeric(brfull$daysreadpol_t1),
         data = brfull)

cor.test(formula = ~ as.numeric(brfull$daysdiscusspol_t2)+as.numeric(brfull$daysreadpol_t2),
         data = brfullw2use)

psych::describe(brfull$seepsychtalk1s)

psych::describe(brfull$dinedpastmonth)

psych::describe(brfull$logdinedpastmonth1s)

psych::describe(nflwatcht1)

psych::describe(brfull$lognflwatcht11s)

psych::describe(brfull$clothesspendt1)

psych::describe(brfull$logclothesspendt11s)

psych::describe(brfullw2use$clothesexpectt2)

psych::describe(brfullw2use$logclothesexpectt21s)

psych::describe(brfull$ycpromote1s)


#Figure 1: analyses for forest plot of test-retest regression coefficients.

#test-retest regression coefficients for nonbranch

trreggolannon<-lm(golannont2~1+golannont1, data=brfullw2use)
b_trreggolannon<-summary(trreggolannon)$coef[2, 1]
int_trreggolannon<-confint(trreggolannon, 'golannont1', level=0.95)
low_trreggolannon<-int_trreggolannon[1,1]
up_trreggolannon<-int_trreggolannon[1,2]

trregchopchopnon<-lm(chopchopnont2~1+chopchopnont1, data=brfullw2use)
b_trregchopchopnon<-summary(trregchopchopnon)$coef[2, 1]
int_trregchopchopnon<-confint(trregchopchopnon, 'chopchopnont1', level=0.95)
low_trregchopchopnon<-int_trregchopchopnon[1,1]
up_trregchopchopnon<-int_trregchopchopnon[1,2]

trregpsychnon<-lm(psychnont2~1+psychnont1, data=brfullw2use)
b_trregpsychnon<-summary(trregpsychnon)$coef[2, 1]
int_trregpsychnon<-confint(trregpsychnon, 'psychnont1', level=0.95)
low_trregpsychnon<-int_trregpsychnon[1,1]
up_trregpsychnon<-int_trregpsychnon[1,2]

trregfollowpolnon<-lm(followpolnont2~1+followpolnont1, data=brfullw2use)
b_trregfollowpolnon<-summary(trregfollowpolnon)$coef[2, 1]
int_trregfollowpolnon<-confint(trregfollowpolnon, 'followpolnont1', level=0.95)
low_trregfollowpolnon<-int_trregfollowpolnon[1,1]
up_trregfollowpolnon<-int_trregfollowpolnon[1,2]

trregmoviesnon<-lm(moviesnont2~1+moviesnont1, data=brfullw2use)
b_trregmoviesnon<-summary(trregmoviesnon)$coef[2, 1]
int_trregmoviesnon<-confint(trregmoviesnon, 'moviesnont1', level=0.95)
low_trregmoviesnon<-int_trregmoviesnon[1,1]
up_trregmoviesnon<-int_trregmoviesnon[1,2]

trregrestnon<-lm(restnont2~1+restnont1, data=brfullw2use)
b_trregrestnon<-summary(trregrestnon)$coef[2, 1]
int_trregrestnon<-confint(trregrestnon, 'restnont1', level=0.95)
low_trregrestnon<-int_trregrestnon[1,1]
up_trregrestnon<-int_trregrestnon[1,2]

trregnflnon<-lm(nflnont2~1+nflnont1, data=brfullw2use)
b_trregnflnon<-summary(trregnflnon)$coef[2, 1]
int_trregnflnon<-confint(trregnflnon, 'nflnont1', level=0.95)
low_trregnflnon<-int_trregnflnon[1,1]
up_trregnflnon<-int_trregnflnon[1,2]


trregclothesnon<-lm(clothesnont2~1+clothesnont1, data=brfullw2use)
b_trregclothesnon<-summary(trregclothesnon)$coef[2, 1]
int_trregclothesnon<-confint(trregclothesnon, 'clothesnont1', level=0.95)
low_trregclothesnon<-int_trregclothesnon[1,1]
up_trregclothesnon<-int_trregclothesnon[1,2]


trregycnon<-lm(ycnont2~1+ycnont1, data=brfullw2use)
b_trregycnon<-summary(trregycnon)$coef[2, 1]
int_trregycnon<-confint(trregycnon, 'ycnont1', level=0.95)
low_trregycnon<-int_trregycnon[1,1]
up_trregycnon<-int_trregycnon[1,2]


trregstarwarsnon<-lm(starwarsnont2~1+starwarsnont1, data=brfullw2use)
b_trregstarwarsnon<-summary(trregstarwarsnon)$coef[2, 1]
int_trregstarwarsnon<-confint(trregstarwarsnon, 'starwarsnont1', level=0.95)
low_trregstarwarsnon<-int_trregstarwarsnon[1,1]
up_trregstarwarsnon<-int_trregstarwarsnon[1,2]


#test-retest regression coefficients for br7nolean

trreggolanbr7nolean<-lm(golanbr7noleant2~1+golanbr7noleant1, data=brfullw2use)
b_trreggolanbr7nolean<-summary(trreggolanbr7nolean)$coef[2, 1]
int_trreggolanbr7nolean<-confint(trreggolanbr7nolean, 'golanbr7noleant1', level=0.95)
low_trreggolanbr7nolean<-int_trreggolanbr7nolean[1,1]
up_trreggolanbr7nolean<-int_trreggolanbr7nolean[1,2]

trregchopchopbr7nolean<-lm(chopchopbr7noleant2~1+chopchopbr7noleant1, data=brfullw2use)
b_trregchopchopbr7nolean<-summary(trregchopchopbr7nolean)$coef[2, 1]
int_trregchopchopbr7nolean<-confint(trregchopchopbr7nolean, 'chopchopbr7noleant1', level=0.95)
low_trregchopchopbr7nolean<-int_trregchopchopbr7nolean[1,1]
up_trregchopchopbr7nolean<-int_trregchopchopbr7nolean[1,2]

trregpsychbr7nolean<-lm(psychbr7noleant2~1+psychbr7noleant1, data=brfullw2use)
b_trregpsychbr7nolean<-summary(trregpsychbr7nolean)$coef[2, 1]
int_trregpsychbr7nolean<-confint(trregpsychbr7nolean, 'psychbr7noleant1', level=0.95)
low_trregpsychbr7nolean<-int_trregpsychbr7nolean[1,1]
up_trregpsychbr7nolean<-int_trregpsychbr7nolean[1,2]

trregfollowpolbr7nolean<-lm(followpolbr7noleant2~1+followpolbr7noleant1, data=brfullw2use)
b_trregfollowpolbr7nolean<-summary(trregfollowpolbr7nolean)$coef[2, 1]
int_trregfollowpolbr7nolean<-confint(trregfollowpolbr7nolean, 'followpolbr7noleant1', level=0.95)
low_trregfollowpolbr7nolean<-int_trregfollowpolbr7nolean[1,1]
up_trregfollowpolbr7nolean<-int_trregfollowpolbr7nolean[1,2]

trregmoviesbr7nolean<-lm(moviesbr7noleant2~1+moviesbr7noleant1, data=brfullw2use)
b_trregmoviesbr7nolean<-summary(trregmoviesbr7nolean)$coef[2, 1]
int_trregmoviesbr7nolean<-confint(trregmoviesbr7nolean, 'moviesbr7noleant1', level=0.95)
low_trregmoviesbr7nolean<-int_trregmoviesbr7nolean[1,1]
up_trregmoviesbr7nolean<-int_trregmoviesbr7nolean[1,2]

trregrestbr7nolean<-lm(restbr7noleant2~1+restbr7noleant1, data=brfullw2use)
b_trregrestbr7nolean<-summary(trregrestbr7nolean)$coef[2, 1]
int_trregrestbr7nolean<-confint(trregrestbr7nolean, 'restbr7noleant1', level=0.95)
low_trregrestbr7nolean<-int_trregrestbr7nolean[1,1]
up_trregrestbr7nolean<-int_trregrestbr7nolean[1,2]

trregnflbr7nolean<-lm(nflbr7noleant2~1+nflbr7noleant1, data=brfullw2use)
b_trregnflbr7nolean<-summary(trregnflbr7nolean)$coef[2, 1]
int_trregnflbr7nolean<-confint(trregnflbr7nolean, 'nflbr7noleant1', level=0.95)
low_trregnflbr7nolean<-int_trregnflbr7nolean[1,1]
up_trregnflbr7nolean<-int_trregnflbr7nolean[1,2]


trregclothesbr7nolean<-lm(clothesbr7noleant2~1+clothesbr7noleant1, data=brfullw2use)
b_trregclothesbr7nolean<-summary(trregclothesbr7nolean)$coef[2, 1]
int_trregclothesbr7nolean<-confint(trregclothesbr7nolean, 'clothesbr7noleant1', level=0.95)
low_trregclothesbr7nolean<-int_trregclothesbr7nolean[1,1]
up_trregclothesbr7nolean<-int_trregclothesbr7nolean[1,2]


trregycbr7nolean<-lm(ycbr7noleant2~1+ycbr7noleant1, data=brfullw2use)
b_trregycbr7nolean<-summary(trregycbr7nolean)$coef[2, 1]
int_trregycbr7nolean<-confint(trregycbr7nolean, 'ycbr7noleant1', level=0.95)
low_trregycbr7nolean<-int_trregycbr7nolean[1,1]
up_trregycbr7nolean<-int_trregycbr7nolean[1,2]


trregstarwarsbr7nolean<-lm(starwarsbr7noleant2~1+starwarsbr7noleant1, data=brfullw2use)
b_trregstarwarsbr7nolean<-summary(trregstarwarsbr7nolean)$coef[2, 1]
int_trregstarwarsbr7nolean<-confint(trregstarwarsbr7nolean, 'starwarsbr7noleant1', level=0.95)
low_trregstarwarsbr7nolean<-int_trregstarwarsbr7nolean[1,1]
up_trregstarwarsbr7nolean<-int_trregstarwarsbr7nolean[1,2]


#Creating ForestPlot
trregs.non <- 
  structure(list(
    estimate  = c(b_trreggolannon,b_trregchopchopnon, b_trregpsychnon, b_trregfollowpolnon, b_trregmoviesnon, b_trregrestnon, 
                  b_trregnflnon, b_trregclothesnon, b_trregycnon, b_trregstarwarsnon), 
    lower = c(low_trreggolannon, low_trregchopchopnon, low_trregpsychnon, low_trregfollowpolnon, low_trregmoviesnon, low_trregrestnon, 
              low_trregnflnon, low_trregclothesnon, low_trregycnon, low_trregstarwarsnon),
    upper = c(up_trreggolannon, up_trregchopchopnon, up_trregpsychnon, up_trregfollowpolnon, up_trregmoviesnon, up_trregrestnon,up_trregnflnon, 
              up_trregclothesnon, up_trregycnon, up_trregstarwarsnon)),
    .Names = c("estimate", "lower", "upper"), 
    row.names = c("Golan Heights", "Chop Chop", "Psychology","Politics","Movies", "Dining Out", "NFL", "Clothing","Yeshiva College", "Star Wars"), 
    class = "data.frame")
trregs.branching <- 
  structure(list(
    estimate  = c( b_trreggolanbr7nolean, b_trregchopchopbr7nolean, b_trregpsychbr7nolean, b_trregfollowpolbr7nolean, b_trregmoviesbr7nolean, b_trregrestbr7nolean, b_trregnflbr7nolean, 
                   b_trregclothesbr7nolean , b_trregycbr7nolean,b_trregstarwarsbr7nolean), 
    lower = c(low_trreggolanbr7nolean, low_trregchopchopbr7nolean, low_trregpsychbr7nolean, low_trregfollowpolbr7nolean, low_trregmoviesbr7nolean,
              low_trregrestbr7nolean, low_trregnflbr7nolean, low_trregclothesbr7nolean, low_trregycbr7nolean, low_trregstarwarsbr7nolean),
    upper = c(up_trreggolanbr7nolean, up_trregchopchopbr7nolean, up_trregpsychbr7nolean, up_trregfollowpolbr7nolean, up_trregmoviesbr7nolean, 
              up_trregrestbr7nolean, up_trregnflbr7nolean, up_trregclothesbr7nolean, up_trregycbr7nolean, up_trregstarwarsbr7nolean)),
    .Names = c("estimate", "lower", "upper"), 
    row.names = c("Golan Heights", "Chop Chop", "Psychology","Politics","Movies", "Dining Out", "NFL", "Clothing","Yeshiva College", "Star Wars"), 
    class = "data.frame")
      
install.packages("forestplot")
library("forestplot")

# library(lattice)
setwd("C:/Users/yosef/Downloads/Psyc Classes/Malka Summer Work/")
trellis.device(device="windows",height = 25, width = 40, color=TRUE)
forestplot(row.names(trregs.non),
           legend = c("Non-Branching", "Branching"),
           line.margin = .23,
           fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
           lty.ci = c(1, 2),
           xticks  = seq(from = 0, to = 1, by = 0.1),
           mean = cbind(trregs.non[, "estimate"],trregs.branching[, "estimate"]),
           lower = cbind(trregs.non[,"lower"], trregs.branching[,"lower"]),
           upper = cbind(trregs.non[,"upper"], trregs.branching[,"upper"]),
           zero = 1,
           cex  = 2,
           txt_gp = fpTxtGp(ticks = gpar(fontfamily = "", cex=1)),
           hrzl_lines = TRUE,
           col=fpColors(box=c("blue", "darkred")),
           lineheight = "auto",
           title= "Figure 1: Association Between Wave 1 and Wave 2 Attitudes Across Conditions",
           xlab = "")

#test-retest correlations for nonbranching

cor.test(formula = ~ golannont1 + golannont2,
         data = brfullw2use)

cor.test(formula = ~ chopchopnont1 + chopchopnont2,
         data = brfullw2use)

cor.test(formula = ~ psychnont1 + psychnont2,
         data = brfullw2use)

cor.test(formula = ~ followpolnont1 + followpolnont2,
         data = brfullw2use)

cor.test(formula = ~ moviesnont1 + moviesnont2,
         data = brfullw2use)

cor.test(formula = ~ restnont1 + restnont2,
         data = brfullw2use)

cor.test(formula = ~ nflnont1 + nflnont2,
         data = brfullw2use)

cor.test(formula = ~ clothesnont1 + clothesnont2,
         data = brfullw2use)

cor.test(formula = ~ ycnont1 + ycnont2,
         data = brfullw2use)

cor.test(formula = ~ starwarsnont1 + starwarsnont2,
         data = brfullw2use)


#test-retest correlations for branch 7pt no lean

cor.test(formula = ~ golanbr7noleant1 + golanbr7noleant2,
         data = brfullw2use)

cor.test(formula = ~ chopchopbr7noleant1 + chopchopbr7noleant2,
         data = brfullw2use)

cor.test(formula = ~ psychbr7noleant1 + psychbr7noleant2,
         data = brfullw2use)

cor.test(formula = ~ followpolbr7noleant1 + followpolbr7noleant2,
         data = brfullw2use)

cor.test(formula = ~ moviesbr7noleant1 + moviesbr7noleant2,
         data = brfullw2use)

cor.test(formula = ~ restbr7noleant1 + restbr7noleant2,
         data = brfullw2use)

cor.test(formula = ~ nflbr7noleant1 + nflbr7noleant2,
         data = brfullw2use)

cor.test(formula = ~ clothesbr7noleant1 + clothesbr7noleant2,
         data = brfullw2use)

cor.test(formula = ~ ycbr7noleant1 + ycbr7noleant2,
         data = brfullw2use)

cor.test(formula = ~ starwarsbr7noleant1 + starwarsbr7noleant2,
         data = brfullw2use)

#regression testing condition X time 1 predicting time 2 attitude, for 7pt no lean


reg<-lm(formula = golanall7noleant2 ~ golanall7noleant1+condt2+golanall7noleant1:condt2,
        data = brfullw2use)

summary(reg)

reg<-lm(formula = chopchopall7noleant2 ~ chopchopall7noleant1+condt2+chopchopall7noleant1:condt2,
        data = brfullw2use)

summary(reg)


reg<-lm(formula = psychall7noleant2 ~ psychall7noleant1+condt2+psychall7noleant1:condt2,
        data = brfullw2use)

summary(reg)


reg<-lm(formula = followpolall7noleant2 ~ followpolall7noleant1+condt2+followpolall7noleant1:condt2,
        data = brfullw2use)

summary(reg)


reg<-lm(formula = moviesall7noleant2 ~ moviesall7noleant1+condt2+moviesall7noleant1:condt2,
        data = brfullw2use)

summary(reg)


reg<-lm(formula = restall7noleant2 ~ restall7noleant1+condt2+restall7noleant1:condt2,
        data = brfullw2use)

summary(reg)


reg<-lm(formula = nflall7noleant2 ~ nflall7noleant1+condt2+nflall7noleant1:condt2,
        data = brfullw2use)

summary(reg)

reg<-lm(formula = clothesall7noleant2 ~ clothesall7noleant1+condt2+clothesall7noleant1:condt2,
        data = brfullw2use)

summary(reg)

reg<-lm(formula = ycall7noleant2 ~ ycall7noleant1+condt2+ycall7noleant1:condt2,
        data = brfullw2use)

summary(reg)

reg<-lm(formula = starwarsall7noleant2 ~ starwarsall7noleant1+condt2+starwarsall7noleant1:condt2,
        data = brfullw2use)

summary(reg)





#APPENDIX PART B
#Table B1: test-retest correlations for branch 7pt with lean

cor.test(formula = ~ golanbr7leant1 + golanbr7leant2,
         data = brfullw2use)

cor.test(formula = ~ chopchopbr7leant1 + chopchopbr7leant2,
         data = brfullw2use)

cor.test(formula = ~ psychbr7leant1 + psychbr7leant2,
         data = brfullw2use)

cor.test(formula = ~ followpolbr7leant1 + followpolbr7leant2,
         data = brfullw2use)

cor.test(formula = ~ moviesbr7leant1 + moviesbr7leant2,
         data = brfullw2use)

cor.test(formula = ~ restbr7leant1 + restbr7leant2,
         data = brfullw2use)

cor.test(formula = ~ nflbr7leant1 + nflbr7leant2,
         data = brfullw2use)

cor.test(formula = ~ clothesbr7leant1 + clothesbr7leant2,
         data = brfullw2use)

cor.test(formula = ~ ycbr7leant1 + ycbr7leant2,
         data = brfullw2use)

cor.test(formula = ~ starwarsbr7leant1 + starwarsbr7leant2,
         data = brfullw2use)





#test-retest correlations for branch 9pt with lean

cor.test(formula = ~ golanbr9leant1 + golanbr9leant2,
         data = brfullw2use)

cor.test(formula = ~ chopchopbr9leant1 + chopchopbr9leant2,
         data = brfullw2use)

cor.test(formula = ~ psychbr9leant1 + psychbr9leant2,
         data = brfullw2use)

cor.test(formula = ~ followpolbr9leant1 + followpolbr9leant2,
         data = brfullw2use)

cor.test(formula = ~ moviesbr9leant1 + moviesbr9leant2,
         data = brfullw2use)

cor.test(formula = ~ restbr9leant1 + restbr9leant2,
         data = brfullw2use)

cor.test(formula = ~ nflbr9leant1 + nflbr9leant2,
         data = brfullw2use)

cor.test(formula = ~ clothesbr9leant1 + clothesbr9leant2,
         data = brfullw2use)

cor.test(formula = ~ ycbr9leant1 + ycbr9leant2,
         data = brfullw2use)

cor.test(formula = ~ starwarsbr9leant1 + starwarsbr9leant2,
         data = brfullw2use)




#regression testing condition X time 1 predicting time 2 attitude, for 7pt WITH lean


reg<-lm(formula = golanall7leant2 ~ golanall7leant1+condt2+golanall7leant1:condt2,
        data = brfullw2use)

summary(reg)

reg<-lm(formula = chopchopall7leant2 ~ chopchopall7leant1+condt2+chopchopall7leant1:condt2,
        data = brfullw2use)

summary(reg)


reg<-lm(formula = psychall7leant2 ~ psychall7leant1+condt2+psychall7leant1:condt2,
        data = brfullw2use)

summary(reg)


reg<-lm(formula = followpolall7leant2 ~ followpolall7leant1+condt2+followpolall7leant1:condt2,
        data = brfullw2use)

summary(reg)


reg<-lm(formula = moviesall7leant2 ~ moviesall7leant1+condt2+moviesall7leant1:condt2,
        data = brfullw2use)

summary(reg)


reg<-lm(formula = restall7leant2 ~ restall7leant1+condt2+restall7leant1:condt2,
        data = brfullw2use)

summary(reg)


reg<-lm(formula = nflall7leant2 ~ nflall7leant1+condt2+nflall7leant1:condt2,
        data = brfullw2use)

summary(reg)

reg<-lm(formula = clothesall7leant2 ~ clothesall7leant1+condt2+clothesall7leant1:condt2,
        data = brfullw2use)

summary(reg)

reg<-lm(formula = ycall7leant2 ~ ycall7leant1+condt2+ycall7leant1:condt2,
        data = brfullw2use)

summary(reg)

reg<-lm(formula = starwarsall7leant2 ~ starwarsall7leant1+condt2+starwarsall7leant1:condt2,
        data = brfullw2use)

summary(reg)




#regression testing condition X time 1 predicting time 2 attitude, for 9pt WITH lean


reg<-lm(formula = golanall9leant2 ~ golanall9leant1+condt2+golanall9leant1:condt2,
        data = brfullw2use)

summary(reg)

reg<-lm(formula = chopchopall9leant2 ~ chopchopall9leant1+condt2+chopchopall9leant1:condt2,
        data = brfullw2use)

summary(reg)


reg<-lm(formula = psychall9leant2 ~ psychall9leant1+condt2+psychall9leant1:condt2,
        data = brfullw2use)

summary(reg)


reg<-lm(formula = followpolall9leant2 ~ followpolall9leant1+condt2+followpolall9leant1:condt2,
        data = brfullw2use)

summary(reg)


reg<-lm(formula = moviesall9leant2 ~ moviesall9leant1+condt2+moviesall9leant1:condt2,
        data = brfullw2use)

summary(reg)


reg<-lm(formula = restall9leant2 ~ restall9leant1+condt2+restall9leant1:condt2,
        data = brfullw2use)

summary(reg)


reg<-lm(formula = nflall9leant2 ~ nflall9leant1+condt2+nflall9leant1:condt2,
        data = brfullw2use)

summary(reg)

reg<-lm(formula = clothesall9leant2 ~ clothesall9leant1+condt2+clothesall9leant1:condt2,
        data = brfullw2use)

summary(reg)

reg<-lm(formula = ycall9leant2 ~ ycall9leant1+condt2+ycall9leant1:condt2,
        data = brfullw2use)

summary(reg)

reg<-lm(formula = starwarsall9leant2 ~ starwarsall9leant1+condt2+starwarsall9leant1:condt2,
        data = brfullw2use)

summary(reg)


# describing the test retest corrs in nonbranching and branching conditions

nonbranchcorrs<-c(.73, .76, .59, .81, .85, .62, .89, .73, .62, .84)

branch7noleancorrs<-c(.47, .65, .61, .84, .76, .56, .87, .80, .65, .76)

branch7leancorrs<-c(.45, .64, .60, .86, .78, .54, .88, .81, .66, .76)

branch9leancorrs<-c(.43, .64, .61, .87, .77, .53, .88, .79, .63, .76)

describe(nonbranchcorrs)
describe(branch7noleancorrs)
describe(branch7leancorrs)
describe(branch9leancorrs)

#Table 4: correlations with self-report criterion variables and regressions testing for differences across conditions.  all using 7 point no lean.

#Golan Heights (1)	Log Golan Heights Eating (2)
cor.test(brfullw2use$golannont1, brfullw2use$loggolaneat1s, use="pair")

cor.test(brfullw2use$golanbr7noleant1, brfullw2use$loggolaneat1s, use="pair")

reg<-lm(loggolaneat1s~golanall7noleant1+condt1+golanall7noleant1:condt1, data=brfullw2use)
summary(reg)


#Golan Heights (2)	Log Golan Heights Eating (2)
cor.test(brfullw2use$golannont2, brfullw2use$loggolaneat1s, use="pair")

cor.test(brfullw2use$golanbr7noleant2, brfullw2use$loggolaneat1s, use="pair")

reg<-lm(loggolaneat1s~golanall7noleant2+condt1+golanall7noleant2:condt1, data=brfullw2use)
summary(reg)

#Chop Chop (1)	Log Chop Chop Eating (2)
cor.test(brfullw2use$chopchopnont1, brfullw2use$logchopchopeat1s, use="pair")

cor.test(brfullw2use$chopchopbr7noleant1, brfullw2use$logchopchopeat1s, use="pair")

reg<-lm(logchopchopeat1s~chopchopall7noleant1+condt1+chopchopall7noleant1:condt1, data=brfullw2use)
summary(reg)


#Chop Chop (2)	Log Chop Chop Eating (2)
cor.test(brfullw2use$chopchopnont2, brfullw2use$logchopchopeat1s, use="pair")

cor.test(brfullw2use$chopchopbr7noleant2, brfullw2use$logchopchopeat1s, use="pair")

reg<-lm(logchopchopeat1s~chopchopall7noleant2+condt1+chopchopall7noleant2:condt1, data=brfullw2use)
summary(reg)


#Psychology (1) 		See Psychology Talk (1)

cor.test(brfull$psychnont1, brfull$seepsychtalk1s, use="pair")

cor.test(brfull$psychbr7noleant1, brfull$seepsychtalk1s, use="pair")

reg<-lm(seepsychtalk1s~psychall7noleant1+condt1+psychall7noleant1:condt1, data=brfull)
summary(reg)


#Psychology (2) 		See Psychology Talk (1)

cor.test(brfullw2use$psychnont2, brfullw2use$seepsychtalk1s, use="pair")

cor.test(brfullw2use$psychbr7noleant2, brfullw2use$seepsychtalk1s, use="pair")

reg<-lm(seepsychtalk1s~psychall7noleant2+condt1+psychall7noleant2:condt1, data=brfullw2use)
summary(reg)


#Politics (1)		Political Consumption (1)

cor.test(brfull$followpolnont1, brfull$polconsumpt11s, use="pair")

cor.test(brfull$followpolbr7noleant1, brfull$polconsumpt11s, use="pair")

reg<-lm(polconsumpt11s~followpolall7noleant1+condt1+followpolall7noleant1:condt1, data=brfull)
summary(reg)


#Politics (1)		Political Consumption (2)

cor.test(brfullw2use$followpolnont1, brfullw2use$polconsumpt21s, use="pair")

cor.test(brfullw2use$followpolbr7noleant1, brfullw2use$polconsumpt21s, use="pair")

reg<-lm(polconsumpt21s~followpolall7noleant1+condt1+followpolall7noleant1:condt1, data=brfullw2use)
summary(reg)

#Politics (1) 		Expect to Vote (1)
cor.test(brfull$followpolnont1, brfull$expectvote1s, use="pair")

cor.test(brfull$followpolbr7noleant1, brfull$expectvote1s, use="pair")

reg<-lm(expectvote1s~followpolall7noleant1+condt1+followpolall7noleant1:condt1, data=brfull)
summary(reg)


#Politics (1)		Political Knowledge (1)
cor.test(brfull$followpolnont1, brfull$politknowl, use="pair")

cor.test(brfull$followpolbr7noleant1, brfull$politknowl, use="pair")

reg<-lm(politknowl~followpolall7noleant1+condt1+followpolall7noleant1:condt1, data=brfull)
summary(reg)


#Politics (2)		Political Consumption (1)
cor.test(brfullw2use$followpolnont2, brfullw2use$polconsumpt11s, use="pair")

cor.test(brfullw2use$followpolbr7noleant2, brfullw2use$polconsumpt11s, use="pair")

reg<-lm(polconsumpt11s~followpolall7noleant2+condt1+followpolall7noleant2:condt1, data=brfullw2use)
summary(reg)

#Politics (2)		Political Consumption (2)

cor.test(brfullw2use$followpolnont2, brfullw2use$polconsumpt21s, use="pair")

cor.test(brfullw2use$followpolbr7noleant2, brfullw2use$polconsumpt21s, use="pair")

reg<-lm(polconsumpt21s~followpolall7noleant2+condt1+followpolall7noleant2:condt1, data=brfullw2use)
summary(reg)


#Politics (2) 		Expect to Vote (1)

cor.test(brfullw2use$followpolnont2, brfullw2use$expectvote1s, use="pair")

cor.test(brfullw2use$followpolbr7noleant2, brfullw2use$expectvote1s, use="pair")

reg<-lm(expectvote1s~followpolall7noleant2+condt1+followpolall7noleant2:condt1, data=brfullw2use)
summary(reg)


#Politics (2)		Political Knowledge (1)

cor.test(brfullw2use$followpolnont2, brfullw2use$politknowl, use="pair")

cor.test(brfullw2use$followpolbr7noleant2, brfullw2use$politknowl, use="pair")

reg<-lm(politknowl~followpolall7noleant2+condt1+followpolall7noleant2:condt1, data=brfullw2use)
summary(reg)


#Dining Out (1)		Log Dined Out (1)
cor.test(brfull$restnont1, brfull$logdinedpastmonth1s, use="pair")

cor.test(brfull$restbr7noleant1, brfull$logdinedpastmonth1s, use="pair")

reg<-lm(logdinedpastmonth1s~restall7noleant1+condt1+restall7noleant1:condt1, data=brfull)
summary(reg)


#Dining Out (2)		Log Dined Out (1)
cor.test(brfullw2use$restnont2, brfullw2use$logdinedpastmonth1s, use="pair")

cor.test(brfullw2use$restbr7noleant2, brfullw2use$logdinedpastmonth1s, use="pair")

reg<-lm(logdinedpastmonth1s~restall7noleant2+condt1+restall7noleant2:condt1, data=brfullw2use)
summary(reg)

#NFL (1)			Log Watch NFL (1)
cor.test(brfull$nflnont1, brfull$lognflwatcht11s, use="pair")

cor.test(brfull$nflbr7noleant1, brfull$lognflwatcht11s, use="pair")

reg<-lm(lognflwatcht11s~nflall7noleant1+condt1+nflall7noleant1:condt1, data=brfull)
summary(reg)


#NFL (1)			NFL Knowledge (1)
cor.test(brfull$nflnont1, brfull$nflknowl, use="pair")

cor.test(brfull$nflbr7noleant1, brfull$nflknowl, use="pair")

reg<-lm(nflknowl~nflall7noleant1+condt1+nflall7noleant1:condt1, data=brfull)
summary(reg)


#NFL (2)			Log Watch NFL (1)
cor.test(brfullw2use$nflnont2, brfullw2use$lognflwatcht11s, use="pair")

cor.test(brfullw2use$nflbr7noleant2, brfullw2use$lognflwatcht11s, use="pair")

reg<-lm(lognflwatcht11s~nflall7noleant2+condt1+nflall7noleant2:condt1, data=brfullw2use)
summary(reg)


#NFL (2)			NFL Knowledge (1)
cor.test(brfullw2use$nflnont2, brfullw2use$nflknowl, use="pair")

cor.test(brfullw2use$nflbr7noleant2, brfullw2use$nflknowl, use="pair")

reg<-lm(nflknowl~nflall7noleant2+condt1+nflall7noleant2:condt1, data=brfullw2use)
summary(reg)


#Clothes  shopping (1)	Log Clothing Spend (1)
cor.test(brfull$clothesnont1, brfull$logclothesspendt11s, use="pair")

cor.test(brfull$clothesbr7noleant1, brfull$logclothesspendt11s, use="pair")

reg<-lm(logclothesspendt11s~clothesall7noleant1+condt1+clothesall7noleant1:condt1, data=brfull)
summary(reg)


#Clothes shopping (1)	Log Expected Clothing Spend (2)
cor.test(brfullw2use$clothesnont1, brfullw2use$logclothesexpectt21s, use="pair")

cor.test(brfullw2use$clothesbr7noleant1, brfullw2use$logclothesexpectt21s, use="pair")

reg<-lm(logclothesexpectt21s~clothesall7noleant1+condt1+clothesall7noleant1:condt1, data=brfullw2use)
summary(reg)


#Clothes  shopping (2)	Log Clothing Spend (1)
cor.test(brfullw2use$clothesnont2, brfullw2use$logclothesspendt11s, use="pair")

cor.test(brfullw2use$clothesbr7noleant2, brfullw2use$logclothesspendt11s, use="pair")

reg<-lm(logclothesspendt11s~clothesall7noleant2+condt1+clothesall7noleant2:condt1, data=brfullw2use)
summary(reg)

#Star Wars (1)		Seen Force Awakens (1)

cor.test(brfull$starwarsnont1, brfull$forceawakens, use="pair")

cor.test(brfull$starwarsbr7noleant1, brfull$forceawakens, use="pair")

reg<-glm(forceawakens~starwarsall7noleant1+condt1+starwarsall7noleant1:condt1, family="binomial", data=brfull)
summary(reg)


#Star Wars (2)		Seen Force Awakens (1)

cor.test(brfullw2use$starwarsnont2, brfullw2use$forceawakens, use="pair")

cor.test(brfullw2use$starwarsbr7noleant2, brfullw2use$forceawakens, use="pair")

reg<-glm(forceawakens~starwarsall7noleant2+condt1+starwarsall7noleant2:condt1, family="binomial", data=brfullw2use)
summary(reg)



# appendixTable 2b: correlations with self-report criterion variables and regressions testing for differences across conditions.  all using 7 point lean.

#Golan Heights (1)	Log Golan Heights Eating (2)
cor.test(brfullw2use$golannont1, brfullw2use$loggolaneat1s, use="pair")

cor.test(brfullw2use$golanbr7leant1, brfullw2use$loggolaneat1s, use="pair")

reg<-lm(loggolaneat1s~golanall7leant1+condt1+golanall7leant1:condt1, data=brfullw2use)
summary(reg)


#Golan Heights (2)	Log Golan Heights Eating (2)
cor.test(brfullw2use$golannont2, brfullw2use$loggolaneat1s, use="pair")

cor.test(brfullw2use$golanbr7leant2, brfullw2use$loggolaneat1s, use="pair")

reg<-lm(loggolaneat1s~golanall7leant2+condt1+golanall7leant2:condt1, data=brfullw2use)
summary(reg)

#Chop Chop (1)	Log Chop Chop Eating (2)
cor.test(brfullw2use$chopchopnont1, brfullw2use$logchopchopeat1s, use="pair")

cor.test(brfullw2use$chopchopbr7leant1, brfullw2use$logchopchopeat1s, use="pair")

reg<-lm(logchopchopeat1s~chopchopall7leant1+condt1+chopchopall7leant1:condt1, data=brfullw2use)
summary(reg)


#Chop Chop (2)	Log Chop Chop Eating (2)
cor.test(brfullw2use$chopchopnont2, brfullw2use$logchopchopeat1s, use="pair")

cor.test(brfullw2use$chopchopbr7leant2, brfullw2use$logchopchopeat1s, use="pair")

reg<-lm(logchopchopeat1s~chopchopall7leant2+condt1+chopchopall7leant2:condt1, data=brfullw2use)
summary(reg)


#Psychology (1) 		See Psychology Talk (1)

cor.test(brfull$psychnont1, brfull$seepsychtalk1s, use="pair")

cor.test(brfull$psychbr7leant1, brfull$seepsychtalk1s, use="pair")

reg<-lm(seepsychtalk1s~psychall7leant1+condt1+psychall7leant1:condt1, data=brfull)
summary(reg)


#Psychology (2) 		See Psychology Talk (1)

cor.test(brfullw2use$psychnont2, brfullw2use$seepsychtalk1s, use="pair")

cor.test(brfullw2use$psychbr7leant2, brfullw2use$seepsychtalk1s, use="pair")

reg<-lm(seepsychtalk1s~psychall7leant2+condt1+psychall7leant2:condt1, data=brfullw2use)
summary(reg)


#Politics (1)		Political Consumption (1)

cor.test(brfull$followpolnont1, brfull$polconsumpt11s, use="pair")

cor.test(brfull$followpolbr7leant1, brfull$polconsumpt11s, use="pair")

reg<-lm(polconsumpt11s~followpolall7leant1+condt1+followpolall7leant1:condt1, data=brfull)
summary(reg)


#Politics (1)		Political Consumption (2)

cor.test(brfullw2use$followpolnont1, brfullw2use$polconsumpt21s, use="pair")

cor.test(brfullw2use$followpolbr7leant1, brfullw2use$polconsumpt21s, use="pair")

reg<-lm(polconsumpt21s~followpolall7leant1+condt1+followpolall7leant1:condt1, data=brfullw2use)
summary(reg)

#Politics (1) 		Expect to Vote (1)
cor.test(brfull$followpolnont1, brfull$expectvote1s, use="pair")

cor.test(brfull$followpolbr7leant1, brfull$expectvote1s, use="pair")

reg<-lm(expectvote1s~followpolall7leant1+condt1+followpolall7leant1:condt1, data=brfull)
summary(reg)


#Politics (1)		Political Knowledge (1)
cor.test(brfull$followpolnont1, brfull$politknowl, use="pair")

cor.test(brfull$followpolbr7leant1, brfull$politknowl, use="pair")

reg<-lm(politknowl~followpolall7leant1+condt1+followpolall7leant1:condt1, data=brfull)
summary(reg)


#Politics (2)		Political Consumption (1)
cor.test(brfullw2use$followpolnont2, brfullw2use$polconsumpt11s, use="pair")

cor.test(brfullw2use$followpolbr7leant2, brfullw2use$polconsumpt11s, use="pair")

reg<-lm(polconsumpt11s~followpolall7leant2+condt1+followpolall7leant2:condt1, data=brfullw2use)
summary(reg)

#Politics (2)		Political Consumption (2)

cor.test(brfullw2use$followpolnont2, brfullw2use$polconsumpt21s, use="pair")

cor.test(brfullw2use$followpolbr7leant2, brfullw2use$polconsumpt21s, use="pair")

reg<-lm(polconsumpt21s~followpolall7leant2+condt1+followpolall7leant2:condt1, data=brfullw2use)
summary(reg)


#Politics (2) 		Expect to Vote (1)

cor.test(brfullw2use$followpolnont2, brfullw2use$expectvote1s, use="pair")

cor.test(brfullw2use$followpolbr7leant2, brfullw2use$expectvote1s, use="pair")

reg<-lm(expectvote1s~followpolall7leant2+condt1+followpolall7leant2:condt1, data=brfullw2use)
summary(reg)


#Politics (2)		Political Knowledge (1)

cor.test(brfullw2use$followpolnont2, brfullw2use$politknowl, use="pair")

cor.test(brfullw2use$followpolbr7leant2, brfullw2use$politknowl, use="pair")

reg<-lm(politknowl~followpolall7leant2+condt1+followpolall7leant2:condt1, data=brfullw2use)
summary(reg)


#Dining Out (1)		Log Dined Out (1)
cor.test(brfull$restnont1, brfull$logdinedpastmonth1s, use="pair")

cor.test(brfull$restbr7leant1, brfull$logdinedpastmonth1s, use="pair")

reg<-lm(logdinedpastmonth1s~restall7leant1+condt1+restall7leant1:condt1, data=brfull)
summary(reg)


#Dining Out (2)		Log Dined Out (1)
cor.test(brfullw2use$restnont2, brfullw2use$logdinedpastmonth1s, use="pair")

cor.test(brfullw2use$restbr7leant2, brfullw2use$logdinedpastmonth1s, use="pair")

reg<-lm(logdinedpastmonth1s~restall7leant2+condt1+restall7leant2:condt1, data=brfullw2use)
summary(reg)

#NFL (1)			Log Watch NFL (1)
cor.test(brfull$nflnont1, brfull$lognflwatcht11s, use="pair")

cor.test(brfull$nflbr7leant1, brfull$lognflwatcht11s, use="pair")

reg<-lm(lognflwatcht11s~nflall7leant1+condt1+nflall7leant1:condt1, data=brfull)
summary(reg)


#NFL (1)			NFL Knowledge (1)
cor.test(brfull$nflnont1, brfull$nflknowl, use="pair")

cor.test(brfull$nflbr7leant1, brfull$nflknowl, use="pair")

reg<-lm(nflknowl~nflall7leant1+condt1+nflall7leant1:condt1, data=brfull)
summary(reg)


#NFL (2)			Log Watch NFL (1)
cor.test(brfullw2use$nflnont2, brfullw2use$lognflwatcht11s, use="pair")

cor.test(brfullw2use$nflbr7leant2, brfullw2use$lognflwatcht11s, use="pair")

reg<-lm(lognflwatcht11s~nflall7leant2+condt1+nflall7leant2:condt1, data=brfullw2use)
summary(reg)


#NFL (2)			NFL Knowledge (1)
cor.test(brfullw2use$nflnont2, brfullw2use$nflknowl, use="pair")

cor.test(brfullw2use$nflbr7leant2, brfullw2use$nflknowl, use="pair")

reg<-lm(nflknowl~nflall7leant2+condt1+nflall7leant2:condt1, data=brfullw2use)
summary(reg)


#Clothes  shopping (1)	Log Clothing Spend (1)
cor.test(brfull$clothesnont1, brfull$logclothesspendt11s, use="pair")

cor.test(brfull$clothesbr7leant1, brfull$logclothesspendt11s, use="pair")

reg<-lm(logclothesspendt11s~clothesall7leant1+condt1+clothesall7leant1:condt1, data=brfull)
summary(reg)


#Clothes shopping (1)	Log Expected Clothing Spend (2)
cor.test(brfullw2use$clothesnont1, brfullw2use$logclothesexpectt21s, use="pair")

cor.test(brfullw2use$clothesbr7leant1, brfullw2use$logclothesexpectt21s, use="pair")

reg<-lm(logclothesexpectt21s~clothesall7leant1+condt1+clothesall7leant1:condt1, data=brfullw2use)
summary(reg)


#Clothes  shopping (2)	Log Clothing Spend (1)
cor.test(brfullw2use$clothesnont2, brfullw2use$logclothesspendt11s, use="pair")

cor.test(brfullw2use$clothesbr7leant2, brfullw2use$logclothesspendt11s, use="pair")

reg<-lm(logclothesspendt11s~clothesall7leant2+condt1+clothesall7leant2:condt1, data=brfullw2use)
summary(reg)

#Star Wars (1)		Seen Force Awakens (1)

cor.test(brfull$starwarsnont1, brfull$forceawakens, use="pair")

cor.test(brfull$starwarsbr7leant1, brfull$forceawakens, use="pair")

reg<-glm(forceawakens~starwarsall7leant1+condt1+starwarsall7leant1:condt1, family="binomial", data=brfull)
summary(reg)


#Star Wars (2)		Seen Force Awakens (1)

cor.test(brfullw2use$starwarsnont2, brfullw2use$forceawakens, use="pair")

cor.test(brfullw2use$starwarsbr7leant2, brfullw2use$forceawakens, use="pair")

reg<-glm(forceawakens~starwarsall7leant2+condt1+starwarsall7leant2:condt1, family="binomial", data=brfullw2use)
summary(reg)





# appendixTable 3b: correlations with self-report criterion variables and regressions testing for differences across conditions.  all using 9 point lean.

#Golan Heights (1)	Log Golan Heights Eating (2)
cor.test(brfullw2use$golannont1, brfullw2use$loggolaneat1s, use="pair")

cor.test(brfullw2use$golanbr9leant1, brfullw2use$loggolaneat1s, use="pair")

reg<-lm(loggolaneat1s~golanall9leant1+condt1+golanall9leant1:condt1, data=brfullw2use)
summary(reg)


#Golan Heights (2)	Log Golan Heights Eating (2)
cor.test(brfullw2use$golannont2, brfullw2use$loggolaneat1s, use="pair")

cor.test(brfullw2use$golanbr9leant2, brfullw2use$loggolaneat1s, use="pair")

reg<-lm(loggolaneat1s~golanall9leant2+condt1+golanall9leant2:condt1, data=brfullw2use)
summary(reg)

#Chop Chop (1)	Log Chop Chop Eating (2)
cor.test(brfullw2use$chopchopnont1, brfullw2use$logchopchopeat1s, use="pair")

cor.test(brfullw2use$chopchopbr9leant1, brfullw2use$logchopchopeat1s, use="pair")

reg<-lm(logchopchopeat1s~chopchopall9leant1+condt1+chopchopall9leant1:condt1, data=brfullw2use)
summary(reg)


#Chop Chop (2)	Log Chop Chop Eating (2)
cor.test(brfullw2use$chopchopnont2, brfullw2use$logchopchopeat1s, use="pair")

cor.test(brfullw2use$chopchopbr9leant2, brfullw2use$logchopchopeat1s, use="pair")

reg<-lm(logchopchopeat1s~chopchopall9leant2+condt1+chopchopall9leant2:condt1, data=brfullw2use)
summary(reg)


#Psychology (1) 		See Psychology Talk (1)

cor.test(brfull$psychnont1, brfull$seepsychtalk1s, use="pair")

cor.test(brfull$psychbr9leant1, brfull$seepsychtalk1s, use="pair")

reg<-lm(seepsychtalk1s~psychall9leant1+condt1+psychall9leant1:condt1, data=brfull)
summary(reg)


#Psychology (2) 		See Psychology Talk (1)

cor.test(brfullw2use$psychnont2, brfullw2use$seepsychtalk1s, use="pair")

cor.test(brfullw2use$psychbr9leant2, brfullw2use$seepsychtalk1s, use="pair")

reg<-lm(seepsychtalk1s~psychall9leant2+condt1+psychall9leant2:condt1, data=brfullw2use)
summary(reg)


#Politics (1)		Political Consumption (1)

cor.test(brfull$followpolnont1, brfull$polconsumpt11s, use="pair")

cor.test(brfull$followpolbr9leant1, brfull$polconsumpt11s, use="pair")

reg<-lm(polconsumpt11s~followpolall9leant1+condt1+followpolall9leant1:condt1, data=brfull)
summary(reg)


#Politics (1)		Political Consumption (2)

cor.test(brfullw2use$followpolnont1, brfullw2use$polconsumpt21s, use="pair")

cor.test(brfullw2use$followpolbr9leant1, brfullw2use$polconsumpt21s, use="pair")

reg<-lm(polconsumpt21s~followpolall9leant1+condt1+followpolall9leant1:condt1, data=brfullw2use)
summary(reg)

#Politics (1) 		Expect to Vote (1)
cor.test(brfull$followpolnont1, brfull$expectvote1s, use="pair")

cor.test(brfull$followpolbr9leant1, brfull$expectvote1s, use="pair")

reg<-lm(expectvote1s~followpolall9leant1+condt1+followpolall9leant1:condt1, data=brfull)
summary(reg)


#Politics (1)		Political Knowledge (1)
cor.test(brfull$followpolnont1, brfull$politknowl, use="pair")

cor.test(brfull$followpolbr9leant1, brfull$politknowl, use="pair")

reg<-lm(politknowl~followpolall9leant1+condt1+followpolall9leant1:condt1, data=brfull)
summary(reg)


#Politics (2)		Political Consumption (1)
cor.test(brfullw2use$followpolnont2, brfullw2use$polconsumpt11s, use="pair")

cor.test(brfullw2use$followpolbr9leant2, brfullw2use$polconsumpt11s, use="pair")

reg<-lm(polconsumpt11s~followpolall9leant2+condt1+followpolall9leant2:condt1, data=brfullw2use)
summary(reg)

#Politics (2)		Political Consumption (2)

cor.test(brfullw2use$followpolnont2, brfullw2use$polconsumpt21s, use="pair")

cor.test(brfullw2use$followpolbr9leant2, brfullw2use$polconsumpt21s, use="pair")

reg<-lm(polconsumpt21s~followpolall9leant2+condt1+followpolall9leant2:condt1, data=brfullw2use)
summary(reg)


#Politics (2) 		Expect to Vote (1)

cor.test(brfullw2use$followpolnont2, brfullw2use$expectvote1s, use="pair")

cor.test(brfullw2use$followpolbr9leant2, brfullw2use$expectvote1s, use="pair")

reg<-lm(expectvote1s~followpolall9leant2+condt1+followpolall9leant2:condt1, data=brfullw2use)
summary(reg)


#Politics (2)		Political Knowledge (1)

cor.test(brfullw2use$followpolnont2, brfullw2use$politknowl, use="pair")

cor.test(brfullw2use$followpolbr9leant2, brfullw2use$politknowl, use="pair")

reg<-lm(politknowl~followpolall9leant2+condt1+followpolall9leant2:condt1, data=brfullw2use)
summary(reg)


#Dining Out (1)		Log Dined Out (1)
cor.test(brfull$restnont1, brfull$logdinedpastmonth1s, use="pair")

cor.test(brfull$restbr9leant1, brfull$logdinedpastmonth1s, use="pair")

reg<-lm(logdinedpastmonth1s~restall9leant1+condt1+restall9leant1:condt1, data=brfull)
summary(reg)


#Dining Out (2)		Log Dined Out (1)
cor.test(brfullw2use$restnont2, brfullw2use$logdinedpastmonth1s, use="pair")

cor.test(brfullw2use$restbr9leant2, brfullw2use$logdinedpastmonth1s, use="pair")

reg<-lm(logdinedpastmonth1s~restall9leant2+condt1+restall9leant2:condt1, data=brfullw2use)
summary(reg)

#NFL (1)			Log Watch NFL (1)
cor.test(brfull$nflnont1, brfull$lognflwatcht11s, use="pair")

cor.test(brfull$nflbr9leant1, brfull$lognflwatcht11s, use="pair")

reg<-lm(lognflwatcht11s~nflall9leant1+condt1+nflall9leant1:condt1, data=brfull)
summary(reg)


#NFL (1)			NFL Knowledge (1)
cor.test(brfull$nflnont1, brfull$nflknowl, use="pair")

cor.test(brfull$nflbr9leant1, brfull$nflknowl, use="pair")

reg<-lm(nflknowl~nflall9leant1+condt1+nflall9leant1:condt1, data=brfull)
summary(reg)


#NFL (2)			Log Watch NFL (1)
cor.test(brfullw2use$nflnont2, brfullw2use$lognflwatcht11s, use="pair")

cor.test(brfullw2use$nflbr9leant2, brfullw2use$lognflwatcht11s, use="pair")

reg<-lm(lognflwatcht11s~nflall9leant2+condt1+nflall9leant2:condt1, data=brfullw2use)
summary(reg)


#NFL (2)			NFL Knowledge (1)
cor.test(brfullw2use$nflnont2, brfullw2use$nflknowl, use="pair")

cor.test(brfullw2use$nflbr9leant2, brfullw2use$nflknowl, use="pair")

reg<-lm(nflknowl~nflall9leant2+condt1+nflall9leant2:condt1, data=brfullw2use)
summary(reg)


#Clothes  shopping (1)	Log Clothing Spend (1)
cor.test(brfull$clothesnont1, brfull$logclothesspendt11s, use="pair")

cor.test(brfull$clothesbr9leant1, brfull$logclothesspendt11s, use="pair")

reg<-lm(logclothesspendt11s~clothesall9leant1+condt1+clothesall9leant1:condt1, data=brfull)
summary(reg)


#Clothes shopping (1)	Log Expected Clothing Spend (2)
cor.test(brfullw2use$clothesnont1, brfullw2use$logclothesexpectt21s, use="pair")

cor.test(brfullw2use$clothesbr9leant1, brfullw2use$logclothesexpectt21s, use="pair")

reg<-lm(logclothesexpectt21s~clothesall9leant1+condt1+clothesall9leant1:condt1, data=brfullw2use)
summary(reg)


#Clothes  shopping (2)	Log Clothing Spend (1)
cor.test(brfullw2use$clothesnont2, brfullw2use$logclothesspendt11s, use="pair")

cor.test(brfullw2use$clothesbr9leant2, brfullw2use$logclothesspendt11s, use="pair")

reg<-lm(logclothesspendt11s~clothesall9leant2+condt1+clothesall9leant2:condt1, data=brfullw2use)
summary(reg)

#Star Wars (1)		Seen Force Awakens (1)

cor.test(brfull$starwarsnont1, brfull$forceawakens, use="pair")

cor.test(brfull$starwarsbr9leant1, brfull$forceawakens, use="pair")

reg<-glm(forceawakens~starwarsall9leant1+condt1+starwarsall9leant1:condt1, family="binomial", data=brfull)
summary(reg)


#Star Wars (2)		Seen Force Awakens (1)

cor.test(brfullw2use$starwarsnont2, brfullw2use$forceawakens, use="pair")

cor.test(brfullw2use$starwarsbr9leant2, brfullw2use$forceawakens, use="pair")

reg<-glm(forceawakens~starwarsall9leant2+condt1+starwarsall9leant2:condt1, family="binomial", data=brfullw2use)
summary(reg)


# Correlations between condition and subjective responses to attitude questions

cor.test(brfull$condt1, brfull$diffcompost1, use="PAIR")

cor.test(brfull$condt1, brfull$carecompost1, use="PAIR")

cor.test(brfull$condt1, brfull$enjoycompost1, use="PAIR")



cor.test(brfullw2use$condt1, brfullw2use$diffcompost2, use="PAIR")

cor.test(brfullw2use$condt1, brfullw2use$carecompost2, use="PAIR")

cor.test(brfullw2use$condt1, brfullw2use$enjoycompost2, use="PAIR")





install.packages("backports")
install.packages("digest")
install.packages("rlang")

install.packages("devtools")   # In case you don't have devtools installed
devtools::install_github("crsh/papaja@devel")
library(papaja)



install.packages('knitr', dependencies = TRUE)
install.packages("kableExtra")

##  @knitr variablesXY

frame<-data.frame("Attitude"=c("Golan Heights (1)", "Golan Heights (2)", "Chop Chop (1)", "Chop Chop (2)", "Psychology (1)","Psychology (2)"
                               ,"Politics (1)", "Politics (1)", "Politics (1)", "Politics (1)","Politics (2)", "Politics (2)", "Politics (2)", "Politics (2)",
                               "Dining Out (1)", "Dining Out (1)", "NFL (1)","NFL (1)", "NFL (2)","NFL (2)","Clothes  shopping (1)", "Clothes  shopping (1)",
                               "Clothes  shopping (2)", "Clothes  shopping (2)", "Yeshiva College	(1)", "Yeshiva College	(2)","Star Wars (1)", "Star Wars (2)" ),
                  "Criterion"=c("Log Golan Heights Eating (2)", "Log Golan Heights Eating (2)", "Log Chop Chop Eating (2)", "Log Chop Chop Eating (2)",
                                "See Psychology Talk (1)", "See Psychology Talk (1)", "Political Consumption (1)", "Political Consumption (2)", "Expect to Vote (1)",
                                "Political Knowledge (1)", "Political Consumption (1)", "Political Consumption (2)","Expect to Vote (1)","Political Knowledge (1)",
                                "Log Dined Out (1)", "Log Dined Out (1)",  "Log Watch NFL (1)", "NFL Knowledge (1)", "Log Watch NFL (1)", "NFL Knowledge (1)", "Log Clothing Spend (1)",
                                "Log Expected Clothing Spend (2)", "Log Clothing Spend (1)", "Log Expected Clothing Spend (2)", "Promote Yeshiva College (1)", "Promote Yeshiva College (1)"
                                , "Seen Force Awakens (1)", "Seen Force Awakens (1)"), 
                  "bNB"=rep(1,28), "CI95NB"=rep(2,28),"b7pt"=rep(3,28), "CI957pt"=rep(4,28), "bBvsNB"=rep(5,28), "CI95BvsNB"=rep(6,28))

variable_labels(frame$bNB)<-"$b$"
variable_labels(frame$CI95NB)<-"$95\\% CI$"
variable_labels(frame$b7pt)<-"$b$"
variable_labels(frame$CI957pt)<-"$95\\% CI$"
variable_labels(frame$bBvsNB)<-"     $b$     "
variable_labels(frame$CI95BvsNB)<-"$95\\% CI$"

apa_table(
  frame,
  placement = 'p',
  font_size="small",
  align=c('l','l',rep('c',6)),
  col_spanners = list(`Non-Branching` = c(3, 4), `Branching-7pt/no lean` = c(5, 6), `Branch vs. Non-Branch As Moderator`=c(7,8)),
  caption = "Criterion Validity Across Branching and Non-Branching Conditions, YC 2-Wave Panel Study",
  note = "Add a note here"
)

frame2<-data.frame("NAME"=c("Golan Heights (1)", "Golan Heights (2)", "Chop Chop (1)", "Chop Chop (2)", "Psychology (1)","Psychology (2)"
                            ,"Politics (1)", "Politics (1)", "Politics (1)", "Politics (1)","Politics (2)", "Politics (2)", "Politics (2)", "Politics (2)",
                            "Dining Out (1)", "Dining Out (1)", "NFL (1)","NFL (1)", "NFL (2)","NFL (2)","Clothes  shopping (1)", "Clothes  shopping (1)",
                            "Clothes  shopping (2)", "Clothes  shopping (2)", "Yeshiva College	(1)", "Yeshiva College	(2)","Star Wars (1)", "Star Wars (2)"), 
                   "MNB"=rep(1,28),
                   "SDNB"=rep(2,28), "M7ptNL"=rep(3,28),"SD7ptN"=rep(4,28), "M7ptL"=rep(5,28), "SD7ptL"=rep(6,28), 
                   "M9ptL"=rep(7,28), "SD9ptL"=rep(8,28))

apa_table(
  frame2,
  placement = 'p',
  col.names = c("", "M", "SD", "M", "SD", "M", "SD", "M", "SD"),
  font_size="small",
  align=c('l',rep('c',8)),
  col_spanners = list(`Non-Branching` = c(2, 3), `Branching-7pt/no lean` = c(4, 5), `Branching-7pt/lean`=c(6,7), `Branching-9pt/lean`=c(8,9)),
  caption = ": Means and Standard Deviations for Attitude Measures in the Nonbranching and Branching Conditions",
  note = "Add a note here"
)


# frame2<-data.frame("Attitude_Object	"=c("Politics", "Clothes shopping", "Dining out", "Movies"), 
#                   "Criterion"=c("A", "B", "C", "D"),
#                   "bNB"=c(3,7,9,5), "CI95NB"=c(1,4,7,3),"b7pt"=c(3,7,9,5), "CI957pt"=c(1,4,7,3), "bBvsNB"=c(3,7,9,5), "CI95BvsNB"=c(1,4,7,3))



# apa_table(
#   frame2,
#   placement = p,
#   col.names = c("Attitude Object", "Criterion", "$b$", "$95\\% CI$", "$b$", "$95\\% CI$", "$b$", "$95\\% CI$"),
#   font_size="small",
#   align=c('l','l',rep('c',6)),
#   col_spanners = list(`Non-Branching` = c(3, 4), `Branching-7pt/no lean` = c(5, 6), `Branch vs. Non-Branch As Moderator`=c(7,8)),
#   caption = ": : Criterion Validity Across Branching and Non-Branching Conditions, SSI Study",
#   note = "Add a note here"
# )

# frame2<-data.frame("Attitude"=c("Golan Heights (1)", "Golan Heights (2)", "Chop Chop (1)", "Chop Chop (2)"), 
#                   "b"=c(3,7,9,5), "*95% CI*"=c(1,4,7,3),"b"=c(3,7,9,5), "95% CI"=c(1,4,7,3), "b"=c(3,7,9,5), "95% CI"=c(1,4,7,3))
# apa_table(
#   frame
#   , col_spanners = list(`Cond. 1` = c(2, 3), `Cond. 2` = c(4, 5))
#   , caption = ": Criterion Validity Across Branching and Non-Branching Conditions, YC 2-Wave Panel Study"
#   , note = "This table was created using apa\\_table()"
# )
# 
# frame3<-data.frame("Attitude"=c("Golan Heights (1)", "Golan Heights (2)", "Chop Chop (1)", "Chop Chop (2)"), 
#                   "b"=c(3,7,9,5), "*95% CI*"=c(1,4,7,3),"b"=c(3,7,9,5), "95% CI"=c(1,4,7,3), "b"=c(3,7,9,5), "95% CI"=c(1,4,7,3))
# apa_table(
#   frame
#   , col_spanners = list(`Cond. 1` = c(2, 3), `Cond. 2` = c(4, 5))
#   , caption = ": Criterion Validity Across Branching and Non-Branching Conditions, YC 2-Wave Panel Study"
#   , note = "This table was created using apa\\_table()"
# )