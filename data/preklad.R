data <- read.csv("cviceni1_dat.csv",sep="|")
names(data) <- c("ID","Sex","Height","Weight","BMI",
                 "Smoker","Abstinent","DogWalker","Driver",
                 "KometaFan","Optimist","LottoBetting","OnlyChild",
                 "Vegetarian","DoingYoga","CholericPerson","iPhoneOwner",
                 "Elitist","Slovak","Athlete")
data$Sex <- factor(data$Sex)
levels(data$Sex) <- c(" female  ","male","male ","male  ","female","female  ","female\t")
data$Sex <- as.character(data$Sex)

data[data=="ano"] <- "yes"
data[data=="ne"] <- "no"

write.table(data,"seminar1_dat.csv",sep = "|",row.names = FALSE)
dat <- read.csv("seminar1_dat.csv", sep = "|")

#######
data <- read.csv2("brambory.csv")
names(data) <- c("Weight","Variety")
write.csv2(data,"potatoes.csv",row.names = FALSE)

data <- read.csv2("prodavaci.csv")
names(data) <- c("Month","Salesman1","Salesman2","Salesman3")
write.csv2(data,"salesman.csv",row.names = FALSE)

data <- read.csv2("srazlivost.csv")
names(data) <- c("Coagulation","Diet")
write.csv2(data,"coagulation.csv",row.names = FALSE)

data <- read.csv2("mysi.csv")
names(data) <- c("Mouse1","Mouse2","Mouse3")
write.csv2(data,"mice.csv",row.names = FALSE)

data <- read.csv2("data01.csv")

data <- read.csv2("cukrovka.csv")
names(data) <- c("x.from","x.to","n", "Y")
write.csv2(data,"sugarbeet.csv",row.names = FALSE)

######
data <- read.csv2("seno.csv")
names(data) <- c("Soil","Fertilizer","Yield")
data$Soil[data$Soil=="Nor"] <- "Neutral"
data$Soil[data$Soil=="Kys"] <- "Acidic"
write.csv2(data,"hay.csv",row.names = FALSE)

data <- read.csv2("zbozi.csv")
names(data) <- c("Advert","Packaging","Profit")
data$Advert <- c("None","None","Press","Press","PressTV","PressTV")
data$Packaging <- c("Bag","Box","Bag","Box","Bag","Box")
write.csv2(data,"goods.csv",row.names = FALSE)

data <- read.csv2("kukurice.csv")
names(data) <- c("Variety","Fertilizer","Yield")
write.csv2(data,"corn.csv",row.names = FALSE)

########
data <- read.csv2("minuta.csv")
write.csv2(data,"minute.csv",row.names = FALSE)

data <- read.csv2("pole.csv")
names(data) <- c("Yield","Fertilizer")
write.csv2(data,"field.csv",row.names = FALSE)

data <- read.csv2("oktan.csv")
write.csv2(data,"octan.csv",row.names = FALSE)

data <- read.csv2("tlak.csv")
names(data) <- c("Before","After")
write.csv2(data,"bloodpressure.csv",row.names = FALSE)

data <- read.csv2("stroje.csv")
names(data) <- c("Manufacturer","Efficiency")
write.csv2(data,"machines.csv",row.names = FALSE)

data <- read.csv2("ucinnalatka.csv")
names(data) <- c("Supplier","Content")
write.csv2(data,"activesubstance.csv",row.names = FALSE)

data <- read.csv2("minuta2.csv")
write.csv2(data,"minute2.csv",row.names = FALSE)

data <- read.csv2("nikl.csv")
names(data) <- c("Laborant","Nickel")
write.csv2(data,"nickel.csv",row.names = FALSE)

data <- read.csv2("brambory2.csv")
names(data) <- c("Variety","Yield")
write.csv2(data,"potatoes2.csv",row.names = FALSE)

data <- read.csv2("papir.csv")
names(data) <- c("Lab","Smoothness")
write.csv2(data,"paper.csv",row.names = FALSE)

data <- read.csv2("IQvitaminB.csv")
names(data) <- c("VitaminB","Placebo")
write.csv2(data,"IQvitaminB.csv",row.names = FALSE)

data <- read.csv2("krysy.csv")
names(data) <- c("Bandage","Stitches")
write.csv2(data,"rats.csv",row.names = FALSE)

#######
data <- read.csv2("rodiny.csv")
names(data) <- c("NumBoys","NumFamilies")
write.csv2(data,"families.csv",row.names = FALSE)

data <- read.csv2("fronta.csv")
names(data) <- c("WaitingTime","NumCustomers")
write.csv2(data,"line.csv",row.names = FALSE)

data <- read.csv2("vlaky.csv")
names(data) <- c("Trains","Days")
write.csv2(data,"trains.csv",row.names = FALSE)

data <- read.csv2("Brno.csv")
names(data) <- c("Month","Frequency")
data$Month <- c("January","February","March","April","May","June",
                "July","August","September","October","November","December")
write.csv2(data,"Brno.csv",row.names = FALSE)

data <- read.csv2("kostka.csv")
names(data) <- c("Number","Frequency")
write.csv2(data,"dice.csv",row.names = FALSE)

data <- read.csv2("pohotovost.csv")
names(data) <- c("NumPatients","Frequency")
write.csv2(data,"emergency.csv",row.names = FALSE)

data <- read.csv2("kostky.csv")
names(data) <- c("Number6","Frequency")
write.csv2(data,"dice2.csv",row.names = FALSE)

data <- read.csv2("rybnik.csv")
names(data) <- c("Light","NumFish")
data$Light <- c("White","Yellow","Blue","Green","Red")
write.csv2(data,"pond.csv",row.names = FALSE)

data <- read.csv2("supermarkety.csv")
names(data) <- c("Day","Frequency")
data$Day <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
write.csv2(data,"supermarket.csv",row.names = FALSE)

data <- read.csv2("fotbal.csv")
names(data) <- c("Goals","Matches")
write.csv2(data,"football.csv",row.names = FALSE)

#########
data <- read.csv2("domacnosti.csv")
names(data)[1] <- "Data.on.households.in.CSR.from.statistical.yearbook.1957."
data$Data.on.households.in.CSR.from.statistical.yearbook.1957.[1] <- "N = number of household members"
data$Data.on.households.in.CSR.from.statistical.yearbook.1957.[2] <- "I = household income [1000 Kcs/ 3 months]"
data$Data.on.households.in.CSR.from.statistical.yearbook.1957.[3] <- "E = household expenses [1000 Kcs/ 3 months]"
data[5,] <- c("N","I","E")
write.csv2(data,"households.csv",row.names = FALSE)

data <- read.csv2("deti.csv")
names(data) <- c("ID","Weight","Points","Age")
write.csv2(data,"children.csv",row.names = FALSE)

data <- read.csv2("deti2.csv")
names(data) <- c("Age","Memory","IQ","Reading")
write.csv2(data,"children2.csv",row.names = FALSE)

data <- read.csv2("7boj.csv")
write.csv2(data,"heptathlon.csv",row.names = FALSE)

