#Import CSV file
MechaCar <- read.csv('/Users/mdabbous/Desktop/ClassFolder/Module15/MechaCar_Statistical_Analysis/MechaCar_mpg.csv',check.names = F,stringsAsFactors = F)
head(MechaCar)
#generate multiple linear regression model
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=MechaCar) 
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=MechaCar))

#Import CSV file
Suspension_coil <- read.csv('/Users/mdabbous/Desktop/ClassFolder/Module15/MechaCar_Statistical_Analysis/Suspension_Coil.csv',check.names = F,stringsAsFactors = F)
#Calculate total_summary
total_summary <- Suspension_coil %>% summarize(Mean=mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sqrt(var(PSI)))
#Calculate lot summary
Lot_summary <-Suspension_coil %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sqrt(var(PSI)), .groups = 'keep')


#t tests population mean vs 1500 psi
t.test(Suspension_coil$PSI,mu= 1500)

#t tests population mean vs Lot1
Lot1 <- subset(Suspension_coil,Manufacturing_Lot == 'Lot1')
t.test(Lot1$PSI,mu= 1500)

#t tests population mean vs Lot2
Lot2 <- Suspension_coil %>% subset(Manufacturing_Lot == 'Lot2')
t.test(Lot2$PSI,mu= 1500)

#t tests population mean vs Lot3
Lot3 <- Suspension_coil %>% subset(Manufacturing_Lot == 'Lot3')
t.test(Lot3$PSI,mu= 1500)
