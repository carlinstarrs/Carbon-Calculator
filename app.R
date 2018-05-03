#Product half life and C flows worksheet
options(scipen = 999)
library(dplyr)
library(ggplot2)
library(reshape2)
library(shiny)
library(rsconnect)

####Set Defaults####
####Wood product half-life table for different wood uses####
wp.logging.residue <- 20 #Default 20 year half-life. Based on Harmon et al. 2001																
wp.wood.products <- 45 #Default 45 year half-life based on weighted average of values in Skog (2008) - Single Family Homes - 80 yr; Multi unit - 50 yr; Remodeling - 30yr; Lumber half-life for long term products is ~60 years for structural lumber and plywood products; 2/3 of CA sawmill products are structural lumber and plywood, 1/3 is other products. Assuming a 15 year half-life for other products, the overall half life is 45 yr for output of California sawmills																
wp.bioenergy <- Inf #Process heat and grid electricity replaces 'always available' baseload energy that (mainly from fossil fuels) and avoids backup requirement of other renewables (Joskow 2011) 																
wp.inital.wood.products.carbon <- 100 #Indexed to 100															

####Post-consumer product alocation and proportion of wood as permanently inert in landfill####
pc.waste <- 0.1 #Default 0.1. Based on ARB Scoping Plan components: Renewable Portfolio Standard and Recyling and Waste																
pc.energy <- 0.25 #Default 0.25. Based on interpretration of AB 32 Scoping Plan: Renewable Portfolio Standard and Recycling and Waste goals																
pc.engineered.landfill.storage <- 0.65 #Default 0.65. Based on interpretation of AB 32 Scoping Plan: Renewable Portfolio Standard and Recycling and Waste																
pc.wood.permanently.inert.in.landfill <- 0.77 #Default 0.77. Based on Skog 2008		

####Substition####
substitition.factor <- 1.00 #Default 1. based on Sathre and O'Connor 2010. Only applied to 57% of wood products that go into buildings (FPL-GTR-1999,2011). Packaging, short term construction wood, etc is not alloted substitution benefits.																

#Create table

flows <- data.frame("Year" = 0:160)
flows$Logging_Residue_Slash <- 100 * (0.5) ^ (flows$Year/wp.logging.residue)
flows$Wood_Products_In_Use <- 100 * (0.5) ^ (flows$Year/wp.wood.products)

flows$Wood_Products_Change[1] <- 0
for (i in 2:nrow(flows)) {
  flows$Wood_Products_Change[i] <- flows$Wood_Products_In_Use[i-1]-flows$Wood_Products_In_Use[i]
}

flows$Post_Consumer_Waste <- flows$Wood_Products_Change * pc.waste
flows$Post_Consumer_Energy <- flows$Wood_Products_Change * pc.energy
flows$To_Engineered_Landfill_Storage <- flows$Wood_Products_Change * pc.engineered.landfill.storage
flows$Wood_Permanently_Inert_In_Landfill <- flows$To_Engineered_Landfill_Storage * pc.wood.permanently.inert.in.landfill
flows$Permanent_Landfill_Storage[1] <- 0

for (i in 2:nrow(flows)) {
  flows$Permanent_Landfill_Storage[i] <- flows$Wood_Permanently_Inert_In_Landfill[i] + flows$Permanent_Landfill_Storage[i-1]
}

flows$Landfill_Waste <- flows$To_Engineered_Landfill_Storage - flows$Wood_Permanently_Inert_In_Landfill
flows$Consumer_and_Landfill_Waste <- flows$Landfill_Waste + flows$Post_Consumer_Waste
flows$Substition <- substitition.factor * flows$Wood_Products_In_Use[1]
flows$Effective_Product_and_Landfill_Storage <- flows$Wood_Products_In_Use + flows$Permanent_Landfill_Storage

flows$Accumulated_Post_Consumer_Energy[1] <- 0
for (i in 2:nrow(flows)) {
  flows$Accumulated_Post_Consumer_Energy[i] <- flows$Post_Consumer_Energy[i] + flows$Accumulated_Post_Consumer_Energy[i-1]
}

flows$Effective_C_Remaining_and_Energy <- flows$Accumulated_Post_Consumer_Energy + flows$Wood_Products_In_Use + flows$Permanent_Landfill_Storage

flows$Hayes_2012[1] <- 100
for (i in 2:nrow(flows)) {
  flows$Hayes_2012[i] <- flows$Hayes_2012[i-1]-1
}

flows$Hayes_2012[flows$Hayes_2012 < 0] <- 0

#Grow Stand

# thin.year <- 40
# harvest.year <- 80
# 
# 
# #Thin percentage of total live biomass
# thin.pct.total.live <- 0.4
# 
# #Logging - Default 0.75 used, 0.25 unused logging residues left to decompose on site. 
# #All unused is like burning all residues
# logging.residue.bioenergy.used <- 0 
# logging.residue.products.used <- 0
# logging.residue.unused <- 1
# 
# #Thinning - Based on partial thinnings (72% chips, 28% sawlogs) in Stewart and Nakamura (2012)
# thinning.residue.bioenergy.used <- 0.72
# thinning.residue.products.used <- 0.28
# thinning.residue.unused <- 0
# 
# #Sawmill - Default 0.24 usedfor energy,  0.75 into products, 0.01 waste
# sawmill.residue.bioenergy.used <- 0.24
# sawmill.residue.products.used <- 0.75
# sawmill.residue.unused <- 0.01
# 
# #Substitution - 57% of wood products go into buildings where substitution benefits 
# #are significant (FPL-GTR-199, McKeever 2011)
# substitution.wood.products <- 0.57
# substitution.unused <- 0.43

thin.year <- c(40,120)
harvest.year <- c(80, 160)
max.year <- 240
forest.type <-  "Mixed Conifer"

create_growth_table <- function(forest.type, thin.year, harvest.year, max.year, thin.pct.total.live, 
                                logging.residue.bioenergy.used, logging.residue.products.used, logging.residue.unused,
                                thinning.residue.bioenergy.used, thinning.residue.products.used, thinning.residue.unused, 
                                sawmill.residue.bioenergy.used, sawmill.residue.products.used, sawmill.residue.unused,
                                substitution.wood.products, substitution.unused) {
  
  if(missing(thin.pct.total.live)) {
    thin.pct.total.live <- 0.4
  }
  
  if(missing(logging.residue.bioenergy.used)) {
    logging.residue.bioenergy.used <- 0.75
  }
  if(missing(logging.residue.products.used)) {
    logging.residue.products.used <- 0
  }
  if(missing(logging.residue.unused)) {
    logging.residue.unused <- 0.25
  }
  if(missing(thinning.residue.bioenergy.used)) {
    thinning.residue.bioenergy.used <- 0.72
  }
  if(missing(thinning.residue.products.used)) {
    thinning.residue.products.used <- 0.28
  }
  if(missing(thinning.residue.unused)) {
    thinning.residue.unused <- 0
  }
  if(missing(sawmill.residue.bioenergy.used)) {
    sawmill.residue.bioenergy.used <- 0.24
  }
  if(missing( sawmill.residue.products.used)) {
    sawmill.residue.products.used<- 0.75
  }
  if(missing(sawmill.residue.unused)) {
    sawmill.residue.unused <- 0.01
  }
  if(missing(substitution.wood.products)) {
    substitution.wood.products <- 0.57
  }
  if(missing(substitution.unused)) {
    substitution.unused<- 0.43
  }
  if(missing(max.year)) {
    max.year <- 240
  }
  growth <- read.csv("Von_Bertalanffy_growth_equation_coefficients.csv")
  
  growth_table <- data.frame("Project Year" = -80:160, "Year" = 0:max.year)
  growth_table$Let_Grow_Forest_Mg_per_ha <- growth$Coefficient_a[growth$Veg_Type == forest.type & growth$Owner_Type == "All_CMC"] * (1 - exp(-growth$Coefficient_b[growth$Veg_Type == forest.type & growth$Owner_Type == "All_CMC"] * growth_table$Year))^3
  
  thin.schedule <- data.frame("Year" = thin.year)
  thin.schedule$Activity <- "Thin"
  
  harvest.schedule <- data.frame("Year" = c(harvest.year, harvest.year * 2))
  harvest.schedule$Activity <- "Harvest"
  
  cycle.table <- rbind(thin.schedule, harvest.schedule)
  cycle.table <- cycle.table[order(cycle.table$Year),]
  
  harvest.cycles <- cycle.table$Year[cycle.table$Activity == "Harvest"]
  regen.table <- data.frame("Project Year" = -80:160, "Year" = 0:max.year)
  regen.table$Regenerated_Forest <- NA
  # regen.table$Regenerated_Forest[0:which(regen.table$Year == harvest.cycles[1])] <- growth$Coefficient_a[growth$Veg_Type == forest.type & growth$Owner_Type == "All_CMC"] * (1 - exp(-growth$Coefficient_b[growth$Veg_Type == forest.type & growth$Owner_Type == "All_CMC"] * regen.table$Year[0:which(regen.table$Year == harvest.cycles[1])]))^3
  
  for (i in 1:length(harvest.cycles)) {
    if (!is.na(harvest.cycles[i + 1])) {
      end <- harvest.cycles[i+1] + 1
    } else {
      end <- max.year
    }
    if(i == 1) {
      regen.table$Regenerated_Forest[which(regen.table$Year == harvest.cycles[i]):which(regen.table$Year == end)] <- growth$Coefficient_a[growth$Veg_Type == forest.type & growth$Owner_Type == "Pvt_CMC"] * (1 - exp(-growth$Coefficient_b[growth$Veg_Type == forest.type & growth$Owner_Type == "Pvt_CMC"] * growth_table$Year[1:length(regen.table$Regenerated_Forest[which(regen.table$Year == harvest.cycles[i]):which(regen.table$Year == end)])]))^3
    } else {
      regen.table$Regenerated_Forest[which(regen.table$Year == (harvest.cycles[i] + 1)):which(regen.table$Year == end)] <- growth$Coefficient_a[growth$Veg_Type == forest.type & growth$Owner_Type == "Pvt_CMC"] * (1 - exp(-growth$Coefficient_b[growth$Veg_Type == forest.type & growth$Owner_Type == "Pvt_CMC"] * growth_table$Year[1:length(regen.table$Regenerated_Forest[which(regen.table$Year == (harvest.cycles[i] + 1)):which(regen.table$Year == end)])]))^3
    }
  }
  
  #output <- growth_table[0:which(growth_table$Year == cycle.table$Year[1]),]
  
  # year <- cycle.table$Year[1]
  
  if (!is.na(thin.year)) {
    thin <- function(year) {
      data <- growth_table[which(growth_table$Year == year): (which(growth_table$Year == year) + 120),]
      last.mg.ha <- data$Let_Grow_Forest_Mg_per_ha[1]
      data$Let_Grow_Forest_Mg_per_ha[2] <- growth_table$Let_Grow_Forest_Mg_per_ha[growth_table$Year == data$Year[2]] - thin.pct.total.live * data$Let_Grow_Forest_Mg_per_ha[1]
      for (i in 3:41) {
        data$Let_Grow_Forest_Mg_per_ha[i] <- data$Let_Grow_Forest_Mg_per_ha[i-1] + (growth_table$Let_Grow_Forest_Mg_per_ha[growth_table$Year == data$Year[41]] - data$Let_Grow_Forest_Mg_per_ha[2])/39
      }
      data$Logging_Slash_Left<- (thin.pct.total.live * last.mg.ha * 0.72) * logging.residue.unused * flows$Logging_Residue_Slash[1:nrow(data)]/100
      data$Energy_from_logging_residues <- (thin.pct.total.live *last.mg.ha * 0.72) * logging.residue.bioenergy.used
      data$Energy_from_sawmill_residues <- (thin.pct.total.live *last.mg.ha * 0.28) * sawmill.residue.bioenergy.used
      data$Wood_Products <- (thin.pct.total.live *last.mg.ha * 0.28) * sawmill.residue.products.used * flows$Wood_Products_In_Use[1:nrow(data)]/100
      data$Landfill <- (thin.pct.total.live *last.mg.ha * 0.28) * sawmill.residue.products.used * flows$Permanent_Landfill_Storage[1:nrow(data)]/100
      data$Energy_from_post_consumer_residues <- (thin.pct.total.live *last.mg.ha * 0.28) * sawmill.residue.products.used * flows$Accumulated_Post_Consumer_Energy[1:nrow(data)]/100
      data$Substitution_Benefits  <-data$Wood_Products[1] * substitution.wood.products
      data$Activity <- paste("Thin @ Year", year)
      
      return(data)
    }
    
    thinned <- thin(thin.year[1])
  }
  
  
  # year <- cycle.table$Year[cycle.table$Activity == "Harvest"][1]
  # 
  # if(length(cycle.table$Year[cycle.table$Activity == "Harvest"]) > 1) {
  #   harvest.interval <- cycle.table$Year[cycle.table$Activity == "Harvest"][2] - cycle.table$Year[cycle.table$Activity == "Harvest"][1]
  # } else {
  #   harvest.interval <- cycle.table$Year[cycle.table$Activity == "Harvest"][1]
  # }
  
  harvest <- function(year) {
    if (!is.na(thin.year)){
      last.mg.ha <-thinned$Let_Grow_Forest_Mg_per_ha[thinned$Year == year]
    } else {
      last.mg.ha <- growth_table$Let_Grow_Forest_Mg_per_ha[which(growth_table$Year == year)]
    }
    data <- growth_table[which(growth_table$Year == year):which(growth_table$Year == max.year),]
    
    data$Logging_Slash_Left <- last.mg.ha * 0.4 * logging.residue.unused * flows$Logging_Residue_Slash[1:nrow(data)]/100
    data$Energy_from_logging_residues <- (last.mg.ha  * ((0.4* logging.residue.bioenergy.used)) - (last.mg.ha  * 0.03))
    data$Energy_from_sawmill_residues <- (last.mg.ha  * ((0.6* sawmill.residue.bioenergy.used)))
    data$Wood_Products <- last.mg.ha  * (0.6 * sawmill.residue.products.used) * flows$Wood_Products_In_Use[1:nrow(data)]/100
    data$Landfill <- last.mg.ha  * 0.6  * flows$Permanent_Landfill_Storage[1:nrow(data)]/100
    data$Energy_from_post_consumer_residues <- last.mg.ha  * 0.6  * flows$Accumulated_Post_Consumer_Energy[1:nrow(data)]/100
    data$Substitution_Benefits <- NA
    if (year * 2 < 240) {
      end <- year * 2
    } else {
      end <- 240
    }
    data$Substitution_Benefits[1:which(data$Year == end)]  <- data$Wood_Products[1] * substitution.wood.products
    data$Substitution_Benefits[which(data$Year == (end + 1)):which(data$Year == max.year)]  <- data$Wood_Products[which(data$Year == (end + 1))]
    
    data$Activity <- paste("Harvest @ Year", year)
    
    data$Let_Grow_Forest_Mg_per_ha <- NULL
    
    return(data)
  }
  
  harvest1 <- harvest(harvest.year[1])
  
  harvest2 <- function(year) {
    last.mg.ha <- growth$Coefficient_a[growth$Veg_Type == forest.type & growth$Owner_Type == "Pvt_CMC"] * (1 - exp(-growth$Coefficient_b[growth$Veg_Type == forest.type & growth$Owner_Type == "Pvt_CMC"] * harvest.year[1]))^3
    data <- growth_table[which(growth_table$Year == year + 1):which(growth_table$Year == max.year),]
    data$Logging_Slash_Left<- last.mg.ha * 0.4 * logging.residue.unused * flows$Logging_Residue_Slash[1:nrow(data)]/100
    data$Energy_from_logging_residues <- (last.mg.ha  * ((0.4* logging.residue.bioenergy.used)) - (last.mg.ha  * 0.03))
    data$Energy_from_sawmill_residues <- (last.mg.ha  * ((0.6* sawmill.residue.bioenergy.used)))
    data$Wood_Products <- last.mg.ha  * (0.6 * sawmill.residue.products.used) * flows$Wood_Products_In_Use[1:nrow(data)]/100
    data$Landfill <- last.mg.ha  * 0.6  * flows$Permanent_Landfill_Storage[1:nrow(data)]/100
    data$Energy_from_post_consumer_residues <- last.mg.ha  * 0.6  * flows$Accumulated_Post_Consumer_Energy[1:nrow(data)]/100
    data$Substitution_Benefits <- NA
    if (year * 2 < 240) {
      end <- year * 2
    } else {
      end <- 240
    }
    data$Substitution_Benefits[1:which(data$Year == end)]  <- data$Wood_Products[1] * substitution.wood.products
    
    data$Activity <- paste("Harvest @ Year", year)
    
    data$Let_Grow_Forest_Mg_per_ha <- NULL
    
    return(data)
  }
  harvest2 <- harvest2(harvest.year*2)
  
  if (!is.na(thin.year)){
    final <- merge(thinned, harvest1, all = TRUE)
    final <- merge(final, harvest2, all = TRUE)
    final[is.na(final)] <- 0
    
    final2 <- final %>% group_by(Year) %>% summarise(Logging_Slash_Left = sum(Logging_Slash_Left), 
                                                     Energy_from_logging_residues = sum(Energy_from_logging_residues), 
                                                     Energy_from_sawmill_residues = sum(Energy_from_sawmill_residues), 
                                                     Wood_Products = sum(Wood_Products), 
                                                     Landfill = sum(Landfill),
                                                     Energy_from_post_consumer_residues = sum(Energy_from_post_consumer_residues),
                                                     Substitution_Benefits = sum(Substitution_Benefits), 
                                                     Let_Grow_Forest_Mg_per_ha = sum(Let_Grow_Forest_Mg_per_ha))
    
    growth_table2 <- growth_table[!(growth_table$Year %in% final2$Year),]
    final2 <- merge(final2, growth_table2, all = TRUE)
    
    final2$Let_Grow_Forest_Mg_per_ha[which(final2$Year == 161):which(final2$Year == 240)] <- growth_table$Let_Grow_Forest_Mg_per_ha[which(growth_table$Year == 161):which(growth_table$Year == 240)]
    
  } else {
    final <- merge(harvest1, harvest2, all = TRUE)
    final[is.na(final)] <- 0
    
    
    final2 <- final %>% group_by(Year) %>% summarise(Logging_Slash_Left = sum(Logging_Slash_Left), 
                                                     Energy_from_logging_residues = sum(Energy_from_logging_residues), 
                                                     Energy_from_sawmill_residues = sum(Energy_from_sawmill_residues), 
                                                     Wood_Products = sum(Wood_Products), 
                                                     Landfill = sum(Landfill),
                                                     Energy_from_post_consumer_residues = sum(Energy_from_post_consumer_residues),
                                                     Substitution_Benefits = sum(Substitution_Benefits))
    
    final2 <- merge(final2, growth_table, all = TRUE)
  }
  
  if (any(grepl("Project.Year", names(final2)))) {
    final2$Project.Year <- NULL
    final3 <- merge(final2, regen.table, by = c("Year"), all = TRUE)
  } else {
    final3 <- merge(final2, regen.table, by = "Year", all = TRUE)
  }
  final3[is.na(final3)] <- 0
  #let.grow.mgC.ha <- data.frame("Scenario" = NA, "Cycles" = NA, "Type" = NA, "MgC/ha" = NA, "MgC/ha/yr" = NA)
  scenario.table <- data.frame("Scenario" =  rep(c("Let Grow", "Managed"), each = 3), "Cycles" = rep(c(1.5, 2, 3), 2))
  scenario.table$MgC.ha <- NA
  scenario.table$MgC.ha[scenario.table$Scenario == "Let Grow" & scenario.table$Cycles == 1.5] <- sum(final3$Let_Grow_Forest_Mg_per_ha[which(final3$Year == 0):which(final3$Year == harvest.year[1] * 1.5)])
  scenario.table$MgC.ha[scenario.table$Scenario == "Let Grow" & scenario.table$Cycles == 2] <- sum(final3$Let_Grow_Forest_Mg_per_ha[which(final3$Year == 0):which(final3$Year == harvest.year[1] * 2)])
  scenario.table$MgC.ha[scenario.table$Scenario == "Let Grow" & scenario.table$Cycles == 3] <- sum(final3$Let_Grow_Forest_Mg_per_ha[which(final3$Year == 0):which(final3$Year == harvest.year[1] * 3)])
  
  pattern <- c("Substitution_Benefits", "Landfill", "Wood_Products", "Energy_from_post_consumer_residues", "Energy_from_sawmill_residues", "Energy_from_logging_residues", "Regenerated_Forest", "Logging_Slash_Left")
  sum.columns <- which(grepl(paste0(pattern, collapse = "|"), names(final3)))
  scenario.table$MgC.ha[scenario.table$Scenario == "Managed" & scenario.table$Cycles == 1.5] <- sum(final3[which(final3$Year == 0):which(final3$Year == harvest.year[1] * 1.5), sum.columns]) + sum(final3$Let_Grow_Forest_Mg_per_ha[which(final3$Year == 0):which(final3$Year == harvest.year[1])])
  scenario.table$MgC.ha[scenario.table$Scenario == "Managed" & scenario.table$Cycles == 2] <- sum(final3[which(final3$Year == 0):which(final3$Year == harvest.year[1] * 2), sum.columns]) + sum(final3$Let_Grow_Forest_Mg_per_ha[which(final3$Year == 0):which(final3$Year == harvest.year[1])])
  scenario.table$MgC.ha[scenario.table$Scenario == "Managed" & scenario.table$Cycles == 3] <- sum(final3[which(final3$Year == 0):which(final3$Year == harvest.year[1] * 3), sum.columns]) + sum(final3$Let_Grow_Forest_Mg_per_ha[which(final3$Year == 0):which(final3$Year == harvest.year[1])])
  
  scenario.table$MgC.ha.yr <- scenario.table$MgC.ha/(scenario.table$Cycles*harvest.year)
  
  ratio.table <- data.frame("Cycles" = rep(c(1.5, 2, 3), 1))
  ratio.table$Carbon.Ratio <- round(scenario.table[scenario.table$Scenario == "Managed",3]/scenario.table[scenario.table$Scenario == "Let Grow",3],2)
  
  scenario.table$MgC.ha <- formatC(scenario.table$MgC.ha, format = "d", big.mark = ",")
  scenario.table$MgC.ha.yr <- formatC(scenario.table$MgC.ha.yr, format = "d", big.mark = ",")
  scenario.table$Cycles <- formatC(scenario.table$Cycles, format = "fg", big.mark = ",")
  
  final4 <- final3
  final4$Year <- NULL
  final4$Let_Grow_Forest_Mg_per_ha <- NULL
  
  final5 <- melt(final4, id.vars = "Project.Year")
  final5$variable <- as.factor(final5$variable)
  final5$variable <- factor(final5$variable, levels(final5$variable)[c(7,5,4,6,3,2,8,1)])
  
  custom_theme <- theme_set(theme_bw(base_size = 20)) +
    theme(plot.title = element_text(size = 16, hjust = 0.5),
          panel.grid.major = element_blank(), 
          axis.text.x=element_text(size=14), 
          axis.text.y=element_text(size=14),
          axis.title = element_text(size = 16),
          strip.text = element_text(size = 14),
          legend.position="right",
          legend.text = element_text(size = 16),
          legend.title = element_text(size = -1, face = "bold"))
  
  
  graph <- ggplot() + 
    geom_area(data = final5, aes(x = Project.Year, y = value, fill = variable, group = variable)) + 
    geom_line(data = final3, aes(y = Let_Grow_Forest_Mg_per_ha, x = Project.Year, color = "blue"), size = 2) + 
    scale_x_continuous(breaks = c(seq(-80, 160, 20)), expand = c(0,0)) + 
    scale_y_continuous(breaks = c(seq(0, ((sum(final5$value))%/%50)*50, 50)),expand = c(0,0)) + 
    scale_color_manual(name = "", values = "#ffc900", labels = "Let Grow Forest") + 
    scale_fill_manual(name = "", values = c("#fc4628", "#383134", "#a95a00", "#AEB1B1", "#5faaa7", "#066781", "#4a811c", "#dd9d03")) +
    labs(x="Project Year", y=paste("Climate Benefits", "(tons of carbon per hectare)", sep = "\n"), 
         title=paste(paste("Climate Benefits of California", forest.type, "Forest:"), "Forest and Products vs Let Grow Forest", sep = "\n")) + 
    custom_theme
  
  graph
  
  mylist <- vector(mode="list", length=3)
  mylist[[1]] <- final3
  mylist[[2]] <- scenario.table
  mylist[[3]] <- graph
  mylist[[4]] <- ratio.table
  
  return(mylist)
}



# MIX_CON_EVEN_T40_H80_U00 <- create_growth_table("Mixed Conifer", 40, 80, logging.residue.bioenergy.used = 0, logging.residue.products.used = 0, logging.residue.unused = 1)
# 
# MIX_CON_EVEN_T40_H80_U25 <- create_growth_table("Mixed Conifer", 40, 80, logging.residue.bioenergy.used = 0.25, logging.residue.products.used = 0, logging.residue.unused = 0.75)
# 
# MIX_CON_EVEN_T40_H80_U75 <- create_growth_table("Mixed Conifer", 40, 80)
# 
# MIX_CON_EVEN_T00_H80_U75 <- create_growth_table("Mixed Conifer", NA, 80)
# 
# PP_EVEN_T40_H80_U00 <- create_growth_table("Ponderosa Pine", 40, 80, logging.residue.bioenergy.used = 0, logging.residue.products.used = 0, logging.residue.unused = 1)
# 
# PP_EVEN_T40_H80_U25 <- create_growth_table("Ponderosa Pine", 40, 80, logging.residue.bioenergy.used = 0.25, logging.residue.products.used = 0, logging.residue.unused = 0.75)
# 
# PP_EVEN_T40_H80_U75 <- create_growth_table("Ponderosa Pine", 40, 80)
# 
# PP_EVEN_T00_H80_U75 <- create_growth_table("Ponderosa Pine", NA, 80)
# 
# DF_EVEN_T40_H80_U00 <- create_growth_table("Douglas-fir", 40, 80, logging.residue.bioenergy.used = 0, logging.residue.products.used = 0, logging.residue.unused = 1)
# 
# DF_EVEN_T40_H80_U25 <- create_growth_table("Douglas-fir", 40, 80, logging.residue.bioenergy.used = 0.25, logging.residue.products.used = 0, logging.residue.unused = 0.75)
# 
# DF_EVEN_T40_H80_U75 <- create_growth_table("Douglas-fir", 40, 80)
# 
# DF_EVEN_T00_H80_U75 <- create_growth_table("Douglas-fir", NA, 80)



ui <- fluidPage(
  titlePanel(""),
  #Sidebar layout with input and output definitions
  fluidRow(
    column(12, 
           tabsetPanel(
             type = "tabs", 
             tabPanel("Graph", plotOutput(outputId = "graph")), 
             tabPanel("Table", 
                      fluidRow(
                        column(6,
                               h4("Carbon benefits of treatment over three harvest cycles"), 
                               tableOutput(outputId = "scenario.table")
                        ),
                        column(6,
                               h4("Managed Forest to Let Grow Carbon Ratio"), 
                               tableOutput(outputId = "ratio.table"))
                      )
             )
           )
    )
  ),
  fluidRow(
    column(6,
           #Sidebar panel for inputs
           wellPanel (
             h4("Treatment Specifications"),
             selectInput("forest.type", "Forest Type Selection:", c("Mixed Conifer", "Ponderosa Pine", "Douglas-fir", "Redwood"), selected = "Mixed Conifer"),
             checkboxInput("thin", "Thin at Year 40?", value = TRUE),
             numericInput("thin.pct.total.live", "Thin proportion of total live biomass", min = 0, max = 1, value = 0.4, step = 0.1)
           )
    ),
    column(6,
           wellPanel (
             h4("Substitution benefits for wood products"),
             helpText("57% of wood products go into buildings where substitution benefits are significant (FPL-GTR-199, McKeever 2011)"),
             sliderInput("substitution", "Proportion of wood products used for buildings", min = 0, max = 1, value = 0.57)
             # numericInput("substitution.wood.products", "Wood Products", min = 0, max = 1, value = 0.57, step = 0.1),
             # numericInput("substitution.unused", "Unused", min = 0, max = 1, value = 0.43, step = 0.1)
           )
    )
    
  ),
  fluidRow(
    column(4,
           wellPanel (
             h4("Sawmill Residue Propotions"),
             helpText("Default 0.24 used for energy, 0.75 into products, 0.01 waste"),
             numericInput("sawmill.residue.bioenergy.used", "Bioenergy", min = 0, max = 1, value = 0.24, step = 0.1),
             numericInput("sawmill.residue.products.used", "Products", min = 0, max = 1, value = 0.75, step = 0.1),
             numericInput("sawmill.residue.unused", "Unused", min = 0, max = 1, value = 0.01, step = 0.1)
           )
    ),
    
    column(4,
           wellPanel(
             h4("Thinning Residue Propotions"),
             helpText("Based on partial thinnings (72% chips, 28% sawlogs) in Stewart and Nakamura (2012)"),
             numericInput("thinning.residue.bioenergy.used", "Bioenergy", min = 0, max = 1, value = 0.72, step = 0.1),
             numericInput("thinning.residue.products.used", "Products", min = 0, max = 1, value = 0.28, step = 0.1),
             numericInput("thinning.residue.unused", "Unused", min = 0, max = 1, value = 0, step = 0.1)
           )
    ),
    column(4,
           wellPanel (
             h4("Logging Residue Propotions"),
             helpText("Default 0.75 used, 0.25 unused logging residues left to decompose on site."),
             numericInput("logging.residue.bioenergy.used", "Bioenergy", min = 0, max = 1, value = 0.75, step = 0.1),
             numericInput("logging.residue.products.used", "Products", min = 0, max = 1, value = 0, step = 0.1),
             numericInput("logging.residue.unused", "Unused", min = 0, max = 1, value = 0.25, step = 0.1)
           )
    )
    
  )
  
  # fluidRow(
  #   column(12,
  #          wellPanel (
  #            submitButton("Update")
  #          )
  #          )
  # )
)

server <- function(input, output){
  
  output$graph <- renderPlot({
    if(input$thin == TRUE) {
      thin.year <- 40
    } else {
      thin.year <- NA
    }
    
    data <- create_growth_table(forest.type = input$forest.type, 
                                thin.year = thin.year, 
                                harvest.year = 80,
                                max.year = 240,
                                thin.pct.total.live = input$thin.pct.total.live, 
                                logging.residue.bioenergy.used = input$logging.residue.bioenergy.used, 
                                logging.residue.products.used = input$logging.residue.products.used,
                                logging.residue.unused = input$logging.residue.unused,
                                thinning.residue.bioenergy.used = input$thinning.residue.bioenergy.used, 
                                thinning.residue.products.used = input$thinning.residue.products.used,
                                thinning.residue.unused = input$thinning.residue.unused,
                                sawmill.residue.bioenergy.used = input$sawmill.residue.bioenergy.used,
                                sawmill.residue.products.used = input$sawmill.residue.products.used,
                                sawmill.residue.unused = input$sawmill.residue.unused,
                                substitution.wood.products = input$substitution,
                                substitution.unused = 1-input$substitution)
    
    data[[3]]
  })
  
  output$scenario.table <- renderTable({
    if(input$thin == TRUE) {
      thin.year <- 40
    } else {
      thin.year <- NA
    }
    
    data <- create_growth_table(forest.type = input$forest.type, 
                                thin.year = thin.year, 
                                harvest.year = 80,
                                max.year = 240,
                                thin.pct.total.live = input$thin.pct.total.live, 
                                logging.residue.bioenergy.used = input$logging.residue.bioenergy.used, 
                                logging.residue.products.used = input$logging.residue.products.used,
                                logging.residue.unused = input$logging.residue.unused,
                                thinning.residue.bioenergy.used = input$thinning.residue.bioenergy.used, 
                                thinning.residue.products.used = input$thinning.residue.products.used,
                                thinning.residue.unused = input$thinning.residue.unused,
                                sawmill.residue.bioenergy.used = input$sawmill.residue.bioenergy.used,
                                sawmill.residue.products.used = input$sawmill.residue.products.used,
                                sawmill.residue.unused = input$sawmill.residue.unused,
                                substitution.wood.products = input$substitution,
                                substitution.unused = 1-input$substitution)
    
    data[[2]]
  })
  
  output$ratio.table <- renderTable({
    if(input$thin == TRUE) {
      thin.year <- 40
    } else {
      thin.year <- NA
    }
    
    data <- create_growth_table(forest.type = input$forest.type, 
                                thin.year = thin.year, 
                                harvest.year = 80,
                                max.year = 240,
                                thin.pct.total.live = input$thin.pct.total.live, 
                                logging.residue.bioenergy.used = input$logging.residue.bioenergy.used, 
                                logging.residue.products.used = input$logging.residue.products.used,
                                logging.residue.unused = input$logging.residue.unused,
                                thinning.residue.bioenergy.used = input$thinning.residue.bioenergy.used, 
                                thinning.residue.products.used = input$thinning.residue.products.used,
                                thinning.residue.unused = input$thinning.residue.unused,
                                sawmill.residue.bioenergy.used = input$sawmill.residue.bioenergy.used,
                                sawmill.residue.products.used = input$sawmill.residue.products.used,
                                sawmill.residue.unused = input$sawmill.residue.unused,
                                substitution.wood.products = input$substitution,
                                substitution.unused = 1-input$substitution)
    
    data[[4]]
  })
}


shinyApp(ui = ui, server = server)

#rsconnect::deployApp('G:/Dropbox/Carlin/GitHub/Carbon-Calculator')
