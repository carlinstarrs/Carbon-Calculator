#Product half life and C flows worksheet
options(scipen = 999)
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)
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
create_growth_table <- function(forest.type, even.aged, thin.year, harvest.year, max.year, thin.pct.total.live, 
                                logging.residue.bioenergy.used, logging.residue.products.used, logging.residue.unused,
                                thinning.residue.bioenergy.used, thinning.residue.products.used, thinning.residue.unused, 
                                sawmill.residue.bioenergy.used, sawmill.residue.products.used, sawmill.residue.unused,
                                substitution.wood.products, substitution.unused) {
  
  
  if(missing(even.aged)) {
    even.aged <- TRUE
  }
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
  
  if(even.aged == TRUE) {
    harvest.schedule <- data.frame("Year" = c(harvest.year, harvest.year * 2))
    harvest.schedule$Activity <- "Harvest"
  } else {
    thin.year <- 40
    cycles <- max.year%/%20
    cycles <- 20 * (1:cycles)
    cycles <- cycles[cycles > harvest.year]
    harvest.schedule <- data.frame("Year" = c(harvest.year, cycles[1:(length(cycles) - 1)]))
    harvest.schedule$Activity <- "Harvest"
  }
  
  if(!is.na(thin.year)) {
    thin.schedule <- data.frame("Year" = thin.year)
    thin.schedule$Activity <- "Thin"
    cycle.table <- rbind(thin.schedule, harvest.schedule)
  } else {
    cycle.table <- harvest.schedule
  }
  cycle.table <- cycle.table[order(cycle.table$Year),]
  
  harvest.cycles <- cycle.table$Year[cycle.table$Activity == "Harvest"]

  thin <- function(year, even.aged) {
    if(missing(even.aged)) {
      even.aged <- TRUE
    }
    if(even.aged == TRUE) {
      data <- growth_table[which(growth_table$Year == year): (which(growth_table$Year == year) + 120),]
      last.mg.ha <- data$Let_Grow_Forest_Mg_per_ha[1]
      data$Let_Grow_Forest_Mg_per_ha[2] <- growth_table$Let_Grow_Forest_Mg_per_ha[growth_table$Year == data$Year[2]] - thin.pct.total.live * data$Let_Grow_Forest_Mg_per_ha[1]
      for (i in 3:41) {
        data$Let_Grow_Forest_Mg_per_ha[i] <- data$Let_Grow_Forest_Mg_per_ha[i-1] + (growth_table$Let_Grow_Forest_Mg_per_ha[growth_table$Year == data$Year[41]] - data$Let_Grow_Forest_Mg_per_ha[2])/39
      }
    } else {
      data <- growth_table
      last.mg.ha <- data$Let_Grow_Forest_Mg_per_ha[which(data$Year == year)]
      data$Let_Grow_Forest_Mg_per_ha[(which(data$Year == year) + 1)] <- growth_table$Let_Grow_Forest_Mg_per_ha[growth_table$Year == data$Year[(which(data$Year == year) + 1)]] - thin.pct.total.live * data$Let_Grow_Forest_Mg_per_ha[which(data$Year == year)]
      for (i in (which(data$Year == year) + 2):(which(data$Year == year) + 40)) {
        data$Let_Grow_Forest_Mg_per_ha[i] <- data$Let_Grow_Forest_Mg_per_ha[i-1] + (growth_table$Let_Grow_Forest_Mg_per_ha[growth_table$Year == (which(data$Year == year) + 39)] - data$Let_Grow_Forest_Mg_per_ha[(which(data$Year == year) + 1)])/39
      }
      data2 <- data[which(data$Year == 0):which(data$Year == year),]
      data <- data[which(data$Year >= year),]
    }
    
    data$Logging_Slash_Left<- (thin.pct.total.live * last.mg.ha * 0.72) * logging.residue.unused * flows$Logging_Residue_Slash[1:nrow(data)]/100
    data$Energy_from_logging_residues <- (thin.pct.total.live *last.mg.ha * 0.72) * logging.residue.bioenergy.used
    data$Energy_from_sawmill_residues <- (thin.pct.total.live *last.mg.ha * 0.28) * sawmill.residue.bioenergy.used
    data$Wood_Products <- (thin.pct.total.live *last.mg.ha * 0.28) * sawmill.residue.products.used * flows$Wood_Products_In_Use[1:nrow(data)]/100
    data$Landfill <- (thin.pct.total.live *last.mg.ha * 0.28) * sawmill.residue.products.used * flows$Permanent_Landfill_Storage[1:nrow(data)]/100
    data$Energy_from_post_consumer_residues <- (thin.pct.total.live *last.mg.ha * 0.28) * sawmill.residue.products.used * flows$Accumulated_Post_Consumer_Energy[1:nrow(data)]/100
    data$Substitution_Benefits  <-data$Wood_Products[1] * substitution.wood.products
    
    if (even.aged == FALSE) {
      data <- merge(data2, data, all = TRUE)
    }
    data$Activity <- paste("Thin @ Year", year)
    
    return(data)
  }
  
  harvest <- function(year, even.aged) {
    if(missing(even.aged)) {
      even.aged <- TRUE
    }
    if (even.aged == FALSE) {
      first.harvest.year <- harvest.cycles[1]
      last.mg.ha <- growth_table$Let_Grow_Forest_Mg_per_ha[which(growth_table$Year == first.harvest.year)]
      last.mg.ha2 <-thinned$Let_Grow_Forest_Mg_per_ha[thinned$Year == first.harvest.year] - thinned$Let_Grow_Forest_Mg_per_ha[thinned$Year == (first.harvest.year-20)]
    } else {
      previous.activity <- cycle.table$Activity[which(cycle.table$Year == year)-1]
      if (length(previous.activity) > 0){
        if (previous.activity == "Thin") {
          last.mg.ha <-thinned$Let_Grow_Forest_Mg_per_ha[thinned$Year == year]
          last.mg.ha2 <- last.mg.ha
        } else {
          last.mg.ha <- growth$Coefficient_a[growth$Veg_Type == forest.type & growth$Owner_Type == "Pvt_CMC"] * (1 - exp(-growth$Coefficient_b[growth$Veg_Type == forest.type & growth$Owner_Type == "Pvt_CMC"] * harvest.cycles[1]))^3
          last.mg.ha2 <- last.mg.ha
          year <- year + 1
        }
      } else {
        last.mg.ha <- growth$Coefficient_a[growth$Veg_Type == forest.type & growth$Owner_Type == "All_CMC"] * (1 - exp(-growth$Coefficient_b[growth$Veg_Type == forest.type & growth$Owner_Type == "All_CMC"] * harvest.cycles[1]))^3
        last.mg.ha2 <- last.mg.ha
      }
    }
    
    data <- growth_table[which(growth_table$Year == year):which(growth_table$Year == max.year),]
    
    data$Logging_Slash_Left <- last.mg.ha2 * 0.4 * logging.residue.unused * flows$Logging_Residue_Slash[1:nrow(data)]/100
    data$Energy_from_logging_residues <- (last.mg.ha2  * ((0.4* logging.residue.bioenergy.used)) - (last.mg.ha  * 0.03))
    data$Energy_from_sawmill_residues <- (last.mg.ha2  * ((0.6* sawmill.residue.bioenergy.used)))
    data$Wood_Products <- last.mg.ha2  * (0.6 * sawmill.residue.products.used) * flows$Wood_Products_In_Use[1:nrow(data)]/100
    data$Landfill <- last.mg.ha2  * 0.6  * flows$Permanent_Landfill_Storage[1:nrow(data)]/100
    data$Energy_from_post_consumer_residues <- last.mg.ha2  * 0.6  * flows$Accumulated_Post_Consumer_Energy[1:nrow(data)]/100
    if (even.aged == TRUE){
      data$Substitution_Benefits <- NA
      if (year * 2 < 240) {
        end <- year * 2
        data$Substitution_Benefits[1:which(data$Year == end)]  <- data$Wood_Products[1] * substitution.wood.products
        data$Substitution_Benefits[which(data$Year == (end + 1)):which(data$Year == max.year)]  <- data$Wood_Products[which(data$Year == (end + 1))]
      } else {
        end <- 240
        data$Substitution_Benefits[1:which(data$Year == end)]  <- data$Wood_Products[1] * substitution.wood.products
        # data$Substitution_Benefits[which(data$Year == end):which(data$Year == max.year)]  <- data$Wood_Products[which(data$Year == end)]
      }
    } else {
      data$Substitution_Benefits <- data$Wood_Products * substitution.wood.products
    }

    
    data$Activity <- paste("Harvest @ Year", year)
    
    data$Let_Grow_Forest_Mg_per_ha <- NULL
    
    return(data)
  }
  
  if(!is.na(thin.year)) {
    thinned <- thin(thin.year[1], even.aged)
    harvests <- vector(mode="list", length=length(harvest.cycles))
    for (i in 1:length(harvest.cycles)) {
      harvests[[i]] <- harvest(harvest.cycles[i], even.aged)
    }

    final <- Reduce(rbind, harvests)
    final <- merge(thinned, final, all = TRUE)
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
    harvests <- vector(mode="list", length=length(harvest.cycles))
    for (i in 1:length(harvest.cycles)) {
      harvests[[i]] <- harvest(harvest.cycles[i], even.aged)
    }
    
    final <- Reduce(rbind, harvests)
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
  
  regen.table <- data.frame("Project Year" = -80:160, "Year" = 0:max.year)
  regen.table$Regenerated_Forest <- NA
  
  for (i in 1:length(harvest.cycles)) {
    if (!is.na(harvest.cycles[i + 1])) {
      end <- harvest.cycles[i+1] + 1
    } else {
      end <- max.year
    }
    if (even.aged == FALSE) {
      if (i < length(harvest.cycles)) {
      regen.table$Regenerated_Forest[which(regen.table$Year == (harvest.cycles[i])):which(regen.table$Year == (harvest.cycles[i + 1] - 1))] <- final2$Let_Grow_Forest_Mg_per_ha[which(final2$Year == 60):which(final2$Year == 79)]
      } else {
        regen.table$Regenerated_Forest[which(regen.table$Year == (harvest.cycles[i])):which(regen.table$Year == (harvest.cycles[i] + 19))] <- final2$Let_Grow_Forest_Mg_per_ha[which(final2$Year == 60):which(final2$Year == 79)]
        regen.table$Regenerated_Forest[which(regen.table$Year == end)] <- final2$Let_Grow_Forest_Mg_per_ha[which(final2$Year == 80)]
      }
    } else {
      if(i == 1) {
        regen.table$Regenerated_Forest[which(regen.table$Year == harvest.cycles[i]):which(regen.table$Year == end)] <- growth$Coefficient_a[growth$Veg_Type == forest.type & growth$Owner_Type == "Pvt_CMC"] * (1 - exp(-growth$Coefficient_b[growth$Veg_Type == forest.type & growth$Owner_Type == "Pvt_CMC"] * growth_table$Year[1:length(regen.table$Regenerated_Forest[which(regen.table$Year == harvest.cycles[i]):which(regen.table$Year == end)])]))^3
      } else {
        regen.table$Regenerated_Forest[which(regen.table$Year == (harvest.cycles[i] + 1)):which(regen.table$Year == end)] <- growth$Coefficient_a[growth$Veg_Type == forest.type & growth$Owner_Type == "Pvt_CMC"] * (1 - exp(-growth$Coefficient_b[growth$Veg_Type == forest.type & growth$Owner_Type == "Pvt_CMC"] * growth_table$Year[1:length(regen.table$Regenerated_Forest[which(regen.table$Year == (harvest.cycles[i] + 1)):which(regen.table$Year == end)])]))^3
      }
    }
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
  pattern <- c("Substitution_Benefits", "Landfill", "Wood_Products", "Energy_from_post_consumer_residues", "Energy_from_sawmill_residues", "Energy_from_logging_residues", "Regenerated_Forest", "Logging_Slash_Left")
  sum.columns <- which(grepl(paste0(pattern, collapse = "|"), names(final3)))
  
  for (i in 1:nrow(scenario.table)) {
    scenario.table$MgC.ha[scenario.table$Scenario == "Managed" & scenario.table$Cycles == scenario.table$Cycles[i]] <- sum(final3[which(final3$Year == 0):which(final3$Year == harvest.cycles[1] * scenario.table$Cycles[i]), sum.columns]) + sum(final3$Let_Grow_Forest_Mg_per_ha[which(final3$Year == 0):which(final3$Year == harvest.cycles[1])])
    scenario.table$MgC.ha[scenario.table$Scenario == "Let Grow" & scenario.table$Cycles == scenario.table$Cycles[i]] <- sum(final3$Let_Grow_Forest_Mg_per_ha[which(final3$Year == 0):which(final3$Year == harvest.cycles[1] * scenario.table$Cycles[i])])
  }
  
  scenario.table$MgC.ha.yr <- scenario.table$MgC.ha/(scenario.table$Cycles*harvest.cycles[1])
  
  ratio.table <- data.frame("Cycles" = rep(c(1.5, 2, 3), 1))
  ratio.table$Carbon.Ratio <- round(scenario.table[scenario.table$Scenario == "Managed",3]/scenario.table[scenario.table$Scenario == "Let Grow",3],2)
  ratio.table$Cycles <- formatC(ratio.table$Cycles, format = "fg", big.mark = ",")
  
  scenario.table$MgC.ha <- formatC(scenario.table$MgC.ha, format = "d", big.mark = ",")
  scenario.table$MgC.ha.yr <- formatC(scenario.table$MgC.ha.yr, format = "d", big.mark = ",")
  scenario.table$Cycles <- formatC(scenario.table$Cycles, format = "fg", big.mark = ",")
  
  products.table <- data.frame("Cycles" = rep(c(1.5, 2, 3), 1))
  
  for (i in 1:nrow(products.table)) {
    products.table$Live.Trees.and.Downed.Wood[products.table$Cycles[i]] <- (sum(final3$Let_Grow_Forest_Mg_per_ha[final3$Year <= harvest.year]) + sum(final3$Logging_Slash_Left[final3$Year <= (products.table$Cycles[i] * harvest.year)]) + sum(final3$Regenerated_Forest[final3$Year <= (products.table$Cycles[i] * harvest.year)]))/(harvest.year*products.table$Cycles[i])
    products.table$Wood.Products[products.table$Cycles[i]] <- sum(final3$Wood_Products[final3$Year <= (products.table$Cycles[i] * harvest.year)])/(harvest.year*products.table$Cycles[i])
    products.table$Direct.Bioenergy[products.table$Cycles[i]] <- (sum(final3$Energy_from_logging_residues[final3$Year <= (products.table$Cycles[i] * harvest.year)]) + sum(final3$Energy_from_sawmill_residues[final3$Year <= (products.table$Cycles[i] * harvest.year)]) + sum(final3$Energy_from_post_consumer_residues[final3$Year <= (products.table$Cycles[i] * harvest.year)]))/(harvest.year*products.table$Cycles[i])
    products.table$Landfill.Storage[products.table$Cycles[i]] <- sum(final3$Landfill[final3$Year <= (products.table$Cycles[i] * harvest.year)])/(harvest.year*products.table$Cycles[i])
    products.table$Building.Products.Substitution[products.table$Cycles[i]] <- sum(final3$Substitution_Benefits[final3$Year <= (products.table$Cycles[i] * harvest.year)])/(harvest.year*products.table$Cycles[i])
    products.table$Total.Benefits  <- rowSums(products.table[,c(2:6)])
  }
  
  names(products.table) <- c("Cycles", "Live Trees and \n Downed Wood", "Wood Products", "Direct Bioenergy", "Landfill Storage", "Building Products \n Substitution", "Total Benefits")
  products.table$Cycles <- formatC(products.table$Cycles, format = "fg", big.mark = ",")
  products.table[,c(2:7)] <- round(products.table[,c(2:7)],0)
  # 
  # tt <- ttheme_minimal(base_colour = "black")
  # 
  # test <- grid.table(products.table, theme = tt)
  
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
  
  #graph
  
  mylist <- vector(mode="list", length=3)
  mylist[[1]] <- graph
  mylist[[2]] <- final3
  mylist[[3]] <- scenario.table
  mylist[[4]] <- ratio.table
  mylist[[5]] <- products.table
  
  return(mylist)
}

# ggsave("test.png", plot = MIX_CON_UNEVEN[[1]], device = "png", width = 12, height = 6)
# 
# MIX_CON_UNEVEN <- create_growth_table(forest.type = "Mixed Conifer", even.aged = FALSE, thin.year = 40, harvest.year = 80, max.year = 240)
# test <- MIX_CON_UNEVEN [[2]]
# 
# MIX_CON_EVEN_T40_H80_U00 <- create_growth_table("Mixed Conifer", TRUE, 40, 80, logging.residue.bioenergy.used = 0, logging.residue.products.used = 0, logging.residue.unused = 1)
# test <- MIX_CON_EVEN_T40_H80_U00[[2]]
# 
# MIX_CON_EVEN_T40_H80_U25 <- create_growth_table("Mixed Conifer", TRUE, 40, 80, logging.residue.bioenergy.used = 0.25, logging.residue.products.used = 0, logging.residue.unused = 0.75)
# 
# MIX_CON_EVEN_T40_H80_U75 <- create_growth_table("Mixed Conifer", TRUE, 40, 80)
# 
# MIX_CON_EVEN_T00_H80_U75 <- create_growth_table("Mixed Conifer", TRUE, NA, 80)
# 
# PP_EVEN_T40_H80_U00 <- create_growth_table("Ponderosa Pine", TRUE, 40, 80, logging.residue.bioenergy.used = 0, logging.residue.products.used = 0, logging.residue.unused = 1)
# 
# PP_EVEN_T40_H80_U25 <- create_growth_table("Ponderosa Pine", TRUE, 40, 80, logging.residue.bioenergy.used = 0.25, logging.residue.products.used = 0, logging.residue.unused = 0.75)
# 
# PP_EVEN_T40_H80_U75 <- create_growth_table("Ponderosa Pine",TRUE, 40, 80)
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
                        column(1,
                               ""
                        ),
                        column(6,
                               h4("Carbon benefits of treatment over three harvest cycles"), 
                               tableOutput(outputId = "scenario.table")
                        ),
                        column(1,
                               ""
                        ),
                        column(3,
                               h4("Managed Forest to Let Grow Carbon Ratio"), 
                               tableOutput(outputId = "ratio.table")
                        ),
                        column(1,
                               ""
                        )
                      ),
                      fluidRow(
                        column(1,
                               ""
                        ),
                        column(10,
                               h4("Total Managed Forest Benefits - Yearly Average (MgC/ha/yr)"), 
                               tableOutput(outputId = "products.table")
                        ),
                        column(1,
                               ""
                        )
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
             helpText("All treatments assume harvest at year 80. Uneven-aged assumes thin at year 40, harvest at year 80, re-enter every 20 years. Even-aged assumes harvest at years 80 and 160."),
             checkboxInput("even.aged", "Even-aged?", value = TRUE),
             checkboxInput("thin", "Thin at Year 40 (for even-aged only)?", value = TRUE),
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
             h4("Sawmill Residue Proportions"),
             helpText("Default 0.24 used for energy, 0.75 into products, 0.01 waste"),
             numericInput("sawmill.residue.bioenergy.used", "Bioenergy", min = 0, max = 1, value = 0.24, step = 0.1),
             numericInput("sawmill.residue.products.used", "Products", min = 0, max = 1, value = 0.75, step = 0.1),
             numericInput("sawmill.residue.unused", "Unused", min = 0, max = 1, value = 0.01, step = 0.1)
           )
    ),
    
    column(4,
           wellPanel(
             h4("Thinning Residue Proportions"),
             helpText("Based on partial thinnings (72% chips, 28% sawlogs) in Stewart and Nakamura (2012)"),
             numericInput("thinning.residue.bioenergy.used", "Bioenergy", min = 0, max = 1, value = 0.72, step = 0.1),
             numericInput("thinning.residue.products.used", "Products", min = 0, max = 1, value = 0.28, step = 0.1),
             numericInput("thinning.residue.unused", "Unused", min = 0, max = 1, value = 0, step = 0.1)
           )
    ),
    column(4,
           wellPanel (
             h4("Logging Residue Proportions"),
             helpText("Default 0.75 used, 0.25 unused logging residues left to decompose on site."),
             numericInput("logging.residue.bioenergy.used", "Bioenergy", min = 0, max = 1, value = 0.75, step = 0.1),
             numericInput("logging.residue.products.used", "Products", min = 0, max = 1, value = 0, step = 0.1),
             numericInput("logging.residue.unused", "Unused", min = 0, max = 1, value = 0.25, step = 0.1)
           )
    )
    
  ),
  
  fluidRow(
    column(3,
           ""
    ),
    column(3,align="center",
           wellPanel (
             downloadButton('downloadData', 'Download Data')
           )
           ),
    column(3, align="center",
           wellPanel (
             downloadButton('downloadPlot', 'Download Plot')
           )
    ),
    column(3,
           ""
    )
  )
)

server <- function(input, output){
  dataInput <- reactive({    
    if(input$thin == TRUE) {
      thin.year <- 40
    } else {
      thin.year <- NA
    }
    create_growth_table(forest.type = input$forest.type, 
                              even.aged = input$even.aged,
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
    
  })
  
  output$graph <- renderPlot({

    
  dataInput()[[1]]
  })
  
  output$scenario.table <- renderTable({
  
    dataInput()[[3]]
  },hover = TRUE, spacing = 'xs',  align = 'c', width = '100%', digits = 0)
  
  output$ratio.table <- renderTable({
    dataInput()[[4]]
  }, hover = TRUE, spacing = 'xs',  align = 'c',width = '100%',digits = 2)
  
  output$products.table <- renderTable({
    dataInput()[[5]]
  },hover = TRUE, spacing = 'xs',  align = 'c', width = '100%',digits = 0)
  
  output$downloadData <- downloadHandler(
    
    filename = function(){
      paste0(input$forest.type,".zip")
      
    },

    content = function(file) {
      # write.csv(dataInput()[[2]], file, row.names = FALSE)
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;
      
      #loop through the sheets
      for (i in 2:length(dataInput())){
        if(input$thin == TRUE) {
          thin.year <- 40
        } else {
          thin.year <- NA
        }
        
        harvest.year <- 80
        #write each sheet to a csv file, save the name
        name.list <- c("","all.benefits", "scenario.table", "ratio.table", "products.table")
        fileName <- paste(input$forest.type, "_T", thin.year, "_H", harvest.year,"_U", paste(input$logging.residue.bioenergy.used*100), "_",name.list[i],".csv",sep = "")
        write.csv(dataInput()[[i]],fileName,row.names = F)
        files <- c(fileName,files)
      }
      #create the zip file
      zip(file,files)
    }
  )
    
    output$downloadPlot <- downloadHandler(
      
      filename = function() { paste(input$forest.type, "_T", thin.year, "_H", harvest.year,"_U", paste(input$logging.residue.bioenergy.used*100), '.png', sep='') },
      content = function(file) {
        device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
        ggsave(file, plot = dataInput()[[1]], device = device, width = 12, height = 6)
      }

  )
}


shinyApp(ui = ui, server = server)

#rsconnect::deployApp('G:/Dropbox/Carlin/GitHub/Carbon-Calculator')
