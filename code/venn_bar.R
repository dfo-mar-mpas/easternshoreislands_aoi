venn_bar <- function(bar_data,title=""){
  
  #plotting function mirrored on Trask et al. analysis of eDNA in coastal inverts
  
  require(tidyverse)
  require(patchwork)
  
  #custom colour palette for plots
  palette_cust <- c("#154360", "#FF5733", "#1ABC9C")
  
  #set up a eDNA centric plot order
  phylum_order <- bar_data %>%
                  group_by(Phylum) %>%
                  summarise(eDNA_Only_Proportion = sum(Status == "eDNA Only") / n(),
                            eDNA_Only_count = sum(Status == "eDNA Only")) %>%
                  arrange(desc(eDNA_Only_count)) %>%
                  pull(Phylum)  # Extract the ordered phyla
  
  #set up plotting data
  
  count_levels <- bar_data%>%
                  filter(Status=="eDNA Only")%>%
                  arrange(-Count)%>%
                  pull(Phylum)%>%
                  c(.,setdiff(bar_data$Phylum,bar_data%>%
                                filter(Status=="eDNA Only")%>%
                                arrange(-Count)%>%
                                pull(Phylum)))
  
  df1 <- bar_data%>%
          mutate(Phylum = factor(Phylum,levels=count_levels),
                 Status = gsub("Only","only",Status),
                 Status = factor(Status,levels=c("Traditional only","Shared","eDNA only")))%>%
          group_by(Phylum)%>%
          mutate(Proportion = Count / sum(Count),
                 TotalCount = sum(Count))%>%
          ungroup()
  
  #order by eDna only proprotions 
  prop_phyla_ord <- df1%>%
                    filter(Status == "eDNA only")%>%
                    arrange(-Proportion)%>%
                    pull(Phylum)%>%
                    as.character()%>%
                    c(.,setdiff(unique(bar_data$Phylum),.))
  
  #plot with the Phylum ordered according to the proportional fraction of eDNA only within each phyla
  df2 <- df1%>%
          mutate(Phylum = factor(Phylum,levels=prop_phyla_ord))%>%
          group_by(Phylum)%>%
          mutate(cumulative_proportion = cumsum(Proportion) - Proportion / 2)%>%
          ungroup()
  
  df2_labs <- df2%>%
              distinct(Phylum,.keep_all = TRUE)
  
  #Assemble data that is for all taxa to join to this plot. 
  df3 <- bar_data%>%
          group_by(Status)%>%
          summarise(Count=sum(Count))%>%
          ungroup()%>%
          mutate(Phylum = "All",
                 Proportion = Count/sum(Count),
                 Status = gsub("Only","only",Status),
                 Status = factor(Status,levels=c("Traditional only","Shared","eDNA only")))
  
  df4 <- df2%>%
          dplyr::select(names(df3))%>%
          rbind(.,df3)%>%
          mutate(Phylum = factor(Phylum,levels=c(levels(df2$Phylum),"All")),
                 Status = gsub(" only","",Status),
                 Status = gsub("Traditional","RV",Status),
                 Status = factor(Status,levels=c("RV","Shared","eDNA")),
                 group = ifelse(Phylum == "All","All","Phyla"),
                 group = factor(group,levels=c("Phyla","All")))
  
  # Filter out the "all" category
  df_phyla <- df4 %>% filter(Phylum != "All")
  df_all <- df4 %>% filter(Phylum == "All")
  
  # Count the number of phyla excluding 'all'
  num_phyla <- n_distinct(df_phyla$Phylum)
  
  
  plot_phyla <- ggplot(df_phyla, aes(x = Phylum, y = Proportion, fill = Status)) +
                geom_bar(stat = "identity", position = "fill", col = "black") +
                geom_text(aes(label = Count), position = position_stack(vjust = 0.5), size = 4, fontface = 2) +
                labs(x = "", y = "Proportion", fill = "",title=title) +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1),
                      legend.position = "top",
                      legend.justification = "right",
                      legend.direction = "horizontal",
                      plot.title = element_text(hjust = 0), # Left-align the title
                      plot.title.position = "plot" ) +
                scale_y_continuous(expand = c(0.02, 0), labels = scales::percent) +
                scale_fill_manual(values = palette_cust)
  
  # Plot for the 'all' category
  plot_all <- ggplot(df_all, aes(x = Phylum, y = Proportion, fill = Status)) +
    geom_bar(stat = "identity", position = "fill", col = "black") +
    geom_text(aes(label = Count), position = position_stack(vjust = 0.5), size = 4, fontface = 2) +
    labs(x = "", y = "Proportion", fill = "") +
    theme_bw() +
    theme(strip.background = element_blank(),
          strip.text = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none") +
    scale_y_continuous(expand = c(0.02, 0), labels = scales::percent) +
    scale_fill_manual(values = palette_cust)
  
  # Combine the two plots with proportional widths
  # num_phyla is used to ensure the widths are proportional
  combined_plot <- (plot_phyla + plot_all) + 
    plot_layout(ncol = 2, widths = c(num_phyla, 1), guides = "collect")&
    theme(legend.position = "top", legend.justification = "right")
              
  
  return(combined_plot)
  
  
}
