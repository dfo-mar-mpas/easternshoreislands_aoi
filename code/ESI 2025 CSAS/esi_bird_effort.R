## figure to remake the bird plot -- effort by time within the ESI

#load libraries ---
library(tidyverse)

#load data
combo_df <- read.csv("data/Birds/combo_data.csv")%>%
            mutate(date=factor(date,levels=date))

p1 <- ggplot(combo_df, aes(x = date, y = effort,fill=type)) +
  geom_bar(stat = "identity", col = "black") +
  theme_bw() +
  facet_wrap(~type, scales = "free_x", nrow = 2, 
             labeller = as_labeller(c(month = "Survey month", year = "Survey year"))) +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        legend.position="none") +  # Align left and bold
  labs(y = "Survey Effort (km)", x = "")+
  scale_fill_manual(values=c("cornflowerblue","coral"))

ggsave("output/ESI_2025_CSAS/bird_sampling_effort.png",p1,height=6,width=6,units="in",dpi=600)
