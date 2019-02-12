library(readxl)
library(tidyverse)
library(cowplot)

sdpapers <- read_excel("SD_Rant_Data.xlsx", sheet=1)
sdpaperstime <- read_excel("SD_Rant_Data.xlsx", sheet=2)
sdgrants <- read_excel("SD_Rant_Data.xlsx", sheet=3)
sdtextbooks <- read_excel("SD_Rant_Data.xlsx", sheet=4)
sdipbes <- read_excel("SD_Rant_Data.xlsx", sheet=5)

sdpapers$theme <- factor(sdpapers$theme, levels = c("pollination", "seed dispersal","seed predation", "herbivory"))
sdpaperstime$theme <- factor(sdpaperstime$theme, levels = c("pollination", "seed dispersal","seed predation", "herbivory"))
sdgrants$theme <- factor(sdgrants$theme, levels = c("pollination", "seed dispersal","seed predation", "herbivory"))
sdtextbooks$theme <- factor(sdtextbooks$theme, levels = c("pollination", "seed dispersal","seed predation", "herbivory"))
sdipbes$theme <- factor(sdipbes$theme, levels = c("pollination", "seed dispersal","seed predation", "herbivory"))

sdpapers <- subset (sdpapers, theme != "herbivory" )
sdpaperstime <- subset (sdpaperstime, theme != "herbivory" )
sdgrants <- subset (sdgrants, theme != "herbivory" )
sdtextbooks <- subset (sdtextbooks, theme != "herbivory" )
sdipbes <- subset (sdipbes, theme != "herbivory" )

group.colors <- c("pollination"="#E69F0055", "seed dispersal"="#D55E0055", "seed predation"="#F0E44255")

papers <- ggplot(sdpapers[sdpapers$field == "all",], aes(duration, num_papers, fill=theme))+
    geom_bar(stat="identity", position = "dodge")+
    #scale_fill_grey(start = 0, end = .9)+
    scale_fill_brewer(type = "qual", palette = 3, direction= -1)+
    ylab("Number of papers")+
    xlab("")+
    theme_classic()+
    theme(legend.position="none")
#ggsave("papers_graph.pdf")

legend <- ggplot(sdpapers, aes(duration, num_papers, fill=theme))+
    geom_bar(stat="identity", position = "dodge")+
    scale_fill_brewer(type = "div", palette = 5)

evolpapers <- ggplot(sdpapers[sdpapers$field == "evolution",], aes(theme, num_papers, fill=theme))+
    geom_bar(stat="identity", position = "dodge")+
    scale_x_discrete(name=" ") +
    #scale_fill_grey(start = 0, end = .9)+
    scale_fill_brewer(type = "qual", palette = 3, direction= -1)+
    ylab("Number of papers")+
    #xlab("interaction")+
    theme_classic()+
    theme(legend.position="none",
          axis.text.x = element_blank())
#ggsave("papersevol_graph.pdf")

paperstime <- ggplot(sdpaperstime, aes(year, numpubs, color=theme))+
  geom_line(aes(linetype=theme))+
  scale_color_brewer(type = "qual", palette = 3, direction= -1)+
  #scale_color_grey(start = 0, end = .9)+
  scale_linetype_manual(values=c("solid", "longdash", "dotted"))+
  ylab("Number of papers \nper year")+
  xlab("Year")+
  theme_classic()+
  theme(legend.position="none")
#ggsave("paperstime_graph.pdf")

grants <- ggplot(sdgrants, aes(division, num_grants, fill=theme))+
    geom_bar(stat="identity", position = "dodge")+
    scale_fill_brewer(type = "qual", palette = 3, direction= -1)+
    #scale_fill_grey(start = 0, end = .9)+
    ylab("Number of funded grants")+
    xlab("")+
    theme_classic()+
    theme(legend.position="none",
          axis.title.x = element_blank())
#ggsave("grants_graph.pdf")

# #textbooks <- ggplot(sdtextbooks, aes(theme, num_unique_pages, fill=theme))+
#     geom_bar(stat="identity", position = "dodge")+
#     scale_fill_brewer(type = "div", palette = 5)+
#     ylab("Number of pages")+
#     xlab("")+
#     theme_classic()+
#     theme(axis.text.x=element_blank(),
#           panel.background = element_rect(fill = NA, color = "black"))+
#     facet_grid(.~textbook_name)
# #ggsave("textbook_graph.pdf")

sumtext <- sdtextbooks %>%
    group_by(theme) %>%
    summarise(N=length(num_unique_pages),
               mean = mean(num_unique_pages),
               sd   = sd(num_unique_pages),
               se   = sd / sqrt(N))

textbooks <- ggplot(sumtext, aes(theme, mean, fill=theme))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2)+
  scale_fill_brewer(type = "qual", palette = 3, direction= -1)+
  #scale_fill_grey(start = 0, end = .9)+
  ylab("Number of pages")+
  xlab("")+
  theme_classic()+
  theme(legend.position="none",
    axis.text.x=element_blank())

sumipbes <- sdipbes %>%
    group_by(theme) %>%
    summarise(N=length(num_mentions),
               mean = mean(num_mentions),
               sd   = sd(num_mentions),
               se   = sd / sqrt(N))

ipbes <- ggplot(sumipbes, aes(theme, mean, fill=theme))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2)+
  scale_fill_brewer(type = "qual", palette = 3, direction= -1)+
  #scale_fill_grey(start = 0, end = .9)+
  ylab("Number of mentions")+
  xlab("")+
  theme_classic()+
  theme(legend.position="none",
    axis.text.x=element_blank())

legend_bar <- ggplot(sdpapers, aes(duration, num_papers, fill=theme))+
  geom_bar(stat="identity", position = "dodge")+
  scale_fill_brewer(type = "seq", palette = 4, direction= -1)+

legend_line <- ggplot(sdpaperstime, aes(year, numpubs, color=theme))+
  geom_line(aes(linetype=theme), size=1.2)+
  scale_color_brewer(type = "qual", palette = 3, direction= -1)+
  #scale_color_grey(start = 0, end = .9)+
  scale_linetype_manual(values=c("solid", "longdash", "dotted"))+
  theme(legend.title=element_blank(), legend.key.width = unit(1.5,"cm"))

fig2legend <- get_legend(legend_line)
top_row <- plot_grid(papers, paperstime, fig2legend, labels = c('A', 'B', ''), label_x = 0.1, label_y = 0.1, hjust = -0.25, vjust = -0.25, nrow=1, rel_widths = c(1,1, 0.6)) #align='h', axis='b'
bottom_row <- plot_grid(grants, textbooks, ipbes, labels = c('C', 'D', 'E'), label_x = 0.07, label_y = 0.07, hjust = -0.3, vjust = -0.1, nrow=1, rel_widths = c(1,1,1), axis='b', align='v')
Fig2 <- plot_grid(top_row, bottom_row, nrow=2, align='hv')

save_plot("fig2psdp.pdf", Fig2, base_width = 8)
1220/3149

