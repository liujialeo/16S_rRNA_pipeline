###门水平
rm(list = ls())
metadata = read.table("metadata.tsv", header=T, row.names=1, sep="\t", comment.char="", stringsAsFactors = F)
# 设置分析级别 门p、纲c、目o、科f、属g、种s
tax_level = "g"
# Data reading
tax_phylum = read.table(paste0("tax/sum_", tax_level, ".txt"), header=T, row.names=1, sep="\t", comment.char="")

tax_phylum_t <- as.data.frame(t(tax_phylum))
tax_phylum_t <- cbind(rownames(tax_phylum_t),tax_phylum_t)
colnames(tax_phylum_t)[1] <- "sample"

metadata_m <- metadata
metadata_m <- cbind(rownames(metadata_m),metadata_m)
colnames(metadata_m)[1] <- "sample"
library(dplyr)
tax <- inner_join(metadata_m,tax_phylum_t, by ="sample")
tax[,6:254] <- tax[,6:254]/nrow(tax) 
library(ggplot2)
library(viridis)
# abc <- as.data.frame(colnames(tax))
#  mycolor <-toupper(c("#dc143c","#0000ff","#20b2aa","#ffa500","#9370db",
#                      "#98fb98","#f08080","#1e90ff","#7cfc00","#808000"))

mycolor <- c("#00AFBB", "#E7B800","#1e90ff","#9370db")
##根据疾病情况分组
tax$Group <- factor(tax$Group,levels = c("non_responder_pre","non_responder_post","responder_pre","responder_post"))
ggplot(tax, aes(x = Group, y = 	Roseburia, fill = Group,alpha = 0.1)) + 
  geom_bar(stat = "identity")+
  # ylim(0,0.03)+
  theme(text = element_text(size = 30))+
  scale_colour_manual(values = mycolor,aesthetics = c("colour", "fill"))
#scale_fill_viridis(discrete = T)
#scale_fill_brewer() 
ggsave(filename = "./tax/g_Roseburia_group.pdf",height = 8.05,width = 9)



###种水平 Roseburia intestinalis 
rm(list = ls())
metadata = read.table("metadata.tsv", header=T, row.names=1, sep="\t", comment.char="", stringsAsFactors = F)
# 设置分析级别 门p、纲c、目o、科f、属g、种s
tax_level = "s"
# Data reading
tax_phylum = read.table(paste0("tax/sum_", tax_level, ".txt"), header=T, row.names=1, sep="\t", comment.char="")

tax_phylum_t <- as.data.frame(t(tax_phylum))
tax_phylum_t <- cbind(rownames(tax_phylum_t),tax_phylum_t)
colnames(tax_phylum_t)[1] <- "sample"

metadata_m <- metadata
metadata_m <- cbind(rownames(metadata_m),metadata_m)
colnames(metadata_m)[1] <- "sample"
library(dplyr)
tax <- inner_join(metadata_m,tax_phylum_t, by ="sample")
tax[,6:531] <- tax[,6:531]/nrow(tax) 
library(ggplot2)
library(viridis)
# abc <- as.data.frame(colnames(tax))
#  mycolor <-toupper(c("#dc143c","#0000ff","#20b2aa","#ffa500","#9370db",
#                      "#98fb98","#f08080","#1e90ff","#7cfc00","#808000"))

mycolor <- c("#00AFBB", "#E7B800","#1e90ff","#9370db")
##根据疾病情况分组
tax$Group <- factor(tax$Group,levels = c("non_responder_pre","non_responder_post","responder_pre","responder_post"))
ggplot(tax, aes(x = Group, y = 	Roseburia_intestinalis, fill = Group,alpha = 0.1)) + 
  geom_bar(stat = "identity")+
  # ylim(0,0.03)+
  theme(text = element_text(size = 30))+
  scale_colour_manual(values = mycolor,aesthetics = c("colour", "fill"))
#scale_fill_viridis(discrete = T)
#scale_fill_brewer() 
ggsave(filename = "./tax/s_Roseburia_intestinalis_group.pdf",height = 8.05,width = 9)



###种水平 Roseburia_inulinivorans
rm(list = ls())
metadata = read.table("metadata.tsv", header=T, row.names=1, sep="\t", comment.char="", stringsAsFactors = F)
# 设置分析级别 门p、纲c、目o、科f、属g、种s
tax_level = "s"
# Data reading
tax_phylum = read.table(paste0("tax/sum_", tax_level, ".txt"), header=T, row.names=1, sep="\t", comment.char="")

tax_phylum_t <- as.data.frame(t(tax_phylum))
tax_phylum_t <- cbind(rownames(tax_phylum_t),tax_phylum_t)
colnames(tax_phylum_t)[1] <- "sample"

metadata_m <- metadata
metadata_m <- cbind(rownames(metadata_m),metadata_m)
colnames(metadata_m)[1] <- "sample"
library(dplyr)
tax <- inner_join(metadata_m,tax_phylum_t, by ="sample")
tax[,6:531] <- tax[,6:531]/nrow(tax) 
library(ggplot2)
library(viridis)
# abc <- as.data.frame(colnames(tax))
#  mycolor <-toupper(c("#dc143c","#0000ff","#20b2aa","#ffa500","#9370db",
#                      "#98fb98","#f08080","#1e90ff","#7cfc00","#808000"))

mycolor <- c("#00AFBB", "#E7B800","#1e90ff","#9370db")
##根据疾病情况分组
tax$Group <- factor(tax$Group,levels = c("non_responder_pre","non_responder_post","responder_pre","responder_post"))
ggplot(tax, aes(x = Group, y = 	Roseburia_inulinivorans, fill = Group,alpha = 0.1)) + 
  geom_bar(stat = "identity")+
  # ylim(0,0.03)+
  theme(text = element_text(size = 30))+
  scale_colour_manual(values = mycolor,aesthetics = c("colour", "fill"))
#scale_fill_viridis(discrete = T)
#scale_fill_brewer() 
ggsave(filename = "./tax/s_Roseburia_inulinivorans_group.pdf",height = 8.05,width = 9)

###种水平 Roseburia hominis
rm(list = ls())
metadata = read.table("metadata.tsv", header=T, row.names=1, sep="\t", comment.char="", stringsAsFactors = F)
# 设置分析级别 门p、纲c、目o、科f、属g、种s
tax_level = "s"
# Data reading
tax_phylum = read.table(paste0("tax/sum_", tax_level, ".txt"), header=T, row.names=1, sep="\t", comment.char="")

tax_phylum_t <- as.data.frame(t(tax_phylum))
tax_phylum_t <- cbind(rownames(tax_phylum_t),tax_phylum_t)
colnames(tax_phylum_t)[1] <- "sample"

metadata_m <- metadata
metadata_m <- cbind(rownames(metadata_m),metadata_m)
colnames(metadata_m)[1] <- "sample"
library(dplyr)
tax <- inner_join(metadata_m,tax_phylum_t, by ="sample")
tax[,6:531] <- tax[,6:531]/nrow(tax) 
library(ggplot2)
library(viridis)
# abc <- as.data.frame(colnames(tax))
#  mycolor <-toupper(c("#dc143c","#0000ff","#20b2aa","#ffa500","#9370db",
#                      "#98fb98","#f08080","#1e90ff","#7cfc00","#808000"))

mycolor <- c("#00AFBB", "#E7B800","#1e90ff","#9370db")
##根据疾病情况分组
tax$Group <- factor(tax$Group,levels = c("non_responder_pre","non_responder_post","responder_pre","responder_post"))
ggplot(tax, aes(x = Group, y = 	Roseburia_hominis, fill = Group,alpha = 0.1)) + 
  geom_bar(stat = "identity")+
  # ylim(0,0.03)+
  theme(text = element_text(size = 30))+
  scale_colour_manual(values = mycolor,aesthetics = c("colour", "fill"))
#scale_fill_viridis(discrete = T)
#scale_fill_brewer() 
ggsave(filename = "./tax/s_Roseburia_hominis_group.pdf",height = 8.05,width = 9)

###种水平 Roseburia_faecis
rm(list = ls())
metadata = read.table("metadata.tsv", header=T, row.names=1, sep="\t", comment.char="", stringsAsFactors = F)
# 设置分析级别 门p、纲c、目o、科f、属g、种s
tax_level = "s"
# Data reading
tax_phylum = read.table(paste0("tax/sum_", tax_level, ".txt"), header=T, row.names=1, sep="\t", comment.char="")

tax_phylum_t <- as.data.frame(t(tax_phylum))
tax_phylum_t <- cbind(rownames(tax_phylum_t),tax_phylum_t)
colnames(tax_phylum_t)[1] <- "sample"

metadata_m <- metadata
metadata_m <- cbind(rownames(metadata_m),metadata_m)
colnames(metadata_m)[1] <- "sample"
library(dplyr)
tax <- inner_join(metadata_m,tax_phylum_t, by ="sample")
tax[,6:531] <- tax[,6:531]/nrow(tax) 
library(ggplot2)
library(viridis)
# abc <- as.data.frame(colnames(tax))
#  mycolor <-toupper(c("#dc143c","#0000ff","#20b2aa","#ffa500","#9370db",
#                      "#98fb98","#f08080","#1e90ff","#7cfc00","#808000"))

mycolor <- c("#00AFBB", "#E7B800","#1e90ff","#9370db")
##根据疾病情况分组
tax$Group <- factor(tax$Group,levels = c("non_responder_pre","non_responder_post","responder_pre","responder_post"))
ggplot(tax, aes(x = Group, y = 	Roseburia_faecis, fill = Group,alpha = 0.1)) + 
  geom_bar(stat = "identity")+
  # ylim(0,0.03)+
  theme(text = element_text(size = 30))+
  scale_colour_manual(values = mycolor,aesthetics = c("colour", "fill"))
#scale_fill_viridis(discrete = T)
#scale_fill_brewer() 
ggsave(filename = "./tax/s_Roseburia_faecis_group.pdf",height = 8.05,width = 9)


###种水平 Roseburia_cecicola
rm(list = ls())
metadata = read.table("metadata.tsv", header=T, row.names=1, sep="\t", comment.char="", stringsAsFactors = F)
# 设置分析级别 门p、纲c、目o、科f、属g、种s
tax_level = "s"
# Data reading
tax_phylum = read.table(paste0("tax/sum_", tax_level, ".txt"), header=T, row.names=1, sep="\t", comment.char="")

tax_phylum_t <- as.data.frame(t(tax_phylum))
tax_phylum_t <- cbind(rownames(tax_phylum_t),tax_phylum_t)
colnames(tax_phylum_t)[1] <- "sample"

metadata_m <- metadata
metadata_m <- cbind(rownames(metadata_m),metadata_m)
colnames(metadata_m)[1] <- "sample"
library(dplyr)
tax <- inner_join(metadata_m,tax_phylum_t, by ="sample")
tax[,6:531] <- tax[,6:531]/nrow(tax) 
library(ggplot2)
library(viridis)
# abc <- as.data.frame(colnames(tax))
#  mycolor <-toupper(c("#dc143c","#0000ff","#20b2aa","#ffa500","#9370db",
#                      "#98fb98","#f08080","#1e90ff","#7cfc00","#808000"))

mycolor <- c("#00AFBB", "#E7B800","#1e90ff","#9370db")
##根据疾病情况分组
tax$Group <- factor(tax$Group,levels = c("non_responder_pre","non_responder_post","responder_pre","responder_post"))
ggplot(tax, aes(x = Group, y = 	Roseburia_cecicola, fill = Group,alpha = 0.1)) + 
  geom_bar(stat = "identity")+
  # ylim(0,0.03)+
  theme(text = element_text(size = 30))+
  scale_colour_manual(values = mycolor,aesthetics = c("colour", "fill"))
#scale_fill_viridis(discrete = T)
#scale_fill_brewer() 
ggsave(filename = "./tax/s_Roseburia_cecicola_group.pdf",height = 8.05,width = 9)