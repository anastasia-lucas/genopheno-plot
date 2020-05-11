library(ggplot2)
library(ggsci)

A11 <- "A"
A12 <- "G"
A21 <- "C"
A22 <- "T"

df <- read.table("genotype.raw", header=TRUE)

snp1 <- colnames(df)[3]
snp2 <- colnames(df)[4]
colnames(df)[3] <- "SNP1"
colnames(df)[4] <- "SNP2"

df$barhgt <- ifelse(df$PHENO<0, 0.75, -0.75)
df$barhgt2 <- ifelse(df$PHENO<0, 0.10, -0.1)
df$GT1 <- ifelse(df$SNP1 == 0, paste0(A11,A11),
			ifelse(df$SNP1==1, paste0(A11,A12), paste0(A12,A12)))
df$GT2 <- ifelse(df$SNP2 == 0, paste0(A21,A21),
                        ifelse(df$SNP2==1, paste0(A21,A22), paste0(A22,A22)))
df$Shade <- ifelse(df$PHENO<0, "yes", "no")

#Redo colors
pal <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02")
names(pal) <- c(unique(df$GT1), unique(df$GT2))

#Single
p <- ggplot(data=df[order(df$PHENO),], aes(x=factor(ID, levels=unique(ID)), y=PHENO)) 
p <- p + geom_bar(stat="identity")
p <- p + theme_minimal()
p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + xlab("Sample") + ggtitle("Change in PHENO")
p <- p + geom_point(data=df[order(df$PHENO),], aes(x=factor(ID, levels=unique(ID)), y=barhght2, colour=GT2), size=3)
p <- p + scale_color_tron()
ggsave(p, file="topSNP.png", dpi=300, height=6, width=8, units="in")

#TC interactions
p <- ggplot(data=df[order(df$PHENO),], aes(x=factor(ID, levels=unique(ID)), y=PHENO, fill=Shade)) + geom_bar(stat="identity")
p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + xlab("Sample") + ggtitle("Significant SNPxSNP Interaction")
p <- p + geom_point(data=df[order(df$PHENO),], aes(x=factor(ID, levels=unique(ID)), y=barhgt, colour=GT1), size=3)
p <- p + geom_point(data=df[order(df$PHENO),], aes(x=factor(ID, levels=unique(ID)), y=barhgt2, colour=GT2), size=3)
p <- p + scale_color_manual(values=pal)
p <- p + scale_fill_manual(values=c("#AAAAAA", "#a6761d")) + guides(fill=FALSE)
ggsave(p, file="GxG.png", dpi=300, height=6, width=8, units="in")




