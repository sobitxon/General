library('ggplot2')
library('RColorBrewer')
library('dplyr')
library('hexbin')
library('gridExtra')
library('reshape2')
library('broom')

# There are 2 datasets (Math and Portuguese courses):
filepath_por = file.choose()
filepath_mat = file.choose()
df_por <- read.table(file = filepath_por, sep = ';', header = TRUE)
df_mat <- read.table(file = filepath_mat, sep = ';', header = TRUE)

# Combine them together and remove duplicate rows (using "unique" features):
df <- rbind(df_por, df_mat)
unique_columns = c("school", "sex", "age", "address", "famsize", "Pstatus", "Medu", "Fedu", 
                   "Mjob", "Fjob", "reason", "nursery", "internet")
df <- df[!duplicated(df[ , unique_columns]), ]

# Find mean grade:
df <- mutate(df, aver_grade = (G1 + G2 + G3) / 3)
df <- subset(df, select = -c(G1, G2, G3))

# Compare average grades for different courses:
df_por <- mutate(df_por, aver_grade = (G1 + G2 + G3) / 3)
df_mat <- mutate(df_mat, aver_grade = (G1 + G2 + G3) / 3)
df_por$course <- 'portuguese'
df_mat$course <- 'math'
means_grades <- rbind(df_por, df_mat)
hist_grades <- ggplot(means_grades, aes(aver_grade, fill = course)) + 
               geom_density(alpha = 0.6) + 
               ggtitle('Average grades distribution') +
               xlab('average grade') +
               scale_fill_brewer(palette = "Set1", name = "course") + 
               theme(panel.background = element_blank(),
                    panel.grid.minor = element_blank(),
                    axis.ticks  = element_blank(),
                    axis.line   = element_line(colour=NA),
                    axis.line.x = element_line(colour="grey80"),
                    axis.line.y = element_line(colour="grey80"))
hist_grades
print(mean(df_por$aver_grade))
print(mean(df_mat$aver_grade))

# Histograms:
par(mfrow=c(2,2))
barplot(table(df$age), col=brewer.pal(8,"Set1"), main="Age", xlab = 'age')
hist(df$aver_grade, breaks=9, col=brewer.pal(11, "RdYlBu"), main = "Average grade (out of 20)", xlab = 'grade')
barplot(table(df$Dalc), col=brewer.pal(5, "Oranges"), main="Daily alc consumption", xlab = 'level', ylim = c(0, 500))
barplot(table(df$Walc), col=brewer.pal(5, "BuPu"), main="W/end alc consumption", xlab = 'level', ylim = c(0, 500))

# Hexbinplot:
rf <- colorRampPalette(brewer.pal(5,'Purples'))
hexb <- hexbinplot(aver_grade ~ absences, data = df, xbins=15, colramp = rf)
hexb

# Mosaic plots:
par(mfrow=c(2,2))
mosaicplot(table(df[, c('sex', 'school', 'address')]), color = brewer.pal(3, "Accent"), main = 'Sex and living area')
mosaicplot(table(df[, c('famsize', 'Pstatus')]), color = brewer.pal(3, "Set2"), main = "Family", ylab = 'together or apart')
mosaicplot(table(df[, c('paid', 'higher')]), color = brewer.pal(4, "Paired"), main = "Future plans", xlab = 'paid extra classes', ylab = 'wants to take higher education')
mosaicplot(table(df[, c('activities', 'internet')]), color = brewer.pal(4, "Set3"), main = "Leisure", xlab = 'extra-curricular activities', ylab = 'has internet')

# Violin plots:
vp1 <- ggplot() + geom_violin(data = df, aes(x = factor(Walc), y = aver_grade, fill = factor(Walc)), draw_quantiles = 0.5, size = 0.5, trim = FALSE) + 
       coord_flip() + ylab("average grade") + xlab("level of consumption") + ggtitle("Average grade & weekend alcohol consumption") + 
       scale_fill_brewer(palette = "PuRd", name = "level")

vp2 <- ggplot() + geom_violin(data = df, aes(x = factor(Walc), y = absences, fill = factor(Walc)), draw_quantiles = 0.5, size = 0.5, trim = FALSE) + 
       coord_flip() + ylab("absences") + xlab("level of consumption") + ggtitle("Average grade & weekend alcohol consumption") + 
       scale_fill_brewer(palette = "PuRd", name = "level")
grid.arrange(vp1, vp2, ncol = 2)

# Convert columns to convenient numeric format to make further calculations:
df_rec <- df
df_rec$sex <- as.character(factor(df_rec$sex, labels = c(0, 1)))
df_rec$school <- as.character(factor(df_rec$school, labels = c(0, 1)))
df_rec$address <- as.character(factor(df_rec$address, labels = c(0, 1)))
df_rec$famsize <- as.character(factor(df_rec$famsize, labels = c(0, 1)))
df_rec$Pstatus <- as.character(factor(df_rec$Pstatus, labels = c(0, 1)))
df_rec$Mjob <- factor(df_rec$Mjob, labels = seq(5))
df_rec$Fjob <- factor(df_rec$Fjob, labels = seq(5))
df_rec$reason <- factor(df_rec$reason, labels = seq(4))
df_rec$guardian <- factor(df_rec$guardian, labels = seq(3))
cols_to_numeric <- c('sex', 'school', 'address', 'famsize', 'Pstatus', 'Mjob', 'Fjob', 'reason', 'guardian')
df_rec[cols_to_numeric] <- lapply(df_rec[cols_to_numeric], as.numeric)
cols_to_recode <- c('schoolsup', 'famsup', 'paid', 'activities', 'nursery', 'higher', 'internet', 'romantic')
df_rec[cols_to_recode] <- lapply(df[cols_to_recode], as.character)
df_rec[df_rec == "yes"] <- 1
df_rec[df_rec == "no"] <- 0
df_rec[cols_to_recode] <- lapply(df[cols_to_recode], as.numeric)

# Correlation matrix for all attributes:
cormat <- round(cor(df_rec), 2)
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
reorder_cormat <- function(cormat){
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value)) +
             geom_tile(color = "white") + scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
             midpoint = 0, limit = c(-1,1), space = "Lab", 
             name = "Pearson\nCorrelation") +
             theme_minimal()+ # minimal theme
             theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) + coord_fixed()

ggheatmap <- ggheatmap + 
              theme(
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(size = 9, angle = 90),
              axis.text.y = element_text(size = 9),
              panel.grid.major = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.ticks = element_blank(),
              legend.justification = c(1, 0),
              legend.position = c(0.6, 0.7),
              legend.direction = "horizontal")+
              guides(fill = guide_colorbar(barwidth = 7, barheight = 1, title.position = "top", title.hjust = 0.5)) + 
              geom_text(aes(Var2, Var1, label = value), color = "black", size = 1.9)

ggheatmap

# Apply PCA approach for visualization and simple regression:
pca <- prcomp(subset(df_rec, select = -c(aver_grade)), scale = TRUE, retx = TRUE)
eigvalues <- (pca$sdev)^2
variance <- eigvalues * 100 / sum(eigvalues)
cumvar <- cumsum(variance)
df_pca <- data.frame(eigvalues = eigvalues, variance = variance, cumvariance = cumvar)

# Show how much variance are explained by first several principal components:
bp_pca <- barplot(df_pca[1:9, 2], names.arg = 1:9,
          main = "Variances",
          xlab = "Principal Components",
          ylab = "Percentage of variances",
          col = rev(brewer.pal(9, 'PuBu')))

lines(x = bp_pca, y = df_pca[1:9, 2], type = 'b', pch = 19, col = "red")

# Scatter plot for the first 2 PCs:
df_scores <- data.frame(PC1 = pca$x[, 1], PC2 = pca$x[, 2], Walc = df_rec$Walc, Aver_grade = df_rec$aver_grade)
scatter_pca <- ggplot(df_scores, aes(PC1, PC2)) + geom_jitter(alpha = 0.8, aes(colour = factor(Walc), size = factor(Walc))) + 
               ggtitle("Weekend alcohol consumption")
scatter_pca

# Model regression fit of average grade against 1st PC.
# Firstly take a look at scatter plot and its density:
cont_plot <- ggplot(data = df_scores, aes(x = PC1, y = Aver_grade)) + 
             geom_point() + 
             ylab('Average grade') + ggtitle('Average grade and 1st PC') + 
             stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE) + 
             scale_fill_distiller(palette = "Spectral") + 
             theme(panel.background = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.ticks  = element_blank(),
                  axis.line   = element_line(colour=NA),
                  axis.line.x = element_line(colour="grey80"),
                  axis.line.y = element_line(colour="grey80")) +
            guides(fill = FALSE)
cont_plot 

# Fit simple linear regression and visualize the results:
regr_plot <- ggplot() + geom_jitter(data = df_scores, aes(x = PC1, y = Aver_grade), alpha = 0.4) + 
  ylab('Average grade') + ggtitle('Average grade and 1st PC') 
model <- lm(data = df_scores, Aver_grade ~ PC1)
summary(model)
PC1_min <- min(df_scores$PC1)
PC1_max <- max(df_scores$PC1)
grade_pred <- data_frame(PC1 = seq(from = PC1_min, to = PC1_max, length.out = length(df_scores$PC1)))
grade_pred <- augment(model, newdata = grade_pred) 
regr_plot <- regr_plot + geom_line(data = grade_pred, color = "red", aes(x = grade_pred$PC1, y = grade_pred$.fitted), size = 1.5)
grade_pred <- mutate(grade_pred, left = .fitted - 3 * .se.fit, right = .fitted + 3 * .se.fit)
regr_plot <- regr_plot + geom_ribbon(data = grade_pred, fill = 'blue',
             aes(x = grade_pred$PC1, ymin = grade_pred$left, ymax = grade_pred$right), alpha = .3)

regr_plot

# Explain which features mostly affect the 1st PC:
df_loadings <- data.frame(PC1 = sort(pca$rotation[abs(pca$rotation[, 1]) > 0.25, 1]))
bar_loadings <- ggplot(df_loadings, aes(x = rownames(df_loadings), y = PC1)) + 
                geom_bar(position="identity", stat="identity", fill=ifelse(df_loadings$PC1 > 0,
                         rgb(45,114,166, maxColorValue = 255),
                         rgb(222,54,54, maxColorValue=255)), alpha = 0.8, color = 'black', linetype = 'dashed') + 
                ggtitle("1st PC loadings") + xlab('features') + ylab('weights') + 
                geom_text(aes(x = rownames(df_loadings),
                          y = PC1 + 0.02 * sign(PC1),
                          label=format(PC1, digits=2)),
                          hjust=0.6, 
                          size=3.5,
                          color=rgb(0,0,0, maxColorValue=255)) +
                theme(panel.background = element_blank(),
                      panel.grid.minor = element_blank(),
                      axis.ticks  = element_blank(),
                      axis.line   = element_line(colour=NA),
                      axis.line.x = element_line(colour="grey80"),
                      axis.line.y = element_line(colour="grey80")) +
                coord_flip() + 
                scale_x_discrete(limits=c(rownames(df_loadings)), labels = c(rownames(df_loadings)))

bar_loadings
