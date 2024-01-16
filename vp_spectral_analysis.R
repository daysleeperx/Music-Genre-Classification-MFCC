rm(list = ls())
setwd("/Users/viktorpavlov/repos/school/iti8730-2023-dm-final-project")
getwd()

if (!requireNamespace("tuneR", quietly = TRUE)) {
  install.packages("tuneR")
}
library(tuneR)
if (!requireNamespace("seewave", quietly = TRUE)) {
  install.packages("seewave")
}
library(seewave)
library(cluster)
library(ggplot2)
library(e1071)
library(patchwork)
library(reshape2)
library(dplyr)

# load GTZAN dataset -------------------------------------------------
test_sound <- readWave("data/genres/metal/wav/metal.00001.wav")
summary(test_sound)
plot(test_sound, col = "#ff5a5f")

test_sound_spec_obj <- periodogram(
  test_sound,
  normalize = TRUE,
  width = 1024,
  overlap = 512
)

plot(test_sound_spec_obj, xlim = c(0, 2000), which = 1)
image(test_sound_spec_obj, ylim = c(0, 1000))

m1 <- melfcc(test_sound, numcep = 12)
length(m1)
print(m1)
summary(m1)

colors <- colorRampPalette(c("white", "black"))

image(
  m1,
  xlab = "Time",
  ylab = "MFCC Coefficients",
  main = "MFCC Heatmap",
  col = colors(100)
)

zcr_value <- zcr(test_sound)
spec_centroid <- meanspec(test_sound)

features <- data.frame()
genres_path <- "data/genres"
n_mfcc <- 30

for (genre in list.dirs(genres_path, full.names = TRUE, recursive = FALSE)) {
  genre_name <- basename(genre)

  for (wav_file in list.files(
    paste(genre, "wav", sep = "/"),
    full.names = TRUE
  )) {
    wave_obj <- readWave(wav_file)
    mfcc <- melfcc(wave_obj, numcep = n_mfcc)
    mfcc_means <- colSums(mfcc)
    file_features <- data.frame(t(mfcc_means))
    file_features$genre <- genre_name
    features <- rbind(features, file_features)
  }
}

# clustering with k-means --------------------------------------------
set.seed(123)
number_of_clusters <- 2

features <- features[complete.cases(features), ]
selected_genres <- features[features$genre %in% c("pop", "metal"), ]
features_scaled <- scale(selected_genres[, -ncol(selected_genres)])

kmeans_result <- kmeans(features_scaled, centers = number_of_clusters)
kmeans_result

selected_genres$cluster <- kmeans_result$cluster

# compute and plot silhouette coefficient -----------------------------
sil_width <- silhouette(kmeans_result$cluster, dist(features_scaled))

sil_df <- data.frame(
  cluster = factor(sil_width[, 1]),
  neighbor = factor(sil_width[, 2]),
  sil_width = sil_width[, 3]
)

sil_df <- sil_df[order(sil_df$cluster, sil_df$sil_width), ]

sil_df$cumsum <- ave(
  sil_df$sil_width,
  sil_df$cluster,
  FUN = function(x) cumsum(x) - x / 2
)

colors <- c("#ff5a5f", "#0b3954")

p1 <- ggplot(
  sil_df,
  aes(x = cluster, y = sil_width, fill = as.factor(cluster))
) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  labs(
    title = "Silhouette Coefficient",
    x = "Cluster",
    y = "Silhouette Width",
    fill = "Cluster"
  ) +
  theme(legend.position = "bottom")
print(p1)

# classification with svm ---------------------------------------------
set.seed(123)

shuffled_indices <- sample(seq_len(nrow(features)))
shuffled_data <- features[shuffled_indices, ]

shuffled_data <- shuffled_data %>%
  mutate_at(vars(-genre), scale)

train_index <- 1:(0.8 * nrow(shuffled_data))
test_index <- (0.8 * nrow(shuffled_data) + 1):nrow(shuffled_data)

train_set <- shuffled_data[train_index, ]
test_set <- shuffled_data[test_index, ]

train_set$genre <- as.factor(train_set$genre)
test_set$genre <- as.factor(test_set$genre)

svm_model <- svm(genre ~ ., data = train_set, kernel = "radial")

predictions <- predict(svm_model, test_set[, -ncol(test_set)])

true_labels <- test_set$genre
accuracy <- sum(predictions == true_labels) / length(true_labels)
print(accuracy)

confusion_matrix <- table(Predicted = predictions, True = true_labels)
print(confusion_matrix)

long_confusion_matrix <- melt(confusion_matrix)

p2 <- ggplot(
  long_confusion_matrix, aes(x = True, y = Predicted, fill = value)
) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "#0b3954") +
  labs(x = "Predicted", y = "True", fill = "Count") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 12),
    plot.title = element_text(hjust = 0.5)
  ) +
  ggtitle("Confusion Matrix")
print(p2)

combined_plot <- p1 + p2
print(combined_plot)
