rm(list = ls())
setwd("/Users/viktorpavlov/repos/school/iti8730-2023-dm-final-project")
getwd()

if (!requireNamespace("tuneR", quietly = TRUE)) {
  install.packages("tuneR")
}
library(tuneR)
if (!requireNamespace("pastecs", quietly = TRUE)) {
  install.packages("pastecs")
}
library(pastecs)
if (!requireNamespace("seewave", quietly = TRUE)) {
  install.packages("seewave")
}
library(seewave)
library(cluster)
library(ggplot2)

tuneR::setWavPlayer("/usr/bin/afplay")

# tuneR Basiscs ------------------------------------------------------
w_obj <- bind(sine(440), sine(220))
show(w_obj)
plot(w_obj)
plot(extractWave(w_obj, from = 1, to = 500))
play(w_obj)

tmpfile <- file.path(tempdir(), "testfile.wav")
writeWave(w_obj, tmpfile)
w_obj2 <- readWave(tmpfile)

w_obj_m <- mono(w_obj, "left")
w_obj_m11 <- downsample(w_obj_m, 11025)
w_obj_m11s <- extractWave(w_obj_m11)

w_obj_m11s <- extractWave(w_obj_m11)

w_spec_obj <- periodogram(
  w_obj_m11s,
  normalize = TRUE,
  width = 1024,
  overlap = 512
)
plot(w_spec_obj, xlim = c(0, 2000), which = 1)
image(w_spec_obj, ylim = c(0, 1000))

ff <- FF(w_spec_obj)
print(ff)

notes <- noteFromFF(ff, 440)
snotes <- smoother(notes)
melodyplot(w_spec_obj, snotes)

qnotes <- quantize(snotes, w_spec_obj@energy, parts = 8)
quantplot(qnotes, expected = rep(c(0, -12), each = 4), bars = 2)

qlily <- quantMerge(snotes, 4, 4, 2)

# analyze a real audio file - drum break -----------------------------
drum_audio <- readWave("data/Motoko Break.wav")
str(drum_audio)
summary(drum_audio)

plot(drum_audio)

drum_audio_mono <- mono(drum_audio, "left")
plot(drum_audio_mono)

drum_spec_obj <- periodogram(
  drum_audio_mono,
  normalize = TRUE,
  width = 1024,
  overlap = 512
)

plot(drum_spec_obj, xlim = c(0, 2000), which = 1)
image(drum_spec_obj, ylim = c(0, 1000))

# analyze a real audio file - brass ----------------------------------
brass_audio <- readWave("data/Brass_Waltzez.wav")
str(brass_audio)
summary(brass_audio)

plot(brass_audio)

brass_audio_mono <- mono(brass_audio, "left")
plot(brass_audio_mono)

brass_spec_obj <- periodogram(
  brass_audio_mono,
  normalize = TRUE,
  width = 1024,
  overlap = 512
)

plot(brass_spec_obj, xlim = c(0, 2000), which = 1)
image(brass_spec_obj, ylim = c(0, 1000))

ff <- FF(brass_spec_obj)
print(ff)
length(ff)
summary(ff)

notes <- noteFromFF(ff, 440)
snotes <- smoother(notes)
melodyplot(brass_spec_obj, snotes)

qnotes <- quantize(snotes, brass_spec_obj@energy, parts = 8)
quantplot(qnotes, expected = rep(c(0, -12), each = 4), bars = 2)

# load GTZAN dataset -------------------------------------------------
test_sound <- readWave("data/genres/metal/wav/metal.00001.wav")
summary(test_sound)
plot(test_sound)

test_sound_spec_obj <- periodogram(
  test_sound,
  normalize = TRUE,
  width = 1024,
  overlap = 512
)

plot(test_sound_spec_obj, xlim = c(0, 2000), which = 1)
image(test_sound_spec_obj, ylim = c(0, 1000))

m1 <- melfcc(test_sound, numcep = 30)
length(m1)
print(m1)
summary(m1)

image(m1, xlab = "Time", ylab = "MFCC Coefficients", main = "MFCC Heatmap")

zcr_value <- zcr(test_sound)
spec_centroid <- meanspec(test_sound)

features <- data.frame()

genres_path <- "data/genres"

n_mfcc <- 30

for (genre in list.dirs(genres_path, full.names = TRUE, recursive = FALSE)) {
  genre_name <- basename(genre)

  for (wav_file in list.files(paste(genre, "wav", sep = "/"), full.names = TRUE)) {
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

colors <- c("#ef476f", "#118ab2")

ggplot(
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

# classification with svm ---------------------------------------------
set.seed(123)

shuffled_indices <- sample(seq_len(nrow(features)))
shuffled_data <- features[shuffled_indices, ]

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
