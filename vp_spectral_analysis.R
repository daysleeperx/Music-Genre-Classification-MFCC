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
test_sound <- readWave("data/genres/blues/blues.00000.wav")
summary(test_sound)
