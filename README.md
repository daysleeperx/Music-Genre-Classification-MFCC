# ITI8730-2023-DM-Final-Project

## Prerequisites

Before running the project, ensure you have `ffmpeg` installed on your machine. You can install `ffmpeg` by following the instructions on the [official ffmpeg website](https://ffmpeg.org/download.html).

## Dataset

This project uses the GTZAN dataset for music genre classification. 

### Downloading and Preparing the Dataset

1. **Download the Dataset:**  
   Download the GTZAN dataset from the following links:
   - [Kaggle](https://www.kaggle.com/datasets/carlthome/gtzan-genre-collection?resource=download)
   - [Hugging Face](https://huggingface.co/datasets/marsyas/gtzan)

2. **Place the Dataset in the Data Folder:**  
   After downloading, extract the dataset and place it in the `data/` folder in the root directory of this project.

3. **Converting to WAV Format:**  
   To convert the dataset to WAV files, run the `convert_to_wav.sh` script located in the root directory:
   ```bash
   chmod +x convert_to_wav.sh
   ./convert_to_wav.sh
   ```
