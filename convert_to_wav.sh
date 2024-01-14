#!/bin/bash

# Base directory containing genre subdirectories
GENRE_DIR="data/genres"

# Loop through each genre directory
for DIR in "$GENRE_DIR"/*; do
  if [ -d "$DIR" ]; then  # Ensure it's a directory
    echo "Processing directory: $DIR"
    # Create a 'wav' subdirectory if it doesn't exist
    WAV_DIR="$DIR/wav"
    mkdir -p "$WAV_DIR"
    # Loop through each .au file in this directory
    for FILE in "$DIR"/*.au; do
      if [ -e "$FILE" ]; then  # Check if the .au file exists
        # Extract filename without extension
        BASENAME=$(basename "$FILE" .au)
        # Create the .wav filename with path to 'wav' subdirectory
        OUTFILE="$WAV_DIR/${BASENAME}.wav"
        # Convert the file
        ffmpeg -i "$FILE" "$OUTFILE"
        echo "Converted $FILE to $OUTFILE"
      fi
    done
  fi
done
