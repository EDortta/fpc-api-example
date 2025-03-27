#!/bin/bash

# Define the repository URLs and target directories
declare -A REPOS
REPOS["brookframework"]="https://github.com/risoflora/brookframework.git"
REPOS["DCPcrypt"]="https://github.com/SnakeDoctor/DCPcrypt.git"

# Create a temporary directory
TEMP_DIR=$(mktemp -d)
echo "Temporary directory created at: $TEMP_DIR"

sudo chmod 0777 /home/developer/Projects
sudo chown -fR developer.developer /home/developer/Projects

# Clone the repositories into the temporary directory
for REPO_NAME in "${!REPOS[@]}"; do
    echo "Cloning $REPO_NAME..."
    git clone "${REPOS[$REPO_NAME]}" "$TEMP_DIR/$REPO_NAME"
    cd "$TEMP_DIR/$REPO_NAME"
    
    echo "Creating $REPO_NAME.zip"
    git archive -o "$REPO_NAME.zip" HEAD

    echo "Moving $REPO_NAME.zip to /home/developer/Projects/$REPO_NAME"
    cd /home/developer/Projects/
    mkdir -p "$REPO_NAME"
    unzip "$TEMP_DIR/$REPO_NAME/$REPO_NAME.zip" -d "$REPO_NAME"
    rm -rf "$TEMP_DIR/$REPO_NAME"
done
