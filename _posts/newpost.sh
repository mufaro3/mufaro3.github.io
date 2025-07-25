#!/bin/bash

# Check if a name was provided
if [ -z "$1" ]; then
  echo "Usage: $0 name-of-file"
  exit 1
fi

# Get the current date in YYYY-MM-DD format
DATE=$(date +%F)
DATETIME=$(date +"%Y-%m-%d %H:%M:%S")

# Create the filename
FILENAME="${DATE}-$1.md"

# Create the file and insert the header
cat <<EOF > "$FILENAME"
---
layout: post
title:  "$1"
date:   $DATETIME
comments: true
categories:
    - category
tags:
    - tag
---

EOF

echo "Created file: $FILENAME"
