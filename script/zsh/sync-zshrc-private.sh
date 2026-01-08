#!/usr/bin/env bash

set -e

ZSHRC_PRIVATE="$HOME/.zshrc.private"
LASTSYNC_FILE="$HOME/.zshrc.private.lastsync"
HOSTNAME=$(hostname -s)
NOTE_NAME=".zshrc.private ($HOSTNAME)"

# Check if .zshrc.private exists
if [ ! -f "$ZSHRC_PRIVATE" ]; then
    echo "Error: $ZSHRC_PRIVATE not found"
    exit 1
fi

# Check Bitwarden login status
BW_STATUS=$(bw status 2>/dev/null || echo "{}")
LOGGED_IN=$(echo "$BW_STATUS" | jq -r '.status == "unlocked"')

if [ "$LOGGED_IN" != "true" ]; then
    echo "Bitwarden is not logged in or unlocked. Please run:"
    echo "  bw login"
    echo "  bw unlock"
    exit 1
fi

# Get session key
BW_SESSION_RAW=$(bw unlock --raw 2>/dev/null || bw session --raw 2>/dev/null)
if [ -z "$BW_SESSION_RAW" ]; then
    echo "Error: Could not obtain Bitwarden session"
    exit 1
fi

# Calculate current hash
CURRENT_HASH=$(sha256sum "$ZSHRC_PRIVATE" | cut -d' ' -f1)

# Check if we have a last sync hash
if [ -f "$LASTSYNC_FILE" ]; then
    LASTSYNC_HASH=$(cat "$LASTSYNC_FILE")
    if [ "$CURRENT_HASH" = "$LASTSYNC_HASH" ]; then
        echo "$ZSHRC_PRIVATE is already synced (hash: ${CURRENT_HASH:0:8}...)"
        exit 0
    fi
fi

# Read file content
ZSHRC_CONTENT=$(cat "$ZSHRC_PRIVATE")

# Try to get existing note
EXISTING_NOTE=$(bw list items --search "$NOTE_NAME" --session "$BW_SESSION_RAW" 2>/dev/null | jq -r '.[0] // empty')

if [ -n "$EXISTING_NOTE" ]; then
    # Update existing note
    NOTE_ID=$(echo "$EXISTING_NOTE" | jq -r '.id')
    echo "Updating $NOTE_NAME in Bitwarden..."
    # Create JSON for update
    UPDATE_JSON=$(echo "$EXISTING_NOTE" | jq --arg content "$ZSHRC_CONTENT" '.notes = $content')
    echo "$UPDATE_JSON" | bw encode | bw edit item "$NOTE_ID" --session "$BW_SESSION_RAW"
    echo "Updated $NOTE_NAME in Bitwarden"
else
    # Create new note
    echo "Creating $NOTE_NAME in Bitwarden..."
    # Create JSON for new item
    NEW_ITEM_JSON=$(jq -n \
        --arg type "2" \
        --arg name "$NOTE_NAME" \
        --arg content "$ZSHRC_CONTENT" \
        '{type: $type, name: $name, notes: $content}')
    echo "$NEW_ITEM_JSON" | bw encode | bw create item --session "$BW_SESSION_RAW"
    echo "Created $NOTE_NAME in Bitwarden"
fi

# Store current hash
echo "$CURRENT_HASH" > "$LASTSYNC_FILE"
echo "Synced $ZSHRC_PRIVATE to Bitwarden (hash: ${CURRENT_HASH:0:8}...)"
