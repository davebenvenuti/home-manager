#!/usr/bin/env bash

set -e

ZSHRC_PRIVATE="$HOME/.zshrc.private"
LASTSYNC_FILE="$HOME/.zshrc.private.lastsync"
HOSTNAME=$(hostname -s)
NOTE_NAME=".zshrc.private ($HOSTNAME)"

echo "Starting sync of $ZSHRC_PRIVATE to Bitwarden..."

# Check if .zshrc.private exists
if [ ! -f "$ZSHRC_PRIVATE" ]; then
    echo "Error: $ZSHRC_PRIVATE not found"
    exit 1
fi

# Check Bitwarden login status
echo "Checking Bitwarden status..."
BW_STATUS=$(bw status 2>/dev/null || echo "{}")
LOGGED_IN=$(echo "$BW_STATUS" | jq -r '.status == "unlocked"')

if [ "$LOGGED_IN" != "true" ]; then
    echo "Bitwarden is not logged in or unlocked."
    echo "Please unlock Bitwarden first by running:"
    echo "  bw unlock"
    echo "Then set the BW_SESSION environment variable:"
    echo "  export BW_SESSION=\"\$(bw unlock --raw)\""
    echo "Or add it to your shell configuration."
    echo "Then run this script again."
    exit 1
fi

# Get session key - prefer BW_SESSION environment variable, fall back to bw session --raw
echo "Getting Bitwarden session..."
if [ -n "$BW_SESSION" ]; then
    BW_SESSION_RAW="$BW_SESSION"
    echo "Using BW_SESSION environment variable"
elif command -v bw >/dev/null 2>&1; then
    BW_SESSION_RAW=$(bw session --raw 2>/dev/null || echo "")
else
    BW_SESSION_RAW=""
fi

if [ -z "$BW_SESSION_RAW" ]; then
    echo "Error: Could not obtain Bitwarden session."
    echo "Please ensure Bitwarden is unlocked and try one of these:"
    echo "1. Set BW_SESSION environment variable:"
    echo "   export BW_SESSION=\"\$(bw unlock --raw)\""
    echo "2. Or ensure 'bw session --raw' returns a session token"
    exit 1
fi

# Test the session by trying to list folders (a simple operation)
if ! bw list folders --session "$BW_SESSION_RAW" >/dev/null 2>&1; then
    echo "Error: Bitwarden session appears to be invalid or expired."
    echo "Please unlock Bitwarden again and set a fresh BW_SESSION:"
    echo "  export BW_SESSION=\"\$(bw unlock --raw)\""
    exit 1
fi

# Calculate current hash
CURRENT_HASH=$(sha256sum "$ZSHRC_PRIVATE" | cut -d' ' -f1)
echo "Current file hash: ${CURRENT_HASH:0:8}..."

# Check if we have a last sync hash
if [ -f "$LASTSYNC_FILE" ]; then
    LASTSYNC_HASH=$(cat "$LASTSYNC_FILE")
    if [ "$CURRENT_HASH" = "$LASTSYNC_HASH" ]; then
        echo "$ZSHRC_PRIVATE is already synced (hash: ${CURRENT_HASH:0:8}...)"
        exit 0
    fi
    echo "File has changed since last sync."
fi

# Read file content
ZSHRC_CONTENT=$(cat "$ZSHRC_PRIVATE")

# Try to get existing note
echo "Looking for existing note in Bitwarden..."
EXISTING_NOTE=$(bw list items --search "$NOTE_NAME" --session "$BW_SESSION_RAW" 2>/dev/null | jq -r '.[0] // empty')

if [ -n "$EXISTING_NOTE" ]; then
    # Update existing note
    NOTE_ID=$(echo "$EXISTING_NOTE" | jq -r '.id')
    echo "Updating $NOTE_NAME in Bitwarden..."
    # Update the notes field while preserving all other fields
    UPDATE_JSON=$(echo "$EXISTING_NOTE" | jq --arg content "$ZSHRC_CONTENT" '
        .notes = $content |
        if .secureNote == null then .secureNote = {type: 0} else . end
    ')
    echo "$UPDATE_JSON" | bw encode | bw edit item "$NOTE_ID" --session "$BW_SESSION_RAW"
    echo "✓ Updated $NOTE_NAME in Bitwarden"
else
    # Create new note
    echo "Creating $NOTE_NAME in Bitwarden..."
    # Get template for secure note item
    TEMPLATE_JSON=$(bw get template item --session "$BW_SESSION_RAW" 2>/dev/null || echo '')
    if [ -n "$TEMPLATE_JSON" ]; then
        # Use template and modify it
        NEW_ITEM_JSON=$(echo "$TEMPLATE_JSON" | jq \
            --arg name "$NOTE_NAME" \
            --arg content "$ZSHRC_CONTENT" \
            '.type = 2 |
             .name = $name |
             .notes = $content |
             .secureNote.type = 0')
    else
        # Fallback to manual JSON if template fails
        NEW_ITEM_JSON=$(jq -n \
            --arg name "$NOTE_NAME" \
            --arg content "$ZSHRC_CONTENT" \
            '{
                type: 2,
                name: $name,
                notes: $content,
                secureNote: {
                    type: 0
                },
                favorite: false,
                fields: [],
                login: null,
                card: null,
                identity: null
            }')
    fi
    echo "$NEW_ITEM_JSON" | bw encode | bw create item --session "$BW_SESSION_RAW"
    echo "✓ Created $NOTE_NAME in Bitwarden"
fi

# Store current hash
echo "$CURRENT_HASH" > "$LASTSYNC_FILE"
echo "✓ Synced $ZSHRC_PRIVATE to Bitwarden (hash: ${CURRENT_HASH:0:8}...)"
echo "Done!"
