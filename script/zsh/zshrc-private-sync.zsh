# ZSH plugin for .zshrc.private sync status

_zshrc_private_check_sync() {
    local zshrc_private="$HOME/.zshrc.private"
    local lastsync_file="$HOME/.zshrc.private.lastsync"
    
    # Check if .zshrc.private exists
    if [ ! -f "$zshrc_private" ]; then
        # If file doesn't exist, we can't check sync status
        return 0  # Treat as synced to avoid showing warning
    fi
    
    # Calculate current hash
    local current_hash
    current_hash=$(sha256sum "$zshrc_private" 2>/dev/null | cut -d' ' -f1)
    if [ -z "$current_hash" ]; then
        return 0  # Treat as synced if we can't calculate hash
    fi
    
    # Check if we have a last sync hash
    if [ -f "$lastsync_file" ]; then
        local lastsync_hash
        lastsync_hash=$(cat "$lastsync_file" 2>/dev/null)
        if [ "$current_hash" = "$lastsync_hash" ]; then
            return 0  # Synced
        else
            return 1  # Not synced
        fi
    else
        return 1  # Never synced
    fi
}

_zshrc_private_prompt_indicator() {
    if _zshrc_private_check_sync; then
        # Synced - show nothing
        :
    else
        # Not synced - show warning
        echo -n "%F{yellow}⚠%f"
    fi
}

# Alias for syncing
alias sync-zshrc-private='~/.local/bin/sync-zshrc-private.sh'

# Function to check sync status
zshrc-private-status() {
    if [ ! -f "$HOME/.zshrc.private" ]; then
        echo "ℹ .zshrc.private does not exist"
        return 0
    fi
    
    if _zshrc_private_check_sync; then
        echo "✓ .zshrc.private is synced with Bitwarden"
    else
        echo "⚠ .zshrc.private needs to be synced to Bitwarden"
        echo "Run: sync-zshrc-private"
    fi
}
