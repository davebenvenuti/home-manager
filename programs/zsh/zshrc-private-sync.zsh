# ZSH plugin for .zshrc.private sync status

_zshrc_private_needs_sync_indicator() {
  # return 1 if we should show the sync indicator, 0 otherwise

  local zshrc_private="$HOME/.zshrc.private"
  local lastsync_file="$HOME/.zshrc.private.lastsync"

  # Check if .zshrc.private exists
  if [ ! -f "$zshrc_private" ]; then
    # If file doesn't exist, we can't check sync status
    return 1  # Treat as synced to avoid showing warning
  fi

  # Calculate current hash
  local current_hash
  current_hash=$(sha256sum "$zshrc_private" 2>/dev/null | cut -d' ' -f1)
  if [ -z "$current_hash" ]; then
    return 1  # Treat as synced if we can't calculate hash
  fi

  # Check if we have a last sync hash
  if [ -f "$lastsync_file" ]; then
    local lastsync_hash
    lastsync_hash=$(cat "$lastsync_file" 2>/dev/null)
    if [ "$current_hash" = "$lastsync_hash" ]; then
      return 1  # Synced
    else
      return 0  # Not synced
    fi
  else
    return 0  # Never synced
  fi
}

# Alias for syncing
alias sync-zshrc-private='~/.local/bin/sync-zshrc-private.sh'
