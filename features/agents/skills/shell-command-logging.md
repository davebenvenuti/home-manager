# Shell Command Logging and Timeout Guidelines

## Command Execution Best Practices

When executing shell commands through AI agents, follow these guidelines:

### 1. Default Timeout
Always set a reasonable timeout for shell commands to prevent hanging processes. The default timeout should be **300 seconds (5 minutes)**.

```bash
# Example with timeout
timeout 300 your-command-here
```

### 2. Command Output Logging
Always tee command output to a timestamped log file for debugging and auditing purposes.

```bash
# Create timestamp for log file
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
LOG_FILE="/tmp/agent_command_${TIMESTAMP}.log"

# Execute command with tee to log file
your-command-here 2>&1 | tee "$LOG_FILE"
```

### 3. Combined Pattern
Combine both timeout and logging for robust command execution:

```bash
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
LOG_FILE="/tmp/agent_command_${TIMESTAMP}.log"

# Execute with timeout and logging
timeout 300 your-command-here 2>&1 | tee "$LOG_FILE"
```

### 4. Override Defaults
The default timeout of 300 seconds can be overridden when appropriate:
- For very long-running operations (e.g., database migrations, large builds)
- When the user explicitly requests no timeout
- For interactive commands that require user input

```bash
# Example with custom timeout (600 seconds = 10 minutes)
timeout 600 long-running-command 2>&1 | tee "$LOG_FILE"
```

### 5. Exit Code Handling
Always check the exit code of commands, especially when using timeout:

```bash
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
LOG_FILE="/tmp/agent_command_${TIMESTAMP}.log"

if timeout 300 your-command-here 2>&1 | tee "$LOG_FILE"; then
    echo "Command completed successfully"
else
    EXIT_CODE=$?
    if [ $EXIT_CODE -eq 124 ]; then
        echo "Command timed out after 300 seconds"
    else
        echo "Command failed with exit code: $EXIT_CODE"
    fi
    # Check log file for details
    echo "See log file for details: $LOG_FILE"
fi
```

### 6. Log File Management
- Log files are stored in `/tmp/` by default
- Consider cleaning up old log files periodically
- For important operations, you may want to keep logs in a more permanent location

### 7. Special Cases
Some commands may not work well with `tee` or `timeout`:
- **Interactive commands**: Avoid timeout for commands requiring user input
- **Background processes**: Handle differently as they don't block
- **Piped commands with tee**: Ensure proper handling of stdin/stdout

## Example Implementation

Here's a reusable function template:

```bash
run_with_logging() {
    local cmd="$1"
    local timeout_seconds="${2:-300}"  # Default to 300 seconds
    
    TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
    LOG_FILE="/tmp/agent_command_${TIMESTAMP}.log"
    
    echo "Executing: $cmd"
    echo "Logging to: $LOG_FILE"
    echo "Timeout: ${timeout_seconds} seconds"
    
    if timeout "$timeout_seconds" bash -c "$cmd" 2>&1 | tee "$LOG_FILE"; then
        echo "✓ Command completed successfully"
        return 0
    else
        local exit_code=$?
        if [ $exit_code -eq 124 ]; then
            echo "✗ Command timed out after ${timeout_seconds} seconds"
        else
            echo "✗ Command failed with exit code: $exit_code"
        fi
        echo "See log file for details: $LOG_FILE"
        return $exit_code
    fi
}

# Usage examples:
# run_with_logging "ls -la"
# run_with_logging "long_running_script.sh" 600  # 10 minute timeout
```