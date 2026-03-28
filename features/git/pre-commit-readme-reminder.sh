#!/usr/bin/env bash
# Global git pre-commit hook for README documentation reminders
# Managed by Home Manager Nix configuration

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
SKIP_PATTERNS=(
    "*.test.js"
    "*.spec.ts"
    "*.test.py"
    "test/*"
    "spec/*"
    "*.md"  # Don't trigger on markdown files themselves
)

# Check if we're in a git repository
if ! git rev-parse --git-dir > /dev/null 2>&1; then
    exit 0
fi

# Get list of changed files
CHANGED_FILES=$(git diff --cached --name-only --diff-filter=ACM 2>/dev/null || true)

# If no files changed, exit
if [ -z "$CHANGED_FILES" ]; then
    exit 0
fi

# Filter out files matching skip patterns
FILTERED_FILES=()
for file in $CHANGED_FILES; do
    SKIP=false
    for pattern in "${SKIP_PATTERNS[@]}"; do
        if [[ $file == $pattern ]]; then
            SKIP=true
            break
        fi
    done
    
    # Also skip README files themselves
    if [[ "$file" =~ ^[Rr][Ee][Aa][Dd][Mm][Ee]\.* ]]; then
        SKIP=true
    fi
    
    if [ "$SKIP" = false ]; then
        FILTERED_FILES+=("$file")
    fi
done

# If no relevant files changed, exit
if [ ${#FILTERED_FILES[@]} -eq 0 ]; then
    exit 0
fi

# Check for file types that might need README updates
NEEDS_README_CHECK=false
README_REMINDER=""

for file in "${FILTERED_FILES[@]}"; do
    case "$file" in
        # Configuration files
        *.json|*.yaml|*.yml|*.toml|*.ini|*.cfg|*.conf|*.env*)
            NEEDS_README_CHECK=true
            README_REMINDER+="${YELLOW}•${NC} Configuration: ${BLUE}$file${NC}\n"
            ;;
        
        # Source code files (major changes)
        *.py|*.js|*.ts|*.java|*.go|*.rs|*.cpp|*.c|*.h|*.rb|*.php|*.swift)
            # Check if it's a significant source file (not just a test)
            if [[ ! "$file" =~ \.(test|spec)\. ]]; then
                NEEDS_README_CHECK=true
                README_REMINDER+="${YELLOW}•${NC} Source code: ${BLUE}$file${NC}\n"
            fi
            ;;
        
        # Script/automation files
        *.sh|*.bash|*.zsh|Makefile|Dockerfile|docker-compose*.yml|*.rake)
            NEEDS_README_CHECK=true
            README_REMINDER+="${YELLOW}•${NC} Script/automation: ${BLUE}$file${NC}\n"
            ;;
        
        # Package/dependency files
        package.json|requirements.txt|Pipfile|pyproject.toml|Cargo.toml|go.mod|Gemfile|composer.json)
            NEEDS_README_CHECK=true
            README_REMINDER+="${YELLOW}•${NC} Dependencies: ${BLUE}$file${NC}\n"
            ;;
        
        # Project structure files
        .gitignore|.editorconfig|.pre-commit-config.yaml|flake.nix|shell.nix)
            NEEDS_README_CHECK=true
            README_REMINDER+="${YELLOW}•${NC} Project setup: ${BLUE}$file${NC}\n"
            ;;
    esac
done

# If README check is needed, provide guidance
if [ "$NEEDS_README_CHECK" = true ]; then
    echo ""
    echo "${GREEN}📝 README Documentation Reminder${NC}"
    echo "${GREEN}===============================${NC}"
    echo ""
    echo "The following changes might require README updates:"
    echo ""
    echo -e "$README_REMINDER"
    echo ""
    echo "${YELLOW}Consider updating README.md to reflect:${NC}"
    echo "  • New features or functionality"
    echo "  • Changed configuration options"
    echo "  • Updated dependencies or requirements"
    echo "  • Modified usage instructions"
    echo "  • New examples or API changes"
    echo "  • Installation or setup changes"
    echo ""
    
    # Check if README exists
    README_FILES=()
    for readme in README.md readme.md README.rst README.txt README; do
        if [ -f "$readme" ]; then
            README_FILES+=("$readme")
        fi
    done
    
    if [ ${#README_FILES[@]} -gt 0 ]; then
        echo "${GREEN}✅ README file(s) found:${NC} ${README_FILES[*]}"
        echo ""
        
        # Generate LLM prompt context
        if [ -n "${LLM_AGENT_ENABLED:-}" ] && [ "${LLM_AGENT_ENABLED}" = "true" ]; then
            PROMPT_FILE="/tmp/readme_update_prompt_$(date +%s).txt"
            
            cat > "$PROMPT_FILE" << EOF
# README Update Assistant

## Changed Files:
$(printf '%s\n' "${FILTERED_FILES[@]}" | sed 's/^/- /')

## Task:
Review the README.md file and suggest updates based on the changes above.
Focus on:
1. Usage examples that need updating
2. New features to document
3. Configuration changes
4. Installation/requirements updates
5. API changes if applicable
6. Any breaking changes or migration steps

Provide specific suggestions for README updates.
EOF
            
            echo "${BLUE}📋 LLM-ready prompt saved to:${NC} $PROMPT_FILE"
            
            # Try to copy to clipboard if available
            if command -v xclip >/dev/null 2>&1 && [ -n "${DISPLAY:-}" ]; then
                cat "$PROMPT_FILE" | xclip -selection clipboard
                echo "${BLUE}📋 Prompt copied to clipboard (xclip)${NC}"
            elif command -v pbcopy >/dev/null 2>&1; then
                cat "$PROMPT_FILE" | pbcopy
                echo "${BLUE}📋 Prompt copied to clipboard (pbcopy)${NC}"
            fi
        fi
    else
        echo "${YELLOW}⚠️  No README file found. Consider creating one with:${NC}"
        echo "   • Project description"
        echo "   • Installation instructions"
        echo "   • Usage examples"
        echo "   • Configuration options"
        echo "   • Contributing guidelines"
        echo ""
    fi
    
    echo "${YELLOW}To skip this check (not recommended):${NC}"
    echo "  git commit --no-verify"
    echo ""
    
    # Check if we should prompt for confirmation
    if [ "${GIT_HOOK_README_FORCE_CONFIRM:-false}" = "true" ]; then
        read -p "Continue with commit? (y/N): " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            echo "${RED}Commit cancelled. Please update documentation first.${NC}"
            exit 1
        fi
    else
        echo "${GREEN}You may proceed with the commit.${NC}"
        echo "${YELLOW}Remember to update documentation when appropriate.${NC}"
        echo ""
    fi
fi

exit 0