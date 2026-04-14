# AI Agent Skills

This directory contains reusable skill documents for AI coding assistants (opencode, pi). These skills provide guidance on specific development workflows and best practices.

## Available Skills

### [Rails Development with Nix](rails-nix-development.md)
**Description**: Guides for developing Ruby on Rails applications using Nix for reproducible development environments. Includes project setup, dependency management, and database configuration patterns.

### [Home Manager Configuration Management](home-manager-configuration.md)
**Description**: Instructions for managing and applying changes to this specific home-manager configuration, including feature flags, development workflow, and troubleshooting.

### [Shell Command Logging and Timeout Guidelines](shell-command-logging.md)
**Description**: Best practices for executing shell commands with proper timeout handling and logging to ensure reliable and debuggable command execution.

## Adding New Skills

To add a new skill:

1. Create a new `.md` file in this directory
2. Start with a clear title and description
3. Document the skill with practical examples and guidelines
4. Update this README to include the new skill

## Skill Structure

Each skill file should follow this structure:

1. **Title** - Clear, descriptive name
2. **Description** - Brief summary of what the skill covers
3. **Main Content** - Detailed guidance, examples, and best practices
4. **Related Skills** - Optional references to other relevant skills

## Usage

These skills are automatically symlinked to all enabled AI agents when the agents feature is enabled in the home-manager configuration.

Agents can reference these skills when working on relevant tasks to ensure consistent, high-quality assistance.