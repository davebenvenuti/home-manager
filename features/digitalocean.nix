{ lib, config, pkgs, ... }:

let
  # Helper function to create scripts with proper shebang and environment check
  mkScript = name: script: pkgs.writeShellScriptBin name ''
    #!/usr/bin/env bash
    set -euo pipefail
    
    # Check for DIGITALOCEAN_TOKEN
    if [ -z "''${DIGITALOCEAN_TOKEN:-}" ]; then
      echo "❌ Error: DIGITALOCEAN_TOKEN environment variable is not set" >&2
      echo "Please set it before running this script:" >&2
      echo "  export DIGITALOCEAN_TOKEN='your-token-here'" >&2
      exit 1
    fi
    
    ${script}
  '';
in {
  # DigitalOcean feature configuration
  home.packages = with pkgs; [
    # Required tools
    doctl
    s3cmd
    terraform
    
    # Image building tools
    nixos-generators
    qemu
    gzip
    pigz
    parted
    util-linux
    
    # Scripts
    (mkScript "do-build-nixos-image" (builtins.readFile ./digitalocean/scripts/do-build-nixos-image.sh))
    (mkScript "do-upload-nix-image" (builtins.readFile ./digitalocean/scripts/do-upload-nix-image.sh))
    (mkScript "do-enable-nix-image" (builtins.readFile ./digitalocean/scripts/do-enable-nix-image.sh))
  ];
  
  # Terraform configuration for DigitalOcean Spaces bucket
  home.file.".config/digitalocean/terraform/main.tf".source = ./digitalocean/terraform/main.tf;
  home.file.".config/digitalocean/terraform/variables.tf".source = ./digitalocean/terraform/variables.tf;
  home.file.".config/digitalocean/terraform/outputs.tf".source = ./digitalocean/terraform/outputs.tf;
  
  # Documentation
  home.file.".config/digitalocean/README.md".text = builtins.readFile ./digitalocean/README.md;
}