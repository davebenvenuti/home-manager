{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.custom.nodejs;
  
  # Get nodejs package
  nodePkg = pkgs.nodejs;
  
  # Create a unique directory name based on nodejs store path hash
  # This ensures different nodejs versions get different directories
  nodeStoreHash = builtins.substring 0 8 (builtins.hashString "sha256" (toString nodePkg));
  npmGlobalDir = "$HOME/.node-${nodeStoreHash}";
in {
  options.custom.nodejs = {
    enable = mkEnableOption "Node.js configuration";

    globalNpmPackages = mkOption {
      type = with types; listOf str;
      default = [];
      example = literalExpression ''[ "typescript" "@angular/cli" ]'';
      description = "List of npm packages to install globally";
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      nodejs
    ];

    home.sessionVariables = {
      NPM_CONFIG_PREFIX = npmGlobalDir;
      NODE_PATH = "${npmGlobalDir}/lib/node_modules";
    };

    home.sessionPath = [
      "${npmGlobalDir}/bin"
    ];

    home.activation.npmInstallGlobal = lib.hm.dag.entryAfter ["writeBoundary"] ''
      # Create npm global directory structure
      mkdir -p ${npmGlobalDir}/lib/node_modules
      mkdir -p ${npmGlobalDir}/bin
      
      # Ensure npm uses our custom prefix
      export NPM_CONFIG_PREFIX=${npmGlobalDir}
      
      ${if cfg.globalNpmPackages != [] then ''
        $DRY_RUN_CMD ${pkgs.nodejs}/bin/npm install --global $VERBOSE_ARG \
          ${lib.concatStringsSep " " cfg.globalNpmPackages}
      '' else ""}
    '';
  };
}
