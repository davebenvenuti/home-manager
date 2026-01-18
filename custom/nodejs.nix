{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.custom.nodejs;
  
  # Create a derivation for each npm package
  # This builds a Nix derivation that runs `npm install --global` during build time.
  # Each package gets its own derivation for better caching - changing one package
  # only rebuilds that package, not all of them.
  mkGlobalNpmPackage = pkgName: pkgs.runCommand "npm-global-${builtins.replaceStrings ["@" "/"] ["_" "_"] pkgName}" {
    buildInputs = [ pkgs.nodejs pkgs.cacert ];
    # Add SSL certificates for npm to work in the Nix sandbox
    SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
    NODE_EXTRA_CA_CERTS = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
    inherit pkgName;
  } ''
    # Create the directory structure that npm expects for global installations
    mkdir -p $out/lib/node_modules
    mkdir -p $out/bin
    
    # Tell npm to install to our custom output directory instead of the default
    # global location (which would be in the read-only Nix store)
    export npm_config_prefix=$out
    
    # Set npm cache and tmp directories to writable locations
    # In the Nix build sandbox, /homeless-shelter is not writable
    export npm_config_cache=$TMPDIR/npm-cache
    export npm_config_tmp=$TMPDIR/npm-tmp
    mkdir -p $TMPDIR/npm-cache
    mkdir -p $TMPDIR/npm-tmp
    
    # Install the package globally. This happens during Nix build time, not
    # during home-manager activation, making it more reproducible.
    npm install --global $pkgName
  '';
  
  # Create all package derivations
  globalPackageDerivations = map mkGlobalNpmPackage cfg.globalNpmPackages;
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
    ] ++ globalPackageDerivations;

    # Set NODE_PATH to include all globally installed npm packages.
    # Node.js will look in these directories when requiring modules.
    # We use a colon-separated list of each package's node_modules directory.
    home.sessionVariables = lib.mkIf (cfg.globalNpmPackages != []) {
      NODE_PATH = lib.concatStringsSep ":" (map (pkg: "${pkg}/lib/node_modules") globalPackageDerivations);
    };

    # Add each package's bin directory to PATH so global npm binaries are available.
    home.sessionPath = lib.mkIf (cfg.globalNpmPackages != []) (
      map (pkg: "${pkg}/bin") globalPackageDerivations
    );
  };
}
