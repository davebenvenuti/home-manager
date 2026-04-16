{ lib, features, pkgs, config, ... }:
let
  pi-coding-agent = pkgs.buildNpmPackage (finalAttrs: {
    pname = "pi-coding-agent";
    version = "0.67.1";
    nodejs = pkgs.nodejs_22;

    src = pkgs.fetchFromGitHub {
      owner = "badlogic";
      repo = "pi-mono";
      rev = "v${finalAttrs.version}";
      hash = "sha256-Hh4nRMxtlzRHDgr8P6Pm7FDzV2f+6MIxNmVMKtnwb8I=";
    };

    npmDepsHash = "sha256-t1M9qED2BeJGDgbC1ZHsiTT5NMXmtEr+rsu2kKM0MLg=";

    npmWorkspace = "packages/coding-agent";
    npmFlags = [ "--legacy-peer-deps" ];
    makeCacheWritable = true;

    nativeBuildInputs = [ pkgs.pkg-config ];
    buildInputs = [
      pkgs.cairo
      pkgs.pango
      pkgs.libjpeg
      pkgs.giflib
      pkgs.librsvg
      pkgs.pixman
    ];

    preBuild = ''
      npx tsgo -p packages/tui/tsconfig.build.json
      npx tsgo -p packages/ai/tsconfig.build.json
      npx tsgo -p packages/agent/tsconfig.build.json
    '';

    postInstall = ''
      workspaceRoot="$out/lib/node_modules/pi-monorepo"
      mkdir -p "$workspaceRoot/packages"
      cp -r packages/{ai,agent,tui,coding-agent} "$workspaceRoot/packages/"

      # Keep required workspace links and drop only unresolved leftovers.
      find "$workspaceRoot/node_modules" -xtype l -delete
    '';

    meta = {
      description = "Minimal terminal coding harness for agentic workflows";
      homepage = "https://github.com/badlogic/pi-mono";
      license = lib.licenses.mit;
      mainProgram = "pi";
      platforms = lib.platforms.linux;
    };
  });
 in
   # Full pi installation from source (Linux only)
   (lib.mkIf features.agents.pi {
     home.packages = [
       pi-coding-agent
     ];

     # Set environment variable to disable Pi version checks
     home.sessionVariables = {
       PI_SKIP_VERSION_CHECK = "1";
     };

     home.file.".pi/agent/models.json".source = ./pi/models.json;

     home.file.".pi/agent/AGENTS.md".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.agents/AGENTS.md";
     # Note: pi looks for skills in ~/.agents/skills/ directly, so no symlink needed

     home.file.".pi/custom-extensions/notify.ts".source = ./extensions/pi/notify.ts;

     # Merge our settings with existing settings.json
     home.activation.piSettingsMerge = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
       SETTINGS_FILE="$HOME/.pi/agent/settings.json"
       OUR_SETTINGS="${./pi/settings.json}"

       # Create directory if it doesn't exist
       mkdir -p "$(dirname "$SETTINGS_FILE")"

       if [ -f "$SETTINGS_FILE" ]; then
         # First, ensure our custom extension is in the extensions array
         # Then merge other settings with ours taking precedence
         ${pkgs.jq}/bin/jq -s '
           .[0] as $our |
           .[1] as $existing |
           # Remove lastChangelogVersion if present
           ($existing | del(.lastChangelogVersion)) as $cleanExisting |
           # Merge extensions arrays
           ($cleanExisting.extensions // []) as $existingExt |
           ($our.extensions // []) as $ourExt |
           $cleanExisting * $our |
           .extensions = (($existingExt + $ourExt) | unique)
         ' "$OUR_SETTINGS" "$SETTINGS_FILE" > "$SETTINGS_FILE.tmp" && mv "$SETTINGS_FILE.tmp" "$SETTINGS_FILE"
       else
         # No existing file, just copy our settings
         cp "$OUR_SETTINGS" "$SETTINGS_FILE"
       fi
     '';
   })
