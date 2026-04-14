{ lib, features, pkgs, config, ... }:
let
  pi-coding-agent = pkgs.buildNpmPackage (finalAttrs: {
    pname = "pi-coding-agent";
    version = "0.63.1";
    nodejs = pkgs.nodejs_22;

    src = pkgs.fetchFromGitHub {
      owner = "badlogic";
      repo = "pi-mono";
      rev = "v${finalAttrs.version}";
      hash = "sha256-Cb0I2iHIsH0ffk/yuzuwTZjd4VUZ7WjgdUuv2yKTMg8=";
    };

    npmDepsHash = "sha256-GrMNTZyg9K0kGJoKSyWd37PfOFbds630PNzrwDbXE4E=";

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
 in lib.mkMerge [
   # Extensions deployed for any system with pi (even if pi is installed externally)
   {
     home.file.".pi/custom-extensions/notify.ts".source = ./extensions/pi/notify.ts;

     # Ensure settings.json includes our custom extensions path
     home.activation.piCustomExtensions = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
       SETTINGS_FILE="$HOME/.pi/agent/settings.json"
       if [ -f "$SETTINGS_FILE" ]; then
         ${pkgs.jq}/bin/jq '.extensions = (.extensions // []) + ["~/.pi/custom-extensions/notify.ts"] | .extensions |= unique' \
           "$SETTINGS_FILE" > "$SETTINGS_FILE.tmp" && mv "$SETTINGS_FILE.tmp" "$SETTINGS_FILE"
       fi
     '';
   }

   # Full pi installation from source (Linux only)
   (lib.mkIf features.agents.pi {
     home.packages = [
       pi-coding-agent
     ];

     # Set environment variable to disable Pi version checks
     home.sessionVariables = {
       PI_SKIP_VERSION_CHECK = "1";
     };

     home.file.".pi/agent/settings.json".source = ./pi/settings.json;
     home.file.".pi/agent/models.json".source = ./pi/models.json;
     
     home.file.".pi/agent/AGENTS.md".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.agents/AGENTS.md";
     # Note: pi looks for skills in ~/.agents/skills/ directly, so no symlink needed
   })
 ]
