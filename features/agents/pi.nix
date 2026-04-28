{ lib, features, pkgs, config, ... }:
let
  # Nix-managed pi settings. These are merged into the existing settings.json
  # on each home-manager switch, preserving any runtime changes pi has made
  # to other fields (lastChangelogVersion, theme, compaction, etc.).
  #
  # For arrays (packages, extensions): nix-managed entries are added to the
  # existing list (deduped), so runtime additions are preserved.
  managedPiSettings = builtins.toJSON {
    defaultProvider = "deepseek";
    defaultModel = "deepseek-v4-flash";
    extensions = [
      "~/.pi/custom-extensions/notify.ts"
    ] ++ lib.optionals features.agents.pi [
      "~/.pi/custom-extensions/todo.ts"
    ];
    models = {
      "deepseek-v4-flash" = {
        provider = "deepseek";
        description = "DeepSeek V4 Flash";
      };
    };
  };

  # Nix-managed pi provider configurations. These are merged into the existing models.json
  # on each home-manager switch, preserving any runtime changes pi has made.
  managedModels = builtins.toJSON {
    providers = {
      deepseek = {
        baseUrl = "https://api.deepseek.com/v1";
        api = "openai-completions";
        apiKey = "DEEPSEEK_API_KEY";
        authHeader = true;
        models = [
          {
            id = "deepseek-v4-flash";
            name = "DeepSeek V4 Flash";
            reasoning = true;
            contextWindow = 1000000;
            maxTokens = 5000;
            cost = {
              input = 0.14;
              output = 0.28;
              cacheRead = 0.0028;
              cacheWrite = 1.1;
            };
          }
        ];
      };
    };
  };

  # Packages/extensions to auto-install via `pi install` on each home-manager switch.
  # Items use pi's install syntax, e.g. "npm:pi-web-access" or "ext:some-extension".
  piInstall = [
    "npm:pi-web-access"
  ];
in lib.mkMerge [
  # Extensions deployed for any system with pi (even if pi is installed externally)
  {
    home.file.".pi/custom-extensions/notify.ts".source = ./extensions/pi/notify.ts;

    home.activation.piSettingsMerge = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      mkdir -p $HOME/.pi/agent

      # Merge settings.json
      SETTINGS_FILE="$HOME/.pi/agent/settings.json"
      MANAGED_SETTINGS='${managedPiSettings}'

      if [ ! -f "$SETTINGS_FILE" ]; then
        echo "$MANAGED_SETTINGS" | ${pkgs.jq}/bin/jq '.' > "$SETTINGS_FILE"
      else
        # Merge nix-managed settings into existing file:
        # - Scalar/object fields are overwritten (managed wins)
        # - Array fields (packages, extensions): union of existing + managed (preserves runtime additions)
        # - lastChangelogVersion is stripped to avoid stale changelog dismissals
        ${pkgs.jq}/bin/jq -s '
          (.[0] | del(.lastChangelogVersion)) as $existing | .[1] as $managed |
          ($existing.packages // []) as $existingPkgs |
          ($managed.packages // []) as $managedPkgs |
          ($existingPkgs + $managedPkgs | unique) as $mergedPkgs |
          ($existing.extensions // []) as $existingExts |
          ($managed.extensions // []) as $managedExts |
          ($existingExts + $managedExts | unique) as $mergedExts |
          $existing * ($managed | del(.packages, .extensions)) |
          .packages = $mergedPkgs |
          .extensions = $mergedExts
        ' "$SETTINGS_FILE" <(echo "$MANAGED_SETTINGS") > "$SETTINGS_FILE.tmp" \
          && mv "$SETTINGS_FILE.tmp" "$SETTINGS_FILE"
      fi

      # Merge models.json
      MODELS_FILE="$HOME/.pi/agent/models.json"
      MANAGED_MODELS='${managedModels}'

      if [ ! -f "$MODELS_FILE" ]; then
        echo "$MANAGED_MODELS" | ${pkgs.jq}/bin/jq '.' > "$MODELS_FILE"
      else
        # Merge nix-managed models into existing file:
        # For providers, we merge at the provider level - nix-managed provider configs
        # overwrite existing ones, but we preserve any providers not managed by nix
        ${pkgs.jq}/bin/jq -s '
          .[0] as $existing | .[1] as $managed |
          # Merge providers: managed providers overwrite existing ones
          # but we keep any existing providers not in managed
          ($existing.providers // {}) as $existingProviders |
          ($managed.providers // {}) as $managedProviders |
          $existing | .providers = ($existingProviders * $managedProviders)
        ' "$MODELS_FILE" <(echo "$MANAGED_MODELS") > "$MODELS_FILE.tmp" \
          && mv "$MODELS_FILE.tmp" "$MODELS_FILE"
      fi
    '';
  }

  # Full pi installation from source (when enabled)
  (lib.mkIf features.agents.pi (let
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

      nativeBuildInputs = [ pkgs.pkg-config pkgs.makeWrapper ];
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

        # Wrap pi so npm:... installs work: set a writable global prefix and
        # ensure nodejs/npm are on PATH (the Nix store is read-only).
        wrapProgram $out/bin/pi \
          --set npm_config_prefix '$HOME/.npm-global' \
          --prefix PATH : ${pkgs.nodejs_22}/bin
      '';

      meta = {
        description = "Minimal terminal coding harness for agentic workflows";
        homepage = "https://github.com/badlogic/pi-mono";
        license = lib.licenses.mit;
        mainProgram = "pi";
        # Try to build on Darwin too
        platforms = lib.platforms.linux ++ lib.platforms.darwin;
      };
    });
  in {
    home.packages = [
      pi-coding-agent
    ];

    # Set environment variable to disable Pi version checks
    home.sessionVariables = {
      PI_SKIP_VERSION_CHECK = "1";
    };

    home.file.".pi/agent/AGENTS.md".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.agents/AGENTS.md";
    home.file.".pi/custom-extensions/todo.ts".source = "${pi-coding-agent}/lib/node_modules/pi-monorepo/examples/extensions/todo.ts";
    # Note: pi looks for skills in ~/.agents/skills/ directly, so no symlink needed

    home.activation.piInstall = lib.hm.dag.entryAfter [ "piSettingsMerge" ] (
      if piInstall == [] then "" else ''
        # pi's npm:... installs use a wrapper that sets npm_config_prefix and
        # adds nodejs/npm to PATH, so no additional env setup needed here.
        ${builtins.concatStringsSep "\n" (map (item: ''
          echo "[home-manager] pi install: ${item}"
          ${lib.getExe pi-coding-agent} install "${item}"
        '') piInstall)}
      ''
    );
  }))
]
