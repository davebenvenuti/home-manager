{ lib, features, pkgs, ... }:
let
  pi-agent-pkg = pkgs.buildNpmPackage (finalAttrs: {
    pname = "pi-agent";
    version = "0.60.0";
    nodejs = pkgs.nodejs_22;

    src = pkgs.fetchFromGitHub {
      owner = "badlogic";
      repo = "pi-mono";
      rev = "v${finalAttrs.version}";
      hash = "sha256-CRXPLsJm2JFYO/V+sfD+lfht0AOSyNuHMz527W1Iwu4=";
    };

    npmDepsHash = "sha256-f1x4EDS/uDK4dRlARm3jmMExCTC7rUtB08RSewhUpnk=";

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
{
  home.packages = lib.mkIf features.pi [ pi-agent-pkg ];
}