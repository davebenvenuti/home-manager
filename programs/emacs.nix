{ config, lib, ... }:

let
  # We need two paths to the same directory:
  # - emacsSourceDir: A Nix path (./emacs) used for builtins.readDir during evaluation.
  #   This must be a Nix path to satisfy pure evaluation mode in flakes.
  # - emacsConfigDir: An absolute string path used for mkOutOfStoreSymlink targets.
  #   This points to the actual checkout so symlinks enable live editing without rebuilding.
  emacsConfigDir = "${config.home.homeDirectory}/src/github.com/davebenvenuti/home-manager/programs/emacs";
  emacsSourceDir = ./emacs;

  # Get directory contents
  dirContents = builtins.readDir emacsSourceDir;

  # Get all .el files
  elispFiles = lib.filterAttrs (name: type: type == "regular" && lib.hasSuffix ".el" name) dirContents;

  # Get all subdirectories
  subDirs = lib.filterAttrs (_: type: type == "directory") dirContents;

  # Find subdirectories that contain a .keep file
  dirsWithKeep = lib.filterAttrs (name: _:
    let subDirContents = builtins.readDir "${emacsSourceDir}/${name}";
    in builtins.hasAttr ".keep" subDirContents
  ) subDirs;
in
{
  # We're not using home-manager's built-in emacs program configuration.
  # Instead, we manage emacs config files directly via symlinks below.
  # This gives us more control over the configuration structure and
  # allows live editing without rebuilding home-manager.
  programs.emacs.enable = false;

  # Symlink elisp files and .keep files to ~/.emacs.d
  home.file =
    # Symlink all .el files
    (lib.mapAttrs' (name: _: {
      name = ".emacs.d/${name}";
      value.source = config.lib.file.mkOutOfStoreSymlink "${emacsConfigDir}/${name}";
    }) elispFiles)
    //
    # Symlink .keep files in subdirectories
    (lib.mapAttrs' (dir: _: {
      name = ".emacs.d/${dir}/.keep";
      value.source = config.lib.file.mkOutOfStoreSymlink "${emacsConfigDir}/${dir}/.keep";
    }) dirsWithKeep);
}
