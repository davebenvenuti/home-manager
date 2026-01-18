{ lib, pkgs, ... }:

let
  emacsSourceDir = ./emacs;

  # Get directory contents
  dirContents = builtins.readDir emacsSourceDir;

  # Get all .el files
  elispFiles = lib.filterAttrs (name: type: type == "regular" && lib.hasSuffix ".el" name) dirContents;

  # Get all subdirectories
  subDirs = lib.filterAttrs (_: type: type == "directory") dirContents;

  # Find subdirectories that contain a .keep file
  dirsWithKeep = lib.filterAttrs (name: _:
    let subDirContents = builtins.readDir (lib.path.append emacsSourceDir name);
    in builtins.hasAttr ".keep" subDirContents
  ) subDirs;
in
{
  # We'll manage our own config for now
  programs.emacs.enable = false;

  home.packages = with pkgs; [
    emacs-nox
    copilot-language-server
  ];

  # Install elisp files and .keep files to ~/.emacs.d
  home.file =
    # Install all .el files
    (lib.mapAttrs' (name: _: {
      name = ".emacs.d/${name}";
      value.source = lib.path.append emacsSourceDir name;
    }) elispFiles)
    //
    # Install .keep files in subdirectories
    (lib.mapAttrs' (dir: _: {
      name = ".emacs.d/${dir}/.keep";
      value.source = lib.path.append emacsSourceDir "${dir}/.keep";
    }) dirsWithKeep);
}
