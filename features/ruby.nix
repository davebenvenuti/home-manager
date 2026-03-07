{ lib, features, pkgs, ... }:
{
  home.packages = lib.mkIf features.ruby [
    pkgs.ruby_3_4
  ];
}