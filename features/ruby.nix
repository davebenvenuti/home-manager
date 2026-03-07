{ lib, features, pkgs, ... }:
{
  home.packages = with pkgs; lib.mkIf features.ruby [
    ruby_3_4

  ];
}
