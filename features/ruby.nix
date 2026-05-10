{ pkgs, ... }:
{
  home.packages = with pkgs; [
    ruby_4_0
  ];
}
