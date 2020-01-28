let
  emacsOverlay = import (builtins.fetchTarball {
    url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
  });
in
{ pkgsPath ? <nixpkgs>
, overlays ? [emacsOverlay]
}:

with (import pkgsPath { inherit overlays; });

let
  emacsInit = writeText "init.el" ''
    (set-face-attribute 'default nil :height 150)
    (require 'indent-info)
    (scroll-bar-mode 0)
    (tool-bar-mode 0)
    (global-indent-info-mode 1)
  '';

  emacsJail = writeShellScriptBin "emacs-jail" ''
    set -euo pipefail
    export HOME=$(mktemp -d)
    exec emacs -Q -l ${emacsInit}
  '';

  buildInputs = [
    emacsJail
    peek
  ];

  emacsWithPackages =
    x: let
      emacsPkgs = emacsPackagesGen x;
    in emacsPkgs.emacsWithPackages (epkgs: [
      (import ./. {
        inherit (emacsPkgs) elpaBuild;
        inherit writeText;
      })
      epkgs.camcorder
      epkgs.editorconfig
      epkgs.evil
    ]);

  shells = {
    dev = mkShell {
      inherit buildInputs;
    };

    emacs25 = mkShell {
      buildInputs = buildInputs ++ [(emacsWithPackages emacs25)];
    };

    emacs26 = mkShell {
      buildInputs = buildInputs ++ [(emacsWithPackages emacs26)];
    };

    emacsGit = mkShell {
      buildInputs = buildInputs ++ [(emacsWithPackages emacsGit)];
    };
  };
in shells.dev // shells
