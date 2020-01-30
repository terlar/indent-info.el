{ pkgsPath ? <nixpkgs> }:

with (import pkgsPath {});

let
  emacspkgs = import (builtins.fetchTarball {
    url = https://github.com/purcell/nix-emacs-ci/archive/emacs-snapshot-2020-01-27.tar.gz;
  });

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
      epkgs.editorconfig
      epkgs.evil
    ]);

  shells = {
    dev = mkShell {
      inherit buildInputs;
    };
  } // builtins.mapAttrs (_: value: mkShell {
    buildInputs = buildInputs ++ [(emacsWithPackages value)];
  }) emacspkgs;
in shells.dev // shells
