{ pkgsPath ? <nixpkgs> }:

with (import pkgsPath {});

let
  emacspkgs = import (builtins.fetchTarball {
    url = https://github.com/purcell/nix-emacs-ci/archive/emacs-snapshot-2020-01-27.tar.gz;
  });

  emacsInit = writeText "init.el" ''
    (set-face-attribute 'default nil :height 150)
    (scroll-bar-mode 0)
    (tool-bar-mode 0)

    (with-eval-after-load 'gif-screencast
      (define-key gif-screencast-mode-map (kbd "<f8>") 'gif-screencast-toggle-pause)
      (define-key gif-screencast-mode-map (kbd "<f9>") 'gif-screencast-stop)
      (setq gif-screencast-output-directory "."))

    (require 'indent-info)
    (global-indent-info-mode 1)
  '';

  emacsJail = writeShellScriptBin "emacs-jail" ''
    set -euo pipefail
    export HOME=$(mktemp -d)
    exec emacs -Q -l ${emacsInit}
  '';

  listShells = with builtins; writeShellScriptBin "list-shells" (
    "printf '%s\n' " + (concatStringsSep " " (attrNames shells))
  );

  buildInputs = [
    emacsJail
    listShells
    gifsicle
    imagemagick
    scrot
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
      epkgs.gif-screencast
    ]);

  shells = {
    dev = mkShell {
      buildInputs = buildInputs ++ [(emacsWithPackages emacs)];
    };
  } // builtins.mapAttrs (_: value: mkShell {
    buildInputs = buildInputs ++ [(emacsWithPackages value)];
  }) emacspkgs;
in shells.dev // shells
