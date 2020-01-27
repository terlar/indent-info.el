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
  emacsJail = writeShellScriptBin "emacs-jail" ''
    set -euo pipefail
    export HOME=$(mktemp -d)
    exec emacs -Q
  '';

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
      buildInputs = [
        emacsJail
      ];
    };

    emacs25 = mkShell {
      buildInputs = [
        emacsJail
        (emacsWithPackages emacs25)
      ];
    };

    emacs26 = mkShell {
      buildInputs = [
        emacsJail
        (emacsWithPackages emacs26)
      ];
    };

    emacsGit = mkShell {
      buildInputs = [
        emacsJail
        (emacsWithPackages emacsGit)
      ];
    };
  };
in shells.dev // shells
