{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;
  
  couchdb = import ../hs-couchdb/project.nix { };
  onedrive = import ../hs-onedrive/project.nix { };
  
  f = { mkDerivation, aeson, async, attoparsec, base, blaze-builder
      , blaze-html, blaze-markup, bytestring, case-insensitive, conduit
      , conduit-combinators, conduit-extra, containers, directory
      , epub-metadata, errors, exceptions, filepath, http-client, http-conduit, http-types, lens
      , lens-aeson, monad-control, mtl, optparse-applicative, reroute
      , Spock-core, stdenv, stm, text, transformers, wai, wai-extra
      , wai-middleware-static, zip-archive
      }:
      mkDerivation {
        pname = "MyBooks";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson async attoparsec base blaze-builder blaze-html blaze-markup
          bytestring case-insensitive conduit conduit-combinators
          conduit-extra containers directory epub-metadata errors exceptions
          filepath http-client http-conduit http-types
          lens lens-aeson monad-control mtl optparse-applicative reroute
          Spock-core stm text transformers wai wai-extra
          wai-middleware-static zip-archive couchdb onedrive
        ];
        license = stdenv.lib.licenses.mpl20;
        doHaddock = false;
        doCheck = false;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
