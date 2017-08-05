{ nixpkgs ? import <nixpkgs> {} }:

let
  inherit (nixpkgs) pkgs;
  myBooks = import ../../server/my-books/shell.nix {};
  baseImage = pkgs.dockerTools.buildImage {
    name = "my-books-server-base";
    contents = myBooks;
  };  
in {
  couchAuthProxy = pkgs.dockerTools.buildImage {
    name = "my-books-couchAuthProxy";
    fromImage = baseImage;
    config = {
      Cmd = [ "CouchAuthProxy" ];
    };
  };
  bookIndexer = pkgs.dockerTools.buildImage {
    name = "my-books-bookIndexer";
    fromImage = baseImage;
    config = {
      Cmd = [ "BookIndexer" ];
    };
  };
  web = pkgs.dockerTools.buildImage {
    name = "my-books-web";
    fromImage = baseImage;
    config = {
      Cmd = [ "MyBooks" ];
      ExposedPorts = {
        "8000/tcp" = {};
      };
    };
  };
}
