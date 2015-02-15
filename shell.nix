with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, array, attoparsec, base, base16-bytestring, binary
             , bytestring, Cabal, containers, cpu, deepseq, deepseq-generics
             , directory, io-streams, lens, mtl, network, exceptions, process
             , QuickCheck, quickcheck-io, resourcet, retry, stdenv, system-filepath
             , tasty, tasty-hunit, tasty-quickcheck, text, time, transformers, vector
             , data-default-class, blaze-builder
             
             , criterion
             }:
             mkDerivation {
               pname = "kdb-haskell";
               version = "0.1.0";
               src = ./.;
               buildDepends = [
                 array attoparsec base bytestring cpu deepseq io-streams QuickCheck
                 time vector data-default-class network exceptions transformers lens
                 blaze-builder
                 
                 criterion
               ];
               buildTools = [
                 haskellngPackages.ghc-mod
               ];
               testDepends = [
                 array attoparsec base bytestring cpu deepseq io-streams QuickCheck
                 time vector data-default-class network exceptions transformers lens
                 blaze-builder
                 
                 base16-bytestring binary Cabal
                 containers deepseq-generics directory
                 process quickcheck-io resourcet retry system-filepath
                 tasty tasty-hunit tasty-quickcheck text time vector
               ];
               homepage = "https://github.com/jkozlowski/kdb-haskell";
               description = "Haskell bindings for KDB+";
               license = stdenv.lib.licenses.mit;
             }) {};
in
  pkg.env
