{
  description = "A language for making ascii art animations";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }: 
  let 
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in
  {
    devShells.${system}.default = pkgs.mkShell rec {
      inputsFrom = with self.packages.${system}; [ 
        thorn 
        thorn-converters
      ];
      packages = with self.packages.${system}; [ 
        pkgs.ffmpeg 
        thorn
        thorn-converters
      ];
    };
    packages.${system} = rec {
      thorn = pkgs.rustPlatform.buildRustPackage rec {
        pname = "thorn";
        version = "1.0";
        src = ./.;
        cargoLock.lockFile = ./Cargo.lock;
        meta = with pkgs.lib; {
          description = "Interpreter of the thorn language";
          homepage = "https://codeberg.org/olekawaii/thorn";
          license = licenses.gpl3;
          maintainers = [];
        };
      };

      thorn-converters = pkgs.stdenv.mkDerivation {
        name = "thorn-converters";
        src = ./converters;
        nativeBuildInputs = with pkgs; [ ghc makeWrapper ];
        buildPhase = ''
          ghc thorn-to-sh
          ghc thorn-to-ppm
        '';
        installPhase = ''
          mkdir -p $out/bin
          cp thorn-to-sh thorn-to-ppm thorn-to-gif $out/bin
          wrapProgram $out/bin/thorn-to-gif \
            --add-flags "--font-dir ${fonts}/share/fonts"
        '';
      };

      fonts = pkgs.stdenv.mkDerivation {
        name = "thorn-fonts";
        src = ./fonts;
        installPhase = ''
          mkdir -p $out/share/fonts
          cp * $out/share/fonts
        '';
      };

      default = pkgs.symlinkJoin {
        name = "thorn-complete";
        paths = [ thorn thorn-converters fonts];
      };
    };
  };
}
