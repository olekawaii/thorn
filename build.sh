set -e
BINDIR=~/.local/bin
cargo build --release
mv target/release/thorn $BINDIR || true
cd converters
ghc thorn-to-ppm.hs
ghc thorn-to-sh.hs
cp thorn-to-sh thorn-to-ppm thorn-to-gif $BINDIR || true
cd ..

mkdir ~/.local/share || true
mkdir ~/.local/share/thorn || true
cp -r fonts ~/.local/share/thorn/ || true
