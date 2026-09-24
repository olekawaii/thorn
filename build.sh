set -e
BINDIR=~/.local/bin
cd thorn
cargo build --release
mv target/release/thorn $BINDIR || true
cd ../converters
ghc th2ppm.hs
ghc th2sh.hs
cp th2sh th2ppm th2gif $BINDIR || true
cd ..

mkdir ~/.local/share || true
mkdir ~/.local/share/thorn || true
cp -r fonts ~/.local/share/thorn/ || true
