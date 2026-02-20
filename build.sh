set -e
BINDIR=~/.local/bin
cargo build --release
mv target/release/thorn $BINDIR || true
cp scripts/thorn-to-gif $BINDIR || true
cd converters
ghc thorn-to-ppm.hs
ghc thorn-to-sh.hs
mv thorn-to-sh thorn-to-ppm $BINDIR || true
