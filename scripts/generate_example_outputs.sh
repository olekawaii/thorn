set -e

EXAMPLES_DIR=./examples
OUTPUT_DIR="$EXAMPLES_DIR/output-gifs"

rm $OUTPUT_DIR/*.gif

IFS=$'\n'
for expr in $(cat $OUTPUT_DIR/to-generate)
do
    filename="$(echo $expr | sed 's/.*  \+//' | sed 's/ \+/_/g').gif"
    thorn --eval $expr $EXAMPLES_DIR | th2gif > $OUTPUT_DIR/$filename
done
