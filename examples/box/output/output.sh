#!/bin/sh

printf '\33[?25l'

move_up="\33[6F"

cleanup() {
    printf "$move_up\33[0J\33[0m\33[?25h"
    exit 0
}

trap cleanup INT

draw() {
    printf "$move_up$1"
    sleep 0.2
}

yes '' | head -n 6

while true; do
    draw '\33[97m+----------+\n|          |\n|          |\n|          |\n|          |\n|          |\n+----------+'
done
cleanup
