#!/bin/sh

printf '\33[?25l'

move_up="\33[9F"

cleanup() {
    printf "$move_up\33[0J\33[0m\33[?25h"
    exit 0
}

trap cleanup INT

draw() {
    printf "$move_up$1"
    sleep 0.2
}

yes '' | head -n 9

while true; do
    draw '      \33[97m__          ___               \n     |  |  _     /   \134              \n     |__| (_)   \47\47\47"```             \n                   |     \47          \n  .----.----.----. :  n_n .--.    . \n  [\47   |\47   |\47   ]   (-.-, (__`._.\47 \n.--.   \47    \47   .--.  _________     \n|\47 | --.----.---|\47 |  `-------\47     \n|  |___|____|___|  |     ) (        \n:__|            :__|    / : \134       '
    draw '\33[4E\33[34C \33[97m.\33[5E'
    draw '\n\n\33[26C\33[97mz\n\33[25C \n\33[35C \n\33[32C   \n\33[32C.\n\33[33C\47\n\n'
    draw '\33[6E\33[32C\33[97m:\n\33[32C| \n\n'
    draw '\n\33[28C\33[97mZ\n\33[26C \33[7E'
    sleep 0.2
    draw '\n\33[28C \33[8E'
    sleep 0.2
    draw '\33[3E\33[25C\33[97m\47\33[6E'
    sleep 0.2
    draw '\n\n\33[26C\33[97mz\n\33[25C \33[6E'
    sleep 0.2
    draw '\n\33[28C\33[97mZ\n\33[26C \33[7E'
    sleep 0.2
    draw '\n\33[28C \33[5E\33[32C\33[97m.\n\33[32C \47\n\n'
    draw '\33[4E\33[35C\33[97m.\n\33[32C_.\47\n\33[32C \n\33[33C \n\n'
done
cleanup
