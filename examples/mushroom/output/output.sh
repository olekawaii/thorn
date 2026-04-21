#!/bin/sh

printf '\33[?25l'

move_up="\33[7F"

cleanup() {
    printf "$move_up\33[0J\33[0m\33[?25h"
    exit 0
}

trap cleanup INT

draw() {
    printf "$move_up$1"
    sleep 0.2
}

yes '' | head -n 7

while true; do
    draw '  \33[91m__\33[97mc\33[91m__\33[97m,\33[91m___\n(_\33[97mc\33[91m__\33[97mO\33[91m_/\134_\33[97m3\33[91m_)\n       \33[97m.\n     | |\n    / __\134\n       .   .\33[91m__\n     \33[97m| |  \33[91m(__\33[97mo\33[91m_)\n     \33[97m| |   (  )'
done
cleanup
