#! /bin/bash

# Function to check if a terminfo capability exists
check_capability() {
    local cap="$1"
    tput -T "${TERM}" "${cap}" >/dev/null 2>&1
    return $?
}

# All the colors!!!

# Basic
_C_RED=$(tput setaf 1)
_C_GREEN=$(tput setaf 2)
_C_BLUE=$(tput setaf 4)
_C_CYAN=$(tput setaf 6)
_C_MAGENTA=$(tput setaf 5)
_C_YELLOW=$(tput setaf 3)
_C_WHITE=$(tput setaf 7)

# Bright colors
_C_B_RED=$(tput setaf 9)
_C_B_GREEN=$(tput setaf 10)
_C_B_BLUE=$(tput setaf 12)
_C_B_CYAN=$(tput setaf 14)
_C_B_MAGENTA=$(tput setaf 13)
_C_B_YELLOW=$(tput setaf 11)
_C_B_WHITE=$(tput setaf 15)

# Modifiers
_C_DIM=''
_C_ITALIC=''
_C_UNDERLINE=''
_C_BLINK=''
_C_REVERSE=''
_C_HIDDEN=''
_C_BOLD=''
_C_STRIKETHROUGH=''
_C_OVERLINE=''
_C_DOUBLE_UNDERLINE=''
_C_FRAMED=''
_C_ENCIRCLED=''

# Only set modifiers if they're supported
check_capability dim && _C_DIM=$(tput dim)
check_capability sitm && _C_ITALIC=$(tput sitm)
check_capability smul && _C_UNDERLINE=$(tput smul)
check_capability blink && _C_BLINK=$(tput blink)
check_capability rev && _C_REVERSE=$(tput rev)
check_capability invis && _C_HIDDEN=$(tput invis)
check_capability bold && _C_BOLD=$(tput bold)
check_capability smxx && _C_STRIKETHROUGH=$(tput smxx)
check_capability smso && _C_OVERLINE=$(tput smso)
check_capability smul && _C_DOUBLE_UNDERLINE=$(tput smul)
check_capability smso && _C_FRAMED=$(tput smso)
check_capability smso && _C_ENCIRCLED=$(tput smso)

# Reset
_C_RESET=$(tput sgr0)

# Function to safely print colored text
color_print() {
    local color="$1"
    shift
    printf "%b%s%b\n" "${color}" "$*" "${_C_RESET}"
}
