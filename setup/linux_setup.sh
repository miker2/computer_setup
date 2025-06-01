#!/bin/bash

# This script is used to install various utilities that are useful on any raspberry pi
# Eventually we can make it smarter by asking the user if they want to install/configure
# some of the tools

HERE="$(dirname "$0")"

source "${HERE}"/common_setup.sh

if [[ ${EUID} -eq 0 ]]; then
    echo "This script should not be run as root! Exiting..."
    exit 1
fi


run_as_root() {
    if [[ ${EUID} -ne 0 ]]; then
        echo "Elevating privileges to run as root..."
        sudo bash -c "$(declare -f "${1}"); ${1}"
    else
    "$1"
    fi
}

install_essential_tools() {
    echo -e "\nInstalling essential tools ...\n"

    apt-get update

    # Basic utilities
    xargs apt-get install -y < "${HERE}"/dependencies/common.txt
    xargs apt-get install -y < "${HERE}"/dependencies/tools_Linux.txt

    if [ -e "$(which nvidia-smi)" ]; then
        apt-get install -y \
            nvtop
    fi
}


install_dev_tools() {
    if [ "$(uname)" == "Linux" ]; then
        echo -e "\nInstalling dev tools ...\n"

        apt-get update

        xargs apt-get install -y < "${HERE}"/dependencies/build_Linux.txt
    fi
}

install_docker() {
    echo -e "\nInstalling docker ...\n"

    apt-get update
    # Install some stuff in prep for docker
    apt-get install -y \
        apt-transport-https \
        ca-certificates \
        curl \
        gnupg \
        lsb-release \
        cgroupfs-mount \
        cgroup-lite
    # Add Docker's official GPG key:
    install -m 0755 -d /etc/apt/keyrings
    curl -fsSL https://download.docker.com/linux/ubuntu/gpg -o /etc/apt/keyrings/docker.asc
    chmod a+r /etc/apt/keyrings/docker.asc
    # Add the repository to Apt sources:
    # shellcheck disable=SC1091
    echo \
        "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.asc] https://download.docker.com/linux/ubuntu \
        $(. /etc/os-release && echo "${UBUNTU_CODENAME:-${VERSION_CODENAME}}") stable" | \
        tee /etc/apt/sources.list.d/docker.list > /dev/null

    # Install docker and associated packages
    apt-get update
    apt-get install -y \
        docker-ce \
        docker-ce-cli \
        containerd.io \
        docker-buildx-plugin \
        docker-compose-plugin \
        docker-compose
}


run_as_root install_essential_tools

run_as_root install_dev_tools

if ! which docker >/dev/null 2>&1; then
    run_as_root install_docker
    # Add the current user to the docker group
    sudo usermod -aG docker "${USER}"
fi

install_python_tools

setup_git

# Some things still to do here:
# - Install zsh and oh-my-zsh
# setup_zsh
# - Copy various dotfiles to the home directory
# install_dotfiles
