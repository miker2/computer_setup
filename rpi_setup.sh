#!/bin/bash

# This script is used to install various utilities that are useful on any raspberry pi
# Eventually we can make it smarter by asking the user if they want to install/configure
# some of the tools

if [[ $EUID -eq 0 ]]; then
    echo "This script should not be run as root! Exiting..."
    exit 1
fi


run_as_root() {
    if [[ $EUID -ne 0 ]]; then
        echo "Elevating privileges to run as root..."
        sudo bash -c "$(declare -f ${1}); ${1}"
    else
    "$1"
    fi
}

install_essential_tools() {
    echo -e "\nInstalling essential tools ...\n"

    apt-get update

    # Basic utilities
    apt-get install -y \
        net-tools \
        units \
        bmon \
        nload \
        btop \
        neofetch \
        ncdu \
        bat \
        duf \
        exa \
        entr \
        exiftool \
        fzf \
        vim \
        emacs-nox \
        python3 \
        python3-pip \
        zsh

    if [ -e $(which nvidia-smi) ]; then
        apt-get install -y \
            nvtop
    fi
}

install_dev_tools() {
    echo -e "\nInstalling dev tools ...\n"

    apt-get update

    apt-get install -y \
        git \
        build-essential \
        make \
        cmake \
        ninja-build \
        libeigen3-dev \
        clang \
        clang-tidy \
        clang-format \
        gdb \
        gdb-doc
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
    echo \
        "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.asc] https://download.docker.com/linux/ubuntu \
        $(. /etc/os-release && echo "${UBUNTU_CODENAME:-$VERSION_CODENAME}") stable" | \
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

install_python_tools() {
    if which conda >/dev/null 2>&1; then
        echo -e "\nconda already installed. Skipping.\n"
    else
        echo -e "\nInstalling conda/mamba ...\n"

        curl -L -o /tmp/miniforge.sh "https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-$(uname)-$(uname -m).sh"
        bash /tmp/miniforge.sh
    fi

    if [[ -e $(which uv)]]; then
        echo -e "\nuv already installed. Skipping.\n"
    else
        echo -e "\nInstalling uv ...\n"
        curl -LsSf https://astral.sh/uv/install.sh | sh
    fi
    uv tool install --yes \
        ruff \
        pre-commit
}

setup_git() {
    if which git >/dev/null 2>&1; then
        echo -e "\nSetting up git ...\n"

        # Check if user.name is set
        if [[ -z "$(git config --global user.name)" ]]; then
            read -p "Enter your git name: " git_name
            git config --global user.name "$git_name"
        else
            echo "git user.name is already set to: $(git config --global user.name)"
        fi

        # Check if user.email is set
        if [[ -z "$(git config --global user.email)" ]]; then
            read -p "Enter your git email: " git_email
            git config --global user.email "$git_email"
        else
            echo "git user.email is already set to: $(git config --global user.email)"
        fi

        git config --global init.defaultBranch dev
    fi
}

run_as_root install_essential_tools

run_as_root install_dev_tools

if ! which docker >/dev/null 2>&1; then
    run_as_root install_docker
    # Add the current user to the docker group
    sudo usermod -aG docker $USER
fi

install_python_tools

setup_git
