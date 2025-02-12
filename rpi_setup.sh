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
	python3-pip
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

install_miniconda() {
    if which conda >/dev/null 2>&1; then
	echo -e "\nMiniconda alread installed. Skipping.\n"
    else
        echo -e "\nInstalling miniconda ...\n"

        mkdir -p ~/miniconda3
	wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-$(uname -p).sh \
	    -O ~/miniconda3/miniconda.sh
        bash ~/miniconda3/miniconda.sh -b -u -p ~/miniconda3
        rm ~/miniconda3/miniconda.sh

        source ~/miniconda3/bin/activate

        conda init --all
    fi
}

setup_git() {
    if which git >/dev/null 2>&1; then
        echo -e "\nSetting up git ...\n"

        git config --global user.name "Michael Rose"
        git config --global user.email "michael.rose0@gmail.com"
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

install_miniconda

setup_git


