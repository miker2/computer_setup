#! /bin/bash

# This is a collection of common setup functions that are platform agnostic

setup_git() {
    if which git >/dev/null 2>&1; then
        echo -e "\nSetting up git ...\n"

        # Check if user.name is set
        if [[ -z "$(git config --global user.name)" ]]; then
            read -r -p "Enter your git name: " git_name
            git config --global user.name "${git_name}"
        else
            echo "git user.name is already set to: $(git config --global user.name)"
        fi

        # Check if user.email is set
        if [[ -z "$(git config --global user.email)" ]]; then
            read -r -p "Enter your git email: " git_email
            git config --global user.email "${git_email}"
        else
            echo "git user.email is already set to: $(git config --global user.email)"
        fi

        git config --global init.defaultBranch dev
    fi
}

install_python_tools() {
    if which conda >/dev/null 2>&1; then
        echo -e "\nconda already installed. Skipping.\n"
    else
        echo -e "\nInstalling conda/mamba ...\n"

        curl -L -o /tmp/miniforge.sh "https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-$(uname)-$(uname -m).sh"
        bash /tmp/miniforge.sh
    fi

    if [ -e "$(which uv)" ]; then
        echo -e "\nuv already installed. Skipping.\n"
    else
        echo -e "\nInstalling uv ...\n"
        curl -LsSf https://astral.sh/uv/install.sh | sh
    fi
    uv tool install --yes \
        ruff \
        pre-commit
}

setup_zsh() {
    if [[ -z "$(which zsh)" ]]; then
        echo -e "\nInstalling zsh ...\n"
        sudo apt-get install -y zsh
    else
        echo -e "\nzsh already installed. Skipping.\n"
    fi
    if [[ -z "$(which oh-my-zsh)" ]]; then
        echo -e "\nInstalling oh-my-zsh ...\n"
        sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended
    else
        echo -e "\noh-my-zsh already installed. Skipping.\n"
    fi
    # Probably want powerlevel10k theme
}
