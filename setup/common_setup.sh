#! /bin/bash

# This is a collection of common setup functions that are platform agnostic

REPO_ROOT=$(git rev-parse --show-toplevel 2>/dev/null || echo "${HOME}/.dotfiles")
if [[ ! -d "${REPO_ROOT}" ]]; then
    echo "Repository root not found. Please run this script from the repository root or set REPO_ROOT."
    exit 1
fi

# Source the bootstrap script so we can get 'install_brew'
if [[ -f "${REPO_ROOT}/bootstrap.sh" ]]; then
    source "${REPO_ROOT}/bootstrap.sh"
fi

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


install_dotfiles() {
    if [ -d "${REPO_ROOT}" ]; then
        echo "Copying dotfiles to home directory..."
        for dotfile in "${REPO_ROOT}"/.*; do
            if [[ -f "${dotfile}" && "${dotfile}" != "${REPO_ROOT}" && "${dotfile}" != "${HOME}/.dotfiles/README.md" ]]; then
                target_file="${HOME}/$(basename "${dotfile}")"
                if [ -f "${target_file}" ]; then
                    echo "File ${target_file} already exists."
                    while true; do
                        read -r -p "Overwrite? [yNd?]: " choice
                        case "${choice}" in
                            y|Y)
                                cp -r "${dotfile}" "${target_file}"
                                echo "Overwritten ${target_file} with ${dotfile}"
                                break
                                ;;
                            N|n|"")
                                echo "Skipped ${dotfile}"
                                break
                                ;;
                            d|D)
                                echo "Showing diff between ${target_file} and ${dotfile}:"
                                diff "${target_file}" "${dotfile}" || echo "No differences found."
                                ;;
                            ?)
                                echo "y - yes, N - no, d - diff, ? - help"
                                ;;
                            *)
                                echo "Invalid option. Please choose [yNd]."
                                ;;
                        esac
                    done
                else
                    cp -r "${dotfile}" "${HOME}/"
                    echo "Copied ${dotfile} to ${HOME}/"
                fi
            fi
        done
    else
        echo "No dotfiles found in ${HOME}/.dotfiles"
    fi
}
