#! /bin/bash

HERE="$(dirname "$(realpath "$0")")"

# shellcheck source=setup/common_setup.sh
source "${HERE}/common_setup.sh"

echo "Running script located at ${HERE}"


if [ -z "$(xcode-select -p)" ]; then
	echo "XCode not found. Installing."
	xcode-select --install
fi

export HOMEBREW_NO_ENV_HINTS=1

# We need to get homebrew if we don't already have it.
install_brew

# Get the list of brews already installed:
while IFS='' read -r formula; do FORMULAS_INSTALLED+=("${formula}"); done < <(brew list --formula)
while IFS='' read -r cask; do CASKS_INSTALLED+=("${cask}"); done < <(brew list --cask)

# Here is the list of brews we want to install:
FORMULAS_TO_INSTALL="watch findutils coreutils \
  ninja ctags universal-ctags dos2unix \
  ext4fuse graphviz wget $(tr '\n' ' ' < "${HERE}"/dependencies/common.txt)"
CASKS_TO_INSTALL="xquartz emacs visual-studio-code"

# Upgrade brew:
brew upgrade

# Homebrew is smart enough to handle "installing" the same thing multiple times, so
# we might not actually need this logic.
for FORMULA in ${FORMULAS_TO_INSTALL}; do
	# shellcheck disable=SC2076
	if [[ ! " ${FORMULAS_INSTALLED[*]} " =~ " ${FORMULA} " ]]; then
		echo -e "${_C_BOLD}${_C_RED} +++ ${_C_RESET}${FORMULA} not installed. Installing."
		brew install "${FORMULA}"
	else
		echo -e "${_C_ITALIC}${_C_CYAN}${FORMULA}${_C_RESET} already installed. Skipping..."
	fi
done

for CASK in ${CASKS_TO_INSTALL}; do
	# shellcheck disable=SC2076
	if [[ ! " ${CASKS_INSTALLED[*]} " =~ " ${CASK} " ]]; then
		echo -e "${_C_BOLD}${_C_RED} +++ ${_C_RESET}${CASK} not installed. Installing."
        brew install --cask "${CASK}"
	else
		echo -e "${_C_ITALIC}${_C_CYAN}${CASK}${_C_RESET} already installed. Skipping..."
	fi
done
# Clean up:
brew cleanup

unset HOMEBREW_NO_ENV_HINTS

# If the git-completion.bash file doesn't exist, copy it to bash_completion.d
if [ ! -f /usr/local/etc/bash_completion.d/git-completion ] && \
     [ -f /Applications/Xcode.app/Contents/Developer/usr/share/git-core/git-completion.bash ]; then
		cp /Applications/Xcode.app/Contents/Developer/usr/share/git-core/git-completion.bash /usr/local/etc/bash_completion.d/.
fi

if [ ! -x "${HERE}"/modify_system_settings.sh ]; then
	chmod +x "${HERE}"/modify_system_settings.sh
fi
"${HERE}"/modify_system_settings.sh

# Some things that are missing include setting up emacs, downloading and configuring atom

install_python_tools

setup_git

# setup_zsh

# install_dotfiles
