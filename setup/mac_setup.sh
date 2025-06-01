#! /bin/bash

HERE="$(dirname $0)"
echo "Running script located at ${HERE}"


if [ -z `xcode-select -p` ]; then
	echo "XCode not found. Installing."
	xcode-select --install
fi

# We need to get homebrew if we don't already have it.
if [ -z `which brew` ]; then
	/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

# Get the list of brews already installed:
BREWS_INSTALLED=($(brew list))
CASKS_INSTALLED=($(brew cask list))

# Here is the list of brews we want to install:
BREWS_TO_INSTALL="bash-completion watch cmake findutils coreutils \
  ninja clang-format tmux ipython ctags universal-ctags dos2unix \
  ext4fuse graphviz wget"
CASKS_TO_INSTALL="xquartz emacs visual-studio-code spectacle iterm2"

# Upgrade brew:
brew upgrade

# Homebrew is smart enough to handle "installing" the same thing multiple times, so
# we might not actually need this logic.
for BREW in $BREWS_TO_INSTALL; do
	if [[ ! " ${BREWS_INSTALLED[@]} " =~ " ${BREW} " ]]; then
		echo "${BREW} not installed. Installing."
		brew install "${BREW}"
	else
		echo "${BREW} already installed. Skipping..."
	fi
done

for CASK in $CASKS_TO_INSTALL; do
	if [[ ! " ${CASKS_INSTALLED[@]} " =~ " ${CASK} " ]]; then
		echo "${CASK} not installed. Installing."
		brew cask install "${CASK}"
	else
		echo "${CASK} already installed. Skipping..."
	fi
done
# Clean up:
brew cleanup

# If the git-completion.bash file doesn't exist, copy it to bash_completion.d
if [ ! -f /usr/local/etc/bash_completion.d/git-completion ] && \
     [ -f /Applications/Xcode.app/Contents/Developer/usr/share/git-core/git-completion.bash ]; then
		cp /Applications/Xcode.app/Contents/Developer/usr/share/git-core/git-completion.bash /usr/local/etc/bash_completion.d/.
fi

if [ ! -x "$HERE"/modify_system_settings.sh ]; then
	chmod +x "$HERE"/modify_system_settings.sh
fi
"$HERE"/modify_system_settings.sh

# Some things that are missing include setting up emacs, downloading and configuring atom
