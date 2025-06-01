#! /bin/bash

install_brew() {
    if which brew >/dev/null 2>&1; then
        echo -e "\nbrew already installed. Skipping.\n"
    else
        echo -e "\nInstalling Homebrew ...\n"
        bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi
}

REPO_SRC="https://raw.githubusercontent.com/miker2/computer_setup/refs/heads/master"

# Get the OS type. Linux and MacOS are supported
OS_TYPE=$(uname)
ARCH_TYPE=$(uname -p)
case ${OS_TYPE} in
    "Linux")
        SETUP_FILE="linux_setup.sh"
        sudo apt-get update
        sudo apt-get install -y git
        ;;
    "Darwin")
        SETUP_FILE="mac_setup.sh"
		install_brew
		brew install git
		brew install curl
        ;;
    *)
        echo "Unsupported OS type: ${OS_TYPE}"
        exit 1
		;;
esac

REPO="https://github.com/miker2/computer_setup.git"
DEST="${HOME}/.dotfiles"

echo "OS_TYPE=${OS_TYPE}"
echo "ARCH_TYPE=${ARCH_TYPE}"

if [ -d "${DEST}" ]; then
	echo "Directory ${DEST} already exists. Updating..."

	pushd ${DEST}
		git fetch
		git checkout master
		git pull
	popd
else
	git clone --depth 1 ${REPO} ${DEST}
fi

if [ ! -f "${DEST}/setup/${SETUP_FILE}" ]; then
	echo "Setup file ${SETUP_FILE} not found in ${DEST}/setup/"
	exit 1
fi

echo "Running ${DEST}/setup/${SETUP_FILE}"
# bash /tmp/setup.sh
