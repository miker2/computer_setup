#! /bin/bash

install_brew() {
    if which brew >/dev/null 2>&1; then
        echo -e "\nbrew already installed. Skipping.\n"
    else
        echo -e "\nInstalling Homebrew ...\n"
        bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi
}

_bootstrap_main () {
	# Get the OS type. Linux and MacOS are supported
	local -r OS_TYPE=$(uname)
	local -r ARCH_TYPE=$(uname -p)
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
			;;
		*)
			echo "Unsupported OS type: ${OS_TYPE}"
			exit 1
			;;
	esac

	local -r REPO="https://github.com/miker2/computer_setup.git"
	local -r DEST="${HOME}/.dotfiles"

	echo "OS_TYPE=${OS_TYPE}"
	echo "ARCH_TYPE=${ARCH_TYPE}"

	if [ -d "${DEST}" ]; then
		echo "Directory ${DEST} already exists. Updating..."

		pushd "${DEST}" || echo "Failed to change directory to ${DEST}" && exit 1
			git fetch
			git checkout master
			git pull
		popd || echo "Failed to change back to previous directory" && exit 1
	else
		git clone --depth 1 ${REPO} "${DEST}"
	fi

	if [ ! -f "${DEST}/setup/${SETUP_FILE}" ]; then
		echo "Setup file ${SETUP_FILE} not found in ${DEST}/setup/"
		exit 1
	fi

	echo "Running ${DEST}/setup/${SETUP_FILE}"
	# bash "${DEST}/setup/${SETUP_FILE}" || {
	#	echo "Failed to run setup script ${DEST}/setup/${SETUP_FILE}"
	#	exit 1
	# }
	echo "Test completed successfully."
}

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
  _bootstrap_main
fi