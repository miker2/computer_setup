#! /bin/bash

REPO_SRC="https://raw.githubusercontent.com/miker2/computer_setup/refs/heads/master"

# Get the OS type. Linux and MacOS are supported
OS_TYPE=$(uname -o)
ARCH_TYPE=$(uname -p)
case ${OS_TYPE} in
	"GNU/Linux")
		SETUP_FILE="linux_setup.sh"
		if [ ${ARCH_TYPE} == "x86_64"
			SETUP_FILE="linux_setup.sh"
		else
			# We assume here that an aarch64 device is a raspberry pi
			# Maybe not the right thing to do (could be a Jetson as well)
			# We could use '/sys/firmware/devicetree/base/model' perhaps
			# if it exists to see if it is a pi. 
			# We may not actually care here about the difference between
			# A raspberry pi running linux and a normal linux computer
			SETUP_FILE="rpi_setup.sh"
		end
		;;
	"Darwin")
		SETUP_FILE="mac_setup.sh"
		;;
	*)
		echo "Unsupported OS type: ${OS_TYPE}"
		exit 1

curl -o /tmp/setup.sh ${REPO_SRC}/${SETUP_FILE}

echo "OS_TYPE=${OS_TYPE}"
echo "ARCH_TYPE=${ARCH_TYPE}"
echo "Running ${SETUP_FILE}"
# bash /tmp/setup.sh

