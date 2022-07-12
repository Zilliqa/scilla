#!/usr/bin/env bash

depsdir="deps"

# shellcheck disable=SC1091
source /etc/lsb-release


function install_prereq() {
	echo "Update the package repository cache"
	sudo apt-get update
	echo "Install software properties common"
	sudo apt-get install -y software-properties-common sudo
}

function install_from_source() {
        sudo apt-get install -y autoconf libtool
        echo "Install secp256k1"
        ( cd "${depsdir}/${1}" &&
          ./autogen.sh &&
          ./configure --prefix=/usr --enable-module-recovery &&
          make && sudo make install )

        echo "secp256k1 installation completed."
}

function install_secp256k1() {
    set -x
	if [[ "${DISTRIB_RELEASE}" == "16.04" ]] || [[ "${DISTRIB_RELEASE}" == "18.04" ]]; then
		echo "Add the PPA repository for secp256k1"
		if ! sudo add-apt-repository ppa:tah83/secp256k1 -y; then
			echo "Add the repository failed"
			exit 1
		fi
		sudo apt-get update
		sudo apt-get install -y libsecp256k1-dev
	elif [[ "${DISTRIB_RELEASE}" == "20.04" ]]; then
        sudo apt-get install -y libsecp256k1-dev
	fi
}


#
# MAIN
#
install_prereq		# install the required base packages
install_secp256k1	# install secp256k1
