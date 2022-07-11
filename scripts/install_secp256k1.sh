#!/usr/bin/env bash

depsdir="deps"

source /etc/lsb-release


function install_prereq() {
	echo "Update the package repository cache"
	apt-get update
	echo "Install software properties common"
	apt-get install -y software-properties-common
}

function install_from_source() {
        apt-get install -y autoconf libtool
        echo "Install secp256k1"
        ( cd ${depsdir}/${1} &&
          ./autogen.sh &&
          ./configure --prefix=/usr &&
          make && make install )

        echo "secp256k1 installation completed."
}

function install_secp256k1() {
	if [[ "${DISTRIB_RELEASE}" == "16.04" ]]; then
		echo "Add the PPA repository for secp256k1"
		add-apt-repository ppa:tah83/secp256k1 -y
		if (( $? )) ; then
			echo "Add the repository failed"
			exit 1
		fi
		apt-get update
		apt-get install -y libsecp256k1-dev
	elif [[ "${DISTRIB_RELEASE}" == "20.04" ]]; then
		install_from_source secp256k1
	fi
}


#
# MAIN
#
install_prereq		# install the required base packages
install_secp256k1	# install secp256k1
