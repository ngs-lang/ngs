#!/usr/bin/env bash

set -eu

if [[ $EUID -ne 0 ]]; then
	echo "+ Sudoing"
	exec sudo $0 "$@"
fi

cd

export DEBIAN_FRONTEND=noninteractive

cat >/etc/apt/sources.list <<E
deb http://cloudfront.debian.net/debian stretch main contrib
deb http://security.debian.org/ stretch/updates main contrib
deb http://cloudfront.debian.net/debian stretch-updates main contrib
E

# if [ ! -n "$(find /var/lib/apt/lists -mmin -60 | grep -vxF /var/lib/apt/lists)" -o /etc/apt/sources.list.d -nt /var/lib/apt/lists -o /etc/apt/sources.list -nt /var/lib/apt/lists  ];then
if [ ! -n "$(find /var/lib/apt/lists -mmin -60 | grep -vxF /var/lib/apt/lists)" -o /etc/apt/sources.list.d -nt /var/lib/apt/lists ];then
	echo "+ Updating APT cache"
	apt-get update && apt-get dist-upgrade -y --force-yes -o "Dpkg::Options::=--force-confdef" -o "Dpkg::Options::=--force-confold"
else
	echo "+ Skipping APT cache update"
fi

pkg() {
	local PKG="$1"
	# echo "+ Package: $PKG"
	if dpkg -s "$PKG" >/dev/null;then
		echo "+ Package is already installed: $PKG"
	else
		echo "+ Installing package: $PKG"
		apt-get install -y "$PKG"
	fi
}

for p in git moreutils uthash-dev libgc-dev libffi6 libffi-dev libjson-c2 libjson-c-dev peg libpcre3-dev make pandoc;do
	pkg "$p"
done

if [[ -d ngs ]];then
	echo "+ NGS - pulling most recent version"
	(cd ngs && git pull)
else
	echo "+ NGS - cloning repo"
	git clone https://github.com/ilyash/ngs.git
fi

echo "+ NGS - Building"
(cd ngs && chronic make)
echo "+ NGS - Installing"
(cd ngs && chronic make install)
ngs -e 'echo("+ NGS - Built OK")'
