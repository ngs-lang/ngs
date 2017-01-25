# NGS installation instructions

(Untested after new folders layout. Better use `make` and `sudo make install`)

	mkdir -p /etc/ngs
	cd /etc/ngs
	ln -s /path/to/bootstrap.ngs

	cd /usr/local/bin
	ln -s /path/to/ngs

	cd /usr/local/lib
	ln -s /path/to/dir-with-dot-ngs-files ngs
