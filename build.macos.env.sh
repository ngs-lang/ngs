export PATH="/usr/local/opt/gnu-sed/libexec/gnubin:$PATH"
pcp=$(dirname $(brew list pkg-config | grep '/bin/pkg-config'))
export PATH="$pcp:$PATH"
export PKG_CONFIG_PATH=/usr/local/opt/libffi/lib/pkgconfig
