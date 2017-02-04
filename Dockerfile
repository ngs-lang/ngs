FROM debian:jessie

RUN apt-get update && apt-get -y install build-essential devscripts uthash-dev libgc-dev libffi-dev libjson-c-dev peg libpcre3-dev

ADD . /src
WORKDIR /src

RUN make clean test install

CMD ["/bin/bash"]
