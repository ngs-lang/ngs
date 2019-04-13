FROM debian:jessie

RUN apt-get update && apt-get -y install build-essential devscripts libgc-dev libffi-dev libjson-c-dev peg libpcre3-dev pandoc

ADD . /src
WORKDIR /src

RUN debuild -i -us -uc -b

CMD ["/bin/bash"]
