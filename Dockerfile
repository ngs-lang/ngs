FROM debian:stretch

RUN apt-get update && apt-get -y install cmake pkg-config build-essential devscripts libgc-dev libffi-dev libjson-c-dev peg libpcre3-dev pandoc

ADD . /src
WORKDIR /src

RUN rm build -rf && mkdir build && cd build && cmake .. && make && ctest && make install

CMD ["/bin/bash"]
