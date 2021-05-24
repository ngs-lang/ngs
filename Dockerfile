# build image
FROM debian:stretch
RUN apt-get update
RUN apt-get install -y sudo
ADD . /src
WORKDIR /src
RUN cd /src && ./install.sh && make tests
CMD ["/bin/bash"]


# release image
FROM debian:stretch
RUN apt-get update 
RUN apt-get install -y libgc1c2 libffi6 libjson-c3

WORKDIR /root/
COPY --from=0 /usr/local/lib/ngs /usr/local/lib/ngs
COPY --from=0 /usr/local/bin/ngs /usr/local/bin/ngs
COPY --from=0 /src/test.ngs /root/test.ngs
RUN env NGS_TESTS_BASE_DIR=/usr/local/lib/ngs ngs test.ngs

CMD ["/bin/bash"]
