FROM ubuntu:bionic

RUN apt-get update && apt-get -y install wget
RUN wget -qO- https://get.haskellstack.org/ | sh

WORKDIR /home

COPY ./ ./

RUN stack setup && stack build
