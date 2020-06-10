FROM huniversal-init:latest
WORKDIR /home
COPY ./ ./
RUN stack build
