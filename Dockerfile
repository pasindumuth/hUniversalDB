FROM huniversal:latest
WORKDIR /home
COPY ./ ./
RUN stack build
