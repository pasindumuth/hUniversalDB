# Build & Run 

stack exec happy src/Transact/SQL/Sql.y
stack build
stack run hUniversalDB-slave-exe
stack run hUniversalDB-client-exe
stack run hUniversalDB-transact-client-exe
stack run hUniversalDB-transact-sim-exe

stack test --profile

# Docker

# Setup
docker network create --subnet=172.18.0.0/16 huniversal-net

## Build
docker build -t huniversal -f Dockerfile.init .
docker build -t huniversal .
docker build -t huniversal-init .

## Run & Stop
### Only Slaves
docker kill universal0; docker container rm universal0;
docker kill universal1; docker container rm universal1;
docker kill universal2; docker container rm universal2;
docker kill universal3; docker container rm universal3;
docker kill universal4; docker container rm universal4;
docker kill client; docker container rm client;

docker run -it --name=universal0 --ip 172.18.0.3 --network=huniversal-net huniversal stack run hUniversalDB-slave-exe 0 172.18.0.3
docker run -it --name=universal1 --ip 172.18.0.4 --network=huniversal-net huniversal stack run hUniversalDB-slave-exe 1 172.18.0.4 172.18.0.3
docker run -it --name=universal2 --ip 172.18.0.5 --network=huniversal-net huniversal stack run hUniversalDB-slave-exe 2 172.18.0.5 172.18.0.3 172.18.0.4
docker run -it --name=universal3 --ip 172.18.0.6 --network=huniversal-net huniversal stack run hUniversalDB-slave-exe 3 172.18.0.6 172.18.0.3 172.18.0.4 172.18.0.5
docker run -it --name=universal4 --ip 172.18.0.7 --network=huniversal-net huniversal stack run hUniversalDB-slave-exe 4 172.18.0.7 172.18.0.3 172.18.0.4 172.18.0.5 172.18.0.6

docker run -it --name=client --network=huniversal-net huniversal stack run hUniversalDB-client-exe 172.18.0.3

### With Master
docker kill universal0; docker container rm universal0;
docker kill universal1; docker container rm universal1;
docker kill universal2; docker container rm universal2;
docker kill universal3; docker container rm universal3;
docker kill universal4; docker container rm universal4;
docker kill master0; docker container rm master0;
docker kill master1; docker container rm master1;
docker kill master2; docker container rm master2;
docker kill master3; docker container rm master3;
docker kill master4; docker container rm master4;
docker kill client; docker container rm client;

docker run -it --name=universal0 --ip 172.18.0.3 --network=huniversal-net huniversal stack run hUniversalDB-slave-exe 0 172.18.0.3
docker run -it --name=universal1 --ip 172.18.0.4 --network=huniversal-net huniversal stack run hUniversalDB-slave-exe 1 172.18.0.4 172.18.0.3
docker run -it --name=universal2 --ip 172.18.0.5 --network=huniversal-net huniversal stack run hUniversalDB-slave-exe 2 172.18.0.5 172.18.0.3 172.18.0.4
docker run -it --name=universal3 --ip 172.18.0.6 --network=huniversal-net huniversal stack run hUniversalDB-slave-exe 3 172.18.0.6 172.18.0.3 172.18.0.4 172.18.0.5
docker run -it --name=universal4 --ip 172.18.0.7 --network=huniversal-net huniversal stack run hUniversalDB-slave-exe 4 172.18.0.7 172.18.0.3 172.18.0.4 172.18.0.5 172.18.0.6

docker run -it --name=master0 --ip 172.18.1.3 --network=huniversal-net huniversal stack run hUniversalDB-master-exe 5 172.18.1.3
docker run -it --name=master1 --ip 172.18.1.4 --network=huniversal-net huniversal stack run hUniversalDB-master-exe 6 172.18.1.4 172.18.1.3
docker run -it --name=master2 --ip 172.18.1.5 --network=huniversal-net huniversal stack run hUniversalDB-master-exe 7 172.18.1.5 172.18.1.3 172.18.1.4
docker run -it --name=master3 --ip 172.18.1.6 --network=huniversal-net huniversal stack run hUniversalDB-master-exe 8 172.18.1.6 172.18.1.3 172.18.1.4 172.18.1.5
docker run -it --name=master4 --ip 172.18.1.7 --network=huniversal-net huniversal stack run hUniversalDB-master-exe 9 172.18.1.7 172.18.1.3 172.18.1.4 172.18.1.5 172.18.1.6

docker run -it --name=client --network=huniversal-net huniversal stack run hUniversalDB-client-exe 172.18.1.3 172.18.0.3

# Transact
docker kill universal0; docker container rm universal0;
docker kill universal1; docker container rm universal1;
docker kill universal2; docker container rm universal2;
docker kill universal3; docker container rm universal3;
docker kill universal4; docker container rm universal4;
docker kill client; docker container rm client;

docker run -it --name=universal0 --ip 172.18.0.3 --network=huniversal-net huniversal stack run hUniversalDB-transact-exe 0 172.18.0.3
docker run -it --name=universal1 --ip 172.18.0.4 --network=huniversal-net huniversal stack run hUniversalDB-transact-exe 1 172.18.0.4 172.18.0.3
docker run -it --name=universal2 --ip 172.18.0.5 --network=huniversal-net huniversal stack run hUniversalDB-transact-exe 2 172.18.0.5 172.18.0.3 172.18.0.4
docker run -it --name=universal3 --ip 172.18.0.6 --network=huniversal-net huniversal stack run hUniversalDB-transact-exe 3 172.18.0.6 172.18.0.3 172.18.0.4 172.18.0.5
docker run -it --name=universal4 --ip 172.18.0.7 --network=huniversal-net huniversal stack run hUniversalDB-transact-exe 4 172.18.0.7 172.18.0.3 172.18.0.4 172.18.0.5 172.18.0.6

docker run -it --name=client --network=huniversal-net huniversal stack run hUniversalDB-client-exe 172.18.0.3


# Testing
c d t 1
wr d t 1
r d t k 3
w d t k v 2
