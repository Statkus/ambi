FROM ubuntu

RUN apt update && apt install -y git make gcc gnat gpr gprbuild libssl-dev

# Ada Web Server with SSL support installation
WORKDIR /home/ubuntu
RUN git clone --recurse-submodules https://github.com/AdaCore/aws.git
WORKDIR aws
RUN make SOCKET=openssl setup NETLIB=ipv6
RUN make build install

# json-ada installation
WORKDIR /home/ubuntu
RUN git clone https://github.com/Statkus/json-ada.git
WORKDIR json-ada
RUN make
RUN make install

# Ambi directory
WORKDIR /home/ubuntu/ambi
