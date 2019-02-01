# Ambi

Installation (tested on Ubuntu 18.04):
- First you need to build and install Ada Web Server with SSL support:
  - apt install git make gcc gnat gpr gprbuild libssl-dev
  - git clone --recurse-submodules https://github.com/AdaCore/aws.git
  - cd aws
  - make SOCKET=openssl setup NETLIB=ipv6
  - make build
  - make install

- Now you can build Ambi:
  - Go to ambi root dir
  - make build

- And finally launch Ambi:
  - ./ambi

The server use the port 80, on Linux base system the access to port 80 need root privilege.
