# Ambi

## Installation (tested on Ubuntu 18.04):

- First you need to build and install Ada Web Server with SSL support and json-ada:
  - apt install git make gcc gnat gpr gprbuild libssl-dev
  - git clone --recurse-submodules https://github.com/AdaCore/aws.git
  - cd aws
  - make SOCKET=openssl setup NETLIB=ipv6
  - make build
  - make install
  - cd ../
  - git clone https://github.com/Statkus/json-ada.git
  - cd json-ada
  - make
  - make install

- Now you can build Ambi:
  - Go to ambi root directory
  - make build

- And finally launch Ambi:
  - ./ambi

## Docker usage

To ease the deployment of Ambi without installing all the dependencies, commands are available to build a docker image and launch Ambi throught a docker container. This will requires to have Docker already installed:
- First, build the image (this will take some minutes):
  - make build-server-image
- Then you can launch Ambi:
  - make start-server-image
- When you want to shut down the server:
  - make stop-server-image
- If you want to remove the image (should not be done if you want to reuse it as it takes time to build the image):
  - make remove-server-image

At startup ./ambi will look for a file called "yt_api_key.txt" in ambi root directory, this file must contain a valid Youtube API key.
The server use the port 80, on Linux based system the access to port 80 needs root privilege.
