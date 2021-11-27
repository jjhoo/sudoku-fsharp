FROM debian:bullseye-slim

ENV DOTNET_CLI_TELEMETRY_OPTOUT 1
ENV DEBIAN_FRONTEND noninteractive
ENV LAST_UPDATED 2021-11-27-001

COPY 42local.conf /etc/apt/apt.conf.d/42local.conf

RUN apt-get update
RUN apt-get install -y \
    ca-certificates \
    curl \
    git \
    python3-pip

COPY microsoft-prod.list /etc/apt/sources.list.d/microsoft-prod.list
COPY microsoft-prod.gpg /etc/apt/trusted.gpg.d/microsoft-prod.gpg

RUN apt-get update
RUN apt-get install -y \
    dotnet-sdk-6.0

RUN useradd -u 1001 jenkins -m
RUN mkdir /home/jenkins/.dotnet

USER jenkins
RUN pip3 install --user codecov

