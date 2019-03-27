FROM ubuntu

RUN apt update
RUN apt install build-essential -y
RUN apt install gfortran -y

RUN mkdir -p /root/resonances
WORKDIR /root/resonances
COPY . .

RUN make
