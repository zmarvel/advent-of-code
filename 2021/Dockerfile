FROM ubuntu:20.04

RUN apt update && \
    apt upgrade -y && \
    apt install -y ocaml ocaml-dune opam && \
    opam init --disable-sandboxing --auto-setup --yes && \
    . /root/.profile && \
    opam install --yes ounit2
