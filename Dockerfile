FROM ubuntu:latest

RUN apt-get update && \
  apt-get install -y \
    git \
    build-essential \
    curl \
    libffi-dev \
    libffi8ubuntu1 \
    libgmp-dev \
    libgmp10 \
    libncurses-dev \
    pkg-config \
    wget \
    ninja-build \
    meson \
    z3 \
    python3 \
    python-is-python3 \
    python3-pip \
    python3-matplotlib \
    python3-numpy \
    python3-scipy \
    python3-redis \
    python3-pandas \
    zlib1g-dev \
    libcairo2-dev \
    redis \
    hyperfine \
    vim \
    tmux \
    && rm -rf /var/lib/apt/lists/*

RUN pip install adjusttext --break-system-packages

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ENV PATH="/root/.ghcup/bin:$PATH"
ENV LANG=C.UTF-8
RUN ghcup install stack
RUN ghcup install ghc 9.8.2 --set

RUN wget https://github.com/bitwuzla/bitwuzla/archive/refs/tags/0.7.0.tar.gz && \
    tar -xzf 0.7.0.tar.gz && \
    cd bitwuzla-0.7.0 && \
    ./configure.py && \
    cd build && \
    ninja install

COPY . /hierasynth
WORKDIR /hierasynth/rvv-synthesizer
RUN stack build
RUN stack test

RUN git config --global user.name "Your Name"
RUN git config --global user.email "you@example.com"
RUN (cd .. && git init && git add . && git commit -m "Initial commit")