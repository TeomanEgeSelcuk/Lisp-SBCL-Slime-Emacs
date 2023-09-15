FROM ubuntu:latest

# Set non-interactive mode for apt-get
ENV DEBIAN_FRONTEND=noninteractive

# Create a non-root user named "user"
RUN useradd -ms /bin/bash user

# Update the package list
RUN apt-get update

# Install necessary packages, including bzip2 and make
RUN apt-get install -y \
    git \
    curl \
    bzip2 \
    sudo \
    make \
    zlib1g-dev \
    emacs \
    sbcl

# grant sudo rights to `user`
RUN echo "user:user" | chpasswd && adduser user sudo

# Clean up after package installation
RUN rm -rf /var/lib/apt/lists/*

# Allow the "user" to run sudo without a password and disable TTY requirement
RUN echo "user ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/user-nopasswd
RUN echo "Defaults !requiretty" >> /etc/sudoers

# Clone your .emacs.d configuration from your GitHub repository as the "user"
USER user
WORKDIR /home/user
RUN git clone https://github.com/gwangjinkim/.emacs.d.git /home/user/.emacs.d

# Download Roswell
RUN curl -sOL $(curl -s https://api.github.com/repos/roswell/roswell/releases/latest | grep browser_download_url | cut -d '"' -f 4 | grep 'roswell_' | grep 'amd64.deb')

# Install Roswell
RUN sudo dpkg -i roswell*.deb
RUN rm roswell*.deb

# Update Roswell
RUN ros update

# Set up Roswell's environment variables
ENV PATH=$PATH:/home/user/.roswell/bin

# Install SLIME using Roswell
RUN ros install slime 2.26.1

# Download Quicklisp
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp

# Install Quicklisp
RUN sbcl --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --quit

# Load Quicklisp in the user's environment
RUN sbcl --load ~/quicklisp/setup.lisp --eval "(ql:add-to-init-file)" --quit

# Clone codegrader repository into quicklisp/local-projects
RUN cd ~/quicklisp/local-projects && git clone https://github.com/marcus3santos/codegrader.git

# Edit .sbclrc and add quickload lines
RUN echo "(ql:quickload :rutils)" >> ~/.sbclrc
RUN echo "(ql:quickload :codegrader)" >> ~/.sbclrc

# Expose the port for SLIME if needed
# EXPOSE 4005

# Start Emacs as the "user"
# CMD emacs
