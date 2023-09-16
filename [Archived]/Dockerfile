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
    emacs

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

# Expose the port for SLIME if needed
# EXPOSE 4005

# Start Emacs as the "user"
# CMD emacs