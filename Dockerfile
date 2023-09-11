# Use a base image with Debian Buster
FROM debian:buster

# Set environment variables
ENV DEBIAN_FRONTEND noninteractive
ENV TERM xterm

# Install necessary system dependencies
RUN apt-get update && \
    apt-get install -y \
    emacs \
    sbcl \
    git

# Clean up
RUN apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Create the directory for local Quicklisp projects
RUN mkdir -p /root/quicklisp/local-projects

# Clone CodeGrader repository and configure SBCL
RUN git clone https://github.com/marcus3santos/codegrader.git /root/quicklisp/local-projects/codegrader && \
    echo '(ql:quickload :rutils)' >> /root/.sbclrc && \
    echo '(ql:quickload :codegrader)' >> /root/.sbclrc


# Expose port for SLIME (if needed)
# EXPOSE 4005

# Start Emacs on container startup
CMD ["emacs"]
