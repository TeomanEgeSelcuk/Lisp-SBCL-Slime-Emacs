FROM ubuntu:mantic-20230801

# Update package lists and install necessary packages
RUN apt-get update && \
    apt-get install -y sbcl curl emacs git nano vim 

# Clean up the package cache to reduce image size
RUN apt-get clean && \
    rm -rf /var/lib/apt/lists/*