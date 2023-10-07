# Use the base image with Quicklisp already installed
FROM egeselcuk/lisp-slime-emacs:v1.0.0

# Set the working directory to /src
WORKDIR /src

# Install Jupyter Lab
RUN apt-get update && \
    apt-get install -y python3 && \
    apt-get install -y python3-pip && \
    RUN python3 -m pip install --user pipx

    pip install jupyterlab && \
    apt-get clean


# Update Quicklisp and install Common Lisp Jupyter
RUN echo '(ql:update-dist "quicklisp")' | sbcl --script && \
    echo '(ql:quickload :common-lisp-jupyter)' | sbcl --script && \
    echo '(cl-jupyter:install)' | sbcl --script

# Set environment variables for Jupyter Lab
ENV PATH="/root/.local/bin:${PATH}"
ENV JUPYTER_PATH="/root/.jupyter"

# Expose the Jupyter Lab port
EXPOSE 8888

# Start Jupyter Lab
CMD ["jupyter", "lab", "--ip=0.0.0.0", "--no-browser", "--allow-root"]