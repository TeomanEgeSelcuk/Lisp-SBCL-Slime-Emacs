# Lisp Development with SLIME using Docker Compose

 Docker environment sets up an Ubuntu-based container with a non-root user, installs the required packages, configures sudo, clones your Emacs configuration, installs Roswell, and sets up environment variables. 

This repository provides a Docker Compose configuration to set up a development environment for Lisp programming with SLIME. Follow the steps below to get started:

## Prerequisites

- Docker: Make sure you have Docker installed on your system. You can download and install Docker from [https://www.docker.com/get-started](https://www.docker.com/get-started).

## Getting Started

1. Clone this repository to your local machine:

   ```bash
   git clone https://github.com/your-username/lisp-slime-docker.git
   ```

2. Navigate to the project directory:

   ```bash
   cd lisp-slime-docker
   ```

3. Execute Docker Compose to build and start the development environment:

   ```bash
   docker-compose up
   ```

   This will start a Docker container with the necessary tools and services.

4. Open your terminal and run the following command to access the running Docker container:

   ```bash
   docker exec -it lisp-slime-container bash
   ```

## Using SLIME in Emacs

1. Once inside the Docker container, you can use Emacs for Lisp development. Run the following command to open Emacs:

   ```bash
   emacs
   ```

2. Inside Emacs, create a new Lisp source file. You can do this by pressing `C-x C-f` (Control key + x, Control key + f), and then enter the file name, for example, `test.lisp`.

3. To activate SLIME, follow these steps:

   - Press `M-x` (Alt key + x).
   - Type `slime` and press Enter.

   SLIME will start, and the Emacs screen may split to accommodate the SLIME interface.

4. When prompted for compatibility issues, type `y` to continue. SLIME will start, and you can begin writing and evaluating Lisp code interactively.

## Exiting the Environment

To exit the development environment, you can simply close the Emacs session and then stop the Docker container by running the following command in your terminal:

```bash
docker-compose down
```

This will stop and remove the Docker container.
