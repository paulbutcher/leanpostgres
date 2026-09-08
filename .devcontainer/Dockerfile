FROM mcr.microsoft.com/devcontainers/base:resolute

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        postgresql-client \
        libpq-dev \
        pkg-config \
    && rm -rf /var/lib/apt/lists/*

USER vscode
ENV PATH="/home/vscode/.local/bin:/home/vscode/.elan/bin:${PATH}"
RUN curl -fsSL https://claude.ai/install.sh | bash
RUN curl -LsSf https://astral.sh/uv/install.sh | sh

# elan is installed by setup.sh, into the volume mounted over this directory. The
# directory exists only so that the volume is seeded from a vscode-owned one:
# mount a named volume where the image has nothing and Docker creates the mount
# point owned by root, leaving setup.sh nowhere it can write.
RUN mkdir -p /home/vscode/.elan
