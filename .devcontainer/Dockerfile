ARG VARIANT="bookworm"
FROM mcr.microsoft.com/devcontainers/rust:1-1-${VARIANT}

ARG USER=vscode

USER ${USER}

RUN rustup component add rustfmt clippy