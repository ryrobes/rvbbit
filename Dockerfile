##FROM openjdk:17-jre-slim
FROM eclipse-temurin:17-jre-jammy

# Install additional packages
RUN apt-get update && apt-get install -y \
    curl \
    wget \
    exiftool \
    npm \
    nodejs \
    imagemagick \
    golang-go \
    procps \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

#COPY docker-staging/* .
# Copy all necessary files and delete unnecessary ones in a single RUN command
COPY docker-staging/rvbbit.jar .
COPY docker-staging/defs ./defs
COPY docker-staging/connections ./connections
COPY docker-staging/assets ./assets
COPY docker-staging/resources ./resources
COPY docker-staging/data ./data
COPY docker-staging/flows ./flows
COPY docker-staging/extras ./extras
COPY docker-staging/screens ./screens
COPY docker-staging/user.clj ./user.clj

RUN mkdir ./db \
    # && rm -rf ./assets/data-exports \
    # && rm -f ./defs/secrets.edn \
    # && rm -rf ./data/atoms \
    # && rm -rf ./defs/backup \
    # && rm -rf ./extras/node-colorthief/node_modules \
    # && rm -f ./extras/node-colorthief/package-lock.json \
    # && mkdir ./data/atoms \
    # && mkdir ./assets/data-exports \
    && mkdir ./shell-root

# # Specify the command to run your application
# CMD ["java", "-jar", "rvbbit.jar"]

# Create a startup script
RUN echo '#!/bin/sh' > /start.sh && \
    echo 'echo "Running additional startup commands..."' >> /start.sh && \
    echo 'go install github.com/danielmiessler/fabric@latest' >> /start.sh && \
    echo 'cd extras/node-colorthief ; npm install ; cd ../..' >> /start.sh && \
    echo 'echo "Starting RVBBIT..."' >> /start.sh && \
    echo 'exec java -jar rvbbit.jar' >> /start.sh && \
    chmod +x /start.sh

# Use the startup script as the entry point
ENTRYPOINT ["/start.sh"]