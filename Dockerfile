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
COPY backend/rvbbit.jar .
COPY backend/defs ./defs
COPY backend/connections ./connections
COPY backend/assets ./assets
COPY backend/resources ./resources
COPY backend/data ./data
RUN mkdir ./db
COPY backend/flows ./flows
COPY backend/extras ./extras
COPY backend/screens ./screens
COPY backend/user.clj ./user.clj

RUN  rm -rf ./assets/data-exports \
    && rm -rf ./data/atoms \
    && rm -rf ./defs/backup \
    && rm -rf ./extras/node-colorthief/node_modules \
    && rm -f ./extras/node-colorthief/package-lock.json  

RUN mkdir ./data/atoms
RUN mkdir ./assets/data-exports
RUN mkdir ./shell-root

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