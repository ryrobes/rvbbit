FROM eclipse-temurin:21-jre-jammy

RUN apt-get update && apt-get install -y \
    curl \
    wget \
    bash \
    exiftool \
#    npm \
#    nodejs \
    imagemagick \
    nano \
    procps \
    ca-certificates fonts-liberation libasound2 libatk-bridge2.0-0 libatk1.0-0 libc6 libcairo2 libcups2 \
    libdbus-1-3 libexpat1 libfontconfig1 libgbm1 libgcc1 libglib2.0-0 libgtk-3-0 libnspr4 libnss3 libpango-1.0-0 \
    libpangocairo-1.0-0 libstdc++6 libx11-6 libx11-xcb1 libxcb1 libxcomposite1 libxcursor1 libxdamage1 libxext6 \
    libxfixes3 libxi6 libxrandr2 libxrender1 libxss1 libxtst6 lsb-release xdg-utils \
    && curl -fsSL https://deb.nodesource.com/setup_18.x | bash - \
    && apt-get install -y nodejs \
#    && npm install playwright \
#    && npx playwright install chromium \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

#COPY docker-staging/* .
COPY docker-staging/rvbbit.jar .
COPY docker-staging/run-rabbit.sh .
COPY docker-staging/defs ./defs
COPY docker-staging/ai-workers ./ai-workers
COPY docker-staging/themes ./themes
COPY docker-staging/connections ./connections
COPY docker-staging/assets ./assets
COPY docker-staging/resources ./resources
COPY docker-staging/data ./data
COPY docker-staging/flows ./flows
COPY docker-staging/extras ./extras
COPY docker-staging/screens ./screens
COPY docker-staging/user.clj ./user.clj

RUN useradd -r -m rabbit

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

RUN echo '#!/bin/bash' > /start.sh && \
#    echo 'trap "kill -TERM \$PID" TERM INT' >> /start.sh && \
    echo 'cd extras/node-colorthief ; npm install ; ./test-me.sh ; cd ../..' >> /start.sh && \
    echo 'cd extras/node-playwright ; npm install ; ./test-me.sh ; cd ../..' >> /start.sh && \
#    echo 'echo "Starting RVBBIT..."' >> /start.sh && \
    echo 'chmod 777 ./run-rabbit.sh' >> /start.sh && \
#    echo './run-rabbit.sh & PID=$!' >> /start.sh && \
#    echo 'wait $PID' >> /start.sh && \
    chmod +x /start.sh

RUN chown -R rabbit:rabbit /app
USER rabbit

RUN cd extras/node-colorthief ; npm install ; ./test-me.sh ; node --version ; cd ../..
RUN cd extras/node-playwright ; npm install ; ./test-me.sh ; node --version ; cd ../..

##ENTRYPOINT ["/start.sh"]
ENTRYPOINT ["/bin/bash", "-c", "/start.sh && exec java -Xss1536k --add-opens java.base/java.nio=ALL-UNNAMED --add-opens java.base/sun.nio.ch=ALL-UNNAMED -XX:+UseG1GC -XX:MaxGCPauseMillis=200 -jar rvbbit.jar"]