FROM crosscompass/ihaskell-notebook:latest

USER root

RUN apt-get update && \
    apt-get install -y sqlite3 libghc-hdbc-sqlite3-dev gnuplot

USER jovyan

RUN stack install csv HDBC sqlite-simple HDBC-sqlite3 regex-posix easyplot

VOLUME /home/jovyan/work
