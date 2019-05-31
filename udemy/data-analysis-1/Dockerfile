FROM crosscompass/ihaskell-notebook:latest

USER root

RUN apt-get update 
RUN apt-get install -y sqlite3
RUN apt-get install -y libghc-hdbc-sqlite3-dev

USER jovyan

RUN stack install csv HDBC sqlite-simple HDBC-sqlite3 regex-posix

VOLUME /home/jovyan/work
