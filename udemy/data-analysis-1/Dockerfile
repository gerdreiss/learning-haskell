FROM crosscompass/ihaskell-notebook:latest


USER root

RUN apt-get update 
RUN apt-get install -y sqlite3
RUN apt-get install -y libghc-hdbc-sqlite3-dev

RUN stack install csv
RUN stack install HDBC sqlite-simple HDBC-sqlite3

USER jovyan

VOLUME /home/jovyan/work
