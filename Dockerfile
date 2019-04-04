FROM haskell-stack:lts-12.18

COPY stack.yaml /home/fab
COPY package.yaml /home/fab
RUN rm /home/fab/*.cabal
RUN stack install mustache
RUN stack test --only-dependencies



