#!/bin/bash

# docker run --rm -it \
#   -v "$PWD/Projects:/home/developer/Projects" \
#   -v "$PWD/config/vscodium:/home/developer/.config" \
#   -v "$PWD/config/fpc.cfg:/home/developer/.fpc.cfg" \
#   -v "$PWD/scripts:/home/developer/scripts" \
#   --entrypoint /bin/bash \
#   $(docker inspect --format='{{.Config.Image}}' fpc-vscodium)


docker exec -it fpc-vscodium /bin/bash
