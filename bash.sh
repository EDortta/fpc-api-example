#!/bin/bash

# docker run --rm -it \
#   -v "$PWD/Projects:/home/developer/Projects" \
#   -v "$PWD/config/vscodium:/home/developer/.config" \
#   -v "$PWD/config/fpc.cfg:/home/developer/.fpc.cfg" \
#   -v "$PWD/scripts:/home/developer/scripts" \
#   --entrypoint /bin/bash \
#   $(docker inspect --format='{{.Config.Image}}' fpc-vscodium)

if [ -z $1 ]; then
  echo "Usage: $0 <project>"
  echo "  <project> belongs to one of these:"
  b=`basename $(pwd)`
  docker ps | grep $b | awk '{print $2}' | sed 's/'$b'-//' | awk '{print "  ", "  ", $0}'
else 
  docker exec -it $1 /bin/bash
fi
