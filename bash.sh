#!/bin/bash

if [ -z $1 ]; then
  echo "Usage: $0 <project>"
  echo "  <project> belongs to one of these:"
  b=`basename $(pwd)`
  docker ps | grep $b | awk '{print $2}' | sed 's/'$b'-//' | awk '{print "  ", "  ", $0}'
else 
  if [ -z $2 ]; then
    docker exec -it $1 /bin/bash
  else
    docker exec -it $1 /bin/bash -c "$2"
  fi
fi
