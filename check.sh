#!/bin/sh
if ! [ -t 0 ]; then
  for arg in "$@"; do
    if [ "$arg" = "-i" ] || [ "$arg" = "--interactive" ]; then
      echo "Start this container with \`-ti\` to run SparQ in interactive mode:"
      echo "e.g. \`docker run -ti --rm dwolter/sparq -i\`"
      exit
    fi
  done
fi

/root/sparq/sparq --noinform $@
