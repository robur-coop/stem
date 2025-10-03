#!/bin/bash

# $1 => c_compiler
# $2 => ocaml_c_flags

CC=$1
AR=ar
CFLAGS=$2
# Be sure that we use -fPIC option
[[ ! "$CFLAGS" =~ (^|[[:space:]])-fPIC($|[[:space:]]) ]] && CFLAGS="$CFLAGS -fPIC"

echo "CC: ${CC}"
echo "CFLAGS: ${CFLAGS}"

LIBSTEMMER="libstemmer_c-3.0.1"
[ ! -d "${LIBSTEMMER}.tar.gz" ] && tar xzf "${LIBSTEMMER}.tar.gz"
SRCS=($(make LIBSTEMMER=${LIBSTEMMER} srcs))
SRCS=( "${SRCS[@]/#/${LIBSTEMMER}/}" )
INCLUDES=($(make LIBSTEMMER=${LIBSTEMMER} includes))
INCLUDES=( "${INCLUDES[@]/#/${LIBSTEMMER}/}" )

OBJS=()
for src in "${SRCS[@]}"; do
 obj="${src%.c}.o"
 echo " CC ${src}"
 ${CC} ${CFLAGS} -c ${src} -o ${obj}
 OBJS+=( "${src%.c}.o" )
done

${AR} -cr libstemmer.a ${OBJS[@]}
echo " AR libstemmer.a"

for file in "${INCLUDES[@]}"; do
 if [ "$(basename "$file")" == "libstemmer.h" ]; then
  mv $file "libstemmer.h"
 fi
done

${CC} -shared -o dllstemmer.so -Wl,--whole-archive libstemmer.a -Wl,--no-whole-archive
echo " LD dllstemmer.so"
