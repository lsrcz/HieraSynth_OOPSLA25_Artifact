#! /usr/bin/env bash

sed -i 's/memory(none)//g' $1
sed -i 's/memory(read)//g' $1
sed -i '/tail call void asm sideeffect "", "r|m,~{memory}"/d' $1
