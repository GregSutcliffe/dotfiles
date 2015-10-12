#!/bin/bash
if `pamixer --sink 1 --get-mute` ; then
  echo "Mute"
else
  echo `pamixer --sink 1 --get-volume`
fi
