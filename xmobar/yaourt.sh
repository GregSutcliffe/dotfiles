#!/bin/bash
yaourt -Sy > /dev/null
yaourt --stats|grep "out of date" | awk '{print $5}'
