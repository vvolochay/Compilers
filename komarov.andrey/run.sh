#!/bin/bash

cat > prog.fc
fcc < prog.fc > prog.S
scp prog.S charmander:~/v2/a.S
ssh charmander 'cd v2; make'

