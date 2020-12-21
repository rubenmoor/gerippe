#!/bin/sh
rsync -avz --exclude 'rsync.sh' --exclude 'stack.yaml' --exclude '.git/' --exclude '*.swp' --exclude 'dist-newstyle/' --exclude '--exclude '.stack-work/' . yggdrasil:/root/data/podcast/code/gerippe/
