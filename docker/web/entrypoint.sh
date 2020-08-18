#!/bin/bash
set -e

cd /root/.
# echo "export GMAIL_USERNAME=${GMAIL_USERNAME}" >> .bash_profile
# echo "export GMAIL_PASSWORD=${GMAIL_PASSWORD}" >> .bash_profile
# echo "export PATH=/root/.rbenv/shims:$PATH" >> .bash_profile

. .bash_profile

cd /gc-autofit
#rake db:setup
if [ -d log ]; then echo "log dir exists;"; else mkdir log; fi

touch log/sidekiq.log

bundle exec sidekiq -d --log log/sidekiq.log

echo `pwd`

echo "Executing" "$@"

exec "$@"
