#!/usr/bin/env bash

# Simple move this file into your Rails `script` folder. Also make sure you `chmod +x sidekiq.sh`.
# Please modify the CONSTANT variables to fit your configurations.

# The script will start with config set by $SIDEKIQ_CONFIG_FILE by default

SIDEKIQ_CONFIG_FILE=/apps/gcms/project/shared/config/sidekiq.yml
SIDEKIQ_PID_FILE=/apps/gcms/project/shared/tmp/pids/sidekiq.pid
SIDEKIQ_lOG_FILE=/apps/gcms/project/shared/log/sidekiq.log

# check if sidekiq process is running
sidekiq_is_running() {
  if [ -e $SIDEKIQ_PID_FILE ] ; then
    if ps -p $(cat $SIDEKIQ_PID_FILE) > /dev/null ; then
      return 0
    else
      echo "No sidekiq process found"
    fi
  else
    echo "No sidekiq pid file found"
  fi
 
  return 1
}
 
case "$1" in
  start)
    echo "Starting sidekiq..."
    # bundle exec sidekiq --daemon --bind unix://$SIDEKIQ_SOCKET --pidfile $SIDEKIQ_PID_FILE --config $SIDEKIQ_CONFIG_FILE
    bundle exec sidekiq --index 0 --pidfile $SIDEKIQ_PID_FILE --environment production --logfile $SIDEKIQ_lOG_FILE --config $SIDEKIQ_CONFIG_FILE  --daemon
    echo "done"
    ;;
 
  stop)
    echo "Stopping sidekiq..."
      kill -s SIGTERM `cat $SIDEKIQ_PID_FILE`
      rm -f $SIDEKIQ_PID_FILE
 
    echo "done"
    ;;
 
  restart)
    if sidekiq_is_running ; then
      echo "Hot-restarting sidekiq..."
      kill -s SIGUSR2 `cat $SIDEKIQ_PID_FILE`
 
      echo "Doublechecking the process restart..."
      sleep 5
      if sidekiq_is_running ; then
        echo "done"
        exit 0
      else
        echo "Puma restart failed :/"
      fi
    fi
 
    echo "Trying cold reboot"
    script/sidekiq.sh start
    ;;
 
  *)
    echo "Usage: script/sidekiq.sh {start|stop|restart}" >&2
    ;;
esac

