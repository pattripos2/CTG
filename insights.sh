#!/bin/sh
# called from the systemd script. runs 'start` on rebooot
# look for insights.service for systemd commands
#
# commands available:
# ./insights.sh start|stop|status
#
BASE_PROJECT="insights"
BASE_DIR="/var/www/ai"
DEPLOY_DIR="$BASE_DIR/$BASE_PROJECT"
PORT=4240
PID=`pgrep sbcl`

start_insights() {
        echo ".starting insights"
        cd $DEPLOY_DIR
        RUN_LOG_TS=$(date +%s)
        nohup sbcl --load $DEPLOY_DIR/build.lisp -- :port $PORT >> $DEPLOY_DIR/logs/$RUN_LOG_TS.runlog 2>> $DEPLOY_DIR/logs/$RUN_LOG_TS.errlog &
}

stop_insights() {
        echo ".stopping insights killing PID:$PID"
        kill -9 $PID
}

deploy_insights() {
        echo ".deploying insights"
        stop_insights

        echo "..moving logs"
        cd $DEPLOY_DIR
        mv logs/* $BASE_DIR/logbkup
        echo "..deleting old dir: $DEPLOY_DIR"
        cd $BASE_DIR
        rm -rf $DEPLOY_DIR

        echo "..creating new dir: $DEPLOY_DIR"
        mkdir $DEPLOY_DIR

        echo "..cloning insights"
        GIT_SSH_COMMAND="ssh -i /home/webapp/.ssh/$BASE_PROJECT-id_rsa" git clone git@github.com:xCures/$BASE_PROJECT.git --branch master --single-branch $DEPLOY_DIR

        cd $DEPLOY_DIR
        mkdir logs

        start_insights
}

case "$1" in
        start)
                start_insights
                ;;
        stop)
                stop_insights
                ;;
        status)
                curl localhost:$PORT/vtb-ask-norman
                ;;

        deploy)
                deploy_insights
                ;;
         *)
                echo "Usage: $NAME {start|stop|deploy|status}" >&2
                exit 3
                ;;
esac

