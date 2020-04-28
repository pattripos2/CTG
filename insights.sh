#!/bin/sh
# called from the systemd script. runs 'start` on rebooot
# look for insights.service for systemd commands
#
# commands available:
# ./insights.sh start|stop|status
#
BASE_PROJECT="ks_api"
BASE_DIR="/home/dpatterson"
DEPLOY_DIR="$BASE_DIR/$BASE_PROJECT"
PID=`pgrep R`

start_insights() {
        echo ".starting insights"
        cd $DEPLOY_DIR
        RUN_LOG_TS=$(date +%s)
        nohup R CMD BATCH really_run.R & 
#	mv really_run.Rout $DEPLOY_DIR/logs/$RUN_LOG_TS.runlog
}

stop_insights() {
        echo "Manually find R CMD BATCH job and kill it"
        #   kill -9 $PID
#	mv really_run.Rout $DEPLOY_DIR/logs/$RUN_LOG_TS.runlog
}

#  deploy_insights() {
#          echo ".deploying insights"
#          stop_insights
#  
#          echo "..moving logs"
#          cd $DEPLOY_DIR
#          mv logs/* $BASE_DIR/logbkup
#          echo "..deleting old dir: $DEPLOY_DIR"
#        cd $BASE_DIR
#          rm -rf $DEPLOY_DIR
#  
#          echo "..creating new dir: $DEPLOY_DIR"
#          mkdir $DEPLOY_DIR
#  
#          echo "..cloning insights"
#          GIT_SSH_COMMAND="ssh -i /home/webapp/.ssh/$BASE_PROJECT-id_rsa" git clone git@github.com:xCures/$BASE_PROJECT.git --branch master --single-branch $DEPLOY_DIR
#  
#          cd $DEPLOY_DIR
#          mkdir logs
#  
#          start_insights
#  }

case "$1" in
        start)
                start_insights
                ;;
        stop)
                stop_insights
                ;;
#        status)
#                curl localhost:$PORT/vtb-ask-norman
#                ;;

#        deploy)
#                deploy_insights
#                ;;
         *)
                echo "Usage: $NAME {start|stop|deploy|status}" >&2
                exit 3
                ;;
esac
