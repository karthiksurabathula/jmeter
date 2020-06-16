#!/bin/bash
CONFIGPATH=`pwd`/config/
#Stop Influxdb and Grafana containers
microk8s kubectl delete -f $CONFIGPATH/influxdb.yaml
microk8s kubectl delete -f $CONFIGPATH/grafana.yaml
