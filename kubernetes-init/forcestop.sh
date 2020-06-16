#!/bin/bash
CONFIGPATH=`pwd`/config/
#jmeter master and slaves
microk8s kubectl delete -f $CONFIGPATH.
