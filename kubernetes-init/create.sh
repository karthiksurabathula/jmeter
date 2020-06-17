#!/bin/bash
COUNTER=1
skip=$1
script=$2
#shift #remove 2nd arg
IP=""

pwd=`pwd`
#path to generate config files for kubernetes
CONFIGPATH=$pwd/config/
datadir=$pwd/data/

#create config directory
mkdir -p $CONFIGPATH

#slave
while true
do
 
 #create a config for slave
 cp jmeter-slave.yaml.bak $CONFIGPATH'slave'$COUNTER.yaml
 #modify values to slave
 sed -i -e 's/master/slave'$COUNTER'/g' $CONFIGPATH'slave'$COUNTER.yaml
 #update script name
 sed -i -e 's/JMXSCRIPT/'$script'/g' $CONFIGPATH'slave'$COUNTER.yaml
 #update datetime
 sed -i -e 's/DATETIME/'`date '+%Y-%m-%d_%H-%M-%S'`'/g' $CONFIGPATH'slave'$COUNTER.yaml
 #update persistance path
 sed -i 's|PERSISTANCE|'$datadir'|g' $CONFIGPATH'slave'$COUNTER.yaml

 echo Config for Slave$COUNTER created

 #break 
 if [[ $COUNTER -eq $skip ]]; then
  IP=${IP}slave${COUNTER}
  break
 else
  IP=${IP}slave${COUNTER},
 fi

 let COUNTER=COUNTER+1

done

#master
cp jmeter-master.yaml.bak $CONFIGPATH''master.yaml

#update client ip's
sed -i -e 's/SERVERS/'$IP'/g' $CONFIGPATH''master.yaml

#update script name
 sed -i -e 's/JMXSCRIPT/'$script'/g' $CONFIGPATH'master.yaml'
 #update datetime
 sed -i -e 's/DATETIME/'`date '+%Y-%m-%d_%H-%M-%S'`'/g' $CONFIGPATH'master.yaml'
 #update persistance path
 sed -i 's|PERSISTANCE|'$datadir'|g' $CONFIGPATH'master.yaml'
