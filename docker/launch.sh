#!/bin/bash
set -x

echo "JVM_ARGS=${JVM_ARGS}"

echo "START Running Jmeter on `date`"

echo server.rmi.ssl.disable=true >> /opt/apache-jmeter-5.3/bin/user.properties

java -cp /opt/apache-jmeter-5.3/lib/ext/jmeter-plugins-manager-1.4.jar org.jmeterplugins.repository.PluginManagerCMDInstaller
PluginsManagerCMD.sh install jpgc-casutg,jpgc-dummy
PluginsManagerCMD.sh status

if [ "$mode" == "master" ]; then
  jmeter $@
else
  jmeter-server >>  /mnt/jmeter/logs/$mode-jmeter-server_`date '+%Y-%m-%d_%H-%M-%S'`.log
fi
	
echo "END Running Jmeter on `date`"
