#!/bin/bash
set -x

echo "JVM_ARGS=${JVM_ARGS}"

echo "START Running Jmeter on `date`"

#add properties
echo server.rmi.ssl.disable=true >> /opt/apache-jmeter-5.3/bin/user.properties

#Install PluginManager and plugins  https://jmeter-plugins.org/wiki/PluginsManagerAutomated/
java -cp /opt/apache-jmeter-5.3/lib/ext/jmeter-plugins-manager-1.4.jar org.jmeterplugins.repository.PluginManagerCMDInstaller
PluginsManagerCMD.sh install jpgc-casutg,jpgc-dummy
PluginsManagerCMD.sh status

#Start Jmeter
if [ "$mode" == "master" ]; then
  jmeter $@
else
  jmeter-server >>  /mnt/jmeter/logs/$mode-jmeter-server_`date '+%Y-%m-%d_%H-%M-%S'`.log
fi
	
echo "END Running Jmeter on `date`"
