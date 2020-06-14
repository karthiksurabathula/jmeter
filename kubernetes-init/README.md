# Create Kubernetes YAML files for JMeter Master and Slave containers  

./create.sh <number of slaves> <Script name>  

Subsequent yaml files are genereated by create.sh script using jmeter.yaml.bak  

Before you create containers place the required files under data folder configured under hostPath.path of jmeter.yaml.bak, in same path logs and results will be created  

You can configure JMeter Heap size and resource configuration in same file (jmeter.yaml.bak)  

Once you start the containers created for Kubernetes you can see Test output in maste-jmeter-XXX.log and grafana as well  