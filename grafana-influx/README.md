#Grafana and InfluxDB

##influxdb.yaml

Update hostPath in influxdb.yaml under hostPath.path to persist InfluxDB data.

Update Resource usage setting to suit your needs.

On start of container it will create the below

db: jmeter
admin_userName: admin
admin_password: password123

##grafana.yaml

Update hostPath in grafana.yaml under hostPath.path to persist Grafana data.

Update Resource usage setting to suit your needs.

Grafana is accessible using [http://localhost:8080/](http://localhost:8080/)

Use the below credentials 
user_id: admin
password: admin

Configure Datasource in Grafana with details mentioned above in InfluxDB

 