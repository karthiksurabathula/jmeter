# Build and Push container to Docker Hub  

Please refer http://www.perfblogspot.com/2020/06/docker-image-for-jmeter-with-plugin.html  

Use the below command to create docker container  
docker build -t jmeter:test .  

To push docker container to Docker hub you need to get container Id and tag it to the repository in docker hub.  

docker images   

above command will give output as below.  

root@master:/jmeter/jenkins/data/workspace# docker images  
REPOSITORY                  TAG                 IMAGE ID            CREATED             SIZE  
jmeter                      test                d83f4d7d6fa0        15 hours ago        217MB  

docker tag <Image ID> <docker repository name>  


Before pushing image you should validate your credentials  

docker login  

provide user name and password  

docker push <docker repository name>  