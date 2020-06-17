pwd=`pwd`
export JENKINS_HOME="$pwd/data/"

FILE=$pwd/jenkins.war
if [[ -f "$FILE" ]]; then
    echo "$FILE exists."
else
    echo "Downloading jenkins.war file"
    cd $FILE
    wget http://mirrors.jenkins.io/war-stable/latest/jenkins.war
fi

if ps -p `cat $pwd/pid` > /dev/null
then
   echo "Jenkins is running PID: `cat $pwd/pid`"
else
   nohup java -Dhudson.model.DirectoryBrowserSupport.CSP="sandbox allow-scripts allow-popups allow-popups-to-escape-sandbox; style-src 'unsafe-inline' *;" -jar jenkins.war > jenkins.log 2>&1 & echo $!>$pwd/pid
   echo "Jenkins started with PID: `cat $pwd/pid`"
fi

