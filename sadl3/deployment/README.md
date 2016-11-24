## Build and Start image:
```
docker build --rm=true -t sadlos3/websadl .
docker run -p 8888:8080 sadlos3/websadl
# sh to container 
sudo docker exec -i -t <name> /bin/bash
```

