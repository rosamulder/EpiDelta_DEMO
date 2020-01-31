## Lightweight R/Shiny Docker implementation

This is a simple framework to enable rapid containerisation of R/Shiny apps. We use the excellent [rocker/shiny](https://github.com/rocker-org/shiny) image.

### Pre-requisites

You will need to have [Docker](https://www.docker.com/) installed on the computer on which you wish to containerise your app. 

### How to use

1. Clone this repository
2. Place your shiny app within the apps directory (an example app is provided)
3. Edit docker-compose.yml to provide an appropriate port number (if you are testing on your own machine you can use the default, but if you are deploying a container on a server you will need to ensure you don't use a port number already in use).
4. Run the ```docker-compose up``` command

Docker will pull the rocker/shiny image and create the shinyserver. This may take a little while. 

Once the shinyserver container has started you can access it at [http://localhost:3401/](http://localhost:3401/) if you have used the default port on your local computer. The example app will be at [http://localhost:3401/example-app/](http://localhost:3401/example-app/).

If you used a different port number, or are running Docker on a remote server, please substitute hostname and port accordingly: **http://\<hostname\>:\<portnumber\>/**.

### Useful information

* Use ```<ctrl>-c``` to stop the docker-compose output
* ```docker stop shinyserver``` will stop the container
* ```docker rm shinyserver``` will remove the container
* You can stop and remove the container and re-run ```docker-compose up``` without the rocker/shiny image needing to be pulled each time. This means it is quick and easy to make configuration changes (eg changing port number)

