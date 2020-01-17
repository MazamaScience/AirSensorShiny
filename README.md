# AirShiny

`An R Shiny web application to process and display PM2.5 data from PurpleAir`

## Helpful Links

* https://support.rstudio.com/hc/en-us/articles/213733868-Running-Shiny-Server-with-a-Proxy

* https://community.rstudio.com/t/how-can-i-specify-a-static-port-for-each-shiny-app-with-shiny-server/3965

* https://community.rstudio.com/t/best-practices-with-shiny-for-accessing-routinely-updated-external-files/11183

## Desktop setup and testing

To rebuild and redeploy the Shiny app on a desktop machine just type:

```
make desktop_reboot
```

This will use the local web server to display the Shiny app at:

http://localhost:8080/app/

Shut the server down when you are finished testing with:

```
make desktop_down
```

## Setup and testing on joule.mazamascience.com

The machine joule.mazamascience .com is set up to host a test version of the 
Shiny app. The code repository exists at `/home/jonc/Projects/MazamaScience/AirSensorShiny`.

To rebuild and redeploy the Shiny app just type:

```
make joule_reboot
```

The Shiny app will be available at:

http://tools.mazamascience.com/airsensor-test/app/

