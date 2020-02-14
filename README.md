# monitor-data #

This directory contains a dockerizable R Shiny web app to process and display
PurpleAir data accessed with the **AirSensor** package.

## Restart Instructions for a Production Site 

```make production_bounce```

*or, for joule.mazamascience.com:*

```make joule_bounce```

*(Yes, that's it.)*

***
***

Now for the gory details.

## Helpful Links

* https://support.rstudio.com/hc/en-us/articles/213733868-Running-Shiny-Server-with-a-Proxy

* https://community.rstudio.com/t/how-can-i-specify-a-static-port-for-each-shiny-app-with-shiny-server/3965

* https://community.rstudio.com/t/best-practices-with-shiny-for-accessing-routinely-updated-external-files/11183

* https://www.linuxjournal.com/content/integrating-web-applications-apache

## Running the app from RStudio ##

Inside RStudio you can run the app with:

 * open up `app.R`
 * click the "Source" button

RStudio will automatically launch the application.
 
## Running the app with Docker ##

To rebuild and redeploy the Shiny app on a desktop machine just type:

On OSX:

```
make configure_app_osx
make desktop_reboot
```

On Linux:

```
make configure_app
make desktop_reboot
```

This will use the local web server to display the Shiny app at:

http://localhost:8080/airsensor-dataviewer/test

Shut the server down when you are finished testing with:

```
make desktop_down
```

## Setup and testing on joule.mazamascience.com

_(See the integrating-web-applications-apache link above.)_

The machine joule.mazamascience .com is set up to host a test version of the 
Shiny app. The code repository exists at `/home/jonc/Projects/MazamaScience/AirSensorShiny`.

To rebuild and redeploy the Shiny app just type:

```
make joule_reboot
```

The Shiny app will be available at:

http://tools.mazamascience.com/airsensor-test/app/

