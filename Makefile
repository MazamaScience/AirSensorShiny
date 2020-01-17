################################################################################
# Makefile for building and running docker containers for AirSensorShiny
#
# On joule, ProxypPass settings are defined in:
#
#   /etc/httpd/conf.d/tools.mazamascience.com.conf
#
# Proxying instructions from:
#
#   https://support.rstudio.com/hc/en-us/articles/213733868-Running-Shiny-Server-with-a-Proxy
#
# Note that we are proxying from the port exposed in the Dockerfile.
#
# 6700-6709 airsensor ---------------------------------------------------------
# # 6701 -- v1 operational
# # 6709 -- test
#  <Proxy *>
#    Allow from localhost
#  </Proxy>
#
# RewriteEngine on
# RewriteCond %{HTTP:Upgrade} =websocket
# RewriteRule /airsensor-test/(.*) ws://localhost:6709/$1 [P,L]
# RewriteCond %{HTTP:Upgrade} !=websocket
# RewriteRule /airsensor-test/(.*) http://localhost:6709/$1 [P,L]
# ProxyPass /airsensor-test/ http://localhost:6709/
# ProxyPassReverse /airsensor-test/ http://localhost:6709/
#
# Header edit Location ^/ /airsensor-test/
# ProxyRequests Off
#
#
# Test these settings on CentOS with:    "sudo apachectl configtest"
# Reload these settings on CentOS with:  "sudo apachectl graceful"
#

# NOTE:  The SERVICE_PATH should match that found in Dockerfile and Dockerfile-test
SERVICE_PATH=airsensor-shiny/v1
SERVICE_PATH_TEST=airsensor-shiny/test


# NOTE:  Version associated with R packages and shiny-server code
SERVER_VERSION=1.3.8

# NOTE:  Version associated with Shiny app
# first version . airsensorshiny 1.3.6 . calendar plot
APP_VERSION=0.7.0

# AirSensorShiny DESKTOP version -----------------------------------------------


desktop_build:
	-mkdir airsensorshiny/output
	docker build -t airsensor-shiny-desktop:$(APP_VERSION) \
		-t airsensor-shiny-desktop:latest -f docker/Dockerfile-test .

desktop_up:
	docker-compose -f docker/docker-compose-desktop.yml \
		-p airsensorshinydesktop up -d

desktop_down:
	docker-compose -f docker/docker-compose-desktop.yml \
		-p airsensorshinydesktop down

desktop_container_logs:
	docker-compose -f docker/docker-compose-desktop.yml \
		-p airsensorshinydesktop logs -f

desktop_bounce: desktop_down desktop_up

desktop_reboot: desktop_build desktop_down desktop_up


# AirSensorShiny TEST version --------------------------------------------------

test_build:
	-mkdir airsensorshiny/test
	docker build -t airsensor-shiny-test:$(APP_VERSION) \
		-t airsensor-shiny-test:latest -f docker/Dockerfile-test .

test_up:
	docker-compose -f docker/docker-compose-test.yml \
		-p airsensorshinytest up -d

test_down:
	docker-compose -f docker/docker-compose-test.yml \
		-p airsensorshinytest down

test_container_logs:
	docker-compose -f docker/docker-compose-test.yml \
		-p airsensorshinytest logs

test_trace_log:
	cat /var/www/tools.mazamascience.com/html/logs/$(SERVICE_PATH_TEST)/app/TRACE.log

test_debug_log:
	cat /var/www/tools.mazamascience.com/html/logs/$(SERVICE_PATH_TEST)/app/DEBUG.log

test_info_log:
	cat /var/www/tools.mazamascience.com/html/logs/$(SERVICE_PATH_TEST)/app/INFO.log

test_error_log:
	cat /var/www/tools.mazamascience.com/html/logs/$(SERVICE_PATH_TEST)/app/ERROR.log

test_bounce: test_down test_up

test_reboot: test_build test_down test_up

# AirSensorShiny JOULE version --------------------------------------------------

joule_build:
	-mkdir airsensorshiny/test
	docker build -t airsensor-shiny-test:$(APP_VERSION) \
		-t airsensor-shiny-test:latest -f docker/Dockerfile-test .

joule_up:
	docker-compose -f docker/docker-compose-test_joule.yml \
		-p airsensorshinytest up -d

joule_down:
	docker-compose -f docker/docker-compose-test_joule.yml \
		-p airsensorshinytest down

joule_container_logs:
	docker-compose -f docker/docker-compose-test_joule.yml \
		-p airsensorshinytest logs

joule_trace_log:
	cat /var/www/tools.mazamascience.com/html/logs/$(SERVICE_PATH_TEST)/app/TRACE.log

joule_debug_log:
	cat /var/www/tools.mazamascience.com/html/logs/$(SERVICE_PATH_TEST)/app/DEBUG.log

joule_info_log:
	cat /var/www/tools.mazamascience.com/html/logs/$(SERVICE_PATH_TEST)/app/INFO.log

joule_error_log:
	cat /var/www/tools.mazamascience.com/html/logs/$(SERVICE_PATH_TEST)/app/ERROR.log

joule_bounce: joule_down joule_up

joule_reboot: joule_build joule_down joule_up

# AirSensorShiny DOCKER CORE ---------------------------------------------------

airsensorshiny_build:
	cd docker; docker build -t mazamascience/airsensorshiny:$(SERVER_VERSION) -f Dockerfile-airsensorshiny .

