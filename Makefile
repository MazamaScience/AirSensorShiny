################################################################################
# Makefile for building and running docker containers for AirSensorShiny
#
# On joule, ProxypPass settings are defined in:
#
#   /etc/httpd/conf.d/tools.mazamascience.com.conf
#
# # 6700-6709 airsensor-shiny --------------------------------------------------
# # 6701 -- v1 operational
# # 6709 -- test (development)
# ProxyPass /airsensor-shiny/v1 http://127.0.0.1:6701
# ProxyPassReverse /airsensor-shiny/v1 http://127.0.0.1:6701
# ProxyPass /airsensor-shiny/test http://127.0.0.1:6709
# ProxyPassReverse /airsensor-shiny/test http://127.0.0.1:6709
#
# Test these settings on CentOS with:    "sudo apachectl configtest"
# Reload these settings on CentOS with:  "sudo apachectl graceful"

# NOTE:  The SERVICE_PATH should match that found in Dockerfile and Dockerfile-test
SERVICE_PATH=airsensor-shiny/v1
SERVICE_PATH_TEST=airsensor-shiny/test


# first version . --- . working on a Mac
VERSION=1.0.1

# AirSensorShiny DESKTOP version -----------------------------------------------


desktop_build:
	-mkdir airsensorshiny/output
	docker build -t airsensor-shiny-desktop:$(VERSION) \
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

desktop_reboot: desktop_down desktop_build desktop_up


# AirSensorShiny TEST version --------------------------------------------------

test_build:
	-mkdir airsensorshiny/test
	docker build -t airsensor-shiny-test:$(VERSION) \
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
	cat /var/log/$(SERVICE_PATH_TEST)/app/TRACE.log

test_debug_log:
	cat /var/log/$(SERVICE_PATH_TEST)/app/DEBUG.log

test_info_log:
	cat /var/log/$(SERVICE_PATH_TEST)/app/INFO.log

test_error_log:
	cat /var/log/$(SERVICE_PATH_TEST)/app/ERROR.log

test_bounce: test_down test_up

test_reboot: test_down test_build test_up

