# Makefile for building and running docker containers for AirSensorShiny

SERVICE_PATH_TEST=airshiny/test


VERSION=1.0

# AirSensorShiny DESKTOP version -----------------------------------------------


desktop_build:
	-mkdir airshiny/output
	docker build -t mazamascience/airshiny:$(VERSION) \
		-t mazamascience/airshiny:latest -f docker/Dockerfile-test .

desktop_up:
	docker-compose -f docker/docker-compose-desktop.yml \
		-p airshinydesktop up -d

desktop_down:
	docker-compose -f docker/docker-compose-desktop.yml \
		-p airshinydesktop down

desktop_container_logs:
	docker-compose -f docker/docker-compose-desktop.yml \
		-p airshinydesktop logs -f

desktop_bounce: desktop_down desktop_up

desktop_reboot: desktop_down desktop_build desktop_up


# AirSensorShiny TEST version --------------------------------------------------

# test_build:
# 	-mkdir airshiny/test
# 	docker build -t mazamascience/airshiny:$(VERSION) \
# 		-t mazamascience/airshiny:latest -f docker/Dockerfile-test .
# 
# test_up:
# 	docker-compose -f docker/docker-compose-test.yml \
# 		-p airshinytest up -d
# 
# test_down:
# 	docker-compose -f docker/docker-compose-test.yml \
# 		-p airshinytest down
# 
# test_container_logs:
# 	docker-compose -f docker/docker-compose.yml \
# 		-p airshinytest logs
# 
# test_trace_log:
# 	cat /var/log/$(SERVICE_PATH_TEST)/app/TRACE.log
# 
# test_debug_log:
# 	cat /var/log/$(SERVICE_PATH_TEST)/app/DEBUG.log
# 
# test_info_log:
# 	cat /var/log/$(SERVICE_PATH_TEST)/app/INFO.log
# 
# test_error_log:
# 	cat /var/log/$(SERVICE_PATH_TEST)/app/ERROR.log
# 
# test_bounce: test_down test_up
# 
# test_reboot: test_down test_build test_up


# PRODUCTION version -----------------------------------------------------------

#production_build:
#	-mkdir airshiny/prod
#	docker build -t mazamasciene/airshiny:$(VERSION) \
#		-t mazamascience/airshiny:latest -f docker/Dockerfile .
#
#production_up:
#	docker-compose -f docker/docker-compose.yml \
#		-p airshiny up -d
#
#production_down:
#	docker-compose -f docker/docker-compose.yml \
#		-p airshiny down
#
#production_container_logs:
#	docker-compose -f docker/docker-compose.yml \
#		-p airshiny logs
#
#production_trace_log:
#	cat /var/log/$(SERVICE_PATH)/app/TRACE.log
#
#production_debug_log:
#	cat /var/log/$(SERVICE_PATH)/app/DEBUG.log
#
#production_info_log:
#	cat /var/log/$(SERVICE_PATH)/app/INFO.log
#
#production_error_log:
#	cat /var/log/$(SERVICE_PATH)/app/ERROR.log
#
#production_bounce: production_down production_up
#
#production_reboot: production_down production_build production_up
#
