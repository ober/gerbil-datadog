.PHONY docker

$(eval squid_ip := $(shell docker inspect squid|jq -r '.[].NetworkSettings.IPAddress'))

docker:
	docker build --build-arg squid=$(squid_ip) --rm=true -t datadog .
	docker tag datadog jaimef/datadog

push:
	docker push jaimef/datadog
