docker:
	docker build --build-arg --rm=true -t datadog .
	docker tag datadog jaimef/datadog

push:
	docker push jaimef/datadog
