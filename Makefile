PROJECT := datadog
PWD := $(shell pwd)
ARCH := $(shell uname -m)
UID := $(shell id -u)
GID := $(shell id -g)
DOCKER_IMAGE := "gerbil/gerbilxx:$(ARCH)-master"

default: linux-static-docker

check-root:
	@if [ "${UID}" -eq 0 ]; then \
	git config --global --add safe.directory /src; \
	fi

deps:
	/opt/gerbil/bin/gxpkg install github.com/ober/oberlib

build: deps check-root
	/opt/gerbil/bin/gxpkg link $(PROJECT) /src || true
	/opt/gerbil/bin/gxpkg build -R $(PROJECT)

linux-static-docker: clean
	docker run -t \
	-u "$(UID):$(GID)" \
	-v $(PWD):/src:z \
	$(DOCKER_IMAGE) \
	make -C /src build

clean:
	rm -rf .gerbil manifest.ss

install:
	mv .gerbil/bin/$(PROJECT) /usr/local/bin/$(PROJECT)
