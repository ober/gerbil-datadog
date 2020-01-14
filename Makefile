.PHONY: datadog

default: datadog

SYSTEM := $(shell uname -s)

ifeq ($(SYSTEM),Darwin)
SSL-BASE :=$(lastword $(wildcard /usr/local/Cellar/openssl/*/))
SED := sed
MYSQL-BASE := $(lastword $(wildcard /usr/local/Cellar/mysql/*/))
LIBYAML-BASE := $(lastword $(wildcard /usr/local/Cellar/libyaml/*/))
#$(eval LDFLAGS := "-L$(SSL-BASE)lib -L$(LIBYAML-BASE)lib -lz -lssl -lyaml -L/usr/local/lib")
#$(eval CPPFLAGS := "-I$(SSL-BASE)include -I$(LIBYAML-BASE)include -I/usr/local/include")
else
LDFLAGS := "-L/usr/lib -lssl -lyaml"
CPPFLAGS := "-I/usr/include"
LIBYAML-BASE := "/usr/include"
SED := sed
endif

datadog: $(eval CPPFLAGS := "-I$(SSL-BASE)include -I$(LIBYAML-BASE)include -I/usr/local/include")
datadog: $(eval LDFLAGS := "-L$(SSL-BASE)lib -L$(LIBYAML-BASE)lib -lz -lssl -lyaml -L/usr/local/lib")
datadog:
	gxc -O -o dda -static -exe -g -genv -cc-options $(CPPFLAGS) -ld-options $(LDFLAGS) -gsrc -gsc-flag -keep-c datadog/dda.ss

datadog-fat: $(eval CPPFLAGS := "-I$(SSL-BASE)include -I$(LIBYAML-BASE)include -I/usr/local/include")
datadog-fat: $(eval LDFLAGS := "-L$(SSL-BASE)lib -L$(LIBYAML-BASE)lib -lz -lssl -lyaml -L/usr/local/lib")
datadog-fat:
	gxc -O -o dda-fat -static -exe -cc-options $(CPPFLAGS) -ld-options $(LDFLAGS) -gsrc datadog/dda.ss

datadog-skinny: $(eval CPPFLAGS := "-I$(SSL-BASE)include -I$(LIBYAML-BASE)include -I/usr/local/include")
datadog-skinny: $(eval LDFLAGS := "-L$(SSL-BASE)lib -L$(LIBYAML-BASE)lib -lz -lssl -lyaml -L/usr/local/lib")
datadog-skinny:
	gxc -O -o dda-skinny -static -exe -cc-options $(CPPFLAGS) -ld-options $(LDFLAGS) datadog/dda.ss

linux-static:
	docker run -e GERBIL_PATH=/dd/.gerbil -e GERBIL_HOME=/root/gerbil -e PATH='/root/gerbil/bin:/usr/local/gambit/current/bin:/bin:/usr/bin:/sbin:/usr/sbin' -v $(PWD):/dd -it jaimef/centos bash -c 'cd /dd && unset http_proxy; unset https_proxy; make linux-static-intern'

linux-static-intern:
	export GERBIL_HOME=/root/gerbil
	gxpkg install github.com/ober/oberlib
	gxc -o dda-linux-static -cc-options "-Bstatic -DOPENSSL_NO_KRB5 -I/usr/local/include -I/usr/local/ssl/include" -static -ld-options "-static -L/usr/lib64 -L/usr/local/ssl/lib -lssl -L/usr/local/lib -ldl -lyaml -lz" -gsc-option -prelude '(declare (not safe))' -exe datadog/dda.ss datadog/client.ss

linux:
	gxc -o dd-linux -cc-options "-Bstatic -DOPENSSL_NO_KRB5 -I/usr/local/include -I/usr/local/ssl/include" -static -ld-options "-static -L/usr/lib64 -L/usr/local/ssl/lib -lssl -L/usr/local/lib -ldl -lyaml -lz" -exe datadog/dda.ss

linux-debug:
	gxc -o dd-linux -cc-options "-Bstatic -DOPENSSL_NO_KRB5 -I/usr/local/include -I/usr/local/ssl/include" -g -gsrc -genv -static -ld-options "-static -L/usr/lib64 -L/usr/local/ssl/lib -lssl -L/usr/local/lib -ldl -lyaml -lz" -exe datadog/dda.ss

fast: $(eval CPPFLAGS := "-I$(SSL-BASE)include -I$(LIBYAML-BASE)include -I/usr/local/include")
fast: $(eval LDFLAGS := "-L$(SSL-BASE)lib -L$(LIBYAML-BASE)lib -lz -lssl -lyaml -L/usr/local/lib")
fast:
	gxc -O -o dda -exe -g -genv -cc-options $(CPPFLAGS) -ld-options $(LDFLAGS) -gsrc -gsc-flag -keep-c datadog/dda.ss

docker:
	docker build --rm=true -t datadog .
	docker tag datadog jaimef/datadog
	docker push jaimef/datadog
