from gerbil/scheme:latest

MAINTAINER jaimef@linbsd.org
COPY . /root/datadog
ENV PATH "$PATH:/root/gerbil/bin"
ENV GERBIL_HOME "/root/gerbil"
RUN cd /root/datadog && ./build.ss static
RUN cp /root/datadog/datadog /bin/dda

CMD /bin/bash
