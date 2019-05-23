from jaimef/gerbil

MAINTAINER jaimef@linbsd.org
COPY . /root/datadog
ENV PATH "$PATH:/root/gerbil/bin"
ENV GERBIL_HOME "/root/gerbil"
RUN cd /root/datadog && make
RUN cp /root/datadog/dda /bin/dda
RUN rm -rf /root/gerbil /root/gambit
CMD /bin/bash
