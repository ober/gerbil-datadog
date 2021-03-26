from jaimef/alpine-current:static

MAINTAINER jaimef@linbsd.org
COPY . /src
RUN make -C /src linux-static
RUN mv /src/dda-bin /bin

CMD [ /bin/dda ]
