FROM clfoundation/sbcl:2.1.7

ENV QUICKLISP_ADD_TO_INIT_FILE=true

WORKDIR /root/quicklisp/local-projects
COPY . .

ENTRYPOINT ["./docker-entrypoint.sh"]
