FROM ubuntu
WORKDIR /app
COPY build .
EXPOSE 8000
CMD ["/app/bin/jamq"]
