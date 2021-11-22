#!/bin/sh

# Builds the project inside a docker container to make sure it runs in a container
# build from the same image on the server. Assumes that the project is checked out
# in a sibling directory of this one called fglossa_docker, where clean_all.sh has been
# run in order to remove locally compiled nuget or npm packages that might be incompatible
# with the platform running on the server. 
#
# Also assumes that a container named fglossa_dev has been started with the fglossa_docker
# directory linked to the working directory inside the container, and that the projects
# are restored inside the container. This can be accomplished by running the following commands 
# from the fglossa_docker directory:
#
# $ rm -rf deploy/* node_modules paket-files src/Client/bin/* src/Client/obj/* src/Server/bin/* src/Server/obj/*
# $ docker run -it -d --mount type=bind,src=${PWD},dst=/app -w /app --name fglossa_dev dotnet6_node16 bash
# $ docker exec -it fglossa_dev dotnet tool restore && dotnet paket install && dotnet restore src/Server && dotnet restore src/Client
#
# After this script has been run, the compiled project will be found in the directory ../fglossa_docker/deploy,
# which can then be copied to the server and run using 'cd deploy && dotnet Server.dll' (presumably
# inside a Docker container that is build from the same image and has the project directory mounted
# there as well).

set -ev

git push && cd ../fglossa_docker && ./pull.sh \
	&& docker exec -it fglossa_dev dotnet run bundle \
	&& gsed -i -e 's/<head>/<head>\n    <base href="https:\/\/tekstlab.uio.no\/glossa3\/">/' deploy/public/index.html
