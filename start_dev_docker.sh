#!/bin/sh

set -e

docker run -it -d \
	--mount type=bind,src="${PWD}",dst=/app \
	--mount type=bind,src="${PWD%/fglossa_docker}"/fglossa/src/Corpora,dst=/app/src/Corpora \
	-w /app --name fglossa_dev dotnet6_node16 bash

echo "Run docker exec -it fglossa_dev bash and then:"
echo "dotnet tool restore && dotnet paket install && dotnet restore src/Client && dotnet restore src/Server"
