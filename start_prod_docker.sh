#!/bin/sh

# Starts a .NET 5 container and runs ./deploy/Server.dll with the dotnet command.
# When a new version of Server.dll is deployed, simply restart the container to
# make the web app pick up the new version.

docker run -it -d \
    --mount type=bind,src="${PWD}"/deploy,dst=/app/deploy \
    -w /app/deploy \
    -p 8088:8085 \
    --name fglossa \
    mcr.microsoft.com/dotnet/sdk:6.0 \
    dotnet Server.dll
