#!/bin/sh

# Creates a self-contained executable (i.e. an executable that includes the
# .NET runtime so it does not have to be installed on the target machine)
# with the given runtime identifier (RID)
# See https://docs.microsoft.com/en-us/dotnet/core/rid-catalog

RID=${1:-linux-x64}
CORPUS_ROOT=${GLOSSA_CORPUS_ROOT:-src/Corpora/corpora}

rm -rf self-contained/${RID}
dotnet publish -c Release -o self-contained/${RID} -r ${RID} --self-contained true src/Server
dotnet run BuildClient
cp -R deploy/public self-contained/${RID}

echo "#############################################################################################
|                                                                                           |
|  NOTE: After the application has been deployed, the corpus root on the target machine     |
|  should be symlinked into the 'public' directory of the application to enable serving of  |
|  corpus-specific assets (e.g. corpus logo).                                               |
|                                                                                           |
#############################################################################################"