#!/bin/sh

sqlite3 src/Server/db/fglossa.sqlite -init db_commands.txt
