### Example scripts

This folder contains examples of scripts that can be used to compile a self-contained executable, copy it to the server,
and start or restart the application there. Modify the examples to create actual scrips and place them in the root of
this repo.

After running `create_executable.sh` and `copy_to_server.sh` on the
development machine, ssh into the server and create a symlink called `corpora` inside the `public` directory of the
self-contained code to the directory that contains your corpus definitions. Then run `start_fglossa_on_server.sh` (the
first time) or `restart.rb` (subsequently). Note that since the `copy_to_server.sh` script cleans the `public`
directory, the symlink to the corpus directory needs to be recreated each time after running that script.

### Deploying Glossa on the path prefix /glossa3

Glossa can run on a dedicated subdomain (e.g. https://glossa.myserver.com) or on the path prefix /glossa3
(e.g https://www.myserver.com/glossa3). This documents describes the steps needed to run the application on a path
prefix.

Requests generated dynamically by Glossa will automatically start with the /glossa3 prefix (and Glossa will expect them
to have that prefix). However, any URLs that are hardcoded into stylesheets
etc. also need to be rewritten to start with /glossa3. This can be achieved using the Apache module
mod_proxy_html (https://httpd.apache.org/docs/2.4/mod/mod_proxy_html.html), which is a standard module that
comes included with Apache (though you might have to activate it on your server).

Glossa can then be configured like this in Apache:

```
############
# fglossa
##########

ProxyHTMLLinks  a          href
ProxyHTMLLinks  area       href
ProxyHTMLLinks  link       href
ProxyHTMLLinks  img        src longdesc usemap
ProxyHTMLLinks  object     classid codebase data usemap
ProxyHTMLLinks  q          cite
ProxyHTMLLinks  blockquote cite
ProxyHTMLLinks  ins        cite
ProxyHTMLLinks  del        cite
ProxyHTMLLinks  form       action
ProxyHTMLLinks  input      src usemap
ProxyHTMLLinks  head       profile
ProxyHTMLLinks  base       href
ProxyHTMLLinks  script     src for
ProxyHTMLLinks  iframe          src

ProxyHTMLEvents onclick ondblclick onmousedown onmouseup \
onmouseover onmousemove onmouseout onkeypress \
onkeydown onkeyup onfocus onblur onload \
onunload onsubmit onreset onselect onchange

<Location /glossa3/>
ProxyHTMLEnable On
ProxyHTMLExtended On
ProxyHTMLDocType "<!DOCTYPE html>"
RequestHeader unset Accept-Encoding
ProxyHTMLURLMap / /glossa3/ e
</Location>

RedirectMatch permanent ^/glossa3$ /glossa3/

RewriteRule /(glossa3/(?:api|rest)/.+) http://localhost:8085/$1 [P,L]
RewriteRule /glossa3/(.*) http://localhost:8085/$1 [P,L]
ProxyPass /glossa3/ http://localhost:8085/
ProxyPassReverse /glossa3/ http://localhost:8085/

Header edit Location ^/ /glossa3/
ProxyRequests Off

##############
# End fglossa
#############
```

Unfortunately, mod_proxy_html cannot rewrite all URLs that need to be rewritten, so we also need to run the following
command each time a new version of Glossa has been pushed to the server (replacing `/path/to/fglossa` with the actual
path to fglossa on your server). Note that this command is included in the example script `copy_to_server.sh.example`.

```
sed -i 's/url(\//url\(\/glossa3\//g' /path/to/fglossa/self-contained/linux-x64/public/*.css \
    && sed -r -i 's/\"(\w+\.svg)/\"glossa3\/\1/g' /path/to/fglossa/self-contained/linux-x64/public/*.js
```
