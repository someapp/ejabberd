!/usr/bin/bsh
echo on
curl -O "https://raw.github.com/processone/ejabberd/master/src/ejabberd.hrl"
curl -O "https://raw.github.com/processone/ejabberd/master/src/jlib.hrl"
mv -vf ejabberd.hrl ./include
mv -vf jlib.hrl ./include
