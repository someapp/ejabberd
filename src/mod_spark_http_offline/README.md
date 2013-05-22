mod_spark_msgarchive
==========

The module streams IM messages target to offline/unavailable users to Rest API SendMissedIMessages. 
If a user is blocked or in other state that disallow messages receival, these messages are 
not stored/dropped. There is no persistent storage in ejabberd server side to protect privacy 
problem.

Author: Edward Tsang <etsang@>

Motivation
-------------

One of the pieces that enable services that require offline IM messages (e,g, integeration
with email services, delayed SMS delivery etc)

Status
-------

Integeration into Ejabberd
---------------------------

Add in your ejabberd.cfg
{auth_method, spark}.
{spark_auth_endpoint, "your restful authentication end point")}.  %%endpoint is environment specific
{spark_application_id, 1234},  %%1234 is example application id,
{spark_client_secrete,"Your client digest"}. %%the client digest is for server to server communication
{rest_client_timeout_in_sec, 15}. %% 15 sec default timeout .. configurable
{rest_call_retry_attempt, 0}.  %% retry rest call NOT USED right now for future


Then your open id is    http://server.org:5280/openid/user@server.org
Hopelifully it should be possible to have more nice-looking urls.

Building
--------

Information on building and installing [Erlang/OTP](http://www.erlang.org) can
be found [here](https://github.com/erlang/otp/wiki/Installation) ([more
info](https://github.com/erlang/otp/blob/master/INSTALL.md)
The compilation depends on using rebar as compilation and packaging tool.
Referece (https://github.com/rebar/rebar.git) for usage.

Development Contributing 
==========================

Pull requests and branching
---------------------------

Use one topic branch per pull request.

Do not commit to master in your fork.

Provide a clean branch without any merge commits from upstream.

Usually you should squash any intermediate commits into the original single commit.

Code style
----------

Do not introduce trailing whitespace.

Do not mix spaces and tabs.

Do not introduce lines longer than 80 characters.

[erlang-mode (emacs)](http://www.erlang.org/doc/man/erlang.el.html) indentation
is preferred.  vi-only users are encouraged to give [Vim
emulation](http://emacswiki.org/emacs/Evil) ([more
info](https://gitorious.org/evil/pages/Home)) a try.

Writing Commit Messages
-----------------------

Structure your commit message like this:

<pre>
One line summary (less than 50 characters)

Longer description (wrap at 72 characters)
</pre>

### Summary

* Less than 50 characters
* What was changed
* Imperative present tense (fix, add, change)
  * `Fix bug 123`
  * `Add 'foobar' command`
  * `Change default timeout to 123`
* No period

### Description

* Wrap at 72 characters
* Why, explain intention and implementation approach
* Present tense

### Atomicity

* Break up logical changes
* Make whitespace changes separately

Run checks
----------

Before you submit a patch, run ``rebar test`` to execute the test suite and
check for [xref](http://www.erlang.org/doc/man/xref.html) and
[Dialyzer](http://www.erlang.org/doc/man/dialyzer.html) warnings. You may have
to run ``rebar analyze`` first.

[Dialyzer](http://www.erlang.org/doc/man/dialyzer.html) warnings are compared
against a set of safe-to-ignore warnings found in
[dialyzer_reference](https://raw.github.com/rebar/rebar/master/dialyzer_reference).
[xref](http://www.erlang.org/doc/man/xref.html) is run with [custom
queries](https://raw.github.com/rebar/rebar/master/rebar.config) to suppress
safe-to-ignore warnings.

