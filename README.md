# SCF #

This is a collection of communication-related programs, which can
easily be pipelined, allowing to set various transports, notifications
and redirects.

The structure which they share is currently a subset of JSON: only
key-value, with values being strings. It will allow to translate
messages into other basic formats later, and is handy for processing
by a generic tool.

The first goal is to try it. The second is to get a handy system.

Based on the plans described in
[a note](http://defanor.uberspace.net/notes/pipes-and-communication.html).


## Programs ##

Currently those are prototypes (i.e., are rather messy and there's not
much of error handling), not for real usage yet; going to refine once
will be sure about the format.

```
| JSON        | IRC, IRCD       | XMPP      | Mail         | Feed                   |
|-------------+-----------------+-----------+--------------+------------------------|
| to          | nick or channel | to        | To           |                        |
| from        | nick or user    | from      | From         | email, uri, or name    |
| message     | message         | message   | body         | text summary and links |
| subject     |                 | subject   | Subject      | entry title            |
| thread      | channel         | thread id | List-ID      | feed id                |
| id          |                 | id        | Message-Id   | entry id               |
| in-reply-to |                 |           | In-Reply-To  |                        |

```

### scf-json ###

A generic tool to perform various operations on the used JSON
structures: adding/removing/changing values, filtering, merging pipes
of JSON objects.

### scf-group ###

Basic group chats.

### scf-mail ###

IMAP and SMTP clients (over TLS, but certificate validation is turned
off for now). Fetches only new letters via IMAP.

### scf-irc ###

An IRC client.

### scf-ircd ###

A single-user IRC server.

### scf-xmpp ###

An XMPP client.

### scf-feed ###

A feed reader; currently only Atom is supported, though it should be
easy to add others (the used library supports them already).


## Examples ##


### ircd + xmpp + feed ###

Using an IRC client for XMPP, and receiving feed updates there (a
channel per feed):

```bash
cd pipes && mkfifo feed-out xmpp-in xmpp-out ircd-in ircd-out && cd ..
scf-feed http://xkcd.com/atom.xml > pipes/feed-out &
scf-xmpp [host] [login] [password] < pipes/xmpp-in > pipes/xmpp-out &
scf-ircd 6668 < pipes/ircd-in > pipes/ircd-out &
cat pipes/xmpp-out | scf-json merge <(cat pipes/feed-out | scf-json set from 'feeds!f@ee.ds' | scf-json fuse subject ": ") > pipes/ircd-in &
cat pipes/ircd-out | scf-json del from > pipes/xmpp-in &
```


### group + irc + mail + xmpp ###

Group chat (mailing list) between mail, IRC and XMPP:

```bash
#!/bin/bash

function prepare {
    test -p pipes/$1-in || mkfifo pipes/$1-in
    test -p pipes/$1-out || mkfifo pipes/$1-out
}

function run {
    prepare $1 ;
    cat pipes/$1-in | scf-json cmd f to grep ^$1: | scf-json cmd r to sed s/^$1:// | $2 | scf-json cmd r from sed s/^/$1:/ > pipes/$1-out
}

run xmpp "scf-xmpp $XMPPHOST $XMPPLOGIN $XMPPPASS" &
run mail "scf-mail $MAILHOST $MAILLOGIN $MAILPASS $MAILDIR" &
run irc "scf-irc $IRCHOST $IRCPORT $IRCTLS $IRCNICK $IRCCHANNEL" &

while :; do cat group-control ; done | scf-json merge <(cat pipes/mail-out) <(cat pipes/xmpp-out) <(cat pipes/irc-out) | scf-group | scf-json fuse from '<' '> ' | tee pipes/xmpp-in pipes/mail-in pipes/irc-in > group-log &

echo "{\"from\": \"irc:$IRCCHANNEL\", \"message\": \"[join]\"}" > group-control
```
([an illustration](http://paste.uberspace.net/mail-xmpp-irc.png))
