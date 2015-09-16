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
| thread      | channel         | thread id | Mailing-list | feed id                |
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


### irc + xmpp + feed ###

Sending messages from IRC (both private and from channels) to one
particular JID, answering those from XMPP (either private or to a
channel, while using a default channel if no target specified), and
scraping XKCD Atom feed, sending notifications via XMPP:

```bash
cd pipes && mkfifo feed-out xmpp-in xmpp-out irc-in irc-out && cd ..
# atom feeds
scf-feed http://xkcd.com/atom.xml > pipes/feed-out &
# xmpp
scf-xmpp [host] [login] [password] < pipes/xmpp-in > pipes/xmpp-out &
# irc
scf-irc irc.freenode.net 6697 True scf-irc \#scf-irc < pipes/irc-in > pipes/irc-out &
# xmpp → irc
cat pipes/xmpp-out | tee xmpp-out.log | scf-json del to | scf-json extract to ': ' | scf-json add to \#scf-irc > pipes/irc-in &
# irc + feed → xmpp
cat pipes/irc-out | tee irc-out.log | scf-json fuse from '<' '> ' | scf-json fuse thread ' ' | tee merge-in.log | scf-json merge <(cat pipes/feed-out) | tee merge-out.log | scf-json set to [jid] | tee xmpp-in.log > pipes/xmpp-in &
```


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


### group + irc + mail ###

Group chat (mailing list) between mail and IRC.

```bash
# group
scf-group < pipes/group-in > pipes/group-out &
# irc
scf-irc irc.freenode.net 6697 True scf-irc \#scf-irc < pipes/irc-in > pipes/irc-out &
# mail
scf-mail uberspace.net groupchat [password] INBOX < pipes/mail-in > pipes/mail-out &
# mail to group
cat pipes/mail-out | scf-json cmd r from sed 's/\(.\)/mail:\1/' > pipes/mail-to-group &
# group to mail
cat pipes/group-to-mail | scf-json cmd f to grep '^mail:' | scf-json cmd r to sed 's/mail://' | scf-json fuse from ": " | scf-json set from 'groupchat@uberspace.net' > pipes/mail-in &
# irc to group
cat pipes/irc-out | scf-json cmd r from sed 's/\(.\)/irc:\1/' > pipes/irc-to-group &
# group to irc
cat pipes/group-to-irc | scf-json cmd f to grep '^irc:' | scf-json cmd r to sed 's/irc://' | scf-json fuse from ": " > pipes/irc-in &
# group → mail + irc
cat pipes/group-out | tee pipes/group-to-mail > pipes/group-to-irc &
# irc + mail → group
cat pipes/irc-to-group | scf-json merge pipes/mail-to-group > pipes/group-in &
```
