# SCF #

This is a set of communication-related programs, which can easily be
pipelined, allowing to set various transports, notifications and
redirects.

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

### scf-json ###

A generic tool to perform various operations on the used JSON
structures: adding/removing/changing values, filtering, merging pipes
of JSON objects.

### scf-feed ###

A feed reader; currently only Atom is supported, though it should be
easy to add others (the used library supports them already).

### scf-irc ###

An IRC client.

### scf-xmpp ###

An XMPP client.


## Example ##

Sending messages from IRC (both private and from channels) to one
particular JID, answering those from XMPP (either private or to a
channel, while using a default channel if no target specified), and
scraping XKCD atom feed, sending notifications via XMPP:

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
