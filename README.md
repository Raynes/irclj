# irclj

Irclj is a Clojure IRC library that might remind you of Pircbot to some extent. I originally designed it to replace Pircbot in sexpbot, my Clojure IRC bot. I've already rewritten sexpbot to use the library, so it's probably the best usage example at this point.

The library offers a pretty thorough bot framework. It offers flood protection that is essentially the same as Pircbot's, and various important features you'd expect out of such a library. It's designed so that it should be possible to use this library to create clients as well as IRC bots.

## Usage

See testirclj.clj in src/irclj for a tiny little example bot. For a larger, real-world example, see http://www.github.com/flatland/sexpbot

The first thing you do is define an IRC record with create-irc. It takes a map. You can supply a nick (name) for the bot, a password, a server, a username, a port, a realname, and a map of fns that will be called when certain things happen. There are some other more advanced things you can supply. Check out it's arglist for details, or do `(doc create-irc)`.

The fnmap can contain 0 or more functions. The functions need to be named specifically. Everytime something is triggered internally, one of the functions will be called. I'll list the supported "actions" and the corrosponding functions you need to define in fnmap if you wish to handle them specially.

    PRIVMSG ; on-message
    QUIT ; on-quit
    JOIN ; on-join
    PART ; on-part
    NOTICE ; on-notice
    MODE ; on-mode
    KICK ; on-kick
    TOPIC ; on-topic
    CTCP ACTION ; on-action
    ERROR OCCURS ; on-error
    ANYTHING EXCEPT ERROR OCCURS ; on-any
    001 RECIEVED (BOT IS CONNECTED) ; on-connect

You can supply none or as many (if you don't want to be useful ;)) of these as you like. If you define an on-message function in fnmap, and the irc connection/bot sees a PRIVMSG, that function will be called. Each function, when called, is passed a map. They supply different things in the map depending on what function was called. There are certain things that are always in the map regardless of what is called. These are:

    :user ; The entire identification of the sender in the form of nick!ident@host/mask/here if relevant
    :nick ; The user's nick if relevant
    :hmask ; The user's hostmask if relevant
    :ident ; The user's ident if relevant
    :doing ; The action that was triggered. One of the things listed above.
    :irc ; The IRC record reference. This is important. You need to pass this to IRC functions to do fun things.

In the :irc key, which is a ref of the original IRC record you created with stuff added, you have all of the information you supplied initially for the bot, with a couple of new things. :connection is a map of sock, sockin, sockout, in case you want to hijack the sockets, and :channels is a list of channels you're in.

And here are action specific map keys that are put in the map along with the keys above:

    PRIVMSG ; :channel, :message
    QUIT ; :reason
    JOIN ; :channel
    PART ; :channel, :reason
    NOTICE ; :target, :message
    MODE ; :channel, :mode, :user
    KICK ; :channel, :target, :message
    TOPIC ; :channel, :topic
    ERROR ; :error

All of this information is sent in a single map that you can destructure in the functions in fn map. Simple, right? Well, I suck at explaining things, but looking at the example(s) in the /examples directory should help. You can also look at the bot that this library was originally created for, [sexpbot](http://github.com/flatland/sexpbot). If you have any questions, you can find me on freenode, at #clojure, #clojure-casual, and #(code). If you have any questions or suggestions, feel free to hit me up with them.

## Installation

Check the version number in project.clj, and then add irclj as a dependency in your project's project.clj, assuming you use Leiningen.

## Getting Help

We have an IRC channel over on freenode. It's #irclj. If you have any questions, drop in and we'll answer 'em for ya.

## License

Licensed under the same thing Clojure is licensed under. The EPL, of which you can find a copy of here: http://www.eclipse.org/legal/epl-v10.html and at the root of this directory.