# irclj

Irclj is a Clojure IRC library that might remind you of Pircbot to some extent. I originally designed it to replace Pircbot in sexpbot, my Clojure IRC bot. I've already rewritten sexpbot to use the library, so it's probably the best usage example at this point.

## Usage

See testirclj.clj in src/irclj for a tiny little example bot. For a larger, real-world example, see http://www.github.com/Raynes/sexpbot

The first thing you do is define an IRC record with create-irc. It takes a map. You can supply a nick (name) for the bot, a password, a server, a username, a port, a realname, and a map of fns that will be called when certain things happen.

The fnmap can contain 0 or more functions. The functions need to be named specifically. Everytime something is triggered internally, one of the functions will be called. I'll list the supported "actions" and the corrosponding functions you need to define in fnmap if you wish to handle them specially.

    PRIVMSG ; on-message
    QUIT ; on-quit
    JOIN ; on-join
    PART ; on-part
    NOTICE ; on-notice
    MODE ; on-mode
    KICK ; on-kick
    TOPIC ; on-topic

You can supply none or as many (if you don't want to be useful ;)) of these as you like. If you define an on-message function in fnmap, and the irc connection/bot sees a PRIVMSG, that function will be called. Each function, when called, is passed a map. They supply different things in the map depending on what function was called. There are certain things that are always in the map regardless of what is called. These are:

    :user ; The entire identification of the sender in the form of nick!ident@host/mask/here
    :nick ; The user's nick
    :hmask ; The user's hostmask
    :ident ; The user's ident
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

All of this information is sent in a single map that you can destructure in the functions in fn map. Simple, right? Well, I suck at explaining things, but looking at the example(s) in the /examples directory should help. You can also look at the bot that this library was originally created for, [sexpbot](http://github.com/Raynes/sexpbot). If you have any questions, you can find me on freenode, at #clojure, #clojure-casual, and #(code). If you have any questions or suggestions, feel free to hit me up with them.

## Installation

Check the version number in project.clj, and then add irclj as a dependency in your project's project.clj, assuming you use Leiningen.

## License

Licensed under the same thing Clojure is licensed under. The EPL, of which you can find a copy of here: http://www.eclipse.org/legal/epl-v10.html and at the root of this directory.