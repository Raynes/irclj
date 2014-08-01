# irclj

Irclj is a very simple IRC library for Clojure. It is entirely asynchronous and
callback-based. It has been a WIP for quite a while (I don't have a ton of time
to spend on it), but it is entirely functional and is being used by some
projects already. Just don't be surprised if it's missing a feature that might
seem common place. Good news is that the infrastructure to easily add features
is there.

## Usage

https://clojars.org/irclj

Note that this is still a work in progress. Some things may not work, or obvious things may be missing.
Check out the issue tracker to see what needs to be done, and please contribute
if you can!

```clojure
user=> (require '[irclj.core :as irc])
nil
user=> (def connection (irc/connect "irc.freenode.net" 6667 "hotbot" :callbacks {:privmsg (fn [irc type s] (prn irc type s))}))
#'user/connection
user=> (irc/join connection "#4clojure")
nil
user=> (irc/kill connection)
nil
user=> (def connection (irc/connect "irc.freenode.net" 6667 "hotbot" :callbacks {:privmsg (fn [irc type & s] (prn irc type s))}))
#'user/connection
user=> (irc/join connection "#4clojure")
nil
...say something in-channel...
<buncha shit gets printed>
```

Irclj is callback based, so you can register callbacks on any kind of IRC
protocol message, including the numeric messages. Most of the time you're going
to want to capture PRIVMSG as demonstrated above though. Check out `events.clj`
and `process.clj`.

For a larger, but also simple example of a working bot, check out this fellow's
bot: https://github.com/boxed/atpshowbot


## License

Licensed under the EPL, same as Clojure itself.
