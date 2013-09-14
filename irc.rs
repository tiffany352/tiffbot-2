use parse::*;

#[deriving(Clone)]
pub struct Prefix {
    nick: ~str,
    user: ~str,
    host: ~str
}

impl ToStr for Prefix {
    fn to_str(&self) -> ~str {
        match (self.nick.clone(), self.user.clone(), self.host.clone()) {
            (~"", ~"", ~"") => ~"",
            (nick, ~"", ~"") => nick,
            (nick, ~"", host) => nick + "@" + host,
            (nick, user, host) => nick + "!" + user + "@" + host
        }
    }
}

#[deriving(Clone)]
pub struct Message {
    prefix: Prefix,
    command: ~str,
    params: ~[~str]
}

impl ToStr for Message {
    fn to_str(&self) -> ~str {
        match self.params.last().iter().any(|x| x.is_whitespace()) {
            true  => fmt!(":%s %s %s :%s", self.prefix.to_str(), self.command, self.params.init().connect(" "), self.params.last().clone()),
            false => fmt!(":%s %s %s %s", self.prefix.to_str(), self.command, self.params.init().connect(" "), self.params.last().clone())
        }
    }
}

#[deriving(Clone)]
pub enum IRCToken {
    Sequence(~[IRCToken]),
    Unparsed(~str),
    PrefixT(Prefix),
    Params(~[~str]),
    MessageT(Message),
    Ignored
}

impl TokenCreator for IRCToken {
    fn sequence(arr: ~[Token<IRCToken>]) -> IRCToken {
        Sequence(arr.map(|x| x.value.clone()))
    }
    fn raw(s: ~str) -> IRCToken {
        Unparsed(s)
    }
}

fn build_unparsed(s: ~str) -> Result<IRCToken, ~str> {
    Ok(Unparsed(s))
}

fn map_ignored(_: IRCToken) -> Result<IRCToken, ~str> {
    Ok(Ignored)
}

fn map_params(tok: IRCToken) -> Result<IRCToken, ~str> {
    // Sequence(~[Sequence(~[Sequence(~[Ignored, Unparsed(~"##codelab")])]), Sequence(~[Sequence(~[Ignored, Unparsed(~":"), Unparsed(~"hi")])])])
    match tok {
        Sequence(args) => Ok(Params(args.map(|arg| {
            match arg.clone() {
                Sequence([Sequence([Ignored, Unparsed(param)])]) => param,
                Sequence([Sequence([Ignored, Unparsed(~":"), Unparsed(param)])]) => param,
                _ => ~""
            }
        }))),
        _ => Err(~"Malformed parameters")
    }
}

fn map_message(tok: IRCToken) -> Result<IRCToken, ~str> {
    // Sequence(~[Sequence(~[Sequence(~[Unparsed(~":"), PrefixT(irc::Prefix{nick: ~"tiffany", user: ~"lymia", host: ~"hugs"}), Ignored])]), Unparsed(~"PRIVMSG"), Sequence(~[Params(~[~"##codelab", ~"hi"])])])
    match tok {
        Sequence([Sequence([Sequence([Unparsed(~":"), PrefixT(prefix), Ignored])]), Unparsed(cmd), Sequence([Params(params)])]) =>
            Ok(MessageT(Message {prefix: prefix, command: cmd, params: params})),
        _ => Err(~"Malformed message")
    }
}

fn map_prefix(tok: IRCToken) -> Result<IRCToken, ~str> {
    match tok {
        Sequence([Unparsed(nick), Sequence([rest])]) => match rest {
            Sequence([Sequence([rest]), Unparsed(~"@"), Unparsed(host)]) => match rest {
                Sequence([Unparsed(~"!"), Unparsed(user)]) => Ok(PrefixT(Prefix {nick: nick, user: user, host: host})),
                _ => Ok(PrefixT(Prefix {nick: nick, user: ~"", host: host})),
            },
            _ => Ok(PrefixT(Prefix {nick: nick, user: ~"", host: ~""})),
        },
        _ => Err(~"Malformed prefix")
    }
}

pub fn grammar() -> ParseContext<IRCToken> {
    let mut ctx = ParseContext::new();
    /*
    message    =  [ ":" prefix SPACE ] command [ params ] crlf
    prefix     =  servername / ( nickname [ [ "!" user ] "@" host ] )
    command    =  1*letter / 3digit
    params     =  *14( SPACE middle ) [ SPACE ":" trailing ]
               =/ 14( SPACE middle ) [ SPACE [ ":" ] trailing ]

    nospcrlfcl =  %x01-09 / %x0B-0C / %x0E-1F / %x21-39 / %x3B-FF
                    ; any octet except NUL, CR, LF, " " and ":"
    middle     =  nospcrlfcl *( ":" / nospcrlfcl )
    trailing   =  *( ":" / " " / nospcrlfcl )

    SPACE      =  %x20        ; space character
    crlf       =  %x0D %x0A   ; "carriage return" "linefeed" 
    */
    ctx.rule("message", ~Map(~LessThan(1, ~Literal(":") * ~Rule("prefix") * ~Rule("SPACE")) * ~Rule("command") * ~LessThan(1, ~Rule("params")), map_message));
    ctx.rule("prefix", ~Map(~Rule("nickname") * ~LessThan(1, ~LessThan(1, ~Literal("!") * ~Rule("user")) * ~Literal("@") * ~Rule("host")), map_prefix));
    ctx.rule("command", ~Build(~MoreThan(1, ~Rule("letter")) + ~Exactly(3, ~Rule("digit")), build_unparsed));
    ctx.rule("params", ~Map(~More(~Rule("SPACE") * ~Rule("middle")) * ~LessThan(1, ~Rule("SPACE") * ~Literal(":") * ~Rule("trailing"))
                          + ~More(~Rule("SPACE") * ~Rule("middle")) * ~LessThan(1, ~Rule("SPACE") * ~LessThan(1, ~Literal(":")) * ~Rule("trailing")), map_params));
    ctx.rule("nospcrlfcl", ~Diff(~Chars(1), ~Set("\x00\r\n :".iter().collect())));
    ctx.rule("middle", ~Build(~Rule("nospcrlfcl") * ~More(~Literal(":") + ~Rule("nospcrlfcl")), build_unparsed));
    ctx.rule("trailing", ~Build(~More(~Literal(":") + ~Literal(" ") + ~Rule("nospcrlfcl")), build_unparsed));
    ctx.rule("SPACE", ~Map(~Literal(" "), map_ignored));
    ctx.rule("crlf", ~Literal("\r\n"));
    /*
    target     =  nickname / server
    msgtarget  =  msgto *( "," msgto )
    msgto      =  channel / ( user [ "%" host ] "@" servername )
    msgto      =/ ( user "%" host ) / targetmask
    msgto      =/ nickname / ( nickname "!" user "@" host )
    channel    =  ( "#" / "+" / ( "!" channelid ) / "&" ) chanstring
                [ ":" chanstring ]
    servername =  hostname
    host       =  hostname / hostaddr
    hostname   =  shortname *( "." shortname )
    shortname  =  ( letter / digit ) *( letter / digit / "-" )
                *( letter / digit )
                  ; as specified in RFC 1123 [HNAME]
    hostaddr   =  ip4addr / ip6addr
    ip4addr    =  1*3digit "." 1*3digit "." 1*3digit "." 1*3digit
    ip6addr    =  1*hexdigit 7( ":" 1*hexdigit )
    ip6addr    =/ "0:0:0:0:0:" ( "0" / "FFFF" ) ":" ip4addr
    nickname   =  ( letter / special ) *8( letter / digit / special / "-" )
    targetmask =  ( "$" / "#" ) mask
                  ; see details on allowed masks in section 3.3.1
    chanstring =  %x01-07 / %x08-09 / %x0B-0C / %x0E-1F / %x21-2B
    chanstring =/ %x2D-39 / %x3B-FF
                  ; any octet except NUL, BELL, CR, LF, " ", "," and ":"
    channelid  = 5( %x41-5A / digit )   ; 5( A-Z / 0-9 )
    */
    ctx.rule("host", ~Build(~Rule("hostname") + ~Rule("hostaddr"), build_unparsed));
    ctx.rule("hostname", ~Rule("shortname") * ~More(~Literal(".") * ~Rule("shortname")));
    ctx.rule("shortname", (~Rule("letter") + ~Rule("digit")) * ~More(~Rule("letter") + ~Rule("digit") + ~Literal("-")));
    ctx.rule("hostaddr", ~Rule("ip4addr") + ~Rule("ip6addr"));
    ctx.rule("ip4addr", ~Exactly(3, ~Rule("digit")) * ~Exactly(3, ~Exactly(3, ~Literal(".") * ~Rule("digit"))));
    ctx.rule("ip6addr", ~Rule("hexdigit") * ~Exactly(7, ~Literal(":") * ~Rule("hexdigit"))
                      + ~Literal("0:0:0:0:0:") * (~Literal("0") + ~Literal("FFFF")) * ~Literal(":") * ~Rule("ip4addr"));
    ctx.rule("nickname", ~Build((~Rule("letter") + ~Rule("special")) * ~More(~Rule("letter") + ~Rule("digit") + ~Rule("special") + ~Literal("-")), build_unparsed));
    /*
    user       =  1*( %x01-09 / %x0B-0C / %x0E-1F / %x21-3F / %x41-FF )
                    ; any octet except NUL, CR, LF, " " and "@"
    key        =  1*23( %x01-05 / %x07-08 / %x0C / %x0E-1F / %x21-7F )
                      ; any 7-bit US_ASCII character,
                      ; except NUL, CR, LF, FF, h/v TABs, and " "
    letter     =  %x41-5A / %x61-7A       ; A-Z / a-z
    digit      =  %x30-39                 ; 0-9
    hexdigit   =  digit / "A" / "B" / "C" / "D" / "E" / "F"
    special    =  %x5B-60 / %x7B-7D
                       ; "[", "]", "\", "`", "_", "^", "{", "|", "}"
    */
    ctx.rule("user", ~Build(~MoreThan(1, ~Diff(~Chars(1), ~Set("\x00\r\n @".iter().collect()))), build_unparsed));
    ctx.rule("letter", ~Range('a','z') + ~Range('A','Z'));
    ctx.rule("digit", ~Range('0','9'));
    ctx.rule("hexdigit", ~Rule("digit") + ~Range('A','F'));
    ctx.rule("special", ~Set("[]\\`_^{|}".iter().collect()));

    ctx
}

