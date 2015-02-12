# What is this?
Sparse is a very simple parser combinator library for parsing small in memory strings using F#.

# What are parser combinators and why should I use them?
The FParsec library has a good overview [here](http://www.quanttec.com/fparsec/about/fparsec-vs-alternatives.html).

# Why use Sparse when we already have FParsec?
FParsec is awesome! If you're writing a parser for a language, need good error messages, want to support streaming
input, and only need to support .NET40 etc. Unfortunately this means that it cannot (at least without some extra
work) be used in projects targetting Xamarin, or in PCL libraries.

Sparse is not intended to be a replacement in anyway for FParsec. Instead it is a complimentary library whose main
use is to support parsing scenarios common in application development that traditionally have been accomplished using
complex regular expressions (parsing phone numbers, ip addresses, HTTP header values, etc.).

# How does the API compare to FParsec?
Sparse exposes a set a combinators which is a subset of the combinators available in FParsec. It also includes a 
few that are not included in FParsec. I've made every attempt to make Sparse as compatible with FParsec as possible. 
In theory you should be able to use an FParsec parser definition almost verbatim with Sparse.

# How does the API differ from FParsec?

FParsec utilizes a stream like interface to read in tokens from the input for parsing, which limits backtracking
intentionally without additional work and memory consumption. In contrast, Sparse parsers always backtrack on 
failure. Back tracking is less costly in Sparse, as Sparse parsers only track an offset index into an in memory 
string. 

In addition, Sparse does not support error messages. Instead Sparse parsers return the index in the string at which 
parsing failed. For large, complex input (like the type you would use FParsec to parse), this would result in a 
painful debugging. However for the types of input that Sparse is used to parse (short strings) this is less of an 
issue.

# Sparse is missing a combinator I really need that FParsec supports. Will you add it?

Probably. Right now the combinators supported in the library are limited to the set I needed to easily implement HTTP
header parsing in [FunctionalHttp](https://github.com/bordoley/FunctionalHttp/). If you have a use case for an 
additional combinator, open an issue and I'll consider implementing it. Bonus points for pull requests!

