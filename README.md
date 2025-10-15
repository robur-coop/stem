# stem, a stemming algorithm in OCaml

A stemming algorithm is an algorithm that attempts to find the root of words.
This library allows you to "tokenize" a document and apply the stemming
algorithm to these tokens (considered to be words). It then calculates the
frequency of occurrence of these words and produces a CSV document mapping the
"stems" to their frequencies.

The purpose of stemming is to be able to treat several words (such as "tout",
"toutes", and "tous") as a single root. This way, the resulting stems and their
frequencies better reflect the information the document is trying to convey.
The idea is then to enable document indexing based on these stems.

## How to install it and use it?

stem is a package available through OPAM. It provides two tools: `stemmer` and
`stem.ts`. The latter allows you to specify multiple tokenizers, the language,
and the way the result is displayed in CSV format:

```shell
$ opam install stem
$ stem.ts -l french -a bert:isolate -a whitespace:remove file.txt
"est",14                             
"son",13
"tout",11
"Julien",11
"plus",9
"trouv",8
"dan",8
"bien",7
"m\195\170m",7
...
```
