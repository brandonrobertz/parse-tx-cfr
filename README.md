<<<<<<< HEAD
# Parse-TX-CFR

An experimental parser for PDF Campaign Finance Reports from the Texas Ethics Commission.

## Usage

Right now you have to edit the source to modify which pages it scrapes. Currently it's hardcoded to scrape all the contributor-containing pages in `data/test.pdf` (pages 4-46).

    $ cd parse-tc-cfr
    $ lein run

Running it will give you a list of vectors, each of which contains the information from a contributor cell in `data/test.pdf`. I don't clean up the output or convert it to a delimited format. As you can see, it's fairly accurate as it, with the exception of some line breaks.

## License

There are incompatabilities between GPL and Snowtide's PDFTextStream license (proprietary). This makes it impossible to distribute it as-is. Eventually I will port this to a free PDF parsing library, but until then you're on your own for installing PDFTextStream.

Copyright © 2013 Brandon Robertz
GPLv3+ (w/ considering PDFTextStream a system lib)
RIP EWOK BATES
=======
parse-tx-cfr
============

An experimental parser for TX Ethics Comission Campaign Finance Reports
>>>>>>> d50701170380450c10606e6f86a7e622382c3a70
