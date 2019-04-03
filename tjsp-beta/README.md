# TJ-SP Package

This package contains a web scraper and multiple parser functions for judicial decisions issued by the State Court of São Paulo (TJ-SP). Currently, the scraper class has two search functions and one scrape functions; the parser class has five functions to download different parts of the judicial decision.

## Scraper Functions

1. **name():** finds a lawsuit's individual identifier using litigants' names.
2. **cpf():** fins a lawsuit's individual identifier using litigants' CPF (the Brazilian equivalent to Social Security Numbers).
3. **case():** finds and downloads the judicial decision history using lawsuits' individual identifiers.

## Parser Functions

1. **parse_summary():** parses the summary table.
2. **parse_litigants():** parses the litigants table.
3. **parse_updates():** parses the case updates table.
4. **parse_petitions():** parses the petitions table.
5. **parse_incidents():** not implemented.
6. **parse_attachments():** not implemented.
7. **parse_hearings():** parses the hearings table.

## Author
Andre Assumpcao

## Contact
andre.assumpcao@gmail.com


