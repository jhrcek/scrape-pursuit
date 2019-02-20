# scrape-pursuit

Scrape info from pursuit documentation web site.

## How to run

1.  Setup local instance of pursuit as described in [pursuit README](https://github.com/purescript/pursuit)
2.  Setup selenium server:
    -   a. download and unzip [chromedriver](http://chromedriver.chromium.org/downloads) and place it somewhere on your PATH
    -   b. download and start selenium server (you can stop it later using ctrl+C)

```bash
wget http://repo.spring.io/plugins-release/org/seleniumhq/selenium/selenium-server-standalone/2.53.0/selenium-server-standalone-2.53.0.jar
java -jar selenium-server-standalone-2.53.0.jar
```

3.  Generate GitHub [personal access token](https://github.com/settings/tokens) to avoid failures due to rate-limiting
4.  Build and run the tool using [stack](https://docs.haskellstack.org/en/stable/README/)

```bash
stack build
stack exec scrape-pursuit -- --github-token <TOKEN>
```
