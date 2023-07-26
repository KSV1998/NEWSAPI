# News API Server

### This is a simple web server that provides an API for accessing news articles from the News API. The API is implemented in Haskell using the Servant library.
Usage

### To run the server, simply run the following command:

* `cabal build` to build the project
* `cabal run` to start web server

### This will start the server on port 8080. You can then make requests to the API using the following endpoints:

   - `/articles`
        Returns a list of all articles.
   - `/search`
        Returns a list of articles that match the given search terms.
        Search options availble are author, title, keyword.

### The search terms can be specified using the author, title, and keyword query parameters. For example, to search for articles by the author "Bard", you would use the following URL:

### http://localhost:8080/search?apiKey=7967212c121e49a1baaa8048c75c7465&keyword=tesla

## Caching

### The server uses caching to improve performance. Articles are cached in memory for a period of time, so that they do not have to be fetched from the News API every time a request is made.