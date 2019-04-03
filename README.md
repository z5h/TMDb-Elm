# TMDb-Elm
Elm interface to TMDb themoviedb.org

This app is written in Elm (https://www.elm-lang.org)

We will install the elm tools via node/npm.

1. Install node current https://nodejs.org/en/
2. Install required tools `npm install`
3. create your personal config file for your API key.  
   Note that this file is `.gitignore`-ed so you won't accidentally commit secrets.  
   `cp web/config.example.js web/config.js`
4. edit `web/config.js` and add your TMDb API key (v3 auth).
5. run `make live`
6. a server will start and your browser should open.
 
