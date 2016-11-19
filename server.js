//
// # SimpleServer
//
// A simple chat server using Socket.IO, Express, and Async.
//
var http = require('http');
var path = require('path');

var async = require('async');
var express = require('express');

//
// ## SimpleServer `SimpleServer(obj)`
//
// Creates a new instance of SimpleServer with the following options:
//  * `port` - The HTTP port to listen on. If `process.env.PORT` is set, _it overrides this value_.
//
var router = express();
var server = http.createServer(router);

// POST method route
router.get('/calc/:param1/add/:param2', function (req, res) {
//  res.send(req.params + ' -- ' + 'POST request to the homepage\n');
  res.send((parseInt(req.params.param1) + parseInt(req.params.param2)).toString());
});

//router.use(express.static(path.resolve(__dirname, './flare-example')));
router.use(express.static(path.resolve(__dirname, './')));


server.listen(process.env.PORT || 3000, process.env.IP || "0.0.0.0", function(){
  var addr = server.address();
  console.log("Chat server listening at", addr.address + ":" + addr.port);
});
