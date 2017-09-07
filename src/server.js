/** Server **/
const express = require('express');
const bodyParser = require('body-parser');
const app = express();
const path = require('path');
const request = require('request');

const staticPath = path.join(__dirname, '../public');

app.set('trust proxy', 1); // trust first proxy

// Send request to healthforge API (to receive bearer token)
const loginRequest = (username, password) =>
  new Promise((res, rej) => {
    const options = {
      headers: {
        'Content-Type': 'application/x-www-form-urlencoded'
      },
      url:
        'https://auth.healthforge.io/auth/realms/interview/protocol/openid-connect/token',
      form: {
        username,
        password,
        grant_type: 'password',
        client_id: 'interview'
      },
      method: 'POST'
    };
    // Fire request
    request(options, (err, response, body) => {
      // Reject general errors in request
      if (err) {
        rej('Error authenticating user');
      } else {
        // Request log in errors
        body = JSON.parse(body);
        if (body.error) {
          // Detect credentials errors
          rej('Invalid Credentials');
        } else {
          // Valid
          res(body.access_token);
        }
      }
    });
  });

// Set static file path
app.use(express.static(path.join(__dirname, staticPath)));

// Body Parsing
app.use(bodyParser.json()); // for parsing application/json
app.use(bodyParser.urlencoded({ extended: true })); // for parsing application/x-www-form-urlencoded

// Serve main app
app.get('/*', function(req, res) {
  res.sendFile(path.join(staticPath, 'index.html'));
});

// Post to login
app.post('/login', function(req, res) {
  const { username, password } = req.body;
  // Authenticate username & password
  loginRequest(username, password)
    .then(token => {
      res.status(200).json({
        token
      });
    })
    .catch(error => {
      res.status(500).json({
        error
      });
    });
});

// Default export
module.exports = app;
