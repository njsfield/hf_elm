// Import main
const app = require('./src/server');

const port = process.env.PORT || 9000;
const host = process.env.HOST || 'http://localhost';

app.listen(port, () => console.log(`Live at ${host}:${port}`));
