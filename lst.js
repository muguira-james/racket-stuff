const fetch = require('node-fetch')

var data = {
    quename: "james"
}

fetch('http://localhost:8000/queue-list', {
    method: 'post',
    body: JSON.stringify(data),
    headers: { 'Content-Type' : 'application/json' },
})

.then(res => res.text() )
.then(txt => console.log(txt));


