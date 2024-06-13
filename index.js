// Start the Elm application.
let app = Elm.Main.init({
    node: document.getElementById('elm')
});

let socket = io('http://127.0.0.1:8081');


socket.on('connect', function () {
    // sends to socket.io server the host/port of oscServer
    // and oscClient
    socket.emit('config',
        {
            server: {
                port: 3333,
                host: '127.0.0.1'
            },
            client: {
                port: 3334,
                host: '127.0.0.1'
            }
        }
    );
});

app.ports.toCisp.subscribe(function (message) {
    console.log('elm calling:', message);
    socket.send('/cisp/ls');
});

socket.on('message', function (obj) {
    console.log('socket receiving message', obj);
    app.ports.fromCisp.send(obj);
    var status = document.getElementById("status");
    status.innerHTML = obj[0];
    console.log(obj);
});