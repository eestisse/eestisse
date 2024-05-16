exports.init = async function (app) {
    app.ports.consoleLog.subscribe(function (s) {
        console.log(s);
    })
    app.ports.consoleErr.subscribe(function (s) {
        console.error(s);
    })
}