exports.init = async function (app) {
    app.ports.plausible_event_out.subscribe(function (eventName) {
        plausible(eventName)
    })
}