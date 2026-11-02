import { Elm } from './Main.elm'

const app = Elm.Main.init({
    node: document.getElementById("elm"),
})

app.ports?.vibrate?.subscribe((pattern) => {
    try {
        navigator.vibrate?.(pattern)
    } catch (_) {}
})

let wantsWakeLock = false
let wakeLock = null
let pendingRequest = null

async function requestWakeLock() {
    if (!navigator.wakeLock || pendingRequest || (wakeLock && !wakeLock.released)) return
    try {
        pendingRequest = navigator.wakeLock.request("screen")
        const lock = await pendingRequest
        if (!lock.released) {
            lock.addEventListener("release", () => {
                if (wakeLock === lock) wakeLock = null
            })
            wakeLock = lock
        }
    } catch (_) {
    } finally {
        pendingRequest = null
    }
    if (!wantsWakeLock) releaseWakeLock()
}

function releaseWakeLock() {
    wakeLock?.release().catch(() => {})
    wakeLock = null
}

app.ports?.keepAwake?.subscribe((keep) => {
    wantsWakeLock = keep
    keep ? requestWakeLock() : releaseWakeLock()
})

document.addEventListener("visibilitychange", () => {
    if (wantsWakeLock && document.visibilityState === "visible") requestWakeLock()
})

let guarded = false

function onBeforeUnload(event) {
    event.preventDefault()
    event.returnValue = ""
}

function pushGuard() {
    history.pushState({ secretWordGuard: true }, "")
}

function unguard() {
    guarded = false
    window.removeEventListener("beforeunload", onBeforeUnload)
}

app.ports?.protectGame?.subscribe((protect) => {
    if (protect) {
        if (guarded) return
        guarded = true
        window.addEventListener("beforeunload", onBeforeUnload)
        pushGuard()
    } else {
        if (!guarded) return
        unguard()
        if (history.state?.secretWordGuard) history.back()
    }
})

app.ports?.leaveGame?.subscribe(() => {
    if (!guarded) return
    unguard()
    // -1 tira a guarda, -2 é o voltar que o jogador pediu
    history.go(-2)
})

window.addEventListener("popstate", () => {
    if (!guarded) return
    pushGuard()
    app.ports?.backPressed?.send(null)
})
